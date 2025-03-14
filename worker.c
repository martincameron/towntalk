
#include "stddef.h"
#include "string.h"

#include "worker.h"

/*
	Extension for multi-threaded sub-interpreters.
	Only buffers can be passed by reference to a worker.
	
	Statements:
		lock worker {statements} Obtain specified worker lock and execute statements.
		locked {statements}      Obtain current worker lock and execute statements.
	
	Expressions:
		$worker(${(){stmts}})    Compile a worker function from an element.
		$execute(worker arg ...) Begin execution of the specified worker and return it.
		$result(worker)          Wait for the return value of a worker function.
*/

#define MAX_STACK 65536

/* Forward-declarations and externals. */
static struct worker* new_worker( char *message );
enum result evaluate_number_literal_expression( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_string_literal_expression( struct expression *this, struct variables *vars, struct variable *result );
struct statement* parse_keywords_indexed( struct keyword **index, struct element *elem, struct function *func, struct variables *vars, struct statement *stmt, char *message );
struct element* parse_expression( struct element *elem, struct function *func, struct variables *vars, struct expression *prev, char *message );
struct function* parse_function( struct element *elem, char *name, struct function *parent, char *message );
int parse_function_body( struct function *func, struct variables *vars, char *message );
struct element* validate_syntax( char *syntax, struct element *elem, struct element *prev, struct environment *env, char *message );

#if !defined( MULTI_THREAD )
/* Add thread-safe custom statements and operators to the specified worker.
   Returns 0 and assigns message on failure. */
int initialize_worker( struct worker *work, char *message ) {
	return 1;
}

/* Begin execution of the specified worker. Returns 0 on failure. */
int start_worker( struct worker *work ) {
	struct variables vars = { 0 };
	struct function_expression expr = { 0 };
	vars.exception = &work->exception;
	initialize_entry_point( &expr, work->env.entry_point );
	expr.expr.parameters = work->parameters;
	work->ret = expr.expr.evaluate( &expr.expr, &vars, &work->result );
	return 1;
}

/* Lock the specified worker mutex. Returns 0 on failure. */
int lock_worker( struct worker *work ) {
	return 1;
}

/* Unlock the specified worker mutex. Returns 0 on failure. */
int unlock_worker( struct worker *work ) {
	return 1;
}

/* Wait for the completion of the specified worker.
   If cancel is non-zero, the worker should be interrupted. */
void await_worker( struct worker *work, int cancel ) {
}
#endif

static void dispose_worker( struct string *str ) {
	int idx, len = 0;
	struct worker *work = ( struct worker * ) str;
	await_worker( work, 1 );
	if( work->env.entry_point ) {
		len = work->env.entry_point->num_parameters;
	}
	dispose_temporary( &work->result );
	dispose_temporary( &work->exception );
	dispose_environment( &work->env );
	for( idx = 0; idx < len; idx++ ) {
		dispose_temporary( &work->args[ idx ] );
	}
	free( work->args );
	free( work->strings );
	free( work->parameters );
	free( work );
}

static struct custom_type worker_type = {
	"Worker", NULL, NULL, dispose_worker
};

static enum result execute_lock_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct statement *stmt = ( ( struct block_statement * ) this )->if_block;
	struct worker *work = ( struct worker * ) vars->func->env->worker;
	struct variable var = { 0, NULL };
	char *locked = NULL;
	enum result ret = this->source->evaluate( this->source, vars, &var );
	if( ret ) {
		if( is_custom_instance( var.string_value, &worker_type ) ) {
			work = ( struct worker * ) var.string_value;
			locked = &work->locked;
		} else if( work ) {
			locked = &work->worker_locked;
		} else {
			ret = throw( vars, this->source, 0, "Not a worker.");
		}
		if( locked ) {
			if( locked[ 0 ] == 0 && lock_worker( work ) ) {
				locked[ 0 ] = 1;
				while( stmt ) {
					ret = stmt->execute( stmt, vars, result );
					if( ret == OKAY ) {
						stmt = stmt->next;
					} else {
						break;
					}
				}
				locked[ 0 ] = 0;
				if( unlock_worker( work ) == 0 ) {
					locked[ 0 ] = 1;
					ret = throw( vars, this->source, 0, "Unable to unlock worker.");
				}
			} else {
				ret = throw( vars, this->source, 0, "Unable to lock worker.");
			}
		}
		dispose_temporary( &var );
	}
	return ret;
}

struct element* parse_lock_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement block = { 0 }, *stmt = calloc( 1, sizeof( struct block_statement ) );
	if( stmt ) {
		stmt->dispose = dispose_block_statement;
		prev->next = stmt;
		if( next->str.string[ 0 ] == '{' ) {
			expr.next = calloc( 1, sizeof( struct value_expression ) );
			if( expr.next ) {
				expr.next->line = elem->line;
				expr.next->evaluate = evaluate_number_literal_expression;
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		} else {
			expr.next = NULL;
			next = parse_expression( next, func, vars, &expr, message );
		}
		if( message[ 0 ] == 0 ) {
			stmt->source = expr.next;
			stmt->execute = execute_lock_statement;
			if( next->child ) {
				block.next = NULL;
				parse_keywords_indexed( func->env->statements_index, next->child, func, vars, &block, message );
				( ( struct block_statement * ) stmt )->if_block = block.next;
			}
			if( message[ 0 ] == 0 ) {
				next = next->next;
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct worker* parse_worker( struct element *elem, struct string *file, char *message ) {
	int params, idx;
	struct function *func, parent;
	struct worker *work = new_worker( message );
	if( work ) {
		parent.library = NULL;
		parent.env = &work->env;
		parent.file = new_string_value( file->string );
		if( parent.file ) {
			func = parse_function( elem, work->custom.str.string, &parent, message );
			if( func ) {
				if( add_decl( &func->str, elem->line, &work->env, message ) ) {
					work->env.entry_point = func;
					params = func->num_parameters;
					work->args = calloc( params, sizeof( struct variable ) );
					if( work->args ) {
						work->strings = calloc( params, sizeof( struct array ) );
					}
					if( work->strings ) {
						work->parameters = calloc( params, sizeof( struct value_expression ) );
					}
					if( work->parameters ) {
						for( idx = 0; idx < params; idx++ ) {
							work->parameters[ idx ].next = &work->parameters[ idx + 1 ];
						}
						parse_function_body( func, NULL, message );
					} else {
						strcpy( message, OUT_OF_MEMORY );
					}
				}
				unref_string( &func->str );
			}
			unref_string( parent.file );
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
		if( message[ 0 ] ) {
			unref_string( &work->custom.str );
			work = NULL;
		}
	}
	return work;
}

enum result evaluate_worker_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct element *elem, prev = { { 1, "$worker", 9, ELEMENT }, NULL, NULL, 0 };
	struct environment *env = vars->func->env;
	struct variable var = { 0, NULL };
	char message[ 128 ] = "";
	struct worker *work;
	enum result ret;
	if( vars->func->env->worker ) {
		ret = throw( vars, this, 0, "Operation not permitted." );
	} else {
		ret = evaluate_element( parameter, vars, &var, 0 );
	}
	if( ret ) {
		elem = ( struct element * ) var.string_value;
		if( ( size_t ) &ret < env->stack_limit ) {
			ret = throw_stack_overflow( vars, this );
		} else {
			prev.line = this->line;
			validate_syntax( "({0", elem, &prev, env, message );
			if( message[ 0 ] == 0 ) {
				work = parse_worker( elem, vars->func->file, message );
				if( work ) {
					result->string_value = &work->custom.str;
				} else {
					ret = throw( vars, this, 0, message );
				}
			} else {
				ret = throw( vars, this, 0, message );
			}
		}
		dispose_temporary( &var );
	}
	return ret;
}

enum result evaluate_execute_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct environment *env = vars->func->env;
	struct variable var = { 0, NULL };
	struct worker *work;
	struct string *str;
	int count, idx;
	enum result ret = evaluate_custom( parameter, &worker_type, vars, &var );
	if( ret ) {
		work = ( struct worker * ) var.string_value;
		parameter = parameter->next;
		count = 0;
		while( parameter ) {
			count++;
			parameter = parameter->next;
		}
		if( work->env.entry_point->num_parameters == count ) {
			if( ( size_t ) &ret < env->stack_limit ) {
				ret = throw_stack_overflow( vars, this );
			} else if( work->locked == 0 ) {
				await_worker( work, 1 );
				idx = 0;
				parameter = this->parameters->next;
				while( parameter && ret ) {
					dispose_variable( &work->args[ idx ] );
					ret = parameter->evaluate( parameter, vars, &work->args[ idx ] );
					if( ret ) {
						work->parameters[ idx ].index = ( long_int ) work->args[ idx ].number_value;
						( ( struct value_expression * ) work->parameters )[ idx ].num = work->args[ idx ].number_value;
						str = work->args[ idx ].string_value;
						if( str ) {
							if( str->type == STRING || ( str->type == ARRAY && !( ( struct array * ) str )->string_values ) ) {
								work->strings[ idx ].str.reference_count = 1;
								work->strings[ idx ].str.type = str->type;
								work->strings[ idx ].str.string = str->string;
								work->strings[ idx ].str.length = str->length;
								if( str->type == ARRAY ) {
									work->strings[ idx ].number_values = ( ( struct array * ) str )->number_values;
									work->strings[ idx ].length = ( ( struct array * ) str )->length;
								}
								( ( struct value_expression * ) work->parameters )[ idx ].str = &work->strings[ idx ].str;
								work->parameters[ idx ].evaluate = evaluate_string_literal_expression;
							} else {
								ret = throw( vars, this, 0, "Values of this type cannot be passed to workers." );
							}
						} else {
							( ( struct value_expression * ) work->parameters )[ idx ].str = NULL;
							work->parameters[ idx ].evaluate = evaluate_number_literal_expression;
						}
					}
					parameter = parameter->next;
					idx++;
				}
				if( ret ) {
					work->ret = OKAY;
					dispose_variable( &work->result );
					dispose_variable( &work->exception );
					env->worker = &work->custom;
					work->env.interrupted = env->interrupted;
					if( start_worker( work ) ) {
						result->string_value = var.string_value;
						result->string_value->reference_count++;
					} else {
						ret = throw( vars, this, 0, "Unable to start worker." );
					}
					env->worker = NULL;
				}
			} else {
				ret = throw( vars, this, 0, "Worker locked." );
			}
		} else {
			ret = throw( vars, this, count, "Incorrect number of parameters to function." );
		}
		dispose_temporary( &var );
	}
	return ret;
}

enum result evaluate_result_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct worker *work;
	int count, idx;
	char *str;
	enum result ret = evaluate_custom( parameter, &worker_type, vars, &var );
	if( ret ) {
		work = ( struct worker * ) var.string_value;
		if( work->locked == 0 ) {
			vars->func->env->worker = &work->custom;
			await_worker( work, vars->func->env->interrupted );
			vars->func->env->worker = NULL;
			count = work->env.entry_point->num_parameters;
			for( idx = 0; idx < count; idx++ ) {
				if( work->args[ idx ].string_value ) {
					/* Reassociate parameter strings if necessary. */
					str = work->args[ idx ].string_value->string;
					if( work->result.string_value && work->result.string_value->string == str ) {
						assign_variable( &work->args[ idx ], &work->result );
					}
					if( work->exception.string_value && work->exception.string_value->string == str ) {
						assign_variable( &work->args[ idx ], &work->exception );
					}
				}
			}
			if( work->ret == OKAY ) {
				if( work->result.string_value && work->result.string_value->type > ELEMENT ) {
					/* Only strings and elements can safely be assigned from another environment. */
					ret = throw( vars, this, 0, "Values of this type cannot be returned from workers." );
				} else {
					assign_variable( &work->result, result );
				}
			} else if( work->exception.string_value && work->exception.string_value->type > ELEMENT ) {
				ret = throw( vars, this, work->exception.number_value, work->exception.string_value->string );
			} else {
				assign_variable( &work->exception, vars->exception );
				ret = EXCEPTION;
			}
		} else {
			ret = throw( vars, this, 0, "Worker locked." );
		}
		dispose_temporary( &var );
	}
	return ret;
}
struct keyword lock_statement[] = {
	{ "lock", "x{", parse_lock_statement, NULL },
	{ NULL }
};

struct keyword locked_statement[] = {
	{ "locked", "{", parse_lock_statement, NULL },
	{ NULL }
};

struct operator worker_operators[] = {
	{ "$worker", '$', 1, evaluate_worker_expression, NULL },
	{ "$execute", '$',-1, evaluate_execute_expression, NULL },
	{ "$result", '$', 1, evaluate_result_expression, NULL },
	{ NULL }
};

int initialize_worker_extension( struct environment *env, char *message ) {
	return add_statements( lock_statement, env, message )
		&& add_operators( worker_operators, env, message );
}

static struct worker* new_worker( char *message ) {
	struct worker *work = calloc( 1, sizeof( struct worker ) );
	if( work ) {
		work->custom.type = &worker_type;
		work->custom.str.string = worker_type.name;
		work->custom.str.length = strlen( worker_type.name );
		work->custom.str.reference_count = 1;
		work->custom.str.type = CUSTOM;
		if( initialize_environment( &work->env, MAX_STACK, message )
		&& add_statements( locked_statement, &work->env, message )
		&& initialize_worker( work, message ) ) {
			work->env.worker = &work->custom;
		} else {
			unref_string( &work->custom.str );
			work = NULL;
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return work;
}
