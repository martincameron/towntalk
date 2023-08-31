
#include "stdio.h"
#include "string.h"

#include "towntalk.h"

/*
	Stack-machine for fast integer arithmetic and assignment.
	
	Sequential statements of the following forms are combined into a single subinterpreter statement:
	
		let local = arithmetic_expr;
		let local = $chr( local expr );
		let local = integer_literal;
		let local = local;
		let local = [ local expr ];
		let [ local expr ] = arithmetic_expr;
		let [ local expr ] = $chr( local expr );
		let [ local expr ] = integer_literal;
		let [ local expr ] = local;
		let [ local expr ] = [ local expr ];
		inc local;
		dec local;
	
	 Best performance is achieved when used in loops and conditional statements of the following forms:
	
		while relational_expr( local local ) ...
		while relational_expr( local integer_literal ) ...
		if relational_expr( local local ) ....
		if relational_expr( local integer_literal ) ....
	
	Relational expressions are one of "<", "<e", "=", "<>", ">e", or ">".
*/

enum result evaluate_arithmetic_expression( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_chr_expression( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_index_expression( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_global( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_local( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_local_post_inc( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_local_post_dec( struct expression *this, struct variables *vars, struct variable *result );
enum result execute_array_assignment( struct statement *this, struct variables *vars, struct variable *result );
enum result execute_local_assignment( struct statement *this, struct variables *vars, struct variable *result );
enum result execute_increment_statement( struct statement *this, struct variables *vars, struct variable *result );
enum result execute_decrement_statement( struct statement *this, struct variables *vars, struct variable *result );
int to_int( struct variable *var );

enum arithmetic_op {
	HALT, PUSH_CONST, PUSH_LOCAL, INC_LOCAL, PUSH_LOCAL_PI, DEC_LOCAL, PUSH_LOCAL_PD, PUSH_EXPR, PUSH_ARRAY, PUSH_STRING,
	POP_LOCAL, STORE_LOCAL, CHECK_ARRAY, POP_ARRAY, STORE_ARRAY, AND, OR, XOR, ADD, SUB, MUL, DIV, MOD, ASL, ASR,
	AND_CONST, OR_CONST, XOR_CONST, ADD_CONST, SUB_CONST, MUL_CONST, DIV_CONST, MOD_CONST, ASL_CONST, ASR_CONST,
	AND_LOCAL, OR_LOCAL, XOR_LOCAL, ADD_LOCAL, SUB_LOCAL, MUL_LOCAL, DIV_LOCAL, MOD_LOCAL, ASL_LOCAL, ASR_LOCAL
};

struct instruction {
	int oper, local;
	struct expression *expr;
};

struct instructions {
	struct instruction *list;
	int capacity, count;
};

struct arithmetic_statement {
	struct statement stmt;
	struct instructions insns;
};

static struct instruction* add_instructions( struct instructions *dest, struct instruction *src, int count, char *message ) {
	struct instruction *insn;
	int capacity, new_count = dest->count + count;
	if( dest->capacity <= new_count ) {
		capacity = 4;
		while( capacity <= new_count ) {
			capacity <<= 1;
		}
		insn = calloc( capacity, sizeof( struct instruction ) );
		if( insn ) {
			memcpy( insn, dest->list, dest->count * sizeof( struct instruction ) );
			free( dest->list );
			dest->list = insn;
			dest->capacity = capacity;
		} else {
			strcpy( message, OUT_OF_MEMORY );
			return NULL;
		}
	}
	memcpy( &dest->list[ dest->count ], src, count * sizeof( struct instruction ) );
	dest->count = new_count;
	return &dest->list[ new_count - 1 ];
}

static struct instruction* add_instruction( struct instructions *insns,
	enum arithmetic_op oper, int local, struct expression *expr, char *message ) {
	struct instruction insn;
	insn.oper = oper;
	insn.local = local;
	insn.expr = expr;
	return add_instructions( insns, &insn, 1, message );
}

static void print_insns( struct instructions *insns, int line ) {
	char *name;
	struct instruction *insn = insns->list;
	fprintf( stderr, "Compiled from line %d:\n", line );
	while( 1 ) {
		switch( insn->oper ) {
			case HALT: return;
			case PUSH_CONST: name = "PUSH_CONST"; break;
			case PUSH_LOCAL: name = "PUSH_LOCAL"; break;
			case INC_LOCAL: name = "INC_LOCAL"; break;
			case PUSH_LOCAL_PI: name = "PUSH_LOCAL_PI"; break;
			case DEC_LOCAL: name = "DEC_LOCAL"; break;
			case PUSH_LOCAL_PD: name = "PUSH_LOCAL_PD"; break;
			case PUSH_EXPR: name = "PUSH_EXPR"; break;
			case PUSH_ARRAY: name = "PUSH_ARRAY"; break;
			case PUSH_STRING: name = "PUSH_STRING"; break;
			case POP_LOCAL: name = "POP_LOCAL"; break;
			case STORE_LOCAL: name = "STORE_LOCAL"; break;
			case CHECK_ARRAY: name = "CHECK_ARRAY"; break;
			case POP_ARRAY: name = "POP_ARRAY"; break;
			case STORE_ARRAY: name = "STORE_ARRAY"; break;
			case AND: name = "AND"; break;
			case OR : name = "OR"; break;
			case XOR: name = "XOR"; break;
			case ADD: name = "ADD"; break;
			case SUB: name = "SUB"; break;
			case MUL: name = "MUL"; break;
			case DIV: name = "DIV"; break;
			case MOD: name = "MOD"; break;
			case ASL: name = "ASL"; break;
			case ASR: name = "ASR"; break;
			case AND_CONST: name = "AND_CONST"; break;
			case OR_CONST : name = "OR_CONST"; break;
			case XOR_CONST: name = "XOR_CONST"; break;
			case ADD_CONST: name = "ADD_CONST"; break;
			case SUB_CONST: name = "SUB_CONST"; break;
			case MUL_CONST: name = "MUL_CONST"; break;
			case DIV_CONST: name = "DIV_CONST"; break;
			case MOD_CONST: name = "MOD_CONST"; break;
			case ASL_CONST: name = "ASL_CONST"; break;
			case ASR_CONST: name = "ASR_CONST"; break;
			case AND_LOCAL: name = "AND_LOCAL"; break;
			case OR_LOCAL : name = "OR_LOCAL"; break;
			case XOR_LOCAL: name = "XOR_LOCAL"; break;
			case ADD_LOCAL: name = "ADD_LOCAL"; break;
			case SUB_LOCAL: name = "SUB_LOCAL"; break;
			case MUL_LOCAL: name = "MUL_LOCAL"; break;
			case DIV_LOCAL: name = "DIV_LOCAL"; break;
			case MOD_LOCAL: name = "MOD_LOCAL"; break;
			case ASL_LOCAL: name = "ASL_LOCAL"; break;
			case ASR_LOCAL: name = "ASR_LOCAL"; break;
			default: name = "XXX"; break;
		}
		fprintf( stderr, "    %s %d:\n", name, insn->local );
		insn++;
	}
}

static enum arithmetic_op get_arithmetic_op( struct expression *expr ) {
	char *chr;
	const char *OPS = "&:3+-*/%12";
	enum arithmetic_op oper = HALT;
	if( expr->evaluate == evaluate_arithmetic_expression ) {
		chr = strchr( OPS, expr->index );
		if( chr ) {
			oper = AND + ( chr - OPS );
		}
	}
	return oper;
}

static struct instruction* compile_arithmetic_expression( struct arithmetic_statement *stmt, struct expression *expr, int top, char *message ) {
	struct instruction *insn;
	struct expression *parameter;
	struct global_variable *global;
	enum arithmetic_op oper = get_arithmetic_op( expr );
	if( top > 6 ) {
		insn = add_instruction( &stmt->insns, PUSH_EXPR, 0, expr, message );
	} else if( oper ) {
		parameter = expr->parameters;
		insn = compile_arithmetic_expression( stmt, parameter, top, message );
		while( insn && parameter->next ) {
			parameter = parameter->next;
			insn = compile_arithmetic_expression( stmt, parameter, top + 1, message );
			if( insn ) {
				if( insn->oper == PUSH_CONST ) {
					insn->oper = oper + AND_CONST - AND;
				} else if( insn->oper == PUSH_LOCAL ) {
					insn->oper = oper + AND_LOCAL - AND;
				} else {
					insn = add_instruction( &stmt->insns, oper, 0, parameter, message );
				}
			}
		}
	} else if( expr->evaluate == evaluate_integer_literal_expression ) {
		insn = add_instruction( &stmt->insns, PUSH_CONST, expr->index, expr, message );
	} else if( expr->evaluate == evaluate_local ) {
		insn = add_instruction( &stmt->insns, PUSH_LOCAL, expr->index, expr, message );
	} else if( expr->evaluate == evaluate_local_post_inc ) {
		insn = add_instruction( &stmt->insns, PUSH_LOCAL_PI, expr->index, expr, message );
	} else if( expr->evaluate == evaluate_local_post_dec ) {
		insn = add_instruction( &stmt->insns, PUSH_LOCAL_PD, expr->index, expr, message );
	} else if( expr->evaluate == evaluate_index_expression && expr->parameters->evaluate == evaluate_local ) {
		insn = compile_arithmetic_expression( stmt, expr->parameters->next, top, message );
		if( insn ) {
			insn = add_instruction( &stmt->insns, PUSH_ARRAY, expr->parameters->index, expr, message );
		}
	} else if( expr->evaluate == evaluate_chr_expression && expr->parameters->evaluate == evaluate_local ) {
		insn = compile_arithmetic_expression( stmt, expr->parameters->next, top, message );
		if( insn ) {
			insn = add_instruction( &stmt->insns, PUSH_STRING, expr->parameters->index, expr, message );
		}
	} else {
		if( expr->evaluate == evaluate_global ) {
			global = ( struct global_variable * ) ( ( struct string_expression * ) expr )->str;
			if( global->str.type == CONST && global->initializer && global->initializer->evaluate == evaluate_integer_literal_expression ) {
				return add_instruction( &stmt->insns, PUSH_CONST, global->initializer->index, expr, message );
			}
		}
		insn = add_instruction( &stmt->insns, PUSH_EXPR, 0, expr, message );
	}
	return insn;
}

static void dispose_arithmetic_statement( struct statement *this ) {
	free( ( ( struct arithmetic_statement * ) this )->insns.list );
	free( this );
}

static enum result divide( int *lhs, int rhs, struct variables *vars, struct expression *src ) {
	if( rhs == 0 ) {
		return throw( vars, src, 0, "Division by zero." );
	}
	*lhs /= rhs;
	return OKAY;
}

static enum result modulo( int *lhs, int rhs, struct variables *vars, struct expression *src ) {
	if( rhs == 0 ) {
		return throw( vars, src, 0, "Modulo division by zero." );
	}
	*lhs %= rhs;
	return OKAY;
}

static enum result execute_arithmetic_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct instruction *insn = ( ( struct arithmetic_statement * ) this )->insns.list;
	int stack[ 8 ], *top = &stack[ -1 ], index;
	struct variable var, *locals = vars->locals, *local;
	struct array *arr;
	while( 1 ) {
		switch( insn->oper ) {
			case HALT:
				return OKAY;
			case PUSH_CONST:
				*++top = insn->local;
				break;
			case PUSH_LOCAL:
				local = locals + insn->local;
				var.integer_value = local->integer_value;
				var.string_value = local->string_value;
				*++top = var.integer_value;
				if( var.string_value ) {
					*top = to_int( local );
				}
				break;
			case INC_LOCAL: /* Fallthrough. */
			case PUSH_LOCAL_PI:
				local = locals + insn->local;
				if( local->string_value ) {
					return throw( vars, insn->expr, 0, "Not an integer." );
				} else if( insn->oper == INC_LOCAL ) {
					local->integer_value++;
				} else {
					*++top = local->integer_value++;
				}
				break;
			case DEC_LOCAL: /* Fallthrough. */
			case PUSH_LOCAL_PD:
				local = locals + insn->local;
				if( local->string_value ) {
					return throw( vars, insn->expr, 0, "Not an integer." );
				} else if( insn->oper == DEC_LOCAL ) {
					local->integer_value--;
				} else {
					*++top = local->integer_value--;
				}
				break;
			case PUSH_EXPR:
				var.integer_value = 0;
				var.string_value = NULL;
				if( insn->expr->evaluate( insn->expr, vars, &var ) ) {
					*++top = var.integer_value;
					if( var.string_value ) {
						*top = to_int( &var );
						unref_string( var.string_value );
					}
				} else {
					return EXCEPTION;
				}
				break;
			case PUSH_ARRAY:
				local = locals + insn->local;
				if( local->string_value && local->string_value->type == ARRAY ) {
					index = *top;
					arr = ( struct array * ) local->string_value;
					if( ( unsigned int ) index < ( unsigned int ) arr->length ) {
						var.integer_value = arr->integer_values[ index ];
						if( arr->string_values && arr->string_values[ index ] ) {
							var.string_value = arr->string_values[ index ];
							*top = to_int( &var );
						} else {
							var.string_value = NULL;
							*top = var.integer_value;
						}
					} else {
						return throw( vars, insn->expr, index, "Array index out of bounds." );
					}
				} else {
					return throw( vars, insn->expr, 0, "Not an array." );
				}
				break;
			case PUSH_STRING:
				local = locals + insn->local;
				if( local->string_value ) {
					index = *top;
					if( ( unsigned int ) index < ( unsigned int ) local->string_value->length ) {
						*top = ( signed char ) local->string_value->string[ index ];
					} else {
						return throw( vars, insn->expr, index, "String index out of bounds." );
					}
				} else {
					return throw( vars, insn->expr, 0, "Not a string." );
				}
				break;
			case POP_LOCAL:
				local = locals + insn->local;
				local->integer_value = *top--;
				if( local->string_value ) {
					unref_string( local->string_value );
					local->string_value = NULL;
				}
				break;
			case STORE_LOCAL:
				local = locals + insn->local;
				local->integer_value = var.integer_value;
				if( local->string_value ) {
					unref_string( local->string_value );
					local->string_value = NULL;
				}
				if( var.string_value ) {
					var.string_value->reference_count++;
					local->string_value = var.string_value;
				}
				top--;
				break;
			case CHECK_ARRAY:
				local = locals + insn->local;
				if( local->string_value && local->string_value->type == ARRAY ) {
					arr = ( struct array * ) local->string_value;
					if( ( unsigned int ) *top >= ( unsigned int ) arr->length ) {
						return throw( vars, insn->expr, *top, "Array index out of bounds." );
					}
				} else {
					return throw( vars, insn->expr, 0, "Not an array." );
				}
				break;
			case POP_ARRAY:
				top -= 2;
				index = top[ 1 ];
				arr = ( struct array * ) locals[ insn->local ].string_value;
				arr->integer_values[ index ] = top[ 2 ];
				if( arr->string_values && arr->string_values[ index ] ) {
					unref_string( arr->string_values[ index ] );
					arr->string_values[ index ] = NULL;
				}
				break;
			case STORE_ARRAY:
				top -= 2;
				index = top[ 1 ];
				arr = ( struct array * ) locals[ insn->local ].string_value;
				arr->integer_values[ index ] = var.integer_value;
				if( arr->string_values ) {
					if( arr->string_values[ index ] ) {
						unref_string( arr->string_values[ index ] );
					}
					arr->string_values[ index ] = NULL;
				}
				if( var.string_value ) {
					var.string_value->reference_count++;
					arr->string_values[ index ] = var.string_value;
				}
				break;
			case AND: top--; *top  &= top[ 1 ]; break;
			case OR : top--; *top  |= top[ 1 ]; break;
			case XOR: top--; *top  ^= top[ 1 ]; break;
			case ADD: top--; *top  += top[ 1 ]; break;
			case SUB: top--; *top  -= top[ 1 ]; break;
			case MUL: top--; *top  *= top[ 1 ]; break;
			case DIV: top--; if( !divide( top, top[ 1 ], vars, insn->expr ) ) return EXCEPTION; break;
			case MOD: top--; if( !modulo( top, top[ 1 ], vars, insn->expr ) ) return EXCEPTION; break;
			case ASL: top--; *top <<= top[ 1 ]; break;
			case ASR: top--; *top >>= top[ 1 ]; break;
			case AND_CONST: *top  &= insn->local; break;
			case OR_CONST : *top  |= insn->local; break;
			case XOR_CONST: *top  ^= insn->local; break;
			case ADD_CONST: *top  += insn->local; break;
			case SUB_CONST: *top  -= insn->local; break;
			case MUL_CONST: *top  *= insn->local; break;
			case DIV_CONST: if( !divide( top, insn->local, vars, insn->expr ) ) return EXCEPTION; break;
			case MOD_CONST: if( !modulo( top, insn->local, vars, insn->expr ) ) return EXCEPTION; break;
			case ASL_CONST: *top <<= insn->local; break;
			case ASR_CONST: *top >>= insn->local; break;
			case AND_LOCAL: local = locals + insn->local; *top  &= local->string_value ? to_int( local ) : local->integer_value; break;
			case OR_LOCAL : local = locals + insn->local; *top  |= local->string_value ? to_int( local ) : local->integer_value; break;
			case XOR_LOCAL: local = locals + insn->local; *top  ^= local->string_value ? to_int( local ) : local->integer_value; break;
			case ADD_LOCAL: local = locals + insn->local; *top  += local->string_value ? to_int( local ) : local->integer_value; break;
			case SUB_LOCAL: local = locals + insn->local; *top  -= local->string_value ? to_int( local ) : local->integer_value; break;
			case MUL_LOCAL: local = locals + insn->local; *top  *= local->string_value ? to_int( local ) : local->integer_value; break;
			case DIV_LOCAL: local = locals + insn->local; if( !divide( top, local->string_value ? to_int( local ) : local->integer_value, vars, insn->expr ) ) return EXCEPTION; break;
			case MOD_LOCAL: local = locals + insn->local; if( !modulo( top, local->string_value ? to_int( local ) : local->integer_value, vars, insn->expr ) ) return EXCEPTION; break;
			case ASL_LOCAL: local = locals + insn->local; *top <<= local->string_value ? to_int( local ) : local->integer_value; break;
			case ASR_LOCAL: local = locals + insn->local; *top >>= local->string_value ? to_int( local ) : local->integer_value; break;
		}
		insn++;
	}
}

static struct statement* combine_arithmetic_statements( struct arithmetic_statement *stmt, struct arithmetic_statement *next, char *message ) {
	struct expression *expr = next->stmt.source;
	while( expr->next ) {
		expr = expr->next;
	}
	expr->next = stmt->stmt.source->next;
	stmt->stmt.source->next = next->stmt.source;
	add_instructions( &stmt->insns, next->insns.list, next->insns.count, message );
	stmt->stmt.next = next->stmt.next;
	dispose_arithmetic_statement( ( struct statement * ) next );
	return &stmt->stmt;
}

static struct arithmetic_statement* new_arithmetic_statement( struct statement *src, struct statement *prev, char *message ) {
	struct arithmetic_statement *arith = calloc( 1, sizeof( struct arithmetic_statement ) );
	if( arith ) {
		arith->stmt.local = src->local;
		arith->stmt.source = src->source;
		arith->stmt.execute = execute_arithmetic_statement;
		arith->stmt.dispose = dispose_arithmetic_statement;
		arith->stmt.next = src->next;
		prev->next = &arith->stmt;
		free( src );
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return arith;
}

static struct statement* optimize_local_assignment( struct statement *stmt, struct statement *prev, char *message ) {
	struct expression *expr = stmt->source;
	struct arithmetic_statement *arith;
	enum arithmetic_op oper = HALT;
	if( expr->evaluate == evaluate_arithmetic_expression || expr->evaluate == evaluate_chr_expression || expr->evaluate == evaluate_integer_literal_expression ) {
		oper = POP_LOCAL;
	} else if( expr->evaluate == evaluate_local || expr->evaluate == evaluate_index_expression ) {
		oper = STORE_LOCAL;
	}
	if( oper ) {
		arith = new_arithmetic_statement( stmt, prev, message );
		if( arith ) {
			stmt = &arith->stmt;
			if( compile_arithmetic_expression( arith, expr, 0, message ) && add_instruction( &arith->insns, oper, arith->stmt.local, expr, message ) ) {
				if( prev && prev->execute == execute_arithmetic_statement ) {
					return combine_arithmetic_statements( ( struct arithmetic_statement * ) prev, arith, message );
				}
			}
		}
	}
	return stmt;
}

static struct statement* optimize_array_assignment( struct statement *stmt, struct statement *prev, char *message ) {
	struct expression *src = stmt->source, *arr = src->next, *idx = arr->next;
	struct arithmetic_statement *arith;
	enum arithmetic_op oper = HALT;
	if( arr->evaluate == evaluate_local ) {
		if( src->evaluate == evaluate_arithmetic_expression || src->evaluate == evaluate_chr_expression || src->evaluate == evaluate_integer_literal_expression ) {
			oper = POP_ARRAY;
		} else if( src->evaluate == evaluate_local || src->evaluate == evaluate_index_expression ) {
			oper = STORE_ARRAY;
		}
		if( oper ) {
			arith = new_arithmetic_statement( stmt, prev, message );
			if( arith ) {
				stmt = &arith->stmt;
				if( compile_arithmetic_expression( arith, idx, 0, message )
				&& add_instruction( &arith->insns, CHECK_ARRAY, arr->index, idx, message )
				&& compile_arithmetic_expression( arith, src, 1, message )
				&& add_instruction( &arith->insns, oper, arr->index, arr, message ) ) {
					if( prev && prev->execute == execute_arithmetic_statement ) {
						return combine_arithmetic_statements( ( struct arithmetic_statement * ) prev, arith, message );
					}
				}
			}
		}
	}
	return stmt;
}

static struct statement* optimize_increment( struct statement *stmt, struct statement *prev, enum arithmetic_op oper, char *message ) {
	if( prev->execute == execute_arithmetic_statement && add_instruction( &( ( struct arithmetic_statement * ) prev )->insns, oper, stmt->local, stmt->source, message ) ) {
		stmt->source->next = prev->source->next;
		prev->source->next = stmt->source;
		prev->next = stmt->next;
		free( stmt );
		return prev;
	}
	return stmt;
}

void optimize_statements( struct statement *prev, char *message ) {
	struct statement *stmt = prev, *next = stmt->next;
	while( next ) {
		if( next->execute == execute_local_assignment ) {
			next = optimize_local_assignment( next, prev, message );
		} else if( next->execute == execute_array_assignment ) {
			next = optimize_array_assignment( next, prev, message );
		} else if( next->execute == execute_increment_statement ) {
			next = optimize_increment( next, prev, INC_LOCAL, message );
		} else if( next->execute == execute_decrement_statement ) {
			next = optimize_increment( next, prev, DEC_LOCAL, message );
		}
		if( message[ 0 ] ) {
			return;
		}
		prev = next;
		next = next->next;
	}
	/*
	while( stmt ) {
		if( stmt->execute == execute_arithmetic_statement ) {
			print_insns( &( ( struct arithmetic_statement * ) stmt )->insns, stmt->source->line );
		}
		stmt = stmt->next;
	}*/
}
