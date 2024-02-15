
#include "stdio.h"
#include "string.h"

#include "towntalk.h"

/* #define PRINT_INSNS */

/*
	Stack-machine for fast integer arithmetic and assignment.
	
	Sequential statements of the following forms are combined into a single subinterpreter statement:
	
		if relational_expr { ... }
		let local = arithmetic_expr;
		let local = $chr( local expr );
		let local = $unpack( global/const expr );
		let local = integer_literal;
		let local = local/global/const;
		let local = [ local expr ];
		let [ local expr ] = arithmetic_expr;
		let [ local expr ] = $chr( local expr );
		let [ local expr ] = $unpack( global/const expr );
		let [ local expr ] = integer_literal;
		let [ local expr ] = local/global/const;
		let [ local expr ] = [ local expr ];
		inc local;
		dec local;
	
	Best performance is achieved when used in loops of the following forms:
	
		while relational_expr( local local ) ...
		while relational_expr( local global ) ...
		while relational_expr( local integer_literal ) ...
	
	Relational expressions are one of "<", "<e", "=", "<>", ">e", or ">".
*/

enum result evaluate_arithmetic_expression( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_chr_expression( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_unpack_expression( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_index_expression( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_global( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_local( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_local_post_inc( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_local_post_dec( struct expression *this, struct variables *vars, struct variable *result );
enum result execute_array_assignment( struct statement *this, struct variables *vars, struct variable *result );
enum result execute_local_assignment( struct statement *this, struct variables *vars, struct variable *result );
enum result execute_increment_statement( struct statement *this, struct variables *vars, struct variable *result );
enum result execute_decrement_statement( struct statement *this, struct variables *vars, struct variable *result );
enum result execute_if_statement( struct statement *this, struct variables *vars, struct variable *result );
enum result execute_statements( struct statement *stmt, struct variables *vars, struct variable *result );
enum result to_int( struct variable *var, int *result, struct variables *vars, struct expression *source );
void dispose_statements( struct statement *statements );

enum arithmetic_op {
	HALT, IF, PUSH_CONST, PUSH_LOCAL, LOAD_LOCAL, PUSH_GLOBAL, LOAD_GLOBAL,
	INC_LOCAL, PUSH_LOCAL_PI, DEC_LOCAL, PUSH_LOCAL_PD, ASSIGN_EXPR,
	PUSH_EXPR, LOAD_EXPR, PUSH_ARRAY, LOAD_ARRAY, PUSH_STRING, PUSH_UNPACK,
	POP_LOCAL, STORE_LOCAL, CHECK_ARRAY, POP_ARRAY, STORE_ARRAY,
	AND_STACK, OR__STACK, XOR_STACK, ADD_STACK, SUB_STACK,
	MUL_STACK, DIV_STACK, MOD_STACK, ASL_STACK, ASR_STACK,
	NE__STACK, LT__STACK, LTE_STACK, EQ__STACK, GTE_STACK, GT__STACK,
	AND_CONST, OR__CONST, XOR_CONST, ADD_CONST, SUB_CONST,
	MUL_CONST, DIV_CONST, MOD_CONST, ASL_CONST, ASR_CONST,
	NE__CONST, LT__CONST, LTE_CONST, EQ__CONST, GTE_CONST, GT__CONST,
	AND_LOCAL, OR__LOCAL, XOR_LOCAL, ADD_LOCAL, SUB_LOCAL,
	MUL_LOCAL, DIV_LOCAL, MOD_LOCAL, ASL_LOCAL, ASR_LOCAL,
	NE__LOCAL, LT__LOCAL, LTE_LOCAL, EQ__LOCAL, GTE_LOCAL, GT__LOCAL
};

#if defined( PRINT_INSNS )
static char* arithmetic_ops[] = {
	"HALT", "IF", "PUSH_CONST", "PUSH_LOCAL", "LOAD_LOCAL", "PUSH_GLOBAL", "LOAD_GLOBAL",
	"INC_LOCAL", "PUSH_LOCAL_PI", "DEC_LOCAL", "PUSH_LOCAL_PD", "ASSIGN_EXPR",
	"PUSH_EXPR", "LOAD_EXPR", "PUSH_ARRAY", "LOAD_ARRAY", "PUSH_STRING", "PUSH_UNPACK",
	"POP_LOCAL", "STORE_LOCAL", "CHECK_ARRAY", "POP_ARRAY", "STORE_ARRAY",
	"AND_STACK", "OR__STACK", "XOR_STACK", "ADD_STACK", "SUB_STACK",
	"MUL_STACK", "DIV_STACK", "MOD_STACK", "ASL_STACK", "ASR_STACK",
	"NE__STACK", "LT__STACK", "LTE_STACK", "EQ__STACK", "GTE_STACK", "GT__STACK",
	"AND_CONST", "OR__CONST", "XOR_CONST", "ADD_CONST", "SUB_CONST",
	"MUL_CONST", "DIV_CONST", "MOD_CONST", "ASL_CONST", "ASR_CONST",
	"NE__CONST", "LT__CONST", "LTE_CONST", "EQ__CONST", "GTE_CONST", "GT__CONST",
	"AND_LOCAL", "OR__LOCAL", "XOR_LOCAL", "ADD_LOCAL", "SUB_LOCAL",
	"MUL_LOCAL", "DIV_LOCAL", "MOD_LOCAL", "ASL_LOCAL", "ASR_LOCAL",
	"NE__LOCAL", "LT__LOCAL", "LTE_LOCAL", "EQ__LOCAL", "GTE_LOCAL", "GT__LOCAL"
};
#endif

struct instruction {
	int oper, local;
	struct expression *expr;
};

struct instructions {
	struct instruction *list;
	int capacity, count;
};

struct blocks {
	struct statement **list;
	int capacity, count;
};

struct arithmetic_statement {
	struct statement stmt;
	struct instructions insns;
	struct blocks blocs;
};

static struct instruction* add_instruction( struct instructions *insns,
	enum arithmetic_op oper, int local, struct expression *expr, char *message ) {
	struct instruction *list;
	int capacity = insns->capacity;
	if( capacity <= insns->count ) {
		capacity = ( capacity << 1 ) + 4;
		list = calloc( capacity + 1, sizeof( struct instruction ) );
		if( list ) {
			memcpy( list, insns->list, insns->count * sizeof( struct instruction ) );
			free( insns->list );
			insns->list = list;
			insns->capacity = capacity;
		} else {
			strcpy( message, OUT_OF_MEMORY );
			return NULL;
		}
	}
	list = &insns->list[ insns->count++ ];
	list->oper = oper;
	list->local = local;
	list->expr = expr;
	return list;
}

static int add_block( struct blocks *blocs, struct statement *src, char *message ) {
	struct statement **list;
	int capacity = blocs->capacity;
	if( capacity <= blocs->count ) {
		capacity = ( capacity << 1 ) + 2;
		list = calloc( capacity, sizeof( struct statement * ) );
		if( list ) {
			memcpy( list, blocs->list, blocs->count * sizeof( struct statement * ) );
			free( blocs->list );
			blocs->list = list;
			blocs->capacity = capacity;
		} else {
			strcpy( message, OUT_OF_MEMORY );
			return 0;
		}
	}
	blocs->list[ blocs->count++ ] = src;
	return blocs->count;
}

static enum arithmetic_op get_arithmetic_op( struct expression *expr ) {
	char *chr;
	const char *OPS = "&:3+-*/%12!<(=)>";
	enum arithmetic_op oper = HALT;
	if( expr->evaluate == evaluate_arithmetic_expression ) {
		chr = strchr( OPS, expr->index );
		if( chr ) {
			oper = AND_STACK + ( chr - OPS );
		}
	}
	return oper;
}

static struct instruction* compile_arithmetic_expression( struct arithmetic_statement *stmt, struct expression *expr, int top, char *message ) {
	struct instruction *insn;
	struct expression *parameter;
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
					insn->oper = oper + AND_CONST - AND_STACK;
				} else if( insn->oper == PUSH_LOCAL ) {
					insn->oper = oper + AND_LOCAL - AND_STACK;
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
	} else if( expr->evaluate == evaluate_unpack_expression && expr->parameters->evaluate == evaluate_global ) {
		insn = compile_arithmetic_expression( stmt, expr->parameters->next, top, message );
		if( insn ) {
			insn = add_instruction( &stmt->insns, PUSH_UNPACK, 0, expr->parameters, message );
		}
	} else if( expr->evaluate == evaluate_global ) {
		insn = add_instruction( &stmt->insns, PUSH_GLOBAL, 0, expr, message );
	} else {
		insn = add_instruction( &stmt->insns, PUSH_EXPR, 0, expr, message );
	}
	return insn;
}

static void dispose_arithmetic_statement( struct statement *this ) {
	struct arithmetic_statement *stmt = ( struct arithmetic_statement * ) this;
	int idx = 0;
	while( idx < stmt->blocs.count ) {
		dispose_statements( stmt->blocs.list[ idx++ ] );
	}
	free( stmt->blocs.list );
	free( stmt->insns.list );
	free( this );
}

static enum result divide( int *lhs, int rhs, struct variables *vars, struct expression *src ) {
	if( rhs == 0 ) {
		return throw( vars, src, 0, "Integer division by zero." );
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
	enum result ret;
	char *chr;
	while( 1 ) {
		switch( insn->oper ) {
			case HALT:
				return OKAY;
			case IF:
				if( *top-- ) {
					ret = execute_statements( ( ( struct arithmetic_statement * ) this )->blocs.list[ insn->local ], vars, result );
					if( ret != OKAY ) {
						return ret;
					}
				}
				break;
			case PUSH_CONST:
				*++top = insn->local;
				break;
			case PUSH_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( !to_int( local, ++top, vars, insn->expr ) ) {
						return EXCEPTION;
					}
				} else {
					*++top = local->integer_value;
				}
				break;
			case LOAD_LOCAL:
				local = locals + insn->local;
				var.integer_value = local->integer_value;
				var.string_value = local->string_value;
				if( var.string_value ) {
					var.string_value->reference_count++;
				}
				break;
			case PUSH_GLOBAL:
				local = &( ( struct global_variable * ) ( ( struct string_expression * ) insn->expr )->str )->value;
				if( local->string_value ) {
					if( !to_int( local, ++top, vars, insn->expr ) ) {
						return EXCEPTION;
					}
				} else {
					*++top = local->integer_value;
				}
				break;
			case LOAD_GLOBAL:
				local = &( ( struct global_variable * ) ( ( struct string_expression * ) insn->expr )->str )->value;
				var.integer_value = local->integer_value;
				var.string_value = local->string_value;
				if( var.string_value ) {
					var.string_value->reference_count++;
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
			case ASSIGN_EXPR:
				var.integer_value = 0;
				var.string_value = NULL;
				if( !insn->expr->evaluate( insn->expr, vars, &var ) ) {
					return EXCEPTION;
				}
				local = locals + insn->local;
				local->integer_value = var.integer_value;
				if( local->string_value ) {
					unref_string( local->string_value );
				}
				local->string_value = var.string_value;
				break;
			case PUSH_EXPR:
				var.integer_value = 0;
				var.string_value = NULL;
				if( insn->expr->evaluate( insn->expr, vars, &var ) ) {
					if( var.string_value ) {
						if( !to_int( &var, ++top, vars, insn->expr ) ) {
							unref_string( var.string_value );
							return EXCEPTION;
						}
						unref_string( var.string_value );
					} else {
						*++top = var.integer_value;
					}
				} else {
					return EXCEPTION;
				}
				break;
			case LOAD_EXPR:
				var.integer_value = 0;
				var.string_value = NULL;
				if( !insn->expr->evaluate( insn->expr, vars, &var ) ) {
					return EXCEPTION;
				}
				break;
			case PUSH_ARRAY:
				local = locals + insn->local;
				if( local->string_value && local->string_value->type == ARRAY ) {
					index = *top;
					arr = ( struct array * ) local->string_value;
					if( ( unsigned int ) index < ( unsigned int ) arr->length ) {
						if( arr->string_values && arr->string_values[ index ] ) {
							var.integer_value = arr->integer_values[ index ];
							var.string_value = arr->string_values[ index ];
							if( !to_int( &var, top, vars, insn->expr ) ) {
								return EXCEPTION;
							}
						} else {
							*top = arr->integer_values[ index ];
						}
					} else {
						return throw( vars, insn->expr, index, "Array index out of bounds." );
					}
				} else {
					return throw( vars, insn->expr, 0, "Not an array." );
				}
				break;
			case LOAD_ARRAY:
				local = locals + insn->local;
				if( local->string_value && local->string_value->type == ARRAY ) {
					index = *top--;
					arr = ( struct array * ) local->string_value;
					if( ( unsigned int ) index < ( unsigned int ) arr->length ) {
						var.integer_value = arr->integer_values[ index ];
						if( arr->string_values && arr->string_values[ index ] ) {
							var.string_value = arr->string_values[ index ];
							var.string_value->reference_count++;
						} else {
							var.string_value = NULL;
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
			case PUSH_UNPACK:
				local = &( ( struct global_variable * ) ( ( struct string_expression * ) insn->expr )->str )->value;
				if( local->string_value ) {
					index = *top;
					if( ( unsigned int ) index < ( unsigned int ) local->string_value->length >> 2 ) {
						index <<= 2;
						chr = local->string_value->string;
						*top = ( ( signed char ) chr[ index ] << 24 ) | ( ( unsigned char ) chr[ index + 1 ] << 16 )
							| ( ( unsigned char ) chr[ index + 2 ] << 8 ) | ( unsigned char ) chr[ index + 3 ];
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
				}
				local->string_value = var.string_value;
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
				index = *top--;
				arr = ( struct array * ) locals[ insn->local ].string_value;
				if( arr->string_values ) {
					if( arr->string_values[ index ] ) {
						unref_string( arr->string_values[ index ] );
					}
					arr->integer_values[ index ] = var.integer_value;
					arr->string_values[ index ] = var.string_value;
				} else if( var.string_value ) {
					if( !to_int( &var, &arr->integer_values[ index ], vars, insn->expr ) ) {
						unref_string( var.string_value );
						return EXCEPTION;
					}
					unref_string( var.string_value );
				} else {
					arr->integer_values[ index ] = var.integer_value;
				}
				break;
			case AND_STACK: top--; *top  &= top[ 1 ]; break;
			case OR__STACK: top--; *top  |= top[ 1 ]; break;
			case XOR_STACK: top--; *top  ^= top[ 1 ]; break;
			case ADD_STACK: top--; *top  += top[ 1 ]; break;
			case SUB_STACK: top--; *top  -= top[ 1 ]; break;
			case MUL_STACK: top--; *top  *= top[ 1 ]; break;
			case DIV_STACK: top--; if( !divide( top, top[ 1 ], vars, insn->expr ) ) return EXCEPTION; break;
			case MOD_STACK: top--; if( !modulo( top, top[ 1 ], vars, insn->expr ) ) return EXCEPTION; break;
			case ASL_STACK: top--; *top <<= top[ 1 ]; break;
			case ASR_STACK: top--; *top >>= top[ 1 ]; break;
			case NE__STACK: top--; *top = *top != top[ 1 ]; break;
			case LT__STACK: top--; *top = *top <  top[ 1 ]; break;
			case LTE_STACK: top--; *top = *top <= top[ 1 ]; break;
			case EQ__STACK: top--; *top = *top == top[ 1 ]; break;
			case GTE_STACK: top--; *top = *top >= top[ 1 ]; break;
			case GT__STACK: top--; *top = *top >  top[ 1 ]; break;
			case AND_CONST: *top  &= insn->local; break;
			case OR__CONST: *top  |= insn->local; break;
			case XOR_CONST: *top  ^= insn->local; break;
			case ADD_CONST: *top  += insn->local; break;
			case SUB_CONST: *top  -= insn->local; break;
			case MUL_CONST: *top  *= insn->local; break;
			case DIV_CONST: if( !divide( top, insn->local, vars, insn->expr ) ) return EXCEPTION; break;
			case MOD_CONST: if( !modulo( top, insn->local, vars, insn->expr ) ) return EXCEPTION; break;
			case ASL_CONST: *top <<= insn->local; break;
			case ASR_CONST: *top >>= insn->local; break;
			case NE__CONST: *top = *top != insn->local; break;
			case LT__CONST: *top = *top <  insn->local; break;
			case LTE_CONST: *top = *top <= insn->local; break;
			case EQ__CONST: *top = *top == insn->local; break;
			case GTE_CONST: *top = *top >= insn->local; break;
			case GT__CONST: *top = *top >  insn->local; break;
			case AND_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top &= index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top &= local->integer_value;
				}
				break;
			case OR__LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top |= index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top |= local->integer_value;
				}
				break;
			case XOR_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top ^= index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top ^= local->integer_value;
				}
				break;
			case ADD_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top += index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top += local->integer_value;
				}
				break;
			case SUB_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top -= index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top -= local->integer_value;
				}
				break;
			case MUL_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top *= index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top *= local->integer_value;
				}
				break;
			case DIV_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( !to_int( local, &index, vars, insn->expr ) ) {
						return EXCEPTION;
					}
				} else {
					index = local->integer_value;
				}
				if( !divide( top, index, vars, insn->expr ) ) {
					return EXCEPTION;
				}
				break;
			case MOD_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( !to_int( local, &index, vars, insn->expr ) ) {
						return EXCEPTION;
					}
				} else {
					index = local->integer_value;
				}
				if( !modulo( top, index, vars, insn->expr ) ) {
					return EXCEPTION;
				}
				break;
			case ASL_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top <<= index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top <<= local->integer_value;
				}
				break;
			case ASR_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top >>= index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top >>= local->integer_value;
				}
				break;
			case NE__LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top = *top != index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = *top != local->integer_value;
				}
				break;
			case LT__LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top = *top < index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = *top < local->integer_value;
				}
				break;
			case LTE_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top = *top <= index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = *top <= local->integer_value;
				}
				break;
			case EQ__LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top = *top == index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = *top == local->integer_value;
				}
				break;
			case GTE_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top = *top >= index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = *top >= local->integer_value;
				}
				break;
			case GT__LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_int( local, &index, vars, insn->expr ) ) {
						*top = *top > index;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = *top > local->integer_value;
				}
				break;
		}
		insn++;
	}
}

/* Replace the source statement with a new arithmetic statement (combined with the previous statement if possible). */
static struct arithmetic_statement *add_arithmetic_statement( struct statement *src, struct statement *prev, char *message ) {
	struct expression *expr;
	struct arithmetic_statement *arith;
	if( prev->execute == execute_arithmetic_statement ) {
		arith = ( struct arithmetic_statement * ) prev;
		expr = src->source;
		while( expr->next ) {
			expr = expr->next;
		}
		expr->next = arith->stmt.source;
		arith->stmt.source = src->source;
		src->source = NULL;
		prev->next = src->next;
	} else {
		arith = calloc( 1, sizeof( struct arithmetic_statement ) );
		if( arith ) {
			arith->stmt.local = src->local;
			arith->stmt.source = src->source;
			src->source = NULL;
			arith->stmt.execute = execute_arithmetic_statement;
			arith->stmt.dispose = dispose_arithmetic_statement;
			arith->stmt.next = src->next;
			prev->next = &arith->stmt;
		} else {
			strcpy( message, OUT_OF_MEMORY );
			return NULL;
		}
	}
	if( src->dispose == dispose_block_statement ) {
		if( add_block( &arith->blocs, ( ( struct block_statement * ) src )->if_block, message ) ) {
			( ( struct block_statement * ) src )->if_block = NULL;
		} else {
			arith = NULL;
		}
	}
	src->next = NULL;
	dispose_statements( src );
	return arith;
}

static struct statement* optimize_local_assignment( struct statement *stmt, struct statement *prev, char *message ) {
	struct expression *expr = stmt->source;
	struct instruction *insn;
	int local = stmt->local, oper, dest = POP_LOCAL;
	struct arithmetic_statement *arith = add_arithmetic_statement( stmt, prev, message );
	if( arith ) {
		stmt = &arith->stmt;
		if( compile_arithmetic_expression( arith, expr, 0, message ) ) {
			insn = &arith->insns.list[ arith->insns.count - 1 ];
			oper = insn->oper;
			if( oper == PUSH_EXPR ) {
				insn->oper = ASSIGN_EXPR;
				insn->local = local;
			} else {
				if( oper == PUSH_LOCAL || oper == PUSH_GLOBAL || oper == PUSH_ARRAY ) {
					dest = STORE_LOCAL;
					insn->oper++;
				}
				add_instruction( &arith->insns, dest, local, expr, message );
			}
		}
	}
	return stmt;
}

static struct statement* optimize_array_assignment( struct statement *stmt, struct statement *prev, char *message ) {
	struct expression *src = stmt->source, *arr = src->next, *idx = arr->next;
	struct arithmetic_statement *arith;
	int *oper, dest = POP_ARRAY;
	if( arr->evaluate == evaluate_local ) {
		arith = add_arithmetic_statement( stmt, prev, message );
		if( arith ) {
			stmt = &arith->stmt;
			if( compile_arithmetic_expression( arith, idx, 0, message )
			&& add_instruction( &arith->insns, CHECK_ARRAY, arr->index, idx, message )
			&& compile_arithmetic_expression( arith, src, 1, message ) ) {
				oper = &arith->insns.list[ arith->insns.count - 1 ].oper;
				if( *oper == PUSH_LOCAL || *oper == PUSH_GLOBAL || *oper == PUSH_EXPR || *oper == PUSH_ARRAY ) {
					dest = STORE_ARRAY;
					(*oper)++;
				}
				add_instruction( &arith->insns, dest, arr->index, arr, message );
			}
		}
	}
	return stmt;
}

static struct statement* optimize_increment( struct statement *stmt, struct statement *prev, enum arithmetic_op oper, char *message ) {
	if( prev->execute == execute_arithmetic_statement && add_instruction( &( ( struct arithmetic_statement * ) prev )->insns, oper, stmt->local, stmt->source, message ) ) {
		stmt->source->next = prev->source;
		prev->source = stmt->source;
		prev->next = stmt->next;
		free( stmt );
		return prev;
	}
	return stmt;
}

static struct statement* optimize_if( struct statement *stmt, struct statement *prev, char *message ) {
	struct expression *expr = stmt->source;
	struct arithmetic_statement *arith;
	struct block_statement *if_stmt = ( struct block_statement * ) stmt;
	if( stmt->source->evaluate == evaluate_arithmetic_expression && if_stmt->else_block == NULL ) {
		arith = add_arithmetic_statement( stmt, prev, message );
		if( arith ) {
			stmt = &arith->stmt;
			if( compile_arithmetic_expression( arith, expr, 0, message ) ) {
				add_instruction( &arith->insns, IF, arith->blocs.count - 1, expr, message );
			}
		}
	}
	return stmt;
}

#if defined( PRINT_INSNS )
static void print_insns( struct function *func, struct arithmetic_statement *stmt, int line ) {
	char *name;
	struct instruction *insn = stmt->insns.list;
	fprintf( stderr, "Compiled from function '%s' on line %d of '%s':\n", func->str.string, func->line, func->file->string );
	while( insn->oper ) {
		if( insn->oper < HALT || insn->oper > GT__LOCAL ) {
			name = "XXX";
		} else {
			name = arithmetic_ops[ insn->oper ];
		}
		fprintf( stderr, "% 6d %s %d:\n", insn->expr->line, name, insn->local );
		insn++;
	}
}
#endif

void optimize_statements( struct function *func, struct statement *prev, char *message ) {
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
		} else if( next->execute == execute_if_statement ) {
			next = optimize_if( next, prev, message );
		}
		if( message[ 0 ] ) {
			return;
		}
		prev = next;
		next = next->next;
	}
#if defined( PRINT_INSNS )
	while( stmt ) {
		if( stmt->execute == execute_arithmetic_statement ) {
			print_insns( func, ( struct arithmetic_statement * ) stmt, stmt->source->line );
		}
		stmt = stmt->next;
	}
#endif
}
