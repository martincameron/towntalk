
#include "stddef.h"
#include "stdio.h"
#include "string.h"

#include "towntalk.h"

/* #define PRINT_OPTIMIZATIONS */

/*
	Stack-machine for fast arithmetic and assignment.
	
	Sequential statements of the following forms are combined into a single subinterpreter statement:
	
		call expr;
		return arithmetic_expr;
		let local = arithmetic_expr;
		let local = $chr( local expr );
		let local = $unpack( global/const expr );
		let local = number_literal;
		let local = local/global/const;
		let local = [ local expr ];
		let [ local expr ] = arithmetic_expr;
		let [ local expr ] = $chr( local expr );
		let [ local expr ] = $unpack( global/const expr );
		let [ local expr ] = number_literal;
		let [ local expr ] = local/global/const;
		let [ local expr ] = [ local expr ];
		inc local;
		dec local;
	
	Best performance is achieved when used in loops of the following forms:
	
		while relational_expression( local local ) { ... }
		while relational_expression( local global ) { ... }
		while relational_expression( local number_literal ) { ... }
	
	A relational expression is one of "<", "<e", "=", "<>", ">e", or ">".
*/

/* Externals. */
enum result evaluate_number_literal_expression( struct expression *this, struct variables *vars, struct variable *result );
enum result evaluate_string_literal_expression( struct expression *this, struct variables *vars, struct variable *result );
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
enum result execute_return_statement( struct statement *this, struct variables *vars, struct variable *result );
enum result execute_call_statement( struct statement *this, struct variables *vars, struct variable *result );
enum result execute_while_statement( struct statement *this, struct variables *vars, struct variable *result );
enum result execute_if_statement( struct statement *this, struct variables *vars, struct variable *result );
enum result to_int( struct variable *var, int *result, struct variables *vars, struct expression *source );
enum result to_num( struct variable *var, number *result, struct variables *vars, struct expression *source );
void dispose_statements( struct statement *statements );

static struct expression* optimize_expression( struct expression *expr, struct expression *prev, char *message );
static struct expression* optimize_parameters( struct expression *expr, char *message );

enum arithmetic_op {
	HALT, PUSH_CONST, PUSH_LOCAL, LOAD_LOCAL, PUSH_GLOBAL, LOAD_GLOBAL,
	INC_LOCAL, PUSH_LOCAL_PI, DEC_LOCAL, PUSH_LOCAL_PD, ASSIGN_EXPR, CALL_EXPR,
	PUSH_EXPR, LOAD_EXPR, PUSH_ARRAY, LOAD_ARRAY, PUSH_STRING, PUSH_UNPACK,
	SAVE_LOCAL, POP_LOCAL, STORE_LOCAL, CHECK_ARRAY, POP_ARRAY,
	STORE_ARRAY, POP_RETURN, STORE_RETURN, POP_RESULT, STORE_RESULT,
	AND_STACK, OR_STACK,  XOR_STACK, ADD_STACK, SUB_STACK,
	MUL_STACK, FDI_STACK, DIV_STACK, MOD_STACK, ASL_STACK, ASR_STACK,
	LT_STACK,  LE_STACK,  EQ_STACK,  GE_STACK,  GT_STACK,  NE_STACK,
	AND_CONST, OR_CONST,  XOR_CONST, ADD_CONST, SUB_CONST,
	MUL_CONST, FDI_CONST, DIV_CONST, MOD_CONST, ASL_CONST, ASR_CONST,
	LT_CONST,  LE_CONST,  EQ_CONST,  GE_CONST,  GT_CONST,  NE_CONST,
	AND_LOCAL, OR_LOCAL,  XOR_LOCAL, ADD_LOCAL, SUB_LOCAL,
	MUL_LOCAL, FDI_LOCAL, DIV_LOCAL, MOD_LOCAL, ASL_LOCAL, ASR_LOCAL,
	LT_LOCAL,  LE_LOCAL,  EQ_LOCAL,  GE_LOCAL,  GT_LOCAL,  NE_LOCAL,
	NOP
};

enum compilable_stmt {
	STMT_NOT_COMPILABLE,
	LOCAL_ASSIGNMENT,
	ARRAY_ASSIGNMENT,
	LOCAL_INCREMENT,
	LOCAL_DECREMENT,
	CALL_STATEMENT,
	RETURN_STATEMENT
};

enum compilable_expr {
	EXPR_NOT_COMPILABLE,
	NUMBER_LITERAL,
	LOCAL_VARIABLE,
	LOCAL_VARIABLE_PI,
	LOCAL_VARIABLE_PD,
	GLOBAL_VARIABLE,
	ARITHMETIC_OPERATOR,
	ARRAY_ELEMENT,
	STRING_CHARACTER,
	STRING_UNPACK
};

#if defined( PRINT_OPTIMIZATIONS )
static char* arithmetic_ops[] = {
	"HALT", "PUSH_CONST", "PUSH_LOCAL", "LOAD_LOCAL", "PUSH_GLOBAL", "LOAD_GLOBAL",
	"INC_LOCAL", "PUSH_LOCAL_PI", "DEC_LOCAL", "PUSH_LOCAL_PD", "ASSIGN_EXPR", "CALL_EXPR",
	"PUSH_EXPR", "LOAD_EXPR", "PUSH_ARRAY", "LOAD_ARRAY", "PUSH_STRING", "PUSH_UNPACK",
	"SAVE_LOCAL", "POP_LOCAL", "STORE_LOCAL", "CHECK_ARRAY", "POP_ARRAY",
	"STORE_ARRAY", "POP_RETURN", "STORE_RETURN", "POP_RESULT", "STORE_RESULT",
	"AND_STACK", "OR_STACK",  "XOR_STACK", "ADD_STACK", "SUB_STACK",
	"MUL_STACK", "FDI_STACK", "DIV_STACK", "MOD_STACK", "ASL_STACK", "ASR_STACK",
	"LT_STACK",  "LE_STACK",  "EQ_STACK",  "GE_STACK",  "GT_STACK",  "NE_STACK",
	"AND_CONST", "OR_CONST",  "XOR_CONST", "ADD_CONST", "SUB_CONST",
	"MUL_CONST", "FDI_CONST", "DIV_CONST", "MOD_CONST", "ASL_CONST", "ASR_CONST",
	"LT_CONST",  "LE_CONST",  "EQ_CONST",  "GE_CONST",  "GT_CONST",  "NE_CONST",
	"AND_LOCAL", "OR_LOCAL",  "XOR_LOCAL", "ADD_LOCAL", "SUB_LOCAL",
	"MUL_LOCAL", "FDI_LOCAL", "DIV_LOCAL", "MOD_LOCAL", "ASL_LOCAL", "ASR_LOCAL",
	"LT_LOCAL",  "LE_LOCAL",  "EQ_LOCAL",  "GE_LOCAL",  "GT_LOCAL",  "NE_LOCAL"
};
#endif

struct instruction {
	int oper, local;
	struct expression *expr;
	number value;
};

struct instructions {
	struct instruction *list, initial[ 5 ];
	int capacity, count;
};

struct blocks {
	struct statement **list;
	int capacity, count;
};

struct arithmetic_statement {
	struct statement stmt;
	struct instructions insns;
};

struct arithmetic_statement_expr {
	struct expression expr;
	struct arithmetic_statement stmt;
};

static enum compilable_stmt can_compile_stmt( struct statement *stmt ) {
	if( stmt ) {
		if( stmt->execute == execute_local_assignment ) {
			return LOCAL_ASSIGNMENT;
		} else if( stmt->execute == execute_array_assignment ) {
			if( stmt->source->next->evaluate == evaluate_local ) {
				return ARRAY_ASSIGNMENT;
			}
		} else if( stmt->execute == execute_increment_statement ) {
			return LOCAL_INCREMENT;
		} else if( stmt->execute == execute_decrement_statement ) {
			return LOCAL_DECREMENT;
		} else if( stmt->execute == execute_call_statement ) {
			return CALL_STATEMENT;
		} else if( stmt->execute == execute_return_statement ) {
			return RETURN_STATEMENT;
		}
	}
	return STMT_NOT_COMPILABLE;
}

static enum arithmetic_op get_arithmetic_op( struct expression *expr ) {
	char *chr;
	const char *OPS = "&:3+-*0/%12<(=)>!";
	enum arithmetic_op oper = HALT;
	if( expr->evaluate == evaluate_arithmetic_expression ) {
		chr = strchr( OPS, expr->index );
		if( chr ) {
			oper = AND_STACK + ( chr - OPS );
		}
	}
	return oper;
}

static enum compilable_expr can_compile_expr( struct expression *expr ) {
	if( get_arithmetic_op( expr ) ) {
		return ARITHMETIC_OPERATOR;
	} else if( expr->evaluate == evaluate_number_literal_expression ) {
		return NUMBER_LITERAL;
	} else if( expr->evaluate == evaluate_local ) {
		return LOCAL_VARIABLE;
	} else if( expr->evaluate == evaluate_local_post_inc ) {
		return LOCAL_VARIABLE_PI;
	} else if( expr->evaluate == evaluate_local_post_dec ) {
		return LOCAL_VARIABLE_PD;
	} else if( expr->evaluate == evaluate_index_expression ) {
		if( expr->parameters->evaluate == evaluate_local ) {
			return ARRAY_ELEMENT;
		}
	} else if( expr->evaluate == evaluate_chr_expression ) {
		if( expr->parameters->evaluate == evaluate_local ) {
			return STRING_CHARACTER;
		}
	} else if( expr->evaluate == evaluate_unpack_expression ) {
		if( expr->parameters->evaluate == evaluate_global ) {
			return STRING_UNPACK;
		}
	} else if( expr->evaluate == evaluate_global ) {
		return GLOBAL_VARIABLE;
	}
	return EXPR_NOT_COMPILABLE;
}

static struct instruction* add_instruction( struct instructions *insns,
	enum arithmetic_op oper, int local, struct expression *expr, char *message ) {
	struct instruction *list;
	int capacity = insns->capacity;
	if( capacity < 1 ) {
		insns->capacity = capacity = 4;
		insns->list = insns->initial;
	}
	if( capacity <= insns->count ) {
		capacity = capacity << 1;
		list = calloc( capacity + 1, sizeof( struct instruction ) );
		if( list ) {
			memcpy( list, insns->list, insns->count * sizeof( struct instruction ) );
			if( insns->list != insns->initial ) {
				free( insns->list );
			}
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

static struct instruction* compile_expression( struct arithmetic_statement *stmt, struct expression *expr, int top, char *message ) {
	struct instruction *insn;
	struct expression *parameter;
	enum arithmetic_op oper;
	if( top > 6 ) {
		insn = add_instruction( &stmt->insns, PUSH_EXPR, 0, expr, message );
		if( !optimize_parameters( expr, message ) ) {
			insn = NULL;
		}
	} else switch( can_compile_expr( expr ) ) {
		case ARITHMETIC_OPERATOR:
			parameter = expr->parameters;
			insn = compile_expression( stmt, parameter, top, message );
			while( insn && parameter->next ) {
				parameter = parameter->next;
				insn = compile_expression( stmt, parameter, top + 1, message );
				if( insn ) {
					oper = get_arithmetic_op( expr );
					if( insn->oper == PUSH_CONST ) {
						insn->oper = oper + AND_CONST - AND_STACK;
					} else if( insn->oper == PUSH_LOCAL ) {
						insn->oper = oper + AND_LOCAL - AND_STACK;
					} else {
						insn = add_instruction( &stmt->insns, oper, 0, parameter, message );
					}
				}
			}
			break;
		case NUMBER_LITERAL:
			insn = add_instruction( &stmt->insns, PUSH_CONST, expr->index, expr, message );
			if( insn ) {
				insn->value = ( ( struct value_expression * ) expr )->num;
			}
			break;
		case LOCAL_VARIABLE:
			if( stmt->insns.count > 1 ) {
				insn = &stmt->insns.list[ stmt->insns.count - 1 ];
				if( insn->oper == POP_LOCAL && insn->local == expr->index ) {
					insn->oper = SAVE_LOCAL;
					break;
				}
			}
			insn = add_instruction( &stmt->insns, PUSH_LOCAL, expr->index, expr, message );
			break;
		case LOCAL_VARIABLE_PI:
			insn = add_instruction( &stmt->insns, PUSH_LOCAL_PI, expr->index, expr, message );
			break;
		case LOCAL_VARIABLE_PD:
			insn = add_instruction( &stmt->insns, PUSH_LOCAL_PD, expr->index, expr, message );
			break;
		case ARRAY_ELEMENT:
			insn = compile_expression( stmt, expr->parameters->next, top, message );
			if( insn ) {
				insn = add_instruction( &stmt->insns, PUSH_ARRAY, expr->parameters->index, expr, message );
			}
			break;
		case STRING_CHARACTER:
			insn = compile_expression( stmt, expr->parameters->next, top, message );
			if( insn ) {
				insn = add_instruction( &stmt->insns, PUSH_STRING, expr->parameters->index, expr, message );
			}
			break;
		case STRING_UNPACK:
			insn = compile_expression( stmt, expr->parameters->next, top, message );
			if( insn ) {
				insn = add_instruction( &stmt->insns, PUSH_UNPACK, 0, expr->parameters, message );
			}
			break;
		case GLOBAL_VARIABLE:
			insn = add_instruction( &stmt->insns, PUSH_GLOBAL, 0, expr, message );
			break;
		default:
			insn = add_instruction( &stmt->insns, PUSH_EXPR, 0, expr, message );
			if( !optimize_parameters( expr, message ) ) {
				insn = NULL;
			}
			break;
	}
	return insn;
}

static void dispose_arithmetic_statement( struct statement *this ) {
	struct arithmetic_statement *stmt = ( struct arithmetic_statement * ) this;
	if( stmt->insns.list != stmt->insns.initial ) {
		free( stmt->insns.list );
	}
	free( this );
}

static enum result divide( number *lhs, number rhs, struct variables *vars, struct expression *src ) {
	if( rhs == 0 ) {
		return throw( vars, src, 0, "Integer division by zero." );
	}
	*lhs = ( long_int ) *lhs / ( long_int ) rhs;
	return OKAY;
}

static enum result modulo( number *lhs, number rhs, struct variables *vars, struct expression *src ) {
	if( rhs == 0 ) {
		return throw( vars, src, 0, "Modulo division by zero." );
	}
	*lhs = ( long_int ) *lhs % ( long_int ) rhs;
	return OKAY;
}

static enum result execute_arithmetic_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct instruction *insn = ( ( struct arithmetic_statement * ) this )->insns.list;
	number stack[ 9 ], *top = stack, value;
	struct variable var, *locals = vars->locals, *local;
	struct array *arr;
	int index;
	char *chr;
	while( 1 ) {
		switch( insn->oper ) {
			case HALT:
				return OKAY;
			case PUSH_CONST:
				*++top = insn->value;
				break;
			case PUSH_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( !to_num( local, ++top, vars, insn->expr ) ) {
						return EXCEPTION;
					}
				} else {
					*++top = local->number_value;
				}
				break;
			case LOAD_LOCAL:
				local = locals + insn->local;
				var.number_value = local->number_value;
				var.string_value = local->string_value;
				if( var.string_value ) {
					var.string_value->reference_count++;
				}
				break;
			case PUSH_GLOBAL:
				local = &( ( struct global_variable * ) ( ( struct value_expression * ) insn->expr )->str )->value;
				if( local->string_value ) {
					if( !to_num( local, ++top, vars, insn->expr ) ) {
						return EXCEPTION;
					}
				} else {
					*++top = local->number_value;
				}
				break;
			case LOAD_GLOBAL:
				local = &( ( struct global_variable * ) ( ( struct value_expression * ) insn->expr )->str )->value;
				var.number_value = local->number_value;
				var.string_value = local->string_value;
				if( var.string_value ) {
					var.string_value->reference_count++;
				}
				break;
			case INC_LOCAL:
				local = locals + insn->local;
				if( !local->string_value ) {
					local->number_value++;
					break;
				}
				/* Fallthrough. */
			case PUSH_LOCAL_PI:
				local = locals + insn->local;
				if( !local->string_value ) {
					*++top = local->number_value++;
					break;
				}
				/* Fallthrough. */
			case DEC_LOCAL: 
				local = locals + insn->local;
				if( !local->string_value ) {
					local->number_value--;
					break;
				}
				/* Fallthrough. */
			case PUSH_LOCAL_PD:
				local = locals + insn->local;
				if( local->string_value ) {
					return throw( vars, insn->expr, 0, "Not a number." );
				}
				*++top = local->number_value--;
				break;
			case ASSIGN_EXPR:
				var.number_value = 0;
				var.string_value = NULL;
				if( !insn->expr->evaluate( insn->expr, vars, &var ) ) {
					return EXCEPTION;
				}
				local = locals + insn->local;
				local->number_value = var.number_value;
				if( local->string_value ) {
					unref_string( local->string_value );
				}
				local->string_value = var.string_value;
				break;
			case CALL_EXPR:
				var.number_value = 0;
				var.string_value = NULL;
				if( !insn->expr->evaluate( insn->expr, vars, &var ) ) {
					return EXCEPTION;
				}
				if( var.string_value ) {
					unref_string( var.string_value );
				}
				break;
			case PUSH_EXPR:
				var.number_value = 0;
				var.string_value = NULL;
				if( insn->expr->evaluate( insn->expr, vars, &var ) ) {
					if( var.string_value ) {
						if( !to_num( &var, ++top, vars, insn->expr ) ) {
							unref_string( var.string_value );
							return EXCEPTION;
						}
						unref_string( var.string_value );
					} else {
						*++top = var.number_value;
					}
				} else {
					return EXCEPTION;
				}
				break;
			case LOAD_EXPR:
				var.number_value = 0;
				var.string_value = NULL;
				if( !insn->expr->evaluate( insn->expr, vars, &var ) ) {
					return EXCEPTION;
				}
				break;
			case PUSH_ARRAY:
				local = locals + insn->local;
				if( local->string_value && local->string_value->type == ARRAY ) {
					index = ( long_int ) *top;
					arr = ( struct array * ) local->string_value;
					if( ( unsigned int ) index < ( unsigned int ) arr->length ) {
						if( arr->string_values && arr->string_values[ index ] ) {
							var.number_value = arr->number_values[ index ];
							var.string_value = arr->string_values[ index ];
							if( !to_num( &var, top, vars, insn->expr ) ) {
								return EXCEPTION;
							}
						} else {
							*top = arr->number_values[ index ];
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
					index = ( long_int ) *top--;
					arr = ( struct array * ) local->string_value;
					if( ( unsigned int ) index < ( unsigned int ) arr->length ) {
						var.number_value = arr->number_values[ index ];
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
					index = ( long_int ) *top;
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
				local = &( ( struct global_variable * ) ( ( struct value_expression * ) insn->expr )->str )->value;
				if( local->string_value ) {
					index = ( long_int ) *top;
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
			case SAVE_LOCAL:
				local = locals + insn->local;
				local->number_value = *top;
				if( local->string_value ) {
					unref_string( local->string_value );
					local->string_value = NULL;
				}
				break;
			case POP_LOCAL:
				local = locals + insn->local;
				local->number_value = *top--;
				if( local->string_value ) {
					unref_string( local->string_value );
					local->string_value = NULL;
				}
				break;
			case STORE_LOCAL:
				local = locals + insn->local;
				local->number_value = var.number_value;
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
				index = ( long_int ) top[ 1 ];
				arr = ( struct array * ) locals[ insn->local ].string_value;
				arr->number_values[ index ] = top[ 2 ];
				if( arr->string_values && arr->string_values[ index ] ) {
					unref_string( arr->string_values[ index ] );
					arr->string_values[ index ] = NULL;
				}
				break;
			case STORE_ARRAY:
				index = ( long_int ) *top--;
				arr = ( struct array * ) locals[ insn->local ].string_value;
				if( arr->string_values ) {
					if( arr->string_values[ index ] ) {
						unref_string( arr->string_values[ index ] );
					}
					arr->number_values[ index ] = var.number_value;
					arr->string_values[ index ] = var.string_value;
				} else if( var.string_value ) {
					if( !to_num( &var, &arr->number_values[ index ], vars, insn->expr ) ) {
						unref_string( var.string_value );
						return EXCEPTION;
					}
					unref_string( var.string_value );
				} else {
					arr->number_values[ index ] = var.number_value;
				}
				break;
			case POP_RETURN: result->number_value = *top; return RETURN;
			case STORE_RETURN:
				result->number_value = var.number_value;
				result->string_value = var.string_value;
				return RETURN;
			case POP_RESULT: result->number_value = *top; return OKAY;
			case STORE_RESULT:
				result->number_value = var.number_value;
				result->string_value = var.string_value;
				return OKAY;
			case AND_STACK: top--; *top = ( long_int ) *top & ( long_int ) top[ 1 ]; break;
			case OR_STACK:  top--; *top = ( long_int ) *top | ( long_int ) top[ 1 ]; break;
			case XOR_STACK: top--; *top = ( long_int ) *top ^ ( long_int ) top[ 1 ]; break;
			case ADD_STACK: top--; *top  += top[ 1 ]; break;
			case SUB_STACK: top--; *top  -= top[ 1 ]; break;
			case MUL_STACK: top--; *top  *= top[ 1 ]; break;
			case FDI_STACK: top--; *top  /= top[ 1 ]; break;
			case DIV_STACK: top--; if( !divide( top, top[ 1 ], vars, insn->expr ) ) return EXCEPTION; break;
			case MOD_STACK: top--; if( !modulo( top, top[ 1 ], vars, insn->expr ) ) return EXCEPTION; break;
			case ASL_STACK: top--; *top = ( long_int ) *top << ( long_int ) top[ 1 ]; break;
			case ASR_STACK: top--; *top = ( long_int ) *top >> ( long_int ) top[ 1 ]; break;
			case LT_STACK: top--; *top = *top <  top[ 1 ]; break;
			case LE_STACK: top--; *top = *top <= top[ 1 ]; break;
			case EQ_STACK: top--; *top = *top == top[ 1 ]; break;
			case GE_STACK: top--; *top = *top >= top[ 1 ]; break;
			case GT_STACK: top--; *top = *top >  top[ 1 ]; break;
			case NE_STACK: top--; *top = *top != top[ 1 ]; break;
			case AND_CONST: *top = ( long_int ) *top & ( long_int ) insn->value; break;
			case OR_CONST:  *top = ( long_int ) *top | ( long_int ) insn->value; break;
			case XOR_CONST: *top = ( long_int ) *top ^ ( long_int ) insn->value; break;
			case ADD_CONST: *top  += insn->value; break;
			case SUB_CONST: *top  -= insn->value; break;
			case MUL_CONST: *top  *= insn->value; break;
			case FDI_CONST: *top  /= insn->value; break;
			case DIV_CONST: if( !divide( top, insn->value, vars, insn->expr ) ) return EXCEPTION; break;
			case MOD_CONST: if( !modulo( top, insn->value, vars, insn->expr ) ) return EXCEPTION; break;
			case ASL_CONST: *top = ( long_int ) *top << ( long_int ) insn->value; break;
			case ASR_CONST: *top = ( long_int ) *top >> ( long_int ) insn->value; break;
			case LT_CONST: *top = *top <  insn->value; break;
			case LE_CONST: *top = *top <= insn->value; break;
			case EQ_CONST: *top = *top == insn->value; break;
			case GE_CONST: *top = *top >= insn->value; break;
			case GT_CONST: *top = *top >  insn->value; break;
			case NE_CONST: *top = *top != insn->value; break;
			case AND_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top = ( long_int ) *top & ( long_int ) value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = ( long_int ) *top & ( long_int ) local->number_value;
				}
				break;
			case OR_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top = ( long_int ) *top | ( long_int ) value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = ( long_int ) *top | ( long_int ) local->number_value;
				}
				break;
			case XOR_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top = ( long_int ) *top ^ ( long_int ) value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = ( long_int ) *top ^ ( long_int ) local->number_value;
				}
				break;
			case ADD_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top += value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top += local->number_value;
				}
				break;
			case SUB_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top -= value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top -= local->number_value;
				}
				break;
			case MUL_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top *= value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top *= local->number_value;
				}
				break;
			case FDI_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top /= value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top /= local->number_value;
				}
				break;
			case DIV_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( !to_num( local, &value, vars, insn->expr ) ) {
						return EXCEPTION;
					}
				} else {
					value = local->number_value;
				}
				if( !divide( top, value, vars, insn->expr ) ) {
					return EXCEPTION;
				}
				break;
			case MOD_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( !to_num( local, &value, vars, insn->expr ) ) {
						return EXCEPTION;
					}
				} else {
					value = local->number_value;
				}
				if( !modulo( top, value, vars, insn->expr ) ) {
					return EXCEPTION;
				}
				break;
			case ASL_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top = ( long_int ) *top << ( long_int ) value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = ( long_int ) *top << ( long_int ) local->number_value;
				}
				break;
			case ASR_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top = ( long_int ) *top >> ( long_int ) value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = ( long_int ) *top >> ( long_int ) local->number_value;
				}
				break;
			case LT_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top = *top < value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = *top < local->number_value;
				}
				break;
			case LE_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top = *top <= value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = *top <= local->number_value;
				}
				break;
			case EQ_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top = *top == value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = *top == local->number_value;
				}
				break;
			case GE_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top = *top >= value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = *top >= local->number_value;
				}
				break;
			case GT_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top = *top > value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = *top > local->number_value;
				}
				break;
			case NE_LOCAL:
				local = locals + insn->local;
				if( local->string_value ) {
					if( to_num( local, &value, vars, insn->expr ) ) {
						*top = *top != value;
					} else {
						return EXCEPTION;
					}
				} else {
					*top = *top != local->number_value;
				}
				break;
		}
		insn++;
	}
}

static enum result execute_while_local_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct environment *env = vars->func->env;
	struct variable *lhs = &vars->locals[ this->local ];
	struct variable *rhs = &vars->locals[ ( ( struct block_statement * ) this )->rhs ];
	int oper = ( ( struct block_statement * ) this )->oper;
	struct statement *stmt;
	enum result ret;
	while( 1 ) {
		if( lhs->string_value || rhs->string_value ) {
			return execute_while_statement( this, vars, result );
		} else {
			switch( oper ) {
				case '!': if( lhs->number_value == rhs->number_value ) return OKAY; break;
				case '(': if( lhs->number_value >  rhs->number_value ) return OKAY; break;
				case ')': if( lhs->number_value <  rhs->number_value ) return OKAY; break;
				case '<': if( lhs->number_value >= rhs->number_value ) return OKAY; break;
				case '=': if( lhs->number_value != rhs->number_value ) return OKAY; break;
				case '>': if( lhs->number_value <= rhs->number_value ) return OKAY; break;
				default: return execute_while_statement( this, vars, result );
			}
		}
		stmt = ( ( struct block_statement * ) this )->if_block;
		while( stmt && ( ret = stmt->execute( stmt, vars, result ) ) == OKAY ) {
			stmt = stmt->next;
		}
		if( stmt ) {
			if( ret == RETURN ) {
				return RETURN;
			} else if( ret == BREAK ) {
				return OKAY;
			} else if( ret == EXCEPTION ) {
				return EXCEPTION;
			}
		}
		if( env->interrupted ) {
			return throw_interrupted( vars, this->source );
		}
	}
}

static enum result execute_while_const_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct environment *env = vars->func->env;
	struct variable *lhs = &vars->locals[ this->local ];
	number rhs = ( ( struct block_statement * ) this )->num;
	int oper = ( ( struct block_statement * ) this )->oper;
	struct statement *stmt;
	enum result ret;
	while( 1 ) {
		if( lhs->string_value ) {
			if( oper ) {
				return execute_while_statement( this, vars, result );
			}
		} else {
			switch( oper ) {
				case  0 : if( lhs->number_value == 0 ) return OKAY; break;
				case '!': if( lhs->number_value == rhs ) return OKAY; break;
				case '(': if( lhs->number_value >  rhs ) return OKAY; break;
				case ')': if( lhs->number_value <  rhs ) return OKAY; break;
				case '<': if( lhs->number_value >= rhs ) return OKAY; break;
				case '=': if( lhs->number_value != rhs ) return OKAY; break;
				case '>': if( lhs->number_value <= rhs ) return OKAY; break;
				default: return execute_while_statement( this, vars, result );
			}
		}
		stmt = ( ( struct block_statement * ) this )->if_block;
		while( stmt && ( ret = stmt->execute( stmt, vars, result ) ) == OKAY ) {
			stmt = stmt->next;
		}
		if( stmt ) {
			if( ret == RETURN ) {
				return RETURN;
			} else if( ret == BREAK ) {
				return OKAY;
			} else if( ret == EXCEPTION ) {
				return EXCEPTION;
			}
		}
		if( env->interrupted ) {
			return throw_interrupted( vars, this->source );
		}
	}
}

static enum result execute_if_local_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct variable *lhs = &vars->locals[ this->local ];
	struct variable *rhs = &vars->locals[ ( ( struct block_statement * ) this )->rhs ];
	struct statement *stmt = ( ( struct block_statement * ) this )->if_block;
	enum result ret = OKAY;
	if( lhs->string_value || rhs->string_value ) {
		return execute_if_statement( this, vars, result );
	}
	switch( ( ( struct block_statement * ) this )->oper ) {
		case '!': if( lhs->number_value == rhs->number_value ) stmt = ( ( struct block_statement * ) this )->else_block; break;
		case '(': if( lhs->number_value >  rhs->number_value ) stmt = ( ( struct block_statement * ) this )->else_block; break;
		case ')': if( lhs->number_value <  rhs->number_value ) stmt = ( ( struct block_statement * ) this )->else_block; break;
		case '<': if( lhs->number_value >= rhs->number_value ) stmt = ( ( struct block_statement * ) this )->else_block; break;
		case '=': if( lhs->number_value != rhs->number_value ) stmt = ( ( struct block_statement * ) this )->else_block; break;
		case '>': if( lhs->number_value <= rhs->number_value ) stmt = ( ( struct block_statement * ) this )->else_block; break;
		default: return execute_if_statement( this, vars, result );
	}
	while( stmt && ( ret = stmt->execute( stmt, vars, result ) ) == OKAY ) {
		stmt = stmt->next;
	}
	return ret;
}

static enum result execute_if_const_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct variable *lhs = &vars->locals[ this->local ];
	number rhs = ( ( struct block_statement * ) this )->num;
	struct statement *stmt = ( ( struct block_statement * ) this )->if_block;
	enum result ret = OKAY;
	if( lhs->string_value ) {
		if( ( ( struct block_statement * ) this )->oper ) {
			return execute_if_statement( this, vars, result );
		}
	} else {
		switch( ( ( struct block_statement * ) this )->oper ) {
			case  0 : if( lhs->number_value == 0 ) stmt = ( ( struct block_statement * ) this )->else_block; break;
			case '!': if( lhs->number_value == rhs ) stmt = ( ( struct block_statement * ) this )->else_block; break;
			case '(': if( lhs->number_value >  rhs ) stmt = ( ( struct block_statement * ) this )->else_block; break;
			case ')': if( lhs->number_value <  rhs ) stmt = ( ( struct block_statement * ) this )->else_block; break;
			case '<': if( lhs->number_value >= rhs ) stmt = ( ( struct block_statement * ) this )->else_block; break;
			case '=': if( lhs->number_value != rhs ) stmt = ( ( struct block_statement * ) this )->else_block; break;
			case '>': if( lhs->number_value <= rhs ) stmt = ( ( struct block_statement * ) this )->else_block; break;
			default: return execute_if_statement( this, vars, result );
		}
	}
	while( stmt && ( ret = stmt->execute( stmt, vars, result ) ) == OKAY ) {
		stmt = stmt->next;
	}
	return ret;
}

/* Replace the source statement with a new arithmetic statement (combined with the previous statement if possible). */
static struct arithmetic_statement* add_arithmetic_statement( struct statement *src, struct statement *prev, char *message ) {
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
	src->next = NULL;
	dispose_statements( src );
	return arith;
}

static void optimize_stmt_source( struct statement *stmt, char *message ) {
	struct expression expr;
	expr.parameters = stmt->source;
	if( optimize_parameters( &expr, message ) ) {
		stmt->source = expr.parameters;
	}
}

static struct statement* optimize_local_assignment( struct statement *stmt, struct statement *prev, char *message ) {
	struct instruction *insn;
	struct expression *expr = stmt->source;
	int local = stmt->local, oper, dest = POP_LOCAL;
	struct arithmetic_statement *arith;
	if( prev->execute == execute_arithmetic_statement || can_compile_stmt( stmt->next ) || can_compile_expr( expr ) >= ARITHMETIC_OPERATOR ) {
		arith = add_arithmetic_statement( stmt, prev, message );
		if( arith ) {
			stmt = &arith->stmt;
			if( compile_expression( arith, expr, 0, message ) ) {
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
	} else {
		optimize_stmt_source( stmt, message );
	}
	return stmt;
}

static struct statement* optimize_array_assignment( struct statement *stmt, struct statement *prev, char *message ) {
	int *oper, dest = POP_ARRAY;
	struct expression *src = stmt->source, *arr = src->next, *idx = arr->next;
	struct arithmetic_statement *arith = add_arithmetic_statement( stmt, prev, message );
	if( arith ) {
		stmt = &arith->stmt;
		if( compile_expression( arith, idx, 0, message )
		&& add_instruction( &arith->insns, CHECK_ARRAY, arr->index, idx, message )
		&& compile_expression( arith, src, 1, message ) ) {
			oper = &arith->insns.list[ arith->insns.count - 1 ].oper;
			if( *oper == PUSH_LOCAL || *oper == PUSH_GLOBAL || *oper == PUSH_EXPR || *oper == PUSH_ARRAY ) {
				dest = STORE_ARRAY;
				(*oper)++;
			}
			add_instruction( &arith->insns, dest, arr->index, arr, message );
		}
	}
	return stmt;
}

static struct statement* optimize_increment( struct statement *stmt, struct statement *prev, enum arithmetic_op oper, char *message ) {
	int local = stmt->local;
	struct expression *expr = stmt->source;
	struct arithmetic_statement *arith;
	if( prev->execute == execute_arithmetic_statement || can_compile_stmt( stmt->next ) ) {
		arith = add_arithmetic_statement( stmt, prev, message );
		if( arith ) {
			stmt = &arith->stmt;
			add_instruction( &arith->insns, oper, local, expr, message );
		}
	}
	return stmt;
}

static struct statement* optimize_return( struct statement *stmt, struct statement *prev, char *message ) {
	int *oper, dest = POP_RETURN;
	struct expression *expr = stmt->source;
	struct arithmetic_statement *arith;
	if( prev->execute == execute_arithmetic_statement || can_compile_expr( expr ) ) {
		arith = add_arithmetic_statement( stmt, prev, message );
		if( arith ) {
			stmt = &arith->stmt;
			if( compile_expression( arith, expr, 0, message ) ) {
				oper = &arith->insns.list[ arith->insns.count - 1 ].oper;
				if( *oper == PUSH_LOCAL || *oper == PUSH_GLOBAL || *oper == PUSH_EXPR || *oper == PUSH_ARRAY ) {
					dest = STORE_RETURN;
					(*oper)++;
				}
				add_instruction( &arith->insns, dest, 0, expr, message );
			}
		}
	} else {
		optimize_stmt_source( stmt, message );
	}
	return stmt;
}

static struct statement* optimize_call( struct statement *stmt, struct statement *prev, char *message ) {
	struct expression *expr = stmt->source;
	struct arithmetic_statement *arith;
	if( prev->execute == execute_arithmetic_statement || can_compile_stmt( stmt->next ) ) {
		arith = add_arithmetic_statement( stmt, prev, message );
		if( arith ) {
			stmt = &arith->stmt;
			add_instruction( &arith->insns, CALL_EXPR, 0, expr, message );
		}
	}
	optimize_stmt_source( stmt, message );
	return stmt;
}

static enum result evaluate_arith_stmt_expr( struct expression *this,
	struct variables *vars, struct variable *result ) {
	return execute_arithmetic_statement( &( ( struct arithmetic_statement_expr * ) this )->stmt.stmt, vars, result );
}

#if defined( PRINT_OPTIMIZATIONS )
static void print_insns( struct function *func, struct arithmetic_statement *stmt ) {
	char *name;
	struct instruction *insn = stmt->insns.list;
	if( func ) {
		fprintf( stderr, "Compiled from function '%s' on line %d of '%s':\n", func->str.string, func->line, func->file->string );
	}
	while( insn->oper ) {
		if( insn->oper < HALT || insn->oper >= NOP ) {
			name = "XXX";
		} else {
			name = arithmetic_ops[ insn->oper ];
		}
#if defined( FLOATING_POINT )
		if( insn->value ) {
			fprintf( stderr, "% 6d %s %.16g\n", insn->expr->line, name, insn->value );
		} else
#endif
		fprintf( stderr, "% 6d %s %d\n", insn->expr->line, name, insn->local );
		insn++;
	}
}

static void print_expr_insns( struct function *func, struct expression * expr ) {
	while( expr ) {
		if( expr->evaluate == evaluate_arith_stmt_expr ) {
			print_insns( func, &( ( struct arithmetic_statement_expr * ) expr )->stmt );
		}
		print_expr_insns( func, expr->parameters );
		expr = expr->next;
	}
}
#endif

static struct expression* compile_arith_stmt_expr( struct expression *expr, struct expression *prev, char *message ) {
	size_t size;
	struct instruction *insn;
	struct instructions *insns;
	int *oper, dest = POP_RESULT;
	struct arithmetic_statement *arith_stmt;
	struct arithmetic_statement_expr *new_arith_expr, *arith_expr = calloc( 1, sizeof( struct arithmetic_statement_expr ) );
	if( arith_expr ) {
		arith_stmt = &arith_expr->stmt;
		if( compile_expression( arith_stmt, expr, 0, message ) ) {
			oper = &arith_stmt->insns.list[ arith_stmt->insns.count - 1 ].oper;
			if( *oper == PUSH_LOCAL || *oper == PUSH_GLOBAL || *oper == PUSH_ARRAY ) {
				dest = STORE_RESULT;
				(*oper)++;
			}
			if( add_instruction( &arith_stmt->insns, dest, 0, expr, message ) ) {
				insns = &arith_stmt->insns;
				if( insns->list != insns->initial ) {
					size = ( insns->count + 1 ) * sizeof( struct instruction );
					new_arith_expr = calloc( 1, sizeof( struct arithmetic_statement_expr ) + size );
					if( new_arith_expr ) {
						insn = ( struct instruction * ) &new_arith_expr[ 1 ];
						memcpy( insn, insns->list, size );
						new_arith_expr->stmt.insns.list = insn;
						arith_stmt = &new_arith_expr->stmt;
					} else {
						strcpy( message, OUT_OF_MEMORY );
					}
					free( insns->list );
					free( arith_expr );
					arith_expr = new_arith_expr;
				}
				if( arith_expr ) {
					arith_expr->expr.line = expr->line;
					arith_expr->expr.next = expr->next;
					arith_expr->expr.parameters = expr;
					expr->next = arith_stmt->stmt.source;
					arith_expr->expr.evaluate = evaluate_arith_stmt_expr;
					expr = &arith_expr->expr;
					if( prev ) {
						prev->next = expr;
					}
					return expr;
				}
			}
		}
		free( arith_expr );
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return NULL;
}

static struct expression* optimize_expression( struct expression *expr, struct expression *prev, char *message ) {
	enum compilable_expr compilable = can_compile_expr( expr );
	if( compilable ) {
		if( compilable >= ARITHMETIC_OPERATOR ) {
			expr = compile_arith_stmt_expr( expr, prev, message );
		}
	} else {
		expr = optimize_parameters( expr, message );
	}
	return expr;
}

static struct expression* optimize_parameters( struct expression *expr, char *message ) {
	struct expression params, *prev = &params, *next = expr->parameters;
	params.next = next;
	while( next ) {
		prev = optimize_expression( next, prev, message );
		if( prev ) {
			next = prev->next;
		} else {
			return NULL;
		}
	}
	expr->parameters = params.next;
	return expr;
}

static int optimize_conditional( struct statement *stmt,
	enum result ( *execute_local )( struct statement *this, struct variables *vars, struct variable *result ),
	enum result ( *execute_const )( struct statement *this, struct variables *vars, struct variable *result ) ) {
	struct expression *param = stmt->source;
	if( param->evaluate == evaluate_arithmetic_expression
	&& strchr( "!<(=)>", param->index ) && param->parameters->evaluate == evaluate_local ) {
		if( param->parameters->next->evaluate == evaluate_local ) {
			stmt->local = param->parameters->index;
			( ( struct block_statement * ) stmt )->rhs = param->parameters->next->index;
			( ( struct block_statement * ) stmt )->oper = param->index;
			stmt->execute = execute_local;
			return 1;
		} else if( param->parameters->next->evaluate == evaluate_number_literal_expression ) {
			stmt->local = param->parameters->index;
			( ( struct block_statement * ) stmt )->num = ( ( struct value_expression * ) param->parameters->next )->num;
			( ( struct block_statement * ) stmt )->oper = param->index;
			stmt->execute = execute_const;
			return 1;
		}
	} else if( param->evaluate == evaluate_local ) {
		stmt->local = param->index;
		stmt->execute = execute_const;
		return 1;
	}
	return 0;
}

/* Returns the last statement after optimization. */
struct statement* optimize_statements( struct function *func, struct statement *prev, char *message ) {
	struct statement *stmt = prev, *next = stmt->next;
	while( next ) {
		switch( can_compile_stmt( next ) ) {
			case LOCAL_ASSIGNMENT:
				next = optimize_local_assignment( next, prev, message );
				break;
			case ARRAY_ASSIGNMENT:
				next = optimize_array_assignment( next, prev, message );
				break;
			case LOCAL_INCREMENT:
				next = optimize_increment( next, prev, INC_LOCAL, message );
				break;
			case LOCAL_DECREMENT:
				next = optimize_increment( next, prev, DEC_LOCAL, message );
				break;
			case CALL_STATEMENT:
				next = optimize_call( next, prev, message );
				break;
			case RETURN_STATEMENT:
				next = optimize_return( next, prev, message );
				break;
			default:
				if( next->execute == execute_while_statement
				&& optimize_conditional( next, execute_while_local_statement, execute_while_const_statement ) ) {
#if defined( PRINT_OPTIMIZATIONS )
					fprintf( stderr, "Optimized while conditional in function '%s' on line %d of '%s'.\n", func->str.string, func->line, func->file->string );
#endif
				} else if( next->execute == execute_if_statement
				&& optimize_conditional( next, execute_if_local_statement, execute_if_const_statement ) ) {
#if defined( PRINT_OPTIMIZATIONS )
					fprintf( stderr, "Optimized if conditional in function '%s' on line %d of '%s'.\n", func->str.string, func->line, func->file->string );
#endif
				} else {
					optimize_stmt_source( next, message );
				}
		}
		if( message[ 0 ] ) {
			return prev;
		}
		prev = next;
		next = next->next;
	}
#if defined( PRINT_OPTIMIZATIONS )
	next = stmt->next;
	while( next ) {
		if( next->execute == execute_arithmetic_statement ) {
			print_insns( func, ( struct arithmetic_statement * ) next );
		}
		print_expr_insns( func, next->source );
		next = next->next;
	}
#endif
	return prev;
}
