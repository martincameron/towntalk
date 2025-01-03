
#include "errno.h"
#include "signal.h"
#include "stddef.h"
#include "stdio.h"
#include "string.h"

#include "towntalk.h"

/*
	Experimental bytecode assembler/interpreter for fast arithmetic.
	Associated references are not assigned for performance reasons.

	Example:

	program test {
		var idx, count = 1000000, arr = $array(count);
		asm {
			loop:
				let [ arr idx ] = idx;
				let idx = +( idx 1 );
			jump <( idx count ) loop;
		}
	}
	
	Opcodes:

	opcode      x y z imm : mnemonic
	--------------------------------
	halt        0 0 0   0 : halt;
	
	jump_lt     0 y z off : jump <( y z ) label;
	jump_le     0 y z off : jump <e( y z ) label;
	jump_eq     0 y z off : jump =( y z ) label;
	jump_ne     0 y z off : jump <>( y z ) label;
	jump_ge     0 y z off : jump >e( y z ) label;
	jump_gt     0 y z off : jump >( y z ) label;
	
	letv_i      x 0 0 imm : let x = imm;
	letv_v      x y 0   0 : let x = y;
	letv_ai     x y 0 imm : let x = [ y imm ];
	letv_av     x y z   0 : let x = [ y z ];
	letv_ap     x y z   0 : let x = [ y z++ ];
	letav_i     x y 0 imm : let [ x y ] = imm;
	letai_v     x 0 z imm : let [ x imm ] = z;
	letav_v     x y z   0 : let [ x y ] = z;
	letap_v     x y z   0 : let [ x y++ ] = z;
	
	letv_add_vi x y 0 imm : let x = +( y imm );
	letv_add_vv x y z   0 : let x = +( y z );
	letv_sub_vi x y 0 imm : let x = -( y imm );
	letv_sub_vv x y z   0 : let x = -( y z );
	letv_mul_vi x y 0 imm : let x = *( y imm );
	letv_mul_vv x y z   0 : let x = *( y z );
	letv_div_vi x y 0 imm : let x = /( y imm );
	letv_div_vv x y z   0 : let x = /( y z );
	letv_mod_vi x y 0 imm : let x = %( y imm );
	letv_mod_vv x y z   0 : let x = %( y z );
	letv_fdi_vi x y 0 imm : let x = $div( y imm );
	letv_fdi_vv x y z   0 : let x = $div( y z );
	letv_shl_vi x y 0 imm : let x = <<( y imm );
	letv_shl_vv x y z   0 : let x = <<( y z );
	letv_asr_vi x y 0 imm : let x = >>( y imm );
	letv_asr_vv x y z   0 : let x = >>( y z );
	letv_and_vi x y 0 imm : let x = &( y imm );
	letv_and_vv x y z   0 : let x = &( y z );
	letv_or_vi  x y 0 imm : let x = |( y imm );
	letv_or_vv  x y z   0 : let x = |( y z );
	letv_xor_vi x y 0 imm : let x = ^( y imm );
	letv_xor_vv x y z   0 : let x = ^( y z );
	letv_chr_vi x y 0 imm : let x = $chr( y imm );
	letv_chr_vv x y z   0 : let x = $chr( y z );
	letv_chr_vp x y z   0 : let x = $chr( y z++ );
	letv_unp_vi x y 0 imm : let x = $unpack( y imm );
	letv_unp_vv x y z   0 : let x = $unpack( y z );
	letv_unp_vp x y z   0 : let x = $unpack( y z++ );
*/

/* Externals. */
int parse_number( char *str, number *result );

enum opcodes {
	HALT,
	JUMP_LT,
	JUMP_LE,
	JUMP_EQ,
	JUMP_NE,
	JUMP_GE,
	JUMP_GT,
	LETV_I,
	LETV_V,
	LETV_AI,
	LETV_AV,
	LETV_AP,
	LETAV_I,
	LETAI_V,
	LETAV_V,
	LETAP_V,
	LETV_ADD_VI,
	LETV_ADD_VV,
	LETV_SUB_VI,
	LETV_SUB_VV,
	LETV_MUL_VI,
	LETV_MUL_VV,
	LETV_DIV_VI,
	LETV_DIV_VV,
	LETV_MOD_VI,
	LETV_MOD_VV,
	LETV_FDI_VI,
	LETV_FDI_VV,
	LETV_SHL_VI,
	LETV_SHL_VV,
	LETV_ASR_VI,
	LETV_ASR_VV,
	LETV_AND_VI,
	LETV_AND_VV,
	LETV_OR_VI,
	LETV_OR_VV,
	LETV_XOR_VI,
	LETV_XOR_VV,
	LETV_CHR_VI,
	LETV_CHR_VV,
	LETV_CHR_VP,
	LETV_UNP_VI,
	LETV_UNP_VV,
	LETV_UNP_VP
};

struct asm_operator {
	char *name;
	unsigned char opcode;
};

static struct asm_operator jump_operators[] = {
	{ "<", JUMP_LT },
	{ "<e", JUMP_LE },
	{ "=", JUMP_EQ },
	{ "<>", JUMP_NE },
	{ ">e", JUMP_GE },
	{ ">", JUMP_GT },
	{ NULL }
};

static struct asm_operator let_vi_operators[] = {
	{ "+", LETV_ADD_VI },
	{ "-", LETV_SUB_VI },
	{ "*", LETV_MUL_VI },
	{ "/", LETV_DIV_VI },
	{ "_/", LETV_DIV_VI },
	{ "%", LETV_MOD_VI },
	{ "$div", LETV_FDI_VI },
	{ "Div", LETV_FDI_VI },
	{ "<<", LETV_SHL_VI },
	{ ">>", LETV_ASR_VI },
	{ "&", LETV_AND_VI },
	{ "|", LETV_OR_VI },
	{ "^", LETV_XOR_VI },
	{ "$chr", LETV_CHR_VI },
	{ "Chr", LETV_CHR_VI },
	{ "$unpack", LETV_UNP_VI },
	{ "Unpack", LETV_UNP_VI },
	{ NULL }
};

static struct asm_operator let_vv_operators[] = {
	{ "+", LETV_ADD_VV },
	{ "-", LETV_SUB_VV },
	{ "*", LETV_MUL_VV },
	{ "/", LETV_DIV_VV },
	{ "_/", LETV_DIV_VV },
	{ "%", LETV_MOD_VV },
	{ "$div", LETV_FDI_VV },
	{ "Div", LETV_FDI_VV },
	{ "<<", LETV_SHL_VV },
	{ ">>", LETV_ASR_VV },
	{ "&", LETV_AND_VV },
	{ "|", LETV_OR_VV },
	{ "^", LETV_XOR_VV },
	{ "$chr", LETV_CHR_VV },
	{ "Chr", LETV_CHR_VV },
	{ "$unpack", LETV_UNP_VV },
	{ "Unpack", LETV_UNP_VV },
	{ NULL }
};

static struct asm_operator let_vp_operators[] = {
	{ "$chr", LETV_CHR_VP },
	{ "Chr", LETV_CHR_VP },
	{ "$unpack", LETV_UNP_VP },
	{ "Unpack", LETV_UNP_VP },
	{ NULL }
};

struct instruction {
	unsigned char opcode, x, y, z;
	number imm;
};

struct asm_statement {
	struct statement stmt;
	struct instruction *instructions;
};

struct label {
	int idx;
	char *name;
	struct label *next;
};

static struct asm_operator* get_asm_operator( struct asm_operator *operators, char *name ) {
	while( operators->name && strcmp( name, operators->name ) ) {
		operators++;
	}
	if( operators->name == NULL ) {
		operators = NULL;
	}
	return operators;
}

static int get_local_variable( struct function *func, struct element *elem, char *suffix, char *message ) {
	int idx = 0;
	char *name = elem->str.string;
	size_t len = strlen( name ) - strlen( suffix );
	struct local_variable *local = func->variable_decls;
	if( len > 0 ) {
		while( local && ( strncmp( local->name, name, len ) || strlen( local->name ) != len ) ) {
			idx++;
			local = local->next;
		}
	}
	if( len <= 0 || local == NULL || idx > 127 ) {
		if( message ) {
			sprintf( message, "Invalid local variable '%.64s' on line %d.", name, elem->line );
		}
		idx = -1;
	}
	return idx;
}

static int alphachar( char chr, char *include ) {
	return ( chr >= 'A' && chr <= 'Z') || ( chr >= 'a' && chr <= 'z' ) || strchr( include, chr );
}

static int validate_label( char *name, int len ) {
	int chr = name[ 0 ], idx = 1, result = 1;
	if( alphachar( chr, "" ) ) {
		chr = name[ idx++ ];
		while( chr && idx <= len ) {
			if( alphachar( chr, "_0123456789" ) ) {
				chr = name[ idx++ ];
			} else {
				result = chr = 0;
			}
		}
	} else {
		result = 0;
	}
	return result;
}

static struct label* new_label( struct element *elem, int idx, char *message ) {
	struct label *label = NULL;
	int len = elem->str.length - 1;
	if( validate_label( elem->str.string, len ) ) {
		label = malloc( sizeof( struct label ) + ( len + 1 ) * sizeof( char ) );
		if( label ) {
			label->idx = idx;
			label->name = ( char * ) &label[ 1 ];
			strncpy( label->name, elem->str.string, len );
			label->name[ len ] = 0;
			label->next = NULL;
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	} else {
		sprintf( message, "Invalid label '%.64s' on line %d.", elem->str.string, elem->line );
	}
	return label;
}

static struct label* get_label( struct label *labels, char *name ) {
	while( labels && strcmp( name, labels->name ) ) {
		labels = labels->next;
	}
	return labels;
}

static void dispose_labels( struct label *labels ) {
	struct label *next;
	while( labels ) {
		next = labels->next;
		free( labels );
		labels = next;
	}
}

static struct element* validate_jump( struct element *elem, char *message ) {
	if( elem->next ) {
		elem = elem->next;
		if( elem->next && strcmp( "()", elem->next->str.string ) == 0 ) {
			elem = elem->next;
			if( elem->child && elem->child->next && elem->child->next->next == NULL ) {
				if( elem->next ) {
					elem = elem->next;
					if( !validate_label( elem->str.string, elem->str.length ) ) {
						sprintf( message, "Invalid jump label '%.64s' on line %d.", elem->str.string, elem->line );
						elem = NULL;
					}
				} else {
					sprintf( message, "Expected label after jump condition on line %d.", elem->line );
					elem = NULL;
				}
			} else {
				sprintf( message, "Invalid jump condition on line %d.", elem->line );
				elem = NULL;
			}
		} else {
			sprintf( message, "Expected '(' after jump condition on line %d.", elem->line );
			elem = NULL;
		}
	} else {
		sprintf( message, "Expected jump condition on line %d.", elem->line );
		elem = NULL;
	}
	return elem;
}

static struct element* parse_jump( struct element *elem, struct function *func, struct label *labels, struct instruction *output, char *message ) {
	/*
		opcode      x y z imm : mnemonic
		jump_lt     0 y z off : jump <( y z ) label;
		jump_le     0 y z off : jump <e( y z ) label;
		jump_eq     0 y z off : jump =( y z ) label;
		jump_ne     0 y z off : jump <>( y z ) label;
		jump_ge     0 y z off : jump >e( y z ) label;
		jump_gt     0 y z off : jump >( y z ) label;
	*/
	int idx;
	struct label *label;
	struct element *next = elem->next;
	struct asm_operator *oper = get_asm_operator( jump_operators, next->str.string );
	if( oper ) {
		output->opcode = oper->opcode;
		next = next->next;
		idx = get_local_variable( func, next->child, "", message );
		if( idx >= 0 ) {
			output->y = idx;
			idx = get_local_variable( func, next->child->next, "", message );
			if( idx >= 0 ) {
				output->z = idx;
				next = next->next;
				label = get_label( labels, next->str.string );
				if( label ) {
					output->imm = label->idx;
				} else {
					sprintf( message, "Undeclared jump label '%.64s' on line %d.", next->str.string, next->line );
					next = NULL;
				}
			} else {
				next = NULL;
			}
		} else {
			next = NULL;
		}
	} else {
		sprintf( message, "Invalid jump condition '%.64s' on line %d.", next->str.string, next->line );
		next = NULL;
	}
	return next;
}

static struct element* validate_let( struct element *elem, char *message ) {
	if( elem->next && alphachar( elem->next->str.string[ 0 ], "[" ) ) {
		elem = elem->next;
		if( elem->next && strcmp( "=", elem->next->str.string ) == 0 ) {
			elem = elem->next;
			if( elem->next && elem->next->str.string[ 0 ] != ';' ) {
				elem = elem->next;
				if( elem->next && strcmp( "()", elem->next->str.string ) == 0 ) {
					elem = elem->next;
				}
			} else {
				sprintf( message, "Invalid source after '=' on line %d.", elem->line );
				elem = NULL;
			}
		} else {
			sprintf( message, "Expected '=' after destination on line %d.", elem->line );
			elem = NULL;
		}
	} else {
		sprintf( message, "Invalid destination after 'let' on line %d.", elem->line );
		elem = NULL;
	}
	return elem;
}

static struct element* parse_leta( struct element *elem, struct function *func, struct instruction *output, char *message ) {
	/*
		opcode      x y z imm : mnemonic
		letai_v     x 0 z imm : let [ x imm ] = z;
		letav_i     x y 0 imm : let [ x y ] = imm;
		letav_v     x y z   0 : let [ x y ] = z;
		letap_v     x y z   0 : let [ x y++ ] = z;
	*/
	int idx;
	char *str;
	struct element *next = elem->next, *child = next->child;
	if( child && child->next && child->next->next == NULL ) {
		idx = get_local_variable( func, child, "", message );
		if( idx >= 0 ) {
			output->x = idx;
			str = child->next->str.string;
			if( str[ 0 ] == '-' || ( str[ 0 ] >= '0' && str[ 0 ] <= '9' ) ) {
				/* letai_v */
				if( parse_number( str, &output->imm ) ) {
					next = next->next->next;
					idx = get_local_variable( func, next, "", message );
					if( idx >= 0 ) {
						output->opcode = LETAI_V;
						output->z = idx;
					} else {
						next = NULL;
					}
				} else {
					sprintf( message, "Invalid immediate '%.64s' at line %d.", child->next->str.string, child->next->line );
					next = NULL;
				}
			} else {
				idx = get_local_variable( func, child->next, "", NULL );
				if( idx >= 0 ) {
					output->y = idx;
					next = next->next->next;
					str = next->str.string;
					if( str[ 0 ] == '-' || ( str[ 0 ] >= '0' && str[ 0 ] <= '9' ) ) {
						/* letav_i */
						if( parse_number( str, &output->imm ) ) {
							output->opcode = LETAV_I;
						} else {
							sprintf( message, "Invalid immediate '%.64s' at line %d.", next->str.string, next->line );
							next = NULL;
						}
					} else {
						/* letav_v */
						idx = get_local_variable( func, next, "", message );
						if( idx >= 0 ) {
							output->z = idx;
							output->opcode = LETAV_V;
						} else {
							next = NULL;
						}
					}
				} else {
					/* letap_v */
					idx = get_local_variable( func, child->next, "++", message );
					if( idx >= 0 ) {
						output->y = idx;
						next = next->next->next;
						idx = get_local_variable( func, next, "", message );
						if( idx >= 0 ) {
							output->z = idx;
							output->opcode = LETAP_V;
						} else {
							next = NULL;
						}
					} else {
						next = NULL;
					}
				}
			}
		} else {
			next = NULL;
		}
	} else {
		sprintf( message, "Invalid index expression on line %d.", next->line );
		next = NULL;
	}
	return next;
}

static struct element* parse_letv_a( struct element *elem, struct function *func, struct instruction *output, char *message ) {
	/*
		opcode      x y z imm : mnemonic
		letv_ai     x y 0 imm : let x = [ y imm ];
		letv_av     x y z   0 : let x = [ y z ];
		letv_ap     x y z   0 : let x = [ y z++ ];
	*/
	char *str;
	struct element *child, *next = elem->next;
	int idx = get_local_variable( func, next, "", message );
	if( idx >= 0 ) {
		output->x = idx;
		next = next->next->next;
		if( next->child && next->child->next && next->child->next->next == NULL ) {
			child = next->child;
			idx = get_local_variable( func, child, "", message );
			if( idx >= 0 ) {
				output->y = idx;
				str = child->next->str.string;
				if( str[ 0 ] == '-' || ( str[ 0 ] >= '0' && str[ 0 ] <= '9' ) ) {
					/* letv_ai */
					if( parse_number( str, &output->imm ) ) {
						output->opcode = LETV_AI;
					} else {
						sprintf( message, "Invalid immediate '%.64s' at line %d.", child->next->str.string, child->next->line );
						next = NULL;
					}
				} else {
					/* letv_av */
					idx = get_local_variable( func, child->next, "", NULL );
					if( idx >= 0 ) {
						output->opcode = LETV_AV;
						output->z = idx;
					} else {
						/* letv_ap*/
						idx = get_local_variable( func, child->next, "++", message );
						if( idx >= 0 ) {
							output->opcode = LETV_AP;
							output->z = idx;
						} else {
							next = NULL;
						}
					}
				}
			} else {
				next = NULL;
			}
		} else {
			sprintf( message, "Invalid index expression on line %d.", next->line );
			next = NULL;
		}
	} else {
		next = NULL;
	}
	return next;
}

static struct element* parse_letv_i( struct element *elem, struct function *func, struct instruction *output, char *message ) {
	/*
		opcode      x y z imm : mnemonic
		letv_i      x 0 0 imm : let x = imm;
	*/
	struct element *next = elem->next;
	int idx = get_local_variable( func, next, "", message );
	if( idx >= 0 ) {
		output->x = idx;
		next = next->next->next;
		if( parse_number( next->str.string, &output->imm ) ) {
			output->opcode = LETV_I;
		} else {
			sprintf( message, "Invalid immediate '%.64s' at line %d.", next->str.string, next->line );
			next = NULL;
		}
	} else {
		next = NULL;
	}
	return next;
}

static struct element* parse_letv_v( struct element *elem, struct function *func, struct instruction *output, char *message ) {
	/*
		opcode      x y z imm : mnemonic
		letv_v      x y 0   0 : let x = y;
	*/
	struct element *next = elem->next;
	int idx = get_local_variable( func, next, "", message );
	if( idx >= 0 ) {
		output->x = idx;
		next = next->next->next;
		idx = get_local_variable( func, next, "", message );
		if( idx >= 0 ) {
			output->y = idx;
			output->opcode = LETV_V;
		} else {
			next = NULL;
		}
	} else {
		next = NULL;
	}
	return next;
}

static struct element* parse_letv_opr( struct element *elem, struct function *func, struct instruction *output, char *message ) {
	/*
		opcode      x y z imm : mnemonic
		letv_opr_vi x y 0 imm : let x = opr( y imm );
		letv_opr_vv x y z   0 : let x = opr( y z );
		letv_opr_vp x y z   0 : let x = opr( y z++ );
	*/
	char *str;
	struct asm_operator *oper;
	struct element *child, *next = elem->next;
	int idx = get_local_variable( func, next, "", message );
	if( idx >= 0 ) {
		output->x = idx;
		next = next->next->next;
		if( next->next && next->next->child && next->next->child->next && next->next->child->next->next == NULL ) {
			child = next->next->child;
			idx = get_local_variable( func, child, "", message );
			if( idx >= 0 ) {
				output->y = idx;
				str = child->next->str.string;
				if( str[ 0 ] == '-' || ( str[ 0 ] >= '0' && str[ 0 ] <= '9' ) ) {
					/* letv_opr_vi */
					oper = get_asm_operator( let_vi_operators, next->str.string );
					if( oper ) {
						output->opcode = oper->opcode;
						if( parse_number( str, &output->imm ) ) {
							next = next->next;
						} else {
							sprintf( message, "Invalid immediate '%.64s' at line %d.", child->next->str.string, child->next->line );
							next = NULL;
						}
					} else {
						sprintf( message, "Invalid operator '%.64s' at line %d.", next->str.string, next->line );
						next = NULL;
					}
				} else {
					idx = get_local_variable( func, child->next, "", NULL );
					if( idx >= 0 ) {
						/* letv_opr_vv */
						oper = get_asm_operator( let_vv_operators, next->str.string );
						if( oper ) {
							output->opcode = oper->opcode;
							output->z = idx;
							next = next->next;
						} else {
							sprintf( message, "Invalid operator '%.64s' at line %d.", next->str.string, next->line );
							next = NULL;
						}
					} else {
						idx = get_local_variable( func, child->next, "++", message );
						if( idx >= 0 ) {
							/* letv_opr_vp */
							oper = get_asm_operator( let_vp_operators, next->str.string );
							if( oper ) {
								output->opcode = oper->opcode;
								output->z = idx;
								next = next->next;
							} else {
								sprintf( message, "Invalid operator '%.64s' at line %d.", next->str.string, next->line );
								next = NULL;
							}
						} else {
							next = NULL;
						}
					}
				}
			} else {
				next = NULL;
			}
		} else {
			sprintf( message, "Invalid operands for '%.64s' on line %d.", next->str.string, next->line );
			next = NULL;
		}
	} else {
		next = NULL;
	}
	return next;
}

static struct element* parse_let( struct element *elem, struct function *func, struct instruction *output, char *message ) {
	char *src = elem->next->str.string;
	char *dest = elem->next->next->next->str.string;
	char *param = elem->next->next->next->next->str.string;
	if( src[ 0 ] == '[' ) {
		return parse_leta( elem, func, output, message );
	} else if( dest[ 0 ] == '[' ) {
		return parse_letv_a( elem, func, output, message );
	} else if( param[ 0 ] == '(' ) {
		return parse_letv_opr( elem, func, output, message );
	} else if( dest[ 0 ] == '-' || ( dest[ 0 ] >= '0' && dest[ 0 ] <= '9' ) ) {
		return parse_letv_i( elem, func, output, message );
	} else {
		return parse_letv_v( elem, func, output, message );
	}
}

static int parse_instructions( struct element *elem, struct function *func, struct instruction *output, struct label *labels, char *message ) {
	int count = 0;
	while( elem ) {
		if( elem->str.string[ elem->str.length - 1 ] == ':' ) {
			if( output == NULL ) {
				labels->next = new_label( elem, count, message );
				labels = labels->next;
				if( labels == NULL ) {
					break;
				}
			}
		} else if( strcmp( elem->str.string, "//" ) ) {
			if( is_keyword( elem->str.string, "let" ) ) {
				if( output ) {
					elem = parse_let( elem, func, &output[ count ], message );
				} else {
					elem = validate_let( elem, message );
				}
				if( elem == NULL ) {
					break;
				}
			} else if( is_keyword( elem->str.string, "jump" ) ) {
				if( output ) {
					elem = parse_jump( elem, func, labels->next, &output[ count ], message );
				} else {
					elem = validate_jump( elem, message );
				}
				if( elem == NULL ) {
					break;
				}
			} else if( is_keyword( elem->str.string, "halt" ) ) {
				if( output ) {
					output[ count ].opcode = HALT;
				}
			} else {
				sprintf( message, "Unrecognized opcode '%.64s' on line %d.", elem->str.string, elem->line );
				break;
			}
			if( elem->next && strcmp( ";", elem->next->str.string ) == 0 ) {
				elem = elem->next;
				count++;
			} else {
				sprintf( message, "Expected ';' after '%.64s' on line %d.", elem->str.string, elem->line );
				break;
			}
		}
		elem = elem->next;
	}
	if( message[ 0 ] ) {
		count = 0;
	} else {
		if( output ) {
			output[ count ].opcode = HALT;
		}
		count++;
	}
	return count;
}

static enum result execute_asm_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct asm_statement *stmt = ( struct asm_statement * ) this;
	unsigned int idx, len, string_bounds[ 128 ], array_bounds[ 128 ];
	struct instruction *ins = stmt->instructions;
	struct environment *env = vars->func->env;
	struct variable *locals = vars->locals;
	struct string *str;
	char *chr;
	for( idx = 0, len = vars->func->num_variables; idx < len; idx++ ) {
		str = locals[ idx ].string_value;
		if( str ) {
			string_bounds[ idx ] = str->length;
			if( str->type == ARRAY ) {
				array_bounds[ idx ] = ( ( struct array * ) str )->length;
			} else {
				array_bounds[ idx ] = 0;
			}
		} else {
			string_bounds[ idx ] = array_bounds[ idx ] = 0;
		}
	}
	while( 1 ) {
		switch( ins->opcode ) {
			case HALT:
				/* halt        0 0 0   0 : halt; */
				return OKAY;
			case JUMP_LT:
				/* jump_lt     0 y z off : jump <( y z ) label; */
				if( locals[ ins->y ].number_value < locals[ ins->z ].number_value ) {
					if( env->interrupted ) {
						return throw_interrupted( vars, this->source );
					} else {
						ins = &stmt->instructions[ ( int ) ins->imm ];
					}
				} else {
					ins++;
				}
				break;
			case JUMP_LE:
				/* jump_le     0 y z off : jump <e( y z ) label; */
				if( locals[ ins->y ].number_value <= locals[ ins->z ].number_value ) {
					if( env->interrupted ) {
						return throw_interrupted( vars, this->source );
					} else {
						ins = &stmt->instructions[ ( int ) ins->imm ];
					}
				} else {
					ins++;
				}
				break;
			case JUMP_EQ:
				/* jump_eq     0 y z off : jump =( y z ) label; */
				if( locals[ ins->y ].number_value == locals[ ins->z ].number_value ) {
					if( env->interrupted ) {
						return throw_interrupted( vars, this->source );
					} else {
						ins = &stmt->instructions[ ( int ) ins->imm ];
					}
				} else {
					ins++;
				}
				break;
			case JUMP_NE:
				/* jump_ne     0 y z off : jump <>( y z ) label; */
				if( locals[ ins->y ].number_value != locals[ ins->z ].number_value ) {
					if( env->interrupted ) {
						return throw_interrupted( vars, this->source );
					} else {
						ins = &stmt->instructions[ ( int ) ins->imm ];
					}
				} else {
					ins++;
				}
				break;
			case JUMP_GE:
				/* jump_ge     0 y z off : jump >e( y z ) label; */
				if( locals[ ins->y ].number_value >= locals[ ins->z ].number_value ) {
					if( env->interrupted ) {
						return throw_interrupted( vars, this->source );
					} else {
						ins = &stmt->instructions[ ( int ) ins->imm ];
					}
				} else {
					ins++;
				}
				break;
			case JUMP_GT:
				/* jump_gt     0 y z off : jump >( y z ) label; */
				if( locals[ ins->y ].number_value > locals[ ins->z ].number_value ) {
					if( env->interrupted ) {
						return throw_interrupted( vars, this->source );
					} else {
						ins = &stmt->instructions[ ( int ) ins->imm ];
					}
				} else {
					ins++;
				}
				break;
			case LETV_I:
				/* letv_i      x 0 0 imm : let x = imm; */
				locals[ ins->x ].number_value = ins->imm;
				ins++;
				break;
			case LETV_V:
				/* letv_v      x y 0   0 : let x = y; */
				locals[ ins->x ].number_value = locals[ ins->y ].number_value;
				ins++;
				break;
			case LETV_AI:
				/* letv_ai     x y 0 imm : let x = [ y imm ]; */
				idx = ( int ) ins->imm;
				if( idx < array_bounds[ ins->y ] ) {
					locals[ ins->x ].number_value = ( ( struct array * ) locals[ ins->y ].string_value )->number_values[ idx ];
				} else {
					return throw( vars, this->source, ins->imm, "Not an array or index out of bounds." );
				}
				ins++;
				break;
			case LETV_AV:
				/* letv_av     x y z   0 : let x = [ y z ]; */
				idx = ( int ) locals[ ins->z ].number_value;
				if( idx < array_bounds[ ins->y ] ) {
					locals[ ins->x ].number_value = ( ( struct array * ) locals[ ins->y ].string_value )->number_values[ idx ];
				} else {
					return throw( vars, this->source, locals[ ins->z ].number_value, "Not an array or index out of bounds." );
				}
				ins++;
				break;
			case LETV_AP:
				/* letv_ap     x y z   0 : let x = [ y z++ ]; */
				idx = ( int ) locals[ ins->z ].number_value;
				if( idx < array_bounds[ ins->y ] ) {
					locals[ ins->x ].number_value = ( ( struct array * ) locals[ ins->y ].string_value )->number_values[ idx ];
					locals[ ins->z ].number_value++;
				} else {
					return throw( vars, this->source, locals[ ins->z ].number_value, "Not an array or index out of bounds." );
				}
				ins++;
				break;
			case LETAV_I:
				/* letav_i     x y 0 imm : let [ x y ] = imm; */
				idx = ( int ) locals[ ins->y ].number_value;
				if( idx < array_bounds[ ins->x ] ) {
					( ( struct array * ) locals[ ins->x ].string_value )->number_values[ idx ] = ins->imm;
				} else {
					return throw( vars, this->source, locals[ ins->y ].number_value, "Not an array or index out of bounds." );
				}
				ins++;
				break;
			case LETAI_V:
				/* letai_v     x 0 z imm : let [ x imm ] = z; */
				idx = ( int ) ins->imm;
				if( idx < array_bounds[ ins->x ] ) {
					( ( struct array * ) locals[ ins->x ].string_value )->number_values[ idx ] = locals[ ins->z ].number_value;
				} else {
					return throw( vars, this->source, ins->imm, "Not an array or index out of bounds." );
				}
				ins++;
				break;
			case LETAV_V:
				/* letav_v     x y z   0 : let [ x y ] = z; */
				idx = ( int ) locals[ ins->y ].number_value;
				if( idx < array_bounds[ ins->x ] ) {
					( ( struct array * ) locals[ ins->x ].string_value )->number_values[ idx ] = locals[ ins->z ].number_value;
				} else {
					return throw( vars, this->source, locals[ ins->y ].number_value, "Not an array or index out of bounds." );
				}
				ins++;
				break;
			case LETAP_V:
				/* letap_v     x y z   0 : let [ x y++ ] = z; */
				idx = ( int ) locals[ ins->y ].number_value;
				if( idx < array_bounds[ ins->x ] ) {
					( ( struct array * ) locals[ ins->x ].string_value )->number_values[ idx ] = locals[ ins->z ].number_value;
					locals[ ins->y ].number_value++;
				} else {
					return throw( vars, this->source, locals[ ins->y ].number_value, "Not an array or index out of bounds." );
				}
				ins++;
				break;
			case LETV_ADD_VI:
				/* letv_add_vi x y 0 imm : let x = +( y imm ); */
				locals[ ins->x ].number_value = locals[ ins->y ].number_value + ins->imm;
				ins++;
				break;
			case LETV_ADD_VV:
				/* letv_add_vv x y z   0 : let x = +( y z ); */
				locals[ ins->x ].number_value = locals[ ins->y ].number_value + locals[ ins->z ].number_value;
				ins++;
				break;
			case LETV_SUB_VI:
				/* letv_sub_vi x y 0 imm : let x = -( y imm ); */
				locals[ ins->x ].number_value = locals[ ins->y ].number_value - ins->imm;
				ins++;
				break;
			case LETV_SUB_VV:
				/* letv_sub_vv x y z   0 : let x = -( y z ); */
				locals[ ins->x ].number_value = locals[ ins->y ].number_value - locals[ ins->z ].number_value;
				ins++;
				break;
			case LETV_MUL_VI:
				/* letv_mul_vi x y 0 imm : let x = *( y imm ); */
				locals[ ins->x ].number_value = locals[ ins->y ].number_value * ins->imm;
				ins++;
				break;
			case LETV_MUL_VV:
				/* letv_mul_vv x y z   0 : let x = *( y z ); */
				locals[ ins->x ].number_value = locals[ ins->y ].number_value * locals[ ins->z ].number_value;
				ins++;
				break;
			case LETV_DIV_VI:
				/* letv_div_vi x y 0 imm : let x = /( y imm ); */
				if( ins->imm ) {
					locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value / ( long_int ) ins->imm;
				} else {
					return throw( vars, this->source, 0, "Integer division by zero." );
				}
				ins++;
				break;
			case LETV_DIV_VV:
				/* letv_div_vv x y z   0 : let x = /( y z ); */
				if( locals[ ins->z ].number_value ) {
					locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value / ( long_int ) locals[ ins->z ].number_value;
				} else {
					return throw( vars, this->source, 0, "Integer division by zero." );
				}
				ins++;
				break;
			case LETV_MOD_VI:
				/* letv_mod_vi x y 0 imm : let x = %( y imm ); */
				if( ins->imm ) {
					locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value % ( long_int ) ins->imm;
				} else {
					return throw( vars, this->source, 0, "Modulo division by zero." );
				}
				ins++;
				break;
			case LETV_MOD_VV:
				/* letv_mod_vv x y z   0 : let x = %( y z ); */
				if( locals[ ins->z ].number_value ) {
					locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value % ( long_int ) locals[ ins->z ].number_value;
				} else {
					return throw( vars, this->source, 0, "Modulo division by zero." );
				}
				ins++;
				break;
			case LETV_FDI_VI:
				/* letv_fdi_vi x y 0 imm : let x = //( y imm ); */
				locals[ ins->x ].number_value = locals[ ins->y ].number_value / ins->imm;
				ins++;
				break;
			case LETV_FDI_VV:
				/* letv_fdi_vv x y z   0 : let x = //( y z ); */
				locals[ ins->x ].number_value = locals[ ins->y ].number_value / locals[ ins->z ].number_value;
				ins++;
				break;
			case LETV_SHL_VI:
				/* letv_shl_vi x y 0 imm : let x = <<( y imm ); */
				locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value << ( long_int ) ins->imm;
				ins++;
				break;
			case LETV_SHL_VV:
				/* letv_shl_vv x y z   0 : let x = <<( y z ); */
				locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value << ( long_int ) locals[ ins->z ].number_value;
				ins++;
				break;
			case LETV_ASR_VI:
				/* letv_asr_vi x y 0 imm : let x = >>( y imm ); */
				locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value >> ( long_int ) ins->imm;
				ins++;
				break;
			case LETV_ASR_VV:
				/* letv_asr_vv x y z   0 : let x = >>( y z ); */
				locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value >> ( long_int ) locals[ ins->z ].number_value;
				ins++;
				break;
			case LETV_AND_VI:
				/* letv_and_vi x y 0 imm : let x = &( y imm ); */
				locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value & ( long_int ) ins->imm;
				ins++;
				break;
			case LETV_AND_VV:
				/* letv_and_vv x y z   0 : let x = &( y z ); */
				locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value & ( long_int ) locals[ ins->z ].number_value;
				ins++;
				break;
			case LETV_OR_VI:
				/* letv_or_vi x y 0 imm : let x = |( y imm ); */
				locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value | ( long_int ) ins->imm;
				ins++;
				break;
			case LETV_OR_VV:
				/* letv_or_vv  x y z   0 : let x = |( y z ); */
				locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value | ( long_int ) locals[ ins->z ].number_value;
				ins++;
				break;
			case LETV_XOR_VI:
				/* letv_xor_vi x y 0 imm : let x = ^( y imm ); */
				locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value ^ ( long_int ) ins->imm;
				ins++;
				break;
			case LETV_XOR_VV:
				/* letv_xor_vv x y z   0 : let x = ^( y z ); */
				locals[ ins->x ].number_value = ( long_int ) locals[ ins->y ].number_value ^ ( long_int ) locals[ ins->z ].number_value;
				ins++;
				break;
			case LETV_CHR_VI:
				/* letv_chr_vi x y 0 imm : let x = $chr( y imm ); */
				idx = ( int ) ins->imm;
				if( idx < string_bounds[ ins->y ] ) {
					locals[ ins->x ].number_value = ( signed char ) locals[ ins->y ].string_value->string[ idx ];
				} else {
					return throw( vars, this->source, ins->imm, "Not a string or index out of bounds." );
				}
				ins++;
				break;
			case LETV_CHR_VV:
				/* letv_chr_vv x y z   0 : let x = $chr( y z ); */
				idx = ( int ) locals[ ins->z ].number_value;
				if( idx < string_bounds[ ins->y ] ) {
					locals[ ins->x ].number_value = ( signed char ) locals[ ins->y ].string_value->string[ idx ];
				} else {
					return throw( vars, this->source, locals[ ins->z ].number_value, "Not a string or index out of bounds." );
				}
				ins++;
				break;
			case LETV_CHR_VP:
				/* letv_chr_vp x y z   0 : let x = $chr( y z++ ); */
				idx = ( int ) locals[ ins->z ].number_value;
				if( idx < string_bounds[ ins->y ] ) {
					locals[ ins->x ].number_value = ( signed char ) locals[ ins->y ].string_value->string[ idx ];
					locals[ ins->z ].number_value++;
				} else {
					return throw( vars, this->source, locals[ ins->z ].number_value, "Not a string or index out of bounds." );
				}
				ins++;
				break;
			case LETV_UNP_VI:
				/* letv_unp_vi x y 0 imm : let x = $unpack( y imm ); */
				idx = ( int ) ins->imm;
				if( idx < string_bounds[ ins->y ] >> 2 ) {
					chr = locals[ ins->y ].string_value->string;
					locals[ ins->x ].number_value = ( ( signed char ) chr[ idx ] << 24 ) | ( ( unsigned char ) chr[ idx + 1 ] << 16 )
						| ( ( unsigned char ) chr[ idx + 2 ] << 8 ) | ( unsigned char ) chr[ idx + 3 ];
				} else {
					return throw( vars, this->source, ins->imm, "Not a string or index out of bounds." );
				}
				ins++;
				break;
			case LETV_UNP_VV:
				/* letv_unp_vv x y z   0 : let x = $unpack( y z ); */
				idx = ( int ) locals[ ins->z ].number_value;
				if( idx < string_bounds[ ins->y ] >> 2 ) {
					chr = locals[ ins->y ].string_value->string;
					locals[ ins->x ].number_value = ( ( signed char ) chr[ idx ] << 24 ) | ( ( unsigned char ) chr[ idx + 1 ] << 16 )
						| ( ( unsigned char ) chr[ idx + 2 ] << 8 ) | ( unsigned char ) chr[ idx + 3 ];
				} else {
					return throw( vars, this->source, ins->imm, "Not a string or index out of bounds." );
				}
				ins++;
				break;
			case LETV_UNP_VP:
				/* letv_unp_vp x y z   0 : let x = $unpack( y z++ ); */
				idx = ( int ) locals[ ins->z ].number_value;
				if( idx < string_bounds[ ins->y ] >> 2 ) {
					chr = locals[ ins->y ].string_value->string;
					locals[ ins->x ].number_value = ( ( signed char ) chr[ idx ] << 24 ) | ( ( unsigned char ) chr[ idx + 1 ] << 16 )
						| ( ( unsigned char ) chr[ idx + 2 ] << 8 ) | ( unsigned char ) chr[ idx + 3 ];
					locals[ ins->z ].number_value++;
				} else {
					return throw( vars, this->source, ins->imm, "Not a string or index out of bounds." );
				}
				ins++;
				break;
			default:
				return throw( vars, this->source, ins->opcode, "Illegal instruction." );
		}
	}
}

struct element* parse_asm_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct label labels = { 0 };
	struct asm_statement *stmt;
	int count;
	if( next->child ) {
		count = parse_instructions( next->child, func, NULL, &labels, message );
		if( count > 0 ) {
			stmt = calloc( 1, sizeof( struct asm_statement ) + sizeof( struct instruction ) * count );
			if( stmt ) {
				prev->next = &stmt->stmt;
				stmt->stmt.source = calloc( 1, sizeof( struct expression ) );
			}
			if( stmt && stmt->stmt.source ) {
				stmt->stmt.source->line = elem->line;
				stmt->instructions = ( struct instruction * ) &stmt[ 1 ];
				parse_instructions( next->child, func, stmt->instructions, &labels, message );
				stmt->stmt.execute = execute_asm_statement;
				next = next->next;
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		}
		dispose_labels( labels.next );
	} else {
		sprintf( message, "Empty asm statement on line %d.", next->line );
	}
	return next;
}

