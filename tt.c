
#include "alloca.h"
#include "errno.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "time.h"

/*
	Towntalk (c)2016 Martin Cameron.

	A program file consists of a list of declarations.
	Variables are integers, strings, elements or array references.
	Arrays are held in separate global variables to avoid reference cycles.
	When a '#' character is encountered, the rest of the line is ignored.
	Variable/Function/Array names must match "[A-Za-Z][A-Za-z0-9_]*".
	Strings have value-semantics and are immutable, can be used as byte arrays.
	Strings are non-null, but evaluate to zero in integer expressions.
	Commas within name and argument lists are optional.
	Elements are immutable trees of strings with next and child references.
	Element strings use the same syntax as program files.
	Elements are separated by whitespace, commas, ';' or '='.
	Elements may contain spaces and separators by enclosing them in quotes.
	Child elements are enclosed in parentheses, square brackets or braces.

	Example:
		rem { Test }
		function add( a b ) {
			# Sum two integers.
			return +( a b );
		}
		program main {
			var a;
			let a = 0;
			while <(a 10) {
				print a;
				let a = add( a 1 );
			}
		}

	Declarations:
		rem {}                   Comment (all brackets must be balanced).
		include "file.tt";       Include declarations from specified file.
		const name = value;      Integer, string or tuple constant.
		global a,b,c;            Global variables.
		array a,b,c;             Global arrays.
		function(a) {statements} Function declaration.
		program {statements}     Entry point function (no arguments).

	Statements:
		rem {}                   Comment.
		var a,b,c;               Local variable.
		let a = expr;            Variable assignment.
		print expr;              Write integer or string to standard output.
		write expr;              Same as print, but do not add a newline.
		error expr;              Same as print, but write to standard error.
		return expr;             Return from the current function.
		throw expr;              Return to the nearest catch statement.
		exit expr;               Terminate program with specified exit code.
		while expr {statements}  Repeat statements until expr is null.
		break;                   Exit current while statement.
		continue;                Stop current iteration of while statement.
		if expr {statements}     Execute statements if expr is non-null.
		   else {statements}     Optional, execute if expr is null.
		switch expr {            Selection statement for integers or strings.
		   case  1 {statements}  Execute statements if expr equals 1.
		   case "a"{statements}  Execute statements if expr equals "a".
		   default {statements}} Execute statements if no valid cases.
		try {statements}         Execute statements unless exception thrown.
		   catch a {statements}  Assign exception to local var and execute.
		call expr;               Evaluate expression and discard result.
		dim [arr len];           Resize specified array.
		set [arr idx] = expr;    Assign expression to array at index.
		aset arr = ${0,"a"};     Initialize array from element.
		inc a;                   Increment local variable.
		save str, "file";        Save bytes from string to file.
		append str, "file";      Append bytes to the end of file.

	Expressions:
		-123                     Decimal integer literal.
		0x100                    Hexadecimal integer literal.
		0888                     Octal integer literal.
		"String"                 String literal.
		${0,"1",$tup("2",3)}     Element literal.
		name                     Value of named local or global variable.
		name(expr, expr)         Call named function with specified args.
		[arr idx]                Array element.
		'(expr operator ...)     Infix operator, eg '( 1 + 2 ).
		+(int int)               Addition.
		-(int int)               Subtraction.
		*(int int)               Multiplication.
		/(int int)               Division.
		%(int int)               Modulo division.
		>>(int int)              Arithmetic shift-right.
		=(int int)               Equality.
		<(int int)               Less than.
		<e(int int)              Less than or equal.
		>(int int)               Greater than.
		>e(int int)              Greater than or equal.
		&(int int)               Bitwise AND.
		|(int int)               Bitwise OR.
		^(int int)               Bitwise XOR.
		~(int)                   Bitwise NOT.
		!(expr)                  Evaluates to 1 if argument is null.
		&&(expr)                 Evaluates to 1 if both arguments are non-null.
		||(expr)                 Evaluates to 1 if either argument is non-null.
		$str(str int ...)        Integer to string and string concatenation.
		$cmp(str str)            String/Tuple comparison, returns 0 if equal.
		$cat(str str ...)        String concatenation (same as $str).
		$chr(str idx)            Character at idx as integer.
		$sub(str off len)        Substring (or byte array to string).
		$asc(int)                Character code to string.
		$int(str)                String to integer.
		$len(str)                String/Array length.
		$tup(str int)            String/Integer tuple.
		$arr(arr)                Array to string (for aset).
		$load("abc.bin")         Load raw bytes into string.
		$flen("file")            Get the length of a file.
		$argc                    Number of command-line arguments.
		$argv(idx)               Command-line argument as string.
		$time                    Current time in seconds as octal string.
		$date(time)              Convert time string to human-readable date.
		$secs(time2 time1)       Difference between time strings as integer.
		$parse(str)              Parse string into element list.
		$next(elem)              Get the next element in the list or null.
		$child(elem)             Get the first child element or null.
		$quote(str)              Encode byte string with quotes and escapes.
		$unquote(str)            Decode quoted-string into byte string.
*/

static const int MAX_INTEGER = 0x7FFFFFFF;

/* Reference-counted type. */
struct element {
	size_t reference_count;
	int length, line;
	char *string;
	struct variable *array;
	struct element *child, *next;
};

/* Variable. */
struct variable {
	int integer_value;
	struct element *element_value;
};

struct string_list {
	char *value;
	struct string_list *next;
};

/* Global variables. */
struct global_variable {
	char *name;
	struct variable value;
	struct global_variable *next;
};

/* Expression list. */
struct expression {
	int line, index;
	struct global_variable *global;
	struct function_declaration *function;
	struct expression *parameters, *next;
	int ( *evaluate )( struct expression *this, struct variable *variables,
		struct variable *result, struct variable *exception );
};

/* Statement list. */
struct statement {
	int local;
	struct variable *global;
	struct expression *source, *destination, *index;
	struct statement *if_block, *else_block, *next;
	int ( *execute )( struct statement *this, struct variable *variables,
		struct variable *result, struct variable *exception );
};

/* Execution environment. */
struct environment {
	int argc;
	char **argv, *file;
	struct keyword *statements;
	struct operator *operators;
	struct global_variable *constants, *globals, *arrays;
	struct function_declaration *functions, *entry_point;
};

/* Parser keyword. */
struct keyword {
	char *name, *syntax;
	/* Parse the current declaration into env, or statement into prev->next. */
	struct element* (*parse)( struct element *elem, struct environment *env,
		struct function_declaration *func, struct statement *prev, char *message );
	struct keyword *next;
};

/* Parser operator. */
struct operator {
	char *name, oper;
	int num_operands;
	int ( *evaluate )( struct expression *this, struct variable *variables,
		struct variable *result, struct variable *exception );
	struct operator *next;
};

/* Function declaration list. */
struct function_declaration {
	char *name, *file;
	int line, num_parameters, num_variables;
	struct element *elem;
	struct environment *env;
	struct string_list *variable_decls, *variable_decls_tail;
	struct statement *statements, *statements_tail;
	struct function_declaration *next;
};

/* Forward declarations. */
static void dispose_variable( struct variable *var );
static int validate_name( char *name, struct environment *env );
static int validate_decl( struct element *elem, struct environment *env, char *message );
static int parse_tt_file( char *file_name, struct environment *env, char *message );
static void parse_keywords( struct keyword *keywords, struct element *elem,
	struct environment *env, struct function_declaration *func, struct statement *stmt, char *message );
static struct element* parse_decl_list( struct element *elem, struct environment *env,
	int (*add)( struct environment *env, struct element *elem, char *message ), char *message );
static struct element* parse_expression( struct element *elem, struct environment *env,
	struct function_declaration *func, struct expression *prev, char *message );
static int parse_expressions( struct element *elem, struct environment *env,
	struct function_declaration *func, struct expression *prev, char *message );
static struct element* parse_if_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message );
static struct element* parse_while_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message );
static struct element* parse_try_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message );
static struct element* parse_case_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message );
static struct element* parse_default_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message );

static int parse_string( char *buffer, int idx, struct element *elem, char *message ) {
	int length, offset = idx;
	char chr = buffer[ idx++ ];
	if( chr == '"' ) {
		while( ( chr & 0x7F ) >= 32 ) {
			chr = buffer[ idx++ ];
			if( chr == '\\' ) {
				chr = buffer[ idx++ ];
			} else if( chr == '"' ) {
				break;
			}
		}
		if( chr == '"' ) {
			length = idx - offset;
			elem->string = malloc( sizeof( char ) * ( length + 1 ) );
			if( elem->string ) {
				memcpy( &elem->string[ 0 ], &buffer[ offset ], length );
				elem->string[ length ] = 0;
				elem->length = length;
			} else {
				strcpy( message, "Out of memory." );
				idx = -1;
			}
		} else {
			sprintf( message, "Unclosed string on line %d.", elem->line );
			idx = -3;
		}
	} else {
		sprintf( message, "Expected '\"' on line %d.", elem->line );
		idx = -3;
	}
	return idx;
}

static int is_time_string( char *str ) {
	int idx = 0;
	char chr = str[ idx++ ];
	if( chr == '-' || chr == '0' ) {
		chr = str[ idx++ ];
		while( chr >= '0' && chr <= '7' ) {
			chr = str[ idx++ ];
		}
	}
	return ( idx > 1 && chr == 0 );
}

static time_t parse_time_string( char *str ) {
	int idx = 1;
	time_t seconds = 0;
	char chr = str[ idx++ ];
	while( chr ) {
		seconds = ( seconds << 3 ) | ( ( chr - '0' ) & 0x7 );
		chr = str[ idx++ ];
	}
	if( str[ 0 ] == '-' ) {
		seconds = -seconds;
	}
	return seconds;
}

static char* new_string( char *source ) {
	char *dest = malloc( sizeof( char ) * ( strlen( source ) + 1 ) );
	if( dest ) {
		strcpy( dest, source );
	}
	return dest;
}

static char* cat_string( char *left, int llen, char *right, int rlen ) {
	char *str = malloc( sizeof( char ) * ( llen + rlen + 1 ) );
	if( str ) {
		if( left ) {
			memcpy( str, left, llen );
		}
		if( right ) {
			memcpy( &str[ llen ], right, rlen );
		}
		str[ llen + rlen ] = 0;
	}
	return str;
}

static int unquote_string( char *string, char *output ) {
	int chr, offset = 0, length = 0;
	if( string ) {
		chr = string[ offset++ ];
		while( chr ) {
			if( chr == '\\' && string[ offset ] ) {
				chr = string[ offset++ ];
				if( chr >= '0' && chr <= '7' ) {
					chr = chr - '0';
					if( string[ offset ] >= '0' && string[ offset ] <= '7' ) {
						chr = ( chr << 3 ) | ( string[ offset++ ] - '0' );
						if( string[ offset ] >= '0' && string[ offset ] <= '7' ) {
							chr = ( chr << 3 ) | ( string[ offset++ ] - '0' );
						}
					}
				}
				if( output ) {
					output[ length++ ] = chr;
				} else {
					length++;
				}
			} else if( chr != '"' ) {
				if( output ) {
					output[ length++ ] = chr;
				} else {
					length++;
				}
			}
			chr = string[ offset++ ];
		}
		if( output ) {
			output[ length ] = 0;
		}
	}
	return length;
}

static int parse_child_element( char *buffer, int idx, struct element *parent, char *message ) {
	struct element *elem = NULL;
	int offset = idx, length = 0, line = parent->line;
	char chr = '\n';
	while( chr ) {
		chr = buffer[ idx++ ];
		if( chr <= 32 || strchr( "\"#(),;=[]{}", chr ) ) {
			if( length > 0 ) {
				if( elem == NULL ) {
					elem = calloc( 1, sizeof( struct element ) );
					parent->child = elem;
				} else {
					elem->next = calloc( 1, sizeof( struct element ) );
					elem = elem->next;
				}
				if( elem ) {
					elem->reference_count = 1;
					elem->line = line;
					elem->string = malloc( sizeof( char ) * ( length + 1 ) );
					if( elem->string ) {
						memcpy( elem->string, &buffer[ offset ], length );
						elem->string[ length ] = 0;
						elem->length = length;
						/*printf("%d %d %c :%s\n",offset,length,chr,elem->string);*/
					} else {
						strcpy( message, "Out of memory." );
						return -1;
					}
				} else {
					strcpy( message, "Out of memory." );
					return -1;
				}
			}
			if( chr == '\n' || chr == '#' ) {
				while( chr && chr != '\n' ) {
					chr = buffer[ idx++ ];
				}
				line++;
			} else if( chr && strchr( "\"(,;=[{", chr ) ) {
				if( elem == NULL ) {
					elem = calloc( 1, sizeof( struct element ) );
					parent->child = elem;
				} else {
					elem->next = calloc( 1, sizeof( struct element ) );
					elem = elem->next;
				}
				if( elem ) {
					elem->reference_count = 1;
					elem->line = line;
					if( chr == '"' ) {
						idx = parse_string( buffer, idx - 1, elem, message );
						if( idx < 0 ) {
							return idx;
						}
					} else {
						elem->string = malloc( sizeof( char ) * 2 );
						if( elem->string ) {
							elem->string[ 0 ] = chr;
							elem->string[ 1 ] = 0;
							elem->length = 1;
							if( chr != ',' && chr != ';' && chr != '=' ) {
								idx = parse_child_element( buffer, idx, elem, message );
								if( idx > 0 ) {
									/* Exchange line and elem->line. */
									line = elem->line - line;
									elem->line = elem->line - line;
									line = elem->line + line;
									offset = buffer[ idx - 1 ] - chr;
									if( offset < 1 || offset > 2 ) {
										sprintf( message, "Unclosed element on line %d.", line );
										return -2;
									}
								} else {
									return idx;
								}
							}
						} else {
							strcpy( message, "Out of memory." );
							return -1;
						}
					}
				} else {
					strcpy( message, "Out of memory." );
					return -1;
				}
			} else if( chr == ')' || chr == ']' || chr == '}' ) {
				parent->line = line;
				return idx;
			}
			offset = idx;
			length = 0;
		} else {
			length++;
		}
	}
	return idx;
}

static void dispose_element( struct element *elem ) {
	int idx, len;
	struct element *next;
	while( elem ) {
		elem->reference_count--;
		if( elem->reference_count == 0 ) {
			free( elem->string );
			if( elem->array ) {
				idx = 0, len = elem->length;
				while( idx < len ) {
					dispose_variable( &elem->array[ idx++ ] );
				}
				free( elem->array );
			}
			if( elem->child ) {
				dispose_element( elem->child );
			}
			next = elem->next;
			free( elem );
			elem = next;
		} else {
			elem = NULL;
		}
	}
}

static struct element* parse_element( char *buffer, char *message ) {
	int idx;
	struct element elem;
	elem.line = 1;
	elem.string = NULL;
	elem.child = elem.next = NULL;
	idx = parse_child_element( buffer, 0, &elem, message );
	if( idx > 0 ) {
		if( buffer[ idx - 1 ] != 0 ) {
			sprintf( message, "Unexpected closing bracket '%c' on line %d.", buffer[ idx - 1 ], elem.line );
			idx = -4;
		}
	}
	if( idx < 0 ) {
		dispose_element( elem.child );
		elem.child = NULL;
	}
	return elem.child;
}

static int load_file( char *file_name, char *buffer, char *message ) {
	long file_length = -1, bytes_read;
	FILE *input_file = fopen( file_name, "rb" );
	if( input_file != NULL ) {
		if( fseek( input_file, 0L, SEEK_END ) == 0 ) {
			file_length = ftell( input_file );
			if( file_length >= 0 && file_length < MAX_INTEGER && buffer ) {
				if( fseek( input_file, 0L, SEEK_SET ) == 0 ) {
					bytes_read = fread( buffer, 1, file_length, input_file ); 
					if( bytes_read != file_length ) {
						file_length = -1;
					}
				} else {
					file_length = -1;
				}
			}
		}
		fclose( input_file );
	}
	if( file_length < 0 ) {
		strncpy( message, strerror( errno ), 63 );
		message[ 63 ] = 0;
	} else if( file_length >= MAX_INTEGER ) {
		strcpy( message, "File too large." );
		file_length = -1;
	}
	return file_length;
}

static int save_file( char *file_name, char *buffer, int length, int append, char *message ) {
	int count = -1;
	FILE *output_file = fopen( file_name, append ? "ab" : "wb" );
	if( output_file != NULL ) {
		count = fwrite( buffer, 1, length, output_file );
		fclose( output_file );
	}
	if( count < length ) {
		strncpy( message, strerror( errno ), 63 );
		message[ 63 ] = 0;
	}
	return count;
}

static void dispose_string_list( struct string_list *str ) {
	struct string_list *next;
	while( str ) {
		next = str->next;
		free( str->value );
		free( str );
		str = next;
	}
}

static void dispose_variable( struct variable *var ) {
	if( var->element_value ) {
		dispose_element( var->element_value );
		var->element_value = NULL;
	}
}

static void assign_variable( struct variable *src, struct variable *dest ) {
	dispose_variable( dest );
	dest->integer_value = src->integer_value;
	dest->element_value = src->element_value;
	if( dest->element_value ) {
		dest->element_value->reference_count++;
	}
}

static void dispose_global_variables( struct global_variable *global ) {
	struct global_variable *next;
	while( global ) {
		next = global->next;
		free( global->name );
		dispose_variable( &global->value );
		free( global );
		global = next;
	}
}

static void dispose_expressions( struct expression *expr ) {
	struct expression *next;
	while( expr ) {
		next = expr->next;
		dispose_expressions( expr->parameters );
		free( expr );
		expr = next;
	}
}

static void dispose_statements( struct statement *statements ) {
	struct statement *next;
	while( statements ) {
		next = statements->next;
		dispose_expressions( statements->source );
		dispose_expressions( statements->destination );
		dispose_expressions( statements->index );
		dispose_statements( statements->if_block );
		dispose_statements( statements->else_block );
		free( statements );
		statements = next;
	}
}

static void dispose_function_declarations( struct function_declaration *function ) {
	struct function_declaration *next;
	while( function ) {
		next = function->next;
		free( function->name );
		free( function->file );
		dispose_string_list( function->variable_decls );
		dispose_statements( function->statements );
		free( function );
		function = next;
	}
}

static void dispose_environment( struct environment *env ) {
	if( env ) {
		dispose_global_variables( env->constants );
		dispose_global_variables( env->globals );
		dispose_global_variables( env->arrays );
		dispose_function_declarations( env->functions );
		free( env );
	}
}

static struct string_list *new_string_list( char *value ) {
	struct string_list *str = malloc( sizeof( struct string_list ) );
	if( str ) {
		str->value = new_string( value );
		if( str->value ) {
			str->next = NULL;
		} else {
			free( str );
			str = NULL;
		}
	}
	return str;
}

static int get_string_list_index( struct string_list *list, char *value ) {
	int idx = 0;
	while( list && strcmp( list->value, value ) ) {
		idx++;
		list = list->next;
	}
	if( list == NULL ) {
		idx = -1;
	}
	return idx;
}

static int throw( struct variable *exception, struct expression *source, int integer, char *string ) {
	struct element *arr = NULL;
	if( string ) {
		arr = calloc( 1, sizeof( struct element ) );
		if( arr ) {
			arr->reference_count = 1;
			arr->string = malloc( sizeof( char ) * ( strlen( string ) + 64 ) );
			if( arr->string ) {
				sprintf( arr->string, "%s (on line %d of '%.32s')",
					string, source->line, source->function->file );
				arr->length = strlen( arr->string );
			}
		}
	}
	dispose_variable( exception );
	exception->integer_value = integer;
	exception->element_value = arr;
	return 0;
}

static int write_byte_string( char *bytes, int count, char *output ) {
	int chr, idx = 0, length = 0;
	if( output ) {
		output[ length++ ] = '"';
		while( idx < count ) {
			chr = bytes[ idx++ ];
			if( chr == '"' || chr == '\\' ) {
				output[ length++ ] = '\\';
				output[ length++ ] = chr;
			} else if( ( chr & 0x7F ) < 32 || chr == 127 ) {
				output[ length++ ] = '\\';
				output[ length++ ] = '0' + ( ( chr >> 6 ) & 0x3 );
				output[ length++ ] = '0' + ( ( chr >> 3 ) & 0x7 );
				output[ length++ ] = '0' + ( chr & 0x7 );
			} else {
				output[ length++ ] = chr;
			}
		}
		output[ length++ ] = '"';
	} else {
		length++;
		while( idx < count ) {
			chr = bytes[ idx++ ];
			if( chr == '"' || chr == '\\' ) {
				length += 2;
			} else if( ( chr & 0x7F ) < 32 || chr == 127 ) {
				length += 4;
			} else {
				length++;
			}
		}
		length++;
	}
	return length;
}

static int write_variable( struct variable *var, char *output ) {
	int count, length = 0;
	char integer[ 32 ];
	if( output ) {
		if( var->element_value && var->element_value->string ) {
			if( var->integer_value ) {
				strcpy( output, "$tup(" );
				length += 5;
				length += write_byte_string( var->element_value->string, var->element_value->length, &output[ length ] );
				output[ length++ ] = ',';
				sprintf( integer, "%d", var->integer_value );
				count = strlen( integer );
				memcpy( &output[ length ], integer, count );
				length += count;
				output[ length++ ] = ')';
			} else {
				length += write_byte_string( var->element_value->string, var->element_value->length, output );
			}
		} else {
			sprintf( integer, "%d", var->integer_value );
			count = strlen( integer );
			memcpy( &output[ length ], integer, count );
			length += count;
		}
	} else {
		if( var->element_value && var->element_value->string ) {
			if( var->integer_value ) {
				sprintf( integer, "%d", var->integer_value );
				length += strlen( integer ) + 7;
			}
			length += write_byte_string( var->element_value->string, var->element_value->length, NULL );
		} else {
			sprintf( integer, "%d", var->integer_value );
			length += strlen( integer );
		}
	}
	return length;
}

static int write_array( struct element *arr, char *output ) {
	int idx = 0, length = 0, count = arr->length;
	if( output ) {
		output[ length++ ] = '{';
		while( idx < count ) {
			length += write_variable( &arr->array[ idx++ ], &output[ length ] );
			output[ length++ ] = ',';
			output[ length++ ] = '\n';
		}
		output[ length++ ] = '}';
	} else {
		length++;
		while( idx < count ) {
			length += write_variable( &arr->array[ idx++ ], NULL );
			length += 2;
		}
		length++;
	}
	return length;
}

static struct element* new_string_constant( char *value ) {
	int length;
	char *string;
	struct element *elem;
	length = unquote_string( value, NULL );
	string = malloc( sizeof( char ) * ( length + 1 ) );
	elem = calloc( 1, sizeof( struct element ) );
	if( elem && string ) {
		elem->reference_count = 1;
		elem->length = unquote_string( value, string );
		elem->string = string;
	} else {
		free( string );
		free( elem );
		elem = NULL;
	}
	return elem;
}

static struct element* parse_constant( struct element *elem, struct variable *constant, char *message ) {
	int integer_value = 0;
	struct element *child, *next = elem->next, *element_value = NULL;
	char *end = NULL;
	if( elem->string[ 0 ] == '"' ) {
		/* String literal. */
		element_value = new_string_constant( elem->string );
		if( element_value == NULL ) {
			strcpy( message, "Out of memory." );
		}
	} else if( elem->string[ 0 ] == '$' && elem->string[ 1 ] == 0 ) {
		/* Element. */
		if( next && next->string[ 0 ] == '{' ) {
			element_value = new_string_constant( "{" );
			if( element_value ) {
				if( next->child ) {
					next->child->reference_count++;
					element_value->child = next->child;
				}
				next = next->next;
			} else {
				strcpy( message, "Out of memory." );
			}
		} else {
			sprintf( message, "Expected '{' after '$' on line %d.", elem->line );
		}
	} else if( strcmp( elem->string, "$tup" ) == 0 ) {
		/* Tuple constant. */
		if( next && next->string[ 0 ] == '(' ) {
			child = next->child;
			if( child && child->string[ 0 ] == '"' ) {
				element_value = new_string_constant( child->string );
				if( element_value ) {
					child = child->next;
					if( child && child->string[ 0 ] == ',' ) {
						child = child->next;
					}
					if( child && child->next == NULL ) {
						integer_value = ( int ) strtol( child->string, &end, 0 );
						if( end[ 0 ] == 0 ) {
							next = next->next;
						} else {
							sprintf( message, "Invalid tuple integer on line %d.", next->line );
						}
					} else {
						sprintf( message, "Invalid tuple constant on line %d.", next->line );
					}
				} else {
					strcpy( message, "Out of memory." );
				}
			} else {
				sprintf( message, "Invalid tuple string on line %d.", next->line );
			}
		} else {
			sprintf( message, "Expected '(' after '$tup' on line %d.", elem->line );
		}
	} else {
		/* Integer constant. */
		integer_value = ( int ) strtol( elem->string, &end, 0 );
		if( end[ 0 ] != 0 ) {
			sprintf( message, "Invalid integer constant '%.16s' at line %d.", elem->string, elem->line );
		}
	}
	constant->integer_value = integer_value;
	constant->element_value = element_value;
	return next;
}

static struct global_variable* new_global_variable( char *name, char *message ) {
	struct global_variable *global = calloc( 1, sizeof( struct global_variable ) );
	if( global ) {
		/*printf("Global '%s'\n", name);*/
		global->name = new_string( name );
		if( global->name == NULL ) {
			free( global );
			global = NULL;
		}
	}
	if( global == NULL ) {
		strcpy( message, "Out of memory." );
	}
	return global;
}

static struct global_variable* new_array_variable( char *name ) {
	struct element *arr;
	struct global_variable *global = malloc( sizeof( struct global_variable ) );
	if( global ) {
		/*printf("Array '%s'\n", name);*/
		global->name = new_string( name );
		if( global->name ) {
			global->value.integer_value = 0;
			global->value.element_value = NULL;
			global->next = NULL;
			arr = calloc( 1, sizeof( struct element ) );
			if( arr ) {
				global->value.element_value = arr;
				arr->string = new_string( "#Array@" );
				if( arr->string ) {
					arr->reference_count = 1;
					arr->array = malloc( sizeof( struct variable ) );
					if( arr->array ) {
						arr->array->integer_value = 0;
						arr->array->element_value = NULL;
					} else {
						dispose_global_variables( global );
						global = NULL;
					}
				} else {
					dispose_global_variables( global );
					global = NULL;
				}
			} else {
				dispose_global_variables( global );
				global = NULL;
			}
		} else {
			free( global );
			global = NULL;
		}
	}
	return global;
}

static struct statement* new_statement( char *message ) {
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt == NULL ) {
		strcpy( message, "Out of memory." );
	}
	return stmt;
}

static int execute_global_assignment( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable *destination = this->global;
	return this->source->evaluate( this->source, variables, destination, exception );
}

static int execute_local_assignment( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable *destination = &variables[ this->local ];
	return this->source->evaluate( this->source, variables, destination, exception );
}

static int execute_print_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable value = { 0, NULL };
	if( this->source->evaluate( this->source, variables, &value, exception ) ) {
		if( value.element_value && value.element_value->string ) {
			puts( value.element_value->string );
		} else {
			printf( "%d\n", value.integer_value );
		}
		dispose_variable( &value );
		return 1;
	}
	return 0;
}

static int execute_error_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable value = { 0, NULL };
	if( this->source->evaluate( this->source, variables, &value, exception ) ) {
		if( value.element_value && value.element_value->string ) {
			fputs( value.element_value->string, stderr );
			fputc( '\n', stderr );
		} else {
			fprintf( stderr, "%d\n", value.integer_value );
		}
		dispose_variable( &value );
		return 1;
	}
	return 0;
}

static int execute_write_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable value = { 0, NULL };
	if( this->source->evaluate( this->source, variables, &value, exception ) ) {
		if( value.element_value && value.element_value->string ) {
			fwrite( value.element_value->string, 1, value.element_value->length, stdout );
		} else {
			printf( "%d", value.integer_value );
		}
		dispose_variable( &value );
		return 1;
	}
	return 0;
}

static int execute_throw_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	this->source->evaluate( this->source, variables, exception, exception );
	return 0;
}

static int execute_exit_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int code;
	this->source->evaluate( this->source, variables, exception, exception );
	code = exception->integer_value;
	dispose_variable( exception );
	exception->integer_value = code;
	exception->element_value = calloc( 1, sizeof( struct element ) );
	if( exception->element_value ) {
		exception->element_value->reference_count = 1;
	}
	return 0;
}

static int execute_return_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	if( this->source->evaluate( this->source, variables, result, exception ) ) {
		return 2;
	}
	return 0;
}

static int execute_break_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	return 3;
}

static int execute_continue_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	return 4;
}

static int execute_try_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret = 1;
	struct variable *exc = &variables[ this->local ];
	struct statement *stmt = this->if_block;
	while( stmt ) {
		ret = stmt->execute( stmt, variables, result, exc );
		if( ret == 1 ) {
			stmt = stmt->next;
		} else {
			break;
		}
	}
	if( ret == 0 ) {
		if( exc->element_value && exc->element_value->string == NULL ) {
			assign_variable( exc, exception );
		} else {
			ret = 1;
			stmt = this->else_block;
			while( stmt ) {
				ret = stmt->execute( stmt, variables, result, exception );
				if( ret == 1 ) {
					stmt = stmt->next;
				} else {
					break;
				}
			}
		}
	}
	return ret;
}

static int execute_if_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable condition = { 0, NULL };
	struct statement *stmt = this->else_block;
	int ret = this->source->evaluate( this->source, variables, &condition, exception );
	if( ret ) {
		if( condition.integer_value || condition.element_value ) {
			stmt = this->if_block;
		}
		dispose_variable( &condition );
		while( stmt ) {
			ret = stmt->execute( stmt, variables, result, exception );
			if( ret == 1 ) {
				stmt = stmt->next;
			} else {
				break;
			}
		}
	}
	return ret;
}

static int execute_while_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable condition = { 0, NULL };
	struct statement *stmt;
	int ret;
	while( this->source->evaluate( this->source, variables, &condition, exception ) ) {
		if( condition.integer_value || condition.element_value ) {
			dispose_variable( &condition );
			stmt = this->if_block;
			while( stmt ) {
				ret = stmt->execute( stmt, variables, result, exception );
				if( ret == 1 ) {
					stmt = stmt->next;
				} else if( ret == 2 ) {
					return 2;
				} else if( ret == 3 ) {
					return 1;
				} else if( ret == 4 ) {
					break;
				} else if( ret == 0 ) {
					return 0;
				}
			}
		} else {
			dispose_variable( &condition );
			return 1;
		}
	}
	return 0;
}

static int execute_call_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable var = { 0, NULL };
	int ret = this->source->evaluate( this->source, variables, &var, exception );
	if( ret ) {
		dispose_variable( &var );
	}
	return ret;
}

static int execute_dim_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, idx, count;
	struct variable arr = { 0, NULL }, len = { 0, NULL }, *old, *new;
	ret = this->destination->evaluate( this->destination, variables, &arr, exception );
	if( ret ) {
		ret = this->source->evaluate( this->source, variables, &len, exception );
		if( ret ) {
			if( arr.element_value && arr.element_value->array ) {
				old = arr.element_value->array;
				if( len.integer_value >= 0 ) {
					new = calloc( len.integer_value + 1, sizeof( struct variable ) );
					if( new ) {
						count = arr.element_value->length;
						if( count > len.integer_value ) {
							memcpy( new, old, sizeof( struct variable ) * len.integer_value );
							idx = len.integer_value;
							while( idx < count ) {
								dispose_variable( &old[ idx++ ] );
							}
						} else {
							memcpy( new, old, sizeof( struct variable ) * count );
						}
						arr.element_value->array = new;
						arr.element_value->length = len.integer_value;
						free( old );
					} else {
						ret = throw( exception, NULL, 0, NULL );
					}
				} else {
					ret = throw( exception, this->source, 0, "Negative array size." );
				}
			} else {
				ret = throw( exception, this->destination, 0, "Not an array." );
			}
			dispose_variable( &len );
		}
		dispose_variable( &arr );
	}
	return ret;
}

static int execute_set_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable arr = { 0, NULL }, idx = { 0, NULL };
	int ret = this->destination->evaluate( this->destination, variables, &arr, exception );
	if( ret ) {
		ret = this->index->evaluate( this->index, variables, &idx, exception );
		if( ret ) {
			if( arr.element_value && arr.element_value->array ) {
				if( idx.integer_value >= 0 && idx.integer_value < arr.element_value->length ) {
					ret = this->source->evaluate( this->source, variables,
						&arr.element_value->array[ idx.integer_value ], exception );
				} else {
					ret = throw( exception, this->index, idx.integer_value, "Array index out of bounds." );
				}
			} else {
				ret = throw( exception, this->destination, 0, "Not an array." );
			}
			dispose_variable( &idx );
		}
		dispose_variable( &arr );
	}
	return ret;
}

static int parse_constant_list( struct element *elem, struct global_variable *prev, char *message ) {
	int count = 0;
	while( elem && message[ 0 ] == 0 ) {
		prev->next = new_global_variable( "#Const#", message );
		if( prev->next ) {
			count++;
			prev = prev->next;
			elem = parse_constant( elem, &prev->value, message );
			if( elem && elem->string[ 0 ] == ',' ) {
				elem = elem->next;
			}
		}
	}
	return count;
}

static int execute_aset_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, idx, length, newlen;
	char msg[ 128 ] = "";
	struct global_variable inputs, *input;
	struct variable dest = { 0, NULL }, src = { 0, NULL }, *values, *newvar;
	ret = this->destination->evaluate( this->destination, variables, &dest, exception );
	if( ret ) {
		if( dest.element_value && dest.element_value->array ) {
			ret = this->source->evaluate( this->source, variables, &src, exception );
			if( ret ) {
				if( src.element_value && src.element_value->string[ 0 ] == '{' ) {
					inputs.next = NULL;
					newlen = parse_constant_list( src.element_value->child, &inputs, msg );
					if( msg[ 0 ] == 0 ) {
						newvar = calloc( newlen + 1, sizeof( struct variable ) );
						if( newvar ) {
							idx = 0;
							length = dest.element_value->length;
							values = dest.element_value->array;
							while( idx < length ) {
								dispose_variable( &values[ idx++ ] );
							}
							free( values );
							dest.element_value->length = newlen;
							dest.element_value->array = newvar;
							idx = 0;
							input = inputs.next;
							while( idx < newlen ) {
								assign_variable( &input->value, &newvar[ idx++ ] );
								input = input->next;
							}
						} else {
							ret = throw( exception, NULL, 0, NULL );
						}
					} else {
						ret = throw( exception, this->source, 0, msg );
					}
					dispose_global_variables( inputs.next );
				} else {
					ret = throw( exception, this->source, 0, "Invalid array element." );
				}
				dispose_variable( &src );
			}
		} else {
			ret = throw( exception, this->destination, 0, "Not an array." );
		}
		dispose_variable( &dest );
	}
	return ret;
}

static int execute_switch_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, length;
	struct statement *stmt = this->if_block;
	struct variable switch_value = { 0, NULL }, *case_value;
	ret = this->source->evaluate( this->source, variables, &switch_value, exception );
	if( ret ) {
		ret = 1;
		while( stmt ) {
			case_value = stmt->global;
			if( case_value->element_value ) {
				length = case_value->element_value->length;
				if( switch_value.element_value
				&& switch_value.element_value->length == length
				&& !memcmp( switch_value.element_value->string, case_value->element_value->string, length )
				&& switch_value.integer_value == case_value->integer_value ) {
					ret = stmt->execute( stmt, variables, result, exception );	
					break;
				}
			} else if( switch_value.element_value == NULL
				&& switch_value.integer_value == case_value->integer_value ) {
				ret = stmt->execute( stmt, variables, result, exception );
				break;
			}
			stmt = stmt->next;
		}
		if( stmt == NULL && this->else_block ) {
			ret = this->else_block->execute( this->else_block, variables, result, exception );
		}
		dispose_variable( &switch_value );
	}
	return ret;
}

static int execute_case_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct statement *stmt = this->if_block;
	int ret = 1;
	while( stmt ) {
		ret = stmt->execute( stmt, variables, result, exception );
		if( ret == 1 ) {
			stmt = stmt->next;
		} else {
			break;
		}
	}
	return ret;
}

static int execute_increment_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable *local = &variables[ this->local ];
	if( local->element_value ) {
		return throw( exception, this->source, 0, "Not an integer." );
	} else {
		local->integer_value++;
	}
	return 1;
}

static int execute_save_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, count;
	char message[ 64 ];
	struct element *sval, *fval;
	struct variable string = { 0, NULL }, file = { 0, NULL };
	ret = this->source->evaluate( this->source, variables, &string, exception );
	if( ret ) {
		ret = this->destination->evaluate( this->destination, variables, &file, exception );
		if( ret ) {
			sval = string.element_value;
			fval = file.element_value;
			if( sval && sval->string && fval && fval->string ) {
				count = save_file( fval->string, sval->string, sval->length, this->local, message );
				if( count != sval->length ) {
					ret = throw( exception, this->source, 0, message );
				}
			} else {
				ret = throw( exception, this->source, 0, "Not a string." );
			}
			dispose_variable( &file );
		}
		dispose_variable( &string );
	}
	return ret;
}

static int evaluate_local( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	assign_variable( &variables[ this->index ], result );
	return 1;
}

static int evaluate_global( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	assign_variable( &this->global->value, result );
	return 1;
}

static int evaluate_function_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int idx, count, ret = 1;
	struct statement *stmt;
	struct variable *locals;
	struct expression *parameter;
	struct function_declaration *function = this->function;
	count = sizeof( struct variable ) * function->num_variables;
	locals = alloca( count );
	memset( locals, 0, count );
	parameter = this->parameters;
	idx = 0, count = function->num_parameters;
	while( idx < count ) {
		ret = parameter->evaluate( parameter, variables, &locals[ idx++ ], exception );
		if( ret ) {
			parameter = parameter->next;
		} else {
			break;
		}
	}
	if( ret ) {
		stmt = function->statements;
		while( stmt ) {
			ret = stmt->execute( stmt, locals, result, exception );
			if( ret == 1 ) {
				stmt = stmt->next;
			} else {
				if( ret > 2 ) {
					ret = throw( exception, this, ret, "Unhandled 'break' or 'continue'." );
				}
				ret = ( ret > 0 );
				break;
			}
		}
	}
	idx = 0, count = function->num_variables;
	while( idx < count ) {
		dispose_variable( &locals[ idx++ ] );
	}
	return ret;
}

static int evaluate_index_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret;
	struct expression *parameter = this->parameters;
	struct variable arr = { 0, NULL }, idx = { 0, NULL };
	ret = parameter->evaluate( parameter, variables, &arr, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &idx, exception );
		if( ret ) {
			if( arr.element_value && arr.element_value->array ) {
				if( idx.integer_value >= 0 && idx.integer_value < arr.element_value->length ) {
					assign_variable( &arr.element_value->array[ idx.integer_value ], result );
				} else {
					ret = throw( exception, this, idx.integer_value, "Array index out of bounds." );
				}
			} else {
				ret = throw( exception, this, 0, "Not an array." );
			}
			dispose_variable( &idx );
		}
		dispose_variable( &arr );
	}
	return ret;
}

static struct function_declaration* new_function_declaration( char *name, char *file ) {
	struct function_declaration *func = malloc( sizeof( struct function_declaration ) );
	if( func ) {
		/*printf("Function '%s'\n", name);*/
		func->name = new_string( name );
		if( func->name ) {
			func->file = new_string( file );
		}
		if( func->name && func->file ) {
			func->line = func->num_parameters = func->num_variables = 0;
			func->elem = NULL;
			func->env = NULL;
			func->variable_decls = func->variable_decls_tail = NULL;
			func->statements = func->statements_tail = NULL;
			func->next = NULL;
		} else {
			free( func );
			func = NULL;
		}
	}
	return func;
}

static struct function_declaration* get_function_declaration( struct function_declaration *functions, char *name ) {
	while( functions && strcmp( functions->name, name ) ) {
		functions = functions->next;
	}
	return functions;
}

static struct global_variable* get_global_variable( struct global_variable *globals, char *name ) {
	while( globals && strcmp( globals->name, name ) ) {
		globals = globals->next;
	}
	return globals;
}

static int add_global_variable( struct environment *env, struct element *elem, char *message ) {
	struct global_variable *global;
	global = new_global_variable( elem->string, message );
	if( global ) {
		global->next = env->globals;
		env->globals = global;
	}
	return message[ 0 ] == 0;
}

static int add_array_variable( struct environment *env, struct element *elem, char *message ) {
	char *name = elem->string;
	struct global_variable *arr = new_array_variable( name );
	if( arr ) {
		arr->next = env->arrays;
		env->arrays = arr;
	} else {
		strcpy( message, "Out of memory." );
	}
	return message[ 0 ] == 0;
}

static int add_function_parameter( struct environment *env, struct element *elem, char *message ) {
	char *name = elem->string;
	struct function_declaration *func = env->functions;
	struct string_list *param = new_string_list( name );
	if( param ) {
		/*printf("Function parameter '%s'\n", name);*/
		if( get_string_list_index( func->variable_decls, name ) < 0 ) {
			func->num_parameters = func->num_variables = func->num_parameters + 1;
			if( func->variable_decls ) {
				func->variable_decls_tail->next = param;
			} else {
				func->variable_decls = param;
			}
			func->variable_decls_tail = param;
		} else {
			dispose_string_list( param );
			sprintf( message, "Parameter '%.16s' already defined on line %d.", name, elem->line );
		}
	} else {
		strcpy( message, "Out of memory." );
	}
	return message[ 0 ] == 0;
}

static int add_local_variable( struct environment *env, struct element *elem, char *message ) {
	char *name = elem->string;
	struct function_declaration *func = env->entry_point;
	struct string_list *param = new_string_list( name );
	if( param ) {
		/*printf("Local variable '%s'\n", name);*/
		if( get_string_list_index( func->variable_decls, name ) < 0 ) {
			func->num_variables = func->num_variables + 1;
			if( func->variable_decls ) {
				func->variable_decls_tail->next = param;
			} else {
				func->variable_decls = param;
			}
			func->variable_decls_tail = param;
		} else {
			dispose_string_list( param );
			sprintf( message, "Local variable '%.8s' already defined on line %d.", name, elem->line );
		}
	} else {
		strcpy( message, "Out of memory." );
	}
	return message[ 0 ] == 0;
}

static struct element* parse_variable_declaration( struct element *elem, struct environment *env,
	int (*add)( struct environment *env, struct element *elem, char *message ), char *message ) {
	struct element *next = elem->next;
	next = parse_decl_list( next, env, add, message );
	if( next && next->string[ 0 ] == ';' ) {
		next = next->next;
	}
	return next;
}

static struct element* parse_const_declaration( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct global_variable *constant;
	char *name = next->string;
	next = next->next->next;
	constant = new_global_variable( name, message );
	if( constant ) {
		constant->next = env->constants;
		env->constants = constant;
		next = parse_constant( next, &constant->value, message );
	}
	return next->next;
}

static struct element* parse_comment( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return elem->next->next;
}

static struct element* parse_global_declaration( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_variable_declaration( elem, env, add_global_variable, message);
}

static struct element* parse_array_declaration( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_variable_declaration( elem, env, add_array_variable, message);
}

static struct element* parse_local_declaration( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_variable_declaration( elem, env, add_local_variable, message);
}

static int evaluate_int_not_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable var = { 0, NULL };
	int ret = this->parameters->evaluate( this->parameters, variables, &var, exception );
	if( ret ) {
		dispose_variable( result );
		result->integer_value = ~var.integer_value;
		result->element_value = NULL;
		dispose_variable( &var );
	}
	return ret;
}

static int evaluate_logical_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable var = { 0, NULL };
	struct expression *parameter = this->parameters;
	int value, ret = parameter->evaluate( parameter, variables, &var, exception );
	if( ret ) {
		parameter = parameter->next;
		value = var.integer_value || var.element_value;
		dispose_variable( &var );
		if( this->index == '!' ) {
			dispose_variable( result );
			result->integer_value = !value;
			result->element_value = NULL;
		} else if( this->index == '&' ) {
			if( value ) {
				ret = parameter->evaluate( parameter, variables, &var, exception );
				if( ret ) {
					dispose_variable( result );
					result->integer_value = var.integer_value || var.element_value;
					result->element_value = NULL;
					dispose_variable( &var );
				}
			} else {
				dispose_variable( result );
				result->integer_value = value;
				result->element_value = NULL;
			}
		} else {
			if( value ) {
				dispose_variable( result );
				result->integer_value = value;
				result->element_value = NULL;
			} else {
				ret = parameter->evaluate( parameter, variables, &var, exception );
				if( ret ) {
					dispose_variable( result );
					result->integer_value = var.integer_value || var.element_value;
					result->element_value = NULL;
					dispose_variable( &var );
				}
			}
		}
	}
	return ret;
}

static int calculate_integer_operation( struct expression *expr,
	int lhs, int rhs, struct variable *result, struct variable *exception ) {
	int value, ret = 1;
	switch( expr->index ) {
		case '%':
			if( rhs != 0 ) {
				value = lhs % rhs;
			} else {
				value = 0;
				ret = throw( exception, expr, 0, "Modulo division by zero." );
			}
			break;
		case '&': value = lhs  & rhs; break;
		case '*': value = lhs  * rhs; break;
		case '+': value = lhs  + rhs; break;
		case '-': value = lhs  - rhs; break;
		case '/':
			if( rhs != 0 ) {
				value = lhs / rhs;
			} else {
				value = 0;
				ret = throw( exception, expr, 0, "Integer division by zero." );
			}
			break;
		case '<': value = lhs  < rhs; break;
		case '>': value = lhs  > rhs; break;
		case 'A': value = lhs >> rhs; break;
		case 'G': value = lhs >= rhs; break;
		case 'L': value = lhs <= rhs; break;
		case '^': value = lhs  ^ rhs; break;
		case '=': value = lhs == rhs; break;
		case '|': value = lhs  | rhs; break;
		default :
			value = 0;
			ret = throw( exception, expr, 0, "Unhandled integer operator." );
			break;
	}
	if( ret ) {
		dispose_variable( result );
		result->integer_value = value;
		result->element_value = NULL;
	}
	return ret;
}

static int evaluate_integer_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable lhs = { 0, NULL }, rhs = { 0, NULL };
	struct expression *parameter = this->parameters;
	int ret = parameter->evaluate( parameter, variables, &lhs, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &rhs, exception );
		if( ret ) {
			ret = calculate_integer_operation( this, lhs.integer_value, rhs.integer_value, result, exception );
			dispose_variable( &rhs );
		}
		dispose_variable( &lhs );
	}
	return ret;
}

static int evaluate_fastint_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int lhs, rhs;
	struct expression *parameter = this->parameters;
	if( parameter->global ) {
		lhs = parameter->global->value.integer_value;
	} else {
		lhs = variables[ parameter->index ].integer_value;
	}
	parameter = parameter->next;
	if( parameter->global ) {
		rhs = parameter->global->value.integer_value;
	} else {
		rhs = variables[ parameter->index ].integer_value;
	}
	return calculate_integer_operation( this, lhs, rhs, result, exception );
}

static int evaluate_sint_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, val;
	char *end;
	struct variable str = { 0, NULL };
	ret = this->parameters->evaluate( this->parameters, variables, &str, exception );
	if( ret ) {
		if( str.element_value && str.element_value->string ) {
			val = ( int ) strtol( str.element_value->string, &end, 0 );
			if( end[ 0 ] == 0 && str.element_value->string != end ) {
				dispose_variable( result );
				result->integer_value = val;
				result->element_value = NULL;
			} else {
				ret = throw( exception, this, 0, "Unable to convert string to integer." );
			}
		} else {
			ret = throw( exception, this, 0, "Not a string." );
		}
		dispose_variable( &str );
	}
	return ret;
}

static int evaluate_sstr_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret = 1, len = 0, vallen;
	char num[ 24 ], *str = NULL, *new;
	struct expression *parameter = this->parameters;
	struct variable val = { 0, NULL };
	struct element *arr;
	while( parameter && ret ) {
		ret = parameter->evaluate( parameter, variables, &val, exception );
		if( ret ) {
			if( val.element_value && val.element_value->string ) {
				vallen = val.element_value->length;
				new = val.element_value->string;
			} else {
				sprintf( num, "%d", val.integer_value );
				vallen = strlen( num );
				new = num;
			}
			if( MAX_INTEGER - vallen > len ) {
				new = cat_string( str, len, new, vallen );
				free( str );
				str = new;
				len += vallen;
			} else {
				ret = throw( exception, this, len, "String too large." );
			}
			dispose_variable( &val );
			parameter = parameter->next;
		}
	}
	if( ret ) {
		arr = calloc( 1, sizeof( struct element ) );
		if( arr && str ) {
			arr->reference_count = 1;
			arr->string = str;
			arr->length = len;
			dispose_variable( result );
			result->integer_value = 0;
			result->element_value = arr;
		} else {
			free( arr );
			free( str );
			ret = throw( exception, this, 0, NULL );
		}
	} else {
		free( str );
	}
	return ret;
}

static int evaluate_sasc_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret;
	char *str;
	struct element *arr;
	struct variable val = { 0, NULL };
	ret = this->parameters->evaluate( this->parameters, variables, &val, exception );
	if( ret ) {
		arr = calloc( 1, sizeof( struct element ) );
		str = malloc( sizeof( char ) * 2 );
		if( arr && str ) {
			arr->reference_count = arr->length = 1;
			arr->string = str;
			arr->string[ 0 ] = val.integer_value;
			arr->string[ 1 ] = 0;
			dispose_variable( result );
			result->integer_value = 0;
			result->element_value = arr;
		} else {
			free( arr );
			free( str );
			ret = throw( exception, this, 0, NULL );
		}
		dispose_variable( &val );
	}
	return ret;
}

static int evaluate_slen_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret;
	struct variable len = { 0, NULL };
	ret = this->parameters->evaluate( this->parameters, variables, &len, exception );
	if( ret ) {
		if( len.element_value ) {
			dispose_variable( result );
			result->integer_value = len.element_value->length;
			result->element_value = NULL;
		} else {
			ret = throw( exception, this, 0, "Not a string or array." );
		}
		dispose_variable( &len );
	}
	return ret;
}

static int evaluate_stup_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret;
	struct expression *parameter = this->parameters;
	struct variable str = { 0, NULL }, val = { 0, NULL };
	ret = parameter->evaluate( parameter, variables, &str, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &val, exception );
		if( ret ) {
			assign_variable( &str, result );
			result->integer_value = val.integer_value;
			dispose_variable( &val );
		}
		dispose_variable( &str );
	}
	return ret;
}

static int evaluate_sload_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, len;
	char message[ 64 ], *buf;
	struct element *arr;
	struct variable file = { 0, NULL };
	ret = this->parameters->evaluate( this->parameters, variables, &file, exception );
	if( ret ) {
		if( file.element_value && file.element_value->string ) {
			len = load_file( file.element_value->string, NULL, message );
			if( len >= 0 ) {
				buf = malloc( len + 1 );
				if( buf ) {
					len = load_file( file.element_value->string, buf, message );
					if( len >= 0 ) {
						buf[ len ] = 0;
						arr = calloc( 1, sizeof( struct element ) );
						if( arr ) {
							arr->reference_count = 1;
							arr->length = len;
							arr->string = buf;
							dispose_variable( result );
							result->integer_value = 0;
							result->element_value = arr;
						} else {
							free( buf );
							ret = throw( exception, this, 0, NULL );
						}
					} else {
						free( buf );
						ret = throw( exception, this, 0, message );
					}
				} else {
					ret = throw( exception, this, 0, NULL );
				}
			} else {
				ret = throw( exception, this, 0, message );
			}
		} else {
			ret = throw( exception, this, 0, "Not a string." );
		}
		dispose_variable( &file );
	}
	return ret;
}

static int evaluate_sflen_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, len;
	char message[ 64 ];
	struct variable file = { 0, NULL };
	ret = this->parameters->evaluate( this->parameters, variables, &file, exception );
	if( ret ) {
		if( file.element_value && file.element_value->string ) {
			len = load_file( file.element_value->string, NULL, message );
			if( len >= 0 ) {
				dispose_variable( result );
				result->integer_value = len;
				result->element_value = NULL;
			} else {
				ret = throw( exception, this, 0, message );
			}
		} else {
			ret = throw( exception, this, 0, "Not a string." );
		}
		dispose_variable( &file );
	}
	return ret;
}

static int evaluate_scmp_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret;
	struct element *arr1, *arr2;
	struct expression *parameter = this->parameters;
	struct variable str1 = { 0, NULL }, str2 = { 0, NULL };
	ret = parameter->evaluate( parameter, variables, &str1, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &str2, exception );
		if( ret ) {
			arr1 = str1.element_value;
			arr2 = str2.element_value;
			if( arr1 && arr1->string && arr2 && arr2->string ) {
				dispose_variable( result );
				if( arr1->length == arr2->length && str1.integer_value == str2.integer_value ) {
					result->integer_value = memcmp( arr1->string, arr2->string, arr1->length );
				} else if( arr1->length > arr2->length || str1.integer_value > str2.integer_value ){
					result->integer_value = 1;
				} else {
					result->integer_value = -1;
				}
				result->element_value = NULL;
			} else {
				ret = throw( exception, this, 0, "Not a string." );
			}
			dispose_variable( &str2 );
		}
		dispose_variable( &str1 );
	}
	return ret;
}

static int evaluate_schr_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret;
	struct expression *parameter = this->parameters;
	struct variable str = { 0, NULL }, idx = { 0, NULL };
	ret = parameter->evaluate( parameter, variables, &str, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &idx, exception );
		if( ret ) {
			if( str.element_value && str.element_value->string ) {
				if( idx.integer_value >= 0 && idx.integer_value < str.element_value->length ) {
					dispose_variable( result );
					result->integer_value = str.element_value->string[ idx.integer_value ];
					result->element_value = NULL;
				} else {
					ret = throw( exception, this, idx.integer_value, "String index out of bounds." );
				}
			} else {
				ret = throw( exception, this, 0, "Not a string." );
			}
			dispose_variable( &idx );
		}
		dispose_variable( &str );
	}
	return ret;
}

static int evaluate_ssub_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	char *data;
	int ret, offset;
	struct element *arr;
	struct expression *parameter = this->parameters;
	struct variable str = { 0, NULL }, idx = { 0, NULL }, len = { 0, NULL }, *var;
	ret = parameter->evaluate( parameter, variables, &str, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &idx, exception );
		if( ret ) {
			parameter = parameter->next;
			ret = parameter->evaluate( parameter, variables, &len, exception );
			if( ret ) {
				if( str.element_value && str.element_value->string ) {
					if( idx.integer_value >= 0 && len.integer_value >= 0
						&& MAX_INTEGER - len.integer_value >= idx.integer_value
						&& idx.integer_value + len.integer_value <= str.element_value->length ) {
						arr = calloc( 1, sizeof( struct element ) );
						if( arr ) {
							arr->reference_count = 1;
							arr->string = malloc( sizeof( char ) * ( len.integer_value + 1 ) );
							if( arr->string ) {
								arr->length = len.integer_value;
								if( str.element_value->array ) {
									data = arr->string;
									var = str.element_value->array;
									offset = 0;
									while( offset < len.integer_value ) {
										data[ offset ] = var[ offset + idx.integer_value ].integer_value;
										offset++;
									}
								} else {
									memcpy( arr->string, &str.element_value->string[ idx.integer_value ],
										sizeof( char ) * len.integer_value );
								}
								arr->string[ len.integer_value ] = 0;
								dispose_variable( result );
								result->integer_value = 0;
								result->element_value = arr;
							} else {
								free( arr );
								ret = throw( exception, this, 0, NULL );
							}
						} else {
							ret = throw( exception, this, 0, NULL );
						}
					} else {
						ret = throw( exception, this, idx.integer_value, "Range out of bounds." );
					}
				} else {
					ret = throw( exception, this, 0, "Not a string or array." );
				}	
				dispose_variable( &len );
			}
			dispose_variable( &idx );
		}
		dispose_variable( &str );
	}
	return ret;
}

static int evaluate_sarr_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct element *arr;
	struct expression *parameter = this->parameters;
	struct variable input = { 0, NULL };
	int ret = parameter->evaluate( parameter, variables, &input, exception );
	if( ret ) {
		if( input.element_value && input.element_value->array ) {
			arr = calloc( 1, sizeof( struct element ) );
			if( arr ) {
				arr->reference_count = 1;
				arr->string = malloc( sizeof( char ) * ( write_array( input.element_value, NULL ) + 1 ) );
				if( arr->string ) {
					arr->length = write_array( input.element_value, arr->string );
					arr->string[ arr->length ] = 0;
					dispose_variable( result );
					result->integer_value = 0;
					result->element_value = arr;
				} else {
					free( arr );
					ret = throw( exception, this, 0, NULL );
				}
			} else {
				ret = throw( exception, this, 0, NULL );
			}
		} else {
			ret = throw( exception, this, 0, "Not an array." );
		}
		dispose_variable( &input );
	}
	return ret;
}

static int evaluate_sargc_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = this->function->env->argc;
	result->element_value = NULL;
	return 1;
}

static int evaluate_sargv_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret;
	struct element *arr;
	struct expression *parameter = this->parameters;
	struct variable idx = { 0, NULL };
	ret = parameter->evaluate( parameter, variables, &idx, exception );
	if( ret ) {
		if( idx.integer_value >= 0 && idx.integer_value < this->function->env->argc ) {
			arr = calloc( 1, sizeof( struct element ) );
			if( arr ) {
				arr->reference_count = 1;
				arr->string = new_string( this->function->env->argv[ idx.integer_value ] );
				if( arr->string ) {
					arr->length = strlen( arr->string );
					dispose_variable( result );
					result->integer_value = 0;
					result->element_value = arr;
				} else {
					free( arr );
					ret = throw( exception, this, 0, NULL );
				}
			} else {
				ret = throw( exception, this, 0, NULL );
			}
		} else {
			ret = throw( exception, this, idx.integer_value, "Command-line argument index out of bounds." );
		}
		dispose_variable( &idx );
	}
	return ret;
}

static int evaluate_stime_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int idx, ret = 1;
	char value[ 16 ];
	time_t seconds = time( NULL );
	struct element *arr = calloc( 1, sizeof( struct element ) );
	if( arr ) {
		idx = 14;
		value[ 15 ] = 0;
		while( idx > 0 ) {
			value[ idx-- ] = '0' + ( seconds & 0x7 );
			seconds = seconds >> 3;
		}
		value[ 0 ] = '0';
		arr->reference_count = 1;
		arr->string = new_string( value );
		if( arr->string ) {
			arr->length = strlen( arr->string );
			dispose_variable( result );
			result->integer_value = 0;
			result->element_value = arr;
		} else {
			free( arr );
			ret = throw( exception, this, 0, NULL );
		}
	} else {
		ret = throw( exception, this, 0, NULL );
	}
	return ret;
}

static int evaluate_sdate_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	time_t seconds;
	struct element *arr;
	struct expression *parameter = this->parameters;
	struct variable input = { 0, NULL };
	int ret = parameter->evaluate( parameter, variables, &input, exception );
	if( ret ) {
		if( input.element_value && input.element_value->string && is_time_string( input.element_value->string ) ) {
			seconds = parse_time_string( input.element_value->string );
			arr = calloc( 1, sizeof( struct element ) );
			if( arr ) {
				arr->reference_count = 1;
				arr->string = new_string( ctime( &seconds ) );
				if( arr->string ) {
					arr->length = strlen( arr->string );
					arr->string[ arr->length - 1 ] = 0;
					dispose_variable( result );
					result->integer_value = 0;
					result->element_value = arr;
				} else {
					free( arr );
					ret = throw( exception, this, 0, NULL );
				}
			} else {
				ret = throw( exception, this, 0, NULL );
			}
		} else {
			ret = throw( exception, this, 0, "Not a valid time string." );
		}
		dispose_variable( &input );
	}
	return ret;
}

static int evaluate_ssecs_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	time_t seconds;
	struct element *arr1, *arr2;
	struct expression *parameter = this->parameters;
	struct variable time1 = { 0, NULL }, time2 = { 0, NULL };
	int ret = parameter->evaluate( parameter, variables, &time1, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &time2, exception );
		if( ret ) {
			arr1 = time1.element_value;
			arr2 = time2.element_value;
			if( arr1 && arr1->string && is_time_string( arr1->string )
			&&  arr2 && arr2->string && is_time_string( arr2->string ) ) {
				seconds = parse_time_string( arr1->string ) - parse_time_string( arr2->string );
				dispose_variable( result );
				result->integer_value = ( int ) seconds;
				result->element_value = NULL;
			} else {
				ret = throw( exception, this, 0, "Not a valid time string." );
			}
			dispose_variable( &time2 );
		}
		dispose_variable( &time1 );
	}
	return ret;
}

static int evaluate_snext_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable prev = { 0, NULL };
	struct element *next;
	int ret = parameter->evaluate( parameter, variables, &prev, exception );
	if( ret ) {
		if( prev.element_value ) {
			next = prev.element_value->next;
			if( next ) {
				next->reference_count++;
			}
			dispose_variable( result );
			result->integer_value = 0;
			result->element_value = next;
		} else {
			ret = throw( exception, this, 0, "Not an element." );
		}
		dispose_variable( &prev );
	}
	return ret;
}

static int evaluate_schild_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable parent = { 0, NULL };
	struct element *child;
	int ret = parameter->evaluate( parameter, variables, &parent, exception );
	if( ret ) {
		if( parent.element_value ) {
			child = parent.element_value->child;
			if( child ) {
				child->reference_count++;
			}
			dispose_variable( result );
			result->integer_value = 0;
			result->element_value = child;
		} else {
			ret = throw( exception, this, 0, "Not an element." );
		}
		dispose_variable( &parent );
	}
	return ret;
}

static int evaluate_sparse_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable string = { 0, NULL };
	struct element *elem;
	char message[ 128 ] = "";
	int ret = parameter->evaluate( parameter, variables, &string, exception );
	if( ret ) {
		if( string.element_value ) {
			elem = parse_element( string.element_value->string, message );
			if( message[ 0 ] == 0 ) {
				dispose_variable( result );
				result->integer_value = 0;
				result->element_value = elem;
			} else {
				ret = throw( exception, this, 0, message );
			}
		} else {
			ret = throw( exception, this, 0, "Not a string." );
		}
		dispose_variable( &string );
	}
	return ret;
}

static int evaluate_squote_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, length;
	struct expression *parameter = this->parameters;
	struct variable string = { 0, NULL };
	struct element *elem;
	ret = parameter->evaluate( parameter, variables, &string, exception );
	if( ret ) {
		if( string.element_value ) {
			elem = calloc( 1, sizeof( struct element ) );
			if( elem ) {
				length = write_byte_string( string.element_value->string,
					string.element_value->length, NULL );
				elem->string = malloc( sizeof( char ) * ( length + 1 ) );
				if( elem->string ) {
					elem->reference_count = 1;
					elem->length = write_byte_string( string.element_value->string,
						string.element_value->length, elem->string );
					elem->string[ length ] = 0;
					dispose_variable( result );
					result->integer_value = 0;
					result->element_value = elem;
				} else {
					free( elem );
					ret = throw( exception, this, 0, NULL );
				}
			} else {
				ret = throw( exception, this, 0, NULL );
			}
		} else {
			ret = throw( exception, this, 0, "Not a string." );
		}
		dispose_variable( &string );
	}
	return ret;
}

static int evaluate_sunquote_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable string = { 0, NULL };
	struct element *elem;
	int ret = parameter->evaluate( parameter, variables, &string, exception );
	if( ret ) {
		if( string.element_value ) {
			elem = new_string_constant( string.element_value->string );
			if( elem ) {
				dispose_variable( result );
				result->integer_value = 0;
				result->element_value = elem;
			} else {
				ret = throw( exception, this, 0, NULL );
			}
		} else {
			ret = throw( exception, this, 0, "Not a string." );
		}
		dispose_variable( &string );
	}
	return ret;
}

static struct operator operators[] = {
	{ "%", '%', 2, &evaluate_integer_expression, &operators[ 1 ] },
	{ "&", '&', 2, &evaluate_integer_expression, &operators[ 2 ] },
	{ "*", '*', 2, &evaluate_integer_expression, &operators[ 3 ] },
	{ "+", '+', 2, &evaluate_integer_expression, &operators[ 4 ] },
	{ "-", '-', 2, &evaluate_integer_expression, &operators[ 5 ] },
	{ "/", '/', 2, &evaluate_integer_expression, &operators[ 6 ] },
	{ "<", '<', 2, &evaluate_integer_expression, &operators[ 7 ] },
	{ "<e",'L', 2, &evaluate_integer_expression, &operators[ 8 ] },
	{ ">", '>', 2, &evaluate_integer_expression, &operators[ 9 ] },
	{ ">e",'G', 2, &evaluate_integer_expression, &operators[ 10 ] },
	{ ">>",'A', 2, &evaluate_integer_expression, &operators[ 11 ] },
	{ "^", '^', 2, &evaluate_integer_expression, &operators[ 12 ] },
	{ "=", '=', 2, &evaluate_integer_expression, &operators[ 13 ] },
	{ "|", '|', 2, &evaluate_integer_expression, &operators[ 14 ] },
	{ "~", '~', 1, &evaluate_int_not_expression, &operators[ 15 ] },
	{ "!", '!', 1, &evaluate_logical_expression, &operators[ 16 ] },
	{ "&&",'&', 2, &evaluate_logical_expression, &operators[ 17 ] },
	{ "||",'|', 2, &evaluate_logical_expression, &operators[ 18 ] },
	{ "$int", '$', 1, &evaluate_sint_expression, &operators[ 19 ] },
	{ "$str", '$',-1, &evaluate_sstr_expression, &operators[ 20 ] },
	{ "$len", '$', 1, &evaluate_slen_expression, &operators[ 21 ] },
	{ "$asc", '$', 1, &evaluate_sasc_expression, &operators[ 22 ] },
	{ "$cmp", '$', 2, &evaluate_scmp_expression, &operators[ 23 ] },
	{ "$cat", '$',-1, &evaluate_sstr_expression, &operators[ 24 ] },
	{ "$chr", '$', 2, &evaluate_schr_expression, &operators[ 25 ] },
	{ "$tup", '$', 2, &evaluate_stup_expression, &operators[ 26 ] },
	{ "$sub", '$', 3, &evaluate_ssub_expression, &operators[ 27 ] },
	{ "$arr", '$', 1, &evaluate_sarr_expression, &operators[ 28 ] },
	{ "$load",'$', 1, &evaluate_sload_expression, &operators[ 29 ] },
	{ "$flen",'$', 1, &evaluate_sflen_expression, &operators[ 30 ] },
	{ "$argc",'$', 0, &evaluate_sargc_expression, &operators[ 31 ] },
	{ "$argv",'$', 1, &evaluate_sargv_expression, &operators[ 32 ] },
	{ "$time",'$', 0, &evaluate_stime_expression, &operators[ 33 ] },
	{ "$date",'$', 1, &evaluate_sdate_expression, &operators[ 34 ] },
	{ "$secs",'$', 2, &evaluate_ssecs_expression, &operators[ 35 ] },
	{ "$next",'$', 1, &evaluate_snext_expression, &operators[ 36 ] },
	{ "$child",'$', 1, &evaluate_schild_expression, &operators[ 37 ] },
	{ "$parse",'$', 1, &evaluate_sparse_expression, &operators[ 38 ] },
	{ "$quote",'$', 1, &evaluate_squote_expression, &operators[ 39 ] },
	{ "$unquote",'$', 1, &evaluate_sunquote_expression, NULL },
	{ NULL }
};

static struct operator* get_operator( char *name, struct environment *env ) {
	struct operator *oper = env->operators;
	while( oper && strcmp( oper->name, name ) ) {
		oper = oper->next;
	}
	return oper;
}

static struct element* parse_infix_expression( struct element *elem, struct environment *env,
	struct function_declaration *func, struct expression *expr, char *message ) {
	struct element *next = elem->next;
	struct element *child = next->child;
	struct expression prev;
	struct operator *oper;
	int num_operands;
	if( child ) {	
		prev.next = NULL;
		child = parse_expression( child, env, func, &prev, message );
		expr->parameters = prev.next;
		if( message[ 0 ] == 0 ) {
			if( child ) {
				oper = get_operator( child->string, env );
				if( oper ) {
					expr->index = oper->oper;
					expr->evaluate = oper->evaluate;
					if( oper->num_operands > 0 ) {
						num_operands = parse_expressions( child->next, env, func, expr->parameters, message ) + 1;
						if( message[ 0 ] == 0 ) {
							if( num_operands == oper->num_operands ) {
								next = next->next;
							} else {
								sprintf( message, "Wrong number of arguments to '%.16s()' on line %d.", oper->name, child->line );
							}
						}
					} else {
						sprintf( message, "Wrong number of arguments to '%.16s()' on line %d.", oper->name, child->line );
					}
				} else {
					sprintf( message, "Unhandled operator '%.16s' on line %d.", child->string, child->line );
				}
			} else {
				sprintf( message, "Expected operator after '( on line %d.", elem->line );
			}
		} 
	} else {
		sprintf( message, "Expected expression after '( on line %d.", elem->line );
	}
	return next;
}

static struct element* parse_operator_expression( struct element *elem, struct environment *env,
	struct function_declaration *func, struct expression *expr, char *message ) {
	struct element *next = elem->next;
	struct expression prev;
	int num_operands;
	struct operator *oper = get_operator( elem->string, env );
	if( oper ) {
		expr->index = oper->oper;
		expr->evaluate = oper->evaluate;
		if( oper->num_operands != 0 ) {
			if( next && next->string[ 0 ] == '(' ) {
				prev.next = NULL;
				num_operands = parse_expressions( next->child, env, func, &prev, message );
				expr->parameters = prev.next;
				if( message[ 0 ] == 0 ) {
					if( num_operands == oper->num_operands || ( oper->num_operands < 0 && num_operands > 0 ) ) {
						if( expr->evaluate == &evaluate_integer_expression ) {
							if( ( prev.next->evaluate == &evaluate_local || prev.next->evaluate == &evaluate_global )
							&& ( prev.next->next->evaluate == &evaluate_local || prev.next->next->evaluate == &evaluate_global ) ) {
								/* Optimization for local/global/constant operands. */
								expr->evaluate = &evaluate_fastint_expression;
							}
						}
						next = next->next;
					} else {
						sprintf( message, "Wrong number of arguments to '%.16s()' on line %d.", oper->name, next->line );
					}
				}
			} else {
				sprintf( message, "Expected '(' after '%.16s' on line %d.", oper->name, elem->line );
			}
		}
	} else {
		sprintf( message, "Unhandled expression '%.16s' on line %d.", elem->string, elem->line );
	}
	return next;
}

static struct element* parse_function_expression( struct element *elem, struct environment *env,
	struct function_declaration *func, struct function_declaration *decl, struct expression *expr, char *message ) {
	struct element *next = elem->next;
	struct expression prev;
	int num_params;
	if( next && strcmp( next->string, "(" ) == 0 ) {
		expr->function = decl;
		expr->evaluate = &evaluate_function_expression;
		prev.next = NULL;
		num_params = parse_expressions( next->child, env, func, &prev, message );
		expr->parameters = prev.next;
		if( message[ 0 ] == 0 ) {
			if( num_params == expr->function->num_parameters ) {
				next = next->next;
			} else {
				sprintf( message, "Wrong number of arguments to '%.16s()' on line %d.", elem->string, next->line );
			}
		}
	} else {
		sprintf( message, "Expected '(' after function name on line %d.", next->line );
	}
	return next;
}

static struct element* parse_index_expression( struct element *elem, struct environment *env,
	struct function_declaration *func, struct expression *expr, char *message ) {
	struct expression prev;
	prev.next = NULL;
	parse_expressions( elem->child, env, func, &prev, message );
	if( prev.next ) {
		expr->parameters = prev.next;
		expr->evaluate = &evaluate_index_expression;
	}
	return elem->next;
}

static struct element* parse_expression( struct element *elem, struct environment *env,
	struct function_declaration *func, struct expression *prev, char *message ) {
	struct element *next = elem->next;
	struct global_variable *constant, *global;
	char *value = elem->string;
	int local;
	struct function_declaration *decl;
	struct expression *expr = calloc( 1, sizeof( struct expression ) );
	if( expr ) {
		expr->line = elem->line;
		expr->function = func;
		if( value[ 0 ] == '"' || ( value[ 0 ] >= '0' && value[ 0 ] <= '9' )
			|| ( value[ 0 ] == '-' && ( value[ 1 ] >= '0' && value[ 1 ] <= '9' ) )
			|| ( value[ 0 ] == '$' && value[ 1 ] == 0 ) ) {
			/* Constant. */
			constant = new_global_variable( "#Const#", message );
			if( constant ) {
				constant->next = env->constants;
				env->constants = constant;
				expr->global = constant;
				expr->evaluate = &evaluate_global;
				next = parse_constant( elem, &constant->value, message );
			}
		} else if( value[ 0 ] == '\'' ) {
			/* Infix operator.*/
			next = parse_infix_expression( elem, env, func, expr, message );
		} else if( value[ 0 ] == '[' ) {
			/* Array index operator. */
			next = parse_index_expression( elem, env, func, expr, message );
		} else {
			local = get_string_list_index( func->variable_decls, value );
			if( local >= 0 ) {
				/* Local variable reference.*/
				expr->index = local;
				expr->evaluate = &evaluate_local;
			} else {
				global = get_global_variable( env->constants, value );
				if( global == NULL ) {
					global = get_global_variable( env->globals, value );
				}
				if( global == NULL ) {
					global = get_global_variable( env->arrays, value );
				}
				if( global ) {
					/* Global variable reference.*/
					expr->global = global;
					expr->evaluate = &evaluate_global;
				} else {
					decl = get_function_declaration( env->functions, elem->string );
					if( decl ) {
						/* Function.*/
						next = parse_function_expression( elem, env, func, decl, expr, message );
					} else {
						/* Prefix Operator. */
						next = parse_operator_expression( elem, env, func, expr, message );
					}
				}
			}
		}
		if( message[ 0 ] == 0 && next && next->string[ 0 ] == '(' ) {
			sprintf( message, "Unexpected '(' after expression on line %d.", next->line );
		}
		if( message[ 0 ] == 0 ) {
			prev->next = expr;
		} else {
			dispose_expressions( expr );
		}
	} else {
		strcpy( message, "Out of memory." );
	}
	return next;
}

static int parse_expressions( struct element *elem, struct environment *env,
	struct function_declaration *func, struct expression *prev, char *message ) {
	int line, count = 0;
	while( elem && message[ 0 ] == 0 ) {
		elem = parse_expression( elem, env, func, prev, message );
		prev = prev->next;
		count++;
		while( elem && message[ 0 ] == 0 && elem->string[ 0 ] == ',' ) {
			line = elem->line;
			elem = elem->next;
			if( elem && elem->string[ 0 ] != ',' ) {
				elem = parse_expression( elem, env, func, prev, message );
				prev = prev->next;
				count++;
			} else {
				sprintf( message, "Expected expression after ',' on line %d.", line );
			}
		}
	}
	return count;
}

static struct element* parse_increment_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	int local;
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		local = get_string_list_index( func->variable_decls, next->string );
		if( local >= 0 ) {
			stmt->source = calloc( 1, sizeof( struct expression ) );
			if( stmt->source ) {
				stmt->local = local;
				stmt->source->line = next->line;
				stmt->source->function = func;
				stmt->execute = &execute_increment_statement;
				next = next->next->next;
			} else {
				strcpy( message, "Out of memory." );
			}
		} else {
			sprintf( message, "Undeclared local variable '%.16s' on line %d.", next->string, next->line );
		}
	}
	return next;
}

static struct element* parse_save_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		expr.next = NULL;
		next = parse_expression( next, env, func, &expr, message );
		if( expr.next ) {
			stmt->source = expr.next;
			if( next->string[ 0 ] == ',' ) {
				next = next->next;
			}
			expr.next = NULL;
			next = parse_expression( next, env, func, &expr, message );
			if( expr.next ) {
				stmt->destination = expr.next;
				stmt->execute = &execute_save_statement;
				next = next->next;
			}
		}
	}
	return next;
}

static struct element* parse_append_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct element *next = parse_save_statement( elem, env, func, prev, message );
	if( prev->next && message[ 0 ] == 0 ) {
		prev->next->local = 1;
	}
	return next;
}

static struct element* parse_assignment_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	int local;
	struct expression expr;
	struct global_variable *global = NULL;
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		local = get_string_list_index( func->variable_decls, next->string );
		if( local < 0 ) {
			global = get_global_variable( env->globals, next->string );
		}
		if( local >= 0 || global ) {
			expr.next = NULL;
			next = parse_expression( next->next->next, env, func, &expr, message );
			if( expr.next ) {
				stmt->source = expr.next;
				if( global ) {
					stmt->global = &global->value;
					stmt->execute = &execute_global_assignment;
				} else {
					stmt->local = local;
					stmt->execute = &execute_local_assignment;
				}
				next = next->next;
			}
		} else {
			sprintf( message, "Undeclared variable '%.16s' on line %d.", next->string, next->line );
		}
	}
	return next;
}

static struct element* parse_single_expr_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev,
	int ( *execute )( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ), char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		expr.next = NULL;
		next = parse_expression( next, env, func, &expr, message );
		if( expr.next ) {
			stmt->source = expr.next;
			stmt->execute = execute;
			next = next->next;
		}
	}
	return next;
}

static struct element* parse_print_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_single_expr_statement( elem, env, func, prev, &execute_print_statement, message );
}

static struct element* parse_write_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_single_expr_statement( elem, env, func, prev, &execute_write_statement, message );
}

static struct element* parse_error_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_single_expr_statement( elem, env, func, prev, &execute_error_statement, message );
}

static struct element* parse_return_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_single_expr_statement( elem, env, func, prev, &execute_return_statement, message );
}

static struct element* parse_throw_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_single_expr_statement( elem, env, func, prev, &execute_throw_statement, message );
}

static struct element* parse_exit_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_single_expr_statement( elem, env, func, prev, &execute_exit_statement, message );
}

static struct element* parse_break_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		stmt->execute = &execute_break_statement;
		prev->next = stmt;
		next = next->next;
	}
	return next;
}

static struct element* parse_continue_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		stmt->execute = &execute_continue_statement;
		prev->next = stmt;
		next = next->next;
	}
	return next;
}

static struct element* parse_call_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_single_expr_statement( elem, env, func, prev, &execute_call_statement, message );
}

static struct element* parse_dim_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next, *child = next->child;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		expr.next = NULL;
		child = parse_expression( child, env, func, &expr, message );
		if( expr.next ) {
			stmt->destination = expr.next;
			if( child->string[ 0 ] == ',' ) {
				child = child->next;
			}
			expr.next = NULL;
			child = parse_expression( child, env, func, &expr, message );
			if( expr.next ) {
				stmt->source = expr.next;
				next = next->next->next;
			}
			stmt->execute = &execute_dim_statement;
		}
	}
	return next;
}

static struct element* parse_set_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next, *child = next->child;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		expr.next = NULL;
		child = parse_expression( child, env, func, &expr, message );
		if( expr.next ) {
			stmt->destination = expr.next;
			if( child->string[ 0 ] == ',' ) {
				child = child->next;
			}
			expr.next = NULL;
			child = parse_expression( child, env, func, &expr, message );
			if( expr.next ) {
				stmt->index = expr.next;
				expr.next = NULL;
				next = parse_expression( next->next->next, env, func, &expr, message );
				if( expr.next ) {
					stmt->source = expr.next;
					next = next->next;
				}
			}
			stmt->execute = &execute_set_statement;
		}
	}
	return next;
}

static struct element* parse_aset_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		expr.next = NULL;
		next = parse_expression( next, env, func, &expr, message );
		if( expr.next ) {
			stmt->destination = expr.next;
			next = next->next;
			expr.next = NULL;
			next = parse_expression( next, env, func, &expr, message );
			if( expr.next ) {
				stmt->source = expr.next;
				stmt->execute = &execute_aset_statement;
				next = next->next;
			}
		}
	}
	return next;
}

static struct keyword switch_stmts[] = {
	{ "rem", "{", &parse_comment, &switch_stmts[ 1 ] },
	{ "case", "v{", &parse_case_statement, &switch_stmts[ 2 ] },
	{ "default", "{", &parse_default_statement, NULL },
	{ NULL }
};

static struct element* parse_switch_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message ), block, *cas, *def;
	if( stmt ) {
		prev->next = stmt;
		expr.next = NULL;
		next = parse_expression( next, env, func, &expr, message );
		if( expr.next ) {
			stmt->source = expr.next;
			block.next = NULL;
			parse_keywords( switch_stmts, next->child, env, func, &block, message );
			stmt->if_block = block.next;
			if( message[ 0 ] == 0 ) {
				stmt->if_block = cas = def = NULL;
				while( block.next ) {
					if( block.next->global ) {
						if( cas ) {
							cas->next = block.next;
							cas = cas->next;
						} else {
							stmt->if_block = cas = block.next;
						}
						block.next = block.next->next;
						cas->next = NULL;
					} else {
						if( def ) {
							def->next = block.next;
							def = def->next;
						} else {
							stmt->else_block = def = block.next;
						}
						block.next = block.next->next;
						def->next = NULL;
					}
				}
				next = next->next;
				stmt->execute = &execute_switch_statement;
			}
		}
	}
	return next;
}

static struct keyword statements[] = {
	{ "rem", "{", &parse_comment, &statements[ 1 ] },
	{ "var", "l;", &parse_local_declaration, &statements[ 2 ] },
	{ "let", "n=x;", &parse_assignment_statement, &statements[ 3 ] },
	{ "print", "x;", &parse_print_statement, &statements[ 4 ] },
	{ "write", "x;", &parse_write_statement, &statements[ 5 ] },
	{ "error", "x;", &parse_error_statement, &statements[ 6 ] },
	{ "throw", "x;", &parse_throw_statement, &statements[ 7 ] },
	{ "return", "x;", &parse_return_statement, &statements[ 8 ] },
	{ "exit", "x;", &parse_exit_statement, &statements[ 9 ] },
	{ "break", ";", &parse_break_statement, &statements[ 10 ] },
	{ "continue", ";", &parse_continue_statement, &statements[ 11 ] },
	{ "if", "x{", &parse_if_statement, &statements[ 12 ] },
	{ "while", "x{", &parse_while_statement, &statements[ 13 ] },
	{ "call", "x;", &parse_call_statement, &statements[ 14 ] },
	{ "try", "{cn{", &parse_try_statement, &statements[ 15 ] },
	{ "dim", "[;", &parse_dim_statement, &statements[ 16 ] },
	{ "set", "[=x;", &parse_set_statement, &statements[ 17 ] },
	{ "aset", "x=x;", &parse_aset_statement, &statements[ 18 ] },
	{ "switch", "x{", &parse_switch_statement, &statements[ 19 ] },
	{ "inc", "n;", &parse_increment_statement, &statements[ 20 ] },
	{ "save", "xx;", &parse_save_statement, &statements[ 21 ] },
	{ "append", "xx;", &parse_append_statement, NULL }
};

static struct element* parse_case_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement block, *stmt = new_statement( message );
	struct global_variable *constant;
	if( stmt ) {
		prev->next = stmt;
		constant = new_global_variable( "#Const#", message );
		if( constant ) {
			constant->next = env->constants;
			env->constants = constant;
			next = parse_constant( next, &constant->value, message );
			if( message[ 0 ] == 0 ) {
				stmt->global = &constant->value;
				block.next = NULL;
				parse_keywords( env->statements, next->child, env, func, &block, message );
				stmt->if_block = block.next;
				if( message[ 0 ] == 0 ) {
					stmt->execute = &execute_case_statement;
					next = next->next;
				}
			}
		}
	}
	return next;
}

static struct element* parse_default_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement block, *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		block.next = NULL;
		parse_keywords( env->statements, next->child, env, func, &block, message );
		stmt->if_block = block.next;
		if( message[ 0 ] == 0 ) {
			stmt->execute = &execute_case_statement;
			next = next->next;
		}
	}
	return elem->next->next;
}

static int is_keyword( struct keyword *keywords, char *value ) {
	while( keywords && strcmp( keywords->name, value ) ) {
		keywords = keywords->next;
	}
	return keywords != NULL;
}

static int validate_syntax( char *syntax, struct element *elem,
	struct element *key, struct environment *env, char *message ) {
	int idx = 1, chr = syntax[ 0 ], line = key->line;
	while( chr && message[ 0 ] == 0 ) {
		if( elem ) {
			line = elem->line;
		}
		if( chr == '0' ) {
			/* List end. */
			if( elem ) {
				sprintf( message, "Unexpected '%.16s' after '%.16s' on line %d.", elem->string, key->string, line );
			}
		} else if( strchr( "\",;={", chr ) ) {
			/* Strings, separators or blocks. */
			if( elem == NULL || elem->string[ 0 ] != chr ) {
				sprintf( message, "Expected '%c' after '%.16s' on line %d.", chr, key->string, line );
			}
		} else if( chr == '(' ) {
			/* Bracketed name list. */
			if( elem && elem->string[ 0 ] == '(' ) {
				if( elem->child ) {
					validate_syntax( "l0", elem->child, elem, env, message );
				}
			} else {
				sprintf( message, "Expected '(' after '%.16s' on line %d.", key->string, line );
			}
		} else if( chr == '[' ) {
			/* Index expression. */
			if( elem && elem->string[ 0 ] == '[' ) {
				if( elem->child ) {
					validate_syntax( "xx0", elem->child, key, env, message );
				} else {
					sprintf( message, "Expected '[' after '%.16s' on line %d.", key->string, line );
				}
			} else {
				sprintf( message, "Expected '[' after '%.16s' on line %d.", key->string, line );
			}
		} else if( chr == 'c' ) {
			/* Catch */
			if( elem == NULL || strcmp( elem->string, "catch" ) ) {
				sprintf( message, "Expected 'catch' after '%.16s' on line %d.", key->string, line );
			}
		} else if( chr == 'n' ) {
			/* Name. */
			if( elem == NULL || elem->string[ 0 ] == ';' ) {
				sprintf( message, "Expected name after '%.16s' on line %d.", key->string, line );
			} else if( !validate_name( elem->string, env ) ) {
				sprintf( message, "Invalid name '%.16s' on line %d.", elem->string, line );
			}
		} else if( chr == 'l' ) {
			/* Name list. */
			if( validate_syntax( "n", elem, key, env, message ) ) {
				while( elem->next && elem->next->string[ 0 ] != ';' && message[ 0 ] == 0 ) {
					elem = elem->next;
					line = elem->line;
					if( elem->string[ 0 ] == ',' ) {
						elem = elem->next;
					}
					validate_syntax( "n", elem, key, env, message );
				}
			}
		} else if( chr == 'v' ) {
			/* String, integer or tuple constant. */
			if( elem == NULL || strchr( "\"$-0123456789", elem->string[ 0 ] ) == NULL ) {
				sprintf( message, "Expected constant after '%.16s' on line %d.", key->string, line );
			} else if( elem->string[ 0 ] == '$' && elem->string[ 1 ] == 0 ) {
				elem = elem->next;
				if( elem == NULL || elem->string[ 0 ] != '{' ) {
					sprintf( message, "Expected '{' after '$' on line %d.", line );
				}
			} else if( strcmp( elem->string, "$tup" ) == 0 ) {
				elem = elem->next;
				if( elem == NULL || elem->string[ 0 ] != '(' ) {
					sprintf( message, "Expected '(' after '$tup' on line %d.", line );
				}
			}
		} else if( chr == 'x' ) {
			/* Expression. */
			if( elem && elem->string[ 0 ] == ',' ) {
				elem = elem->next;
			}
			if( elem && strchr( ",;({", elem->string[ 0 ] ) == NULL ) {
				if( elem->string[ 0 ] == '$' && elem->string[ 1 ] == 0 ) {
					elem = elem->next;
					if( elem == NULL || elem->string[ 0 ] != '{' ) {
						sprintf( message, "Expected '{' after '$' on line %d.", line );
					}
				} else if( elem->next && elem->next->string[ 0 ] == '(' ) {
					elem = elem->next;
				}
			} else if( elem && elem->string[ 0 ] == '[' ) {
				validate_syntax( "[", elem, key, env, message );
			} else {
				sprintf( message, "Expected expression after '%.16s' on line %d.", key->string, line );
			}
		} else {
			/* Internal error. */
			sprintf( message, "Internal error. Unknown specifier '%c' in syntax for '%s'.", chr, key->string );
		}
		chr = syntax[ idx++ ];
		if( elem ) {
			elem = elem->next;
		}
	}
	return message[ 0 ] == 0;
}

static void parse_keywords( struct keyword *keywords, struct element *elem,
	struct environment *env, struct function_declaration *func,
	struct statement *stmt, char *message ) {
	struct keyword *key;
	while( elem && message[ 0 ] == 0 ) {
		key = keywords;
		while( key && strcmp( key->name, elem->string ) ) {
			key = key->next;
		}
		if( key ) {
			if( validate_syntax( key->syntax, elem->next, elem, env, message ) ) {
				elem = key->parse( elem, env, func, stmt, message );
				if( stmt && stmt->next ) {
					stmt = stmt->next;
				}
			}
		} else {
			sprintf( message, "Unrecognized keyword '%.16s' on line %d.", elem->string, elem->line );
		}
	}
}

static struct element* parse_if_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement block, *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		expr.next = NULL;
		next = parse_expression( next, env, func, &expr, message );
		if( expr.next ) {
			stmt->source = expr.next;
			stmt->execute = &execute_if_statement;
			if( next->child ) {
				block.next = NULL;
				parse_keywords( env->statements, next->child, env, func, &block, message );
				stmt->if_block = block.next;
			}
			if( message[ 0 ] == 0 ) {
				next = next->next;
				if( next && strcmp( next->string, "else" ) == 0 ) {
					if( next->next && next->next->string[ 0 ] == '{' ) {
						next = next->next;
						if( next->child ) {
							block.next = NULL;
							parse_keywords( env->statements, next->child, env, func, &block, message );
							stmt->else_block = block.next;
						}
						if( message[ 0 ] == 0 ) {
							next = next->next;
						}
					} else {
						sprintf( message, "Expected '{' after 'else' on line %d.", next->line );
					}
				}
			}
		}
	}
	return next;
}

static struct element* parse_while_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement block, *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		expr.next = NULL;
		next = parse_expression( next, env, func, &expr, message );
		if( expr.next ) {
			stmt->source = expr.next;
			if( next->child ) {
				block.next = NULL;
				parse_keywords( env->statements, next->child, env, func, &block, message );
				stmt->if_block = block.next;
			}
			if( message[ 0 ] == 0 ) {
				next = next->next;
			}
			stmt->execute = &execute_while_statement;
		}
	}
	return next;
}

static struct element* parse_try_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct statement block, *stmt;
	struct element *next = elem->next;
	stmt = new_statement( message );
	if( stmt ) {
		if( next->child ) {
			block.next = NULL;
			parse_keywords( env->statements, next->child, env, func, &block, message );
			stmt->if_block = block.next;
		}
		if( message[ 0 ] == 0 ) {
			next = next->next->next;
			stmt->local = get_string_list_index( func->variable_decls, next->string );
			if( stmt->local >= 0 ) {
				next = next->next;
				if( next->child ) {
					block.next = NULL;
					parse_keywords( env->statements, next->child, env, func, &block, message );
					stmt->else_block = block.next;
				}
				if( message[ 0 ] == 0 ) {
					next = next->next;
				}
				stmt->execute = &execute_try_statement;
			} else {
				sprintf( message, "Undeclared local variable '%.16s' on line %d.", next->string, next->line );
			}
		}
		prev->next = stmt;
	}
	return next;
}

static struct element* parse_function_declaration( struct element *elem, struct environment *env,
	struct function_declaration *decl, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	if( validate_decl( next, env, message ) ) {
		decl = new_function_declaration( next->string, env->file );
		if( decl ) {
			decl->next = env->functions;
			env->functions = decl;
			decl->line = elem->line;
			decl->elem = elem;
			decl->env = env;
			next = next->next;
			if( next->string[ 0 ] == '(' ) {
				parse_decl_list( next->child, env, add_function_parameter, message );
				next = next->next;
			}
			next = next->next;
		} else {
			strcpy( message, "Out of memory." );
		}
	}
	return next;
}

static struct element* parse_program_declaration( struct element *elem, struct environment *env,
	struct function_declaration *decl, struct statement *prev, char *message ) {
	struct element *next = parse_function_declaration( elem, env, decl, prev, message );
	env->entry_point = env->functions;
	return next;
}

static struct element* parse_include( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	char *file_name = new_string( next->string );
	if( file_name ) {
		unquote_string( file_name, file_name );
		if( parse_tt_file( file_name, env, message ) ) {
			next = next->next->next;
		}
		free( file_name );
	} else {
		strcpy( message, "Out of memory." );
	}
	return next;
}

static struct keyword declarations[] = {
	{ "rem", "{", &parse_comment, &declarations[ 1 ] },
	{ "include", "\";", &parse_include, &declarations[ 2 ] },
	{ "function", "n({", &parse_function_declaration, &declarations[ 3 ] },
	{ "program", "n{",&parse_program_declaration, &declarations[ 4 ] },
	{ "global", "l;", &parse_global_declaration, &declarations[ 5 ] },
	{ "array", "l;", &parse_array_declaration, &declarations[ 6 ] },
	{ "const", "n=v;", &parse_const_declaration, NULL }
};

static int validate_name( char *name, struct environment *env ) {
	int chr, idx = 0, result = 1;
	chr = name[ idx++ ];
	if( ( chr >= 'A' && chr <= 'Z') || ( chr >= 'a' && chr <= 'z' ) ) {
		/* First character must be alphabetical.*/
		while( chr ) {
			if( chr == '_' || ( chr >= '0' && chr <= '9' )
			|| ( chr >= 'A' && chr <= 'Z' ) || ( chr >= 'a' && chr <= 'z' ) ) {
				/* Subsequent characters must be alphanumerical, or underscore. */
				chr = name[ idx++ ];
			} else {
				result = chr = 0;
			}
		}
	} else {
		result = 0;
	}
	if( result ) {
		/* Declaration keywords not permitted. */
		result = !is_keyword( declarations, name );
	}
	if( result ) {
		/* Statement keywords not permitted. */
		result = !is_keyword( env->statements, name );
	}
	if( result ) {
		/* Operator name not permitted. */
		result = ( get_operator( name, env ) == NULL );
	}
	return result;
}

static int validate_decl( struct element *elem, struct environment *env, char *message ) {
	int result = 1;
	char *name = elem->string;
	struct global_variable *global;
	struct function_declaration *func;
	global = env->globals;
	while( global ) {
		if( strcmp( name, global->name ) == 0 ) {
			/* Existing global variable name not permitted. */
			result = 0;
		}
		global = global->next;
	}
	if( result ) {
		global = env->arrays;
		while( global ) {
			if( strcmp( name, global->name ) == 0 ) {
				/* Existing array name not permitted. */
				result = 0;
			}
			global = global->next;
		}
	}
	if( result ) {
		func = env->functions;
		while( func ) {
			if( strcmp( name, func->name ) == 0 ) {
				/* Existing function name not permitted. */
				result = 0;
			}
			func = func->next;
		}
	}
	if( !result ) {
		sprintf( message, "Name '%.16s' already defined on line %d.", elem->string, elem->line );
	}
	return result;
}

static struct element* parse_decl_list( struct element *elem, struct environment *env,
	int (*add)( struct environment *env, struct element *elem, char *message ), char *message ) {
	while( elem && elem->string[ 0 ] != ';' && message[ 0 ] == 0 ) {
		if( validate_decl( elem, env, message ) ) {
			if( add( env, elem, message ) ) {
				elem = elem->next;
				if( elem && elem->string[ 0 ] == ',' ) {
					elem = elem->next;
				}
			}
		}
	}
	return elem;
}

static int parse_tt_program( char *program, struct environment *env, char *message ) {
	struct statement stmt;
	struct element *elem, *next;
	struct function_declaration *func, *entry;
	elem = parse_element( program, message );
	if( elem ) {
		/* Populate execution environment.*/
		parse_keywords( declarations, elem, env, NULL, NULL, message );
		/* Parse function bodies. */
		func = env->functions;
		while( func && func->elem && message[ 0 ] == 0 ) {
			next = func->elem->next->next;
			if( next->string[ 0 ] == '(' ) {
				next = next->next;
			}
			if( next->child ) {
				entry = env->entry_point;
				env->entry_point = func;
				stmt.next = NULL;
				parse_keywords( env->statements, next->child, env, func, &stmt, message );
				func->statements = stmt.next;
				env->entry_point = entry;
			}
			next = next->next;
			func->elem = NULL;
			func = func->next;
		}
		dispose_element( elem );
	}
	return message[ 0 ] == 0;
}

static int parse_tt_file( char *file_name, struct environment *env, char *message ) {
	int file_length, success = 0;
	char *program_buffer, error[ 128 ] = "", *prev_file;
	/* Load program file into string.*/
	file_length = load_file( file_name, NULL, message );
	if( file_length > 0 ) {
		/*printf( "Parsing '%s'. Length %d\n", file_name, file_length );*/
		program_buffer = malloc( file_length + 1 );
		file_length = load_file( file_name, program_buffer, message );
		if( file_length > 0 ) {
			program_buffer[ file_length ] = 0;
			/* Parse program structure.*/
			prev_file = env->file;
			env->file = file_name;
			success = parse_tt_program( program_buffer, env, message );
			env->file = prev_file;
			free( program_buffer );
		}
	}
	if( !success && strncmp( message, "Unable to parse", 15 ) ) {
		strncpy( error, message, 127 );
		error[ 127 ] = 0;
		sprintf( message, "Unable to parse '%.16s'.\n%s", file_name, error );
	}
	return success;
}

int main( int argc, char **argv ) {
	int success, exit_code = EXIT_FAILURE;
	char *file_name, message[ 256 ] = "";
	struct environment *env;
	struct variable result, except;
	struct expression expr;
	/* Handle command-line.*/
	if( argc < 2 ) {
		fprintf( stderr, "Usage: tt program.tt [args]\n" );
		return EXIT_FAILURE;
	}
	file_name = argv[ 1 ];
	/* Parse program file. */
	env = calloc( 1, sizeof( struct environment ) );
	if( env ) {
		env->argc = argc - 1;
		env->argv = &argv[ 1 ];
		env->statements = statements;
		env->operators = operators;
		success = parse_tt_file( file_name, env, message );
		if( success ) {
			if( env->entry_point ) {
				/* Evaluate entry-point function. */
				result.integer_value = except.integer_value = 0;
				result.element_value = except.element_value = NULL;
				expr.line = env->entry_point->line;
				expr.function = env->entry_point;
				expr.parameters = NULL;
				expr.evaluate = &evaluate_function_expression;
				if( expr.evaluate( &expr, NULL, &result, &except ) ) {
					exit_code = EXIT_SUCCESS;
				} else if( except.element_value && except.element_value->string == NULL ) {
					exit_code = except.integer_value;
				} else {
					fprintf( stderr, "Unhandled exception %d.\n", except.integer_value );
					if( except.element_value && except.element_value->string ) {
						fprintf( stderr, "%s\n", except.element_value->string );
					}
				}
				dispose_variable( &result );
				dispose_variable( &except );
			} else {
				fprintf( stderr, "No programs found.\n" );
			}
		} else {
			fprintf( stderr, "%s\n", message );
		}
		dispose_environment( env );
	} else {
		fputs( "Out of memory.\n", stderr );
	}
	return exit_code;
}
