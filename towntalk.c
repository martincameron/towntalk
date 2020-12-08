
#if defined( __GNUC__ )
#define alloca( size ) __builtin_alloca( size )
#else
#include "alloca.h"
#endif
#include "errno.h"
#include "stdio.h"
#include "string.h"
#include "time.h"

#include "towntalk.h"

/*
	Towntalk (c)2020 Martin Cameron.

	A program file consists of a list of declarations.
	A variable is an integer with an associated referenced value.
	Referenced values may be null, strings, elements, arrays or functions.
	Non-null referenced values may evaluate to zero in integer expressions.
	When a '#' character is encountered, the rest of the line is ignored.
	Variable and function names must be alphanumeric.
	Strings are immutable and can be used as byte arrays.
	String literals support the escape sequences "\"", "\\", and octal "\nnn".
	Commas within name and argument lists are optional.
	Elements are immutable trees of strings with next and child references.
	A valid program file may be parsed into an element tree.
	Elements are separated by whitespace, commas, ';' or '='.
	Elements may contain spaces and separators by enclosing them in quotes.
	Child elements are enclosed in parentheses, square brackets or braces.
	If the program has been interrupted, while loops will throw an exception.

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
		rem {}                   Comment (all brackets inside must be balanced).
		include "file.tt";       Include declarations from specified file.
		const name = value;      Integer, string or tuple constant.
		global a, b, c = expr;   Global variables.
		array a, b, [ c expr ];  Global arrays.
		struct s { a,b,c };      Layout for formatting arrays.
		struct t(s) { d,e,f };   Struct with members included from s.
		function f(param){stmts} Function declaration.
		program name{statements} Entry point function (no arguments).

	Statements:
		rem {}                   Comment.
		var a, b, c = expr;      Local variables.
		let a = expr;            Assign expression to local or global variable.
		let [arr idx] = expr;    Assign expression to array at index.
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
		   case 1,2 {statements} Execute statements if expr equals 1 or 2.
		   case "a" {statements} Execute statements if expr equals "a".
		   default {statements}} Execute statements if no valid cases.
		try {statements}         Execute statements unless exception thrown.
		   catch a {statements}  Assign exception to local var and execute.
		call expr;               Evaluate expression and discard result.
		dim [arr len];           Resize specified array.
		set [arr idx] = expr;    Variable/Array assignment (same as let).
		inc a;                   Increment local variable.
		dec a;                   Decrement local variable.
		save str, "file";        Save bytes from string to file.
		append str, "file";      Append bytes to the end of file.

	Expressions:
		-123                     Decimal integer literal.
		0x100                    Hexadecimal integer literal.
		0888                     Octal integer literal.
		"String"                 String literal.
		${0,"1",$tup("2",3)}     Element literal.
		name                     Value of named local or global variable.
		function(expr, expr)     Call function with specified args.
		[arr idx]                Array element.
		struct                   Length of structure.
		struct.member            Index of named struct member.
		@function                Function reference.
		:(func expr ...)         Call function reference with specified args.
		'(expr operator ...)     Infix operator, eg '( 1 + 2 ).
		+(int int)               Addition.
		-(int int)               Subtraction.
		*(int int)               Multiplication.
		/(int int)               Division.
		%(int int)               Modulo division.
		<<(int int)              Arithmetic shift-left.
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
		$eq(expr expr)           Evaluates to 1 if arguments have the same value.
		$str(str int ...)        Integer to string and string concatenation.
		$cmp(str str)            String/Tuple comparison, returns 0 if equal.
		$cat(str str ...)        String concatenation (same as $str).
		$chr(str idx)            Character at idx as integer.
		$sub(str off len)        Substring (or byte array to string).
		$asc(int)                Character code to string.
		$hex(int)                Integer to fixed-length signed hex string.
		$int(str)                String to integer.
		$len(str/arr)            String/Array length.
		$tup(str int)            String/Integer tuple.
		$array(len)              Create array of specified length.
		$array(${0,"a"})         Create array with values from element.
		$new(struct)             Same as $array(struct).
		$load("abc.bin")         Load raw bytes into string.
		$flen("file")            Get the length of a file.
		$src                     Path of current source file.
		$chop(str,"sepchars")    Return a substring up to the last separator.
		$argc                    Number of command-line arguments.
		$argv(idx)               Command-line argument as string.
		$time                    Current time as seconds/date tuple.
		$parse(str)              Parse string into element list.
		$unparse(elem)           Concatenate element list into string.
		$next(elem)              Get the next element in the list or null.
		$child(elem)             Get the first child element or null.
		$line(elem)              Get the line number of the element.
		$elem(elem child next)   Return a copy of elem with specified references.
		$values(array)           Return an element containing array values.
		$pack(int/arr)           Encode integers as big-endian byte string.
		$unpack(str idx)         Decode the specified big-endian integer.
		$quote(str)              Encode byte string with quotes and escapes.
		$unquote(str)            Decode quoted-string into byte string.
		$interrupted             Check and clear program interrupt status.
		$function(${(){stmts}})  Compile a function reference from an element.
*/

/* The maximum integer value. */
const int MAX_INTEGER = ( 1 << ( sizeof( int ) * 8 - 1 ) ) - 1u;

/* Message to be used to avoid memory allocation in out-of-memory error paths. */
const char *OUT_OF_MEMORY = "Out of memory.";

static struct constant constants[] = {
	{ "FALSE", 0, NULL },
	{  "TRUE", 1, NULL },
	{ NULL }
};

/* Forward declarations. */
static int validate_name( char *name, struct environment *env );
static int validate_decl( struct element *elem, struct environment *env, char *message );
static void parse_keywords( struct keyword *keywords, struct element *elem,
	struct environment *env, struct function *func, struct statement *stmt, char *message );
static struct element* parse_expression( struct element *elem, struct environment *env,
	struct function *func, struct expression *prev, char *message );
static struct element* parse_expressions( struct element *elem, struct environment *env,
	struct function *func, char terminator, struct expression *prev, int *num_exprs, char *message );
static struct element* parse_if_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message );
static struct element* parse_while_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message );
static struct element* parse_try_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message );
static struct element* parse_case_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message );
static struct element* parse_default_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message );

/* Return the index of the last separator char encountered in str. */
int chop( char *str, const char *separators ) {
	int idx = 0, offset = 0;
	char chr = str[ idx++ ];
	while( chr ) {
		if( strchr( separators, chr ) ) {
			offset = idx;
		}
		chr = str[ idx++ ];
	}
	return offset;
}

static int parse_string( char *buffer, int idx, struct element *elem, int line, char *message ) {
	int offset = idx;
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
			if( elem ) {
				memcpy( &elem->str.string[ 0 ], &buffer[ offset ], idx - offset );
			}
		} else {
			sprintf( message, "Unclosed string on line %d.", line );
			idx = -3;
		}
	} else {
		sprintf( message, "Expected '\"' on line %d.", line );
		idx = -3;
	}
	return idx;
}

static struct element* new_element( int str_len ) {
	struct element *elem = malloc( sizeof( struct element ) + sizeof( char ) * ( str_len + 1 ) );
	if( elem ) {
		memset( elem, 0, sizeof( struct element ) );
		elem->str.string = ( char * ) &elem[ 1 ];
		elem->str.string[ str_len ] = 0;
		elem->str.reference_count = 1;
		elem->str.length = str_len;
		elem->str.type = ELEMENT;
	}
	return elem;
}

/* Allocate and return a new array of the specified number of elements. */
struct array* new_array( struct environment *env, int length ) {
	struct array *arr = calloc( 1, sizeof( struct array ) );
	if( arr ) {
		arr->str.string = "#Array#";
		arr->str.reference_count = 1;
		arr->str.length = strlen( arr->str.string );
		arr->str.type = ARRAY;
		arr->length = length;
		arr->array = calloc( length + 1, sizeof( struct variable ) );
		if( arr->array ) {
			arr->prev = &env->arrays;
			arr->next = env->arrays.next;
			if( arr->next ) {
				arr->next->prev = arr;
			} 
			env->arrays.next = arr;
		} else {
			free( arr );
			arr = NULL;
		}
	}
	return arr;
}

/* Allocate and return a string of the specified length and reference count of 1. */
struct string* new_string_value( int length ) {
	struct string *str = malloc( sizeof( struct string ) + sizeof( char ) * ( length + 1 ) );
	if( str ) {
		memset( str, 0, sizeof( struct string ) );
		str->string = ( char * ) &str[ 1 ];
		str->string[ length ] = 0;
		str->reference_count = 1;
		str->length = length;
	}
	return str;
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

static struct string* new_string_literal( char *source ) {
	struct string *str = new_string_value( unquote_string( source, NULL ) );
	if( str ) {
		str->length = unquote_string( source, str->string );
	}
	return str;
}

static int parse_child_element( char *buffer, int idx, struct element *parent, char *message ) {
	struct element *elem = NULL;
	int offset = idx, length = 0, line = parent->line, end, str_len;
	char *bracket, chr = '\n';
	while( chr ) {
		chr = buffer[ idx++ ];
		if( chr <= 32 || strchr( "\"#(),;=[]{}", chr ) ) {
			if( length > 0 ) {
				if( elem == NULL ) {
					elem = new_element( length );
					parent->child = elem;
				} else {
					elem->next = new_element( length );
					elem = elem->next;
				}
				if( elem ) {
					elem->line = line;
					memcpy( elem->str.string, &buffer[ offset ], length );
					/*printf("%d %d %c :%s\n",offset,length,chr,elem->str.string);*/
				} else {
					strcpy( message, OUT_OF_MEMORY );
					return -1;
				}
			}
			if( chr == '\n' || chr == '#' ) {
				while( chr && chr != '\n' ) {
					chr = buffer[ idx++ ];
				}
				line++;
			} else if( chr && strchr( "\"(,;=[{", chr ) ) {
				if( chr == '"' ) {
					end = parse_string( buffer, idx - 1, NULL, line, message );
					if( end < 0 ) {
						return end;
					} else {
						str_len = end - idx + 1;
					}
				} else {
					str_len = 2;
				}
				if( elem == NULL ) {
					elem = new_element( str_len );
					parent->child = elem;
				} else {
					elem->next = new_element( str_len );
					elem = elem->next;
				}
				if( elem ) {
					elem->line = line;
					if( chr == '"' ) {
						idx = parse_string( buffer, idx - 1, elem, line, message );
						if( idx < 0 ) {
							return idx;
						}
					} else {
						bracket = strchr( "()[]{}", chr );
						if( bracket ) {
							elem->str.string[ 0 ] = bracket[ 0 ];
							elem->str.string[ 1 ] = bracket[ 1 ];
							idx = parse_child_element( buffer, idx, elem, message );
							if( idx > 0 ) {
								/* Exchange line and elem->line. */
								line = elem->line - line;
								elem->line = elem->line - line;
								line = elem->line + line;
								if( buffer[ idx - 1 ] != bracket[ 1 ] ) {
									sprintf( message, "Unclosed element on line %d.", line );
									return -2;
								}
							} else {
								return idx;
							}
						} else {
							elem->str.string[ 0 ] = chr;
							elem->str.string[ 1 ] = 0;
							elem->str.length = 1;
						}
					}
				} else {
					strcpy( message, OUT_OF_MEMORY );
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

static struct element* parse_element( char *buffer, char *message ) {
	int idx;
	struct element elem;
	elem.line = 1;
	elem.str.string = NULL;
	elem.str.type = ELEMENT;
	elem.child = elem.next = NULL;
	idx = parse_child_element( buffer, 0, &elem, message );
	if( idx > 0 ) {
		if( buffer[ idx - 1 ] != 0 ) {
			sprintf( message, "Unexpected closing bracket '%c' on line %d.", buffer[ idx - 1 ], elem.line );
			idx = -4;
		}
	}
	if( idx < 0 ) {
		unref_string( &elem.child->str );
		elem.child = NULL;
	}
	return elem.child;
}

/* Load the specified file into buffer (if not null) and returns the file length.
   Returns -1 and writes message on failure. */
long load_file( char *file_name, char *buffer, char *message ) {
	long file_length = -1, bytes_read;
	FILE *input_file = fopen( file_name, "rb" );
	if( input_file != NULL ) {
		if( fseek( input_file, 0L, SEEK_END ) == 0 ) {
			file_length = ftell( input_file );
			if( file_length >= 0 && buffer ) {
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

static int resize_array( struct array *arr, int len ) {
	int idx, count;
	struct variable *old = arr->array;
	struct variable *new = calloc( len + 1, sizeof( struct variable ) );
	if( new ) {
		count = arr->length;
		if( count > len ) {
			memcpy( new, old, sizeof( struct variable ) * len );
			idx = len;
			while( idx < count ) {
				dispose_variable( &old[ idx++ ] );
			}
		} else {
			memcpy( new, old, sizeof( struct variable ) * count );
		}
		arr->array = new;
		arr->length = len;
		free( old );
		return 1;
	}
	return 0;
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

/* Decrement the reference-count of any types referenced by var, and deallocate if necessary.
   This must be called for all variables assigned during program execution. */
void dispose_variable( struct variable *var ) {
	if( var->string_value ) {
		unref_string( var->string_value );
		var->string_value = NULL;
	}
}

/* Assign src variable to dest, managing reference counts. */
void assign_variable( struct variable *src, struct variable *dest ) {
	dispose_variable( dest );
	dest->integer_value = src->integer_value;
	dest->string_value = src->string_value;
	if( dest->string_value ) {
		dest->string_value->reference_count++;
	}
}

static int compare_variables( struct variable *var1, struct variable *var2 ) {
	struct string *str1, *str2;
	int result = var1->integer_value - var2->integer_value;
	if( result == 0 && var1->string_value != var2->string_value ) {
		if( var1->string_value && var2->string_value ) {
			str1 = var1->string_value;
			str2 = var2->string_value;
			if( str1->length > str2->length ) {
				result = memcmp( str1->string, str2->string, str2->length );
			} else {
				result = memcmp( str1->string, str2->string, str1->length );
			}
			if( result == 0 ) {
				result = str1->length - str2->length;
			}
		} else if( var2->string_value ) {
			result = -var2->string_value->length;
		} else {
			result = var1->string_value->length;
		}
	}
	return result;
}

static void dispose_keywords( struct keyword *keywords ) {
	struct keyword *next;
	while( keywords ) {
		next = keywords->next;
		free( keywords );
		keywords = next;
	}
}

static void dispose_operators( struct operator *operators ) {
	struct operator *next;
	while( operators ) {
		next = operators->next;
		free( operators );
		operators = next;
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

static void dispose_global_variables( struct global_variable *global ) {
	struct global_variable *next;
	while( global ) {
		next = global->next;
		free( global->name );
		dispose_variable( &global->value );
		dispose_expressions( global->initializer );
		free( global );
		global = next;
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

/* Decrement the reference count of the specified value and deallocate if necessary. */
void unref_string( struct string *str ) {
	int idx, len;
	struct array *arr;
	struct element *elem;
	struct function *func;
	if( str->reference_count == 1 ) {
		if( str->type == STRING ) {
			free( str );
		} else if( str->type == ELEMENT ) {
			while( str ) {
				if( str->reference_count == 1 ) {
					elem = ( struct element * ) str;
					if( elem->child ) {
						unref_string( &elem->child->str );
					}
					elem = elem->next;
					free( str );
					str = &elem->str;
				} else {
					str->reference_count--;
					str = NULL;
				}
			}
		} else if( str->type == ARRAY ) {
			arr = ( struct array * ) str;
			idx = 0, len = arr->length;
			while( idx < len ) {
				dispose_variable( &arr->array[ idx++ ] );
			}
			arr->prev->next = arr->next;
			if( arr->next ) {
				arr->next->prev = arr->prev;
			}
			free( arr->array );
			free( arr );
		} else if( str->type == FUNCTION ) {
			while( str ) {
				if( str->reference_count == 1 ) {
					func = ( struct function * ) str;
					free( func->str.string );
					free( func->file.string );
					dispose_string_list( func->variable_decls );
					dispose_statements( func->statements );
					dispose_global_variables( func->literals );
					func = func->next;
					free( str );
					str = &func->str;
				} else {
					str->reference_count--;
					str = NULL;
				}
			}
		}
	} else {
		str->reference_count--;
	}
}

static void dispose_arrays( struct array *head ) {
	struct array *arr = head->next;
	while( arr ) {
		arr->str.reference_count++;
		resize_array( arr, 0 );
		arr = arr->next;
	}
	while( head->next ) {
		unref_string( &head->next->str );
	}
}

static void dispose_structure_declarations( struct structure *sct ) {
	struct structure *next;
	while( sct ) {
		next = sct->next;
		free( sct->name );
		dispose_string_list( sct->fields );
		free( sct );
		sct = next;
	}
}

/* Deallocate the specified environment and all types referenced by it. */
void dispose_environment( struct environment *env ) {
	if( env ) {
		dispose_keywords( env->statements );
		dispose_operators( env->operators );
		dispose_global_variables( env->constants );
		dispose_global_variables( env->globals );
		dispose_arrays( &env->arrays );
		if( env->functions ) {
			unref_string( &env->functions->str );
		}
		if( env->entry_points ) {
			unref_string( &env->entry_points->str );
		}
		dispose_structure_declarations( env->structures );
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

/* Assign the specified error code and message to the exception variable and return EXCEPTION. */
enum result throw( struct variable *exception, struct expression *source, int integer, const char *string ) {
	struct string *str = NULL;
	if( string ) {
		str = new_string_value( strlen( string ) + 64 );
		if( str ) {
			if( sprintf( str->string, "%s (on line %d of '%.32s')",
			string, source->line, source->function->file.string ) < 0 ) {
				strcpy( str->string, string );
			}
			str->length = strlen( str->string );
		}
	}
	dispose_variable( exception );
	exception->integer_value = integer;
	exception->string_value = str;
	return EXCEPTION;
}

/* Assign an uncatchable exception variable with the specified exit code. */
enum result throw_exit( struct variable *exception, int exit_code ) {
	static struct string EXIT_STRING = { 2 };
	dispose_variable( exception );
	exception->integer_value = exit_code;
	exception->string_value = &EXIT_STRING;
	return EXCEPTION;
}

static int write_byte_string( char *bytes, int count, char *output ) {
	int chr, size, idx = 0, length = 0;
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
		length = 2;
		while( idx < count && length > 0 ) {
			chr = bytes[ idx++ ];
			if( chr == '"' || chr == '\\' ) {
				size = 2;
			} else if( ( chr & 0x7F ) < 32 || chr == 127 ) {
				size = 4;
			} else {
				size = 1;
			}
			if( MAX_INTEGER - length > size ) {
				length += size;
			} else {
				length = -1;
			}
		}
	}
	return length;
}

static int write_element( struct element *elem, char *output ) {
	int size, length = 0;
	if( output ) {
		while( elem ) {
			if( elem->child ) {
				output[ length++ ] = elem->str.string[ 0 ];
				length += write_element( elem->child, &output[ length ] );
				output[ length++ ] = elem->str.string[ 1 ];
			} else {
				memcpy( &output[ length ], elem->str.string, elem->str.length );
				length += elem->str.length;
				output[ length++ ] = '\n';
			}
			elem = elem->next;
		}
	} else {
		while( elem && length >= 0 ) {
			if( elem->child ) {
				size = write_element( elem->child, NULL );
				if( size >= 0 && MAX_INTEGER - length - 2 > size ) {
					length += size + 2;
				} else {
					length = -1;
				}
			} else {
				size = elem->str.length;
				if( MAX_INTEGER - length - 1 > size ) {
					length += size + 1;
				} else {
					length = -1;
				}
			}
			elem = elem->next;
		}
	}
	return length;
}

static struct element* parse_constant( struct element *elem, struct variable *constant, char *message ) {
	int len = 0, integer_value = 0;
	struct string *string_value = NULL;
	struct element *child, *next = elem->next;
	char *end, *str = NULL;
	if( elem->str.string[ 0 ] == '"' ) {
		/* String literal. */
		string_value = new_string_literal( elem->str.string );
		if( string_value == NULL ) {
			strcpy( message, OUT_OF_MEMORY );
		}
	} else if( elem->str.string[ 0 ] == '$' && elem->str.string[ 1 ] == 0 ) {
		/* Element. */
		if( next && next->str.string[ 0 ] == '{' ) {
			if( next->child ) {
				next->child->str.reference_count++;
				string_value = &next->child->str;
			}
			next = next->next;
		} else {
			sprintf( message, "Expected '{' after '$' on line %d.", elem->line );
		}
	} else if( strcmp( elem->str.string, "$str" ) == 0 ) {
		/* Concatenated string constant. */
		if( next && next->str.string[ 0 ] == '(' ) {
			child = next->child;
			while( child && message[ 0 ] == 0 ) {
				if( child->str.string[ 0 ] == '"' ) {
					end = cat_string( str, len, child->str.string, child->str.length );
					if( end ) {
						len = len + child->str.length;
						free( str );
						str = end;
					} else {
						strcpy( message, OUT_OF_MEMORY );
					}
				} else {
					sprintf( message, "Invalid string literal on line %d.", child->line );
				}
				if( child->next && child->next->str.string[ 0 ] == ',' ) {
					child = child->next;
				}
				child = child->next;
			}
			if( str ) {
				string_value = new_string_literal( str );
				if( string_value ) {
					next = next->next;
				} else {
					strcpy( message, OUT_OF_MEMORY );
				}
				free( str );
			} else {
				sprintf( message, "Invalid string literal on line %d.", next->line );
			}
		} else {
			sprintf( message, "Expected '(' after '$str' on line %d.", elem->line );
		}
	} else if( strcmp( elem->str.string, "$tup" ) == 0 ) {
		/* Tuple constant. */
		if( next && next->str.string[ 0 ] == '(' ) {
			child = next->child;
			if( child && child->str.string[ 0 ] == '"' ) {
				string_value = new_string_literal( child->str.string );
				if( string_value ) {
					child = child->next;
					if( child && child->str.string[ 0 ] == ',' ) {
						child = child->next;
					}
					if( child && child->next == NULL ) {
						integer_value = ( int ) strtol( child->str.string, &end, 0 );
						if( end[ 0 ] == 0 ) {
							next = next->next;
						} else {
							sprintf( message, "Invalid tuple integer on line %d.", next->line );
						}
					} else {
						sprintf( message, "Invalid tuple constant on line %d.", next->line );
					}
				} else {
					strcpy( message, OUT_OF_MEMORY );
				}
			} else {
				sprintf( message, "Invalid tuple string on line %d.", next->line );
			}
		} else {
			sprintf( message, "Expected '(' after '$tup' on line %d.", elem->line );
		}
	} else {
		/* Integer constant. */
		integer_value = ( int ) strtol( elem->str.string, &end, 0 );
		if( end[ 0 ] != 0 ) {
			sprintf( message, "Invalid integer constant '%.64s' at line %d.", elem->str.string, elem->line );
		}
	}
	constant->integer_value = integer_value;
	constant->string_value = string_value;
	return next;
}

static struct function* new_function( char *name, char *file, char *message ) {
	struct function *func = calloc( 1, sizeof( struct function ) );
	if( func ) {
		/*printf("Function '%s'\n", name);*/
		func->str.string = new_string( name );
		if( func->str.string ) {
			func->str.length = strlen( func->str.string );
			func->str.reference_count = 1;
			func->str.type = FUNCTION;
			func->file.reference_count = 1;
			func->file.length = strlen( file );
			func->file.string = new_string( file );
		}
		if( !func->file.string ) {
			unref_string( &func->str );
			strcpy( message, OUT_OF_MEMORY );
			func = NULL;
		}
	}
	return func;
}

static struct global_variable* new_global_variable( char *name, char *message ) {
	struct global_variable *global = calloc( 1, sizeof( struct global_variable ) );
	if( global && name ) {
		/*printf("Global '%s'\n", name);*/
		global->name = new_string( name );
		if( global->name == NULL ) {
			free( global );
			global = NULL;
		}
	}
	if( global == NULL ) {
		strcpy( message, OUT_OF_MEMORY );
	}
	return global;
}

static struct global_variable* new_array_variable( struct environment *env,
	char *name, char *message ) {
	struct array *arr;
	struct global_variable *global = calloc( 1, sizeof( struct global_variable ) );
	if( global ) {
		/*printf("Array '%s'\n", name);*/
		global->name = new_string( name );
		arr = new_array( env, 0 );
		if( global->name && arr ) {
			global->value.string_value = &arr->str;
		} else {
			dispose_global_variables( global );
			strcpy( message, OUT_OF_MEMORY );
			global = NULL;
		}
	}
	return global;
}

/* Allocate and return a new statement. Returns NULL and writes message on failure. */
struct statement* new_statement( char *message ) {
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt == NULL ) {
		strcpy( message, OUT_OF_MEMORY );
	}
	return stmt;
}

static void add_constant( struct environment *env, struct global_variable *constant ) {
	if( env->constants ){
		env->constants_tail->next = constant;
	} else {
		env->constants = constant;
	}
	env->constants_tail = constant;
}

static void add_global( struct environment *env, struct global_variable *global ) {
	if( env->globals ){
		env->globals_tail->next = global;
	} else {
		env->globals = global;
	}
	env->globals_tail = global;
}

/* Add the specified constants to env. Returns zero and writes message on failure. */
int add_constants( struct constant *constants, struct environment *env, char *message ) {
	struct global_variable *global;
	int idx = 0;
	struct constant *con = &constants[ idx++ ];
	while( con->name && message[ 0 ] == 0 ) {
		global = new_global_variable( con->name, message );
		if( global ) {
			global->value.integer_value = con->integer_value;
			if( con->string_value ) {
				global->value.string_value = new_string_literal( con->string_value );
				if( global->value.string_value ) {
					add_constant( env, global );
				} else {
					strcpy( message, OUT_OF_MEMORY );
				}
			} else {
				add_constant( env, global );
			}
		}
		con = &constants[ idx++ ];
	}
	return message[ 0 ] == 0;
}

static enum result execute_global_assignment( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable *destination = this->global;
	return this->source->evaluate( this->source, variables, destination, exception );
}

static enum result execute_local_assignment( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable *destination = &variables[ this->local ];
	return this->source->evaluate( this->source, variables, destination, exception );
}

static enum result execute_print_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable value = { 0, NULL };
	if( this->source->evaluate( this->source, variables, &value, exception ) ) {
		if( value.string_value && value.string_value->string ) {
			puts( value.string_value->string );
		} else {
			printf( "%d\n", value.integer_value );
		}
		dispose_variable( &value );
		return OKAY;
	}
	return EXCEPTION;
}

static enum result execute_error_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable value = { 0, NULL };
	if( this->source->evaluate( this->source, variables, &value, exception ) ) {
		if( value.string_value && value.string_value->string ) {
			fputs( value.string_value->string, stderr );
			fputc( '\n', stderr );
		} else {
			fprintf( stderr, "%d\n", value.integer_value );
		}
		dispose_variable( &value );
		return OKAY;
	}
	return EXCEPTION;
}

static enum result execute_write_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable value = { 0, NULL };
	if( this->source->evaluate( this->source, variables, &value, exception ) ) {
		if( value.string_value && value.string_value->string ) {
			fwrite( value.string_value->string, 1, value.string_value->length, stdout );
		} else {
			printf( "%d", value.integer_value );
		}
		dispose_variable( &value );
		return OKAY;
	}
	return EXCEPTION;
}

static enum result execute_throw_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	this->source->evaluate( this->source, variables, exception, exception );
	return EXCEPTION;
}

static enum result execute_exit_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable exit_code = { 0, NULL };
	enum result ret = this->source->evaluate( this->source, variables, &exit_code, exception );
	if( ret ) {
		ret = throw_exit( exception, exit_code.integer_value );
	}
	return ret;
}

static enum result execute_return_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	if( this->source->evaluate( this->source, variables, result, exception ) ) {
		return RETURN;
	}
	return EXCEPTION;
}

static enum result execute_break_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	return BREAK;
}

static enum result execute_continue_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	return CONTINUE;
}

static enum result execute_try_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	enum result ret = OKAY;
	struct variable *exc = &variables[ this->local ];
	struct statement *stmt = this->if_block;
	while( stmt ) {
		ret = stmt->execute( stmt, variables, result, exc );
		if( ret == OKAY ) {
			stmt = stmt->next;
		} else {
			break;
		}
	}
	if( ret == EXCEPTION ) {
		if( exc->string_value && exc->string_value->string == NULL ) {
			assign_variable( exc, exception );
		} else {
			ret = OKAY;
			stmt = this->else_block;
			while( stmt ) {
				ret = stmt->execute( stmt, variables, result, exception );
				if( ret == OKAY ) {
					stmt = stmt->next;
				} else {
					break;
				}
			}
		}
	}
	return ret;
}

static enum result execute_if_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable condition = { 0, NULL };
	struct statement *stmt = this->else_block;
	enum result ret = this->source->evaluate( this->source, variables, &condition, exception );
	if( ret ) {
		if( condition.integer_value || condition.string_value ) {
			stmt = this->if_block;
		}
		dispose_variable( &condition );
		while( stmt ) {
			ret = stmt->execute( stmt, variables, result, exception );
			if( ret == OKAY ) {
				stmt = stmt->next;
			} else {
				break;
			}
		}
	}
	return ret;
}

static enum result execute_while_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct environment *env = this->source->function->env;
	struct variable condition = { 0, NULL };
	struct statement *stmt;
	enum result ret;
	while( this->source->evaluate( this->source, variables, &condition, exception ) ) {
		if( condition.integer_value || condition.string_value ) {
			dispose_variable( &condition );
			stmt = this->if_block;
			while( stmt ) {
				ret = stmt->execute( stmt, variables, result, exception );
				if( ret == OKAY ) {
					stmt = stmt->next;
				} else if( ret == RETURN ) {
					return RETURN;
				} else if( ret == BREAK ) {
					return OKAY;
				} else if( ret == CONTINUE ) {
					break;
				} else if( ret == EXCEPTION ) {
					return EXCEPTION;
				}
			}
			if( env->interrupted ) {
				return throw( exception, this->source, 0, "Interrupted.");
			}
		} else {
			dispose_variable( &condition );
			return OKAY;
		}
	}
	return EXCEPTION;
}

static enum result execute_call_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable var = { 0, NULL };
	enum result ret = this->source->evaluate( this->source, variables, &var, exception );
	if( ret ) {
		dispose_variable( &var );
	}
	return ret;
}

static enum result execute_dim_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable var = { 0, NULL }, len = { 0, NULL };
	enum result ret = this->destination->evaluate( this->destination, variables, &var, exception );
	if( ret ) {
		ret = this->source->evaluate( this->source, variables, &len, exception );
		if( ret ) {
			if( var.string_value && var.string_value->type == ARRAY ) {
				if( len.integer_value >= 0 ) {
					if( !resize_array( ( struct array * ) var.string_value, len.integer_value ) ) {
						ret = throw( exception, this->source, 0, OUT_OF_MEMORY );
					}
				} else {
					ret = throw( exception, this->source, 0, "Negative array size." );
				}
			} else {
				ret = throw( exception, this->destination, 0, "Not an array." );
			}
			dispose_variable( &len );
		}
		dispose_variable( &var );
	}
	return ret;
}

static enum result execute_array_assignment( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct array *arr;
	struct variable var = { 0, NULL }, idx = { 0, NULL };
	enum result ret = this->destination->evaluate( this->destination, variables, &var, exception );
	if( ret ) {
		ret = this->index->evaluate( this->index, variables, &idx, exception );
		if( ret ) {
			if( var.string_value && var.string_value->type == ARRAY ) {
				arr = ( struct array * ) var.string_value;
				if( idx.integer_value >= 0 && idx.integer_value < arr->length ) {
					ret = this->source->evaluate( this->source, variables,
						&arr->array[ idx.integer_value ], exception );
				} else {
					ret = throw( exception, this->index, idx.integer_value, "Array index out of bounds." );
				}
			} else {
				ret = throw( exception, this->destination, 0, "Not an array." );
			}
			dispose_variable( &idx );
		}
		dispose_variable( &var );
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
			if( elem && elem->str.string[ 0 ] == ',' ) {
				elem = elem->next;
			}
		}
	}
	return count;
}

static enum result execute_switch_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int matched = 0;
	struct expression *case_expr;
	struct statement *stmt = this->if_block;
	struct variable switch_value = { 0, NULL }, case_value = { 0, NULL };
	enum result ret = this->source->evaluate( this->source, variables, &switch_value, exception );
	if( ret ) {
		while( stmt && ret && !matched ) {
			case_expr = stmt->source;
			while( case_expr && ret && !matched ) {
				ret = case_expr->evaluate( case_expr, variables, &case_value, exception );
				if( ret ) {
					matched = compare_variables( &switch_value, &case_value ) == 0;
					dispose_variable( &case_value );
					if( matched ) {
						ret = stmt->execute( stmt, variables, result, exception );
					}
				}
				case_expr = case_expr->next;
			}
			stmt = stmt->next;
		}
		if( ret && !matched && this->else_block ) {
			ret = this->else_block->execute( this->else_block, variables, result, exception );
		}
		dispose_variable( &switch_value );
	}
	return ret;
}

static enum result execute_case_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct statement *stmt = this->if_block;
	enum result ret = OKAY;
	while( stmt ) {
		ret = stmt->execute( stmt, variables, result, exception );
		if( ret == OKAY ) {
			stmt = stmt->next;
		} else {
			break;
		}
	}
	return ret;
}

static enum result execute_increment_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable *local = &variables[ this->local ];
	if( local->string_value ) {
		return throw( exception, this->source, 0, "Not an integer." );
	}
	local->integer_value++;
	return OKAY;
}

static enum result execute_decrement_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable *local = &variables[ this->local ];
	if( local->string_value ) {
		return throw( exception, this->source, 0, "Not an integer." );
	}
	local->integer_value--;
	return OKAY;
}

static enum result execute_save_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int count;
	char message[ 64 ];
	struct string *sval, *fval;
	struct variable str = { 0, NULL }, file = { 0, NULL };
	enum result ret = this->source->evaluate( this->source, variables, &str, exception );
	if( ret ) {
		ret = this->destination->evaluate( this->destination, variables, &file, exception );
		if( ret ) {
			sval = str.string_value;
			fval = file.string_value;
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
		dispose_variable( &str );
	}
	return ret;
}

static enum result evaluate_local( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	assign_variable( &variables[ this->index ], result );
	return OKAY;
}

static enum result evaluate_global( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	assign_variable( &this->global->value, result );
	return OKAY;
}

static enum result evaluate_call_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int idx, count;
	enum result ret = OKAY;
	struct statement *stmt;
	struct variable *locals;
	struct expression *parameter = this->parameters;
	struct function *function = this->function;
	count = sizeof( struct variable ) * function->num_variables;
	locals = alloca( count );
	memset( locals, 0, count );
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
			if( ret == OKAY ) {
				stmt = stmt->next;
			} else if( ret == EXCEPTION ) {
				break;
			} else if( ret == RETURN ) {
				ret = OKAY;
				break;
			} else {
				ret = throw( exception, this, ret, "Unhandled 'break' or 'continue'." );
				break;
			}
		}
		if( ret && stmt == NULL ) {
			dispose_variable( result );
			result->integer_value = 0;
			result->string_value = NULL;
		}
	}
	idx = 0, count = function->num_variables;
	while( idx < count ) {
		dispose_variable( &locals[ idx++ ] );
	}
	return ret;
}

static enum result evaluate_refcall_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int count;
	struct function *function;
	struct expression expr = { 0 };
	struct variable func = { 0, NULL };
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, variables, &func, exception );
	if( ret ) {
		if( func.string_value && func.string_value->type == FUNCTION ) {
			function = ( struct function * ) func.string_value;
			expr.line = this->line;
			expr.function = function;
			parameter = parameter->next;
			count = 0;
			while( parameter ) {
				count++;
				parameter = parameter->next;
			}
			if( function->num_parameters == count ) {
				expr.parameters = this->parameters->next;
				ret = evaluate_call_expression( &expr, variables, result, exception );
			} else {
				ret = throw( exception, this, count, "Incorrect number of parameters to function." );
			}
		} else {
			ret = throw( exception, this, func.integer_value, "Not a function reference." );
		}
		dispose_variable( &func );
	}
	return ret;
}

static enum result evaluate_array_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int idx, len;
	char msg[ 128 ] = "";
	struct variable var = { 0, NULL }, *values;
	struct global_variable inputs, *input;
	struct array *arr;
	enum result ret = this->parameters->evaluate( this->parameters, variables, &var, exception );
	if( ret ) {
		if( var.string_value && var.string_value->type == ELEMENT ) {
			inputs.next = NULL;
			len = parse_constant_list( ( struct element * ) var.string_value, &inputs, msg );
			if( msg[ 0 ] == 0 ) {
				arr = new_array( this->function->env, len );
				if( arr ) {
					idx = 0;
					input = inputs.next;
					values = arr->array;
					while( idx < len ) {
						assign_variable( &input->value, &values[ idx++ ] );
						input = input->next;
					}
					dispose_variable( result );
					result->integer_value = 0;
					result->string_value = &arr->str;
				} else {
					ret = throw( exception, this, 0, OUT_OF_MEMORY );
				}
			} else {
				ret = throw( exception, this, 0, msg );
			}
			dispose_global_variables( inputs.next );
		} else if( var.integer_value >= 0 ) {
			arr = new_array( this->function->env, var.integer_value );
			if( arr ) {
				dispose_variable( result );
				result->integer_value = 0;
				result->string_value = &arr->str;
			} else {
				ret = throw( exception, this, 0, OUT_OF_MEMORY );
			}
		} else {
			ret = throw( exception, this, var.integer_value, "Invalid array length." );
		}
		dispose_variable( &var );
	}
	return ret;
}

static enum result evaluate_index_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct array *arr;
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL }, idx = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, variables, &var, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &idx, exception );
		if( ret ) {
			if( var.string_value && var.string_value->type == ARRAY ) {
				arr = ( struct array * ) var.string_value;
				if( idx.integer_value >= 0 && idx.integer_value < arr->length ) {
					assign_variable( &arr->array[ idx.integer_value ], result );
				} else {
					ret = throw( exception, this, idx.integer_value, "Array index out of bounds." );
				}
			} else {
				ret = throw( exception, this, 0, "Not an array." );
			}
			dispose_variable( &idx );
		}
		dispose_variable( &var );
	}
	return ret;
}

static enum result evaluate_integer_constant_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->string_value = NULL;
	result->integer_value = this->index;
	return OKAY;
}

static struct structure* get_structure( struct structure *structures, char *name ) {
	size_t len;
	char *chr = strchr( name, '.' );
	if( chr ) {
		len = chr - name;
	} else {
		len = strlen( name );
	}
	while( structures && ( strlen( structures->name ) != len || strncmp( structures->name, name, len ) ) ) {
		structures = structures->next;
	}
	return structures;
}

static struct function* get_function( struct environment *env, char *name ) {
	struct function *func = env->functions;
	while( func && strcmp( func->str.string, name ) ) {
		func = func->next;
	}
	if( func == NULL ) {
		func = env->entry_points;
		while( func && strcmp( func->str.string, name ) ) {
			func = func->next;
		}
	}
	return func;
}

static struct global_variable* get_global_variable( struct global_variable *globals, char *name ) {
	while( globals && strcmp( globals->name, name ) ) {
		globals = globals->next;
	}
	return globals;
}

static int add_global_constant( struct element *elem, struct environment *env,
	struct function *func, struct expression *initializer, struct statement *prev, char *message ) {
	struct global_variable *global = new_global_variable( elem->str.string, message );
	if( global ) {
		global->initializer = initializer;
		add_constant( env, global );
	}
	return message[ 0 ] == 0;
}

static int add_global_variable( struct element *elem, struct environment *env,
	struct function *func, struct expression *initializer, struct statement *prev, char *message ) {
	struct global_variable *global = new_global_variable( elem->str.string, message );
	if( global ) {
		global->initializer = initializer;
		add_global( env, global );
	}
	return message[ 0 ] == 0;
}

static int add_array_variable( struct element *elem, struct environment *env,
	struct function *func, struct expression *initializer, struct statement *prev, char *message ) {
	struct global_variable *array = new_array_variable( env, elem->str.string, message );
	if( array ) {
		array->initializer = initializer;
		add_global( env, array );
	}
	return message[ 0 ] == 0;
}

static int add_local_variable( struct element *elem, struct environment *env,
	struct function *func, struct expression *initializer, struct statement *prev, char *message ) {
	struct statement *stmt;
	char *name = elem->str.string;
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
			if( initializer ) {
				stmt = new_statement( message );
				if( stmt ) {
					prev->next = stmt;
					stmt->source = initializer;
					stmt->local = func->num_variables - 1;
					stmt->execute = execute_local_assignment;
				}
			}
		} else {
			dispose_string_list( param );
			sprintf( message, "Local variable '%.8s' already defined on line %d.", name, elem->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return message[ 0 ] == 0;
}

static struct element* parse_variable_declaration( struct element *elem,
	struct environment *env, struct function *func, struct statement *prev,
	int (*add)( struct element *elem, struct environment *env, struct function *func,
		struct expression *initializer, struct statement *prev, char *message ),
	char *message ) {
	struct expression expr = { 0 }, *array_expr;
	struct element *next;
	while( elem && elem->str.string[ 0 ] != ';' && message[ 0 ] == 0 ) {
		if( prev && prev->next ) {
			prev = prev->next;
		}
		if( elem->str.string[ 0 ] == '[' && validate_decl( elem->child, env, message ) ) {
			parse_expression( elem->child->next, env, func, &expr, message );
			if( message[ 0 ] == 0 ) {
				array_expr = calloc( 1, sizeof( struct expression ) );
				if( array_expr ) {
					array_expr->line = elem->line;
					array_expr->function = func;
					array_expr->parameters = expr.next;
					array_expr->evaluate = evaluate_array_expression;
					if( add( elem->child, env, func, array_expr, prev, message ) ) {
						elem = elem->next;
					} else {
						dispose_expressions( array_expr );
					}
				} else {
					strcpy( message, OUT_OF_MEMORY );
				}
			}
		} else if( validate_decl( elem, env, message ) ) {
			if( elem->next->str.string[ 0 ] == '=' ) {
				next = parse_expression( elem->next->next, env, func, &expr, message );
				if( message[ 0 ] == 0 ) {
					if( add( elem, env, func, expr.next, prev, message ) ) {
						elem = next;
					} else {
						dispose_expressions( expr.next );
					}
				}
			} else {
				if( add( elem, env, func, NULL, prev, message ) ) {
					elem = elem->next;
				}
			}
		}
		if( elem && elem->str.string[ 0 ] == ',' ) {
			elem = elem->next;
		}
	}
	if( elem && elem->str.string[ 0 ] == ';' ) {
		elem = elem->next;
	}
	return elem;
}

static struct element* parse_comment( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return elem->next->next;
}

static struct element* parse_const_declaration( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_variable_declaration( elem->next, env, func, prev, add_global_constant, message);
}

static struct element* parse_global_declaration( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_variable_declaration( elem->next, env, func, prev, add_global_variable, message);
}

static struct element* parse_array_declaration( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_variable_declaration( elem->next, env, func, prev, add_array_variable, message);
}

static struct element* parse_local_declaration( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_variable_declaration( elem->next, env, func, prev, add_local_variable, message);
}

static enum result evaluate_bitwise_not_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable var = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, variables, &var, exception );
	if( ret ) {
		dispose_variable( result );
		result->integer_value = ~var.integer_value;
		result->string_value = NULL;
		dispose_variable( &var );
	}
	return ret;
}

static enum result evaluate_logical_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int value;
	struct variable var = { 0, NULL };
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, variables, &var, exception );
	if( ret ) {
		parameter = parameter->next;
		value = var.integer_value || var.string_value;
		dispose_variable( &var );
		if( this->index == '!' ) {
			dispose_variable( result );
			result->integer_value = !value;
			result->string_value = NULL;
		} else if( this->index == '&' ) {
			if( value ) {
				ret = parameter->evaluate( parameter, variables, &var, exception );
				if( ret ) {
					dispose_variable( result );
					result->integer_value = var.integer_value || var.string_value;
					result->string_value = NULL;
					dispose_variable( &var );
				}
			} else {
				dispose_variable( result );
				result->integer_value = value;
				result->string_value = NULL;
			}
		} else {
			if( value ) {
				dispose_variable( result );
				result->integer_value = value;
				result->string_value = NULL;
			} else {
				ret = parameter->evaluate( parameter, variables, &var, exception );
				if( ret ) {
					dispose_variable( result );
					result->integer_value = var.integer_value || var.string_value;
					result->string_value = NULL;
					dispose_variable( &var );
				}
			}
		}
	}
	return ret;
}

static enum result evaluate_arithmetic_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable var = { 0, NULL };
	struct expression *parameter = this->parameters;
	enum result ret = OKAY;
	int lhs, rhs, value;
	if( parameter->evaluate == evaluate_integer_constant_expression ) {
		lhs = parameter->index;
	} else if( parameter->evaluate == evaluate_local ) {
		lhs = variables[ parameter->index ].integer_value;
	} else if( parameter->evaluate == evaluate_global ) {
		lhs = parameter->global->value.integer_value;
	} else {
		ret = parameter->evaluate( parameter, variables, &var, exception );
		lhs = var.integer_value;
		dispose_variable( &var );
	}
	if( ret ) {
		parameter = parameter->next;
		if( parameter->evaluate == evaluate_integer_constant_expression ) {
			rhs = parameter->index;
		} else if( parameter->evaluate == evaluate_local ) {
			rhs = variables[ parameter->index ].integer_value;
		} else if( parameter->evaluate == evaluate_global ) {
			rhs = parameter->global->value.integer_value;
		} else {
			ret = parameter->evaluate( parameter, variables, &var, exception );
			rhs = var.integer_value;
			dispose_variable( &var );
		}
		if( ret ) {
			switch( this->index ) {
				case '%':
					if( rhs != 0 ) {
						value = lhs % rhs;
					} else {
						value = 0;
						ret = throw( exception, this, 0, "Modulo division by zero." );
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
						ret = throw( exception, this, 0, "Integer division by zero." );
					}
					break;
				case '<': value = lhs  < rhs; break;
				case '>': value = lhs  > rhs; break;
				case 'A': value = lhs << rhs; break;
				case 'B': value = lhs >> rhs; break;
				case 'G': value = lhs >= rhs; break;
				case 'L': value = lhs <= rhs; break;
				case '^': value = lhs  ^ rhs; break;
				case '=': value = lhs == rhs; break;
				case '|': value = lhs  | rhs; break;
				default :
					value = 0;
					ret = throw( exception, this, 0, "Unhandled integer operator." );
					break;
			}
			if( ret ) {
				dispose_variable( result );
				result->integer_value = value;
				result->string_value = NULL;
			}
		}
	}
	return ret;
}

static enum result evaluate_int_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int val;
	char *end;
	struct variable str = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, variables, &str, exception );
	if( ret ) {
		if( str.string_value && str.string_value->string ) {
			val = ( int ) strtol( str.string_value->string, &end, 0 );
			if( end[ 0 ] == 0 && str.string_value->string != end ) {
				dispose_variable( result );
				result->integer_value = val;
				result->string_value = NULL;
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

static enum result evaluate_str_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int str_len = 0, len;
	char num[ 24 ], *val;
	enum result ret = OKAY;
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct string *str = NULL, *new;
	while( parameter && ret ) {
		ret = parameter->evaluate( parameter, variables, &var, exception );
		if( ret ) {
			if( var.string_value && var.string_value->string ) {
				len = var.string_value->length;
				val = var.string_value->string;
			} else {
				sprintf( num, "%d", var.integer_value );
				len = strlen( num );
				val = num;
			}
			if( MAX_INTEGER - len > str_len ) {
				new = new_string_value( str_len + len );
				if( new ) {
					if( str ) {
						memcpy( new->string, str->string, str_len );
						free( str );
					}
					memcpy( &new->string[ str_len ], val, len );
					str_len += len;
					str = new;
				} else {
					ret = throw( exception, this, 0, OUT_OF_MEMORY );
				}
			} else {
				ret = throw( exception, this, len, "String too large." );
			}
			dispose_variable( &var );
			parameter = parameter->next;
		}
	}
	if( ret ) {
		dispose_variable( result );
		result->integer_value = 0;
		result->string_value = str;
	} else {
		free( str );
	}
	return ret;
}

static enum result evaluate_asc_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct string *str;
	struct variable val = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, variables, &val, exception );
	if( ret ) {
		str = new_string_value( 1 );
		if( str ) {
			str->string[ 0 ] = val.integer_value;
			dispose_variable( result );
			result->integer_value = 0;
			result->string_value = str;
		} else {
			ret = throw( exception, this, 0, OUT_OF_MEMORY );
		}
		dispose_variable( &val );
	}
	return ret;
}

static enum result evaluate_len_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable len = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, variables, &len, exception );
	if( ret ) {
		if( len.string_value ) {
			dispose_variable( result );
			if( len.string_value->type == ARRAY ) {
				result->integer_value = ( ( struct array * ) len.string_value )->length;
			} else {
				result->integer_value = len.string_value->length;
			}
			result->string_value = NULL;
		} else {
			ret = throw( exception, this, 0, "Not a string or array." );
		}
		dispose_variable( &len );
	}
	return ret;
}

static enum result evaluate_tup_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable str = { 0, NULL }, val = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, variables, &str, exception );
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

static enum result evaluate_load_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	long len;
	char message[ 64 ];
	struct string *str;
	struct variable file = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, variables, &file, exception );
	if( ret ) {
		if( file.string_value && file.string_value->string ) {
			len = load_file( file.string_value->string, NULL, message );
			if( len >= 0 ) {
				if( len < MAX_INTEGER ) {
					str = new_string_value( len );
					if( str ) {
						len = load_file( file.string_value->string, str->string, message );
						if( len >= 0 ) {
							dispose_variable( result );
							result->integer_value = 0;
							result->string_value = str;
						} else {
							free( str );
							ret = throw( exception, this, 0, message );
						}
					} else {
						ret = throw( exception, this, 0, OUT_OF_MEMORY );
					}
				} else {
					ret = throw( exception, this, 0, "File too large." );
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

static enum result evaluate_flen_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	long len;
	char message[ 64 ];
	struct variable file = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, variables, &file, exception );
	if( ret ) {
		if( file.string_value && file.string_value->string ) {
			len = load_file( file.string_value->string, NULL, message );
			if( len >= 0 ) {
				if( len < MAX_INTEGER ) {
					dispose_variable( result );
					result->integer_value = len;
					result->string_value = NULL;
				} else {
					ret = throw( exception, this, 0, "File too large." );
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

static enum result evaluate_cmp_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable var1 = { 0, NULL }, var2 = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, variables, &var1, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &var2, exception );
		if( ret ) {
			result->integer_value = compare_variables( &var1, &var2 );
			result->string_value = NULL;
			dispose_variable( &var2 );
		}
		dispose_variable( &var1 );
	}
	return ret;
}

static enum result evaluate_chr_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable str = { 0, NULL }, idx = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, variables, &str, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &idx, exception );
		if( ret ) {
			if( str.string_value && str.string_value->string ) {
				if( idx.integer_value >= 0 && idx.integer_value < str.string_value->length ) {
					dispose_variable( result );
					result->integer_value = ( signed char ) str.string_value->string[ idx.integer_value ];
					result->string_value = NULL;
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

static enum result evaluate_sub_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	char *data;
	int offset, length;
	struct string *str;
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL }, idx = { 0, NULL }, len = { 0, NULL }, *arr;
	enum result ret = parameter->evaluate( parameter, variables, &var, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &idx, exception );
		if( ret ) {
			parameter = parameter->next;
			ret = parameter->evaluate( parameter, variables, &len, exception );
			if( ret ) {
				if( var.string_value && var.string_value->string ) {
					if( var.string_value->type == ARRAY ) {
						length = ( ( struct array * ) var.string_value )->length;
					} else {
						length = var.string_value->length;
					}
					if( idx.integer_value >= 0 && len.integer_value >= 0
					&& MAX_INTEGER - len.integer_value >= idx.integer_value
					&& idx.integer_value + len.integer_value <= length ) {
						str = new_string_value( len.integer_value );
						if( str ) {
							if( var.string_value->type == ARRAY ) {
								data = str->string;
								arr = ( ( struct array * ) var.string_value )->array;
								offset = 0;
								while( offset < len.integer_value ) {
									data[ offset ] = arr[ offset + idx.integer_value ].integer_value;
									offset++;
								}
							} else {
								memcpy( str->string, &var.string_value->string[ idx.integer_value ],
									sizeof( char ) * len.integer_value );
							}
							dispose_variable( result );
							result->integer_value = 0;
							result->string_value = str;
						} else {
							ret = throw( exception, this, 0, OUT_OF_MEMORY );
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
		dispose_variable( &var );
	}
	return ret;
}

static struct element* new_integer_element( int value ) {
	char integer[ 32 ];
	struct element *elem;
	sprintf( integer, "%d", value );
	elem = new_element( strlen( integer ) );
	if( elem ) {
		strcpy( elem->str.string, integer );
	}
	return elem;
}

static struct element* new_string_element( struct string *value ) {
	struct element *elem = NULL;
	int length = write_byte_string( value->string, value->length, NULL );
	if( length > 0 ) {
		elem = new_element( length );
		if( elem ) {
			write_byte_string( value->string, value->length, elem->str.string );
		}
	}
	return elem;
}

static struct element* new_tuple_element( int int_value, struct string *str_value ) {
	struct element *elem = new_element( 4 );
	if( elem ) {
		strcpy( elem->str.string, "$tup" );
		elem->next = new_element( 2 );
		if( elem->next ) {
			strcpy( elem->next->str.string, "()" );
			elem->next->child = new_string_element( str_value );
			if( elem->next->child ) {
				elem->next->child->next = new_integer_element( int_value );
			}
		}
	}
	if( elem && !( elem->next && elem->next->child && elem->next->child->next ) ) {
		unref_string( &elem->str );
		elem = NULL;
	}
	return elem;
}

static struct element* new_literal_element( struct element *child ) {
	struct element *elem = new_element( 1 );
	if( elem ) {
		elem->str.string[ 0 ] = '$';
		elem->next = new_element( 2 );
		if( elem->next ) {
			strcpy( elem->next->str.string, "{}" );
			child->str.reference_count++;
			elem->next->child = child;
		}
	}
	if( elem && !elem->next ) {
		unref_string( &elem->str );
		elem = NULL;
	}
	return elem;
}

static struct element* array_to_element( struct array *arr ) {
	struct element *head = NULL, *tail = NULL, *elem;
	int idx = 0, count = arr->length;
	struct variable *var;
	while( idx < count ) {
		var = &arr->array[ idx++ ];
		if( var->string_value ) {
			if( var->string_value->type == ELEMENT ) {
				elem = new_literal_element( ( struct element * ) var->string_value );
			} else if( var->integer_value ) {
				elem = new_tuple_element( var->integer_value, var->string_value );
			} else {
				elem = new_string_element( var->string_value );
			}
		} else {
			elem = new_integer_element( var->integer_value );
		}
		if( elem ) {
			while( elem ) {
				if( tail ) {
					tail->next = elem;
					tail = elem;
				} else {
					head = tail = elem;
				}
				elem = elem->next;
			}
		} else if( head ) {
			unref_string( &head->str );
			return NULL;
		}
	}
	return head;
}

static enum result evaluate_values_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable arr = { 0, NULL };
	struct element *elem;
	enum result ret = parameter->evaluate( parameter, variables, &arr, exception );
	if( ret ) {
		if( arr.string_value && arr.string_value->type == ARRAY
		&& ( ( struct array * ) arr.string_value )->length > 0 ) {
			elem = array_to_element( ( struct array * ) arr.string_value );
			if( elem ) {
				dispose_variable( result );
				result->integer_value = 0;
				result->string_value = &elem->str;
			} else {
				ret = throw( exception, this, 0, OUT_OF_MEMORY );
			}
		} else {
			ret = throw( exception, this, arr.integer_value, "Not an array or no values." );
		}
		dispose_variable( &arr );
	}
	return ret;
}

static enum result evaluate_argc_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = this->function->env->argc;
	result->string_value = NULL;
	return OKAY;
}

static enum result evaluate_argv_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	char *val;
	enum result ret;
	struct string *str;
	struct expression *parameter = this->parameters;
	struct variable idx = { 0, NULL };
	ret = parameter->evaluate( parameter, variables, &idx, exception );
	if( ret ) {
		if( idx.integer_value >= 0 && idx.integer_value < this->function->env->argc ) {
			val = this->function->env->argv[ idx.integer_value ];
			str = new_string_value( strlen( val ) );
			if( str ) {
				memcpy( str->string, val, str->length );
				dispose_variable( result );
				result->integer_value = 0;
				result->string_value = str;
			} else {
				ret = throw( exception, this, 0, OUT_OF_MEMORY );
			}
		} else {
			ret = throw( exception, this, idx.integer_value, "Command-line argument index out of bounds." );
		}
		dispose_variable( &idx );
	}
	return ret;
}

static enum result evaluate_time_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	enum result ret = OKAY;
	time_t seconds = time( NULL );
	char *time_str = ctime( &seconds );
	struct string *str = new_string_value( strlen( time_str ) );
	if( str ) {
		strcpy( str->string, time_str );
		str->string[ str->length - 1 ] = 0;
		dispose_variable( result );
		result->integer_value = seconds;
		result->string_value = str;
	} else {
		ret = throw( exception, this, 0, OUT_OF_MEMORY );
	}
	return ret;
}

static enum result evaluate_next_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable prev = { 0, NULL };
	struct element *next;
	enum result ret = parameter->evaluate( parameter, variables, &prev, exception );
	if( ret ) {
		if( prev.string_value && prev.string_value->type == ELEMENT ) {
			next = ( ( struct element * ) prev.string_value )->next;
			if( next ) {
				next->str.reference_count++;
			}
			dispose_variable( result );
			result->integer_value = 0;
			result->string_value = &next->str;
		} else {
			ret = throw( exception, this, 0, "Not an element." );
		}
		dispose_variable( &prev );
	}
	return ret;
}

static enum result evaluate_child_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable parent = { 0, NULL };
	struct element *child;
	enum result ret = parameter->evaluate( parameter, variables, &parent, exception );
	if( ret ) {
		if( parent.string_value && parent.string_value->type == ELEMENT ) {
			child = ( ( struct element * ) parent.string_value )->child;
			if( child ) {
				child->str.reference_count++;
			}
			dispose_variable( result );
			result->integer_value = 0;
			result->string_value = &child->str;
		} else {
			ret = throw( exception, this, 0, "Not an element." );
		}
		dispose_variable( &parent );
	}
	return ret;
}

static enum result evaluate_elem_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable elem = { 0 }, child = { 0 }, next = { 0 };
	struct element *value;
	enum result ret = parameter->evaluate( parameter, variables, &elem, exception );
	if( ret ) {
		if( elem.string_value && elem.string_value->type == ELEMENT ) {
			parameter = parameter->next;
			ret = parameter->evaluate( parameter, variables, &child, exception );
			if( ret ) {
				if( ( child.string_value && child.string_value->type == ELEMENT )
				|| !( child.string_value || child.integer_value ) ) {
					if( !child.string_value || strchr( "([{", elem.string_value->string[ 0 ] ) ) {
						parameter = parameter->next;
						ret = parameter->evaluate( parameter, variables, &next, exception );
						if( ret ) {
							if( ( next.string_value && next.string_value->type == ELEMENT )
							|| !( next.string_value || next.integer_value ) ) {
								if( elem.string_value->reference_count > 1 ) {
									value = new_element( elem.string_value->length );
									if( value ) {
										value->line = ( ( struct element * ) elem.string_value )->line;
										memcpy( value->str.string, elem.string_value->string, elem.string_value->length );
									} else {
										ret = throw( exception, this, 0, OUT_OF_MEMORY );
									}
								} else { /* Re-use element. */
									value = ( struct element * ) elem.string_value;
									if( value->child ) {
										unref_string( &value->child->str );
										value->child = NULL;
									}
									if( value->next ) {
										unref_string( &value->next->str );
										value->next = NULL;
									}
									value->str.reference_count++;
								}
								if( ret ) {
									if( child.string_value ) {
										value->child = ( struct element * ) child.string_value;
										child.string_value->reference_count++;
									}
									if( next.string_value ) {
										value->next = ( struct element * ) next.string_value;
										next.string_value->reference_count++;
									}
									dispose_variable( result );
									result->integer_value = 0;
									result->string_value = &value->str;
								}
							} else {
								ret = throw( exception, this, next.integer_value, "Not an element." );
							}
							dispose_variable( &next );
						}
					} else {
						ret = throw( exception, this, 0, "Parent elements must have value '()', '[]' or '{}'." );
					}
				} else {
					ret = throw( exception, this, child.integer_value, "Not an element." );
				}
				dispose_variable( &child );
			}
		} else {
			ret = throw( exception, this, elem.integer_value, "Not an element." );
		}
		dispose_variable( &elem );
	}
	return ret;
}

static enum result evaluate_parse_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable string = { 0, NULL };
	struct element *elem;
	char message[ 128 ] = "";
	enum result ret = parameter->evaluate( parameter, variables, &string, exception );
	if( ret ) {
		if( string.string_value ) {
			elem = parse_element( string.string_value->string, message );
			if( message[ 0 ] == 0 ) {
				dispose_variable( result );
				result->integer_value = 0;
				result->string_value = &elem->str;
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

static enum result evaluate_unparse_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int len;
	struct string *str;
	struct expression *parameter = this->parameters;
	struct variable elem = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, variables, &elem, exception );
	if( ret ) {
		if( elem.string_value && elem.string_value->type == ELEMENT ) {
			len = write_element( ( struct element * ) elem.string_value, NULL );
			if( len >= 0 ) {
				str = new_string_value( len );
				if( str ) {
					write_element( ( struct element * ) elem.string_value, str->string );
					dispose_variable( result );
					result->integer_value = 0;
					result->string_value = str;
				} else {
					ret = throw( exception, this, 0, OUT_OF_MEMORY );
				}
			} else {
				ret = throw( exception, this, 0, "String too large." );
			}
		} else {
			ret = throw( exception, this, 0, "Not an element." );
		}
		dispose_variable( &elem );
	}
	return ret;
}

static enum result evaluate_quote_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int length;
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct string *str;
	enum result ret = parameter->evaluate( parameter, variables, &var, exception );
	if( ret ) {
		if( var.string_value ) {
			length = write_byte_string( var.string_value->string,
				var.string_value->length, NULL );
			if( length >= 0 ) {
				str = new_string_value( length );
				if( str ) {
					write_byte_string( var.string_value->string,
						var.string_value->length, str->string );
					dispose_variable( result );
					result->integer_value = 0;
					result->string_value = str;
				} else {
					ret = throw( exception, this, 0, OUT_OF_MEMORY );
				}
			} else {
				ret = throw( exception, this, 0, "String too large." );
			}
		} else {
			ret = throw( exception, this, 0, "Not a string." );
		}
		dispose_variable( &var );
	}
	return ret;
}

static enum result evaluate_unquote_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct string *str;
	enum result ret = parameter->evaluate( parameter, variables, &var, exception );
	if( ret ) {
		if( var.string_value ) {
			str = new_string_literal( var.string_value->string );
			if( str ) {
				dispose_variable( result );
				result->integer_value = 0;
				result->string_value = str;
			} else {
				ret = throw( exception, this, 0, OUT_OF_MEMORY );
			}
		} else {
			ret = throw( exception, this, 0, "Not a string." );
		}
		dispose_variable( &var );
	}
	return ret;
}

static enum result evaluate_line_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable elem = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, variables, &elem, exception );
	if( ret ) {
		if( elem.string_value && elem.string_value->type == ELEMENT ) {
			dispose_variable( result );
			result->integer_value = ( ( struct element * ) elem.string_value )->line;
			result->string_value = NULL;
		} else {
			ret = throw( exception, this, 0, "Not an element." );
		}
		dispose_variable( &elem );
	}
	return ret;
}

static enum result evaluate_hex_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int len;
	struct string *str;
	struct variable val = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, variables, &val, exception );
	if( ret ) {
		str = new_string_value( sizeof( int ) * 2 + 4 );
		if( str ) {
			if( val.integer_value < 0 ) {
				len = sprintf( str->string, "-0x%08x", ( ~val.integer_value ) + 1 );
			} else {
				len = sprintf( str->string, " 0x%08x", val.integer_value );
			}
			if( len > 0 ) {
				str->length = len;
				dispose_variable( result );
				result->integer_value = 0;
				result->string_value = str;
			} else {
				free( str );
				ret = throw( exception, this, len, "Output error." );
			}
		} else {
			ret = throw( exception, this, 0, OUT_OF_MEMORY );
		}
		dispose_variable( &val );
	}
	return ret;
}

static enum result evaluate_pack_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int idx, len, in;
	char *out;
	struct string *str;
	struct variable val = { 0, NULL }, *src;
	enum result ret = this->parameters->evaluate( this->parameters, variables, &val, exception );
	if( ret ) {
		if( val.string_value && val.string_value->type == ARRAY ) {
			src = ( ( struct array * ) val.string_value )->array;
			len = ( ( struct array * ) val.string_value )->length * 4;
		} else {
			src = &val;
			len = 4;
		}
		str = new_string_value( len );
		if( str ) {
			idx = 0;
			out = str->string;
			while( idx < len ) {
				in = src[ idx >> 2 ].integer_value;
				out[ idx ] = in >> 24;
				out[ idx + 1 ] = in >> 16;
				out[ idx + 2 ] = in >> 8;
				out[ idx + 3 ] = in;
				idx += 4;
			}
			out[ idx ] = 0;
			dispose_variable( result );
			result->integer_value = 0;
			result->string_value = str;
		} else {
			ret = throw( exception, this, 0, OUT_OF_MEMORY );
		}
		dispose_variable( &val );
	}
	return ret;
}

/* Unpack a 32-bit big-endian integer from str at the specified index. */
int unpack( char *str, int idx ) {
	return ( ( str[ idx * 4 ] & 0xFF ) << 24 ) | ( ( str[ idx * 4 + 1 ] & 0xFF ) << 16 )
		| ( (  str[ idx * 4 + 2 ] & 0xFF ) <<  8 ) | ( str[ idx * 4 + 3 ] & 0xFF );
}

static enum result evaluate_unpack_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable str = { 0, NULL }, idx = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, variables, &str, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &idx, exception );
		if( ret ) {
			if( str.string_value ) {
				if( idx.integer_value >= 0 && idx.integer_value * 4 < str.string_value->length - 3 ) {
					dispose_variable( result );
					result->integer_value = unpack( str.string_value->string, idx.integer_value );
					result->string_value = NULL;
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

static enum result evaluate_func_ref_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable src = { 0, NULL };
	src.string_value = &this->function->str;
	assign_variable( &src, result );
	return OKAY;
}

static enum result evaluate_eq_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable lhs = { 0, NULL }, rhs = { 0, NULL };
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, variables, &lhs, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &rhs, exception );
		if( ret ) {
			dispose_variable( result );
			result->integer_value = compare_variables( &lhs, &rhs ) == 0;
			result->string_value = NULL;
			dispose_variable( &rhs );
		}
		dispose_variable( &lhs );
	}
	return ret;
}

static enum result evaluate_chop_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL }, sep = { 0, NULL };
	struct string *str;
	enum result ret = parameter->evaluate( parameter, variables, &var, exception );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, variables, &sep, exception );
		if( ret ) {
			if( var.string_value && sep.string_value ) {
				str = new_string_value( chop( var.string_value->string, sep.string_value->string ) );
				if( str ) {
					memcpy( str->string, var.string_value->string, str->length );
					dispose_variable( result );
					result->integer_value = 0;
					result->string_value = str;
				} else {
					ret = throw( exception, this, 0, OUT_OF_MEMORY );
				}
			} else {
				ret = throw( exception, this, 0, "Not a string." );
			}
			dispose_variable( &sep );
		}
		dispose_variable( &var );
	}
	return ret;
}

static enum result evaluate_interrupted_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct environment *env = this->function->env; 
	dispose_variable( result );
	result->integer_value = env->interrupted;
	result->string_value = NULL;
	env->interrupted = 0;
	return OKAY;
}

static enum result evaluate_source_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable src = { 0, NULL };
	src.string_value = &this->function->file;
	assign_variable( &src, result );
	return OKAY;
}

static struct operator* get_operator( char *name, struct environment *env ) {
	struct operator *oper = env->operators;
	while( oper && strcmp( oper->name, name ) ) {
		oper = oper->next;
	}
	return oper;
}

static struct element* parse_infix_expression( struct element *elem, struct environment *env,
	struct function *func, struct expression *expr, char *message ) {
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
				oper = get_operator( child->str.string, env );
				if( oper ) {
					expr->index = oper->oper;
					expr->evaluate = oper->evaluate;
					if( oper->num_operands > 0 ) {
						parse_expressions( child->next, env, func, 0, expr->parameters, &num_operands, message );
						num_operands++;
						if( message[ 0 ] == 0 ) {
							if( num_operands == oper->num_operands ) {
								next = next->next;
							} else {
								sprintf( message, "Wrong number of arguments to '%.64s()' on line %d.", oper->name, child->line );
							}
						}
					} else {
						sprintf( message, "Wrong number of arguments to '%.64s()' on line %d.", oper->name, child->line );
					}
				} else {
					sprintf( message, "Unhandled operator '%.64s' on line %d.", child->str.string, child->line );
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
	struct function *func, struct expression *expr, char *message ) {
	struct element *next = elem->next;
	struct expression prev;
	int num_operands;
	struct operator *oper = get_operator( elem->str.string, env );
	if( oper ) {
		expr->index = oper->oper;
		expr->evaluate = oper->evaluate;
		if( oper->num_operands != 0 ) {
			if( next && next->str.string[ 0 ] == '(' ) {
				prev.next = NULL;
				parse_expressions( next->child, env, func, 0, &prev, &num_operands, message );
				expr->parameters = prev.next;
				if( message[ 0 ] == 0 ) {
					if( num_operands == oper->num_operands || ( oper->num_operands < 0 && num_operands > 0 ) ) {
						next = next->next;
					} else {
						sprintf( message, "Wrong number of arguments to '%.64s()' on line %d.", oper->name, next->line );
					}
				}
			} else {
				sprintf( message, "Expected '(' after '%.64s' on line %d.", oper->name, elem->line );
			}
		}
	} else {
		sprintf( message, "Unhandled expression '%.64s' on line %d.", elem->str.string, elem->line );
	}
	return next;
}

static struct element* parse_call_expression( struct element *elem, struct environment *env,
	struct function *func, struct function *decl, struct expression *expr, char *message ) {
	struct element *next = elem->next;
	struct expression prev;
	int num_params;
	if( next && next->str.string[ 0 ] == '(' ) {
		expr->function = decl;
		expr->evaluate = evaluate_call_expression;
		prev.next = NULL;
		parse_expressions( next->child, env, func, 0, &prev, &num_params, message );
		expr->parameters = prev.next;
		if( message[ 0 ] == 0 ) {
			if( num_params == expr->function->num_parameters ) {
				next = next->next;
			} else {
				sprintf( message, "Wrong number of arguments to '%.64s()' on line %d.", elem->str.string, next->line );
			}
		}
	} else {
		sprintf( message, "Expected '(' after function name on line %d.", next->line );
	}
	return next;
}

static struct element* parse_func_ref_expression( struct element *elem, struct environment *env,
	struct function *func, struct expression *expr, char *message ) {
	char *name = &elem->str.string[ 1 ];
	struct function *function = get_function( env, name );
	if( function ) {
		expr->function = function;
		expr->evaluate = evaluate_func_ref_expression;
	} else {
		sprintf( message, "Function '%.64s' not defined on line %d.", name, elem->line );
	}
	return elem->next;
}

static struct element* parse_index_expression( struct element *elem, struct environment *env,
	struct function *func, struct expression *expr, char *message ) {
	struct expression prev;
	int num_params;
	prev.next = NULL;
	parse_expressions( elem->child, env, func, 0, &prev, &num_params, message );
	expr->parameters = prev.next;
	if( message[ 0 ] == 0 ) {
		if( num_params == 2 ) {
			expr->evaluate = evaluate_index_expression;
		} else {
			sprintf( message, "Invalid index expression on line %d.", elem->line );
		}
	}
	return elem->next;
}

static struct element* parse_struct_expression( struct element *elem, struct environment *env,
	struct structure *struc, struct expression *expr, char *message ) {
	int idx;
	char *field = strchr( elem->str.string, '.' );
	if( field ) {
		idx = get_string_list_index( struc->fields, &field[ 1 ] );
		if( idx >= 0 ) {
			expr->index = idx;
		} else {
			sprintf( message, "Field '%.64s' not declared on line %d.", elem->str.string, elem->line );
		}
	} else {
		expr->index = struc->length;
	}
	expr->evaluate = evaluate_integer_constant_expression;
	return elem->next;
}

static struct element* parse_expression( struct element *elem, struct environment *env,
	struct function *func, struct expression *prev, char *message ) {
	struct element *next = elem->next;
	struct global_variable *literal, *global;
	char *value = elem->str.string;
	int local;
	struct variable var;
	struct structure *struc;
	struct function *decl;
	struct expression *expr = calloc( 1, sizeof( struct expression ) );
	if( expr ) {
		expr->line = elem->line;
		expr->function = func;
		if( ( value[ 0 ] >= '0' && value[ 0 ] <= '9' )
			|| ( value[ 0 ] == '-' && ( value[ 1 ] >= '0' && value[ 1 ] <= '9' ) ) ) {
			/* Integer literal. */
			next = parse_constant( elem, &var, message );
			expr->index = var.integer_value;
			expr->evaluate = evaluate_integer_constant_expression;
		} else if( value[ 0 ] == '"' || ( value[ 0 ] == '$' && value[ 1 ] == 0 ) ) {
			/* String or element literal. */
			literal = new_global_variable( NULL, message );
			if( literal ) {
				literal->next = func->literals;
				func->literals = literal;
				expr->global = literal;
				expr->evaluate = evaluate_global;
				next = parse_constant( elem, &literal->value, message );
			}
		} else if( value[ 0 ] == '\'' ) {
			/* Infix operator.*/
			next = parse_infix_expression( elem, env, func, expr, message );
		} else if( value[ 0 ] == '[' ) {
			/* Array index operator. */
			next = parse_index_expression( elem, env, func, expr, message );
		} else if( value[ 0 ] == '@' ) {
			/* Function reference operator. */
			next = parse_func_ref_expression( elem, env, func, expr, message );
		} else {
			local = get_string_list_index( func->variable_decls, value );
			if( local >= 0 ) {
				/* Local variable reference.*/
				expr->index = local;
				expr->evaluate = evaluate_local;
			} else {
				global = get_global_variable( env->constants, value );
				if( global == NULL ) {
					global = get_global_variable( env->globals, value );
				}
				if( global ) {
					/* Global variable reference.*/
					expr->global = global;
					expr->evaluate = evaluate_global;
				} else {
					decl = get_function( env, elem->str.string );
					if( decl ) {
						/* Function call.*/
						next = parse_call_expression( elem, env, func, decl, expr, message );
					} else {
						struc = get_structure( env->structures, elem->str.string );
						if( struc ) {
							/* Structure. */
							next = parse_struct_expression( elem, env, struc, expr, message );
						} else {
							/* Prefix Operator. */
							next = parse_operator_expression( elem, env, func, expr, message );
						}
					}
				}
			}
		}
		if( message[ 0 ] == 0 && next && next->str.string[ 0 ] == '(' ) {
			sprintf( message, "Unexpected '(' after expression on line %d.", next->line );
		}
		if( message[ 0 ] == 0 ) {
			prev->next = expr;
		} else {
			dispose_expressions( expr );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_expressions( struct element *elem, struct environment *env,
	struct function *func, char terminator, struct expression *prev, int *num_exprs, char *message ) {
	int line, count = 0;
	while( elem && elem->str.string[ 0 ] != terminator && message[ 0 ] == 0 ) {
		elem = parse_expression( elem, env, func, prev, message );
		prev = prev->next;
		count++;
		while( elem && message[ 0 ] == 0 && elem->str.string[ 0 ] == ',' ) {
			line = elem->line;
			elem = elem->next;
			if( elem && elem->str.string[ 0 ] != ',' ) {
				elem = parse_expression( elem, env, func, prev, message );
				prev = prev->next;
				count++;
			} else {
				sprintf( message, "Expected expression after ',' on line %d.", line );
			}
		}
	}
	if( num_exprs ) {
		*num_exprs = count;
	}
	return elem;
}

static struct element* parse_increment_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	int local;
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		local = get_string_list_index( func->variable_decls, next->str.string );
		if( local >= 0 ) {
			stmt->source = calloc( 1, sizeof( struct expression ) );
			if( stmt->source ) {
				stmt->local = local;
				stmt->source->line = next->line;
				stmt->source->function = func;
				stmt->execute = execute_increment_statement;
				next = next->next->next;
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		} else {
			sprintf( message, "Undeclared local variable '%.64s' on line %d.", next->str.string, next->line );
		}
	}
	return next;
}

static struct element* parse_decrement_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	int local;
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		local = get_string_list_index( func->variable_decls, next->str.string );
		if( local >= 0 ) {
			stmt->source = calloc( 1, sizeof( struct expression ) );
			if( stmt->source ) {
				stmt->local = local;
				stmt->source->line = next->line;
				stmt->source->function = func;
				stmt->execute = execute_decrement_statement;
				next = next->next->next;
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		} else {
			sprintf( message, "Undeclared local variable '%.64s' on line %d.", next->str.string, next->line );
		}
	}
	return next;
}

static struct element* parse_save_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		expr.next = NULL;
		next = parse_expression( next, env, func, &expr, message );
		if( expr.next ) {
			stmt->source = expr.next;
			if( next->str.string[ 0 ] == ',' ) {
				next = next->next;
			}
			expr.next = NULL;
			next = parse_expression( next, env, func, &expr, message );
			if( expr.next ) {
				stmt->destination = expr.next;
				stmt->execute = execute_save_statement;
				next = next->next;
			}
		}
	}
	return next;
}

static struct element* parse_append_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	struct element *next = parse_save_statement( elem, env, func, prev, message );
	if( prev->next && message[ 0 ] == 0 ) {
		prev->next->local = 1;
	}
	return next;
}

static struct element* parse_assignment_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	int local;
	struct expression expr;
	struct global_variable *global = NULL;
	struct element *next = elem->next, *child = next->child;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		if( child ) {
			expr.next = NULL;
			child = parse_expression( child, env, func, &expr, message );
			if( expr.next ) {
				stmt->destination = expr.next;
				if( child->str.string[ 0 ] == ',' ) {
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
				stmt->execute = execute_array_assignment;
			}
		} else {
			local = get_string_list_index( func->variable_decls, next->str.string );
			if( local < 0 ) {
				global = get_global_variable( env->globals, next->str.string );
			}
			if( local >= 0 || global ) {
				expr.next = NULL;
				next = parse_expression( next->next->next, env, func, &expr, message );
				if( expr.next ) {
					stmt->source = expr.next;
					if( global ) {
						stmt->global = &global->value;
						stmt->execute = execute_global_assignment;
					} else {
						stmt->local = local;
						stmt->execute = execute_local_assignment;
					}
					next = next->next;
				}
			} else {
				sprintf( message, "Undeclared variable '%.64s' on line %d.", next->str.string, next->line );
			}
		}
	}
	return next;
}

/* Parse a statement that expects one or more expressions after the keyword. */
struct element* parse_expr_list_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev,
	enum result ( *execute )( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ), char *message ) {
	struct expression head, *expr;
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		head.next = NULL;
		next = parse_expression( next, env, func, &head, message );
		expr = stmt->source = head.next;
		while( expr && next->str.string[ 0 ] != ';' ) {
			if( next->str.string[ 0 ] == ',' ) {
				next = next->next;
			}
			next = parse_expression( next, env, func, expr, message );
			expr = expr->next;
		}
		if( expr ) {
			stmt->execute = execute;
			next = next->next;
		}
	}
	return next;
}

static struct element* parse_print_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_print_statement, message );
}

static struct element* parse_write_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_write_statement, message );
}

static struct element* parse_error_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_error_statement, message );
}

static struct element* parse_return_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_return_statement, message );
}

static struct element* parse_throw_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_throw_statement, message );
}

static struct element* parse_exit_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_exit_statement, message );
}

static struct element* parse_break_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		stmt->execute = execute_break_statement;
		prev->next = stmt;
		next = next->next;
	}
	return next;
}

static struct element* parse_continue_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		stmt->execute = execute_continue_statement;
		prev->next = stmt;
		next = next->next;
	}
	return next;
}

static struct element* parse_call_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, execute_call_statement, message );
}

static struct element* parse_dim_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next, *child = next->child;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		expr.next = NULL;
		child = parse_expression( child, env, func, &expr, message );
		if( expr.next ) {
			stmt->destination = expr.next;
			if( child->str.string[ 0 ] == ',' ) {
				child = child->next;
			}
			expr.next = NULL;
			child = parse_expression( child, env, func, &expr, message );
			if( expr.next ) {
				stmt->source = expr.next;
				next = next->next->next;
			}
			stmt->execute = execute_dim_statement;
		}
	}
	return next;
}

static struct keyword switch_stmts[] = {
	{ "rem", "{", parse_comment, &switch_stmts[ 1 ] },
	{ "case", "X{", parse_case_statement, &switch_stmts[ 2 ] },
	{ "default", "{", parse_default_statement, NULL }
};

static struct element* parse_switch_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
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
					if( block.next->source ) {
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
				stmt->execute = execute_switch_statement;
			}
		}
	}
	return next;
}

static struct keyword statements[] = {
	{ "rem", "{", parse_comment, &statements[ 1 ] },
	{ "var", "V;", parse_local_declaration, &statements[ 2 ] },
	{ "let", "d=x;", parse_assignment_statement, &statements[ 3 ] },
	{ "print", "x;", parse_print_statement, &statements[ 4 ] },
	{ "write", "x;", parse_write_statement, &statements[ 5 ] },
	{ "error", "x;", parse_error_statement, &statements[ 6 ] },
	{ "throw", "x;", parse_throw_statement, &statements[ 7 ] },
	{ "return", "x;", parse_return_statement, &statements[ 8 ] },
	{ "exit", "x;", parse_exit_statement, &statements[ 9 ] },
	{ "break", ";", parse_break_statement, &statements[ 10 ] },
	{ "continue", ";", parse_continue_statement, &statements[ 11 ] },
	{ "if", "x{", parse_if_statement, &statements[ 12 ] },
	{ "while", "x{", parse_while_statement, &statements[ 13 ] },
	{ "call", "x;", parse_call_statement, &statements[ 14 ] },
	{ "try", "{cn{", parse_try_statement, &statements[ 15 ] },
	{ "dim", "[;", parse_dim_statement, &statements[ 16 ] },
	{ "set", "d=x;", parse_assignment_statement, &statements[ 17 ] },
	{ "switch", "x{", parse_switch_statement, &statements[ 18 ] },
	{ "inc", "n;", parse_increment_statement, &statements[ 19 ] },
	{ "dec", "n;", parse_decrement_statement, &statements[ 20 ] },
	{ "save", "xx;", parse_save_statement, &statements[ 21 ] },
	{ "append", "xx;", parse_append_statement, NULL }
};

static struct element* parse_case_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement block, *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		next = parse_expressions( next, env, func, '{', &expr, NULL, message );
		if( message[ 0 ] == 0 ) {
			stmt->source = expr.next;
			block.next = NULL;
			parse_keywords( env->statements, next->child, env, func, &block, message );
			stmt->if_block = block.next;
			if( message[ 0 ] == 0 ) {
				stmt->execute = execute_case_statement;
				next = next->next;
			}
		}
	}
	return next;
}

static struct element* parse_default_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement block, *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		block.next = NULL;
		parse_keywords( env->statements, next->child, env, func, &block, message );
		stmt->if_block = block.next;
		if( message[ 0 ] == 0 ) {
			stmt->execute = execute_case_statement;
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

static struct element* validate_syntax( char *syntax, struct element *elem,
	struct element *key, struct environment *env, char *message ) {
	int idx = 1, chr = syntax[ 0 ], line = key->line;
	while( chr && message[ 0 ] == 0 ) {
		if( elem ) {
			line = elem->line;
		}
		if( chr == '0' ) {
			/* List end. */
			if( elem ) {
				sprintf( message, "Unexpected '%.64s' after '%.64s' on line %d.", elem->str.string, key->str.string, line );
			}
		} else if( strchr( "\",;={", chr ) ) {
			/* Strings, separators or blocks. */
			if( elem == NULL || elem->str.string[ 0 ] != chr ) {
				sprintf( message, "Expected '%c' after '%.64s' on line %d.", chr, key->str.string, line );
			} else {
				elem = elem->next;
			}
		} else if( chr == '(' ) {
			/* Bracketed name list. */
			if( elem && elem->str.string[ 0 ] == '(' ) {
				if( elem->child ) {
					validate_syntax( "N0", elem->child, elem, env, message );
				}
				elem = elem->next;
			} else {
				sprintf( message, "Expected '(' after '%.64s' on line %d.", key->str.string, line );
			}
		} else if( chr == '[' ) {
			/* Index expression. */
			if( elem && elem->str.string[ 0 ] == '[' ) {
				if( elem->child ) {
					validate_syntax( "xx0", elem->child, key, env, message );
					elem = elem->next;
				} else {
					sprintf( message, "Expected '[' after '%.64s' on line %d.", key->str.string, line );
				}
			} else {
				sprintf( message, "Expected '[' after '%.64s' on line %d.", key->str.string, line );
			}
		} else if( chr == 'c' ) {
			/* Catch */
			if( elem == NULL || strcmp( elem->str.string, "catch" ) ) {
				sprintf( message, "Expected 'catch' after '%.64s' on line %d.", key->str.string, line );
			} else {
				elem = elem->next;
			}
		} else if( chr == 'n' ) {
			/* Name. */
			if( elem == NULL || elem->str.string[ 0 ] == ';' ) {
				sprintf( message, "Expected name after '%.64s' on line %d.", key->str.string, line );
			} else if( !validate_name( elem->str.string, env ) ) {
				sprintf( message, "Invalid name '%.64s' on line %d.", elem->str.string, line );
			} else {
				elem = elem->next;
			}
		} else if( chr == 'N' ) {
			/* Name list, terminated by ';' or NULL. */
			elem = validate_syntax( "n", elem, key, env, message );
			while( message[ 0 ] == 0 && elem && elem->str.string[ 0 ] != ';' ) {
				if( elem->str.string[ 0 ] == ',' ) {
					elem = elem->next;
				}
				elem = validate_syntax( "n", elem, key, env, message );
			}
		} else if( chr == 'x' ) {
			/* Expression. */
			if( elem && strchr( ",;({", elem->str.string[ 0 ] ) == NULL ) {
				if( elem->str.string[ 0 ] == '[' ) {
					validate_syntax( "[", elem, key, env, message );
				} else if( elem->str.string[ 0 ] == '$' && elem->str.string[ 1 ] == 0 ) {
					elem = elem->next;
					if( elem == NULL || elem->str.string[ 0 ] != '{' ) {
						sprintf( message, "Expected '{' after '$' on line %d.", line );
					}
				} else if( elem->next && elem->next->str.string[ 0 ] == '(' ) {
					elem = elem->next;
				}
				if( elem->next && elem->next->str.string[ 0 ] == ',' && syntax[ idx ] == 'x' ) {
					elem = elem->next;
				}
				if( elem ) {
					elem = elem->next;
				}
			} else {
				sprintf( message, "Expected expression after '%.64s' on line %d.", key->str.string, line );
			}
		} else if( chr == 'X' ) {
			/* Expression list, terminated by '{' or NULL. */
			elem = validate_syntax( "x", elem, key, env, message );
			while( message[ 0 ] == 0 && elem && elem->str.string[ 0 ] != '{' ) {
				if( elem->str.string[ 0 ] == ',' ) {
					elem = elem->next;
				}
				elem = validate_syntax( "x", elem, key, env, message );
			}
		} else if( chr == 'v' ) {
			/* Variable declaration. */
			if( elem && elem->str.string[ 0 ] == '[' ) {
				validate_syntax( "nx", elem->child, elem, env, message );
				if( message[ 0 ] == 0 ) {
					elem = elem->next;
				}
			} else {
				elem = validate_syntax( "n", elem, key, env, message );
				if( message[ 0 ] == 0 && elem && elem->str.string[ 0 ] == '=' ) {
					elem = validate_syntax( "x", elem->next, key, env, message );
				}
			}
		} else if( chr == 'V' ) {
			/* Variable declaration list, terminated by ';' or NULL. */
			elem = validate_syntax( "v", elem, key, env, message );
			while( message[ 0 ] == 0 && elem && elem->str.string[ 0 ] != ';' ) {
				if( elem->str.string[ 0 ] == ',' ) {
					elem = elem->next;
				}
				elem = validate_syntax( "v", elem, key, env, message );
			}
		} else if( chr == 'd' ) {
			/* Assignment destination. */
			if( elem && elem->str.string[ 0 ] == '[' ) {
				elem = validate_syntax( "[", elem, key, env, message );
			} else {
				elem = validate_syntax( "n", elem, key, env, message );
			}
		} else {
			/* Internal error. */
			sprintf( message, "Internal error. Unknown specifier '%c' in syntax for '%s'.", chr, key->str.string );
		}
		chr = syntax[ idx++ ];
	}
	return elem;
}

static void parse_keywords( struct keyword *keywords, struct element *elem,
	struct environment *env, struct function *func,
	struct statement *stmt, char *message ) {
	struct keyword *key;
	while( elem && message[ 0 ] == 0 ) {
		key = keywords;
		while( key && strcmp( key->name, elem->str.string ) ) {
			key = key->next;
		}
		if( key ) {
			validate_syntax( key->syntax, elem->next, elem, env, message );
			if( message[ 0 ] == 0 ) { 
				elem = key->parse( elem, env, func, stmt, message );
				while( stmt && stmt->next ) {
					stmt = stmt->next;
				}
			}
		} else {
			sprintf( message, "Unrecognized keyword '%.64s' on line %d.", elem->str.string, elem->line );
		}
	}
}

static struct element* parse_if_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement block, *stmt = new_statement( message );
	if( stmt ) {
		prev->next = stmt;
		expr.next = NULL;
		next = parse_expression( next, env, func, &expr, message );
		if( expr.next ) {
			stmt->source = expr.next;
			stmt->execute = execute_if_statement;
			if( next->child ) {
				block.next = NULL;
				parse_keywords( env->statements, next->child, env, func, &block, message );
				stmt->if_block = block.next;
			}
			if( message[ 0 ] == 0 ) {
				next = next->next;
				if( next && strcmp( next->str.string, "else" ) == 0 ) {
					if( next->next && next->next->str.string[ 0 ] == '{' ) {
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
	struct function *func, struct statement *prev, char *message ) {
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
			stmt->execute = execute_while_statement;
		}
	}
	return next;
}

static struct element* parse_try_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
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
			stmt->local = get_string_list_index( func->variable_decls, next->str.string );
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
				stmt->execute = execute_try_statement;
			} else {
				sprintf( message, "Undeclared local variable '%.64s' on line %d.", next->str.string, next->line );
			}
		}
		prev->next = stmt;
	}
	return next;
}

static int add_function_parameter( struct function *func, struct element *elem,
	struct environment *env, char *message ) {
	char *name = elem->str.string;
	struct string_list *param;
	if( validate_decl( elem, env, message ) ) {
		param = new_string_list( name );
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
				sprintf( message, "Parameter '%.64s' already defined on line %d.", name, elem->line );
			}
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	}
	return message[ 0 ] == 0;
}

static struct function* parse_function( struct element *elem, char *name, char *file,
	struct environment *env, char *message ) {
	struct element *child;
	struct function *func = new_function( name, file, message );
	if( func ) {
		func->line = elem->line;
		func->body = elem->next;
		func->env = env;
		child = elem->child;
		while( child && message[ 0 ] == 0 ) {
			if( add_function_parameter( func, child, env, message ) ) {
				child = child->next;
				if( child && child->str.string[ 0 ] == ',' ) {
					child = child->next;
				}
			}
		}
		if( message[ 0 ] ) {
			unref_string( &func->str );
			func = NULL;
		}
	}
	return func;
}

static int parse_function_body( struct function *func, struct environment *env, char *message ) {
	struct statement stmt;
	struct element *next = func->body->child;
	if( next ) {
		stmt.next = NULL;
		parse_keywords( env->statements, next, env, func, &stmt, message );
		func->statements = stmt.next;
	}
	return message[ 0 ] == 0;
}

static enum result evaluate_function_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct function *func;
	struct element *elem, key = { { 1, ELEMENT, "$function", 9 } };
	char message[ 128 ] = "";
	enum result ret = parameter->evaluate( parameter, variables, &var, exception );
	if( ret ) {
		if( var.string_value && var.string_value->type == ELEMENT ) {
			elem = ( struct element * ) var.string_value;
			key.line = this->line;
			validate_syntax( "({0", elem, &key, this->function->env, message );
			if( message[ 0 ] == 0 ) {
				func = parse_function( elem, "function", this->function->file.string, this->function->env, message );
				if( func ) {
					if( parse_function_body( func, this->function->env, message ) ) {
						dispose_variable( result );
						result->integer_value = 0;
						result->string_value = &func->str;
					} else {
						unref_string( &func->str );
						ret = throw( exception, this, 0, message );
					}
				} else {
					ret = throw( exception, this, 0, message );
				}
			} else {
				ret = throw( exception, this, 0, message );
			}
		} else {
			ret = throw( exception, this, 0, "Not an element." );
		}
		dispose_variable( &var );
	}
	return ret;
}

static struct element* parse_function_declaration( struct element *elem, struct environment *env,
	struct function *decl, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	char *name;
	if( validate_decl( next, env, message ) ) {
		name = next->str.string;
		next = next->next;
		decl = parse_function( next, name, decl->file.string, env, message );
		if( decl ) {
			decl->next = env->functions;
			env->functions = decl;
			next = next->next->next;
		}
	}
	return next;
}

static struct element* parse_program_declaration( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	if( validate_decl( next, env, message ) ) {
		func = new_function( next->str.string, func->file.string, message );
		if( func ) {
			func->next = env->entry_points;
			env->entry_points = func;
			func->line = elem->line;
			func->body = next->next;
			func->env = env;
			next = next->next->next;
		}
	}
	return next;
}

static struct element* parse_include( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	int path_len = chop( func->file.string, "/:\\" );
	int name_len = unquote_string( next->str.string, NULL );
	char *path = malloc( path_len + name_len + 1 );
	if( path ) {
		memcpy( path, func->file.string, path_len );
		unquote_string( next->str.string, &path[ path_len ] );
		path[ path_len + name_len ] = 0;
		if( parse_tt_file( path, env, message ) ) {
			next = next->next->next;
		}
		free( path );
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static int add_structure_field( struct environment *env, char *name, int line, char *message ) {
	struct string_list *field;
	struct structure *struc = env->structures;
	if( validate_name( name, env ) ) {
		if( struc->fields && get_string_list_index( struc->fields, name ) >= 0 ) {
			sprintf( message, "Field '%.64s' already defined on line %d.", name, line );
		} else {
			field = new_string_list( name );
			if( field ) {
				if( struc->fields ) {
					struc->fields_tail->next = field;
				} else {
					struc->fields = field;
				}
				struc->fields_tail = field;
				struc->length++;
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		}
	} else {
		sprintf( message, "Invalid name '%.64s' on line %d.", name, line );
	}
	return message[ 0 ] == 0;
}

static struct element* parse_struct_declaration( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message ) {
	char *name;
	struct structure *struc;
	struct string_list *field;
	struct element *child, *next = elem->next;
	if( validate_decl( next, env, message ) ) {
		name = new_string( next->str.string );
		struc = calloc( 1, sizeof( struct structure ) );
		if( name && struc ) {
			struc->next = env->structures;
			env->structures = struc;
			struc->name = name;
			next = next->next;
			if( next && next->str.string[ 0 ] == '(' ) {
				child = next->child;
				if( child && child->next == NULL ) {
					struc = get_structure( env->structures->next, child->str.string );
					if( struc ) {
						field = struc->fields;
						while( field && message[ 0 ] == 0 ) {
							if( add_structure_field( env, field->value, child->line, message ) ) {
								field = field->next;
							}
						}
						next = next->next;
					} else {
						sprintf( message, "Structure '%.64s' not declared on line %d.", child->str.string, child->line );
					}
				} else {
					sprintf( message, "Invalid parent structure declaration on line %d.", next->line );
				}
			}
			if( message[ 0 ] == 0 ) {
				if( next && next->str.string[ 0 ] == '{' ) {
					child = next->child;
					while( child && message[ 0 ] == 0 ) {
						if( add_structure_field( env, child->str.string, child->line, message ) ) {
							child = child->next;
							if( child && ( child->str.string[ 0 ] == ',' || child->str.string[ 0 ] == ';' ) ) {
								child = child->next;
							}
						}
					}
					next = next->next;
					if( next && next->str.string[ 0 ] == ';' ) {
						next = next->next;
					} else {
						sprintf( message, "Expected ';' after 'struct' on line %d.", elem->line );
					}
				} else {
					sprintf( message, "Expected '{' after 'struct' on line %d.", elem->line );
				}
			}
		} else {
			free( name );
			free( struc );
			strcpy( message, OUT_OF_MEMORY );
		}
	}
	return next;
}

static struct operator operators[] = {
	{ ":", ':',-1, evaluate_refcall_expression, &operators[ 1 ] },
	{ "%", '%', 2, evaluate_arithmetic_expression, &operators[ 2 ] },
	{ "&", '&', 2, evaluate_arithmetic_expression, &operators[ 3 ] },
	{ "*", '*', 2, evaluate_arithmetic_expression, &operators[ 4 ] },
	{ "+", '+', 2, evaluate_arithmetic_expression, &operators[ 5 ] },
	{ "-", '-', 2, evaluate_arithmetic_expression, &operators[ 6 ] },
	{ "/", '/', 2, evaluate_arithmetic_expression, &operators[ 7 ] },
	{ "<", '<', 2, evaluate_arithmetic_expression, &operators[ 8 ] },
	{ "<e",'L', 2, evaluate_arithmetic_expression, &operators[ 9 ] },
	{ ">", '>', 2, evaluate_arithmetic_expression, &operators[ 10 ] },
	{ ">e",'G', 2, evaluate_arithmetic_expression, &operators[ 11 ] },
	{ "<<",'A', 2, evaluate_arithmetic_expression, &operators[ 12 ] },
	{ ">>",'B', 2, evaluate_arithmetic_expression, &operators[ 13 ] },
	{ "^", '^', 2, evaluate_arithmetic_expression, &operators[ 14 ] },
	{ "=", '=', 2, evaluate_arithmetic_expression, &operators[ 15 ] },
	{ "|", '|', 2, evaluate_arithmetic_expression, &operators[ 16 ] },
	{ "~", '~', 1, evaluate_bitwise_not_expression, &operators[ 17 ] },
	{ "!", '!', 1, evaluate_logical_expression, &operators[ 18 ] },
	{ "&&",'&', 2, evaluate_logical_expression, &operators[ 19 ] },
	{ "||",'|', 2, evaluate_logical_expression, &operators[ 20 ] },
	{ "$eq", '$', 2, evaluate_eq_expression, &operators[ 21 ] },
	{ "$str", '$',-1, evaluate_str_expression, &operators[ 22 ] },
	{ "$cmp", '$', 2, evaluate_cmp_expression, &operators[ 23 ] },
	{ "$cat", '$',-1, evaluate_str_expression, &operators[ 24 ] },
	{ "$chr", '$', 2, evaluate_chr_expression, &operators[ 25 ] },
	{ "$sub", '$', 3, evaluate_sub_expression, &operators[ 26 ] },
	{ "$asc", '$', 1, evaluate_asc_expression, &operators[ 27 ] },
	{ "$hex", '$', 1, evaluate_hex_expression, &operators[ 28 ] },
	{ "$int", '$', 1, evaluate_int_expression, &operators[ 29 ] },
	{ "$len", '$', 1, evaluate_len_expression, &operators[ 30 ] },
	{ "$tup", '$', 2, evaluate_tup_expression, &operators[ 31 ] },
	{ "$array", '$', 1, evaluate_array_expression, &operators[ 32 ] },
	{ "$new", '$', 1, evaluate_array_expression, &operators[ 33 ] },
	{ "$load", '$', 1, evaluate_load_expression, &operators[ 34 ] },
	{ "$flen", '$', 1, evaluate_flen_expression, &operators[ 35 ] },
	{ "$chop", '$', 2, evaluate_chop_expression, &operators[ 36 ] },
	{ "$argc", '$', 0, evaluate_argc_expression, &operators[ 37 ] },
	{ "$argv", '$', 1, evaluate_argv_expression, &operators[ 38 ] },
	{ "$time", '$', 0, evaluate_time_expression, &operators[ 39 ] },
	{ "$parse", '$', 1, evaluate_parse_expression, &operators[ 40 ] },
	{ "$unparse", '$', 1, evaluate_unparse_expression, &operators[ 41 ] },
	{ "$next", '$', 1, evaluate_next_expression, &operators[ 42 ] },
	{ "$child", '$', 1, evaluate_child_expression, &operators[ 43 ] },
	{ "$line", '$', 1, evaluate_line_expression, &operators[ 44 ] },
	{ "$elem", '$', 3, evaluate_elem_expression, &operators[ 45 ] },
	{ "$values", '$', 1, evaluate_values_expression, &operators[ 46 ] },
	{ "$pack", '$', 1, evaluate_pack_expression, &operators[ 47 ] },
	{ "$unpack", '$', 2, evaluate_unpack_expression, &operators[ 48 ] },
	{ "$quote", '$', 1, evaluate_quote_expression, &operators[ 49 ] },
	{ "$unquote", '$', 1, evaluate_unquote_expression, &operators[ 50 ] },
	{ "$interrupted", '$', 0, evaluate_interrupted_expression, &operators[ 51 ] },
	{ "$function", '$', 1, evaluate_function_expression, &operators[ 52 ] },
	{ "$src", '$', 0, evaluate_source_expression, NULL }
};

static struct keyword declarations[] = {
	{ "rem", "{", parse_comment, &declarations[ 1 ] },
	{ "include", "\";", parse_include, &declarations[ 2 ] },
	{ "function", "n({", parse_function_declaration, &declarations[ 3 ] },
	{ "program", "n{", parse_program_declaration, &declarations[ 4 ] },
	{ "global", "V;", parse_global_declaration, &declarations[ 5 ] },
	{ "array", "V;", parse_array_declaration, &declarations[ 6 ] },
	{ "const", "V;", parse_const_declaration, &declarations[ 7 ] },
	{ "struct", "n", parse_struct_declaration, NULL }
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
	char *name = elem->str.string;
	if( get_global_variable( env->globals, name )
	|| get_global_variable( env->constants, name )
	|| get_function( env, name )
	|| get_structure( env->structures, name ) ) {
		sprintf( message, "Name '%.64s' already defined on line %d.", elem->str.string, elem->line );
		return 0;
	}
	return 1;
}

/* Parse the specified program text into env. Returns zero and writes message on failure. */
int parse_tt_program( char *program, char *file_name, struct environment *env, char *message ) {
	struct element *elem;
	struct function *empty, *func;
	elem = parse_element( program, message );
	if( elem ) {
		/* Create empty function for global evaluation. */
		empty = new_function( "", file_name, message );
		if( empty ) {
			empty->next = env->functions;
			env->functions = empty;
			empty->env = env;
			/* Populate execution environment. */
			parse_keywords( declarations, elem, env, empty, NULL, message );
			/* Parse function bodies. */
			func = env->functions;
			while( func && message[ 0 ] == 0 ) {
				if( func->body ) {
					parse_function_body( func, env, message );
					func->body = NULL;
				}
				func = func->next;
			}
			func = env->entry_points;
			while( func && message[ 0 ] == 0 ) {
				if( func->body ) {
					parse_function_body( func, env, message );
					func->body = NULL;
				}
				func = func->next;
			}
		}
		unref_string( &elem->str );
	}
	return message[ 0 ] == 0;
}

/* Parse the specified program file into env.
   Returns zero and writes up to 256 bytes to message on failure. */
int parse_tt_file( char *file_name, struct environment *env, char *message ) {
	long file_length, success = 0;
	char *program_buffer, error[ 128 ] = "";
	/* Load program file into string.*/
	file_length = load_file( file_name, NULL, message );
	if( file_length >= 0 ) {
		if( file_length < MAX_INTEGER ) {
			/* printf( "Parsing '%s'. Length %ld\n", file_name, file_length ); */
			program_buffer = malloc( file_length + 1 );
			if( program_buffer ) {
				file_length = load_file( file_name, program_buffer, message );
				if( file_length >= 0 ) {
					program_buffer[ file_length ] = 0;
					/* Parse program structure.*/
					success = parse_tt_program( program_buffer, file_name, env, message );
				}
				free( program_buffer );
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		} else {
			strcpy( message, "File too large." );
		}
	}
	if( !success && strncmp( message, "Unable to parse", 15 ) ) {
		strncpy( error, message, 127 );
		error[ 127 ] = 0;
		if( sprintf( message, "Unable to parse '%.96s'.\n%s", file_name, error ) < 0 ) {
			strcpy( message, "Unable to parse. " );
			strcat( message, error );
		}
	}
	return success;
}

/* Evaluate the global-variable initialization expressions for env.
   Returns zero and assigns exception on failure. */
int initialize_globals( struct environment *env, struct variable *exception ) {
	struct expression *init;
	struct global_variable *global = env->constants;
	while( global ) {
		init = global->initializer;
		if( init && init->evaluate( init, NULL, &global->value, exception ) == EXCEPTION ) {
			return 0;
		}
		global = global->next;
	}
	global = env->globals;
	while( global ) {
		init = global->initializer;
		if( init && init->evaluate( init, NULL, &global->value, exception ) == EXCEPTION ) {
			return 0;
		}
		global = global->next;
	}
	return 1;
}

/* Add a copy of the specified statement list to env. Returns zero and writes message on failure. */
int add_statements( struct keyword *statements, struct environment *env, char *message ) {
	struct keyword *statement;
	while( statements ) {
		statement = calloc( 1, sizeof( struct keyword ) );
		if( statement ) {
			memcpy( statement, statements, sizeof( struct keyword ) );
			statement->next = env->statements;
			env->statements = statement;
		} else {
			strcpy( message, OUT_OF_MEMORY );
			return 0;
		}
		statements = statements->next;
	}
	return 1;
}

/* Add a copy of the specified operator list to env. Returns zero and writes message on failure. */
int add_operators( struct operator *operators, struct environment *env, char *message ) {
	struct operator *operator;
	while( operators ) {
		operator = calloc( 1, sizeof( struct operator ) );
		if( operator ) {
			memcpy( operator, operators, sizeof( struct operator ) );
			operator->next = env->operators;
			env->operators = operator;
		} else {
			strcpy( message, OUT_OF_MEMORY );
			return 0;
		}
		operators = operators->next;
	}
	return 1;
}

/* Initialize env with the the standard statements, operators and constants.
   Returns zero and writes message on failure. */
int initialize_environment( struct environment *env, char *message ) {
	return add_statements( statements, env, message )
		&& add_operators( operators, env, message )
		&& add_constants( constants, env, message );
}

/* Initialize expr to execute the specified function when evaluated. */
void initialize_call_expr( struct expression *expr, struct function *func ) {
	memset( expr, 0, sizeof( struct expression ) );
	expr->evaluate = evaluate_call_expression;
	expr->line = func->line;
	expr->function = func;
}

