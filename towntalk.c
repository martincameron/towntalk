
#if defined( __GNUC__ )
#define alloca( size ) __builtin_alloca( size )
#else
#include "alloca.h"
#endif

#if defined( FLOATING_POINT )
#include "math.h"
#endif

#include "errno.h"
#include "stddef.h"
#include "stdio.h"
#include "string.h"
#include "time.h"

#include "towntalk.h"

/*
	Towntalk (c)2025 Martin Cameron.

	A program file consists of a list of declarations.
	When an unquoted '#' or '//' sequence is encountered, the rest of the line is ignored.
	Variable and function names must be alphanumeric.
	Commas within name and argument lists are optional.
	A value may be a number, a reference, or a tuple.
	References may be strings, elements, arrays, structs, functions or custom types.
	A tuple is a reference with an associated number.
	Strings are immutable and can be used as byte arrays.
	String literals may include the escape sequences "\"", "\\", and octal "\nnn".
	Buffers are arrays that cannot hold references but use much less memory.
	Keywords and dollar-expressions may be expressed with a capital, eg. 'Print Time;'.
	Elements are immutable trees of strings with next and child references.
	A valid program file may be parsed into an element tree.
	Elements are separated by whitespace, commas, ';' or '='.
	Elements may contain spaces and separators by enclosing them in quotes.
	Child elements are enclosed in parentheses, square brackets or braces.
	If the program has been interrupted, while loops will throw an exception.
	An '!' may be used in place of '.' to specify a declaration when a local variable has the same name.

	Example:
		rem { Test }
		function add( a b ) {
			# Sum two numbers.
			return +( a b );
		}
		program main {
			var a;
			let a = 0;
			while <( a 10 ) {
				print a;
				let a = add( a 1 );
			}
		}

	Declarations:
		rem {...}                Comment (all brackets inside must be balanced).
		include "file.tt";       Include declarations from specified file.
		library name { decls }   Namespace prefix for specified declarations.
		import name from "f.tt"; Include declarations from file if library not already declared.
		const name = expr;       Global constants.
		global a, b = expr;      Global variables.
		global ( struct ) a;     Global variable with associated struct.
		global [ a expr ];       Global variable initialized with array of specified length.
		struct s { a,b,c }       Enumeration or layout for typed arrays.
		struct t( s ) { d,e,f }  Struct with fields inherited from s.
		function f(param){stmts} Function declaration.
		program name{statements} Entry point function (no arguments).

	Statements:
		rem {...}                Comment.
		var a, b, c = expr;      Local variables.
		var ( struct ) a;        Local variable with associated struct.
		var [ a expr ];          Local variable initialized with array of specified length.
		let var = expr;          Assign expression to local or global variable.
		let var! = expr;         Assign expression to global variable.
		let [ arr idx ] = expr;  Assign expression to array at specified index.
		let struc.f(arr) = expr; Assign expression to array at named field of specified struct.
		let var.field = expr;    Assign expr to local or global array variable at named field of associated struct.
		print expr;              Write number or string to standard output.
		write expr;              Same as print, but do not add a newline.
		error expr;              Same as print, but write to standard error.
		return expr;             Return from the current function.
		throw expr;              Return to the nearest catch statement.
		exit expr;               Terminate program with specified exit code.
		while expr {statements}  Execute statements if expr is not null and repeat.
		until expr {statements}  Execute statements and repeat if expr is null.
		break;                   Exit current while statement.
		continue;                Stop current iteration of while statement.
		if expr {statements}     Execute statements if expr is not null.
		   else {statements}     Optional, execute if expr is null.
		   else if expr {stmts}  Equivalent to 'else { if expr { stmts } }'.
		switch expr {            Selection statement for numbers or strings.
		   case 1,2 {statements} Execute statements if expr equals 1 or 2.
		   case "a" {statements} Execute statements if expr equals "a".
		   default {statements}} Execute statements if no valid cases.
		try {statements}         Execute statements unless exception thrown.
		   catch e {statements}  Assign any thrown value to local variable and execute statements.
		   catch (strc) e {...}  As above, but only handle instances of the specified struct.
		   finally {statements}  Always execute even if exception thrown.
		call expr;               Evaluate expression and discard result.
		set [ arr idx ] = expr;  Variable/Array assignment (same as let).
		inc a;                   Increment local variable.
		dec a;                   Decrement local variable.
		save str, "file";        Save bytes from string to file.
		append str, "file";      Append bytes to the end of file.
		arraycopy a ax b yx n;   Copy n elements of array a from [ a ax ] to array b at [ b yx ].

	Expressions:
		-123                     Decimal number literal.
		0x100                    Hexadecimal number literal.
		0888                     Octal integer literal.
		'A' or \"A"              Integer character literal.
		"String"                 String literal.
		${0,"1",$tup("2",3)}     Element literal.
		variable or global!      Value of named local or global variable.
		local++                  Value of named local variable prior to increment.
		local--                  Value of named local variable prior to decrement.
		func(expr ...) or fn!()  Call declared function with specified args.
		[arr idx]                Array element.
		struct or struct!        Structure reference.
		struct.field             Index of struct field.
		struct.field(array)      Value of specified field of structured array expression.
		variable.field           Value of specified field of associated structure of local or global variable.
		@function                Reference to declared function.
		@(expr ...)              Recursive function call.
		:(func expr ...)         Call function reference with specified args.
		:struct.memb(this ...)   Call member-function. Equivalent to ":(struct.memb(this) this ...)", but this evaluated once.
		:variable.member(...)    Call member-function using associated structure. Equivalent to ":struct.member(variable ...)".
		variable:func(...)       Call static member-function using associated structure. Equivalent to "struct_func(variable ...)".
		global!:func(...)        Call static member-function using associated structure of global variable.
		`(expr operator ...)     Infix expression, eg `( 1 + 2 ). '(...) may also be used.
		+(num num ...)           Addition.
		-(num num ...)           Subtraction.
		*(num num ...)           Multiplication.
		/(int int...) or _/(...) Integer division.
		%(int int ...)           Integer modulo-division.
		$div(num num ...)        Floating-point division (if supported).
		<<(int int)              Integer arithmetic shift-left.
		>>(int int)              Integer arithmetic shift-right.
		=(num num)               Equality.
		<(num num)               Less than.
		<e(num num)              Less than or equal.
		>(num num)               Greater than.
		>e(num num)              Greater than or equal.
		&(int int ...)           Integer bitwise AND.
		|(int int ...)           Integer bitwise OR.
		^(int int ...)           Ingeger bitwise XOR.
		~(int)                   Integer bitwise NOT.
		!(expr)                  Evaluates to 1 if argument is null.
		&&(expr expr ...)        Evaluates to 1 if all arguments are non-null.
		||(expr expr ...)        Evaluates to 1 if any argument is non-null.
		?(expr expr expr)        Evaluates second expr if first is non-null, else third.
		$same(expr expr)         Evaluates to 1 if arguments have the same value.
		$cmp(str str)            String/Tuple comparison, evaluates to 0 if equivalent.
		$eq(expr expr)           Equivalent to !( $cmp( expr expr ) ).
		$str(expr ...)           Number to string and string concatenation.
		$cat(expr ...)           String concatenation (same as $str).
		$chr(str idx)            Character at idx as integer.
		$sub(str off len)        Substring (or byte array to string).
		$asc(int)                Character code to string.
		$hex(int)                Integer to fixed-length signed hex string.
		$int(str)                String or value to integer.
		$num(str)                String or value to number.
		$len(str/arr)            Length of string, array or structure.
		$tup(str num)            String/Number tuple.
		$array(len ...)          Create array of specified length and values.
		$array(${0,"a"})         Create array with values from element.
		$new(struct ...)         Create structured array, same as $array(struct ...).
		$read(len)               Read string from standard input.
		$load("abc.bin")         Load file into string.
		$load("file", off, len)  Load section of file into string.
		$flen("file")            Get the length of a file.
		$src(func)               Source file path of specified function reference.
		$stridx(str,"chars",idx) The index of a member of chars in str from idx.
		$endidx(str,"chars")     The index of the last member of chars in str.
		$argc                    Number of command-line arguments.
		$argv(idx)               Command-line argument as string.
		$time                    Current time as seconds/date tuple.
		$parse(str)              Parse string into element list.
		$unparse(elem)           Concatenate element list into string.
		$next(elem)              Get the next element in the list or null.
		$child(elem)             Get the first child element or null.
		$line(elem)              Get the line number of the element.
		$elem(elem child next)   Return a copy of elem with specified references.
		$expr(expr)              Return an element representing the specified value.
		$eval(elem)              Parse the specified element as an expression and evaluate.
		$pack(int/arr)           Encode integers as big-endian byte string.
		$unpack(str idx)         Decode the specified big-endian integer.
		$quote(str)              Encode byte string with quotes and escapes.
		$unquote(str)            Decode quoted-string into byte string.
		$interrupted             Check and clear program interrupt status.
		$function(${(){stmts}})  Compile a function reference from an element.
		$buffer(len ...)         Create a memory-efficient numerical array.
		$type(expr)              Return a value representing the type of the expression.
		$field(struct idx)       Name of specified struct field.
		$instanceof(arr struct)  Returns arr if an instance of struct, null otherwise.
		$trace(message)          Return a stack-trace array of function and line-number tuples.
		$log(num)                Floating-point natural-logarithm (if supported).
		$exp(num)                Floating-point natural-exponent (if supported).
		$sqrt(num)               Floating-point square-root (if supported).
		$sin(rad)                Floating-point sine (if supported).
		$cos(rad)                Floating-point cosine (if supported).
		$tan(rad)                Floating-point tangent (if supported).
		$asin(num)               Floating-point arc-sine (if supported).
		$acos(num)               Floating-point arc-cosine (if supported).
		$atan(num)               Floating-point arc-tangent (if supported).
*/

struct global_assignment_statement {
	struct statement stmt;
	struct variable *destination;
};

struct structure_statement {
	struct statement stmt;
	struct structure *structure;
};

/* The maximum integer value. */
const int MAX_INTEGER = ( 1 << ( sizeof( int ) * 8 - 1 ) ) - 1u;

/* Message to be used to avoid memory allocation in out-of-memory error paths. */
const char *OUT_OF_MEMORY = "Out of memory.";

static struct constant constants[] = {
	{  "TRUE", 1, NULL },
	{ "FALSE", 0, NULL },
	{  "NULL", 0, NULL },
	{ NULL }
};

/* Forward declarations. */
struct statement * optimize_statements( struct function *func, struct statement *prev, char *message );
static int validate_name( struct element *elem, char *message );
static int validate_local( struct string *decl, int line, struct environment *env, char *message );
static int validate_decl( struct string *decl, int line, struct environment *env, char *message );
static void parse_keywords( struct keyword *keywords, struct element *elem,
	struct function *func, struct variables *vars, struct statement *stmt, char *message );
struct statement* parse_keywords_indexed( struct keyword **index, struct element *elem,
	struct function *func, struct variables *vars, struct statement *stmt, char *message );
struct element* parse_expression( struct element *elem, struct function *func,
	struct variables *vars, struct expression *prev, char *message );
static struct element* parse_expressions( struct element *elem, struct function *func,
	struct variables *vars, char terminator, struct expression *prev, int *num_exprs, char *message );
static struct element* value_to_element( number number_value, struct string *string_value,
	struct environment *env, int max_depth, char *message );
static struct element* parse_infix_expression( struct element *elem,
	struct function *func, struct variables *vars, struct expression *prev, char *message );

/* Allocate and return a new element with the specified string length. */
struct element* new_element( int str_len ) {
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

/* Allocate and return a new array with the specified size, associated string length and reference count of 1. */
struct array* new_array( struct environment *env, int length, int str_len ) {
	size_t refs_size = length * sizeof( struct string * );
	size_t nums_size = length * sizeof( number );
	struct array *arr = calloc( 1, sizeof( struct array ) + refs_size + nums_size + str_len + 1 );
	if( arr ) {
		arr->string_values = ( struct string ** ) &arr[ 1 ];
		arr->number_values = ( number * ) &arr->string_values[ length ];
		arr->str.string = ( char * ) &arr->number_values[ length ];
		arr->str.length = str_len;
		arr->str.reference_count = 1;
		arr->str.type = ARRAY;
		arr->length = length;
		arr->prev = &env->arrays;
		arr->next = env->arrays.next;
		if( arr->next ) {
			arr->next->prev = arr;
		} 
		env->arrays.next = arr;
	}
	return arr;
}

static struct array* new_buffer( int length ) {
	struct array *arr = calloc( 1, sizeof( struct array ) + sizeof( number ) * length );
	if( arr ) {
		arr->number_values = ( number * ) &arr[ 1 ];
		arr->str.string = "[Buffer]";
		arr->str.length = 8;
		arr->str.reference_count = 1;
		arr->str.type = ARRAY;
		arr->length = length;
	}
	return arr;
}

struct source* new_source( int str_len ) {
	struct source *src = malloc( sizeof( struct source ) + sizeof( char ) * ( str_len + 1 ) );
	if( src ) {
		memset( src, 0, sizeof( struct source ) );
		src->str.string = ( char * ) &src[ 1 ];
		src->str.reference_count = 1;
		src->str.length = str_len;
		src->str.type = SOURCE;
	}
	return src;
}

/* Allocate and return a string of the specified length and reference count of 1. */
struct string* new_string( int length ) {
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

struct string* new_string_value( char *source ) {
	struct string *str = new_string( strlen( source ) );
	if( str ) {
		strcpy( str->string, source );
	}
	return str;
}

/* Return a 5-bit hash-code to be used for case-insensitively indexing the specified string. */
static int hash_code( char *str, char terminator ) {
	char chr = str[ 0 ];
	int idx = 1, hash = 0;
	while( chr != terminator && chr ) {
		hash = hash ^ chr;
		chr = str[ idx++ ];
	}
	return hash & 0x1F;
}

/* Return a pointer to the first terminator in str, or the end. */
static char* field_end( char *str, char *terminators ) {
	char mask = 0, *end = terminators;
	while( *end ) {
		mask |= *end++;
	}
	end = str;
	mask = ~mask;
	while( *end & mask || ( *end && !strchr( terminators, *end ) ) ) {
		end++;
	}
	return end;
}

/* Return the first index of a member of chars in str, starting from idx. 
   If idx is negative, return the last index, starting from 0. */
static int str_idx( char *str, char *chars, int idx ) {
	char *chr, *end;
	if( idx < 0 ) {
		chr = end = field_end( str, chars );
		while( *end ) {
			chr = end;
			end = field_end( end + 1, chars );
		}
	} else {
		chr = field_end( str + idx, chars );
	}
	if( *chr ) {
		return chr - str;
	}
	return -1;
}

static int parse_string( char *input, int idx, char *output, int line, char *message ) {
	int offset = idx;
	char chr = input[ idx++ ];
	if( chr == '"' ) {
		while( ( chr & 0x7F ) >= 32 ) {
			chr = input[ idx++ ];
			if( chr == '\\' ) {
				chr = input[ idx++ ];
			} else if( chr == '"' ) {
				break;
			}
		}
		if( chr == '"' ) {
			if( output ) {
				memcpy( output, &input[ offset ], sizeof( char ) * ( idx - offset ) );
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

static int unquote_string( char *string, char *output, char quote ) {
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
				} else if( chr == 'n' ) {
					chr = '\n';
				} else if( chr == 'r' ) {
					chr = '\r';
				} else if( chr == 't' ) {
					chr = '\t';
				}
				if( output ) {
					output[ length++ ] = chr;
				} else {
					length++;
				}
			} else if( chr != quote ) {
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
	struct string *str = new_string( unquote_string( source, NULL, '"' ) );
	if( str ) {
		str->length = unquote_string( source, str->string, '"' );
	}
	return str;
}

/* Return a copy of the specified element-tree. */
static struct element* copy_element( struct element *source ) {
	struct element *elem = NULL, *prev = NULL, *head = NULL;
	while( source ) {
		elem = new_element( source->str.length );
		if( elem ) {
			elem->line = source->line;
			if( prev ) {
				prev->next = elem;
			}
			prev = elem;
			if( head == NULL ) {
				head = elem;
			}
			memcpy( elem->str.string, source->str.string, sizeof( char ) * elem->str.length );
			if( source->child ) {
				elem->child = copy_element( source->child );
				if( elem->child ) {
					source = source->next;
				} else {
					source = elem = NULL;
				}
			} else {
				source = source->next;
			}
		} else {
			source = NULL;
		}
	}
	if( elem == NULL && head ) {
		unref_string( &head->str );
		head = NULL;
	}
	return head;
}

static int parse_child_element( char *buffer, int idx, struct element *parent, int depth, char *message ) {
	struct element *elem = NULL;
	int offset = idx, length = 0, line = parent->line, len;
	char *bracket = NULL, chr = '\n';
	if( depth < 1 ) {
		sprintf( message, "Maximum element depth exceeded on line %d.", line );
		return -5;
	}
	while( chr ) {
		chr = buffer[ idx++ ];
		if( chr <= 32 || ( ( chr < '0' || ( chr & 0x1F ) > 26 ) && ( ( bracket = strchr( "\"#(),;=[]{}", chr ) )
		|| ( chr == '/' && ( buffer[ idx ] == '/' || ( buffer[ idx ] == '*' && buffer[ idx + 1 ] != '/' ) ) ) ) ) ) {
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
					memcpy( elem->str.string, &buffer[ offset ], sizeof( char ) * length );
					/*printf("%d %d %c :%s\n",offset,length,chr,elem->str.string);*/
				} else {
					strcpy( message, OUT_OF_MEMORY );
					return -1;
				}
			}
			if( chr == '\n' || chr == '#' || ( chr == '/' && buffer[ idx ] == '/' ) ) {
				while( chr && chr != '\n' ) {
					chr = buffer[ idx++ ];
				}
				line++;
			} else if( chr == '/' && buffer[ idx ] == '*' ) {
				while( chr && ( chr != '*' || buffer[ idx ] != '/' ) ) {
					chr = buffer[ idx++ ];
					if( chr == '\n' ) {
						line++;
					}
				}
				if( chr ) {
					chr = buffer[ ++idx ];
				} else {
					sprintf( message, "Unclosed comment on line %d.", line );
					return -2;
				}
			} else if( chr == ')' || chr == ']' || chr == '}' ) {
				parent->line = line;
				return idx;
			} else if( chr > 32 ) {
				if( chr == '"' ) {
					len = parse_string( buffer, idx - 1, NULL, line, message );
					if( len < 0 ) {
						return len;
					} else {
						len = len - idx + 1;
					}
				} else {
					len = 2;
				}
				if( elem == NULL ) {
					elem = new_element( len );
					parent->child = elem;
				} else {
					elem->next = new_element( len );
					elem = elem->next;
				}
				if( elem ) {
					elem->line = line;
					if( chr == '"' ) {
						idx = parse_string( buffer, idx - 1, elem->str.string, line, message );
						if( idx < 0 ) {
							return idx;
						}
					} else if( chr == '(' || chr == '[' || chr == '{' ) {
						elem->str.string[ 0 ] = bracket[ 0 ];
						elem->str.string[ 1 ] = bracket[ 1 ];
						idx = parse_child_element( buffer, idx, elem, depth - 1, message );
						if( idx > 0 ) {
							/* Exchange line and elem->line. */
							len = elem->line;
							elem->line = line;
							line = len;
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
				} else {
					strcpy( message, OUT_OF_MEMORY );
					return -1;
				}
			}
			offset = idx;
			length = 0;
		} else {
			length++;
		}
	}
	return idx;
}

static struct element* parse_element( char *buffer, int max_depth, char *message ) {
	int idx;
	struct element elem;
	elem.line = 1;
	elem.str.string = NULL;
	elem.str.type = ELEMENT;
	elem.child = elem.next = NULL;
	idx = parse_child_element( buffer, 0, &elem, max_depth, message );
	if( idx > 0 ) {
		if( buffer[ idx - 1 ] != 0 ) {
			sprintf( message, "Unexpected closing bracket '%c' on line %d.", buffer[ idx - 1 ], elem.line );
			idx = -4;
		}
	}
	if( idx < 0 && elem.child ) {
		unref_string( &elem.child->str );
		elem.child = NULL;
	}
	return elem.child;
}

/* Load the specified portion of a file into buffer (if not null).
   Returns the number of bytes available or read from offset.
   Returns -1 and writes message on failure. */
long load_file( char *file_name, char *buffer, long offset, long count, char *message ) {
	long length, remain = -1;
	FILE *input_file = fopen( file_name, "rb" );
	if( input_file != NULL ) {
		if( fseek( input_file, 0L, SEEK_END ) == 0 ) {
			length = ftell( input_file );
			if( length >= 0 ) {
				if( offset < 0 || offset > length ) {
					offset = length;
				}
				remain = length - offset;
				if( remain > 0 && buffer ) {
					if( count > remain ) {
						count = remain;
					}
					remain = fseek( input_file, offset, SEEK_SET );
					if( remain == 0 ) {
						clearerr( input_file );
						remain = fread( buffer, 1, count, input_file );
						if( ferror( input_file ) ) {
							remain = -1;
						}
					}
				}
			}
		}
		fclose( input_file );
	}
	if( remain < 0 ) {
		strncpy( message, strerror( errno ), 63 );
		message[ 63 ] = 0;
	}
	return remain;
}

static int save_file( char *file_name, char *buffer, int length, int append, char *message ) {
	int count = -1;
	FILE *output_file = fopen( file_name, append ? "ab" : "wb" );
	if( output_file != NULL ) {
		count = fwrite( buffer, sizeof( char ), length, output_file );
		fclose( output_file );
	}
	if( count < length ) {
		strncpy( message, strerror( errno ), 63 );
		message[ 63 ] = 0;
	}
	return count;
}

static void dispose_string_list( struct string_list *list ) {
	struct string_list *next;
	while( list ) {
		next = list->next;
		unref_string( list->str );
		free( list );
		list = next;
	}
}

/* Decrement the reference-count of any referenced types,
   deallocate if necessary, and assign null to the specified variable.
   This must be called for all variables assigned during program execution. */
void dispose_variable( struct variable *var ) {
	var->number_value = 0;
	if( var->string_value ) {
		unref_string( var->string_value );
		var->string_value = NULL;
	}
}

/* As dispose_variable(), but do not assign null for performance reasons.
   The resulting variable may contain an invalid pointer and should not be re-used. */
void dispose_temporary( struct variable *var ) {
	if( var->string_value ) {
		unref_string( var->string_value );
	}
}

/* Assign src variable to dest, managing reference counts. */
void assign_variable( struct variable *src, struct variable *dest ) {
	if( dest->string_value ) {
		unref_string( dest->string_value );
	}
	dest->number_value = src->number_value;
	dest->string_value = src->string_value;
	if( dest->string_value ) {
		dest->string_value->reference_count++;
	}
}

static int compare_variables( struct variable *var1, struct variable *var2 ) {
	int result, len1, len2;
	struct string *str1, *str2;
	if( var1->number_value == var2->number_value ) {
		str1 = var1->string_value;
		str2 = var2->string_value;
		if( str1 == str2 ) {
			result = 0;
		} else if( str1 ) {
			if( str2 ) {
				len1 = str1->length;
				len2 = str2->length;
				if( len1 > len2 ) {
					result = memcmp( str1->string, str2->string, sizeof( char ) * len2 );
				} else {
					result = memcmp( str1->string, str2->string, sizeof( char ) * len1 );
				}
				if( result == 0 ) {
					result = len1 - len2;
				}
			} else {
				result = 1;
			}
		} else {
			result = -1;
		}
	} else if( var1->number_value > var2->number_value ) {
		result = 1;
	} else {
		result = -1;
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

enum result evaluate_number_literal_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	result->number_value = ( ( struct value_expression * ) this )->num;
	return OKAY;
}

enum result evaluate_string_literal_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	result->number_value = ( ( struct value_expression * ) this )->num;
	result->string_value = ( ( struct value_expression * ) this )->str;
	result->string_value->reference_count++;
	return OKAY;
}

static void dispose_expressions( struct expression *expr ) {
	struct expression *next;
	while( expr ) {
		next = expr->next;
		dispose_expressions( expr->parameters );
		if( expr->evaluate == evaluate_string_literal_expression ) {
			unref_string( ( ( struct value_expression * ) expr )->str );
		}
		free( expr );
		expr = next;
	}
}

static void dispose_local_variables( struct local_variable *local ) {
	struct local_variable *next;
	while( local ) {
		next = local->next;
		free( local );
		local = next;
	}
}

static void dispose_global_variable( struct global_variable *global ) {
	dispose_temporary( &global->value );
	if( global->init_function ) {
		unref_string( &global->init_function->str );
	}
	dispose_expressions( global->initializer );
	free( global );
}

void dispose_statements( struct statement *statements ) {
	struct statement *next;
	while( statements ) {
		next = ( struct statement * ) statements->head.next;
		dispose_expressions( statements->head.parameters );
		if( statements->dispose ) {
			statements->dispose( statements );
		} else {
			free( statements );
		}
		statements = next;
	}
}

static void dispose_element( struct element *elem ) {
	struct element *prev;
	while( elem ) {
		if( elem->str.reference_count == 1 ) {
			if( elem->child ) {
				dispose_element( elem->child );
			}
			prev = elem;
			elem = elem->next;
			free( prev );
		} else {
			elem->str.reference_count--;
			elem = NULL;
		}
	}
}

static void truncate_array( struct array *arr ) {
	struct string *str;
	int idx = 0, len = arr->length;
	if( arr->string_values ) {
		while( idx < len ) {
			str = arr->string_values[ idx++ ];
			if( str ) {
				unref_string( str );
			}
		}
	}
	arr->length = 0;
}

static void dispose_array( struct array *arr ) {
	truncate_array( arr );
	if( arr->prev ) {
		arr->prev->next = arr->next;
		if( arr->next ) {
			arr->next->prev = arr->prev;
		}
	}
	free( arr );
}

static void dispose_structure( struct structure *sct ) {
	dispose_string_list( sct->fields );
	free( sct );
}

static void dispose_function( struct function *func ) {
	if( func->file != &func->str ) {
		unref_string( func->file );
	}
	if( func->library ) {
		unref_string( func->library );
	}
	dispose_local_variables( func->variable_decls );
	dispose_statements( func->statements );
	free( func );
}

static void dispose_source( struct source *src ) {
	dispose_string_list( src->imports );
	free( src );
}

/* Decrement the reference count of the specified value and deallocate if necessary. */
void unref_string( struct string *str ) {
	if( str->reference_count == 1 ) {
		switch( str->type ) {
			default:
			case STRING:
				free( str );
				break;
			case ELEMENT:
				dispose_element( ( struct element * ) str );
				break;
			case ARRAY:
				dispose_array( ( struct array * ) str );
				break;
			case STRUCT:
				dispose_structure( ( struct structure * ) str );
				break;
			case GLOBAL: case CONST:
				dispose_global_variable( ( struct global_variable * ) str );
				break;
			case FUNCTION:
				dispose_function( ( struct function * ) str );
				break;
			case SOURCE:
				dispose_source( ( struct source * ) str );
				break;
			case CUSTOM:
				( ( struct custom * ) str )->type->dispose( str );
				break;
		}
	} else {
		str->reference_count--;
	}
}

static void dispose_arrays( struct array *head ) {
	struct array *arr = head->next;
	while( arr ) {
		arr->str.reference_count++;
		truncate_array( arr );
		arr = arr->next;
	}
	while( head->next ) {
		unref_string( &head->next->str );
	}
}

/* Deallocate the specified environment and all types referenced by it. */
void dispose_environment( struct environment *env ) {
	int idx;
	for( idx = 0; idx < 32; idx++ ) {
		dispose_string_list( env->decls_index[ idx ] );
		dispose_keywords( env->statements_index[ idx ] );
		dispose_operators( env->operators_index[ idx ] );
	}
	dispose_string_list( env->globals );
	dispose_arrays( &env->arrays );
}

static struct string_list* new_string_list( struct string *value, struct string_list *next ) {
	struct string_list *list = malloc( sizeof( struct string_list ) );
	if( list ) {
		list->str = value;
		value->reference_count++;
		list->next = next;
	}
	return list;
}

static struct string_list* append_string_list( struct string_list **head, struct string_list **tail, struct string *value ) {
	struct string_list *list = new_string_list( value, NULL );
	if( list ) {
		if( *head ) {
			( *tail )->next = list;
		} else {
			*head = list;
		}
		*tail = list;
	}
	return list;
}

static int get_string_list_index( struct string_list *list, char *value ) {
	int idx = 0;
	while( list && strcmp( list->str->string, value ) ) {
		idx++;
		list = list->next;
	}
	if( list == NULL ) {
		idx = -1;
	}
	return idx;
}

/* Assign an uncatchable exception with the specified exit code and message to vars and return EXCEPTION. */
enum result throw_exit( struct variables *vars, number exit_code, const char *message ) {
	struct variable *exception = vars->exception;
	struct environment *env = vars->func->env;
	env->exit.reference_count = 2;
	env->exit.type = EXIT;
	env->exit.string = ( char * ) message;
	dispose_temporary( exception );
	exception->number_value = exit_code;
	exception->string_value = &env->exit;
	return EXCEPTION;
}

static struct array* stack_trace( struct expression *expr, struct variables *vars, int msg_len, int max_depth ) {
	struct array *arr = new_array( vars->func->env, max_depth, msg_len );
	int idx = 0, line = expr->line;
	if( arr ) {
		while( vars && vars->func && idx < max_depth ) {
			vars->func->str.reference_count++;
			arr->string_values[ idx ] = &vars->func->str;
			arr->number_values[ idx++ ] = line;
			line = vars->line;
			vars = vars->parent;
		}
		arr->length = idx;
	}
	return arr;
}

/* Assign an exception with the specified error code and message to vars and return EXCEPTION. */
enum result throw( struct variables *vars, struct expression *source, number num, const char *string ) {
	struct variable *exception = vars->exception;
	struct array *arr = NULL;
	if( string ) {
		arr = stack_trace( source, vars, strlen( string ), 16 );
		if( arr ) {
			strcpy( arr->str.string, string );
		} else {
			return throw_exit( vars, 1, OUT_OF_MEMORY );
		}
	}
	dispose_temporary( exception );
	exception->number_value = num;
	exception->string_value = &arr->str;
	return EXCEPTION;
}

/* Write the specified bytes as a string literal to output (if not null).
   The encoded length is returned. */
int write_byte_string( char *bytes, int count, char *output ) {
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
				memcpy( &output[ length ], elem->str.string, sizeof( char ) * elem->str.length );
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

static int qualify_decl( struct function *func, char *decl, int len, char *output ) {
	int llen = 0, qlen = len;
	if( func && func->library ) {
		llen = func->library->length;
		qlen = llen + 1 + len;
	}
	if( output ) {
		if( llen ) {
			memcpy( output, func->library->string, sizeof( char ) * llen );
			output[ llen++ ] = '_';
		}
		memcpy( &output[ llen ], decl, sizeof( char ) * len );
		output[ qlen ] = 0;
	}
	return qlen;
}

static struct function* new_function( char *name, struct function *parent ) {
	int nlen, qlen = 0;
	struct function *func;
	if( name ) {
		nlen = strlen( name );
		qlen = qualify_decl( parent, name, nlen, NULL );
	}
	func = calloc( 1, sizeof( struct function ) + sizeof( char ) * ( qlen + 1 ) );
	if( func ) {
		func->str.string = ( char * ) &func[ 1 ];
		if( name ) {
			func->str.length = qualify_decl( parent, name, nlen, func->str.string );
		} else {
			func->str.string = "[Function]";
			func->str.length = 10;
		}
		func->str.reference_count = 1;
		func->str.type = FUNCTION;
	}
	return func;
}

static struct local_variable* new_local_variable( struct function *func, struct element *name, struct structure *type, char *message ) {
	struct local_variable *local = NULL;
	if( func->num_variables >= 128 ) {
		sprintf( message, "Too many local variables in function '%.64s' on line %d.", func->str.string, func->line );
	} else if( validate_local( &name->str, name->line, func->env, message ) ) {
		local = malloc( sizeof( struct local_variable ) + sizeof( char ) * ( name->str.length + 1 ) );
		if( local ) {
			local->index = func->num_variables;
			local->name = ( char * ) &local[ 1 ];
			strcpy( local->name, name->str.string );
			local->type = type;
			local->next = NULL;
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	}
	return local;
}

static struct global_variable* new_global_variable( char *name,
	struct structure *type, struct function *init_function, struct expression *initializer, char *message ) {
	int nlen = strlen( name );
	int qlen = qualify_decl( init_function, name, nlen, NULL );
	struct global_variable *global = malloc( sizeof( struct global_variable ) + sizeof( char ) * ( qlen + 1 ) );
	if( global ) {
		global->str.reference_count = 1;
		global->str.string = ( char * ) &global[ 1 ];
		qualify_decl( init_function, name, nlen, global->str.string );
		global->str.length = qlen;
		global->str.type = GLOBAL;
		global->value.number_value = 0;
		global->value.string_value = NULL;
		global->type = type;
		global->init_function = init_function;
		if( init_function ) {
			init_function->str.reference_count++;
		}
		global->initializer = initializer;
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return global;
}

struct string_list* add_decl( struct string *decl, int line, struct environment *env, char *message ) {
	struct string_list *list = NULL;
	int hash = validate_decl( decl, line, env, message ) - 1;
	if( message[ 0 ] == 0 ) {
		list = new_string_list( decl, env->decls_index[ hash ] );
		if( list ) {
			env->decls_index[ hash ] = list;
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	}
	return list;
}

static struct string_list* add_global( struct global_variable *global, int line, struct environment *env, char *message ) {
	struct string_list *list = append_string_list( &env->globals, &env->globals_tail, &global->str );
	if( list ) {
		list = add_decl( &global->str, line, env, message );
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return list;
}

/* Add the specified constants to env. Returns zero and writes message on failure. */
int add_constants( struct constant *constants, struct environment *env, char *message ) {
	struct global_variable *global;
	while( constants->name && message[ 0 ] == 0 ) {
		global = new_global_variable( constants->name, NULL, NULL, NULL, message );
		if( global ) {
			if( add_global( global, 0, env, message ) ) {
				global->str.type = CONST;
				global->value.number_value = constants->number_value;
				if( constants->string_value ) {
					global->value.string_value = new_string_literal( constants->string_value );
					if( global->value.string_value == NULL ) {
						strcpy( message, OUT_OF_MEMORY );
					}
				}
			}
			unref_string( &global->str );
		}
		constants++;
	}
	return message[ 0 ] == 0;
}

enum result evaluate_local( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable *var = &vars->locals[ this->index ];
	result->number_value = var->number_value;
	if( var->string_value ) {
		result->string_value = var->string_value;
		result->string_value->reference_count++;
	}
	return OKAY;
}

enum result evaluate_local_post_inc( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable *var = &vars->locals[ this->index ];
	if( var->string_value ) {
		return throw( vars, this, 0, "Not a number." );
	} else {
		result->number_value = var->number_value++;
	}
	return OKAY;
}

enum result evaluate_local_post_dec( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable *var = &vars->locals[ this->index ];
	if( var->string_value ) {
		return throw( vars, this, 0, "Not a number." );
	} else {
		result->number_value = var->number_value--;
	}
	return OKAY;
}

enum result evaluate_global( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable *var = &( ( struct global_variable * ) ( ( struct value_expression * ) this )->str )->value;
	result->number_value = var->number_value;
	if( var->string_value ) {
		result->string_value = var->string_value;
		result->string_value->reference_count++;
	}
	return OKAY;
}

static enum result execute_global_assignment( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable var = { 0 }, *dest = ( ( struct global_assignment_statement * ) this )->destination;
	enum result ret = this->parameters->evaluate( this->parameters, vars, &var );
	if( ret ) {
		dispose_temporary( dest );
		dest->number_value = var.number_value;
		dest->string_value = var.string_value;
	}
	return ret;
}

enum result execute_local_assignment( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable var = { 0 }, *dest = &vars->locals[ this->index ];
	enum result ret = this->parameters->evaluate( this->parameters, vars, &var );
	if( ret ) {
		dispose_temporary( dest );
		dest->number_value = var.number_value;
		dest->string_value = var.string_value;
	}
	return ret;
}

static enum result write_string_expression( struct expression *expr, struct variables *vars, int lf, FILE *output ) {
	struct variable value = { 0, NULL };
	struct string *str;
	enum result ret = expr->evaluate( expr, vars, &value );
	if( ret ) {
		if( value.string_value ) {
			if( value.string_value->type == CUSTOM && ( ( struct custom * ) value.string_value )->type->to_str ) {
				ret = ( ( struct custom * ) value.string_value )->type->to_str( &value, &str, vars, expr );
				dispose_temporary( &value );
				if( ret ) {
					value.string_value = str;
				} else {
					return ret;
				}
			}
			fwrite( value.string_value->string, 1, value.string_value->length, output );
		} else {
#if defined( FLOATING_POINT )
			fprintf( output, "%.16g", value.number_value );
#else
			fprintf( output, "%d", value.number_value );
#endif
		}
		if( lf ) {
			fputc( '\n', output );
		}
		dispose_temporary( &value );
	}
	return ret;
}

static enum result execute_print_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	return write_string_expression( this->parameters, vars, 1, stdout );
}

static enum result execute_write_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	return write_string_expression( this->parameters, vars, 0, stdout );
}

static enum result execute_error_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	return write_string_expression( this->parameters, vars, 1, stderr );
}

static enum result execute_throw_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable exc = { 0, NULL };
	this->parameters->evaluate( this->parameters, vars, &exc );
	dispose_temporary( vars->exception );
	vars->exception->number_value = exc.number_value;
	vars->exception->string_value = exc.string_value;
	return EXCEPTION;
}

static enum result execute_exit_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable exit_code = { 0, NULL };
	char *message = NULL;
	enum result ret = this->parameters->evaluate( this->parameters, vars, &exit_code );
	if( ret ) {
		vars->func->env->interrupted = 1;
		if( vars->func->env->worker ) {
			message = "Worker exited.";
		}
		ret = throw_exit( vars, exit_code.number_value, message );
		dispose_temporary( &exit_code );
	}
	return ret;
}

enum result execute_return_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	if( this->parameters->evaluate( this->parameters, vars, result ) ) {
		return RETURN;
	}
	return EXCEPTION;
}

static enum result execute_break_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	return BREAK;
}

static enum result execute_continue_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	return CONTINUE;
}

/* Throw an exception for the specified expression to indicate the interrupted status has been set. */
enum result throw_interrupted( struct variables *vars, struct expression *source ) {
	if( vars->func->env->worker ) {
		return throw_exit( vars, 0, "Interrupted." );
	} else {
		return throw( vars, source, 0, "Interrupted.");
	}
}

static enum result execute_tail_call_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	enum result ret = OKAY;
	struct expression *source = this->parameters, *parameter = source->parameters;
	struct function *func = ( ( struct function_expression * ) source )->function;
	int num_params = func->num_parameters, num_locals = func->num_variables;
	int idx = 0, size = sizeof( struct variable ) * num_params;
	struct variable *locals = alloca( size );
	memset( locals, 0, size );
	while( idx < num_params ) {
		ret = parameter->evaluate( parameter, vars, &locals[ idx++ ] );
		if( ret ) {
			parameter = parameter->next;
		} else {
			break;
		}
	}
	if( ret ) {
		idx = 0;
		while( idx < num_params ) {
			assign_variable( &locals[ idx ], &vars->locals[ idx ] );
			idx++;
		}
		while( idx < num_locals ) {
			dispose_variable( &vars->locals[ idx++ ] );
		}
		if( func->env->interrupted ) {
			ret = throw_interrupted( vars, this );
		} else {
			ret = AGAIN;
		}
	}
	idx = 0;
	while( idx < num_params ) {
		dispose_temporary( &locals[ idx++ ] );
	}
	return ret;
}

void dispose_block_statement( struct statement *this ) {
	dispose_statements( ( ( struct block_statement * ) this )->if_block );
	dispose_statements( ( ( struct block_statement * ) this )->else_block );
	free( this );
}

/* Return OKAY if var contains an instance of the specified structure.
   If vars is non-null a suitable exception is thrown for the specified source expression. */
enum result is_instance( struct variable *var, struct structure *type, struct variables *vars, struct expression *source ) {
	char msg[ 128 ];
	struct structure *struc;
	if( var->string_value && var->string_value->type == ARRAY ) {
		struc = ( ( struct array * ) var->string_value )->structure;
		while( struc ) {
			if( type == struc ) {
				return OKAY;
			}
			struc = struc->super;
		}
	}
	if( vars ) {
		sprintf( msg, "Not an instance of '%.64s'.", type->str.string );
		return throw( vars, source, var->number_value, msg );
	}
	return EXCEPTION;
}

/* Evaluate the specified expression into the specified result variable.
   Throws an exception if the result is not a reference. */
enum result evaluate_string( struct expression *expr, struct variables *vars, struct variable *result ) {
	enum result ret = expr->evaluate( expr, vars, result );
	if( ret && result->string_value == NULL ) {
		ret = throw( vars, expr, 0, "Not a string." );
	}
	return ret;
}

/* Evaluate the specified expression into the specified result variable.
   Throws an exception if the result is not an array reference. */
enum result evaluate_array( struct expression *expr, struct variables *vars, struct variable *result ) {
	enum result ret = expr->evaluate( expr, vars, result );
	if( ret && ( result->string_value == NULL || result->string_value->type != ARRAY ) ) {
		ret = throw( vars, expr, 0, "Not an array." );
		dispose_variable( result );
	}
	return ret;
}

/* Evaluate the specified expression into the specified result variable.
   Throws an exception if the result is not an element reference or null (if allowed). */
enum result evaluate_element( struct expression *expr, struct variables *vars, struct variable *result, int allow_null ) {
	enum result ret = expr->evaluate( expr, vars, result );
	if( ret ) {
		if( result->string_value ) {
			if( result->string_value->type == ELEMENT ) {
				return ret;
			}
		} else if( allow_null && !result->number_value ) {
			return ret;
		}
		ret = throw( vars, expr, 0, "Not an element." );
		dispose_variable( result );
	}
	return ret;
}

enum result to_int( struct variable *var, int *result, struct variables *vars, struct expression *source ) {
	number value;
	enum result ret;
	if( var->string_value ) {
		if( var->string_value->type == CUSTOM && ( ( struct custom * ) var->string_value )->type->to_num ) {
			ret = ( ( struct custom * ) var->string_value )->type->to_num( var, &value, vars, source );
		} else {
			ret = throw( vars, source, 0, "Not a number." );
		}
		if( ret ) {
			*result = ( long_int ) value;
		}
		return ret;
	}
	*result = ( long_int ) var->number_value;
	return OKAY;
}

enum result to_num( struct variable *var, number *result, struct variables *vars, struct expression *source ) {
	if( var->string_value ) {
		if( var->string_value->type == CUSTOM && ( ( struct custom * ) var->string_value )->type->to_num ) {
			return ( ( struct custom * ) var->string_value )->type->to_num( var, result, vars, source );
		} else {
			return throw( vars, source, 0, "Not a number." );
		}
	}
	*result = var->number_value;
	return OKAY;
}

/* Assign src variable to dest array at the specified index, managing reference counts. */
enum result assign_array_variable( struct variable *src, struct array *arr, int idx, struct variables *vars, struct expression *source ) {
	struct string *str;
	if( arr->string_values ) {
		arr->number_values[ idx ] = src->number_value;
		str = arr->string_values[ idx ];
		if( str ) {
			unref_string( str );
		}
		str = src->string_value;
		arr->string_values[ idx ] = str;
		if( str ) {
			str->reference_count++;
		}
		return OKAY;
	}
	return to_num( src, &arr->number_values[ idx ], vars, source );
}

/* Evaluate the specified expression into the specified number result. */
enum result evaluate_number( struct expression *expr, struct variables *vars, number *result ) {
	enum result ret;
	struct variable var, *local;
	if( expr->evaluate == evaluate_number_literal_expression ) {
		*result = ( ( struct value_expression * ) expr )->num;
		return OKAY;
	} else if( expr->evaluate == evaluate_local ) {
		local = &vars->locals[ expr->index ];
		if( local->string_value ) {
			return to_num( local, result, vars, expr );
		}
		*result = local->number_value;
		return OKAY;
	}
	var.number_value = 0;
	var.string_value = NULL;
	ret = expr->evaluate( expr, vars, &var );
	if( ret ) {
		if( var.string_value ) {
			ret = to_num( &var, result, vars, expr );
			dispose_temporary( &var );
		} else {
			*result = var.number_value;
		}
	}
	return ret;
}

/* Evaluate the specified expression into the specified integer result. */
enum result evaluate_integer( struct expression *expr, struct variables *vars, int *result ) {
	enum result ret;
	struct variable var, *local;
	if( expr->evaluate == evaluate_number_literal_expression ) {
		*result = expr->index;
		return OKAY;
	} else if( expr->evaluate == evaluate_local ) {
		local = &vars->locals[ expr->index ];
		if( local->string_value ) {
			return to_int( local, result, vars, expr );
		}
		*result = ( long_int ) local->number_value;
		return OKAY;
	}
	var.number_value = 0;
	var.string_value = NULL;
	ret = expr->evaluate( expr, vars, &var );
	if( ret ) {
		if( var.string_value ) {
			ret = to_int( &var, result, vars, expr );
			dispose_temporary( &var );
		} else {
			*result = ( long_int ) var.number_value;
		}
	}
	return ret;
}

/* Return 1 if the specified reference is an instance of the specified custom type. */
int is_custom_instance( struct string *str, struct custom_type *type ) {
	return str && str->type == CUSTOM && ( ( struct custom * ) str )->type == type;
}

/* Evaluate the specified expression into the specified result variable.
   Throws an exception if the value is not an instance of the specified custom type. */
enum result evaluate_custom( struct expression *expr, struct custom_type *type, struct variables *vars, struct variable *result ) {
	char msg[ 128 ];
	enum result ret = expr->evaluate( expr, vars, result );
	if( ret ) {
		if( is_custom_instance( result->string_value, type ) ) {
			return ret;
		}
		sprintf( msg, "Not an instance of '%.64s'.", type->name );
		ret = throw( vars, expr, 0, msg );
		dispose_variable( result );
	}
	return ret;
}

static enum result execute_statements( struct statement *stmt, struct variables *vars, struct variable *result ) {
	enum result ret = OKAY;
	while( stmt && ( ret = stmt->head.evaluate( ( struct expression * ) stmt, vars, result ) ) == OKAY ) {
		stmt = ( struct statement * ) stmt->head.next;
	}
	return ret;
}

static int is_exit( struct variable *exception ) {
	return exception->string_value && exception->string_value->type == EXIT;
}

static enum result execute_try_finally_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	enum result try_ret, fin_ret;
	struct variables try_vars, fin_vars;
	struct variable try_exc = { 0 }, try_res = { 0 };
	struct variable fin_exc = { 0 }, fin_res = { 0 };
	try_vars.parent = fin_vars.parent = vars->parent;
	try_vars.exception = &try_exc;
	fin_vars.exception = &fin_exc;
	try_vars.locals = fin_vars.locals = vars->locals;
	try_vars.func = fin_vars.func = vars->func;
	try_vars.line = fin_vars.line = vars->line;
	try_ret = execute_statements( ( ( struct block_statement * ) this )->if_block, &try_vars, &try_res );
	fin_ret = execute_statements( ( ( struct block_statement * ) this )->else_block, &fin_vars, &fin_res );
	if( try_ret == OKAY ) {
		if( fin_ret == OKAY ) {
			return OKAY;
		} else if( fin_ret == RETURN ) {
			assign_variable( &fin_res, result );
			dispose_temporary( &fin_res );
		} else if( fin_ret == EXCEPTION ) {
			assign_variable( fin_vars.exception, vars->exception );
			dispose_temporary( fin_vars.exception );
		}
		return fin_ret;
	}
	if( try_ret == RETURN ) {
		if( fin_ret == OKAY ) {
			assign_variable( &try_res, result );
			dispose_temporary( &try_res );
			return RETURN;
		} else if( fin_ret == EXCEPTION ) {
			assign_variable( fin_vars.exception, vars->exception );
			dispose_temporary( fin_vars.exception );
		} else if( fin_ret == RETURN ) {
			assign_variable( &fin_res, result );
			dispose_temporary( &fin_res );
		}
		dispose_temporary( &try_res );
		return fin_ret;
	}
	if( try_ret == EXCEPTION ) {
		if( fin_ret == EXCEPTION ) {
			if( is_exit( fin_vars.exception ) ) {
				assign_variable( fin_vars.exception, vars->exception );
				dispose_temporary( try_vars.exception );
				dispose_temporary( fin_vars.exception );
				return EXCEPTION;
			}
			dispose_temporary( fin_vars.exception );
		} else if( fin_ret == RETURN ) {
			dispose_temporary( &fin_res );
		}
		assign_variable( try_vars.exception, vars->exception );
		dispose_temporary( try_vars.exception );
	}
	return try_ret;
}

static enum result execute_try_catch_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	enum result ret = OKAY;
	int idx = this->index;
	struct variables try_vars;
	struct local_variable *exception_var;
	try_vars.parent = vars->parent;
	try_vars.exception = &vars->locals[ idx ];
	try_vars.locals = vars->locals;
	try_vars.func = vars->func;
	try_vars.line = vars->line;
	ret = execute_statements( ( ( struct block_statement * ) this )->if_block, &try_vars, result );
	if( ret == EXCEPTION ) {
		exception_var = vars->func->variable_decls;
		while( idx-- > 0 ) {
			exception_var = exception_var->next;
		}
		if( is_exit( try_vars.exception )
		|| ( exception_var->type && !is_instance( try_vars.exception, exception_var->type, NULL, NULL ) ) ) {
			assign_variable( try_vars.exception, vars->exception );
		} else {
			return execute_statements( ( ( struct block_statement * ) this )->else_block, vars, result );
		}
	}
	return ret;
}

enum result execute_if_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct statement *stmt = ( ( struct block_statement * ) this )->if_block;
	struct variable condition = { 0 };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &condition );
	if( ret ) {
		if( condition.string_value ) {
			dispose_temporary( &condition );
		} else if( condition.number_value == 0 ) {
			stmt = ( ( struct block_statement * ) this )->else_block;
		}
		while( stmt && ( ret = stmt->head.evaluate( ( struct expression * ) stmt, vars, result ) ) == OKAY ) {
			stmt = ( struct statement * ) stmt->head.next;
		}
	}
	return ret;
}

enum result execute_while_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct environment *env = vars->func->env;
	struct variable condition = { 0 };
	struct statement *stmt;
	enum result ret;
	while( 1 ) {
		if( this->parameters->evaluate( this->parameters, vars, &condition ) ) {
			if( condition.string_value ) {
				dispose_variable( &condition );
			} else if( condition.number_value ) {
				condition.number_value = 0;
			} else {
				return OKAY;
			}
		} else {
			return EXCEPTION;
		}
		stmt = ( ( struct block_statement * ) this )->if_block;
		while( stmt && ( ret = stmt->head.evaluate( ( struct expression * ) stmt, vars, result ) ) == OKAY ) {
			stmt = ( struct statement * ) stmt->head.next;
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
			return throw_interrupted( vars, this );
		}
	}
}

static enum result execute_until_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct environment *env = vars->func->env;
	struct variable condition = { 0 };
	struct statement *stmt;
	enum result ret;
	while( 1 ) {
		stmt = ( ( struct block_statement * ) this )->if_block;
		while( stmt && ( ret = stmt->head.evaluate( ( struct expression * ) stmt, vars, result ) ) == OKAY ) {
			stmt = ( struct statement * ) stmt->head.next;
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
		if( this->parameters->evaluate( this->parameters, vars, &condition ) ) {
			if( condition.string_value || condition.number_value ) {
				dispose_temporary( &condition );
				return OKAY;
			}
		} else {
			return EXCEPTION;
		}
		if( env->interrupted ) {
			return throw_interrupted( vars, this );
		}
	}
}

enum result execute_call_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable var = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &var );
	if( ret ) {
		dispose_temporary( &var );
	}
	return ret;
}

enum result execute_array_assignment( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int idx;
	struct array *arr;
	enum result ret = OKAY;
	struct variable dest = { 0, NULL }, src = { 0, NULL };
	struct expression *src_expr = this->parameters, *dest_expr = src_expr->next, *idx_expr = dest_expr->next;
	if( dest_expr->evaluate == evaluate_local ) {
		arr = ( struct array * ) vars->locals[ dest_expr->index ].string_value;
	} else {
		ret = dest_expr->evaluate( dest_expr, vars, &dest );
		arr = ( struct array * ) dest.string_value;
	}
	if( ret ) {
		if( arr && arr->str.type == ARRAY ) {
			ret = evaluate_integer( idx_expr, vars, &idx );
			if( ret ) {
				if( ( unsigned int ) idx < ( unsigned int ) arr->length ) {
					ret = src_expr->evaluate( src_expr, vars, &src );
					if( ret ) {
						if( arr->string_values ) {
							if( arr->string_values[ idx ] ) {
								unref_string( arr->string_values[ idx ] );
							}
							arr->number_values[ idx ] = src.number_value;
							arr->string_values[ idx ] = src.string_value;
						} else if( src.string_value ) {
							ret = to_num( &src, &arr->number_values[ idx ], vars, src_expr );
							dispose_temporary( &src );
						} else {
							arr->number_values[ idx ] = src.number_value;
						}
					}
				} else {
					ret = throw( vars, idx_expr, idx, "Array index out of bounds." );
				}
			}
		} else {
			ret = throw( vars, dest_expr, 0, "Not an array." );
		}
		dispose_temporary( &dest );
	}
	return ret;
}

static enum result execute_struct_assignment( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct array *arr;
	struct expression *destination = this->parameters->next;
	struct variable obj = { 0, NULL }, var = { 0, NULL };
	enum result ret = destination->evaluate( destination, vars, &obj );
	if( ret ) {
		ret = is_instance( &obj, ( ( struct structure_statement * ) this )->structure, vars, destination );
		if( ret ) {
			arr = ( struct array * ) obj.string_value;
			ret = this->parameters->evaluate( this->parameters, vars, &var );
			if( ret ) {
				arr->number_values[ this->index ] = var.number_value;
				if( arr->string_values ) {
					if( arr->string_values[ this->index ] ) {
						unref_string( arr->string_values[ this->index ] );
					}
					arr->string_values[ this->index ] = var.string_value;
				} else {
					dispose_temporary( &var );
				}
			}
		}
		dispose_temporary( &obj );
	}
	return ret;
}

static enum result execute_switch_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int matched = 0;
	struct expression *case_expr;
	struct variable switch_value = { 0, NULL }, case_value = { 0, NULL };
	struct statement *cases = ( ( struct block_statement * ) this )->if_block;
	struct statement *deflt = ( ( struct block_statement * ) this )->else_block;
	enum result ret = this->parameters->evaluate( this->parameters, vars, &switch_value );
	if( ret ) {
		while( cases && ret && !matched ) {
			case_expr = cases->head.parameters;
			while( case_expr && ret && !matched ) {
				ret = case_expr->evaluate( case_expr, vars, &case_value );
				if( ret ) {
					matched = compare_variables( &switch_value, &case_value ) == 0;
					dispose_variable( &case_value );
					if( matched ) {
						ret = cases->head.evaluate( ( struct expression * ) cases, vars, result );
					}
				}
				case_expr = case_expr->next;
			}
			cases = ( struct statement * ) cases->head.next;
		}
		if( ret && !matched && deflt ) {
			ret = deflt->head.evaluate( ( struct expression * ) deflt, vars, result );
		}
		dispose_temporary( &switch_value );
	}
	return ret;
}

static enum result execute_case_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct statement *stmt = ( ( struct block_statement * ) this )->if_block;
	enum result ret = OKAY;
	while( stmt && ( ret = stmt->head.evaluate( ( struct expression * ) stmt, vars, result ) ) == OKAY ) {
		stmt = ( struct statement * ) stmt->head.next;
	}
	return ret;
}

enum result execute_increment_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable *local = &vars->locals[ this->index ];
	if( local->string_value ) {
		return throw( vars, this, 0, "Not a number." );
	}
	local->number_value++;
	return OKAY;
}

enum result execute_decrement_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable *local = &vars->locals[ this->index ];
	if( local->string_value ) {
		return throw( vars, this, 0, "Not a number." );
	}
	local->number_value--;
	return OKAY;
}

static enum result execute_save_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int count;
	char message[ 64 ];
	struct variable str = { 0, NULL }, file = { 0, NULL };
	enum result ret = evaluate_string( this->parameters, vars, &str );
	if( ret ) {
		ret = evaluate_string( this->parameters->next, vars, &file );
		if( ret ) {
			count = save_file( file.string_value->string,
				str.string_value->string, str.string_value->length, this->index, message );
			if( count != str.string_value->length ) {
				ret = throw( vars, this, 0, message );
			}
			dispose_temporary( &file );
		}
		dispose_temporary( &str );
	}
	return ret;
}

static enum result execute_arraycopy_statement( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *param = this->parameters;
	int src_idx, dest_idx, count;
	struct variable src = { 0 }, dest = { 0 };
	struct array *src_arr, *dest_arr;
	struct string **src_str, **dest_str, **dest_str_end;
	enum result ret = evaluate_array( param, vars, &src );
	if( ret ) {
		param = param->next;
		ret = evaluate_integer( param, vars, &src_idx );
		if( ret ) {
			param = param->next;
			ret = evaluate_array( param, vars, &dest );
			if( ret ) {
				param = param->next;
				ret = evaluate_integer( param, vars, &dest_idx );
				if( ret ) {
					param = param->next;
					ret = evaluate_integer( param, vars, &count );
					if( ret ) {
						src_arr = ( struct array * ) src.string_value;
						dest_arr = ( struct array * ) dest.string_value;
						if( count >= 0 && src_idx >= 0 && src_idx + count <= src_arr->length && dest_idx >= 0 && dest_idx + count <= dest_arr->length ) {
							memmove( &dest_arr->number_values[ dest_idx ], &src_arr->number_values[ src_idx ], count * sizeof( number ) );
							if( dest_arr->string_values ) {
								dest_str = &dest_arr->string_values[ dest_idx ];
								dest_str_end = dest_str + count;
								if( src_arr->string_values ) {
									src_str = &src_arr->string_values[ src_idx ];
									if( src_str + count < dest_str_end ) {
										dest_str_end--;
										src_str += count - 1;
										while( dest_str_end >= dest_str ) {
											if( *src_str ) {
												( *src_str )->reference_count++;
											}
											if( *dest_str_end ) {
												unref_string( *dest_str_end );
											}
											*dest_str_end-- = *src_str--;
										}
									} else {
										while( dest_str < dest_str_end ) {
											if( *src_str ) {
												( *src_str )->reference_count++;
											}
											if( *dest_str ) {
												unref_string( *dest_str );
											}
											*dest_str++ = *src_str++;
										}
									}
								} else {
									while( dest_str < dest_str_end ) {
										if( *dest_str ) {
											unref_string( *dest_str );
											*dest_str = NULL;
										}
										dest_str++;
									}
								}
							}
						} else {
							ret = throw( vars, param, count, "Array index out of bounds." );
						}
					}
				}
				dispose_temporary( &dest );
			}
		}
		dispose_temporary( &src );
	}
	return ret;
}

static enum result evaluate_call_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int idx, count;
	enum result ret = OKAY;
	struct statement *stmt;
	struct variables call_vars;
	struct expression *parameter = this->parameters;
	call_vars.parent = vars;
	call_vars.line = this->line;
	call_vars.func = ( ( struct function_expression * ) this )->function;
	count = sizeof( struct variable ) * call_vars.func->num_variables;
	if( ( size_t ) &this < call_vars.func->env->stack_limit ) {
		return throw( vars, this, 0, "Stack overflow." );
	}
	call_vars.locals = alloca( count );
	call_vars.exception = vars->exception;
	memset( call_vars.locals, 0, count );
	idx = 0, count = call_vars.func->num_parameters;
	while( idx < count ) {
		ret = parameter->evaluate( parameter, vars, &call_vars.locals[ idx++ ] );
		if( ret ) {
			parameter = parameter->next;
		} else {
			break;
		}
	}
	while( ret ) {
		stmt = call_vars.func->statements;
		while( stmt && ( ret = stmt->head.evaluate( ( struct expression * ) stmt, &call_vars, result ) ) == OKAY ) {
			stmt = ( struct statement * ) stmt->head.next;
		}
		if( stmt ) {
			if( ret == RETURN ) {
				ret = OKAY;
			} else if( ret == AGAIN ) {
				continue;
			} else if( ret ) {
				ret = throw( vars, this, ret, "Unhandled 'break' or 'continue'." );
			}
		}
		break;
	}
	idx = 0, count = call_vars.func->num_variables;
	while( idx < count ) {
		dispose_temporary( &call_vars.locals[ idx++ ] );
	}
	return ret;
}

static enum result evaluate_refcall_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct function_expression expr = { 0 };
	struct variable func = { 0, NULL };
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, vars, &func );
	if( ret ) {
		if( func.string_value && func.string_value->type == FUNCTION ) {
			expr.expr.line = this->line;
			expr.function = ( struct function * ) func.string_value;
			if( expr.function->num_parameters == this->index ) {
				expr.expr.parameters = parameter->next;
				ret = evaluate_call_expression( &expr.expr, vars, result );
			} else {
				ret = throw( vars, this, this->index, "Incorrect number of parameters to function." );
			}
		} else {
			ret = throw( vars, this, func.number_value, "Not a function reference." );
		}
		dispose_temporary( &func );
	}
	return ret;
}

static enum result evaluate_thiscall_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int idx, count;
	struct array *arr;
	struct variable obj = { 0 };
	struct string *function = NULL;
	struct function_expression call_expr = { 0 };
	struct value_expression obj_expr = { 0 };
	struct structure *struc = ( struct structure * ) ( ( struct value_expression * ) this )->str;
	enum result ret = this->parameters->evaluate( this->parameters, vars, &obj );
	if( ret ) {
		ret = is_instance( &obj, struc, vars, this );
		if( ret ) {
			arr = ( struct array * ) obj.string_value;
			idx = this->index >> 8;
			count = this->index & 0xFF;
			if( arr->string_values ) {
				function = arr->string_values[ idx ];
			}
			if( function && function->type == FUNCTION ) {
				obj_expr.str = obj.string_value;
				obj_expr.expr.evaluate = evaluate_string_literal_expression;
				obj_expr.expr.next = this->parameters->next;
				call_expr.expr.line = this->line;
				call_expr.function = ( struct function * ) function;
				call_expr.expr.parameters = &obj_expr.expr;
				if( call_expr.function->num_parameters == count ) {
					function->reference_count++;
					ret = evaluate_call_expression( &call_expr.expr, vars, result );
					function->reference_count--;
				} else {
					ret = throw( vars, this, count, "Incorrect number of parameters to function." );
				}
			} else {
				ret = throw( vars, this, arr->number_values[ idx ], "Not a function reference." );
			}
		}
		dispose_temporary( &obj );
	}
	return ret;
}

static enum result evaluate_type_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct structure *struc;
	struct variable var = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &var );
	if( ret && var.string_value ) {
		result->number_value = var.string_value->type + 1;
		if( var.string_value->type == ARRAY ) {
			struc = ( ( struct array * ) var.string_value )->structure;
			if( struc ) {
				result->string_value = &struc->str;
				struc->str.reference_count++;
			}
		}
		dispose_temporary( &var );
	}
	return ret;
}

static enum result evaluate_field_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable struc = { 0, NULL };
	struct string_list *field;
	int idx;
	enum result ret = parameter->evaluate( parameter, vars, &struc );
	if( ret ) {
		if( struc.string_value && struc.string_value->type == STRUCT ) {
			ret = evaluate_integer( parameter->next, vars, &idx );
			if( ret ) {
				if( idx >= 0 && idx < ( ( struct structure * ) struc.string_value )->length ) {
					field = ( ( struct structure * ) struc.string_value )->fields;
					while( idx-- ) {
						field = field->next;
					}
					result->string_value = field->str;
					result->string_value->reference_count++;
				} else {
					ret = throw( vars, this, idx, "Field index out of bounds." );
				}
			}
		} else {
			ret = throw( vars, this, struc.number_value, "Not a structure." );
		}
		dispose_temporary( &struc );
	}
	return ret;
}

static enum result evaluate_instanceof_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable arr = { 0, NULL }, struc = { 0, NULL };
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, vars, &arr );
	if( ret ) {
		ret = parameter->next->evaluate( parameter->next, vars, &struc );
		if( ret ) {
			if( struc.string_value && struc.string_value->type == STRUCT ) {
				if( is_instance( &arr, ( struct structure * ) struc.string_value, NULL, NULL ) ) {
					assign_variable( &arr, result );
				}
			} else {
				ret = throw( vars, this, struc.number_value, "Not a structure." );
			}
			dispose_temporary( &struc );
		}
		dispose_temporary( &arr );
	}
	return ret;
}

static enum result evaluate_trace_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	char *msg = "";
	int msg_len = 0;
	struct array *arr;
	struct variable var = { 0, NULL };
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, vars, &var );
	if( ret ) {
		if( var.string_value ) {
			msg = var.string_value->string;
			msg_len = var.string_value->length;
		}
		arr = stack_trace( this, vars, msg_len, 16 );
		if( arr ) {
			memcpy( arr->str.string, msg, msg_len );
			result->number_value = var.number_value;
			result->string_value = &arr->str;
		} else {
			ret = throw_out_of_memory( vars, this );
		}
		dispose_temporary( &var );
	}
	return ret;
}

static enum result evaluate_member_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct array *arr;
	struct variable obj = { 0, NULL };
	struct expression *parameter = this->parameters;
	struct structure *struc = ( struct structure * ) ( ( struct value_expression * ) this )->str;
	enum result ret = parameter->evaluate( parameter, vars, &obj );
	if( ret ) {
		ret = is_instance( &obj, struc, vars, this );
		if( ret ) {
			arr = ( struct array * ) obj.string_value;
			result->number_value = arr->number_values[ this->index ];
			if( arr->string_values && arr->string_values[ this->index ] ) {
				result->string_value = arr->string_values[ this->index ];
				result->string_value->reference_count++;
			}
		}
		dispose_temporary( &obj );
	}
	return ret;
}

static void expr_set_line( struct expression *expr, int line ) {
	while( expr ) {
		expr->line = line;
		expr_set_line( expr->parameters, line );
		expr = expr->next;
	}
}

static enum result evaluate_array_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression prev, *param = this->parameters, *expr = param->next;
	int idx, len, buf = this->index == 'B';
	struct variable var = { 0, NULL };
	struct structure *struc = NULL;
	char msg[ 128 ] = "";
	struct array *arr;
	enum result ret = param->evaluate( param, vars, &var );
	prev.next = NULL;
	if( ret ) {
		if( var.string_value ) {
			if( var.string_value->type == ELEMENT ) {
				if( expr == NULL ) {
					parse_expressions( ( struct element * ) var.string_value, vars->func, NULL, 0, &prev, &len, msg );
					if( msg[ 0 ] == 0 ) {
						expr = prev.next;
						if( param->evaluate != evaluate_string_literal_expression ) {
							expr_set_line( expr, this->line );
						}
					} else {
						ret = throw( vars, this, 0, msg );
					}
				} else {
					ret = throw( vars, expr, 0, "Unexpected expression." );
				}
			} else if( var.string_value->type == STRUCT && !buf ) {
				struc = ( struct structure * ) var.string_value;
				len = struc->length;
			} else {
				ret = to_int( &var, &len, vars, param );
			}
		} else {
			len = var.number_value;
		}
	}
	if( ret ) {
		if( len >= 0 ) {
			if( buf ) {
				arr = new_buffer( len );
			} else {
				arr = new_array( vars->func->env, len, 0 );
				if( arr ) {
					if( struc ) {
						arr->str.string = struc->instance_name;
						arr->str.length = struc->instance_name_len;
					} else {
						arr->str.string = "[Array]";
						arr->str.length = 7;
					}
				}
			}
			if( arr ) {
				idx = 0;
				while( expr && ret ) {
					if( idx < len ) {
						dispose_variable( &var );
						ret = expr->evaluate( expr, vars, &var );
						if( ret ) {
							ret = assign_array_variable( &var, arr, idx++, vars, expr );
						}
					} else {
						ret = throw( vars, this, idx, "Array index out of bounds." );
					}
					expr = expr->next;
				}
				if( ret ) {
					arr->structure = struc;
					result->string_value = &arr->str;
				} else {
					unref_string( &arr->str );
				}
			} else {
				ret = throw_out_of_memory( vars, this );
			}
		} else {
			ret = throw( vars, this, var.number_value, "Invalid array length." );
		}
	}
	dispose_temporary( &var );
	if( prev.next ) {
		dispose_expressions( prev.next );
	}
	return ret;
}

enum result evaluate_index_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int idx;
	struct array *arr;
	enum result ret = OKAY;
	struct expression *parameter = this->parameters;
	struct variable src = { 0, NULL };
	if( parameter->evaluate == evaluate_local ) {
		arr = ( struct array * ) vars->locals[ parameter->index ].string_value;
	} else {
		ret = parameter->evaluate( parameter, vars, &src );
		arr = ( struct array * ) src.string_value;
	}
	if( ret ) {
		if( arr && arr->str.type == ARRAY ) {
			ret = evaluate_integer( parameter->next, vars, &idx );
			if( ret ) {
				if( ( unsigned int ) idx < ( unsigned int ) arr->length ) {
					result->number_value = arr->number_values[ idx ];
					if( arr->string_values && arr->string_values[ idx ] ) {
						result->string_value = arr->string_values[ idx ];
						result->string_value->reference_count++;
					}
				} else {
					ret = throw( vars, this, idx, "Array index out of bounds." );
				}
			}
		} else {
			ret = throw( vars, this, 0, "Not an array." );
		}
		dispose_temporary( &src );
	}
	return ret;
}

static struct string* get_decl( struct string_list *list, char *name, int len, enum reference_type type ) {
	while( list && ( list->str->length != len || strncmp( list->str->string, name, len ) ) ) {
		list = list->next;
	}
	if( list && ( !type || ( enum reference_type ) list->str->type == type ) ) {
		return list->str;
	}
	return NULL;
}

static struct string* get_decl_indexed( struct function *func, char *name, int len, enum reference_type type ) {
	return get_decl( func->env->decls_index[ hash_code( name, 0 ) ], name, len, type );
}

static struct string* find_decl( struct function *func, char *name, enum reference_type type, char *terminators ) {
	char qname[ 128 ];
	struct string *str;
	struct string_list *imports = NULL;
	int qlen, flen = terminators ? field_end( name, terminators ) - name : strlen( name );
	if( func->file->type == SOURCE ) {
		imports = ( ( struct source * ) func->file )->imports;
	}
	str = get_decl( func->env->decls_index[ hash_code( name, name[ flen ] ) ], name, flen, type );
	while( str == NULL && imports ) {
		qlen = imports->str->length;
		if( qlen + 1 + flen < 128 ) {
			strcpy( qname, imports->str->string );
			qname[ qlen++ ] = '_';
			memcpy( &qname[ qlen ], name, sizeof( char ) * flen );
			qlen += flen;
			qname[ qlen ] = 0;
			str = get_decl( func->env->decls_index[ hash_code( qname, 0 ) ], qname, qlen, type );
		}
		imports = imports->next;
	}
	if( name[ flen ] && str && str->type == FUNCTION && ( name[ flen + 1 ] || name[ flen ] != '!' ) ) {
		str = NULL;
	}
	return str;
}

static struct local_variable* get_local_variable( struct local_variable *locals, char *name, char *terminators ) {
	size_t len = field_end( name, terminators ) - name;
	while( locals && ( strncmp( locals->name, name, len ) || strlen( locals->name ) != len ) ) {
		locals = locals->next;
	}
	return locals;
}

static int add_global_constant( struct element *elem,
	struct function *func, struct structure *type, struct expression *initializer,
	struct statement *prev, char *message ) {
	struct global_variable *global = new_global_variable( elem->str.string, type, func, initializer, message );
	if( global ) {
		if( add_global( global, elem->line, func->env, message ) ) {
			global->str.type = CONST;
		}
		unref_string( &global->str );
	}
	return message[ 0 ] == 0;
}

static int add_global_variable( struct element *elem,
	struct function *func, struct structure *type, struct expression *initializer,
	struct statement *prev, char *message ) {
	struct global_variable *global = new_global_variable( elem->str.string, type, func, initializer, message );
	if( global ) {
		add_global( global, elem->line, func->env, message );
		unref_string( &global->str );
	}
	return message[ 0 ] == 0;
}

static int add_local_variable( struct element *elem,
	struct function *func, struct structure *type, struct expression *initializer,
	struct statement *prev, char *message ) {
	struct statement *stmt;
	struct local_variable *param = new_local_variable( func, elem, type, message );
	if( param ) {
		/*printf("Local variable '%s'\n", elem->str.string);*/
		if( get_local_variable( func->variable_decls, elem->str.string, "" ) == NULL ) {
			func->num_variables = func->num_variables + 1;
			if( func->variable_decls ) {
				func->variable_decls_tail->next = param;
			} else {
				func->variable_decls = param;
			}
			func->variable_decls_tail = param;
			if( initializer ) {
				stmt = calloc( 1, sizeof( struct statement ) );
				if( stmt ) {
					prev->head.next = ( struct expression * ) stmt;
					stmt->head.line = initializer->line;
					stmt->head.index = func->num_variables - 1;
					stmt->head.parameters = initializer;
					stmt->head.evaluate = execute_local_assignment;
				} else {
					strcpy( message, OUT_OF_MEMORY );
				}
			}
		} else {
			dispose_local_variables( param );
			sprintf( message, "Local variable '%.64s' already declared on line %d.", elem->str.string, elem->line );
		}
	}
	return message[ 0 ] == 0;
}

static struct element* parse_variable_declaration( struct element *elem, struct function *func, struct variables *vars, struct statement *prev,
	int (*add)( struct element *elem, struct function *func, struct structure *type, struct expression *initializer, struct statement *prev, char *message ),
	char *message ) {
	struct structure *type = NULL;
	struct expression expr = { 0 }, *array_expr;
	struct element *next;
	while( elem && elem->str.string[ 0 ] != ';' && elem->str.string[ 0 ] != '{' && message[ 0 ] == 0 ) {
		if( prev && prev->head.next ) {
			prev = ( struct statement * ) prev->head.next;
		}
		if( elem->str.string[ 0 ] == '(' ) {
			type = ( struct structure * ) find_decl( func, elem->child->str.string, STRUCT, NULL );
			if( type ) {
				elem = elem->next;
			} else {
				sprintf( message, "Structure '%.64s' not declared on line %d.", elem->child->str.string, elem->child->line );
			}
		}
		if( message[ 0 ] == 0 ) {
			if( elem->str.string[ 0 ] == '[' ) {
				parse_expression( elem->child->next, func, vars, &expr, message );
				if( message[ 0 ] == 0 ) {
					array_expr = calloc( 1, sizeof( struct expression ) );
					if( array_expr ) {
						array_expr->line = elem->line;
						array_expr->parameters = expr.next;
						array_expr->evaluate = evaluate_array_expression;
						if( add( elem->child, func, type, array_expr, prev, message ) ) {
							elem = elem->next;
						} else {
							dispose_expressions( array_expr );
						}
					} else {
						strcpy( message, OUT_OF_MEMORY );
					}
				}
			} else if( elem->next->str.string[ 0 ] == '=' ) {
				next = parse_expression( elem->next->next, func, vars, &expr, message );
				if( message[ 0 ] == 0 ) {
					if( add( elem, func, type, expr.next, prev, message ) ) {
						elem = next;
					} else {
						dispose_expressions( expr.next );
					}
				}
			} else {
				if( add( elem, func, type, NULL, prev, message ) ) {
					elem = elem->next;
				}
			}
		}
		if( elem && elem->str.string[ 0 ] == ',' && message[ 0 ] == 0 ) {
			elem = elem->next;
		}
	}
	if( elem && elem->str.string[ 0 ] == ';' && message[ 0 ] == 0 ) {
		elem = elem->next;
	}
	return elem;
}

static struct element* parse_comment( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	return elem->next->next;
}

static struct element* parse_const_declaration( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	return parse_variable_declaration( elem->next, func, vars, prev, add_global_constant, message);
}

static struct element* parse_global_declaration( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	return parse_variable_declaration( elem->next, func, vars, prev, add_global_variable, message);
}

static struct element* parse_local_declaration( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	return parse_variable_declaration( elem->next, func, vars, prev, add_local_variable, message);
}

enum result evaluate_unary_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable var = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &var );
	if( ret ) {
#if defined( FLOATING_POINT )
		switch( this->index ) {
			case 1: result->number_value = ~( ( long_int ) var.number_value ); break;
			case 2: result->number_value = log( var.number_value ); break;
			case 3: result->number_value = exp( var.number_value ); break;
			case 4: result->number_value = sqrt( var.number_value ); break;
			case 5: result->number_value = sin( var.number_value ); break;
			case 6: result->number_value = cos( var.number_value ); break;
			case 7: result->number_value = tan( var.number_value ); break;
			case 8: result->number_value = asin( var.number_value ); break;
			case 9: result->number_value = acos( var.number_value ); break;
			case 10:result->number_value = atan( var.number_value ); break;
		}
#else
		result->number_value = ~var.number_value;
#endif
		dispose_temporary( &var );
	}
	return ret;
}

enum result evaluate_logical_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int value;
	struct variable var = { 0, NULL };
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, vars, &var );
	if( ret ) {
		value = var.number_value || var.string_value;
		dispose_variable( &var );
		if( this->index == '!' ) {
			value = !value;
		} else {
			while( parameter->next ) {
				parameter = parameter->next;
				if( this->index == '&' ) {
					if( value ) {
						ret = parameter->evaluate( parameter, vars, &var );
						if( ret ) {
							value = var.number_value || var.string_value;
							dispose_variable( &var );
						} else {
							return ret;
						}
					} else {
						break;
					}
				} else {
					if( value ) {
						break;
					} else {
						ret = parameter->evaluate( parameter, vars, &var );
						if( ret ) {
							value = var.number_value || var.string_value;
							dispose_temporary( &var );
						} else {
							return ret;
						}
					}
				}
			}
		}
		result->number_value = value;
	}
	return ret;
}

static enum result evaluate_ternary_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable condition = { 0, NULL };
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, vars, &condition );
	if( ret ) {
		parameter = parameter->next;
		if( !( condition.number_value || condition.string_value ) ) {
			parameter = parameter->next;
		}
		ret = parameter->evaluate( parameter, vars, result );
		dispose_temporary( &condition );
	}
	return ret;
}

enum result evaluate_arithmetic_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	number lhs, rhs;
	if( !evaluate_number( parameter, vars, &lhs ) ) {
		return EXCEPTION;
	}
	while( parameter->next ) {
		parameter = parameter->next;
		if( !evaluate_number( parameter, vars, &rhs ) ) {
			return EXCEPTION;
		}
		switch( this->index ) {
			case '!': lhs = lhs != rhs; break;
			case '%':
				if( rhs != 0 ) {
					lhs = ( long_int ) lhs % ( long_int ) rhs;
				} else {
					return throw( vars, this, 0, "Modulo division by zero." );
				}
				break;
			case '&': lhs = ( long_int ) lhs & ( long_int ) rhs; break;
			case '(': lhs = lhs <= rhs; break;
			case ')': lhs = lhs >= rhs; break;
			case '*': lhs = lhs  * rhs; break;
			case '+': lhs = lhs  + rhs; break;
			case '-': lhs = lhs  - rhs; break;
			case '/':
				if( rhs != 0 ) {
					lhs = ( long_int ) lhs / ( long_int ) rhs;
				} else {
					return throw( vars, this, 0, "Integer division by zero." );
				}
				break;
			case '0': lhs = lhs / rhs; break;
			case '1': lhs = ( long_int ) lhs << ( long_int ) rhs; break;
			case '2': lhs = ( long_int ) lhs >> ( long_int ) rhs; break;
			case '3': lhs = ( long_int ) lhs  ^ ( long_int ) rhs; break;
			case ':': lhs = ( long_int ) lhs  | ( long_int ) rhs; break;
			case '<': lhs = lhs  < rhs; break;
			case '=': lhs = lhs == rhs; break;
			case '>': lhs = lhs  > rhs; break;
			default :
				return throw( vars, this, 0, "Unhandled integer operator." );
		}
	}
	result->number_value = lhs;
	return OKAY;
}

int parse_number( char *str, number *result ) {
	char *end;
	number value;
#if defined( FLOATING_POINT )
	if( str[ 0 ] != '0' || str[ 1 ] == '.' ) {
		errno = 0;
		value = strtod( str, &end );
		if( !( *end || errno ) ) {
			*result = value;
			return 1;
		}
	}
#endif
	errno = 0;
	value = strtoul( str, &end, 0 );
	if( !( *end || errno ) ) {
		*result = value;
		return 1;
	}
	return 0;
}

static enum result evaluate_num_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable str = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &str );
	if( ret ) {
		if( str.string_value ) {
			if( str.string_value->type > ELEMENT ) {
				ret = to_num( &str, &str.number_value, vars, this->parameters );
			} else if( !( parse_number( str.string_value->string, &str.number_value ) ) ) {
				ret = throw( vars, this, 0, "Unable to convert string to number." );
			}
			dispose_temporary( &str );
		}
		if( ret ) {
			result->number_value = str.number_value;
		}
	}
	return ret;
}

static enum result evaluate_int_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable str = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &str );
	if( ret ) {
		if( str.string_value ) {
			if( str.string_value->type > ELEMENT ) {
				ret = to_num( &str, &str.number_value, vars, this->parameters );
			} else if( !parse_number( str.string_value->string, &str.number_value ) ) {
				ret = throw( vars, this, 0, "Unable to convert string to integer." );
			}
			dispose_temporary( &str );
		}
		if( ret ) {
			result->number_value = ( long_int ) str.number_value;
		}
	}
	return ret;
}

static enum result evaluate_str_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int str_len = 0, len;
	char num[ 32 ], *val;
	enum result ret = OKAY;
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct string *str = NULL, *new;
	while( parameter && ret ) {
		ret = parameter->evaluate( parameter, vars, &var );
		if( ret ) {
			if( var.string_value ) {
				if( var.string_value->type == CUSTOM && ( ( struct custom * ) var.string_value )->type->to_str ) {
					ret = ( ( struct custom * ) var.string_value )->type->to_str( &var, &new, vars, parameter );
					dispose_temporary( &var );
					if( ret ) {
						var.string_value = new;
					} else {
						break;
					}
				}
				len = var.string_value->length;
				val = var.string_value->string;
			} else {
#if defined( FLOATING_POINT )
				sprintf( num, "%.16g", var.number_value );
#else
				sprintf( num, "%d", var.number_value );
#endif
				len = strlen( num );
				val = num;
			}
			if( str == NULL && var.string_value && var.string_value->type == STRING ) {
				str = var.string_value;
				var.string_value = NULL;
				var.number_value = 0;
				str_len = len;
			} else {
				if( MAX_INTEGER - len > str_len ) {
					new = new_string( str_len + len );
					if( new ) {
						if( str ) {
							memcpy( new->string, str->string, sizeof( char ) * str_len );
							unref_string( str );
						}
						memcpy( &new->string[ str_len ], val, sizeof( char ) * len );
						str_len += len;
						str = new;
					} else {
						ret = throw_out_of_memory( vars, this );
					}
				} else {
					ret = throw( vars, this, len, "String too large." );
				}
				dispose_variable( &var );
			}
			parameter = parameter->next;
		}
	}
	if( ret ) {
		result->string_value = str;
	} else if( str ) {
		unref_string( str );
	}
	return ret;
}

static enum result evaluate_asc_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct string *str;
	int val;
	enum result ret = evaluate_integer( this->parameters, vars, &val );
	if( ret ) {
		str = new_string( 1 );
		if( str ) {
			str->string[ 0 ] = val;
			result->string_value = str;
		} else {
			ret = throw_out_of_memory( vars, this );
		}
	}
	return ret;
}

static enum result evaluate_len_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	enum result ret = OKAY;
	struct string *str;
	if( parameter->evaluate == evaluate_local ) {
		str = vars->locals[ parameter->index ].string_value;
	} else {
		ret = parameter->evaluate( parameter, vars, &var );
		str = var.string_value;
	}
	if( ret ) {
		if( str ) {
			switch( str->type ) {
				default:
				case STRING:
					result->number_value = str->length;
					break;
				case ARRAY:
					result->number_value = ( ( struct array * ) str )->length;
					break;
				case STRUCT:
					result->number_value = ( ( struct structure * ) str )->length;
					break;
			}
		} else {
			ret = throw( vars, this, 0, "Not a string or array." );
		}
		dispose_temporary( &var );
	}
	return ret;
}

static enum result evaluate_tup_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable str = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, vars, &str );
	if( ret ) {
		parameter = parameter->next;
		if( str.string_value ) {
			ret = evaluate_number( parameter, vars, &str.number_value );
			if( ret ) {
				result->number_value = str.number_value;
				result->string_value = str.string_value;
			} else {
				dispose_temporary( &str );
			}
		} else {
			str.number_value = 0;
			ret = parameter->evaluate( parameter, vars, &str );
			if( ret ) {
				result->number_value = str.number_value;
			}
			dispose_temporary( &str );
		}
	}
	return ret;
}

static enum result evaluate_read_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int count;
	char message[ 64 ];
	struct string *str;
	enum result ret = evaluate_integer( this->parameters, vars, &count );
	if( ret ) {
		if( count >= 0 ) {
			str = new_string( count );
			if( str ) {
				clearerr( stdin );
				if( count > 0 ) {
					count = fread( str->string, 1, count, stdin );
					str->string[ count ] = 0;
					str->length = count;
				}
				if( ferror( stdin ) ) {
					unref_string( str );
					strncpy( message, strerror( errno ), 63 );
					message[ 63 ] = 0;
					ret = throw( vars, this, 0, message );
				} else {
					result->string_value = str;
				}
			} else {
				ret = throw_out_of_memory( vars, this );
			}
		} else {
			ret = throw( vars, this, count, "Invalid length." );
		}
	}
	return ret;
}

static enum result evaluate_load_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	long len;
	char message[ 64 ];
	struct string *str;
	struct variable file = { 0 };
	int offset = 0, count = -1;
	struct expression *parameter = this->parameters;
	enum result ret = evaluate_string( parameter, vars, &file );
	if( ret ) {
		parameter = parameter->next;
		if( parameter ) {
			ret = evaluate_integer( parameter, vars, &offset );
			parameter = parameter->next;
		}
		if( ret && parameter ) {
			ret = evaluate_integer( parameter, vars, &count );
			if( ret && parameter->next ) {
				ret = throw( vars, this, 0, "Too many parameters." );
			}
		}
		if( ret ) {
			len = load_file( file.string_value->string, NULL, offset, 0, message );
			if( len >= 0 ) {
				if( count >= 0 && count < len ) {
					len = count;
				}
				if( len < MAX_INTEGER ) {
					str = new_string( len );
					if( str ) {
						len = load_file( file.string_value->string, str->string, offset, len, message );
						if( len >= 0 ) {
							str->length = len;
							str->string[ len ] = 0;
							result->string_value = str;
						} else {
							unref_string( str );
							ret = throw( vars, this, 0, message );
						}
					} else {
						ret = throw_out_of_memory( vars, this );
					}
				} else {
					ret = throw( vars, this, 0, "File too large." );
				}
			} else {
				ret = throw( vars, this, 0, message );
			}
		}
		dispose_temporary( &file );
	}
	return ret;
}

static enum result evaluate_flen_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	long len;
	char message[ 64 ];
	struct variable file = { 0, NULL };
	enum result ret = evaluate_string( this->parameters, vars, &file );
	if( ret ) {
		len = load_file( file.string_value->string, NULL, 0, 0, message );
		if( len >= 0 ) {
			if( len < MAX_INTEGER ) {
				result->number_value = len;
			} else {
				ret = throw( vars, this, 0, "File too large." );
			}
		} else {
			ret = throw( vars, this, 0, message );
		}
		dispose_temporary( &file );
	}
	return ret;
}

static enum result evaluate_cmp_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable var1 = { 0, NULL }, var2 = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, vars, &var1 );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, vars, &var2 );
		if( ret ) {
			result->number_value = compare_variables( &var1, &var2 );
			dispose_temporary( &var2 );
		}
		dispose_temporary( &var1 );
	}
	return ret;
}

enum result evaluate_chr_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable src = { 0, NULL };
	enum result ret = OKAY;
	int idx;
	struct string *str;
	if( parameter->evaluate == evaluate_local ) {
		str = vars->locals[ parameter->index ].string_value;
	} else {
		ret = evaluate_string( parameter, vars, &src );
		str = src.string_value;
	}
	if( str ) {
		ret = evaluate_integer( parameter->next, vars, &idx );
		if( ret ) {
			if( ( unsigned int ) idx < ( unsigned int ) str->length ) {
				result->number_value = ( signed char ) str->string[ idx ];
			} else {
				ret = throw( vars, this, idx, "String index out of bounds." );
			}
		}
		dispose_temporary( &src );
	} else if( ret == OKAY ) {
		ret = evaluate_string( parameter, vars, &src );
	}
	return ret;
}

static enum result evaluate_sub_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	char *data;
	number *arr;
	struct string *str;
	struct variable var = { 0, NULL };
	int offset, length, idx, len;
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, vars, &var );
	if( ret ) {
		parameter = parameter->next;
		ret = evaluate_integer( parameter, vars, &idx );
		if( ret ) {
			parameter = parameter->next;
			ret = evaluate_integer( parameter, vars, &len );
			if( ret ) {
				if( var.string_value ) {
					if( var.string_value->type == ARRAY ) {
						length = ( ( struct array * ) var.string_value )->length;
					} else {
						length = var.string_value->length;
					}
					if( idx >= 0 && len >= 0 && MAX_INTEGER - len >= idx && idx + len <= length ) {
						str = new_string( len );
						if( str ) {
							if( var.string_value->type == ARRAY ) {
								data = str->string;
								arr = ( ( struct array * ) var.string_value )->number_values;
								offset = 0;
								while( offset < len ) {
									data[ offset ] = arr[ offset + idx ];
									offset++;
								}
							} else {
								memcpy( str->string, &var.string_value->string[ idx ], sizeof( char ) * len );
							}
							result->string_value = str;
						} else {
							ret = throw_out_of_memory( vars, this );
						}
					} else {
						ret = throw( vars, this, idx, "Range out of bounds." );
					}
				} else {
					ret = throw( vars, this, 0, "Not a string or array." );
				}
			}
		}
		dispose_temporary( &var );
	}
	return ret;
}

static struct element* new_number_element( number value ) {
	char chars[ 32 ];
	struct element *elem;
#if defined( FLOATING_POINT )
	sprintf( chars, "%.16g", value );
#else
	sprintf( chars, "%d", value );
#endif
	elem = new_element( strlen( chars ) );
	if( elem ) {
		strcpy( elem->str.string, chars );
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

static struct element* new_tuple_element( struct string *string_value, number number_value,
	struct environment *env, int max_depth, char *message ) {
	struct element *elem = new_element( 4 ), *child = NULL;
	if( elem ) {
		strcpy( elem->str.string, "$tup" );
		elem->next = new_element( 2 );
		if( elem->next ) {
			strcpy( elem->next->str.string, "()" );
			child = value_to_element( 0, string_value, env, max_depth - 1, message );
			if( child ) {
				elem->next->child = child;
				while( child->next ) {
					child = child->next;
				}
				child->next = new_number_element( number_value );
				child = child->next;
			}
		}
		if( child == NULL ) {
			unref_string( &elem->str );
			elem = NULL;
		}
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
		} else {
			unref_string( &elem->str );
			elem = NULL;
		}
	}
	return elem;
}

static struct element* new_array_element( struct array *arr, struct environment *env, int max_depth, char *message ) {
	struct element *tail = NULL, *elem = new_element( 7 );
	int idx = 0, count = 0;
	struct string *str = NULL;
	if( elem ) {
		if( arr->structure ) {
			strcpy( elem->str.string, "$new" );
			elem->str.length = 4;
		} else if( arr->string_values ) {
			strcpy( elem->str.string, "$array" );
			elem->str.length = 6;
		} else {
			strcpy( elem->str.string, "$buffer" );
		}
		elem->next = new_element( 2 );
		if( elem->next ) {
			strcpy( elem->next->str.string, "()" );
			if( arr->structure ) {
				elem->next->child = new_element( arr->structure->str.length );
				if( elem->next->child ) {
					strcpy( elem->next->child->str.string, arr->structure->str.string );
				}
			} else {
				elem->next->child = new_number_element( arr->length );
			}
			while( idx < arr->length ) {
				if( arr->number_values[ idx ] || ( arr->string_values && arr->string_values[ idx ] ) ) {
					count = idx + 1;
				}
				idx++;
			}
			idx = 0;
			tail = elem->next->child;
			while( tail && idx < count ) {
				while( tail->next ) {
					tail = tail->next;
				}
				if( arr->string_values ) {
					str = arr->string_values[ idx ];
				}
				tail->next = value_to_element( arr->number_values[ idx++ ], str, env, max_depth - 1, message );
				tail = tail->next;
			}
		}
		if( tail == NULL ) { 
			unref_string( &elem->str );
			if( message[ 0 ] == 0 ) {
				strcpy( message, OUT_OF_MEMORY );
			}
			elem = NULL;
		}
	}
	return elem;
}

static struct element* value_to_element( number number_value, struct string *string_value,
	struct environment *env, int max_depth, char *message ) {
	struct element *elem = NULL;
	if( max_depth < 1 ) {
		strcpy( message, "Maximum element depth exceeded." );
	} else if( string_value ) {
		if( number_value ) {
			elem = new_tuple_element( string_value, number_value, env, max_depth, message );
		} else {
			switch( string_value->type ) {
				case STRING:
					elem = new_string_element( string_value );
					break;
				case ELEMENT:
					elem = new_literal_element( ( struct element * ) string_value );
					break;
				case ARRAY:
					elem = new_array_element( ( struct array * ) string_value, env, max_depth, message );
					break;
				case STRUCT:
					elem = new_element( string_value->length );
					if( elem ) {
						strcpy( elem->str.string, string_value->string );
					}
					break;
				case FUNCTION:
					if( get_decl( env->decls_index[ hash_code( string_value->string, 0 ) ],
					string_value->string, string_value->length, FUNCTION ) ) {
						elem = new_element( string_value->length + 1 );
						if( elem ) {
							elem->str.string[ 0 ] = '@';
							strcpy( &elem->str.string[ 1 ], string_value->string );
						}
					} else {
						elem = new_string_element( string_value );
					}
					break;
				default:
					strcpy( message, "Unsupported reference type." );
					break;
			}
		}
	} else {
		elem = new_number_element( number_value );
	}
	if( elem == NULL && message[ 0 ] == 0 ) {
		strcpy( message, OUT_OF_MEMORY );
	}
	return elem;
}

/* Throw an exception for the specified expression to indicate a failure to allocate memory. */
enum result throw_out_of_memory( struct variables *vars, struct expression *source ) {
	return throw( vars, source, 0, OUT_OF_MEMORY );
}

/* Throw an exception for the specified expression to indicate a stack-overflow. */
enum result throw_stack_overflow( struct variables *vars, struct expression *source ) {
	return throw( vars, source, 0, "Stack overflow." );
}

static enum result evaluate_expr_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct environment *env = vars->func->env;
	struct variable var = { 0, NULL };
	struct element *elem;
	char msg[ 128 ] = "";
	enum result ret = this->parameters->evaluate( this->parameters, vars, &var );
	if( ret ) {
		if( ( size_t ) &ret < env->stack_limit ) {
			ret = throw_stack_overflow( vars, this );
		} else {
			elem = value_to_element( var.number_value, var.string_value, env, env->element_depth, msg );
			if( elem ) {
				result->string_value = &elem->str;
			} else {
				ret = throw( vars, this, 0, msg );
			}
		}
		dispose_temporary( &var );
	}
	return ret;
}

static int check_element_depth( struct element *elem, int max_depth ) {
	max_depth--;
	while( elem ) {
		if( elem->child && ( max_depth < 0 || !check_element_depth( elem->child, max_depth ) ) ) {
			return 0;
		}
		elem = elem->next;
	}
	return 1;
}

static enum result evaluate_eval_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct environment *env = vars->func->env;
	struct variable var = { 0, NULL };
	struct expression prev;
	struct element *elem;
	char msg[ 128 ] = "";
	enum result ret = evaluate_element( this->parameters, vars, &var, 0 );
	if( ret ) {
		elem = ( struct element * ) var.string_value;
		if( ( size_t ) &ret < env->stack_limit || ( elem->line < 1 && !check_element_depth( elem, env->element_depth ) ) ) {
			ret = throw_stack_overflow( vars, this );
		} else {
			prev.next = NULL;
			parse_expression( elem, vars->func, NULL, &prev, msg );
			if( msg[ 0 ] == 0 ) {
				expr_set_line( prev.next, this->line );
				ret = prev.next->evaluate( prev.next, vars, result );
			} else {
				ret = throw( vars, this, 0, msg );
			}
			dispose_expressions( prev.next );
		}
		dispose_temporary( &var );
	}
	return ret;
}

static enum result evaluate_argc_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	result->number_value = vars->func->env->argc;
	return OKAY;
}

static enum result evaluate_argv_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int idx;
	char *val;
	struct string *str;
	enum result ret = evaluate_integer( this->parameters, vars, &idx );
	if( ret ) {
		if( idx >= 0 && idx < vars->func->env->argc ) {
			val = vars->func->env->argv[ idx ];
			str = new_string( strlen( val ) );
			if( str ) {
				memcpy( str->string, val, sizeof( char ) * str->length );
				result->string_value = str;
			} else {
				ret = throw_out_of_memory( vars, this );
			}
		} else {
			ret = throw( vars, this, idx, "Command-line argument index out of bounds." );
		}
	}
	return ret;
}

static enum result evaluate_time_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	enum result ret = OKAY;
	time_t seconds = time( NULL );
	struct string *str = NULL;
	char *time_str;
	if( vars->func->env->worker == NULL ) {
		time_str = ctime( &seconds );
		str = new_string( strlen( time_str ) - 1 );
		if( str ) {
			memcpy( str->string, time_str, sizeof( char ) * str->length );
		} else {
			ret = throw_out_of_memory( vars, this );
		}
	}
	if( ret ) {
		result->number_value = seconds;
		result->string_value = str;
	}
	return ret;
}

static enum result evaluate_next_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable prev = { 0, NULL };
	struct element *next;
	enum result ret = evaluate_element( parameter, vars, &prev, 0 );
	if( ret ) {
		next = ( ( struct element * ) prev.string_value )->next;
		if( next ) {
			result->string_value = &next->str;
			next->str.reference_count++;
		}
		dispose_temporary( &prev );
	}
	return ret;
}

static enum result evaluate_child_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable parent = { 0, NULL };
	struct element *child;
	enum result ret = evaluate_element( parameter, vars, &parent, 0 );
	if( ret ) {
		child = ( ( struct element * ) parent.string_value )->child;
		if( child ) {
			result->string_value = &child->str;
			child->str.reference_count++;
		}
		dispose_temporary( &parent );
	}
	return ret;
}

static enum result evaluate_elem_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable elem = { 0, NULL }, child = { 0, NULL }, next = { 0, NULL };
	struct element *value;
	enum result ret = evaluate_element( parameter, vars, &elem, 0 );
	if( ret ) {
		parameter = parameter->next;
		ret = evaluate_element( parameter, vars, &child, 1 );
		if( ret ) {
			if( !child.string_value || strchr( "([{", elem.string_value->string[ 0 ] ) ) {
				parameter = parameter->next;
				ret = evaluate_element( parameter, vars, &next, 1 );
				if( ret ) {
					if( elem.string_value->reference_count > 1 ) {
						value = new_element( elem.string_value->length );
						if( value ) {
							memcpy( value->str.string, elem.string_value->string,
								sizeof( char ) * elem.string_value->length );
						} else {
							ret = throw_out_of_memory( vars, this );
						}
					} else { /* Re-use element. */
						value = ( struct element * ) elem.string_value;
						value->line = 0;
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
						result->string_value = &value->str;
					}
					dispose_temporary( &next );
				}
			} else {
				ret = throw( vars, this, 0, "Parent elements must have value '()', '[]' or '{}'." );
			}
			dispose_temporary( &child );
		}
		dispose_temporary( &elem );
	}
	return ret;
}

static enum result evaluate_parse_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable str = { 0, NULL };
	struct element *elem;
	char message[ 128 ] = "";
	enum result ret = evaluate_string( parameter, vars, &str );
	if( ret ) {
		elem = parse_element( str.string_value->string, vars->func->env->element_depth, message );
		if( message[ 0 ] == 0 ) {
			result->string_value = &elem->str;
		} else {
			ret = throw( vars, this, 0, message );
		}
		dispose_temporary( &str );
	}
	return ret;
}

static enum result evaluate_unparse_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int len;
	struct string *str;
	struct element *elem;
	struct expression *parameter = this->parameters;
	struct environment *env = vars->func->env;
	struct variable var = { 0, NULL };
	enum result ret = evaluate_element( parameter, vars, &var, 0 );
	if( ret ) {
		elem = ( struct element * ) var.string_value;
		if( ( size_t ) &ret < env->stack_limit || ( elem->line < 1 && !check_element_depth( elem, env->element_depth ) ) ) {
			ret = throw_stack_overflow( vars, this );
		} else {
			len = write_element( elem, NULL );
			if( len >= 0 ) {
				str = new_string( len );
				if( str ) {
					write_element( elem, str->string );
					result->string_value = str;
				} else {
					ret = throw_out_of_memory( vars, this );
				}
			} else {
				ret = throw( vars, this, 0, "String too large." );
			}
		}
		dispose_temporary( &var );
	}
	return ret;
}

static enum result evaluate_quote_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int length;
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct string *str;
	enum result ret = evaluate_string( parameter, vars, &var );
	if( ret ) {
		length = write_byte_string( var.string_value->string, var.string_value->length, NULL );
		if( length >= 0 ) {
			str = new_string( length );
			if( str ) {
				write_byte_string( var.string_value->string, var.string_value->length, str->string );
				result->string_value = str;
			} else {
				ret = throw_out_of_memory( vars, this );
			}
		} else {
			ret = throw( vars, this, 0, "String too large." );
		}
		dispose_temporary( &var );
	}
	return ret;
}

static enum result evaluate_unquote_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct string *str;
	enum result ret = evaluate_string( parameter, vars, &var );
	if( ret ) {
		str = new_string_literal( var.string_value->string );
		if( str ) {
			result->string_value = str;
		} else {
			ret = throw_out_of_memory( vars, this );
		}
		dispose_temporary( &var );
	}
	return ret;
}

static enum result evaluate_line_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable elem = { 0, NULL };
	enum result ret = evaluate_element( parameter, vars, &elem, 0 );
	if( ret ) {
		result->number_value = ( ( struct element * ) elem.string_value )->line;
		dispose_temporary( &elem );
	}
	return ret;
}

static enum result evaluate_hex_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int len, val;
	struct string *str;
	enum result ret = evaluate_integer( this->parameters, vars, &val );
	if( ret ) {
		str = new_string( sizeof( int ) * 2 + 2 );
		if( str ) {
			len = sprintf( str->string, "0x%08X", val );
			if( len > 0 ) {
				str->length = len;
				result->string_value = str;
			} else {
				unref_string( str );
				ret = throw( vars, this, len, "Output error." );
			}
		} else {
			ret = throw_out_of_memory( vars, this );
		}
	}
	return ret;
}

static enum result evaluate_pack_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int idx, len;
	long_int in;
	number *src;
	char *out;
	struct string *str;
	struct variable val = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &val );
	if( ret ) {
		if( val.string_value && val.string_value && val.string_value->type == ARRAY ) {
			src = ( ( struct array * ) val.string_value )->number_values;
			len = ( ( struct array * ) val.string_value )->length * 4;
		} else {
			src = &val.number_value;
			ret = to_num( &val, src, vars, this->parameters );
			len = 4;
		}
		if( ret ) {
			str = new_string( len );
			if( str ) {
				idx = 0;
				out = str->string;
				while( idx < len ) {
					in = src[ idx >> 2 ];
					out[ idx ] = in >> 24;
					out[ idx + 1 ] = in >> 16;
					out[ idx + 2 ] = in >> 8;
					out[ idx + 3 ] = in;
					idx += 4;
				}
				out[ idx ] = 0;
				result->string_value = str;
			} else {
				ret = throw_out_of_memory( vars, this );
			}
		}
		dispose_temporary( &val );
	}
	return ret;
}

/* Unpack a 32-bit big-endian integer from str at the specified index. */
int unpack( char *str, int idx ) {
	idx = idx << 2;
	return ( ( signed char ) str[ idx ] << 24 ) | ( ( unsigned char ) str[ idx + 1 ] << 16 )
		| ( ( unsigned char ) str[ idx + 2 ] << 8 ) | ( unsigned char ) str[ idx + 3 ];
}

enum result evaluate_unpack_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable str = { 0, NULL };
	int idx;
	enum result ret = evaluate_string( parameter, vars, &str );
	if( ret ) {
		ret = evaluate_integer( parameter->next, vars, &idx );
		if( ret ) {
			if( idx >= 0 && idx * 4 < str.string_value->length - 3 ) {
				result->number_value = unpack( str.string_value->string, idx );
			} else {
				ret = throw( vars, this, idx, "String index out of bounds." );
			}
		}
		dispose_temporary( &str );
	}
	return ret;
}

static enum result evaluate_func_ref_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	result->string_value = &( ( struct function_expression * ) this )->function->str;
	result->string_value->reference_count++;
	return OKAY;
}

static enum result evaluate_same_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable lhs = { 0, NULL }, rhs = { 0, NULL };
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, vars, &lhs );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, vars, &rhs );
		if( ret ) {
			result->number_value = ( lhs.number_value == rhs.number_value )
				&& ( lhs.string_value == rhs.string_value );
			dispose_temporary( &rhs );
		}
		dispose_temporary( &lhs );
	}
	return ret;
}

static enum result evaluate_eq_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable lhs = { 0, NULL }, rhs = { 0, NULL };
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, vars, &lhs );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, vars, &rhs );
		if( ret ) {
			result->number_value = compare_variables( &lhs, &rhs ) == 0;
			dispose_temporary( &rhs );
		}
		dispose_temporary( &lhs );
	}
	return ret;
}

static enum result evaluate_stridx_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable str = { 0, NULL }, sep = { 0, NULL };
	int idx;
	enum result ret = evaluate_string( parameter, vars, &str );
	if( ret ) {
		parameter = parameter->next;
		ret = evaluate_string( parameter, vars, &sep );
		if( ret ) {
			parameter = parameter->next;
			if( parameter ) {
				ret = evaluate_integer( parameter, vars, &idx );
			} else {
				idx = -1;
			}
			if( ret ) {
				if( str.string_value->length > 0 && str.string_value->length > idx ) {
					result->number_value = str_idx( str.string_value->string, sep.string_value->string, idx );
				} else {
					ret = throw( vars, this, idx, "String index out of bounds." );
				}
			}
			dispose_temporary( &sep );
		}
		dispose_temporary( &str );
	}
	return ret;
}

static enum result evaluate_interrupted_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct environment *env = vars->func->env; 
	result->number_value = env->interrupted;
	if( env->worker == NULL ) {
		env->interrupted = 0;
	}
	return OKAY;
}

static enum result evaluate_source_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable var = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &var );
	if( ret ) {
		if( var.string_value && var.string_value->type == FUNCTION ) {
			result->string_value = ( ( struct function * ) var.string_value )->file;
			result->string_value->reference_count++;
		} else {
			ret = throw( vars, this, 0, "Not a function reference." );
		}
		dispose_temporary( &var );
	}
	return ret;
}

static struct operator* get_operator( char *name, struct operator *oper ) {
	while( oper && strcmp( oper->name, name ) ) {
		oper = oper->next;
	}
	return oper;
}

static struct element* parse_operator_expression( struct element *elem, struct operator *oper,
	struct function *func, struct variables *vars, struct expression *prev, char *message ) {
	int count;
	struct element *next = elem->next;
	struct expression param = { 0 }, *expr = calloc( 1, sizeof( struct expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		expr->index = oper->oper;
		expr->evaluate = oper->evaluate;
		if( oper->num_operands != 0 ) {
			if( next && next->str.string[ 0 ] == '(' ) {
				parse_expressions( next->child, func, vars, 0, &param, &count, message );
				expr->parameters = param.next;
				if( message[ 0 ] == 0 ) {
					if( count == oper->num_operands || ( oper->num_operands < 0 && count >= -oper->num_operands ) ) {
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
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_call_expression( struct element *elem,
	struct function *func, struct variables *vars, struct function *decl, struct expression *prev, char *message ) {
	int num_params;
	struct element *next = elem->next;
	struct expression param = { 0 }, *expr = calloc( 1, sizeof( struct function_expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		( ( struct function_expression * ) expr )->function = decl;
		if( next && next->str.string[ 0 ] == '(' ) {
			parse_expressions( next->child, func, vars, 0, &param, &num_params, message );
			expr->parameters = param.next;
			if( message[ 0 ] == 0 ) {
				if( num_params == decl->num_parameters ) {
					expr->evaluate = evaluate_call_expression;
					next = next->next;
				} else {
					sprintf( message, "Wrong number of arguments to '%.64s()' on line %d.", decl->str.string, next->line );
				}
			}
		} else {
			sprintf( message, "Expected '(' after function name on line %d.", elem->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_func_ref_expression( struct element *elem,
	struct function *func, struct expression *prev, char *message ) {
	char *name = &elem->str.string[ 1 ];
	struct function_expression *expr = calloc( 1, sizeof( struct function_expression ) );
	if( expr ) {
		prev->next = &expr->expr;
		expr->expr.line = elem->line;
		expr->function = ( struct function * ) find_decl( func, name, FUNCTION, NULL );
		if( expr->function ) {
			expr->expr.evaluate = evaluate_func_ref_expression;
		} else {
			sprintf( message, "Function '%.64s' not declared on line %d.", name, elem->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return elem->next;
}

static struct element* parse_index_expression( struct element *elem,
	struct function *func, struct variables *vars, struct expression *prev, char *message ) {
	int num_params;
	struct expression param = { 0 }, *expr = calloc( 1, sizeof( struct expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		parse_expressions( elem->child, func, vars, 0, &param, &num_params, message );
		expr->parameters = param.next;
		if( message[ 0 ] == 0 ) {
			if( num_params == 2 ) {
				expr->evaluate = evaluate_index_expression;
			} else {
				sprintf( message, "Invalid index expression on line %d.", elem->line );
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return elem->next;
}

static struct element* parse_struct_expression( struct element *elem,
	struct function *func, struct variables *vars, struct structure *struc, struct expression *prev, char *message ) {
	int idx, count;
	struct element *next = elem->next;
	char *field = field_end( elem->str.string, "!." );
	struct expression param = { 0 }, *expr = calloc( 1, sizeof( struct value_expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		( ( struct value_expression * ) expr )->str = &struc->str;
		if( field[ 0 ] == '!' && !field[ 1 ] ) {
			field++;
		}
		if( field[ 0 ] ) {
			idx = get_string_list_index( struc->fields, &field[ 1 ] );
			if( idx >= 0 ) {
				expr->index = idx;
				( ( struct value_expression * ) expr )->num = idx;
				if( next && next->str.string[ 0 ] == '(' ) {
					parse_expressions( next->child, func, vars, 0, &param, &count, message );
					expr->parameters = param.next;
					if( message[ 0 ] == 0 ) {
						if( count == 1 ) {
							expr->evaluate = evaluate_member_expression;
							next = next->next;
						} else {
							sprintf( message, "Wrong number of arguments to struct expression on line %d.", next->line );
						}
					}
				} else {
					expr->evaluate = evaluate_number_literal_expression;
				}
			} else {
				sprintf( message, "Field '%.64s' not declared on line %d.", elem->str.string, elem->line );
			}
		} else {
			idx = ARRAY + 1;
			expr->index = idx;
			( ( struct value_expression * ) expr )->num = idx;
			struc->str.reference_count++;
			expr->evaluate = evaluate_string_literal_expression;
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_refcall_expression( struct element *elem,
	struct function *func, struct variables *vars, struct expression *prev, char *message ) {
	int count;
	struct element *next = elem->next;
	struct expression param = { 0 }, *expr = calloc( 1, sizeof( struct expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		if( next && next->str.string[ 0 ] == '(' ) {
			parse_expressions( next->child, func, vars, 0, &param, &count, message );
			expr->parameters = param.next;
			if( message[ 0 ] == 0 ) {
				if( count > 0 ) {
					expr->index = count - 1;
					expr->evaluate = evaluate_refcall_expression;
					next = next->next;
				} else {
					sprintf( message, "Expected expression after '(' on line %d.", next->line );
				}
			}
		} else {
			sprintf( message, "Expected '(' after ':' on line %d.", elem->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_thiscall_expression( struct element *elem,
	struct function *func, struct variables *vars, struct expression *prev, char *message ) {
	char *field;
	int idx, count;
	struct variable *var;
	struct structure *struc = NULL;
	struct element *next = elem->next;
	struct global_variable *global = NULL;
	struct string *decl = find_decl( func, &elem->str.string[ 1 ], 0, "!." );
	struct local_variable *captured = NULL, *local = get_local_variable( func->variable_decls, &elem->str.string[ 1 ], "." );
	struct expression param = { 0 }, *this, *expr = calloc( 1, sizeof( struct value_expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		if( local ) {
			struc = local->type;
		} else {
			if( vars ) {
				captured = get_local_variable( vars->func->variable_decls, &elem->str.string[ 1 ], "." );
			}
			if( captured ) {
				struc = captured->type;
			} else if( decl ) {
				if( decl->type == GLOBAL ) {
					global = ( struct global_variable * ) decl;
					struc = global->type;
				} else if( decl->type == STRUCT ) {
					struc = ( struct structure * ) decl;
				}
			}
		}
		field = field_end( elem->str.string, "!." );
		if( struc && field[ 0 ] ) {
			( ( struct value_expression * ) expr )->str = &struc->str;
			idx = get_string_list_index( struc->fields, &field[ 1 ] );
			if( idx >= 0 ) {
				if( next && next->str.string[ 0 ] == '(' ) {
					parse_expressions( next->child, func, vars, 0, &param, &count, message );
					expr->parameters = param.next;
					if( local || captured || global ) {
						this = calloc( 1, sizeof( struct value_expression ) );
						if( this ) {
							if( local ) {
								this->index = local->index;
								this->evaluate = evaluate_local;
							} else if( captured ) {
								var = &vars->locals[ captured->index ];
								this->index = ( long_int ) var->number_value;
								( ( struct value_expression * ) this )->num = var->number_value;
								if( var->string_value ) {
									var->string_value->reference_count++;
									( ( struct value_expression * ) this )->str = var->string_value;
									this->evaluate = evaluate_string_literal_expression;
								} else {
									this->evaluate = evaluate_number_literal_expression;
								}
							} else {
								( ( struct value_expression * ) this )->str = &global->str;
								this->evaluate = evaluate_global;
							}
							this->next = expr->parameters;
							expr->parameters = this;
							count++;
						} else {
							strcpy( message, OUT_OF_MEMORY );
						}
					}
					if( message[ 0 ] == 0 ) {
						if( count > 0 && count < 256 ) {
							expr->index = ( ( idx & 0x7FFFFF ) << 8 ) | count;
							expr->evaluate = evaluate_thiscall_expression;
							next = next->next;
						} else {
							sprintf( message, "Expected expression after '(' on line %d.", next->line );
						}
					}
				} else {
					sprintf( message, "Expected '(' after '%.64s' on line %d.", elem->str.string, elem->line );
				}
			} else {
				sprintf( message, "Field '%.64s' not declared on line %d.", &elem->str.string[ 1 ], elem->line );
			}
		} else {
			sprintf( message, "Undeclared variable, structure or field '%.64s' on line %d.", elem->str.string, elem->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_member_call_expression( struct structure *struc, struct expression *this,
	char *memb, struct element *elem, struct function *func, struct variables *vars, struct expression *prev, char *message ) {
	char qname[ 128 ];
	int num_params, qlen, mlen = strlen( memb );
	struct expression *expr;
	struct function *decl = NULL;
	struct element *next = elem->next;
	if( struc ) {
		while( struc && decl == NULL ) {
			qlen = struc->str.length + 1 + mlen;
			if( qlen < 128 ) {
				strcpy( qname, struc->str.string );
				qname[ struc->str.length ] = '_';
				strcpy( &qname[ struc->str.length + 1 ], memb );
				decl = ( struct function * ) get_decl_indexed( func, qname, qlen, FUNCTION );
			}
			struc = struc->super;
		}
		if( decl ) {
			expr = calloc( 1, sizeof( struct function_expression ) );
			if( expr ) {
				prev->next = expr;
				expr->line = elem->line;
				expr->parameters = this;
				( ( struct function_expression * ) expr )->function = decl;
				if( next && next->str.string[ 0 ] == '(' ) {
					parse_expressions( next->child, func, vars, 0, this, &num_params, message );
					if( message[ 0 ] == 0 ) {
						if( num_params + 1 == ( ( struct function_expression * ) expr )->function->num_parameters ) {
							expr->evaluate = evaluate_call_expression;
							next = next->next;
						} else {
							sprintf( message, "Wrong number of arguments to '%.64s()' on line %d.", decl->str.string, next->line );
						}
					}
				} else {
					sprintf( message, "Expected '(' after function name on line %d.", elem->line );
				}
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		} else {
			sprintf( message, "Member function not found for expression '%.64s' on line %d.", elem->str.string, elem->line );
		}
	} else {
		sprintf( message, "Expression '%.64s' has no associated structure on line %d.", elem->str.string, elem->line );
	}
	return next;
}

static struct element* parse_member_expression( struct structure *struc, struct expression *this,
	char *memb, struct element *elem, struct function *func, struct variables *vars, struct expression *prev, char *message ) {
	struct element *next = elem->next;
	struct expression *expr;
	int idx;
	if( struc ) {
		idx = get_string_list_index( struc->fields, memb );
		if( idx >= 0 ) {
			expr = calloc( 1, sizeof( struct value_expression ) );
			if( expr ) {
				prev->next = expr;
				( ( struct value_expression * ) expr )->str = &struc->str;
				expr->index = idx;
				expr->line = elem->line;
				expr->parameters = this;
				expr->evaluate = evaluate_member_expression;
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		} else {
			sprintf( message, "Field not declared in expression '%.64s' on line %d.", elem->str.string, elem->line );
		}
	} else {
		sprintf( message, "Expression '%.64s' has no associated structure on line %d.", elem->str.string, elem->line );
	}
	return next;
}

static struct element* parse_local_expression( struct element *elem,
	struct function *func, struct variables *vars, struct local_variable *local, struct expression *prev, char *message ) {
	struct element *next = elem->next;
	char *field = &elem->str.string[ strlen( local->name ) ];
	struct expression *expr = calloc( 1, sizeof( struct expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		expr->index = local->index;
		expr->evaluate = evaluate_local;
		if( field[ 0 ] == '.' ) {
			next = parse_member_expression( local->type, expr, &field[ 1 ], elem, func, vars, prev, message );
		} else if( field[ 0 ] == ':' ) {
			next = parse_member_call_expression( local->type, expr, &field[ 1 ], elem, func, vars, prev, message );
		} else if( !strcmp( field, "++" ) ) {
			expr->evaluate = evaluate_local_post_inc;
		} else if( !strcmp( field, "--" ) ) {
			expr->evaluate = evaluate_local_post_dec;
		} else if( field[ 0 ] ) {
			sprintf( message, "Invalid local variable expression '%.64s' on line %d.", elem->str.string, elem->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_capture_expression( struct element *elem,
	struct function *func, struct variables *vars, struct local_variable *local, struct expression *prev, char *message ) {
	struct element *next = elem->next;
	struct variable *captured = &vars->locals[ local->index ];
	char *field = &elem->str.string[ strlen( local->name ) ];
	struct expression *expr = calloc( 1, sizeof( struct value_expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		expr->index = ( long_int ) captured->number_value;
		( ( struct value_expression * ) expr )->num = captured->number_value;
		if( captured->string_value ) {
			captured->string_value->reference_count++;
			( ( struct value_expression * ) expr )->str = captured->string_value;
			expr->evaluate = evaluate_string_literal_expression;
		} else {
			expr->evaluate = evaluate_number_literal_expression;
		}
		if( field[ 0 ] == '.' ) {
			next = parse_member_expression( local->type, expr, &field[ 1 ], elem, func, vars, prev, message );
		} else if( field[ 0 ] == ':' ) {
			next = parse_member_call_expression( local->type, expr, &field[ 1 ], elem, func, vars, prev, message );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_global_expression( struct element *elem, struct function *func,
	struct variables *vars, struct global_variable *global, struct expression *prev, char *message ) {
	struct element *next = elem->next;
	char *field = field_end( elem->str.string, "!.:" );
	struct expression *expr = calloc( 1, sizeof( struct value_expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		if( global->str.type == CONST && global->initializer && global->initializer->evaluate == evaluate_number_literal_expression ) {
			expr->index = global->initializer->index;
			( ( struct value_expression * ) expr )->num = ( ( struct value_expression * ) global->initializer )->num;
			expr->evaluate = evaluate_number_literal_expression;
		} else if( global->str.type == CONST && !( global->initializer || global->value.string_value ) ) {
			( ( struct value_expression * ) expr )->num = global->value.number_value;
			expr->evaluate = evaluate_number_literal_expression;
		} else {
			( ( struct value_expression * ) expr )->str = &global->str;
			expr->evaluate = evaluate_global;
		}
		if( field[ 0 ] == '.' || ( field[ 0 ] == '!' && field[ 1 ] && field[ 1 ] != ':' ) ) {
			next = parse_member_expression( global->type, expr, &field[ 1 ], elem, func, vars, prev, message );
		} else if( field[ 0 ] == ':' ) {
			next = parse_member_call_expression( global->type, expr, &field[ 1 ], elem, func, vars, prev, message );
		} else if( field[ 0 ] == '!' && field[ 1 ] == ':' ) {
			next = parse_member_call_expression( global->type, expr, &field[ 2 ], elem, func, vars, prev, message );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_number_literal_expression( struct element *elem, struct expression *prev, char *message ) {
	struct value_expression *expr = calloc( 1, sizeof( struct value_expression ) );
	if( expr ) {
		prev->next = &expr->expr;
		expr->expr.line = elem->line;
		if( parse_number( elem->str.string, &expr->num ) ) {
			expr->expr.index = ( long_int ) expr->num;
			expr->expr.evaluate = evaluate_number_literal_expression;
		} else {
			sprintf( message, "Invalid number literal '%.64s' on line %d.", elem->str.string, elem->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return elem->next;
}

static struct element* parse_character_literal_expression( struct element *elem, struct expression *prev, char *message ) {
	struct value_expression *expr = calloc( 1, sizeof( struct value_expression ) );
	char *str = elem->str.string, out[ 8 ], quote = str[ 0 ];
	int len = elem->str.length;
	if( expr ) {
		prev->next = &expr->expr;
		expr->expr.line = elem->line;
		if( len > 0 && len < 7 && str[ len - 1 ] == quote && unquote_string( str, out, quote ) == 1 ) {
			expr->num = out[ 0 ];
			expr->expr.index = out[ 0 ];
			expr->expr.evaluate = evaluate_number_literal_expression;
		} else {
			sprintf( message, "Invalid character literal '%.64s' on line %d.", elem->str.string, elem->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return elem->next;
}

static struct expression* new_string_literal_expression( number number_value,
	struct string *string_value, int line, char *message ) {
	struct expression *expr = calloc( 1, sizeof( struct value_expression ) );
	if( expr ) {
		expr->line = line;
		expr->index = ( long_int ) number_value;
		( ( struct value_expression * ) expr )->num = number_value;
		( ( struct value_expression * ) expr )->str = string_value;
		expr->evaluate = evaluate_string_literal_expression;
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return expr;
}

static struct element* parse_string_literal_expression( struct element *elem,
	struct expression *prev, char *message ) {
	struct string *value = new_string_literal( elem->str.string );
	if( value ) {
		prev->next = new_string_literal_expression( 0, value, elem->line, message );
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return elem->next;
}

static struct element* parse_element_literal_expression( struct element *elem,
	struct environment *env, struct expression *prev, char *message ) {
	struct element *child, *next = elem->next;
	if( next && next->str.string[ 0 ] == '{' ) {
		child = next->child;
		if( child ) {
			if( env->worker ) {
				child = copy_element( child );
				if( child == NULL ) {
					strcpy( message, OUT_OF_MEMORY );
				}
			} else {
				child->str.reference_count++;
			}
			if( child ) {
				prev->next = new_string_literal_expression( 0, &child->str, elem->line, message );
			}
		} else {
			prev->next = calloc( 1, sizeof( struct value_expression ) );
			if( prev->next ) {
				prev->next->line = elem->line;
				prev->next->evaluate = evaluate_number_literal_expression;
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		}
		next = next->next;
	} else {
		sprintf( message, "Expected '{' after '$' on line %d.", elem->line );
	}
	return next;
}

struct element* parse_expression( struct element *elem,
	struct function *func, struct variables *vars, struct expression *prev, char *message ) {
	struct element *next = elem->next;
	char *value = elem->str.string, chr = value[ 0 ];
	struct local_variable *local;
	enum reference_type type = 0;
	struct operator *oper;
	struct string *decl;
	if( ( chr >= '0' && chr <= '9' ) || ( chr == '-' && ( value[ 1 ] >= '0' && value[ 1 ] <= '9' ) ) ) {
		/* Number literal. */
		next = parse_number_literal_expression( elem, prev, message );
	} else if( chr == '"' ) {
		/* String literal. */
		next = parse_string_literal_expression( elem, prev, message );
	} else if( chr == '$' && value[ 1 ] == 0 ) {
		/* Element literal. */
		next = parse_element_literal_expression( elem, func->env, prev, message );
	} else if( ( chr == '`' || chr == '\'' ) && value[ 1 ] == 0 ) {
		/* Infix operator.*/
		next = parse_infix_expression( elem, func, vars, prev, message );
	} else if( chr == '\'' && value[ 1 ] ) {
		/* Character literal. */
		next = parse_character_literal_expression( elem, prev, message );
	} else if( chr == '\\' && value[ 1 ] == 0 && elem->next && elem->next->str.string[ 0 ] == '"' ) {
		/* Escaped character literal. */
		next = parse_character_literal_expression( elem->next, prev, message );
	} else if( chr == '[' ) {
		/* Array index operator. */
		next = parse_index_expression( elem, func, vars, prev, message );
	} else if( elem->str.length > 128 ) {
		sprintf( message, "Invalid expression '%.64s' on line %d.", value, elem->line );
	} else if( chr == '@' ) {
		if( value[ 1 ] ) {
			/* Function reference operator. */
			next = parse_func_ref_expression( elem, func, prev, message );
		} else {
			/* Recursive function call. */
			next = parse_call_expression( elem, func, vars, func, prev, message );
		}
	} else if( value[ 0 ] == ':' ) {
		if( value[ 1 ] == 0 ) {
			/* Function reference call. */
			next = parse_refcall_expression( elem, func, vars, prev, message );
		} else {
			/* Member function call. */
			next = parse_thiscall_expression( elem, func, vars, prev, message );
		}
	} else {
		local = get_local_variable( func->variable_decls, value, "+-.:" );
		if( local ) {
			/* Local variable reference.*/
			next = parse_local_expression( elem, func, vars, local, prev, message );
		} else {
			local = NULL;
			if( vars ) {
				local = get_local_variable( vars->func->variable_decls, value, ".:" );
			}
			if( local ) {
				/* Captured local variable. */
				next = parse_capture_expression( elem, func, vars, local, prev, message );
			} else {
				oper = get_operator( value, func->env->operators_index[ hash_code( value, 0 ) ] );
				if( oper ) {
					/* Operator. */
					next = parse_operator_expression( elem, oper, func, vars, prev, message );
				} else {
					decl = find_decl( func, value, 0, "!.:" );
					if( decl ) {
						type = decl->type;
					}
					switch( type ) {
						case CONST: case GLOBAL:
							/* Global variable reference.*/
							next = parse_global_expression( elem, func, vars, ( struct global_variable * ) decl, prev, message );
							break;
						case FUNCTION:
							/* Function call.*/
							next = parse_call_expression( elem, func, vars, ( struct function * ) decl, prev, message );
							break;
						case STRUCT:
							/* Structure. */
							next = parse_struct_expression( elem, func, vars, ( struct structure * ) decl, prev, message );
							break;
						default:
							sprintf( message, "Unhandled expression '%.64s' on line %d.", value, elem->line );
							break;
					}
				}
			}
		}
	}
	if( message[ 0 ] == 0 && next && next->str.string[ 0 ] == '(' ) {
		sprintf( message, "Unexpected '(' after expression on line %d.", next->line );
	}
	if( message[ 0 ] ) {
		dispose_expressions( prev->next );
		prev->next = NULL;
	}
	return next;
}

static struct element* parse_expressions( struct element *elem, struct function *func,
	struct variables *vars, char terminator, struct expression *prev, int *num_exprs, char *message ) {
	int line, count = 0;
	while( elem && elem->str.string[ 0 ] != terminator && message[ 0 ] == 0 ) {
		elem = parse_expression( elem, func, vars, prev, message );
		prev = prev->next;
		count++;
		while( elem && message[ 0 ] == 0 && elem->str.string[ 0 ] == ',' ) {
			line = elem->line;
			elem = elem->next;
			if( elem && elem->str.string[ 0 ] != ',' ) {
				elem = parse_expression( elem, func, vars, prev, message );
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

static struct element* parse_increment_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct local_variable *local;
	struct element *next = elem->next;
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt ) {
		prev->head.next = ( struct expression * ) stmt;
		local = get_local_variable( func->variable_decls, next->str.string, "" );
		if( local ) {
			stmt->head.line = elem->line;
			stmt->head.index = local->index;
			stmt->head.evaluate = execute_increment_statement;
			next = next->next->next;
		} else {
			sprintf( message, "Undeclared local variable '%.64s' on line %d.", next->str.string, next->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_decrement_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct local_variable *local;
	struct element *next = elem->next;
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt ) {
		prev->head.next = ( struct expression * ) stmt;
		local = get_local_variable( func->variable_decls, next->str.string, "" );
		if( local ) {
			stmt->head.line = elem->line;
			stmt->head.index = local->index;
			stmt->head.evaluate = execute_decrement_statement;
			next = next->next->next;
		} else {
			sprintf( message, "Undeclared local variable '%.64s' on line %d.", next->str.string, next->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_save_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, vars, prev, execute_save_statement, message );
}

static struct element* parse_append_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct element *next = parse_save_statement( elem, func, vars, prev, message );
	if( prev->head.next && message[ 0 ] == 0 ) {
		prev->head.index = 1;
	}
	return next;
}

static struct element* parse_arraycopy_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, vars, prev, execute_arraycopy_statement, message );
}

static struct element* parse_array_assignment( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem, *child = elem->child;
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt ) {
		prev->head.next = ( struct expression * ) stmt;
		expr.next = NULL;
		child = parse_expression( child, func, vars, &expr, message );
		if( expr.next ) {
			stmt->head.parameters = expr.next;
			if( child->str.string[ 0 ] == ',' ) {
				child = child->next;
			}
			expr.next = NULL;
			child = parse_expression( child, func, vars, &expr, message );
			if( expr.next ) {
				stmt->head.parameters->next = expr.next;
				expr.next = NULL;
				next = parse_expression( next->next->next, func, vars, &expr, message );
				if( expr.next ) {
					stmt->head.line = elem->line;
					expr.next->next = stmt->head.parameters;
					stmt->head.parameters = expr.next;
					stmt->head.evaluate = execute_array_assignment;
				}
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_struct_assignment( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	char *field;
	int idx, count;
	struct expression expr;
	struct structure *struc;
	struct element *next = elem;
	struct statement *stmt = calloc( 1, sizeof( struct structure_statement ) );
	if( stmt ) {
		prev->head.next = ( struct expression * ) stmt;
		struc = ( struct structure * ) find_decl( func, next->str.string, STRUCT, "!." );
		field = field_end( next->str.string, "!." );
		if( struc && field[ 0 ] ) {
			( ( struct structure_statement * ) stmt )->structure = struc;
			idx = get_string_list_index( struc->fields, &field[ 1 ] );
			if( idx >= 0 ) {
				next = next->next;
				expr.next = NULL;
				parse_expressions( next->child, func, vars, 0, &expr, &count, message );
				stmt->head.parameters = expr.next;
				if( message[ 0 ] == 0 ) {
					if( count == 1 ) {
						expr.next = NULL;
						next = parse_expression( next->next->next, func, vars, &expr, message );
						if( expr.next ) {
							stmt->head.line = elem->line;
							stmt->head.index = idx;
							expr.next->next = stmt->head.parameters;
							stmt->head.parameters = expr.next;
							stmt->head.evaluate = execute_struct_assignment;
						}
					} else {
						sprintf( message, "Invalid structure assignment on line %d.", next->line );
					}
				}
			} else {
				sprintf( message, "Field '%.64s' not declared on line %d.", next->str.string, next->line );
			}
		} else {
			sprintf( message, "Undeclared structure or field '%.64s' on line %d.", next->str.string, next->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_local_assignment( struct element *elem,
	struct function *func, struct variables *vars, struct local_variable *local, struct statement *prev, char *message ) {
	int idx;
	struct statement *stmt;
	struct expression expr;
	struct element *next = elem;
	struct structure *struc = local->type;
	char *field = strchr( next->str.string, '.' );
	if( struc && field ) {
		stmt = calloc( 1, sizeof( struct structure_statement ) );
		if( stmt ) {
			prev->head.next = ( struct expression * ) stmt;
			expr.next = NULL;
			next = parse_expression( next->next->next, func, vars, &expr, message );
			if( expr.next ) {
				stmt->head.line = elem->line;
				stmt->head.parameters = expr.next;
				( ( struct structure_statement * ) stmt )->structure = struc;
				idx = get_string_list_index( struc->fields, &field[ 1 ] );
				if( idx >= 0 ) {
					stmt->head.index = idx;
					expr.next = calloc( 1, sizeof( struct expression ) );
					if( expr.next ) {
						expr.next->line = next->line;
						expr.next->index = local->index;
						expr.next->evaluate = evaluate_local;
						stmt->head.parameters->next = expr.next;
						stmt->head.evaluate = execute_struct_assignment;
					} else {
						strcpy( message, OUT_OF_MEMORY );
					}
				} else {
					sprintf( message, "Field '%.64s' not declared on line %d.", elem->str.string, elem->line );
				}
			}
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	} else if( field ) {
		sprintf( message, "Variable '%.64s' has no associated structure on line %d.", local->name, elem->line );
	} else {
		stmt = calloc( 1, sizeof( struct statement ) );
		if( stmt ) {
			prev->head.next = ( struct expression * ) stmt;
			expr.next = NULL;
			next = parse_expression( next->next->next, func, vars, &expr, message );
			if( expr.next ) {
				stmt->head.line = elem->line;
				stmt->head.index = local->index;
				stmt->head.parameters = expr.next;
				stmt->head.evaluate = execute_local_assignment;
			}
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	}
	return next;
}

static struct element* parse_global_assignment( struct element *elem,
	struct function *func, struct variables *vars, struct global_variable *global, struct statement *prev, char *message ) {
	int idx;
	struct statement *stmt;
	struct expression expr;
	struct element *next = elem;
	struct structure *struc = global->type;
	char *field = field_end( next->str.string, "!." );
	if( field[ 0 ] == '!' && !field[ 1 ] ) {
		field++;
	}
	if( struc && field[ 0 ] ) {
		stmt = calloc( 1, sizeof( struct structure_statement ) );
		if( stmt ) {
			prev->head.next = ( struct expression * ) stmt;
			expr.next = NULL;
			next = parse_expression( next->next->next, func, vars, &expr, message );
			if( expr.next ) {
				stmt->head.line = elem->line;
				stmt->head.parameters = expr.next;
				( ( struct structure_statement * ) stmt )->structure = struc;
				idx = get_string_list_index( struc->fields, &field[ 1 ] );
				if( idx >= 0 ) {
					stmt->head.index = idx;
					expr.next = calloc( 1, sizeof( struct value_expression ) );
					if( expr.next ) {
						expr.next->line = next->line;
						expr.next->evaluate = evaluate_global;
						( ( struct value_expression * ) expr.next )->str = &global->str;
						stmt->head.parameters->next = expr.next;
						stmt->head.evaluate = execute_struct_assignment;
					} else {
						strcpy( message, OUT_OF_MEMORY );
					}
				} else {
					sprintf( message, "Field '%.64s' not declared on line %d.", elem->str.string, elem->line );
				}
			}
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	} else if( field[ 0 ] ) {
		sprintf( message, "Variable '%.64s' has no associated structure on line %d.", global->str.string, elem->line );
	} else {
		stmt = calloc( 1, sizeof( struct global_assignment_statement ) );
		if( stmt ) {
			prev->head.next = ( struct expression * ) stmt;
			expr.next = NULL;
			next = parse_expression( next->next->next, func, vars, &expr, message );
			if( expr.next ) {
				stmt->head.line = elem->line;
				stmt->head.parameters = expr.next;
				( ( struct global_assignment_statement * ) stmt )->destination = &global->value;
				stmt->head.evaluate = execute_global_assignment;
			}
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	}
	return next;
}

static struct element* parse_assignment_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct local_variable *local;
	struct global_variable *global;
	elem = elem->next;
	while( message[ 0 ] == 0 && elem ) {
		if( elem->str.string[ 0 ] == ';' ) {
			elem = elem->next;
			break;
		} else if( elem->str.string[ 0 ] == ',' && elem->next ) {
			elem = elem->next;
		}
		if( elem->str.string[ 0 ] == '[' ) {
			elem = parse_array_assignment( elem, func, vars, prev, message );
		} else if( elem->next && elem->next->str.string[ 0 ] == '(' ) {
			elem = parse_struct_assignment( elem, func, vars, prev, message );
		} else {
			local = get_local_variable( func->variable_decls, elem->str.string, "." );
			if( local ) {
				elem = parse_local_assignment( elem, func, vars, local, prev, message );
			} else {
				global = ( struct global_variable * ) find_decl( func, elem->str.string, GLOBAL, "!." );
				if( global ) {
					elem = parse_global_assignment( elem, func, vars, global, prev, message );
				} else {
					sprintf( message, "Undeclared variable '%.64s' on line %d.", elem->str.string, elem->line );
				}
			}
		}
		prev = ( struct statement * ) prev->head.next;
	}
	return elem;
}

/* Parse a statement that expects one or more expressions after the keyword. */
struct element* parse_expr_list_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev,
	enum result ( *execute )( struct expression *this, struct variables *vars, struct variable *result ),
	char *message ) {
	struct expression head, *expr;
	struct element *next = elem->next;
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt ) {
		stmt->head.line = elem->line;
		prev->head.next = ( struct expression * ) stmt;
		head.next = NULL;
		next = parse_expression( next, func, vars, &head, message );
		expr = stmt->head.parameters = head.next;
		while( expr && next->str.string[ 0 ] != ';' ) {
			if( next->str.string[ 0 ] == ',' ) {
				next = next->next;
			}
			next = parse_expression( next, func, vars, expr, message );
			expr = expr->next;
		}
		if( expr ) {
			stmt->head.evaluate = execute;
			next = next->next;
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_print_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, vars, prev, execute_print_statement, message );
}

static struct element* parse_write_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, vars, prev, execute_write_statement, message );
}

static struct element* parse_error_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, vars, prev, execute_error_statement, message );
}

static struct element* parse_return_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, vars, prev, execute_return_statement, message );
}

static struct element* parse_throw_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, vars, prev, execute_throw_statement, message );
}

static struct element* parse_exit_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, vars, prev, execute_exit_statement, message );
}

static struct element* parse_break_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt ) {
		stmt->head.line = elem->line;
		stmt->head.evaluate = execute_break_statement;
		prev->head.next = ( struct expression * ) stmt;
		next = next->next;
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_continue_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt ) {
		stmt->head.line = elem->line;
		stmt->head.evaluate = execute_continue_statement;
		prev->head.next = ( struct expression * ) stmt;
		next = next->next;
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_call_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, vars, prev, execute_call_statement, message );
}

static struct element* parse_case_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct expression expr = { 0 };
	struct element *next = elem->next;
	struct statement block = { 0 }, *stmt = calloc( 1, sizeof( struct block_statement ) );
	if( stmt ) {
		stmt->head.line = elem->line;
		stmt->dispose = dispose_block_statement;
		prev->head.next = ( struct expression * ) stmt;
		next = parse_expressions( next, func, vars, '{', &expr, NULL, message );
		stmt->head.parameters = expr.next;
		if( message[ 0 ] == 0 ) {
			parse_keywords_indexed( func->env->statements_index, next->child, func, vars, &block, message );
			( ( struct block_statement * ) stmt )->if_block = ( struct statement * ) block.head.next;
			if( message[ 0 ] == 0 ) {
				stmt->head.evaluate = execute_case_statement;
				next = next->next;
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_default_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement block = { 0 }, *stmt = calloc( 1, sizeof( struct block_statement ) );
	if( stmt ) {
		stmt->head.line = elem->line;
		stmt->dispose = dispose_block_statement;
		prev->head.next = ( struct expression * ) stmt;
		block.head.next = NULL;
		parse_keywords_indexed( func->env->statements_index, next->child, func, vars, &block, message );
		( ( struct block_statement * ) stmt )->if_block = ( struct statement * ) block.head.next;
		if( message[ 0 ] == 0 ) {
			stmt->head.evaluate = execute_case_statement;
			next = next->next;
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return elem->next->next;
}

static struct keyword switch_stmts[] = {
	{ "case", "X{", parse_case_statement, &switch_stmts[ 1 ] },
	{ "default", "{", parse_default_statement, &switch_stmts[ 2 ] },
	{ "rem", "{", parse_comment, NULL }
};

static struct element* parse_switch_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement block = { 0 }, *cas, *def;
	struct block_statement *stmt = calloc( 1, sizeof( struct block_statement ) );
	if( stmt ) {
		stmt->stmt.head.line = elem->line;
		stmt->stmt.dispose = dispose_block_statement;
		prev->head.next = ( struct expression * ) stmt;
		expr.next = NULL;
		next = parse_expression( next, func, vars, &expr, message );
		if( expr.next ) {
			stmt->stmt.head.parameters = expr.next;
			block.head.next = NULL;
			parse_keywords( switch_stmts, next->child, func, vars, &block, message );
			if( message[ 0 ] == 0 ) {
				cas = def = NULL;
				while( block.head.next ) {
					if( block.head.next->parameters ) {
						if( cas ) {
							cas->head.next = ( struct expression * ) block.head.next;
							cas = ( struct statement * ) cas->head.next;
						} else {
							stmt->if_block = cas = ( struct statement * ) block.head.next;
						}
						block.head.next = ( struct expression * ) block.head.next->next;
						cas->head.next = NULL;
					} else {
						if( def ) {
							def->head.next = ( struct expression * ) block.head.next;
							def = ( struct statement * ) def->head.next;
						} else {
							stmt->else_block = def = ( struct statement * ) block.head.next;
						}
						block.head.next = ( struct expression * ) block.head.next->next;
						def->head.next = NULL;
					}
				}
				if( stmt->else_block == NULL || stmt->else_block->head.next == NULL ) {
					next = next->next;
					stmt->stmt.head.evaluate = execute_switch_statement;
				} else {
					sprintf( message, "Duplicate default block in switch statement on line %d.", elem->line );
				}
			} else {
				dispose_statements( ( struct statement * ) block.head.next );
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

/* Return 1 if name is equivalent to the specified keyword. */
int is_keyword( char *name, char *key ) {
	int chr = key[ 0 ];
	if( strcmp( &name[ 1 ], &key[ 1 ] ) == 0 ) {
		if( chr == name[ 0 ] || ( chr > 96 && chr - 32 == name[ 0 ] ) ) {
			return 1;
		}
	}
	return 0;
}

static struct keyword* get_keyword( char *name, struct keyword *keywords ) {
	while( keywords && !is_keyword( name, keywords->name ) ) {
		keywords = keywords->next;
	}
	return keywords;
}

struct element* validate_syntax( char *syntax, struct element *elem,
	struct element *prev, struct environment *env, char *message ) {
	int idx = 1, chr = syntax[ 0 ];
	struct element *next;
	while( message[ 0 ] == 0 ) {
		switch( chr ) {
			case 0:
				return elem;
			case '0': /* List end. */
				if( elem ) {
					sprintf( message, "Unexpected '%.64s' after '%.64s' on line %d.", elem->str.string, prev->str.string, elem->line );
				}
				break;
			case '"': case ',': case ';': case '=': case '{':
				/* Strings, separators or blocks. */
				if( elem && elem->str.string[ 0 ] == chr ) {
					prev = elem;
					elem = elem->next;
				} else {
					sprintf( message, "Expected '%c' after '%.64s' on line %d.", chr, prev->str.string, prev->line );
				}
				break;
			case '(': /* Bracketed function parameter list. */
				if( elem && elem->str.string[ 0 ] == '(' ) {
					if( elem->child ) {
						validate_syntax( "P0", elem->child, elem, env, message );
					}
					prev = elem;
					elem = elem->next;
				} else {
					sprintf( message, "Expected '(' after '%.64s' on line %d.", prev->str.string, prev->line );
				}
				break;
			case '[': /* Index expression. */
				if( elem && elem->str.string[ 0 ] == '[' ) {
					if( elem->child ) {
						validate_syntax( "xx0", elem->child, elem, env, message );
						prev = elem;
						elem = elem->next;
					} else {
						sprintf( message, "Invalid index expression after '%.64s' on line %d.", prev->str.string, elem->line );
					}
				} else {
					sprintf( message, "Expected '[' after '%.64s' on line %d.", prev->str.string, prev->line );
				}
				break;
			case 'c': /* Catch or finally. */
				if( elem && ( is_keyword( elem->str.string, "catch" ) || is_keyword( elem->str.string, "finally" ) ) ) {
					chr = elem->str.string[ 0 ];
					prev = elem;
					elem = elem->next;
					if( strchr( "Cc", chr ) ) {
						if( elem && elem->str.string[ 0 ] == '(' ) {
							validate_syntax( "n0", elem->child, elem, env, message );
							prev = elem->child;
							elem = elem->next;
						}
						if( message[ 0 ] == 0 ) {
							next = validate_syntax( "n", elem, prev, env, message );
							prev = elem;
							elem = next;
						}
					}
					if( message[ 0 ] == 0 ) {
						next = validate_syntax( "{", elem, prev, env, message );
						prev = elem;
						elem = next;
						if( message[ 0 ] == 0 && elem
						&& ( is_keyword( elem->str.string, "catch" ) || is_keyword( elem->str.string, "finally" ) ) ) {
							prev = elem;
							elem = validate_syntax( "c", elem, prev, env, message );
						}
					}
				} else {
					sprintf( message, "Expected 'catch' or 'finally' after '%.64s' on line %d.", prev->str.string, prev->line );
				}
				break;
			case 'f': /* From. */
				if( elem && is_keyword( elem->str.string, "from" ) ) {
					prev = elem;
					elem = elem->next;
				} else {
					sprintf( message, "Expected 'from' after '%.64s' on line %d.", prev->str.string, prev->line );
				}
				break;
			case 'n': /* Name. */
				if( elem && elem->str.string[ 0 ] != ';' ) {
					validate_name( elem, message );
					prev = elem;
					elem = elem->next;
				} else {
					sprintf( message, "Expected name after '%.64s' on line %d.", prev->str.string, prev->line );
				}
				break;
			case 'P': /* Function parameter list. */
				while( message[ 0 ] == 0 && elem ) {
					if( elem && elem->str.string[ 0 ] == '(' ) {
						validate_syntax( "n0", elem->child, elem, env, message );
						prev = elem->child;
						elem = elem->next;
					}
					if( message[ 0 ] == 0 ) {
						next = validate_syntax( "n", elem, prev, env, message );
						prev = elem;
						elem = next;
						if( elem && elem->str.string[ 0 ] == ',' && elem->next ) {
							prev = elem;
							elem = elem->next;
						}
					}
				}
				break;
			case 'x': /* Expression. */
				if( elem && strchr( ",;({", elem->str.string[ 0 ] ) == NULL ) {
					if( elem->str.string[ 0 ] == '[' ) {
						validate_syntax( "[", elem, prev, env, message );
					} else if( elem->str.string[ 0 ] == '\\' && elem->str.string[ 1 ] == 0 ) {
						prev = elem;
						elem = elem->next;
						validate_syntax( "\"", elem, prev, env, message );
					} else if( elem->str.string[ 0 ] == '$' && elem->str.string[ 1 ] == 0 ) {
						prev = elem;
						elem = elem->next;
						validate_syntax( "{", elem, prev, env, message );
					} else if( elem->next && elem->next->str.string[ 0 ] == '(' ) {
						prev = elem;
						elem = elem->next;
					}
					if( elem ) {
						prev = elem;
						elem = elem->next;
						if( syntax[ idx ] == 'x' && elem && elem->str.string[ 0 ] == ',' ) {
							prev = elem;
							elem = elem->next;
						}
					}
				} else {
					sprintf( message, "Expected expression after '%.64s' on line %d.", prev->str.string, prev->line );
				}
				break;
			case 'X': /* Expression list, terminated by '{' or NULL. */
				next = NULL;
				while( message[ 0 ] == 0 && elem && elem->str.string[ 0 ] != '{' ) {
					if( next && elem->str.string[ 0 ] == ',' ) {
						prev = elem;
						elem = elem->next;
					}
					next = validate_syntax( "x", elem, prev, env, message );
					prev = elem;
					elem = next;
				}
				break;
			case 'v': /* Variable declaration. */
				if( elem && elem->str.string[ 0 ] == '(' ) {
					validate_syntax( "n0", elem->child, elem, env, message );
					prev = elem->child;
					elem = elem->next;
				}
				if( message[ 0 ] == 0 ) {
					if( elem && elem->str.string[ 0 ] == '[' ) {
						validate_syntax( "nx0", elem->child, elem, env, message );
						prev = elem->child;
						elem = elem->next;
					} else {
						next = validate_syntax( "n", elem, prev, env, message );
						prev = elem;
						elem = next;
						if( message[ 0 ] == 0 && elem && elem->str.string[ 0 ] == '=' ) {
							prev = elem;
							elem = validate_syntax( "x", elem->next, prev, env, message );
						}
					}
				}
				break;
			case 'V': /* Variable declaration list, terminated by ';' or NULL. */
				next = NULL;
				while( message[ 0 ] == 0 && elem && elem->str.string[ 0 ] != ';' ) {
					if( next && elem->str.string[ 0 ] == ',' ) {
						prev = elem;
						elem = elem->next;
					}
					next = validate_syntax( "v", elem, prev, env, message );
					prev = elem;
					elem = next;
				}
				break;
			case 'A': /* Variable assignment list, terminated by ';' or NULL. */
				next = NULL;
				while( message[ 0 ] == 0 && elem && elem->str.string[ 0 ] != ';' ) {
					if( next && elem->str.string[ 0 ] == ',' ) {
						prev = elem;
						elem = elem->next;
					}
					next = validate_syntax( "x=x", elem, prev, env, message );
					prev = elem;
					elem = next;
				}
				break;
			default: /* Internal error. */
				sprintf( message, "Internal error. Unknown specifier '%c' while parsing line %d.", chr, ( elem ? elem : prev )->line );
		}
		chr = syntax[ idx++ ];
	}
	return elem;
}

static struct element* parse_infix_expression( struct element *elem,
	struct function *func, struct variables *vars, struct expression *prev, char *message ) {
	struct element *copy, *oper, *child, *next = elem->next;
	if( next && next->str.string[ 0 ] == '(' && next->child ) {
		copy = new_element( next->str.length );
		if( copy ) {
			copy->line = next->line;
			strcpy( copy->str.string, next->str.string );
			copy->child = copy_element( next->child );
			if( copy->child ) {
				oper = validate_syntax( "x", copy->child, next, func->env, message );
				if( message[ 0 ] == 0 ) {
					if( oper && strcmp( oper->str.string, elem->str.string ) ) {
						child = copy->child;
						while( child->next != oper ) {
							child = child->next;
						}
						child->next = oper->next;
						oper->next = copy;
						copy = oper;
					}
					parse_expression( copy, func, vars, prev, message );
					next = next->next;
				}
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
			dispose_element( copy );
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	} else {
		sprintf( message, "Invalid infix expression on line %d.", elem->line );
	}
	return next;
}

static struct element* parse_keyword( struct keyword *key, struct element *elem,
	struct function *func, struct variables *vars, struct statement *stmt, char *message ) {
	if( key ) {
		validate_syntax( key->syntax, elem->next, elem, func->env, message );
		if( message[ 0 ] == 0 ) { 
			elem = key->parse( elem, func, vars, stmt, message );
		}
	} else {
		sprintf( message, "Unrecognized keyword '%.64s' on line %d.", elem->str.string, elem->line );
	}
	return elem;
}

static void parse_keywords( struct keyword *keywords, struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct statement *stmt = prev;
	while( elem && message[ 0 ] == 0 ) {
		elem = parse_keyword( get_keyword( elem->str.string, keywords ), elem, func, vars, stmt, message );
		while( stmt && stmt->head.next ) {
			stmt = ( struct statement * ) stmt->head.next;
		}
	}
}

struct statement* parse_keywords_indexed( struct keyword **index, struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct statement *stmt = prev;
	while( elem && message[ 0 ] == 0 ) {
		elem = parse_keyword( get_keyword( elem->str.string, index[ hash_code( elem->str.string, 0 ) ] ), elem, func, vars, stmt, message );
		while( stmt && stmt->head.next ) {
			stmt = ( struct statement * ) stmt->head.next;
		}
	}
#if defined( OPTIMIZER )
	if( message[ 0 ] == 0 && prev->head.next ) {
		stmt = optimize_statements( func, prev, message );
	}
#endif
	return stmt;
}

static struct element* parse_if_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement block = { 0 }, *stmt = calloc( 1, sizeof( struct block_statement ) );
	if( stmt ) {
		stmt->head.line = elem->line;
		stmt->dispose = dispose_block_statement;
		prev->head.next = ( struct expression * ) stmt;
		expr.next = NULL;
		next = parse_expression( next, func, vars, &expr, message );
		if( expr.next ) {
			stmt->head.parameters = expr.next;
			stmt->head.evaluate = execute_if_statement;
			if( next->child ) {
				parse_keywords_indexed( func->env->statements_index, next->child, func, vars, &block, message );
				( ( struct block_statement * ) stmt )->if_block = ( struct statement * ) block.head.next;
			}
			if( message[ 0 ] == 0 ) {
				next = next->next;
				if( next && is_keyword( next->str.string, "else" ) ) {
					if( next->next && ( next->next->str.string[ 0 ] == '{' || is_keyword( next->next->str.string, "if" ) ) ) {
						next = next->next;
						block.head.next = NULL;
						if( next->child ) {
							parse_keywords_indexed( func->env->statements_index, next->child, func, vars, &block, message );
							if( message[ 0 ] == 0 ) {
								next = next->next;
							}
						} else {
							validate_syntax( "x{", next->next, next, func->env, message );
							if( message[ 0 ] == 0 ) {
								next = parse_if_statement( next, func, vars, &block, message );
							}
						}
						( ( struct block_statement * ) stmt )->else_block = ( struct statement * ) block.head.next;
					} else {
						sprintf( message, "Expected '{' or 'if' after 'else' on line %d.", next->line );
					}
				}
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_while_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement block = { 0 }, *stmt = calloc( 1, sizeof( struct block_statement ) );
	if( stmt ) {
		stmt->head.line = elem->line;
		stmt->dispose = dispose_block_statement;
		prev->head.next = ( struct expression * ) stmt;
		expr.next = NULL;
		next = parse_expression( next, func, vars, &expr, message );
		if( expr.next ) {
			stmt->head.parameters = expr.next;
			if( next->child ) {
				parse_keywords_indexed( func->env->statements_index, next->child, func, vars, &block, message );
				( ( struct block_statement * ) stmt )->if_block = ( struct statement * ) block.head.next;
			}
			if( message[ 0 ] == 0 ) {
				stmt->head.evaluate = execute_while_statement;
				next = next->next;
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_until_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct element *next = parse_while_statement( elem, func, vars, prev, message );
	if( message[ 0 ] == 0 ) {
		prev->head.next->evaluate = execute_until_statement;
	}
	return next;
}

static struct element* parse_catch_block( struct element *elem,
	struct function *func, struct variables *vars, struct block_statement *try_stmt, char *message ) {
	struct local_variable *local = NULL;
	struct statement block = { 0 };
	if( strchr( "Cc", elem->str.string[ 0 ] ) ) {
		elem = elem->next;
		if( elem->str.string[ 0 ] != '(' ) {
			local = get_local_variable( func->variable_decls, elem->str.string, "" );
		}
		if( local && !local->type ) {
			elem = elem->next;
		} else {
			elem = parse_variable_declaration( elem, func, vars, NULL, add_local_variable, message );
			if( message[ 0 ] == 0 ) {
				local = func->variable_decls_tail;
			}
		}
		if( local ) {
			try_stmt->stmt.head.index = local->index;
			try_stmt->stmt.head.evaluate = execute_try_catch_statement;
		}
	} else {
		elem = elem->next;
		try_stmt->stmt.head.evaluate = execute_try_finally_statement;
	}
	if( message[ 0 ] == 0 ) {
		if( elem->child ) {
			parse_keywords_indexed( func->env->statements_index, elem->child, func, vars, &block, message );
			try_stmt->else_block = ( struct statement * ) block.head.next;
		}
		if( message[ 0 ] == 0 ) {
			elem = elem->next;
		}
	}
	return elem;
}

static struct element* parse_try_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct statement try_block = { 0 };
	struct block_statement *try_stmt = calloc( 1, sizeof( struct block_statement ) );
	if( try_stmt ) {
		try_stmt->stmt.head.line = elem->line;
		try_stmt->stmt.dispose = dispose_block_statement;
		prev->head.next = ( struct expression * ) &try_stmt->stmt;
		elem = elem->next;
		if( elem->child ) {
			parse_keywords_indexed( func->env->statements_index, elem->child, func, vars, &try_block, message );
			try_stmt->if_block = ( struct statement * ) try_block.head.next;
		}
		if( message[ 0 ] == 0 ) {
			elem = parse_catch_block( elem->next, func, vars, try_stmt, message );
			while( message[ 0 ] == 0 && elem
			&& ( is_keyword( elem->str.string, "catch" ) || is_keyword( elem->str.string, "finally" ) ) ) {
				try_stmt = calloc( 1, sizeof( struct block_statement ) );
				if( try_stmt ) {
					try_stmt->stmt.head.line = elem->line;
					try_stmt->stmt.dispose = dispose_block_statement;
					try_stmt->if_block = ( struct statement * ) prev->head.next;
					prev->head.next = ( struct expression * ) &try_stmt->stmt;
					elem = parse_catch_block( elem, func, vars, try_stmt, message );
				} else {
					strcpy( message, OUT_OF_MEMORY );
				}
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return elem;
}

static struct element* add_function_parameter( struct function *func, struct element *elem, char *message ) {
	struct local_variable *param;
	struct structure *type = NULL;
	if( elem->str.string[ 0 ] == '(' ) {
		type = ( struct structure * ) find_decl( func, elem->child->str.string, STRUCT, NULL );
		if( type ) {
			elem = elem->next;
		} else {
			sprintf( message, "Structure '%.64s' not declared on line %d.", elem->child->str.string, elem->child->line );
		}
	}
	if( message[ 0 ] == 0 ) {
		param = new_local_variable( func, elem, type, message );
		if( message[ 0 ] == 0 ) {
			/*printf("Function parameter '%s'\n", name);*/
			if( get_local_variable( func->variable_decls, elem->str.string, "" ) == NULL ) {
				func->num_parameters = func->num_variables = func->num_parameters + 1;
				if( func->variable_decls ) {
					func->variable_decls_tail->next = param;
				} else {
					func->variable_decls = param;
				}
				func->variable_decls_tail = param;
				elem = elem->next;
			} else {
				dispose_local_variables( param );
				sprintf( message, "Parameter '%.64s' already defined on line %d.", elem->str.string, elem->line );
			}
		}
	}
	return elem;
}

struct function* parse_function( struct element *elem, char *name,
	struct function *parent, char *message ) {
	struct element *child;
	struct function *func;
	if( elem->line < 1 && !check_element_depth( elem, parent->env->element_depth ) ) {
		strcpy( message, "Maximum element depth exceeded." );
		return NULL;
	}
	func = new_function( name, parent );
	if( func ) {
		func->line = elem->line;
		func->file = parent->file;
		func->file->reference_count++;
		if( parent->library ) {
			func->library = parent->library;
			func->library->reference_count++;
		}
		func->body = elem->next;
		func->env = parent->env;
		child = elem->child;
		while( child && message[ 0 ] == 0 ) {
			child = add_function_parameter( func, child, message );
			if( child && child->str.string[ 0 ] == ',' && message[ 0 ] == 0 ) {
				child = child->next;
			}
		}
		if( message[ 0 ] ) {
			unref_string( &func->str );
			func = NULL;
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return func;
}

int parse_function_body( struct function *func, struct variables *vars, char *message ) {
	struct statement prev = { 0 }, *stmt = NULL;
	struct element *next = func->body->child;
	struct expression *source;
	if( next ) {
		prev.head.next = NULL;
		stmt = parse_keywords_indexed( func->env->statements_index, next, func, vars, &prev, message );
		func->statements = ( struct statement * ) prev.head.next;
	}
	if( message[ 0 ] == 0 ) {
		if( stmt && stmt->head.evaluate == execute_return_statement ) {
			source = stmt->head.parameters;
			if( source && source->evaluate == evaluate_call_expression
			&& ( ( struct function_expression * ) source )->function == func ) {
				stmt->head.evaluate = execute_tail_call_statement;
			}
		}
		return 1;
	}
	return 0;
}

static enum result evaluate_function_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct environment *env = vars->func->env;
	struct variable var = { 0, NULL };
	struct function *func;
	struct element *elem, key = { { 1, "$function", 9, ELEMENT }, NULL, NULL, 0 };
	char message[ 128 ] = "";
	enum result ret = evaluate_element( parameter, vars, &var, 0 );
	if( ret ) {
		elem = ( struct element * ) var.string_value;
		if( ( size_t ) &ret < env->stack_limit ) {
			ret = throw_stack_overflow( vars, this );
		} else {
			key.line = this->line;
			validate_syntax( "({0", elem, &key, env, message );
			if( message[ 0 ] == 0 ) {
				func = parse_function( elem, NULL, vars->func, message );
				if( func ) {
					if( parameter->evaluate != evaluate_string_literal_expression ) {
						unref_string( func->file );
						func->file = &func->str;
					}
					if( parse_function_body( func, vars, message ) ) {
						result->string_value = &func->str;
					} else {
						unref_string( &func->str );
						ret = throw( vars, this, 0, message );
					}
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

static struct element* parse_function_declaration( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct function *decl = parse_function( next->next, next->str.string, func, message );
	if( decl ) {
		if( add_decl( &decl->str, next->line, func->env, message ) ) {
			next = next->next->next->next;
		}
		unref_string( &decl->str );
	}
	return next;
}

static struct element* parse_program_declaration( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct environment *env = func->env;
	struct element *next = elem->next;
	struct function *prog = new_function( next->str.string, NULL );
	if( prog ) {
		prog->line = elem->line;
		prog->file = func->file;
		prog->file->reference_count++;
		if( func->library ) {
			prog->library = func->library;
			prog->library->reference_count++;
		}
		prog->body = next->next;
		prog->env = env;
		if( add_decl( &prog->str, next->line, env, message ) ) {
			env->entry_point = prog;
			next = next->next->next;
		}
		unref_string( &prog->str );
	}
	return next;
}

static int include_tt_file( struct string *path, struct environment *env, char *message ) {
	long file_length, success = 0;
	char *program_buffer, *file_name = path->string, error[ 128 ] = "";
	/* Load program file into string.*/
	file_length = load_file( file_name, NULL, 0, 0, message );
	if( file_length >= 0 ) {
		if( file_length < MAX_INTEGER ) {
			/* printf( "Parsing '%s'. Length %ld\n", file_name, file_length ); */
			program_buffer = malloc( file_length + 1 );
			if( program_buffer ) {
				file_length = load_file( file_name, program_buffer, 0, file_length, message );
				if( file_length >= 0 ) {
					program_buffer[ file_length ] = 0;
					/* Parse program structure.*/
					success = parse_tt_program( program_buffer, path, env, message );
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

static struct element* parse_include( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct string_list *include;
	int path_len = str_idx( func->file->string, "/:\\", -1 ) + 1;
	int name_len = unquote_string( next->str.string, NULL, '"' );
	struct string *path = ( struct string * ) new_source( path_len + name_len );
	if( path ) {
		memcpy( path->string, func->file->string, sizeof( char ) * path_len );
		unquote_string( next->str.string, &path->string[ path_len ], '"' );
		if( strchr( "/\\", path->string[ path_len ] ) || strchr( &path->string[ path_len ], ':' ) ) {
			/* Absolute path. */
			unquote_string( next->str.string, path->string, '"' );
		}
		if( get_string_list_index( func->env->include_paths, next->str.string ) < 0 ) {
			include = new_string_list( &next->str, func->env->include_paths );
			if( include ) {
				func->env->include_paths = include;
				if( include_tt_file( path, func->env, message ) ) {
					next = next->next->next;
				}
				func->env->include_paths = include->next;
				unref_string( include->str );
				free( include );
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		} else {
			sprintf( message, "File %.64s already included on line %d.\n", next->str.string, elem->line );
		}
		unref_string( path );
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static int import_library( struct string *library, struct string *source, char *message ) {
	struct string_list *imports;
	if( source->type == SOURCE ) {
		imports = ( ( struct source * ) source )->imports;
		while( imports ) {
			if( imports->str == library ) {
				return 1;
			}
			imports = imports->next;
		}
		imports = new_string_list( library, ( ( struct source * ) source )->imports );
		if( imports ) {
			( ( struct source * ) source )->imports = imports;
		} else {
			strcpy( message, OUT_OF_MEMORY );
			return 0;
		}
	}
	return 1;
}

static struct element* parse_import( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct element *next = elem->next, *name = next;
	struct string *library = get_decl_indexed( func, name->str.string, name->str.length, LIBRARY );
	if( library ) {
		next = next->next->next->next->next;
	} else {
		next = parse_include( next->next, func, vars, prev, message );
		library = get_decl_indexed( func, name->str.string, name->str.length, LIBRARY );
	}
	if( message[ 0 ] == 0 ) {
		if( library ) {
			import_library( library, func->file, message );
		} else {
			sprintf( message, "Library '%.32s' not found in file %.64s on line %d.\n", name->str.string, name->next->next->str.string, elem->line );
		}
	}
	return next;
}

static int add_structure_field( struct structure *struc, struct element *elem, char *message ) {
	struct string *field;
	if( validate_name( elem, message ) ) {
		if( struc->fields && get_string_list_index( struc->fields, elem->str.string ) >= 0 ) {
			sprintf( message, "Field '%.64s' already defined on line %d.", elem->str.string, elem->line );
		} else {
			field = new_string_value( elem->str.string );
			if( field ) {
				if( append_string_list( &struc->fields, &struc->fields_tail, field ) ) {
					if( struc->length++ >= 32768 ) {
						sprintf( message, "Too many fields on line %d.", elem->line );
					}
				} else {
					strcpy( message, OUT_OF_MEMORY );
				}
				unref_string( field );
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		}
	}
	return message[ 0 ] == 0;
}

static struct element* skip_comments( struct element *elem, char *message ) {
	int line;
	char *str;
	while( elem ) {
		str = elem->str.string;
		if( strcmp( str, "rem" ) == 0 || strcmp( str, "Rem" ) == 0 ) {
			line = elem->line;
			elem = elem->next;
			if( elem == NULL || elem->str.string[ 0 ] != '{' ) {
				sprintf( message, "Expected '{' after '%s' on line %d.", str, line );
				break;
			}
		} else {
			break;
		}
		elem = elem->next;
	}
	return elem;
}

static struct element* parse_struct_member_function( struct string *struct_name, struct element *elem, struct function *func, char *message ) {
	int slen, qlen;
	char qname[ 64 ];
	struct function *decl;
	struct element *next = skip_comments( elem, message );
	if( next && message[ 0 ] == 0 ) {
		elem = next;
		next = next->next;
		if( strcmp( elem->str.string, "function" ) == 0 ) {
			validate_syntax( "n({", next, elem, NULL, message );
			if( message[ 0 ] == 0 ) {
				slen = struct_name->length;
				qlen = slen + 1 + next->str.length;
				if( qlen < 64 ) {
					strcpy( qname, struct_name->string );
					qname[ slen ] = '_';
					strcpy( &qname[ slen + 1 ], next->str.string );
					decl = parse_function( next->next, qname, func, message );
					if( decl ) {
						if( add_decl( &decl->str, elem->line, func->env, message ) ) {
							next = next->next->next->next;
						}
						unref_string( &decl->str );
					}
				} else {
					sprintf( message, "Name too long on line %d.", next->line );
				}
			}
		} else {
			sprintf( message, "Expected 'function' on line %d.", elem->line );
		}
	}
	return next;
}

static struct element* parse_struct_declaration( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct string_list *field, *tail;
	struct environment *env = func->env;
	struct element *child, *next = elem->next;
	struct string *struct_name = &next->str;
	int qlen = qualify_decl( func, next->str.string, next->str.length, NULL );
	struct structure *struc = calloc( 1, sizeof( struct structure ) + sizeof( char ) * ( qlen + 2 ) * 2 );
	if( struc ) {
		struc->str.reference_count = 1;
		struc->str.string = ( char * ) &struc[ 1 ];
		qualify_decl( func, next->str.string, next->str.length, struc->str.string );
		struc->str.length = qlen;
		struc->str.type = STRUCT;
		struc->instance_name = struc->str.string + qlen + 1;
		struc->instance_name[ 0 ] = '[';
		strcpy( &struc->instance_name[ 1 ], struc->str.string );
		struc->instance_name[ qlen + 1 ] = ']';
		struc->instance_name_len = qlen + 2;
		tail = add_decl( &struc->str, next->line, env, message );
		if( tail ) {
			next = next->next;
			if( next && next->str.string[ 0 ] == '(' ) {
				child = next->child;
				if( child && child->next == NULL ) {
					struc->super = ( struct structure * ) find_decl( func, child->str.string, STRUCT, NULL );
				}
				if( struc->super && struc->super != struc ) {
					field = struc->super->fields;
					while( field && message[ 0 ] == 0 ) {
						if( append_string_list( &struc->fields, &struc->fields_tail, field->str ) ) {
							struc->length++;
						} else {
							strcpy( message, OUT_OF_MEMORY );
						}
						field = field->next;
					}
					next = next->next;
				} else {
					sprintf( message, "Invalid parent structure on line %d.", next->line );
				}
			}
		}
		if( message[ 0 ] == 0 ) {
			if( next && next->str.string[ 0 ] == '{' ) {
				child = next->child;
				while( child && message[ 0 ] == 0 ) {
					child = skip_comments( child, message );
					if( child && message[ 0 ] == 0 && add_structure_field( struc, child, message ) ) {
						child = child->next;
						if( child ) {
							if( child->str.string[ 0 ] == ',' ) {
								child = child->next;
							} else if( child->str.string[ 0 ] == ';' ) {
								child = child->next;
								while( child && message[ 0 ] == 0 ) {
									child = parse_struct_member_function( struct_name, child, func, message );
								}
							}
						}
					}
				}
				next = next->next;
				if( next && next->str.string[ 0 ] == ';' ) {
					next = next->next;
				}
			} else {
				sprintf( message, "Expected '{' after 'struct' on line %d.", elem->line );
			}
		}
		unref_string( &struc->str );
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct keyword statements[] = {
	{ "rem", "{", parse_comment, NULL },
	{ "var", "V;", parse_local_declaration, NULL },
	{ "let", "A;", parse_assignment_statement, NULL },
	{ "print", "x;", parse_print_statement, NULL },
	{ "write", "x;", parse_write_statement, NULL },
	{ "error", "x;", parse_error_statement, NULL },
	{ "throw", "x;", parse_throw_statement, NULL },
	{ "return", "x;", parse_return_statement, NULL },
	{ "exit", "x;", parse_exit_statement, NULL },
	{ "break", ";", parse_break_statement, NULL },
	{ "continue", ";", parse_continue_statement, NULL },
	{ "if", "x{", parse_if_statement, NULL },
	{ "while", "x{", parse_while_statement, NULL },
	{ "until", "x{", parse_until_statement, NULL },
	{ "call", "x;", parse_call_statement, NULL },
	{ "try", "{c", parse_try_statement, NULL },
	{ "set", "A;", parse_assignment_statement, NULL },
	{ "switch", "x{", parse_switch_statement, NULL },
	{ "inc", "n;", parse_increment_statement, NULL },
	{ "dec", "n;", parse_decrement_statement, NULL },
	{ "save", "xx;", parse_save_statement, NULL },
	{ "append", "xx;", parse_append_statement, NULL },
	{ "arraycopy", "xxxxx;", parse_arraycopy_statement, NULL },
	{ NULL }
};

static struct operator operators[] = {
	{ "%", '%',-2, evaluate_arithmetic_expression, NULL },
	{ "&", '&',-2, evaluate_arithmetic_expression, NULL },
	{ "*", '*',-2, evaluate_arithmetic_expression, NULL },
	{ "+", '+',-2, evaluate_arithmetic_expression, NULL },
	{ "-", '-',-2, evaluate_arithmetic_expression, NULL },
	{ "/", '/',-2, evaluate_arithmetic_expression, NULL },
	{ "_/",'/',-2, evaluate_arithmetic_expression, NULL },
	{ "<", '<', 2, evaluate_arithmetic_expression, NULL },
	{ "<e",'(', 2, evaluate_arithmetic_expression, NULL },
	{ ">", '>', 2, evaluate_arithmetic_expression, NULL },
	{ ">e",')', 2, evaluate_arithmetic_expression, NULL },
	{ "<<",'1', 2, evaluate_arithmetic_expression, NULL },
	{ ">>",'2', 2, evaluate_arithmetic_expression, NULL },
	{ "^", '3',-2, evaluate_arithmetic_expression, NULL },
	{ "=", '=', 2, evaluate_arithmetic_expression, NULL },
	{ "<>",'!', 2, evaluate_arithmetic_expression, NULL },
	{ "|", ':',-2, evaluate_arithmetic_expression, NULL },
	{ "~", 1, 1, evaluate_unary_expression, NULL },
#if defined( FLOATING_POINT )
	{ "$div",'0',-2, evaluate_arithmetic_expression, NULL },
	{ "$log", 2, 1, evaluate_unary_expression, NULL },
	{ "$exp", 3, 1, evaluate_unary_expression, NULL },
	{ "$sqrt",4, 1, evaluate_unary_expression, NULL },
	{ "$sin", 5, 1, evaluate_unary_expression, NULL },
	{ "$cos", 6, 1, evaluate_unary_expression, NULL },
	{ "$tan", 7, 1, evaluate_unary_expression, NULL },
	{ "$asin",8, 1, evaluate_unary_expression, NULL },
	{ "$acos",9, 1, evaluate_unary_expression, NULL },
	{ "$atan",10,1, evaluate_unary_expression, NULL },
#endif
	{ "!", '!', 1, evaluate_logical_expression, NULL },
	{ "&&",'&',-2, evaluate_logical_expression, NULL },
	{ "||",'|',-2, evaluate_logical_expression, NULL },
	{ "?", '?', 3, evaluate_ternary_expression, NULL },
	{ "$same", '$', 2, evaluate_same_expression, NULL },
	{ "$eq", '$', 2, evaluate_eq_expression, NULL },
	{ "$str", '$',-1, evaluate_str_expression, NULL },
	{ "$cmp", '$', 2, evaluate_cmp_expression, NULL },
	{ "$cat", '$',-1, evaluate_str_expression, NULL },
	{ "$chr", '$', 2, evaluate_chr_expression, NULL },
	{ "$sub", '$', 3, evaluate_sub_expression, NULL },
	{ "$asc", '$', 1, evaluate_asc_expression, NULL },
	{ "$hex", '$', 1, evaluate_hex_expression, NULL },
	{ "$int", '$', 1, evaluate_int_expression, NULL },
	{ "$num", '$', 1, evaluate_num_expression, NULL },
	{ "$len", '$', 1, evaluate_len_expression, NULL },
	{ "$tup", '$', 2, evaluate_tup_expression, NULL },
	{ "$array", '$', -1, evaluate_array_expression, NULL },
	{ "$new", '$', -1, evaluate_array_expression, NULL },
	{ "$load", '$',-1, evaluate_load_expression, NULL },
	{ "$read", '$', 1, evaluate_read_expression, NULL },
	{ "$flen", '$', 1, evaluate_flen_expression, NULL },
	{ "$stridx", '$', 3, evaluate_stridx_expression, NULL },
	{ "$endidx", '$', 2, evaluate_stridx_expression, NULL },
	{ "$argc", '$', 0, evaluate_argc_expression, NULL },
	{ "$argv", '$', 1, evaluate_argv_expression, NULL },
	{ "$time", '$', 0, evaluate_time_expression, NULL },
	{ "$parse", '$', 1, evaluate_parse_expression, NULL },
	{ "$unparse", '$', 1, evaluate_unparse_expression, NULL },
	{ "$next", '$', 1, evaluate_next_expression, NULL },
	{ "$child", '$', 1, evaluate_child_expression, NULL },
	{ "$line", '$', 1, evaluate_line_expression, NULL },
	{ "$elem", '$', 3, evaluate_elem_expression, NULL },
	{ "$expr", '$', 1, evaluate_expr_expression, NULL },
	{ "$pack", '$', 1, evaluate_pack_expression, NULL },
	{ "$unpack", '$', 2, evaluate_unpack_expression, NULL },
	{ "$quote", '$', 1, evaluate_quote_expression, NULL },
	{ "$unquote", '$', 1, evaluate_unquote_expression, NULL },
	{ "$interrupted", '$', 0, evaluate_interrupted_expression, NULL },
	{ "$function", '$', 1, evaluate_function_expression, NULL },
	{ "$buffer", 'B', -1, evaluate_array_expression, NULL },
	{ "$src", '$', 1, evaluate_source_expression, NULL },
	{ "$type", '$', 1, evaluate_type_expression, NULL },
	{ "$field", '$', 2, evaluate_field_expression, NULL },
	{ "$instanceof", '$', 2, evaluate_instanceof_expression, NULL },
	{ "$trace", '$', 1, evaluate_trace_expression, NULL },
	{ "$eval", '$', 1, evaluate_eval_expression, NULL },
	{ NULL }
};

static struct keyword library_decls[] = {
	{ "function", "n({", parse_function_declaration, &library_decls[ 1 ] },
	{ "struct", "n", parse_struct_declaration, &library_decls[ 2 ] },
	{ "const", "V;", parse_const_declaration, &library_decls[ 3 ] },
	{ "global", "V;", parse_global_declaration, &library_decls[ 4 ] },
	{ "rem", "{", parse_comment, NULL }
};

static struct element* parse_library_declaration( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message ) {
	struct function *init;
	struct element *next = elem->next;
	struct string *library = get_decl_indexed( func, next->str.string, next->str.length, LIBRARY );
	if( library ) {
		library->reference_count++;
	} else {
		library = new_string_value( next->str.string );
		if( library ) {
			library->type = LIBRARY;
			add_decl( library, next->line, func->env, message );
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	}
	if( message[ 0 ] == 0 ) {
		if( import_library( library, func->file, message ) ) {
			init = new_function( "[Init]", NULL );
			if( init ) {
				init->file = func->file;
				init->file->reference_count++;
				init->library = library;
				library->reference_count++;
				init->env = func->env;
				next = next->next;
				parse_keywords( library_decls, next->child, init, NULL, NULL, message );
				next = next->next;
				unref_string( &init->str );
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		}
		unref_string( library );
	}
	return next;
}

static struct keyword declarations[] = {
	{ "import", "nf\";", parse_import, &declarations[ 1 ] },
	{ "include", "\";", parse_include, &declarations[ 2 ] },
	{ "library", "n{", parse_library_declaration, &declarations[ 3 ] },
	{ "program", "n{", parse_program_declaration, library_decls }
};

static int validate_name( struct element *elem, char *message ) {
	int chr = elem->str.string[ 0 ], idx = 1, len = elem->str.length;
	int result = len < 64 && ( ( chr >= 'A' && chr <= 'Z') || ( chr >= 'a' && chr <= 'z' ) );
	if( result ) {
		/* First character must be alphabetical.*/
		while( idx < len ) {
			chr = elem->str.string[ idx++ ];
			if( !( chr == '_' || ( chr >= '0' && chr <= '9' )
			|| ( chr >= 'A' && chr <= 'Z' ) || ( chr >= 'a' && chr <= 'z' ) ) ) {
				/* Subsequent characters must be alphanumerical, or underscore. */
				result = 0;
				idx = len;
			}
		}
	}
	if( !result ) {
		sprintf( message, "Invalid name '%.64s' on line %d.", elem->str.string, elem->line );
	}
	return result;
}

static int validate_local( struct string *decl, int line, struct environment *env, char *message ) {
	int hash = hash_code( decl->string, 0 );
	if( decl->length >= 64 ) {
		sprintf( message, "Invalid name '%.64s' on line %d.", decl->string, line );
	} else if( get_keyword( decl->string, declarations )
	|| get_keyword( decl->string, env->statements_index[ hash ] )
	|| get_operator( decl->string, env->operators_index[ hash ] ) ) {
		sprintf( message, "Name '%.64s' already defined on line %d.", decl->string, line );
	} else {
		return hash + 1;
	}
	return 0;
}

static int validate_decl( struct string *decl, int line, struct environment *env, char *message ) {
	int hash = validate_local( decl, line, env, message );
	if( hash && get_decl( env->decls_index[ hash - 1 ], decl->string, decl->length, 0 ) ) {
		sprintf( message, "Name '%.64s' already declared on line %d.", decl->string, line );
	}
	return hash;
}

/* Parse the specified program text into env. Returns zero and writes message on failure. */
int parse_tt_program( char *program, struct string *file, struct environment *env, char *message ) {
	int idx;
	struct string_list *list;
	struct function *init, *func;
	struct element *elem = parse_element( program, env->element_depth, message );
	if( elem ) {
		/* Create empty function for global evaluation. */
		init = new_function( "[Init]", NULL );
		if( init ) {
			init->file = file;
			file->reference_count++;
			init->env = env;
			/* Populate execution environment. */
			parse_keywords( declarations, elem, init, NULL, NULL, message );
			/* Parse function bodies. */
			for( idx = 0; idx < 32; idx++ ) {
				list = env->decls_index[ idx ];
				while( list && message[ 0 ] == 0 ) {
					if( list->str->type == FUNCTION ) {
						func = ( struct function * ) list->str;
						if( func->body ) {
							parse_function_body( func, NULL, message );
							func->body = NULL;
						}
					}
					list = list->next;
				}
			}
			unref_string( &init->str );
		}
		unref_string( &elem->str );
	}
	if( env->interrupted ) {
		strcpy( message, "Interrupted." );
	}
	return message[ 0 ] == 0;
}

/* Parse the specified program file into env.
   Returns zero and writes up to 256 bytes to message on failure. */
int parse_tt_file( char *file_name, struct environment *env, char *message ) {
	int success = 0;
	struct string *path = ( struct string * ) new_source( strlen( file_name ) );
	if( path ) {
		strcpy( path->string, file_name );
		success = include_tt_file( path, env, message );
		unref_string( path );
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return success;
}

static enum result initialize_global( struct global_variable *global, struct variable *exception ) {
	enum result ret = OKAY;
	struct variables vars = { 0 };
	struct expression *init = global->initializer;
	vars.func = global->init_function;
	if( vars.func ) {
		if( init ) {
			vars.exception = exception;
			ret = init->evaluate( init, &vars, &global->value );
			dispose_expressions( init );
			global->initializer = NULL;
		}
		unref_string( &vars.func->str );
		global->init_function = NULL;
	}
	return ret;
}

/* Evaluate the global-variable initialization expressions for env.
   Returns zero and assigns exception on failure. */
int initialize_globals( struct environment *env, struct variable *exception ) {
	struct string *str;
	struct string_list *list = env->globals;
	while( list ) {
		str = list->str;
		if( str->type == GLOBAL || str->type == CONST ) {
			if( initialize_global( ( struct global_variable * ) str, exception ) == EXCEPTION ) {
				return 0;
			}
		}
		list = list->next;
	}
	return 1;
}

/* Add a copy of the specified null-terminated statement array to env.
   Returns zero and writes message on failure. */
int add_statements( struct keyword *statements, struct environment *env, char *message ) {
	int idx;
	struct keyword *statement;
	while( statements->name ) {
		statement = calloc( 1, sizeof( struct keyword ) );
		if( statement ) {
			memcpy( statement, statements, sizeof( struct keyword ) );
			idx = hash_code( statement->name, 0 );
			statement->next = env->statements_index[ idx ];
			env->statements_index[ idx ] = statement;
		} else {
			strcpy( message, OUT_OF_MEMORY );
			return 0;
		}
		statements++;
	}
	return 1;
}

/* Allocate a new operator, optionally initialized with the specified parameters. */
static struct operator* new_operator( char *name, struct operator *src ) {
	struct operator *oper;
	size_t size = sizeof( struct operator );
	if( name ) {
		size += sizeof( char ) * ( strlen( name ) + 1 );
	}
	oper = calloc( 1, size );
	if( oper ) {
		if( src ) {
			memcpy( oper, src, sizeof( struct operator ) );
		}
		if( name ) {
			oper->name = ( char * ) &oper[ 1 ];
			strcpy( oper->name, name );
		}
	}
	return oper;
}

/* Add a copy of the specified null-terminated operator array to env.
   Returns zero and writes message on failure. */
int add_operators( struct operator *operators, struct environment *env, char *message ) {
	int idx;
	struct operator *oper;
	while( operators->name ) {
		oper = new_operator( NULL, operators );
		if( oper ) {
			idx = hash_code( oper->name, 0 );
			oper->next = env->operators_index[ idx ];
			env->operators_index[ idx ] = oper;
			if( oper->name[ 0 ] == '$' && oper->name[ 1 ] > 96 ) {
				/* Add synonym. */
				oper = new_operator( &oper->name[ 1 ], oper );
				if( oper ) {
					oper->name[ 0 ] -= 32;
					idx = hash_code( oper->name, 0 );
					oper->next = env->operators_index[ idx ];
					env->operators_index[ idx ] = oper;
				} else {
					strcpy( message, OUT_OF_MEMORY );
					return 0;
				}
			}
		} else {
			strcpy( message, OUT_OF_MEMORY );
			return 0;
		}
		operators++;
	}
	return 1;
}

/* Initialize env with the the standard statements, operators and constants.
   The maximum available stack must be specified in bytes.
   Returns zero and writes message on failure. */
int initialize_environment( struct environment *env, size_t max_stack, char *message ) {
	memset( env, 0, sizeof( struct environment ) );
	env->element_depth = max_stack >> 10;
	return add_statements( statements, env, message )
		&& add_operators( operators, env, message )
		&& add_constants( constants, env, message );
}

/* Initialize expr to call the specified entry-point function when evaluated. */
void initialize_entry_point( struct function_expression *expr, struct function *func ) {
	func->env->stack_limit = ( size_t ) &expr - ( func->env->element_depth << 9 );
	memset( expr, 0, sizeof( struct function_expression ) );
	expr->expr.evaluate = evaluate_call_expression;
	expr->expr.line = func->line;
	expr->function = func;
}
