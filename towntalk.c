
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
	Towntalk (c)2022 Martin Cameron.

	A program file consists of a list of declarations.
	When a '#' character is encountered, the rest of the line is ignored.
	Variable and function names must be alphanumeric.
	Commas within name and argument lists are optional.
	A value is an integer with an associated reference.
	References may be null, strings, elements, arrays or functions.
	Non-null values may evaluate to zero in integer expressions.
	Strings are immutable and can be used as byte arrays.
	String literals may include the escape sequences "\"", "\\", and octal "\nnn".
	Buffers are arrays that cannot hold references but use much less memory.
	Dollar-expressions have an alternate form with an initial capital, eg. 'Time'.
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
			while <( a 10 ) {
				print a;
				let a = add( a 1 );
			}
		}

	Declarations:
		rem {}                   Comment (all brackets inside must be balanced).
		include "file.tt";       Include declarations from specified file.
		const name = expr;       Global constants.
		global a, b = expr;      Global variables.
		global ( struct ) a;     Global variable with associated struct.
		global [ a expr ];       Global variable initialized with array of specified length.
		struct s { a,b,c }       Layout for formatting arrays.
		struct t( s ) { d,e,f }  Struct with fields inherited from s.
		function f(param){stmts} Function declaration.
		program name{statements} Entry point function (no arguments).

	Statements:
		rem {}                   Comment.
		var a, b, c = expr;      Local variables.
		var ( struct ) a;        Local variable with associated struct.
		let var = expr;          Assign expression to local or global variable.
		let [ arr idx ] = expr;  Assign expression to array at specified index.
		let struc.f(arr) = expr; Assign expression to array at named field of specified struct.
		let var.field = expr;    Assign expr to array variable at named field of associated struct.
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
		set [ arr idx ] = expr;  Variable/Array assignment (same as let).
		inc a;                   Increment local variable.
		dec a;                   Decrement local variable.
		save str, "file";        Save bytes from string to file.
		append str, "file";      Append bytes to the end of file.
		lock worker {statements} Obtain specified worker lock and execute statements.
		locked {statements}      Obtain current worker lock and execute statements.

	Expressions:
		-123                     Decimal integer literal.
		0x100                    Hexadecimal integer literal.
		0888                     Octal integer literal.
		"String"                 String literal.
		${0,"1",$tup("2",3)}     Element literal.
		variable                 Value of named local or global variable.
		local++                  Value of named local variable prior to increment.
		local--                  Value of named local variable prior to decrement.
		function(expr ...)       Call function with specified args.
		[arr idx]                Array element.
		struct                   Structure reference.
		struct.field             Index of named struct field.
		struct.field(array)      Value of named field of specified array.
		variable.field           Value of named field of associated structure of specified array variable.
		@function                Function reference.
		:(func expr ...)         Call function reference with specified args.
		:struct.memb(this ...)   Call member-function. Equivalent to ":(struct.memb(this) this ...)", but this evaluated once.
		:variable.member(...)    Call member-function using associated structure. Equivalent to ":struct.member(variable ...)".
		variable:func(...)       Call static member-function using associated struct. Equivalent to "struct_func(variable ...)".
		'(expr operator ...)     Infix operator, eg '( 1 + 2 ).
		+(int int ...)           Addition.
		-(int int ...)           Subtraction.
		*(int int ...)           Multiplication.
		/(int int ...)           Division.
		%(int int ...)           Modulo division.
		<<(int int)              Arithmetic shift-left.
		>>(int int)              Arithmetic shift-right.
		=(int int)               Equality.
		<(int int)               Less than.
		<e(int int)              Less than or equal.
		>(int int)               Greater than.
		>e(int int)              Greater than or equal.
		&(int int ...)           Bitwise AND.
		|(int int ...)           Bitwise OR.
		^(int int ...)           Bitwise XOR.
		~(int)                   Bitwise NOT.
		!(expr)                  Evaluates to 1 if argument is null.
		&&(expr expr ...)        Evaluates to 1 if all arguments are non-null.
		||(expr expr ...)        Evaluates to 1 if any argument is non-null.
		?(expr expr expr)        Evaluates second expr if first is non-null, else third.
		$same(expr expr)         Evaluates to 1 if arguments have the same value.
		$cmp(str str)            String/Tuple comparison, evaluates to 0 if equal.
		$eq(expr expr)           Equivalent to !( $cmp( expr expr ) ).
		$str(str int ...)        Integer to string and string concatenation.
		$cat(str str ...)        String concatenation (same as $str).
		$chr(str idx)            Character at idx as integer.
		$sub(str off len)        Substring (or byte array to string).
		$asc(int)                Character code to string.
		$hex(int)                Integer to fixed-length signed hex string.
		$int(str)                String to integer.
		$len(str/arr)            Length of string, array or structure.
		$tup(str int)            String/Integer tuple.
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
		$values(array)           Return an element containing array values.
		$pack(int/arr)           Encode integers as big-endian byte string.
		$unpack(str idx)         Decode the specified big-endian integer.
		$quote(str)              Encode byte string with quotes and escapes.
		$unquote(str)            Decode quoted-string into byte string.
		$interrupted             Check and clear program interrupt status.
		$function(${(){stmts}})  Compile a function reference from an element.
		$worker(${(){stmts}})    Compile a worker function from an element.
		$execute(worker arg ...) Begin execution of the specified worker and return it.
		$result(worker)          Wait for the return value of a worker function.
		$buffer(len ...)         Create a numerical array that may be passed to workers.
		$type(expr)              Return a value representing the type of the expression.
		$field(struct idx)       Name of specified struct field.
		$instanceof(arr struct)  Returns arr if an instance of struct, null otherwise.
		$trace(message)          Return a stack-trace array of function and line-number tuples.
*/

struct global_assignment_statement {
	struct statement stmt;
	struct variable *destination;
};

struct block_statement {
	struct statement stmt;
	struct statement *if_block, *else_block;
};

struct structure_statement {
	struct statement stmt;
	struct structure *structure;
};

struct structure_expression {
	struct expression expr;
	struct structure *structure;
};

struct global_expression {
	struct expression expr;
	struct global_variable *global;
};

struct string_literal_expression {
	struct expression expr;
	struct string *str;
};

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
	struct function *func, struct statement *stmt, char *message );
static void parse_keywords_indexed( struct keyword **index, struct element *elem,
	struct function *func, struct statement *stmt, char *message );
static struct element* parse_expression( struct element *elem,
	struct function *func, struct expression *prev, char *message );
static struct element* parse_expressions( struct element *elem,
	struct function *func, char terminator, struct expression *prev, int *num_exprs, char *message );
static struct worker* parse_worker( struct element *elem, struct environment *env,
	char *file, char *message );

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
	size_t ints_size = length * sizeof( int );
	struct array *arr = calloc( 1, sizeof( struct array ) + refs_size + ints_size + str_len + 1 );
	if( arr ) {
		arr->string_values = ( struct string ** ) &arr[ 1 ];
		arr->integer_values = ( int * ) &arr->string_values[ length ];
		arr->str.string = ( char * ) &arr->integer_values[ length ];
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
	struct array *arr = calloc( 1, sizeof( struct array ) + sizeof( int ) * length );
	if( arr ) {
		arr->integer_values = ( int * ) &arr[ 1 ];
		arr->str.string = "[Buffer]";
		arr->str.length = 8;
		arr->str.reference_count = 1;
		arr->str.type = ARRAY;
		arr->length = length;
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

/* Return a 5-bit hash-code to be used for indexing the specified string. */
static int hash_code( char *str, char terminator ) {
	char chr = str[ 0 ];
	int idx = 1, hash = 0;
	while( chr != terminator && chr ) {
		hash = hash ^ chr;
		chr = str[ idx++ ];
	}
	return hash & 0x1F;
}

/* Return the first index of a member of chars in str, starting from idx. 
   If idx is negative, return the last index, starting from 0. */
static int str_idx( char *str, const char *chars, int idx ) {
	int last = 0, offset = -1, chr;
	if( idx < 0 ) {
		last = 1;
		idx = 0;
	}
	chr = str[ idx++ ];
	while( chr && ( offset < 0 || last ) ) {
		if( strchr( chars, chr ) ) {
			offset = idx - 1;
		}
		chr = str[ idx++ ];
	}
	return offset;
}

/* Return the length of str up to one the terminators. */
static size_t field_length( char *str, char *terminators ) {
	size_t len = 0;
	char chr = str[ 0 ];
	while( chr && strchr( terminators, chr ) == NULL ) {
		chr = str[ ++len ];
	}
	return len;
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
					memcpy( elem->str.string, &buffer[ offset ], sizeof( char ) * length );
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
						idx = parse_string( buffer, idx - 1, elem->str.string, line, message );
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

static void dispose_string_list( struct string_list *str ) {
	struct string_list *next;
	while( str ) {
		next = str->next;
		free( str );
		str = next;
	}
}

/* Decrement the reference-count of any referenced types,
   deallocate if necessary, and assign null to the specified variable.
   This must be called for all variables assigned during program execution. */
void dispose_variable( struct variable *var ) {
	var->integer_value = 0;
	if( var->string_value ) {
		unref_string( var->string_value );
		var->string_value = NULL;
	}
}

/* As dispose_variable(), but do not assign null for performance reasons.
   The resulting variable may contain an invalid pointer and should not be re-used. */
static void dispose_temporary( struct variable *var ) {
	if( var->string_value ) {
		unref_string( var->string_value );
	}
}

/* Assign src variable to dest, managing reference counts. */
void assign_variable( struct variable *src, struct variable *dest ) {
	if( dest->string_value ) {
		unref_string( dest->string_value );
	}
	dest->integer_value = src->integer_value;
	dest->string_value = src->string_value;
	if( dest->string_value ) {
		dest->string_value->reference_count++;
	}
}

/* Assign src variable to dest array at the specified index, managing reference counts. */
void assign_array_variable( struct variable *src, struct array *arr, int idx ) {
	struct variable var = { 0, NULL };
	assign_variable( src, &var );
	arr->integer_values[ idx ] = var.integer_value;
	if( arr->string_values ) {
		arr->string_values[ idx ] = var.string_value;
	} else {
		dispose_temporary( &var );
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
				result = memcmp( str1->string, str2->string, sizeof( char ) * str2->length );
			} else {
				result = memcmp( str1->string, str2->string, sizeof( char ) * str1->length );
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

static enum result evaluate_integer_literal_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	result->integer_value = this->index;
	return OKAY;
}

static enum result evaluate_string_literal_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	result->integer_value = this->index;
	result->string_value = ( ( struct string_literal_expression * ) this )->str;
	result->string_value->reference_count++;
	return OKAY;
}

static void dispose_expressions( struct expression *expr ) {
	struct expression *next;
	while( expr ) {
		next = expr->next;
		dispose_expressions( expr->parameters );
		if( expr->evaluate == evaluate_string_literal_expression ) {
			unref_string( ( ( struct string_literal_expression * ) expr )->str );
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

static void dispose_global_variables( struct global_variable *global ) {
	struct global_variable *next;
	while( global ) {
		next = global->next;
		dispose_temporary( &global->value );
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

static void dispose_functions( struct function *func ) {
	struct string *str;
	while( func ) {
		str = &func->str;
		if( str->reference_count == 1 ) {
			free( func->str.string );
			unref_string( func->file );
			dispose_local_variables( func->variable_decls );
			dispose_statements( func->statements );
			func = func->next;
			free( str );
		} else {
			str->reference_count--;
			func = NULL;
		}
	}
}

static void dispose_worker( struct worker *work ) {
	int idx, len = 0;
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
	free( work->globals );
	free( work->parameters );
	free( work );
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
			case FUNCTION:
				dispose_functions( ( struct function * ) str );
				break;
			case WORKER:
				await_worker( ( struct worker * ) str, 1 );
				dispose_worker( ( struct worker * ) str );
				break;
			case CUSTOM:
				( ( struct custom_type * ) str )->dispose( str );
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

static void dispose_structure_declarations( struct structure *sct ) {
	struct string *str;
	while( sct ) {
		str = &sct->str;
		if( str->reference_count == 1 ) {
			free( sct->str.string );
			dispose_string_list( sct->fields );
			sct = sct->next;
			free( str );
		} else {
			str->reference_count--;
			sct = NULL;
		}
	}
}

/* Deallocate the specified environment and all types referenced by it. */
void dispose_environment( struct environment *env ) {
	int idx;
	for( idx = 0; idx < 32; idx++ ) {
		dispose_global_variables( env->constants_index[ idx ] );
		dispose_global_variables( env->globals_index[ idx ] );
	}
	dispose_string_list( env->constants );
	dispose_string_list( env->globals );
	dispose_arrays( &env->arrays );
	for( idx = 0; idx < 32; idx++ ) {
		if( env->functions_index[ idx ] ) {
			unref_string( &env->functions_index[ idx ]->str );
		}
		dispose_structure_declarations( env->structures_index[ idx ] );
		dispose_keywords( env->statements_index[ idx ] );
		dispose_operators( env->operators_index[ idx ] );
	}
}

static struct string_list *new_string_list( char *value ) {
	struct string_list *str = malloc( sizeof( struct string_list ) + sizeof( char ) * ( strlen( value ) + 1 ) );
	if( str ) {
		str->value = ( char * ) &str[ 1 ];
		strcpy( str->value, value );
		str->next = NULL;
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

/* Assign an uncatchable exception with the specified exit code and message to vars and return EXCEPTION. */
enum result throw_exit( struct variables *vars, int exit_code, const char *message ) {
	struct variable *exception = vars->exception;
	struct environment *env = vars->func->env;
	env->exit.reference_count = 2;
	env->exit.type = EXIT;
	env->exit.string = ( char * ) message;
	dispose_temporary( exception );
	exception->integer_value = exit_code;
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
			arr->integer_values[ idx++ ] = line;
			line = vars->line;
			vars = vars->parent;
		}
		arr->length = idx;
	}
	return arr;
}

/* Assign an exception with the specified error code and message to vars and return EXCEPTION. */
enum result throw( struct variables *vars, struct expression *source, int integer, const char *string ) {
	struct variable *exception = vars->exception;
	struct string *file = vars->func->file;
	struct array *arr = NULL;
	if( string ) {
		arr = stack_trace( source, vars, strlen( string ) + 64, 16 );
		if( arr ) {
			if( sprintf( arr->str.string, "%s (on line %d of '%.32s')", string, source->line, file->string ) < 0 ) {
				strcpy( arr->str.string, string );
			}
			arr->str.length = strlen( arr->str.string );
		} else {
			return throw_exit( vars, 1, OUT_OF_MEMORY );
		}
	}
	dispose_temporary( exception );
	exception->integer_value = integer;
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

static struct function* new_function( char *name, char *file, char *message ) {
	struct function *func = calloc( 1, sizeof( struct function ) );
	if( func ) {
		/*printf("Function '%s'\n", name);*/
		func->str.string = new_string( name );
		if( func->str.string ) {
			func->str.length = strlen( func->str.string );
			func->str.reference_count = 1;
			func->str.type = FUNCTION;
			func->file = new_string_value( strlen( file ) );
			if( func->file ) {
				strcpy( func->file->string, file );
			}
		}
		if( !func->file ) {
			unref_string( &func->str );
			func = NULL;
		}
	}
	if( !func ) {
		strcpy( message, OUT_OF_MEMORY );
	}
	return func;
}

static struct local_variable* new_local_variable( int index, char *name, struct structure *type ) {
	struct local_variable *local = malloc( sizeof( struct local_variable ) + sizeof( char ) * ( strlen( name ) + 1 ) );
	if( local ) {
		local->index = index;
		local->name = ( char * ) &local[ 1 ];
		strcpy( local->name, name );
		local->type = type;
		local->next = NULL;
	}
	return local;
}

static struct global_variable* new_global_variable( char *name,
	struct structure *type, struct function *init_function, struct expression *initializer ) {
	struct global_variable *global = calloc( 1, sizeof( struct global_variable ) + sizeof( char ) * ( strlen( name ) + 1 ) );
	if( global ) {
		global->name = ( char * ) &global[ 1 ];
		strcpy( global->name, name );
		global->type = type;
		global->init_function = init_function;
		global->initializer = initializer;
		global->next = NULL;
	}
	return global;
}

static int add_constant( struct environment *env, struct global_variable *constant ) {
	int idx;
	struct string_list *name = new_string_list( constant->name );
	if( name ) {
		if( env->constants ){
			env->constants_tail->next = name;
		} else {
			env->constants = name;
		}
		env->constants_tail = name;
		idx = hash_code( name->value, 0 );
		constant->next = env->constants_index[ idx ];
		env->constants_index[ idx ] = constant;
	}
	return name != NULL;
}

static int add_global( struct environment *env, struct global_variable *global ) {
	int idx;
	struct string_list *name = new_string_list( global->name );
	if( name ) {
		if( env->globals ){
			env->globals_tail->next = name;
		} else {
			env->globals = name;
		}
		env->globals_tail = name;
		idx = hash_code( name->value, 0 );
		global->next = env->globals_index[ idx ];
		env->globals_index[ idx ] = global;
	}
	return name != NULL;
}

/* Add the specified constants to env. Returns zero and writes message on failure. */
int add_constants( struct constant *constants, struct environment *env, char *message ) {
	struct global_variable *global;
	while( constants->name && message[ 0 ] == 0 ) {
		global = new_global_variable( constants->name, NULL, NULL, NULL );
		if( global && add_constant( env, global ) ) {
			global->value.integer_value = constants->integer_value;
			if( constants->string_value ) {
				global->value.string_value = new_string_literal( constants->string_value );
				if( global->value.string_value == NULL ) {
					strcpy( message, OUT_OF_MEMORY );
				}
			}
		} else {
			dispose_global_variables( global );
			strcpy( message, OUT_OF_MEMORY );
		}
		constants++;
	}
	return message[ 0 ] == 0;
}

static enum result evaluate_local( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable *var = &vars->locals[ this->index ];
	result->integer_value = var->integer_value;
	if( var->string_value ) {
		result->string_value = var->string_value;
		result->string_value->reference_count++;
	}
	return OKAY;
}

static enum result evaluate_local_post_inc( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable *var = &vars->locals[ this->index ];
	if( var->string_value ) {
		return throw( vars, this, 0, "Not an integer." );
	} else {
		result->integer_value = var->integer_value++;
	}
	return OKAY;
}

static enum result evaluate_local_post_dec( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable *var = &vars->locals[ this->index ];
	if( var->string_value ) {
		return throw( vars, this, 0, "Not an integer." );
	} else {
		result->integer_value = var->integer_value--;
	}
	return OKAY;
}

static enum result evaluate_global( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable *var = &( ( struct global_expression * ) this )->global->value;
	result->integer_value = var->integer_value;
	if( var->string_value ) {
		result->string_value = var->string_value;
		result->string_value->reference_count++;
	}
	return OKAY;
}

static enum result execute_global_assignment( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct variable var = { 0 }, *dest = ( ( struct global_assignment_statement * ) this )->destination;
	enum result ret = this->source->evaluate( this->source, vars, &var );
	if( ret ) {
		dispose_temporary( dest );
		dest->integer_value = var.integer_value;
		dest->string_value = var.string_value;
	}
	return ret;
}

static enum result execute_local_assignment( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct variable var = { 0 }, *dest = &vars->locals[ this->local ];
	enum result ret = this->source->evaluate( this->source, vars, &var );
	if( ret ) {
		dispose_temporary( dest );
		dest->integer_value = var.integer_value;
		dest->string_value = var.string_value;
	}
	return ret;
}

static enum result execute_print_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct variable value = { 0, NULL };
	if( this->source->evaluate( this->source, vars, &value ) ) {
		if( value.string_value ) {
			puts( value.string_value->string );
		} else {
			printf( "%d\n", value.integer_value );
		}
		dispose_temporary( &value );
		return OKAY;
	}
	return EXCEPTION;
}

static enum result execute_error_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct variable value = { 0, NULL };
	if( this->source->evaluate( this->source, vars, &value ) ) {
		if( value.string_value ) {
			fputs( value.string_value->string, stderr );
			fputc( '\n', stderr );
		} else {
			fprintf( stderr, "%d\n", value.integer_value );
		}
		dispose_temporary( &value );
		return OKAY;
	}
	return EXCEPTION;
}

static enum result execute_write_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct variable value = { 0, NULL };
	if( this->source->evaluate( this->source, vars, &value ) ) {
		if( value.string_value ) {
			fwrite( value.string_value->string, 1, value.string_value->length, stdout );
		} else {
			printf( "%d", value.integer_value );
		}
		dispose_temporary( &value );
		return OKAY;
	}
	return EXCEPTION;
}

static enum result execute_throw_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct variable exc = { 0, NULL };
	this->source->evaluate( this->source, vars, &exc );
	dispose_temporary( vars->exception );
	vars->exception->integer_value = exc.integer_value;
	vars->exception->string_value = exc.string_value;
	return EXCEPTION;
}

static enum result execute_exit_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct variable exit_code = { 0, NULL };
	char *message = NULL;
	enum result ret = this->source->evaluate( this->source, vars, &exit_code );
	if( ret ) {
		if( vars->func->env->worker ) {
			message = "Worker exited.";
		}
		ret = throw_exit( vars, exit_code.integer_value, message );
		dispose_temporary( &exit_code );
	}
	return ret;
}

static enum result execute_return_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	if( this->source->evaluate( this->source, vars, result ) ) {
		return RETURN;
	}
	return EXCEPTION;
}

static enum result execute_break_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	return BREAK;
}

static enum result execute_continue_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	return CONTINUE;
}

static void dispose_block_statement( struct statement *this ) {
	dispose_statements( ( ( struct block_statement * ) this )->if_block );
	dispose_statements( ( ( struct block_statement * ) this )->else_block );
	free( this );
}

static enum result execute_try_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	enum result ret = OKAY;
	struct variables try_vars;
	struct statement *stmt = ( ( struct block_statement * ) this )->if_block;
	try_vars.parent = vars->parent;
	try_vars.exception = &vars->locals[ this->local ];
	try_vars.locals = vars->locals;
	try_vars.func = vars->func;
	try_vars.line = vars->line;
	while( stmt ) {
		ret = stmt->execute( stmt, &try_vars, result );
		if( ret == OKAY ) {
			stmt = stmt->next;
		} else {
			break;
		}
	}
	if( ret == EXCEPTION ) {
		if( try_vars.exception->string_value && try_vars.exception->string_value->type == EXIT ) {
			assign_variable( try_vars.exception, vars->exception );
		} else {
			ret = OKAY;
			stmt = ( ( struct block_statement * ) this )->else_block;
			while( stmt ) {
				ret = stmt->execute( stmt, vars, result );
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

static enum result execute_if_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct variable condition = { 0, NULL };
	struct statement *stmt = ( ( struct block_statement * ) this )->else_block;
	enum result ret = this->source->evaluate( this->source, vars, &condition );
	if( ret ) {
		if( condition.integer_value || condition.string_value ) {
			stmt = ( ( struct block_statement * ) this )->if_block;
		}
		dispose_temporary( &condition );
		while( stmt ) {
			ret = stmt->execute( stmt, vars, result );
			if( ret == OKAY ) {
				stmt = stmt->next;
			} else {
				break;
			}
		}
	}
	return ret;
}

static enum result execute_lock_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct statement *stmt = ( ( struct block_statement * ) this )->if_block;
	struct variable var = { 0, NULL };
	struct worker *work;
	char lock;
	enum result ret = this->source->evaluate( this->source, vars, &var );
	if( ret ) {
		if( var.string_value && var.string_value->type == WORKER ) {
			work = ( struct worker * ) var.string_value;
			lock = 1;
		} else {
			work = vars->func->env->worker;
			lock = 2;
		}
		if( work->locked != lock && lock_worker( work ) ) {
			work->locked = lock;
			while( stmt ) {
				ret = stmt->execute( stmt, vars, result );
				if( ret == OKAY ) {
					stmt = stmt->next;
				} else {
					break;
				}
			}
			work->locked = 0;
			if( unlock_worker( work ) == 0 ) {
				work->locked = lock;
				ret = throw( vars, this->source, 0, "Unable to unlock worker.");
			}
		} else {
			ret = throw( vars, this->source, 0, "Unable to lock worker.");
		}
		dispose_temporary( &var );
	}
	return ret;
}

static enum result execute_while_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct environment *env = vars->func->env;
	struct variable condition = { 0, NULL }, *lhs = NULL, *rhs = NULL;
	struct statement *stmt;
	enum result ret;
	if( this->local ) {
		lhs = &vars->locals[ this->source->parameters->index ];
		rhs = &vars->locals[ this->source->parameters->next->index ];
	}
	while( 1 ) {
		if( this->local == '<' ) {
			condition.integer_value = lhs->integer_value < rhs->integer_value;
		} else if( this->source->evaluate( this->source, vars, &condition ) ) {
			if( condition.string_value ) {
				dispose_temporary( &condition );
				condition.integer_value = 1;
				condition.string_value = NULL;
			}
		} else {
			break;
		}
		if( condition.integer_value ) {
			condition.integer_value = 0;
			stmt = ( ( struct block_statement * ) this )->if_block;
			while( stmt ) {
				ret = stmt->execute( stmt, vars, result );
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
				if( env->worker ) {
					return throw_exit( vars, 0, "Interrupted." );
				} else {
					return throw( vars, this->source, 0, "Interrupted.");
				}
			}
		} else {
			return OKAY;
		}
	}
	return EXCEPTION;
}

static enum result execute_call_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct variable var = { 0, NULL };
	enum result ret = this->source->evaluate( this->source, vars, &var );
	if( ret ) {
		dispose_temporary( &var );
	}
	return ret;
}

static enum result execute_array_assignment( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct array *arr;
	enum result ret = OKAY;
	struct variable dest = { 0, NULL }, idx = { 0, NULL }, src = { 0, NULL };
	struct expression *src_expr = this->source, *dest_expr = src_expr->next, *idx_expr = dest_expr->next;
	if( dest_expr->evaluate == evaluate_local ) {
		arr = ( struct array * ) vars->locals[ dest_expr->index ].string_value;
	} else {
		ret = dest_expr->evaluate( dest_expr, vars, &dest );
		arr = ( struct array * ) dest.string_value;
	}
	if( ret ) {
		if( arr && arr->str.type == ARRAY ) {
			if( idx_expr->evaluate == evaluate_local ) {
				idx.integer_value = vars->locals[ idx_expr->index ].integer_value;
			} else if( idx_expr->evaluate == evaluate_local_post_inc ) {
				idx.integer_value = vars->locals[ idx_expr->index ].integer_value++;
			} else {
				ret = idx_expr->evaluate( idx_expr, vars, &idx );
				dispose_temporary( &idx );
			}
			if( ret ) {
				if( ( unsigned int ) idx.integer_value < ( unsigned int ) arr->length ) {
					ret = src_expr->evaluate( src_expr, vars, &src );
					if( ret ) {
						arr->integer_values[ idx.integer_value ] = src.integer_value;
						if( arr->string_values ) {
							if( arr->string_values[ idx.integer_value ] ) {
								unref_string( arr->string_values[ idx.integer_value ] );
							}
							arr->string_values[ idx.integer_value ] = src.string_value;
						} else {
							dispose_temporary( &src );
						}
					}
				} else {
					ret = throw( vars, idx_expr, idx.integer_value, "Array index out of bounds." );
				}
			}
		} else {
			ret = throw( vars, dest_expr, 0, "Not an array." );
		}
		dispose_temporary( &dest );
	}
	return ret;
}

static struct structure* instance_of( struct string *str, struct structure *struc ) {
	struct structure *structure;
	if( str->type == ARRAY ) {
		structure = ( ( struct array * ) str )->structure;
		while( struc != structure && structure ) {
			structure = structure->super;
		}
		return structure;
	}
	return NULL;
}

static enum result execute_struct_assignment( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct array *arr;
	struct expression *destination = this->source->next;
	struct variable obj = { 0, NULL }, var = { 0, NULL };
	enum result ret = destination->evaluate( destination, vars, &obj );
	if( ret ) {
		if( obj.string_value && instance_of( obj.string_value, ( ( struct structure_statement * ) this )->structure ) ) {
			arr = ( struct array * ) obj.string_value;
			ret = this->source->evaluate( this->source, vars, &var );
			if( ret ) {
				arr->integer_values[ this->local ] = var.integer_value;
				if( arr->string_values ) {
					if( arr->string_values[ this->local ] ) {
						unref_string( arr->string_values[ this->local ] );
					}
					arr->string_values[ this->local ] = var.string_value;
				} else {
					dispose_temporary( &var );
				}
			}
		} else {
			ret = throw( vars, destination, obj.integer_value, "Not an instance of specified structure." );
		}
		dispose_temporary( &obj );
	}
	return ret;
}

static enum result execute_switch_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	int matched = 0;
	struct expression *case_expr;
	struct variable switch_value = { 0, NULL }, case_value = { 0, NULL };
	struct statement *cases = ( ( struct block_statement * ) this )->if_block;
	struct statement *deflt = ( ( struct block_statement * ) this )->else_block;
	enum result ret = this->source->evaluate( this->source, vars, &switch_value );
	if( ret ) {
		while( cases && ret && !matched ) {
			case_expr = cases->source;
			while( case_expr && ret && !matched ) {
				ret = case_expr->evaluate( case_expr, vars, &case_value );
				if( ret ) {
					matched = compare_variables( &switch_value, &case_value ) == 0;
					dispose_variable( &case_value );
					if( matched ) {
						ret = cases->execute( cases, vars, result );
					}
				}
				case_expr = case_expr->next;
			}
			cases = cases->next;
		}
		if( ret && !matched && deflt ) {
			ret = deflt->execute( deflt, vars, result );
		}
		dispose_temporary( &switch_value );
	}
	return ret;
}

static enum result execute_case_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct statement *stmt = ( ( struct block_statement * ) this )->if_block;
	enum result ret = OKAY;
	while( stmt ) {
		ret = stmt->execute( stmt, vars, result );
		if( ret == OKAY ) {
			stmt = stmt->next;
		} else {
			break;
		}
	}
	return ret;
}

static enum result execute_increment_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct variable *local = &vars->locals[ this->local ];
	if( local->string_value ) {
		return throw( vars, this->source, 0, "Not an integer." );
	}
	local->integer_value++;
	return OKAY;
}

static enum result execute_decrement_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	struct variable *local = &vars->locals[ this->local ];
	if( local->string_value ) {
		return throw( vars, this->source, 0, "Not an integer." );
	}
	local->integer_value--;
	return OKAY;
}

static enum result execute_save_statement( struct statement *this,
	struct variables *vars, struct variable *result ) {
	int count;
	char message[ 64 ];
	struct string *sval, *fval;
	struct variable str = { 0, NULL }, file = { 0, NULL };
	enum result ret = this->source->evaluate( this->source, vars, &str );
	if( ret ) {
		ret = this->source->next->evaluate( this->source->next, vars, &file );
		if( ret ) {
			sval = str.string_value;
			fval = file.string_value;
			if( sval && sval->string && fval && fval->string ) {
				count = save_file( fval->string, sval->string, sval->length, this->local, message );
				if( count != sval->length ) {
					ret = throw( vars, this->source, 0, message );
				}
			} else {
				ret = throw( vars, this->source, 0, "Not a string." );
			}
			dispose_temporary( &file );
		}
		dispose_temporary( &str );
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
	if( ret ) {
		stmt = call_vars.func->statements;
		while( stmt ) {
			ret = stmt->execute( stmt, &call_vars, result );
			if( ret == OKAY ) {
				stmt = stmt->next;
			} else if( ret == EXCEPTION ) {
				break;
			} else if( ret == RETURN ) {
				ret = OKAY;
				break;
			} else {
				ret = throw( vars, this, ret, "Unhandled 'break' or 'continue'." );
				break;
			}
		}
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
			ret = throw( vars, this, func.integer_value, "Not a function reference." );
		}
		dispose_temporary( &func );
	}
	return ret;
}

static enum result evaluate_thiscall_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int idx, count;
	struct array *arr;
	struct string *function = NULL;
	struct global_variable obj = { 0 };
	struct function_expression call_expr = { 0 };
	struct global_expression obj_expr = { 0 };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &obj.value );
	if( ret ) {
		if( obj.value.string_value && instance_of( obj.value.string_value, ( ( struct structure_expression * ) this )->structure ) ) {
			arr = ( struct array * ) obj.value.string_value;
			idx = this->index >> 8;
			count = this->index & 0xFF;
			if( arr->string_values ) {
				function = arr->string_values[ idx ];
			}
			if( function && function->type == FUNCTION ) {
				obj_expr.global = &obj;
				obj_expr.expr.evaluate = evaluate_global;
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
				ret = throw( vars, this, arr->integer_values[ idx ], "Not a function reference." );
			}
		} else {
			ret = throw( vars, this, obj.value.integer_value, "Not an instance of specified structure." );
		}
		dispose_temporary( &obj.value );
	}
	return ret;
}

static enum result evaluate_struct_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	result->integer_value = ARRAY + 1;
	result->string_value = &( ( struct structure_expression * ) this )->structure->str;
	result->string_value->reference_count++;
	return OKAY;
}

static enum result evaluate_type_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable var = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &var );
	if( ret && var.string_value ) {
		result->integer_value = var.string_value->type + 1;
		if( var.string_value->type == ARRAY ) {
			result->string_value = &( ( struct array * ) var.string_value )->structure->str;
			if( result->string_value ) {
				result->string_value->reference_count++;
			}
		}
		dispose_temporary( &var );
	}
	return ret;
}

static enum result evaluate_field_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable struc = { 0, NULL }, index = { 0, NULL };
	struct expression *parameter = this->parameters;
	struct string_list *field;
	int idx, len;
	enum result ret = parameter->evaluate( parameter, vars, &struc );
	if( ret ) {
		if( struc.string_value && struc.string_value->type == STRUCT ) {
			ret = parameter->next->evaluate( parameter->next, vars, &index );
			if( ret ) {
				len = ( ( struct structure * ) struc.string_value )->length;
				if( index.integer_value >= 0 && index.integer_value < len ) {
					field = ( ( struct structure * ) struc.string_value )->fields;
					for( idx = 0; idx < index.integer_value; idx++ ) {
						field = field->next;
					}
					result->string_value = new_string_value( strlen( field->value ) );
					if( result->string_value ) {
						strcpy( result->string_value->string, field->value );
					} else {
						ret = throw( vars, this, 0, OUT_OF_MEMORY );
					}
				} else {
					ret = throw( vars, this, index.integer_value, "Field index out of bounds." );
				}
				dispose_temporary( &index );
			}
		} else {
			ret = throw( vars, this, struc.integer_value, "Not a structure." );
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
				if( arr.string_value && instance_of( arr.string_value, ( struct structure * ) struc.string_value ) ) {
					assign_variable( &arr, result );
				}
			} else {
				ret = throw( vars, this, struc.integer_value, "Not a structure." );	
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
			result->integer_value = var.integer_value;
			result->string_value = &arr->str;
		} else {
			ret = throw( vars, this, 0, OUT_OF_MEMORY );
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
	enum result ret = parameter->evaluate( parameter, vars, &obj );
	if( ret ) {
		if( obj.string_value && instance_of( obj.string_value, ( ( struct structure_expression * ) this )->structure ) ) {
			arr = ( struct array * ) obj.string_value;
			result->integer_value = arr->integer_values[ this->index ];
			if( arr->string_values && arr->string_values[ this->index ] ) {
				result->string_value = arr->string_values[ this->index ];
				result->string_value->reference_count++;
			}
		} else {
			ret = throw( vars, this, obj.integer_value, "Not an instance of specified structure." );
		}
		dispose_temporary( &obj );
	}
	return ret;
}

static enum result evaluate_array_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression prev, *expr = this->parameters->next;
	int idx, len, buf = this->index == 'B';
	struct global_variable inputs, *input;
	struct variable var = { 0, NULL };
	struct structure *struc = NULL;
	char msg[ 128 ] = "";
	struct array *arr;
	enum result ret = this->parameters->evaluate( this->parameters, vars, &var );
	prev.next = NULL;
	if( ret ) {
		if( var.string_value && var.string_value->type == ELEMENT ) {
			if( expr == NULL ) {
				parse_expressions( ( struct element * ) var.string_value, vars->func, 0, &prev, &len, msg );
				expr = prev.next;
				if( msg[ 0 ] ) {
					ret = throw( vars, this, 0, msg );
				}
			} else {
				ret = throw( vars, expr, 0, "Unexpected expression." );
			}
		} else if( var.string_value && var.string_value->type == STRUCT && !buf ) {
			struc = ( struct structure * ) var.string_value;
			len = struc->length;
		} else {
			len = var.integer_value;
		}
	}
	if( ret ) {
		if( len >= 0 ) {
			if( buf ) {
				arr = new_buffer( len );
			} else {
				arr = new_array( vars->func->env, len, 0 );
				if( arr ) {
					arr->str.string = "[Array]";
					arr->str.length = 7;
				}
			}
			if( arr ) {
				idx = 0;
				while( expr && ret ) {
					if( idx < len ) {
						dispose_variable( &var );
						ret = expr->evaluate( expr, vars, &var );
						if( ret ) {
							assign_array_variable( &var, arr, idx++ );
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
				ret = throw( vars, this, 0, OUT_OF_MEMORY );
			}
		} else {
			ret = throw( vars, this, var.integer_value, "Invalid array length." );
		}
	}
	dispose_temporary( &var );
	if( prev.next ) {
		dispose_expressions( prev.next );
	}
	return ret;
}

static enum result evaluate_index_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct array *arr;
	enum result ret = OKAY;
	struct expression *parameter = this->parameters;
	struct variable src = { 0, NULL }, idx = { 0, NULL };
	if( parameter->evaluate == evaluate_local ) {
		arr = ( struct array * ) vars->locals[ parameter->index ].string_value;
	} else {
		ret = parameter->evaluate( parameter, vars, &src );
		arr = ( struct array * ) src.string_value;
	}
	if( ret ) {
		if( arr && arr->str.type == ARRAY ) {
			parameter = parameter->next;
			if( parameter->evaluate == evaluate_local ) {
				idx.integer_value = vars->locals[ parameter->index ].integer_value;
			} else if( parameter->evaluate == evaluate_local_post_inc ) {
				idx.integer_value = vars->locals[ parameter->index ].integer_value++;
			} else {
				ret = parameter->evaluate( parameter, vars, &idx );
				dispose_temporary( &idx );
			}
			if( ret ) {
				if( ( unsigned int ) idx.integer_value < ( unsigned int ) arr->length ) {
					result->integer_value = arr->integer_values[ idx.integer_value ];
					if( arr->string_values && arr->string_values[ idx.integer_value ] ) {
						result->string_value = arr->string_values[ idx.integer_value ];
						result->string_value->reference_count++;
					}
				} else {
					ret = throw( vars, this, idx.integer_value, "Array index out of bounds." );
				}
			}
		} else {
			ret = throw( vars, this, 0, "Not an array." );
		}
		dispose_temporary( &src );
	}
	return ret;
}

static struct structure* get_structure( struct structure *struc, char *name, size_t len ) {
	while( struc && ( strncmp( struc->str.string, name, len ) || strlen( struc->str.string ) != len ) ) {
		struc = struc->next;
	}
	return struc;
}

static struct structure* get_structure_indexed( struct structure **index, char *name ) {
	return get_structure( index[ hash_code( name, '.' ) ], name, field_length( name, "." ) );
}

static struct function* get_function( struct function *func, char *name ) {
	while( func && strcmp( func->str.string, name ) ) {
		func = func->next;
	}
	return func;
}

static struct function* get_function_indexed( struct function **index, char *name ) {
	return get_function( index[ hash_code( name, 0 ) ], name );
}

static struct local_variable* get_local_variable( struct local_variable *locals, char *name, char *terminators ) {
	size_t len = field_length( name, terminators );
	while( locals && ( strncmp( locals->name, name, len ) || strlen( locals->name ) != len ) ) {
		locals = locals->next;
	}
	return locals;
}

static struct global_variable* get_global( struct global_variable *global, char *name, size_t len ) {
	while( global && ( strncmp( global->name, name, len ) || strlen( global->name ) != len ) ) {
		global = global->next;
	}
	return global;
}

static struct global_variable* get_global_indexed( struct global_variable **index, char *name ) {
	size_t len = field_length( name, ".:" );
	return get_global( index[ hash_code( name, name[ len ] ) ], name, len );
}

static int add_global_constant( struct element *elem,
	struct function *func, struct structure *type, struct expression *initializer,
	struct statement *prev, char *message ) {
	struct global_variable *global = new_global_variable( elem->str.string, type, func, initializer );
	if( global == NULL || !add_constant( func->env, global ) ) {
		dispose_global_variables( global );
		strcpy( message, OUT_OF_MEMORY );
	}
	return message[ 0 ] == 0;
}

static int add_global_variable( struct element *elem,
	struct function *func, struct structure *type, struct expression *initializer,
	struct statement *prev, char *message ) {
	struct global_variable *global = new_global_variable( elem->str.string, type, func, initializer );
	if( global == NULL || !add_global( func->env, global ) ) {
		dispose_global_variables( global );
		strcpy( message, OUT_OF_MEMORY );
	}
	return message[ 0 ] == 0;
}

static int add_local_variable( struct element *elem,
	struct function *func, struct structure *type, struct expression *initializer,
	struct statement *prev, char *message ) {
	struct statement *stmt;
	char *name = elem->str.string;
	struct local_variable *param = new_local_variable( func->num_variables, name, type );
	if( param ) {
		/*printf("Local variable '%s'\n", name);*/
		if( get_local_variable( func->variable_decls, name, "" ) == NULL ) {
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
					prev->next = stmt;
					stmt->source = initializer;
					stmt->local = func->num_variables - 1;
					stmt->execute = execute_local_assignment;
				} else {
					strcpy( message, OUT_OF_MEMORY );
				}
			}
		} else {
			dispose_local_variables( param );
			sprintf( message, "Local variable '%.64s' already defined on line %d.", name, elem->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return message[ 0 ] == 0;
}

static struct element* parse_variable_declaration( struct element *elem, struct function *func, struct statement *prev,
	int (*add)( struct element *elem, struct function *func, struct structure *type, struct expression *initializer, struct statement *prev, char *message ),
	char *message ) {
	struct structure *type = NULL;
	struct expression expr = { 0 }, *array_expr;
	struct element *next;
	while( elem && elem->str.string[ 0 ] != ';' && message[ 0 ] == 0 ) {
		if( prev && prev->next ) {
			prev = prev->next;
		}
		if( elem->str.string[ 0 ] == '(' ) {
			type = get_structure_indexed( func->env->structures_index, elem->child->str.string );
			if( type != NULL ) {
				elem = elem->next;
			} else {
				sprintf( message, "Structure '%.64s' not declared on line %d.", elem->child->str.string, elem->child->line );
			}
		}
		if( message[ 0 ] == 0 ) {
			if( elem->str.string[ 0 ] == '[' && validate_decl( elem->child, func->env, message ) ) {
				parse_expression( elem->child->next, func, &expr, message );
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
			} else if( validate_decl( elem, func->env, message ) ) {
				if( elem->next->str.string[ 0 ] == '=' ) {
					next = parse_expression( elem->next->next, func, &expr, message );
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
	struct function *func, struct statement *prev, char *message ) {
	return elem->next->next;
}

static struct element* parse_const_declaration( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	return parse_variable_declaration( elem->next, func, prev, add_global_constant, message);
}

static struct element* parse_global_declaration( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	return parse_variable_declaration( elem->next, func, prev, add_global_variable, message);
}

static struct element* parse_local_declaration( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	return parse_variable_declaration( elem->next, func, prev, add_local_variable, message);
}

static enum result evaluate_bitwise_not_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct variable var = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &var );
	if( ret ) {
		result->integer_value = ~var.integer_value;
		dispose_temporary( &var );
	}
	return ret;
}

static enum result evaluate_logical_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int value;
	struct variable var = { 0, NULL };
	struct expression *parameter = this->parameters;
	enum result ret = parameter->evaluate( parameter, vars, &var );
	if( ret ) {
		value = var.integer_value || var.string_value;
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
							value = var.integer_value || var.string_value;
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
							value = var.integer_value || var.string_value;
							dispose_temporary( &var );
						} else {
							return ret;
						}
					}
				}
			}
		}
		result->integer_value = value;
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
		if( !( condition.integer_value || condition.string_value ) ) {
			parameter = parameter->next;
		}
		ret = parameter->evaluate( parameter, vars, result );
		dispose_temporary( &condition );
	}
	return ret;
}

static enum result evaluate_arithmetic_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable var;
	enum result ret;
	int lhs, rhs, oper = this->index;
	if( parameter->evaluate == evaluate_local ) {
		lhs = vars->locals[ parameter->index ].integer_value;
	} else {
		var.integer_value = 0;
		var.string_value = NULL;
		ret = parameter->evaluate( parameter, vars, &var );
		if( ret ) {
			lhs = var.integer_value;
			dispose_temporary( &var );
		} else {
			return ret;
		}
	}
	while( parameter->next ) {
		parameter = parameter->next;
		if( parameter->evaluate == evaluate_integer_literal_expression ) {
			rhs = parameter->index;
		} else if( parameter->evaluate == evaluate_local ) {
			rhs = vars->locals[ parameter->index ].integer_value;
		} else {
			var.integer_value = 0;
			var.string_value = NULL;
			ret = parameter->evaluate( parameter, vars, &var );
			if( ret ) {
				rhs = var.integer_value;
				dispose_temporary( &var );
			} else {
				return ret;
			}
		}
		evaluate:
		switch( oper & 0xFF ) {
			case 0:
				oper = this->index;
				goto evaluate;
			case '!': lhs = lhs != rhs; break;
			case '%':
				if( rhs != 0 ) {
					lhs = lhs % rhs;
				} else {
					return throw( vars, this, 0, "Modulo division by zero." );
				}
				break;
			case '&': lhs = lhs  & rhs; break;
			case '(': lhs = lhs <= rhs; break;
			case ')': lhs = lhs >= rhs; break;
			case '*': lhs = lhs  * rhs; break;
			case '+': lhs = lhs  + rhs; break;
			case '-': lhs = lhs  - rhs; break;
			case '/':
				if( rhs != 0 ) {
					lhs = lhs / rhs;
				} else {
					return throw( vars, this, 0, "Integer division by zero." );
				}
				break;
			case '1': lhs = lhs << rhs; break;
			case '2': lhs = lhs >> rhs; break;
			case '3': lhs = lhs  ^ rhs; break;
			case ':': lhs = lhs  | rhs; break;
			case '<': lhs = lhs  < rhs; break;
			case '=': lhs = lhs == rhs; break;
			case '>': lhs = lhs  > rhs; break;
			default :
				return throw( vars, this, 0, "Unhandled integer operator." );
		}
		oper = oper >> 8;
	}
	result->integer_value = lhs;
	return OKAY;
}

static enum result evaluate_int_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int val;
	char *end;
	struct variable str = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &str );
	if( ret ) {
		if( str.string_value ) {
			errno = 0;
			val = ( int ) strtoul( str.string_value->string, &end, 0 );
			if( end[ 0 ] || errno ) {
				ret = throw( vars, this, 0, "Unable to convert string to integer." );
			} else {
				result->integer_value = val;
			}
		} else {
			ret = throw( vars, this, 0, "Not a string." );
		}
		dispose_temporary( &str );
	}
	return ret;
}

static enum result evaluate_str_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int str_len = 0, len;
	char num[ 24 ], *val;
	enum result ret = OKAY;
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct string *str = NULL, *new;
	while( parameter && ret ) {
		ret = parameter->evaluate( parameter, vars, &var );
		if( ret ) {
			if( var.string_value ) {
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
						memcpy( new->string, str->string, sizeof( char ) * str_len );
						free( str );
					}
					memcpy( &new->string[ str_len ], val, sizeof( char ) * len );
					str_len += len;
					str = new;
				} else {
					ret = throw( vars, this, 0, OUT_OF_MEMORY );
				}
			} else {
				ret = throw( vars, this, len, "String too large." );
			}
			dispose_variable( &var );
			parameter = parameter->next;
		}
	}
	if( ret ) {
		result->string_value = str;
	} else {
		free( str );
	}
	return ret;
}

static enum result evaluate_asc_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct string *str;
	struct variable val = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &val );
	if( ret ) {
		str = new_string_value( 1 );
		if( str ) {
			str->string[ 0 ] = val.integer_value;
			result->string_value = str;
		} else {
			ret = throw( vars, this, 0, OUT_OF_MEMORY );
		}
		dispose_temporary( &val );
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
		ret = this->parameters->evaluate( this->parameters, vars, &var );
		str = var.string_value;
	}
	if( ret ) {
		if( str ) {
			switch( str->type ) {
				default:
				case STRING:
					result->integer_value = str->length;
					break;
				case ARRAY:
					result->integer_value = ( ( struct array * ) str )->length;
					break;
				case STRUCT:
					result->integer_value = ( ( struct structure * ) str )->length;
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
	struct variable str = { 0, NULL }, val = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, vars, &str );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, vars, &val );
		if( ret ) {
			result->integer_value = val.integer_value;
			result->string_value = str.string_value;
			dispose_temporary( &val );
		} else {
			dispose_temporary( &str );
		}
	}
	return ret;
}

static enum result evaluate_read_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	size_t len;
	char message[ 64 ];
	struct string *str;
	struct variable count = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &count );
	if( ret ) {
		if( count.integer_value >= 0 ) {
			len = count.integer_value;
			str = new_string_value( len );
			if( str ) {
				clearerr( stdin );
				if( len > 0 ) {
					len = fread( str->string, 1, len, stdin );
					str->string[ len ] = 0;
					str->length = len;
				}
				if( ferror( stdin ) ) {
					free( str );
					strncpy( message, strerror( errno ), 63 );
					message[ 63 ] = 0;
					ret = throw( vars, this, 0, message );
				} else {
					result->string_value = str;
				}
			} else {
				ret = throw( vars, this, 0, OUT_OF_MEMORY );
			}
		} else {
			ret = throw( vars, this, count.integer_value, "Invalid length." );
		}
		dispose_temporary( &count );
	}
	return ret;
}

static enum result evaluate_load_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	long len;
	char message[ 64 ];
	struct string *str;
	struct expression *parameter = this->parameters;
	struct variable file = { 0, NULL }, offset = { 0, NULL }, count = { -1, NULL };
	enum result ret = parameter->evaluate( parameter, vars, &file );
	if( ret ) {
		if( file.string_value ) {
			parameter = parameter->next;
			if( parameter ) {
				ret = parameter->evaluate( parameter, vars, &offset );
				parameter = parameter->next;
			}
			if( ret && parameter ) {
				ret = parameter->evaluate( parameter, vars, &count );
				if( ret && parameter->next ) {
					ret = throw( vars, this, 0, "Too many parameters." );
				}
			}
			if( ret ) {
				len = load_file( file.string_value->string, NULL, offset.integer_value, 0, message );
				if( len >= 0 ) {
					if( count.integer_value >= 0 && count.integer_value < len ) {
						len = count.integer_value;
					}
					if( len < MAX_INTEGER ) {
						str = new_string_value( len );
						if( str ) {
							len = load_file( file.string_value->string, str->string, offset.integer_value, len, message );
							if( len >= 0 ) {
								str->length = len;
								str->string[ len ] = 0;
								result->string_value = str;
							} else {
								free( str );
								ret = throw( vars, this, 0, message );
							}
						} else {
							ret = throw( vars, this, 0, OUT_OF_MEMORY );
						}
					} else {
						ret = throw( vars, this, 0, "File too large." );
					}
				} else {
					ret = throw( vars, this, 0, message );
				}
			}
		} else {
			ret = throw( vars, this, 0, "Not a string." );
		}
		dispose_temporary( &file );
		dispose_temporary( &offset );
		dispose_temporary( &count );
	}
	return ret;
}

static enum result evaluate_flen_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	long len;
	char message[ 64 ];
	struct variable file = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &file );
	if( ret ) {
		if( file.string_value ) {
			len = load_file( file.string_value->string, NULL, 0, 0, message );
			if( len >= 0 ) {
				if( len < MAX_INTEGER ) {
					result->integer_value = len;
				} else {
					ret = throw( vars, this, 0, "File too large." );
				}
			} else {
				ret = throw( vars, this, 0, message );
			}
		} else {
			ret = throw( vars, this, 0, "Not a string." );
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
			result->integer_value = compare_variables( &var1, &var2 );
			dispose_temporary( &var2 );
		}
		dispose_temporary( &var1 );
	}
	return ret;
}

static enum result evaluate_chr_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct string *str;
	enum result ret = OKAY;
	struct expression *parameter = this->parameters;
	struct variable src = { 0, NULL }, idx = { 0, NULL };
	if( parameter->evaluate == evaluate_local ) {
		str = vars->locals[ parameter->index ].string_value;
	} else {
		ret = parameter->evaluate( parameter, vars, &src );
		str = src.string_value;
	}
	if( ret ) {
		parameter = parameter->next;
		if( parameter->evaluate == evaluate_local ) {
			idx.integer_value = vars->locals[ parameter->index ].integer_value;
		} else if( parameter->evaluate == evaluate_local_post_inc ) {
			idx.integer_value = vars->locals[ parameter->index ].integer_value++;
		} else {
			ret = parameter->evaluate( parameter, vars, &idx );
			dispose_temporary( &idx );
		}
		if( ret ) {
			if( str ) {
				if( ( unsigned int ) idx.integer_value < ( unsigned int ) str->length ) {
					result->integer_value = ( signed char ) str->string[ idx.integer_value ];
				} else {
					ret = throw( vars, this, idx.integer_value, "String index out of bounds." );
				}
			} else {
				ret = throw( vars, this, 0, "Not a string." );
			}
		}
		dispose_temporary( &src );
	}
	return ret;
}

static enum result evaluate_sub_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	char *data;
	int offset, length, *arr;
	struct string *str;
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL }, idx = { 0, NULL }, len = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, vars, &var );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, vars, &idx );
		if( ret ) {
			parameter = parameter->next;
			ret = parameter->evaluate( parameter, vars, &len );
			if( ret ) {
				if( var.string_value ) {
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
								arr = ( ( struct array * ) var.string_value )->integer_values;
								offset = 0;
								while( offset < len.integer_value ) {
									data[ offset ] = arr[ offset + idx.integer_value ];
									offset++;
								}
							} else {
								memcpy( str->string, &var.string_value->string[ idx.integer_value ],
									sizeof( char ) * len.integer_value );
							}
							result->string_value = str;
						} else {
							ret = throw( vars, this, 0, OUT_OF_MEMORY );
						}
					} else {
						ret = throw( vars, this, idx.integer_value, "Range out of bounds." );
					}
				} else {
					ret = throw( vars, this, 0, "Not a string or array." );
				}	
				dispose_temporary( &len );
			}
			dispose_temporary( &idx );
		}
		dispose_temporary( &var );
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
	struct variable var;
	while( idx < count ) {
		var.integer_value = arr->integer_values[ idx ];
		var.string_value = arr->string_values ? arr->string_values[ idx ] : NULL;
		if( var.string_value ) {
			if( var.string_value->type == ELEMENT ) {
				elem = new_literal_element( ( struct element * ) var.string_value );
			} else if( var.integer_value ) {
				elem = new_tuple_element( var.integer_value, var.string_value );
			} else {
				elem = new_string_element( var.string_value );
			}
		} else {
			elem = new_integer_element( var.integer_value );
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
		idx++;
	}
	return head;
}

static enum result evaluate_values_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable arr = { 0, NULL };
	struct element *elem;
	enum result ret = parameter->evaluate( parameter, vars, &arr );
	if( ret ) {
		if( arr.string_value && arr.string_value->type == ARRAY
		&& ( ( struct array * ) arr.string_value )->length > 0 ) {
			elem = array_to_element( ( struct array * ) arr.string_value );
			if( elem ) {
				result->string_value = &elem->str;
			} else {
				ret = throw( vars, this, 0, OUT_OF_MEMORY );
			}
		} else {
			ret = throw( vars, this, arr.integer_value, "Not an array or no values." );
		}
		dispose_temporary( &arr );
	}
	return ret;
}

static enum result evaluate_argc_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	result->integer_value = vars->func->env->argc;
	return OKAY;
}

static enum result evaluate_argv_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	char *val;
	enum result ret;
	struct string *str;
	struct expression *parameter = this->parameters;
	struct variable idx = { 0, NULL };
	ret = parameter->evaluate( parameter, vars, &idx );
	if( ret ) {
		if( idx.integer_value >= 0 && idx.integer_value < vars->func->env->argc ) {
			val = vars->func->env->argv[ idx.integer_value ];
			str = new_string_value( strlen( val ) );
			if( str ) {
				memcpy( str->string, val, sizeof( char ) * str->length );
				result->string_value = str;
			} else {
				ret = throw( vars, this, 0, OUT_OF_MEMORY );
			}
		} else {
			ret = throw( vars, this, idx.integer_value, "Command-line argument index out of bounds." );
		}
		dispose_temporary( &idx );
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
		str = new_string_value( strlen( time_str ) - 1 );
		if( str ) {
			memcpy( str->string, time_str, sizeof( char ) * str->length );
		} else {
			ret = throw( vars, this, 0, OUT_OF_MEMORY );
		}
	}
	if( ret ) {
		result->integer_value = seconds;
		result->string_value = str;
	}
	return ret;
}

static enum result evaluate_next_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable prev = { 0, NULL };
	struct element *next;
	enum result ret = parameter->evaluate( parameter, vars, &prev );
	if( ret ) {
		if( prev.string_value && prev.string_value->type == ELEMENT ) {
			next = ( ( struct element * ) prev.string_value )->next;
			if( next ) {
				next->str.reference_count++;
			}
			result->string_value = &next->str;
		} else {
			ret = throw( vars, this, 0, "Not an element." );
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
	enum result ret = parameter->evaluate( parameter, vars, &parent );
	if( ret ) {
		if( parent.string_value && parent.string_value->type == ELEMENT ) {
			child = ( ( struct element * ) parent.string_value )->child;
			if( child ) {
				child->str.reference_count++;
			}
			result->string_value = &child->str;
		} else {
			ret = throw( vars, this, 0, "Not an element." );
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
	enum result ret = parameter->evaluate( parameter, vars, &elem );
	if( ret ) {
		if( elem.string_value && elem.string_value->type == ELEMENT ) {
			parameter = parameter->next;
			ret = parameter->evaluate( parameter, vars, &child );
			if( ret ) {
				if( ( child.string_value && child.string_value->type == ELEMENT )
				|| !( child.string_value || child.integer_value ) ) {
					if( !child.string_value || strchr( "([{", elem.string_value->string[ 0 ] ) ) {
						parameter = parameter->next;
						ret = parameter->evaluate( parameter, vars, &next );
						if( ret ) {
							if( ( next.string_value && next.string_value->type == ELEMENT )
							|| !( next.string_value || next.integer_value ) ) {
								if( elem.string_value->reference_count > 1 ) {
									value = new_element( elem.string_value->length );
									if( value ) {
										value->line = ( ( struct element * ) elem.string_value )->line;
										memcpy( value->str.string, elem.string_value->string,
											sizeof( char ) * elem.string_value->length );
									} else {
										ret = throw( vars, this, 0, OUT_OF_MEMORY );
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
									result->string_value = &value->str;
								}
							} else {
								ret = throw( vars, this, next.integer_value, "Not an element." );
							}
							dispose_temporary( &next );
						}
					} else {
						ret = throw( vars, this, 0, "Parent elements must have value '()', '[]' or '{}'." );
					}
				} else {
					ret = throw( vars, this, child.integer_value, "Not an element." );
				}
				dispose_temporary( &child );
			}
		} else {
			ret = throw( vars, this, elem.integer_value, "Not an element." );
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
	enum result ret = parameter->evaluate( parameter, vars, &str );
	if( ret ) {
		if( str.string_value ) {
			elem = parse_element( str.string_value->string, message );
			if( message[ 0 ] == 0 ) {
				result->string_value = &elem->str;
			} else {
				ret = throw( vars, this, 0, message );
			}
		} else {
			ret = throw( vars, this, 0, "Not a string." );
		}
		dispose_temporary( &str );
	}
	return ret;
}

static enum result evaluate_unparse_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int len;
	struct string *str;
	struct expression *parameter = this->parameters;
	struct variable elem = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, vars, &elem );
	if( ret ) {
		if( elem.string_value && elem.string_value->type == ELEMENT ) {
			len = write_element( ( struct element * ) elem.string_value, NULL );
			if( len >= 0 ) {
				str = new_string_value( len );
				if( str ) {
					write_element( ( struct element * ) elem.string_value, str->string );
					result->string_value = str;
				} else {
					ret = throw( vars, this, 0, OUT_OF_MEMORY );
				}
			} else {
				ret = throw( vars, this, 0, "String too large." );
			}
		} else {
			ret = throw( vars, this, 0, "Not an element." );
		}
		dispose_temporary( &elem );
	}
	return ret;
}

static enum result evaluate_quote_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int length;
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct string *str;
	enum result ret = parameter->evaluate( parameter, vars, &var );
	if( ret ) {
		if( var.string_value ) {
			length = write_byte_string( var.string_value->string, var.string_value->length, NULL );
			if( length >= 0 ) {
				str = new_string_value( length );
				if( str ) {
					write_byte_string( var.string_value->string, var.string_value->length, str->string );
					result->string_value = str;
				} else {
					ret = throw( vars, this, 0, OUT_OF_MEMORY );
				}
			} else {
				ret = throw( vars, this, 0, "String too large." );
			}
		} else {
			ret = throw( vars, this, 0, "Not a string." );
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
	enum result ret = parameter->evaluate( parameter, vars, &var );
	if( ret ) {
		if( var.string_value ) {
			str = new_string_literal( var.string_value->string );
			if( str ) {
				result->string_value = str;
			} else {
				ret = throw( vars, this, 0, OUT_OF_MEMORY );
			}
		} else {
			ret = throw( vars, this, 0, "Not a string." );
		}
		dispose_temporary( &var );
	}
	return ret;
}

static enum result evaluate_line_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable elem = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, vars, &elem );
	if( ret ) {
		if( elem.string_value && elem.string_value->type == ELEMENT ) {
			result->integer_value = ( ( struct element * ) elem.string_value )->line;
		} else {
			ret = throw( vars, this, 0, "Not an element." );
		}
		dispose_temporary( &elem );
	}
	return ret;
}

static enum result evaluate_hex_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int len;
	struct string *str;
	struct variable val = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &val );
	if( ret ) {
		str = new_string_value( sizeof( int ) * 2 + 2 );
		if( str ) {
			len = sprintf( str->string, "0x%08X", val.integer_value );
			if( len > 0 ) {
				str->length = len;
				result->string_value = str;
			} else {
				free( str );
				ret = throw( vars, this, len, "Output error." );
			}
		} else {
			ret = throw( vars, this, 0, OUT_OF_MEMORY );
		}
		dispose_temporary( &val );
	}
	return ret;
}

static enum result evaluate_pack_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int idx, len, in, *src;
	char *out;
	struct string *str;
	struct variable val = { 0, NULL };
	enum result ret = this->parameters->evaluate( this->parameters, vars, &val );
	if( ret ) {
		if( val.string_value && val.string_value->type == ARRAY ) {
			src = ( ( struct array * ) val.string_value )->integer_values;
			len = ( ( struct array * ) val.string_value )->length * 4;
		} else {
			src = &val.integer_value;
			len = 4;
		}
		str = new_string_value( len );
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
			ret = throw( vars, this, 0, OUT_OF_MEMORY );
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

static enum result evaluate_unpack_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable str = { 0, NULL }, idx = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, vars, &str );
	if( ret ) {
		if( str.string_value ) {
			parameter = parameter->next;
			if( parameter->evaluate == evaluate_local ) {
				idx.integer_value = vars->locals[ parameter->index ].integer_value;
			} else {
				ret = parameter->evaluate( parameter, vars, &idx );
			}
			if( ret ) {
				if( idx.integer_value >= 0 && idx.integer_value * 4 < str.string_value->length - 3 ) {
					result->integer_value = unpack( str.string_value->string, idx.integer_value );
				} else {
					ret = throw( vars, this, idx.integer_value, "String index out of bounds." );
				}
				dispose_temporary( &idx );
			}
		} else {
			ret = throw( vars, this, 0, "Not a string." );
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
			result->integer_value = ( lhs.integer_value == rhs.integer_value )
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
			result->integer_value = compare_variables( &lhs, &rhs ) == 0;
			dispose_temporary( &rhs );
		}
		dispose_temporary( &lhs );
	}
	return ret;
}

static enum result evaluate_stridx_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable str = { 0, NULL }, sep = { 0, NULL }, idx = { 0, NULL };
	enum result ret = parameter->evaluate( parameter, vars, &str );
	if( ret ) {
		parameter = parameter->next;
		ret = parameter->evaluate( parameter, vars, &sep );
		if( ret ) {
			if( str.string_value && sep.string_value ) {
				if( parameter->next ) {
					parameter = parameter->next;
					ret = parameter->evaluate( parameter, vars, &idx );
				} else {
					idx.integer_value = -1;
				}
				if( ret ) {
					if( str.string_value->length > 0 && str.string_value->length > idx.integer_value ) {
						result->integer_value = str_idx( str.string_value->string, sep.string_value->string, idx.integer_value );
					} else {
						ret = throw( vars, this, idx.integer_value, "String index out of bounds." );
					}
					dispose_temporary( &idx );
				}
			} else {
				ret = throw( vars, this, 0, "Not a string." );
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
	result->integer_value = env->interrupted;
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

static void optimize_expression( struct expression *expr ) {
	int count;
	struct expression *param, *next;
	if( expr->evaluate == evaluate_arithmetic_expression && expr->parameters->next->next == NULL ) {
		/* Expression is a two-operand arithmetic expression. */
		param = expr->parameters;
		if( param->evaluate == evaluate_arithmetic_expression && ( ( param->index & 0xFF00 ) || param->parameters->next->next == NULL ) ) {
			/* Left-hand side is a combined, or two-operand arithmetic expression. */
			count = 0;
			next = param->parameters;
			while( next->next ) {
				next = next->next;
				count++;
			}
			if( count < 4 ) {
				/* Combine arithmetic expressions. */
				expr->index = param->index | ( expr->index << ( count * 8 ) );
				next->next = param->next;
				expr->parameters = param->parameters;
				free( param );
			}
		}
	}
}

static struct element* parse_infix_expression( struct element *elem,
	struct function *func, struct expression *prev, char *message ) {
	int count;
	struct operator *oper;
	struct element *next = elem->next;
	struct element *child = next->child;
	struct expression param = { 0 }, *expr = calloc( 1, sizeof( struct expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		if( child ) {
			child = parse_expression( child, func, &param, message );
			expr->parameters = param.next;
			if( message[ 0 ] == 0 ) {
				if( child ) {
					oper = get_operator( child->str.string, func->env->operators_index[ hash_code( child->str.string, 0 ) ] );
					if( oper ) {
						expr->index = oper->oper;
						expr->evaluate = oper->evaluate;
						if( oper->num_operands != 0 ) {
							parse_expressions( child->next, func, 0, expr->parameters, &count, message );
							if( message[ 0 ] == 0 ) {
								count++;
								if( count == oper->num_operands || ( oper->num_operands < 0 && count >= -oper->num_operands ) ) {
									optimize_expression( expr );
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
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_operator_expression( struct element *elem,
	struct function *func, struct expression *prev, char *message ) {
	int count;
	struct element *next = elem->next;
	struct operator *oper = get_operator( elem->str.string, func->env->operators_index[ hash_code( elem->str.string, 0 ) ] );
	struct expression param = { 0 }, *expr = calloc( 1, sizeof( struct expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		if( oper ) {
			expr->index = oper->oper;
			expr->evaluate = oper->evaluate;
			if( oper->num_operands != 0 ) {
				if( next && next->str.string[ 0 ] == '(' ) {
					parse_expressions( next->child, func, 0, &param, &count, message );
					expr->parameters = param.next;
					if( message[ 0 ] == 0 ) {
						if( count == oper->num_operands || ( oper->num_operands < 0 && count >= -oper->num_operands ) ) {
							optimize_expression( expr );
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
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_call_expression( struct element *elem,
	struct function *func, struct function *decl, struct expression *prev, char *message ) {
	int num_params;
	struct element *next = elem->next;
	struct expression param = { 0 }, *expr = calloc( 1, sizeof( struct function_expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		( ( struct function_expression * ) expr )->function = decl;
		if( next && next->str.string[ 0 ] == '(' ) {
			parse_expressions( next->child, func, 0, &param, &num_params, message );
			expr->parameters = param.next;
			if( message[ 0 ] == 0 ) {
				if( num_params == ( ( struct function_expression * ) expr )->function->num_parameters ) {
					expr->evaluate = evaluate_call_expression;
					next = next->next;
				} else {
					sprintf( message, "Wrong number of arguments to '%.64s()' on line %d.", elem->str.string, next->line );
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
		expr->function = get_function_indexed( func->env->functions_index, name );
		if( expr->function ) {
			expr->expr.evaluate = evaluate_func_ref_expression;
		} else {
			sprintf( message, "Function '%.64s' not defined on line %d.", name, elem->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return elem->next;
}

static struct element* parse_index_expression( struct element *elem,
	struct function *func, struct expression *prev, char *message ) {
	int num_params;
	struct expression param = { 0 }, *expr = calloc( 1, sizeof( struct expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		parse_expressions( elem->child, func, 0, &param, &num_params, message );
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
	struct function *func, struct structure *struc, struct expression *prev, char *message ) {
	int idx, count;
	struct element *next = elem->next;
	char *field = strchr( elem->str.string, '.' );
	struct expression param = { 0 }, *expr = calloc( 1, sizeof( struct structure_expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		( ( struct structure_expression * ) expr )->structure = struc;
		if( field ) {
			idx = get_string_list_index( struc->fields, &field[ 1 ] );
			if( idx >= 0 ) {
				expr->index = idx;
				if( next && next->str.string[ 0 ] == '(' ) {
					parse_expressions( next->child, func, 0, &param, &count, message );
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
					expr->evaluate = evaluate_integer_literal_expression;
				}
			} else {
				sprintf( message, "Field '%.64s' not declared on line %d.", elem->str.string, elem->line );
			}
		} else {
			expr->evaluate = evaluate_struct_expression;
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_refcall_expression( struct element *elem,
	struct function *func, struct expression *prev, char *message ) {
	int count;
	struct element *next = elem->next;
	struct expression param = { 0 }, *expr = calloc( 1, sizeof( struct expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		if( next && next->str.string[ 0 ] == '(' ) {
			parse_expressions( next->child, func, 0, &param, &count, message );
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
	struct function *func, struct expression *prev, char *message ) {
	int idx, count;
	struct structure *struc;
	struct element *next = elem->next;
	char *field = strchr( elem->str.string, '.' );
	struct local_variable *local = get_local_variable( func->variable_decls, &elem->str.string[ 1 ], "." );
	struct global_variable *global = get_global_indexed( func->env->globals_index, &elem->str.string[ 1 ] );
	struct expression param = { 0 }, *this, *expr = calloc( 1, sizeof( struct structure_expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		if( local ) {
			struc = local->type;
		} else if( global ) {
			struc = global->type;
		} else {
			struc = get_structure_indexed( func->env->structures_index, &elem->str.string[ 1 ] );
		}
		if( struc && field ) {
			( ( struct structure_expression * ) expr )->structure = struc;
			idx = get_string_list_index( struc->fields, &field[ 1 ] );
			if( idx >= 0 ) {
				if( next && next->str.string[ 0 ] == '(' ) {
					parse_expressions( next->child, func, 0, &param, &count, message );
					expr->parameters = param.next;
					if( local || global ) {
						this = calloc( 1, sizeof( struct global_expression ) );
						if( this ) {
							if( local ) {
								this->index = local->index;
								this->evaluate = evaluate_local;
							} else {
								( ( struct global_expression * ) this )->global = global;
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
	char *memb, struct element *elem, struct function *func, struct expression *prev, char *message ) {
	char *name;
	int num_params;
	struct expression *expr;
	struct function *decl = NULL;
	struct element *next = elem->next;
	while( struc && decl == NULL ) {
		name = malloc( struc->str.length + 1 + strlen( memb ) + 1 );
		strcpy( name, struc->str.string );
		name[ struc->str.length ] = '_';
		strcpy( &name[ struc->str.length + 1 ], memb );
		decl = get_function_indexed( func->env->functions_index, name );
		free( name );
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
				parse_expressions( next->child, func, 0, this, &num_params, message );
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
	return next;
}

static struct element* parse_local_expression( struct element *elem,
	struct function *func, struct local_variable *local, struct expression *prev, char *message ) {
	int idx, len;
	struct element *next = elem->next;
	struct structure *struc = local->type;
	char *field = &elem->str.string[ strlen( local->name ) ];
	struct expression *expr = calloc( 1, sizeof( struct expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		expr->index = local->index;
		expr->evaluate = evaluate_local;
		if( field[ 0 ] == '.' || field[ 0 ] == ':' ) {
			if( struc ) {
				if( field[ 0 ] == ':' ) {
					next = parse_member_call_expression( struc, expr, &field[ 1 ], elem, func, prev, message );
				} else {
					idx = get_string_list_index( struc->fields, &field[ 1 ] );
					if( idx >= 0 ) {
						prev->next = calloc( 1, sizeof( struct structure_expression ) );
						if( prev->next ) {
							( ( struct structure_expression * ) prev->next )->structure = struc;
							prev->next->index = idx;
							prev->next->line = elem->line;
							prev->next->parameters = expr;
							prev->next->evaluate = evaluate_member_expression;
						} else {
							prev->next = expr;
							strcpy( message, OUT_OF_MEMORY );
						}
					} else {
						sprintf( message, "Field not declared in expression '%.64s' on line %d.", elem->str.string, elem->line );
					}
				}
			} else {
				sprintf( message, "Expression '%.64s' has no associated structure on line %d.", elem->str.string, elem->line );
			}
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

static struct element* parse_global_expression( struct element *elem,
	struct function *func, struct global_variable *global, struct expression *prev, char *message ) {
	int idx, count;
	struct element *next = elem->next;
	struct structure *struc = global->type;
	char *field = &elem->str.string[ strlen( global->name ) ];
	struct expression *expr = calloc( 1, sizeof( struct global_expression ) );
	if( expr ) {
		prev->next = expr;
		( ( struct global_expression * ) expr )->global = global;
		expr->line = elem->line;
		expr->evaluate = evaluate_global;
		if( field[ 0 ] == '.' || field[ 0 ] == ':' ) {
			if( struc ) {
				if( field[ 0 ] == ':' ) {
					next = parse_member_call_expression( struc, expr, &field[ 1 ], elem, func, prev, message );
				} else {
					idx = get_string_list_index( struc->fields, &field[ 1 ] );
					if( idx >= 0 ) {
						prev->next = calloc( 1, sizeof( struct structure_expression ) );
						if( prev->next ) {
							( ( struct structure_expression * ) prev->next )->structure = struc;
							prev->next->index = idx;
							prev->next->line = elem->line;
							prev->next->parameters = expr;
							prev->next->evaluate = evaluate_member_expression;
						} else {
							prev->next = expr;
							strcpy( message, OUT_OF_MEMORY );
						}
					} else {
						sprintf( message, "Field not declared in expression '%.64s' on line %d.", elem->str.string, elem->line );
					}
				}
			} else {
				sprintf( message, "Expression '%.64s' has no associated structure on line %d.", elem->str.string, elem->line );
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_integer_literal_expression( struct element *elem, struct expression *prev, char *message ) {
	char *end;
	struct variable var = { 0 };
	struct expression *expr = calloc( 1, sizeof( struct expression ) );
	if( expr ) {
		prev->next = expr;
		expr->line = elem->line;
		expr->index = ( int ) strtoul( elem->str.string, &end, 0 );
		if( end[ 0 ] == 0 ) {
			expr->evaluate = evaluate_integer_literal_expression;
		} else {
			sprintf( message, "Invalid integer literal '%.64s' on line %d.", elem->str.string, elem->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return elem->next;
}

static struct expression* new_string_literal_expression( int integer_value,
	struct string *string_value, int line, char *message ) {
	struct expression *expr = calloc( 1, sizeof( struct string_literal_expression ) );
	if( expr ) {
		expr->line = line;
		expr->index = integer_value;
		( ( struct string_literal_expression * ) expr )->str = string_value;
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
	struct expression *prev, char *message ) {
	struct element *next = elem->next;
	if( next && next->str.string[ 0 ] == '{' ) {
		if( next->child ) {
			next->child->str.reference_count++;
			prev->next = new_string_literal_expression( 0, &next->child->str, elem->line, message );
		}
		next = next->next;
	} else {
		sprintf( message, "Expected '{' after '$' on line %d.", elem->line );
	}
	return next;
}

static struct element* parse_expression( struct element *elem,
	struct function *func, struct expression *prev, char *message ) {
	struct element *next = elem->next;
	char *value = elem->str.string;
	struct global_variable *global;
	struct local_variable *local;
	struct structure *struc;
	struct function *decl;
	if( ( value[ 0 ] >= '0' && value[ 0 ] <= '9' )
		|| ( value[ 0 ] == '-' && ( value[ 1 ] >= '0' && value[ 1 ] <= '9' ) ) ) {
		/* Integer literal. */
		next = parse_integer_literal_expression( elem, prev, message );
	} else if( value[ 0 ] == '"' ) {
		/* String literal. */
		next = parse_string_literal_expression( elem, prev, message );
	} else if( value[ 0 ] == '$' && value[ 1 ] == 0 ) {
		/* Element literal. */
		next = parse_element_literal_expression( elem, prev, message );
	} else if( value[ 0 ] == '\'' ) {
		/* Infix operator.*/
		next = parse_infix_expression( elem, func, prev, message );
	} else if( value[ 0 ] == '[' ) {
		/* Array index operator. */
		next = parse_index_expression( elem, func, prev, message );
	} else if( value[ 0 ] == '@' ) {
		/* Function reference operator. */
		next = parse_func_ref_expression( elem, func, prev, message );
	} else if( value[ 0 ] == ':' ) {
		if( value[ 1 ] == 0 ) {
			/* Function reference call. */
			next = parse_refcall_expression( elem, func, prev, message );
		} else {
			/* Member function call. */
			next = parse_thiscall_expression( elem, func, prev, message );
		}
	} else {
		local = get_local_variable( func->variable_decls, value, ".:+-" );
		if( local ) {
			/* Local variable reference.*/
			next = parse_local_expression( elem, func, local, prev, message );
		} else {
			global = get_global_indexed( func->env->constants_index, value );
			if( global == NULL ) {
				global = get_global_indexed( func->env->globals_index, value );
			}
			if( global ) {
				/* Global variable reference.*/
				next = parse_global_expression( elem, func, global, prev, message );
			} else {
				decl = get_function_indexed( func->env->functions_index, value );
				if( decl ) {
					/* Function call.*/
					next = parse_call_expression( elem, func, decl, prev, message );
				} else {
					struc = get_structure_indexed( func->env->structures_index, value );
					if( struc ) {
						/* Structure. */
						next = parse_struct_expression( elem, func, struc, prev, message );
					} else {
						/* Prefix Operator. */
						next = parse_operator_expression( elem, func, prev, message );
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
	char terminator, struct expression *prev, int *num_exprs, char *message ) {
	int line, count = 0;
	while( elem && elem->str.string[ 0 ] != terminator && message[ 0 ] == 0 ) {
		elem = parse_expression( elem, func, prev, message );
		prev = prev->next;
		count++;
		while( elem && message[ 0 ] == 0 && elem->str.string[ 0 ] == ',' ) {
			line = elem->line;
			elem = elem->next;
			if( elem && elem->str.string[ 0 ] != ',' ) {
				elem = parse_expression( elem, func, prev, message );
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
	struct function *func, struct statement *prev, char *message ) {
	struct local_variable *local;
	struct element *next = elem->next;
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt ) {
		prev->next = stmt;
		local = get_local_variable( func->variable_decls, next->str.string, "" );
		if( local ) {
			stmt->source = calloc( 1, sizeof( struct expression ) );
			if( stmt->source ) {
				stmt->local = local->index;
				stmt->source->line = next->line;
				stmt->execute = execute_increment_statement;
				next = next->next->next;
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		} else {
			sprintf( message, "Undeclared local variable '%.64s' on line %d.", next->str.string, next->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_decrement_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct local_variable *local;
	struct element *next = elem->next;
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt ) {
		prev->next = stmt;
		local = get_local_variable( func->variable_decls, next->str.string, "" );
		if( local ) {
			stmt->source = calloc( 1, sizeof( struct expression ) );
			if( stmt->source ) {
				stmt->local = local->index;
				stmt->source->line = next->line;
				stmt->execute = execute_decrement_statement;
				next = next->next->next;
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		} else {
			sprintf( message, "Undeclared local variable '%.64s' on line %d.", next->str.string, next->line );
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_save_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, prev, execute_save_statement, message );
}

static struct element* parse_append_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct element *next = parse_save_statement( elem, func, prev, message );
	if( prev->next && message[ 0 ] == 0 ) {
		prev->next->local = 1;
	}
	return next;
}

static struct element* parse_array_assignment( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next, *child = next->child;
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt ) {
		prev->next = stmt;
		expr.next = NULL;
		child = parse_expression( child, func, &expr, message );
		if( expr.next ) {
			stmt->source = expr.next;
			if( child->str.string[ 0 ] == ',' ) {
				child = child->next;
			}
			expr.next = NULL;
			child = parse_expression( child, func, &expr, message );
			if( expr.next ) {
				stmt->source->next = expr.next;
				expr.next = NULL;
				next = parse_expression( next->next->next, func, &expr, message );
				if( expr.next ) {
					expr.next->next = stmt->source;
					stmt->source = expr.next;
					stmt->execute = execute_array_assignment;
					next = next->next;
				}
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_struct_assignment( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	char *field;
	int idx, count;
	struct expression expr;
	struct structure *struc;
	struct element *next = elem->next, *child = next->child;
	struct statement *stmt = calloc( 1, sizeof( struct structure_statement ) );
	if( stmt ) {
		prev->next = stmt;
		struc = get_structure_indexed( func->env->structures_index, next->str.string );
		field = strchr( next->str.string, '.' );
		if( struc && field ) {
			( ( struct structure_statement * ) stmt )->structure = struc;
			idx = get_string_list_index( struc->fields, &field[ 1 ] );
			if( idx >= 0 ) {
				stmt->local = idx;
				next = next->next;
				expr.next = NULL;
				parse_expressions( next->child, func, 0, &expr, &count, message );
				stmt->source = expr.next;
				if( message[ 0 ] == 0 ) {
					if( count == 1 ) {
						expr.next = NULL;
						next = parse_expression( next->next->next, func, &expr, message );
						if( expr.next ) {
							expr.next->next = stmt->source;
							stmt->source = expr.next;
							stmt->execute = execute_struct_assignment;
							next = next->next;
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
	struct function *func, struct local_variable *local, struct statement *prev, char *message ) {
	int idx;
	struct statement *stmt;
	struct expression expr;
	struct element *next = elem->next;
	struct structure *struc = local->type;
	char *field = strchr( next->str.string, '.' );
	if( struc && field ) {
		stmt = calloc( 1, sizeof( struct structure_statement ) );
		if( stmt ) {
			prev->next = stmt;
			expr.next = NULL;
			next = parse_expression( next->next->next, func, &expr, message );
			if( expr.next ) {
				stmt->source = expr.next;
				( ( struct structure_statement * ) stmt )->structure = struc;
				idx = get_string_list_index( struc->fields, &field[ 1 ] );
				if( idx >= 0 ) {
					stmt->local = idx;
					expr.next = calloc( 1, sizeof( struct expression ) );
					if( expr.next ) {
						stmt->source->next = expr.next;
						expr.next->line = next->line;
						expr.next->index = local->index;
						expr.next->evaluate = evaluate_local;
						stmt->execute = execute_struct_assignment;
						next = next->next;
					} else {
						strcpy( message, OUT_OF_MEMORY );
					}
				} else {
					sprintf( message, "Field '%.64s' not declared on line %d.", &field[ 1 ], elem->line );
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
			prev->next = stmt;
			expr.next = NULL;
			next = parse_expression( next->next->next, func, &expr, message );
			if( expr.next ) {
				stmt->source = expr.next;
				stmt->local = local->index;
				stmt->execute = execute_local_assignment;
				next = next->next;
			}
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	}
	return next;
}

static struct element* parse_global_assignment( struct element *elem,
	struct function *func, struct global_variable *global, struct statement *prev, char *message ) {
	int idx;
	struct expression expr;
	struct element *next = elem->next;
	struct structure *struc = global->type;
	char *field = strchr( next->str.string, '.' );
	struct statement *stmt;
	if( struc && field ) {
		stmt = calloc( 1, sizeof( struct structure_statement ) );
		if( stmt ) {
			prev->next = stmt;
			expr.next = NULL;
			next = parse_expression( next->next->next, func, &expr, message );
			if( expr.next ) {
				stmt->source = expr.next;
				( ( struct structure_statement * ) stmt )->structure = struc;
				idx = get_string_list_index( struc->fields, &field[ 1 ] );
				if( idx >= 0 ) {
					stmt->local = idx;
					expr.next = calloc( 1, sizeof( struct global_expression ) );
					if( expr.next ) {
						stmt->source->next = expr.next;
						expr.next->line = next->line;
						expr.next->evaluate = evaluate_global;
						( ( struct global_expression * ) expr.next )->global = global;
						stmt->execute = execute_struct_assignment;
						next = next->next;
					} else {
						strcpy( message, OUT_OF_MEMORY );
					}
				} else {
					sprintf( message, "Field '%.64s' not declared on line %d.", &field[ 1 ], elem->line );
				}
			}
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	} else if( field ) {
		sprintf( message, "Variable '%.64s' has no associated structure on line %d.", global->name, elem->line );
	} else {
		stmt = calloc( 1, sizeof( struct global_assignment_statement ) );
		if( stmt ) {
			prev->next = stmt;
			expr.next = NULL;
			next = parse_expression( next->next->next, func, &expr, message );
			if( expr.next ) {
				stmt->source = expr.next;
				( ( struct global_assignment_statement * ) stmt )->destination = &global->value;
				stmt->execute = execute_global_assignment;
				next = next->next;
			}
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	}
	return next;
}

static struct element* parse_assignment_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct local_variable *local;
	struct global_variable *global;
	struct element *next = elem->next, *child = next->child;
	if( next->str.string[ 0 ] == '[' ) {
		return parse_array_assignment( elem, func, prev, message );
	} else if( next->next->str.string[ 0 ] == '(' ) {
		return parse_struct_assignment( elem, func, prev, message );
	} else {
		local = get_local_variable( func->variable_decls, next->str.string, "." );
		if( local ) {
			return parse_local_assignment( elem, func, local, prev, message );
		} else {
			global = get_global_indexed( func->env->globals_index, next->str.string );
			if( global ) {
				return parse_global_assignment( elem, func, global, prev, message );
			} else {
				sprintf( message, "Undeclared variable '%.64s' on line %d.", next->str.string, next->line );
			}
		}
	}
	return next;
}

/* Parse a statement that expects one or more expressions after the keyword. */
struct element* parse_expr_list_statement( struct element *elem, struct function *func, struct statement *prev,
	enum result ( *execute )( struct statement *this, struct variables *vars, struct variable *result ),
	char *message ) {
	struct expression head, *expr;
	struct element *next = elem->next;
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt ) {
		prev->next = stmt;
		head.next = NULL;
		next = parse_expression( next, func, &head, message );
		expr = stmt->source = head.next;
		while( expr && next->str.string[ 0 ] != ';' ) {
			if( next->str.string[ 0 ] == ',' ) {
				next = next->next;
			}
			next = parse_expression( next, func, expr, message );
			expr = expr->next;
		}
		if( expr ) {
			stmt->execute = execute;
			next = next->next;
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_print_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, prev, execute_print_statement, message );
}

static struct element* parse_write_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, prev, execute_write_statement, message );
}

static struct element* parse_error_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, prev, execute_error_statement, message );
}

static struct element* parse_return_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, prev, execute_return_statement, message );
}

static struct element* parse_throw_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, prev, execute_throw_statement, message );
}

static struct element* parse_exit_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, prev, execute_exit_statement, message );
}

static struct element* parse_break_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt ) {
		stmt->execute = execute_break_statement;
		prev->next = stmt;
		next = next->next;
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_continue_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement *stmt = calloc( 1, sizeof( struct statement ) );
	if( stmt ) {
		stmt->execute = execute_continue_statement;
		prev->next = stmt;
		next = next->next;
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_call_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, func, prev, execute_call_statement, message );
}

static struct element* parse_case_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement block, *stmt = calloc( 1, sizeof( struct block_statement ) );
	if( stmt ) {
		stmt->dispose = dispose_block_statement;
		prev->next = stmt;
		next = parse_expressions( next, func, '{', &expr, NULL, message );
		stmt->source = expr.next;
		if( message[ 0 ] == 0 ) {
			block.next = NULL;
			parse_keywords_indexed( func->env->statements_index, next->child, func, &block, message );
			( ( struct block_statement * ) stmt )->if_block = block.next;
			if( message[ 0 ] == 0 ) {
				stmt->execute = execute_case_statement;
				next = next->next;
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_default_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement block, *stmt = calloc( 1, sizeof( struct block_statement ) );
	if( stmt ) {
		stmt->dispose = dispose_block_statement;
		prev->next = stmt;
		block.next = NULL;
		parse_keywords_indexed( func->env->statements_index, next->child, func, &block, message );
		( ( struct block_statement * ) stmt )->if_block = block.next;
		if( message[ 0 ] == 0 ) {
			stmt->execute = execute_case_statement;
			next = next->next;
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return elem->next->next;
}

static struct keyword switch_stmts[] = {
	{ "rem", "{", parse_comment, &switch_stmts[ 1 ] },
	{ "case", "X{", parse_case_statement, &switch_stmts[ 2 ] },
	{ "default", "{", parse_default_statement, NULL }
};

static struct element* parse_switch_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement block, *cas, *def;
	struct block_statement *stmt = calloc( 1, sizeof( struct block_statement ) );
	if( stmt ) {
		stmt->stmt.dispose = dispose_block_statement;
		prev->next = &stmt->stmt;
		expr.next = NULL;
		next = parse_expression( next, func, &expr, message );
		if( expr.next ) {
			stmt->stmt.source = expr.next;
			block.next = NULL;
			parse_keywords( switch_stmts, next->child, func, &block, message );
			if( message[ 0 ] == 0 ) {
				cas = def = NULL;
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
				if( stmt->else_block == NULL || stmt->else_block->next == NULL ) {
					next = next->next;
					stmt->stmt.execute = execute_switch_statement;
				} else {
					sprintf( message, "Duplicate default block in switch statement on line %d.", elem->line );
				}
			} else {
				dispose_statements( block.next );
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_lock_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement block, *stmt = calloc( 1, sizeof( struct block_statement ) );
	if( stmt ) {
		stmt->dispose = dispose_block_statement;
		prev->next = stmt;
		if( next->str.string[ 0 ] == '{' ) {
			expr.next = calloc( 1, sizeof( struct expression ) );
			if( expr.next ) {
				expr.next->line = elem->line;
				expr.next->evaluate = evaluate_integer_literal_expression;
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		} else {
			expr.next = NULL;
			next = parse_expression( next, func, &expr, message );
		}
		if( message[ 0 ] == 0 ) {
			stmt->source = expr.next;
			stmt->execute = execute_lock_statement;
			if( next->child ) {
				block.next = NULL;
				parse_keywords_indexed( func->env->statements_index, next->child, func, &block, message );
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

static struct keyword* get_keyword( char *name, struct keyword *keywords ) {
	while( keywords && strcmp( keywords->name, name ) ) {
		keywords = keywords->next;
	}
	return keywords;
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
			/* Bracketed function parameter list. */
			if( elem && elem->str.string[ 0 ] == '(' ) {
				if( elem->child ) {
					validate_syntax( "P0", elem->child, elem, env, message );
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
					sprintf( message, "Invalid index expression after '%.64s' on line %d.", key->str.string, line );
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
		} else if( chr == 'p' ) {
			/* Function parameter. */
			if( elem && elem->str.string[ 0 ] == '(' ) {
				validate_syntax( "n0", elem->child, elem, env, message );
				if( message[ 0 ] == 0 ) {
					elem = validate_syntax( "n", elem->next, elem->child, env, message );
				}
			} else {
				elem = validate_syntax( "n", elem, key, env, message );
			}
		} else if( chr == 'P' ) {
			/* Function parameter list. */
			elem = validate_syntax( "p", elem, key, env, message );
			while( message[ 0 ] == 0 && elem ) {
				if( elem->str.string[ 0 ] == ',' ) {
					elem = elem->next;
				}
				elem = validate_syntax( "p", elem, key, env, message );
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
			if( elem && elem->str.string[ 0 ] == '(' ) {
				validate_syntax( "n0", elem->child, elem, env, message );
				if( message[ 0 ] == 0 ) {
					elem = elem->next;
				}
			}
			if( message[ 0 ] == 0 ) {
				if( elem && elem->str.string[ 0 ] == '[' ) {
					validate_syntax( "nx0", elem->child, elem, env, message );
					if( message[ 0 ] == 0 ) {
						elem = elem->next;
					}
				} else {
					elem = validate_syntax( "n", elem, key, env, message );
					if( message[ 0 ] == 0 && elem && elem->str.string[ 0 ] == '=' ) {
						elem = validate_syntax( "x", elem->next, key, env, message );
					}
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
		} else {
			/* Internal error. */
			sprintf( message, "Internal error. Unknown specifier '%c' in syntax for '%s'.", chr, key->str.string );
		}
		chr = syntax[ idx++ ];
	}
	return elem;
}

static void parse_keywords( struct keyword *keywords, struct element *elem,
	struct function *func, struct statement *stmt, char *message ) {
	struct keyword *key;
	while( elem && message[ 0 ] == 0 ) {
		key = get_keyword( elem->str.string, keywords );
		if( key ) {
			validate_syntax( key->syntax, elem->next, elem, func->env, message );
			if( message[ 0 ] == 0 ) { 
				elem = key->parse( elem, func, stmt, message );
				while( stmt && stmt->next ) {
					stmt = stmt->next;
				}
			}
		} else {
			sprintf( message, "Unrecognized keyword '%.64s' on line %d.", elem->str.string, elem->line );
		}
	}
}

static void parse_keywords_indexed( struct keyword **index, struct element *elem,
	struct function *func, struct statement *stmt, char *message ) {
	struct keyword *key;
	while( elem && message[ 0 ] == 0 ) {
		key = get_keyword( elem->str.string, index[ hash_code( elem->str.string, 0 ) ] );
		if( key ) {
			validate_syntax( key->syntax, elem->next, elem, func->env, message );
			if( message[ 0 ] == 0 ) { 
				elem = key->parse( elem, func, stmt, message );
				while( stmt && stmt->next ) {
					stmt = stmt->next;
				}
			}
		} else {
			sprintf( message, "Unrecognized keyword '%.64s' on line %d.", elem->str.string, elem->line );
		}
	}
}

static struct element* parse_if_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement block, *stmt = calloc( 1, sizeof( struct block_statement ) );
	if( stmt ) {
		stmt->dispose = dispose_block_statement;
		prev->next = stmt;
		expr.next = NULL;
		next = parse_expression( next, func, &expr, message );
		if( expr.next ) {
			stmt->source = expr.next;
			stmt->execute = execute_if_statement;
			if( next->child ) {
				block.next = NULL;
				parse_keywords_indexed( func->env->statements_index, next->child, func, &block, message );
				( ( struct block_statement * ) stmt )->if_block = block.next;
			}
			if( message[ 0 ] == 0 ) {
				next = next->next;
				if( next && strcmp( next->str.string, "else" ) == 0 ) {
					if( next->next && next->next->str.string[ 0 ] == '{' ) {
						next = next->next;
						if( next->child ) {
							block.next = NULL;
							parse_keywords_indexed( func->env->statements_index, next->child, func, &block, message );
							( ( struct block_statement * ) stmt )->else_block = block.next;
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
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_while_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct expression expr;
	struct element *next = elem->next;
	struct statement block, *stmt = calloc( 1, sizeof( struct block_statement ) );
	if( stmt ) {
		stmt->dispose = dispose_block_statement;
		prev->next = stmt;
		expr.next = NULL;
		next = parse_expression( next, func, &expr, message );
		if( expr.next ) {
			stmt->source = expr.next;
			if( next->child ) {
				block.next = NULL;
				parse_keywords_indexed( func->env->statements_index, next->child, func, &block, message );
				( ( struct block_statement * ) stmt )->if_block = block.next;
			}
			if( message[ 0 ] == 0 ) {
				if( stmt->source->evaluate == evaluate_arithmetic_expression
				&& stmt->source->parameters->evaluate == evaluate_local
				&& stmt->source->parameters->next->evaluate == evaluate_local
				&& stmt->source->parameters->next->next == NULL ) {
					stmt->local = stmt->source->index;
				}
				stmt->execute = execute_while_statement;
				next = next->next;
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* parse_try_statement( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct local_variable *local;
	struct element *next = elem->next;
	struct statement block, *stmt = calloc( 1, sizeof( struct block_statement ) );
	if( stmt ) {
		stmt->dispose = dispose_block_statement;
		prev->next = stmt;
		if( next->child ) {
			block.next = NULL;
			parse_keywords_indexed( func->env->statements_index, next->child, func, &block, message );
			( ( struct block_statement * ) stmt )->if_block = block.next;
		}
		if( message[ 0 ] == 0 ) {
			next = next->next->next;
			local = get_local_variable( func->variable_decls, next->str.string, "" );
			if( local ) {
				stmt->local = local->index;
				next = next->next;
				if( next->child ) {
					block.next = NULL;
					parse_keywords_indexed( func->env->statements_index, next->child, func, &block, message );
					( ( struct block_statement * ) stmt )->else_block = block.next;
				}
				if( message[ 0 ] == 0 ) {
					next = next->next;
				}
				stmt->execute = execute_try_statement;
			} else {
				sprintf( message, "Undeclared local variable '%.64s' on line %d.", next->str.string, next->line );
			}
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static struct element* add_function_parameter( struct function *func, struct element *elem,
	struct environment *env, char *message ) {
	char *name;
	struct structure *type = NULL;
	struct local_variable *param;
	if( elem->str.string[ 0 ] == '(' ) {
		type = get_structure_indexed( env->structures_index, elem->child->str.string );
		if( type != NULL ) {
			elem = elem->next;
		} else {
			sprintf( message, "Structure '%.64s' not declared on line %d.", elem->child->str.string, elem->child->line );
		}
	}
	if( message[ 0 ] == 0 && validate_decl( elem, env, message ) ) {
		name = elem->str.string;
		param = new_local_variable( func->num_variables, name, type );
		if( param ) {
			/*printf("Function parameter '%s'\n", name);*/
			if( get_local_variable( func->variable_decls, name, "" ) == NULL ) {
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
				sprintf( message, "Parameter '%.64s' already defined on line %d.", name, elem->line );
			}
		} else {
			strcpy( message, OUT_OF_MEMORY );
		}
	}
	return elem;
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
			child = add_function_parameter( func, child, env, message );
			if( child && child->str.string[ 0 ] == ',' && message[ 0 ] == 0 ) {
				child = child->next;
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
		parse_keywords_indexed( env->statements_index, next, func, &stmt, message );
		func->statements = stmt.next;
	}
	return message[ 0 ] == 0;
}

static enum result evaluate_function_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct function *func;
	struct element *elem, key = { { 1, "$function", 9, ELEMENT }, NULL, NULL, 0 };
	char message[ 128 ] = "";
	enum result ret = parameter->evaluate( parameter, vars, &var );
	if( ret ) {
		if( var.string_value && var.string_value->type == ELEMENT ) {
			elem = ( struct element * ) var.string_value;
			key.line = this->line;
			validate_syntax( "({0", elem, &key, vars->func->env, message );
			if( message[ 0 ] == 0 ) {
				func = parse_function( elem, "[Function]", vars->func->file->string, vars->func->env, message );
				if( func ) {
					if( parse_function_body( func, vars->func->env, message ) ) {
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
		} else {
			ret = throw( vars, this, 0, "Not an element." );
		}
		dispose_temporary( &var );
	}
	return ret;
}

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
	expr.expr.line = work->env.entry_point->line;
	expr.expr.parameters = work->parameters;
	expr.function = work->env.entry_point;
	work->ret = evaluate_call_expression( &expr.expr, &vars, &work->result );
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

static enum result evaluate_worker_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct element *elem, key = { { 1, "$worker", 9, ELEMENT }, NULL, NULL, 0 };
	struct variable var = { 0, NULL };
	char message[ 128 ] = "";
	struct worker *work;
	enum result ret;
	if( vars->func->env->worker ) {
		ret = throw( vars, this, 0, "Operation not permitted." );
	} else {
		ret = parameter->evaluate( parameter, vars, &var );
	}
	if( ret ) {
		if( var.string_value && var.string_value->type == ELEMENT ) {
			/* Copy source to avoid sharing element-literals. */
			elem = copy_element( ( struct element * ) var.string_value );
			if( elem ) {
				key.line = this->line;
				validate_syntax( "({0", elem, &key, vars->func->env, message );
				if( message[ 0 ] == 0 ) {
					work = parse_worker( elem, vars->func->env, vars->func->file->string, message );
					if( work ) {
						result->string_value = &work->str;
					} else {
						ret = throw( vars, this, 0, message );
					}
				} else {
					ret = throw( vars, this, 0, message );
				}
				unref_string( &elem->str );
			} else {
				ret = throw( vars, this, 0, OUT_OF_MEMORY );
			}
		} else {
			ret = throw( vars, this, 0, "Not an element." );
		}
		dispose_temporary( &var );
	}
	return ret;
}

static enum result evaluate_execute_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct worker *work;
	struct string *str;
	int count, idx;
	enum result ret = parameter->evaluate( parameter, vars, &var );
	if( ret ) {
		if( var.string_value && var.string_value->type == WORKER ) {
			work = ( struct worker * ) var.string_value;
			parameter = parameter->next;
			count = 0;
			while( parameter ) {
				count++;
				parameter = parameter->next;
			}
			if( work->env.entry_point->num_parameters == count ) {
				if( work->locked != 1 ) {
					await_worker( work, 1 );
					idx = 0;
					parameter = this->parameters->next;
					while( parameter && ret ) {
						ret = parameter->evaluate( parameter, vars, &work->args[ idx ] );
						if( ret ) {
							work->globals[ idx ].value.integer_value = work->args[ idx ].integer_value;
							str = work->args[ idx ].string_value;
							if( str ) {
								if( str->type == STRING || ( str->type == ARRAY && !( ( struct array * ) str )->string_values ) ) {
									work->strings[ idx ].str.reference_count = 1;
									work->strings[ idx ].str.type = str->type;
									work->strings[ idx ].str.string = str->string;
									work->strings[ idx ].str.length = str->length;
									if( str->type == ARRAY ) {
										work->strings[ idx ].integer_values = ( ( struct array * ) str )->integer_values;
										work->strings[ idx ].length = ( ( struct array * ) str )->length;
									}
									work->globals[ idx ].value.string_value = &work->strings[ idx ].str;
								} else {
									ret = throw( vars, this, 0, "Values of this type cannot be passed to workers." );
								}
							}
						}
						parameter = parameter->next;
						idx++;
					}
				} else {
					ret = throw( vars, this, 0, "Worker locked." );
				}
				if( ret ) {
					work->ret = OKAY;
					work->env.interrupted = vars->func->env->interrupted;
					vars->func->env->worker = work;
					if( start_worker( work ) ) {
						result->string_value = var.string_value;
						result->string_value->reference_count++;
					} else {
						ret = throw( vars, this, 0, "Unable to start worker." );
					}
					vars->func->env->worker = NULL;
				}
			} else {
				ret = throw( vars, this, count, "Incorrect number of parameters to function." );
			}
		} else {
			ret = throw( vars, this, 0, "Not a worker." );
		}
		dispose_temporary( &var );
	}
	return ret;
}

static enum result evaluate_result_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	struct expression *parameter = this->parameters;
	struct variable var = { 0, NULL };
	struct worker *work;
	int count, idx;
	char *str;
	enum result ret = parameter->evaluate( parameter, vars, &var );
	if( ret ) {
		if( var.string_value && var.string_value->type == WORKER ) {
			work = ( struct worker * ) var.string_value;
			if( work->locked != 1 ) {
				vars->func->env->worker = work;
				await_worker( work, 0 );
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
					ret = throw( vars, this, work->exception.integer_value, work->exception.string_value->string );
				} else {
					assign_variable( &work->exception, vars->exception );
					ret = EXCEPTION;
				}
			} else {
				ret = throw( vars, this, 0, "Worker locked." );
			}
		} else {
			ret = throw( vars, this, 0, "Not a worker." );
		}
		dispose_temporary( &var );
	}
	return ret;
}

static struct element* parse_function_declaration( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct environment *env = func->env;
	struct element *next = elem->next;
	char *name;
	int idx;
	if( validate_decl( next, env, message ) ) {
		name = next->str.string;
		next = next->next;
		func = parse_function( next, name, func->file->string, env, message );
		if( func ) {
			idx = hash_code( name, 0 );
			func->next = env->functions_index[ idx ];
			env->functions_index[ idx ] = func;
			next = next->next->next;
		}
	}
	return next;
}

static struct element* parse_program_declaration( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct environment *env = func->env;
	struct element *next = elem->next;
	int idx;
	if( validate_decl( next, env, message ) ) {
		func = new_function( next->str.string, func->file->string, message );
		if( func ) {
			idx = hash_code( next->str.string, 0 );
			func->next = env->functions_index[ idx ];
			env->functions_index[ idx ] = func;
			env->entry_point = func;
			func->line = elem->line;
			func->body = next->next;
			func->env = env;
			next = next->next->next;
		}
	}
	return next;
}

static struct element* parse_include( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	int path_len = str_idx( func->file->string, "/:\\", -1 ) + 1;
	int name_len = unquote_string( next->str.string, NULL );
	char *path = malloc( path_len + name_len + 1 );
	if( path ) {
		memcpy( path, func->file->string, sizeof( char ) * path_len );
		unquote_string( next->str.string, &path[ path_len ] );
		path[ path_len + name_len ] = 0;
		if( parse_tt_file( path, func->env, message ) ) {
			next = next->next->next;
		}
		free( path );
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return next;
}

static int add_structure_field( struct structure *struc, char *name,
	struct environment *env, int line, char *message ) {
	struct string_list *field;
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

static struct element* parse_struct_declaration( struct element *elem,
	struct function *func, struct statement *prev, char *message ) {
	int idx;
	char *name;
	struct structure *struc;
	struct string_list *field;
	struct environment *env = func->env;
	struct element *child, *next = elem->next;
	if( validate_decl( next, env, message ) ) {
		name = new_string( next->str.string );
		struc = calloc( 1, sizeof( struct structure ) );
		if( name && struc ) {
			struc->str.reference_count = 1;
			struc->str.string = name;
			struc->str.length = strlen( name );
			struc->str.type = STRUCT;
			idx = hash_code( name, 0 );
			struc->next = env->structures_index[ idx ];
			env->structures_index[ idx ] = struc;
			next = next->next;
			if( next && next->str.string[ 0 ] == '(' ) {
				child = next->child;
				if( child && child->next == NULL && strcmp( child->str.string, name ) ) {
					struc->super = get_structure_indexed( env->structures_index, child->str.string );
					if( struc->super ) {
						field = struc->super->fields;
						while( field && message[ 0 ] == 0 ) {
							if( add_structure_field( struc, field->value, env, child->line, message ) ) {
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
						if( add_structure_field( struc, child->str.string, env, child->line, message ) ) {
							child = child->next;
							if( child && ( child->str.string[ 0 ] == ',' || child->str.string[ 0 ] == ';' ) ) {
								child = child->next;
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
		} else {
			free( name );
			free( struc );
			strcpy( message, OUT_OF_MEMORY );
		}
	}
	return next;
}

static struct keyword statements[] = {
	{ "rem", "{", parse_comment, NULL },
	{ "var", "V;", parse_local_declaration, NULL },
	{ "let", "x=x;", parse_assignment_statement, NULL },
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
	{ "call", "x;", parse_call_statement, NULL },
	{ "try", "{cn{", parse_try_statement, NULL },
	{ "set", "x=x;", parse_assignment_statement, NULL },
	{ "switch", "x{", parse_switch_statement, NULL },
	{ "inc", "n;", parse_increment_statement, NULL },
	{ "dec", "n;", parse_decrement_statement, NULL },
	{ "save", "xx;", parse_save_statement, NULL },
	{ "append", "xx;", parse_append_statement, NULL },
	{ "lock", "x{", parse_lock_statement, NULL },
	{ "locked", "{", parse_lock_statement, NULL },
	{ NULL }
};

static struct operator operators[] = {
	{ "%", '%',-2, evaluate_arithmetic_expression, NULL },
	{ "&", '&',-2, evaluate_arithmetic_expression, NULL },
	{ "*", '*',-2, evaluate_arithmetic_expression, NULL },
	{ "+", '+',-2, evaluate_arithmetic_expression, NULL },
	{ "-", '-',-2, evaluate_arithmetic_expression, NULL },
	{ "/", '/',-2, evaluate_arithmetic_expression, NULL },
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
	{ "~", '~', 1, evaluate_bitwise_not_expression, NULL },
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
	{ "$values", '$', 1, evaluate_values_expression, NULL },
	{ "$pack", '$', 1, evaluate_pack_expression, NULL },
	{ "$unpack", '$', 2, evaluate_unpack_expression, NULL },
	{ "$quote", '$', 1, evaluate_quote_expression, NULL },
	{ "$unquote", '$', 1, evaluate_unquote_expression, NULL },
	{ "$interrupted", '$', 0, evaluate_interrupted_expression, NULL },
	{ "$function", '$', 1, evaluate_function_expression, NULL },
	{ "$worker", '$', 1, evaluate_worker_expression, NULL },
	{ "$execute", '$',-1, evaluate_execute_expression, NULL },
	{ "$result", '$', 1, evaluate_result_expression, NULL },
	{ "$buffer", 'B', -1, evaluate_array_expression, NULL },
	{ "$src", '$', 1, evaluate_source_expression, NULL },
	{ "$type", '$', 1, evaluate_type_expression, NULL },
	{ "$field", '$', 2, evaluate_field_expression, NULL },
	{ "$instanceof", '$', 2, evaluate_instanceof_expression, NULL },
	{ "$trace", '$', 1, evaluate_trace_expression, NULL },
	{ NULL }
};

static struct keyword declarations[] = {
	{ "rem", "{", parse_comment, &declarations[ 1 ] },
	{ "include", "\";", parse_include, &declarations[ 2 ] },
	{ "function", "n({", parse_function_declaration, &declarations[ 3 ] },
	{ "program", "n{", parse_program_declaration, &declarations[ 4 ] },
	{ "global", "V;", parse_global_declaration, &declarations[ 5 ] },
	{ "const", "V;", parse_const_declaration, &declarations[ 6 ] },
	{ "struct", "n", parse_struct_declaration, NULL }
};

static int validate_name( char *name, struct environment *env ) {
	int chr = name[ 0 ], idx = 1, result = 1;
	if( ( chr >= 'A' && chr <= 'Z') || ( chr >= 'a' && chr <= 'z' ) ) {
		/* First character must be alphabetical.*/
		chr = name[ idx++ ];
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
	return result;
}

static int validate_decl( struct element *elem, struct environment *env, char *message ) {
	char *name = elem->str.string;
	int hash = hash_code( name, 0 );
	size_t len = strlen( name );
	if( get_keyword( name, declarations )
	|| get_keyword( name, env->statements_index[ hash ] )
	|| get_operator( name, env->operators_index[ hash ] )
	|| get_global( env->constants_index[ hash ], name, len )
	|| get_global( env->globals_index[ hash ], name, len )
	|| get_function( env->functions_index[ hash ], name )
	|| get_structure( env->structures_index[ hash ], name, len ) ) {
		sprintf( message, "Name '%.64s' already defined on line %d.", elem->str.string, elem->line );
		return 0;
	}
	return 1;
}

static struct worker* new_worker( char *message ) {
	struct worker *work = calloc( 1, sizeof( struct worker ) );
	if( work ) {
		work->str.string = "[Worker]";
		work->str.length = strlen( work->str.string );
		work->str.reference_count = 1;
		work->str.type = WORKER;
		if( add_operators( operators, &work->env, message )
		&& add_statements( statements, &work->env, message ) && initialize_worker( work, message ) ) {
			work->env.worker = work;
		} else {
			unref_string( &work->str );
			work = NULL;
		}
	} else {
		strcpy( message, OUT_OF_MEMORY );
	}
	return work;
}

static struct worker* parse_worker( struct element *elem, struct environment *env, char *file, char *message ) {
	int params, idx;
	struct function *func;
	struct worker *work = new_worker( message );
	if( work ) {
		func = parse_function( elem, work->str.string, file, &work->env, message );
		if( func ) {
			work->env.functions_index[ hash_code( work->str.string, 0 ) ] = func;
			work->env.entry_point = func;
			params = func->num_parameters;
			work->args = calloc( params, sizeof( struct variable ) );
			if( work->args ) {
				work->strings = calloc( params, sizeof( struct array ) );
			}
			if( work->strings ) {
				work->globals = calloc( params, sizeof( struct global_variable ) );
			}
			if( work->globals ) {
				work->parameters = calloc( params, sizeof( struct global_expression ) );
			}
			if( work->parameters ) {
				for( idx = 0; idx < params; idx++ ) {
					work->parameters[ idx ].evaluate = evaluate_global;
					( ( struct global_expression * ) work->parameters )[ idx ].global = &work->globals[ idx ];
					work->parameters[ idx ].next = &work->parameters[ idx + 1 ];
				}
				parse_function_body( func, &work->env, message );
			} else {
				strcpy( message, OUT_OF_MEMORY );
			}
		}
		if( message[ 0 ] ) {
			unref_string( &work->str );
			work = NULL;
		}
	}
	return work;
}

/* Parse the specified program text into env. Returns zero and writes message on failure. */
int parse_tt_program( char *program, char *file_name, struct environment *env, char *message ) {
	int idx;
	struct element *elem;
	struct function *empty, *func;
	elem = parse_element( program, message );
	if( elem ) {
		/* Create empty function for global evaluation. */
		empty = new_function( "", file_name, message );
		if( empty ) {
			idx = hash_code( empty->str.string, 0 );
			empty->next = env->functions_index[ idx ];
			env->functions_index[ idx ] = empty;
			empty->env = env;
			/* Populate execution environment. */
			parse_keywords( declarations, elem, empty, NULL, message );
			/* Parse function bodies. */
			for( idx = 0; idx < 32; idx++ ) {
				func = env->functions_index[ idx ];
				while( func && message[ 0 ] == 0 ) {
					if( func->body ) {
						parse_function_body( func, env, message );
						func->body = NULL;
					}
					func = func->next;
				}
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
	struct variables vars = { 0 };
	struct expression *init;
	struct global_variable *global;
	struct string_list *name = env->constants;
	vars.exception = exception;
	while( name ) {
		global = get_global_indexed( env->constants_index, name->value );
		vars.func = global->init_function;
		init = global->initializer;
		if( init && init->evaluate( init, &vars, &global->value ) == EXCEPTION ) {
			return 0;
		}
		name = name->next;
	}
	name = env->globals;
	while( name ) {
		global = get_global_indexed( env->globals_index, name->value );
		vars.func = global->init_function;
		init = global->initializer;
		if( init && init->evaluate( init, &vars, &global->value ) == EXCEPTION ) {
			return 0;
		}
		name = name->next;
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
   Returns zero and writes message on failure. */
int initialize_environment( struct environment *env, char *message ) {
	memset( env, 0, sizeof( struct environment ) );
	return add_statements( statements, env, message )
		&& add_operators( operators, env, message )
		&& add_constants( constants, env, message );
}

/* Initialize expr to execute the specified function when evaluated. */
void initialize_call_expr( struct function_expression *expr, struct function *func ) {
	memset( expr, 0, sizeof( struct function_expression ) );
	expr->expr.evaluate = evaluate_call_expression;
	expr->expr.line = func->line;
	expr->function = func;
}
