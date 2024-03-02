
#ifndef _TOWNTALK_H
#define _TOWNTALK_H 1

#include "stdlib.h"

/* Towntalk (c)2024 Martin Cameron. */

/* Return value of execute/evaluate functions. */
enum result {
	EXCEPTION, OKAY, RETURN, BREAK, CONTINUE
};

/* Reference type. */
enum reference_type {
	STRING, ELEMENT, ARRAY, STRUCT, GLOBAL, CONST, FUNCTION, LIBRARY, CUSTOM, EXIT
};

/* String list. */
struct string_list {
	struct string *str;
	struct string_list *next;
};

/* Reference-counted string. */
struct string {
	size_t reference_count;
	char *string;
	int length;
	char type;
};

/* Reference-counted parse-tree element. */
struct element {
	struct string str;
	struct element *child, *next;
	int line;
};

/* Reference-counted array. */
struct array {
	struct string str;
	int *integer_values;
	struct string **string_values;
	struct structure *structure;
	struct array *prev, *next;
	int length;
};

/* Reference-counted function. */
struct function {
	struct string str;
	int line, num_parameters, num_variables;
	struct string *file, *library;
	struct element *body;
	struct environment *env;
	struct local_variable *variable_decls, *variable_decls_tail;
	struct statement *statements, *statements_tail;
};

/* Variable value. */
struct variable {
	int integer_value;
	struct string *string_value;
};

/* Execution environment. */
struct environment {
	int argc;
	char **argv, interrupted;
	struct string exit;
	struct array arrays;
	struct keyword *statements_index[ 32 ];
	struct operator *operators_index[ 32 ];
	struct string_list *decls_index[ 32 ];
	struct string_list *globals, *globals_tail;
	struct string_list *include_paths;
	struct function *entry_point;
	struct custom *worker;
};

/* Reference-counted custom type instance. */
struct custom {
	struct string str;
	struct custom_type *type;
};

/* Reference-counted structure declaration.*/
struct structure {
	struct string str;
	int length;
	struct string_list *fields, *fields_tail;
	struct structure *super;
};

/* Local variables. */
struct local_variable {
	int index;
	char *name;
	struct structure *type;
	struct local_variable *next;
};

/* Global variables. */
struct global_variable {
	struct string str;
	struct variable value;
	struct structure *type;
	struct function *init_function;
	struct expression *initializer;
};

/* Current local variable array and exception. */
struct variables {
	struct variables *parent;
	struct variable *exception;
	struct variable *locals;
	struct function *func;
	int line;
};

/* Expression list. */
struct expression {
	int line, index;
	struct expression *parameters, *next;
	enum result ( *evaluate )( struct expression *this,
		struct variables *vars, struct variable *result );
};

/* Expression with associated reference. */
struct string_expression {
	struct expression expr;
	struct string *str;
};

/* Expression with associated function. */
struct function_expression {
	struct expression expr;
	struct function *function;
};

/* Statement list. */
struct statement {
	int local;
	struct expression *source;
	enum result ( *execute )( struct statement *this,
		struct variables *vars, struct variable *result );
	void ( *dispose )( struct statement *this );
	struct statement *next;
};

/* Statement with associated statement lists. */
struct block_statement {
	struct statement stmt;
	struct statement *if_block, *else_block;
};

/* Parser keyword. */
struct keyword {
	char *name, *syntax;
	/* Parse the current declaration into env, or statement into prev->next. */
	struct element* ( *parse )( struct element *elem, struct function *func,
		struct variables *vars, struct statement *prev, char *message );
	struct keyword *next;
};

/* Parser operator. */
struct operator {
	char *name, oper;
	int num_operands;
	enum result ( *evaluate )( struct expression *this,
		struct variables *vars, struct variable *result );
	struct operator *next;
};

/* Named constant. */
struct constant {
	char *name;
	int integer_value;
	char *string_value;
};

/* Custom type. */
struct custom_type {
	char *name;
	enum result ( *to_int )( struct variable *var, int *result, struct variables *vars, struct expression *source );
	enum result ( *to_str )( struct variable *var, struct string **result, struct variables *vars, struct expression *source );
	void ( *dispose )( struct string *this );
};

/* The maximum integer value. */
extern const int MAX_INTEGER;

/* Message to be used to avoid memory allocation in out-of-memory error paths. */
extern const char *OUT_OF_MEMORY;

/* Initialize env with the the standard statements, operators and constants.
   Returns zero and writes message on failure. */
int initialize_environment( struct environment *env, char *message );

/* Add a copy of the specified null-terminated statement array to env.
   Returns zero and writes message on failure. */
int add_statements( struct keyword *statements, struct environment *env, char *message );

/* Add a copy of the specified null-terminated operator array to env.
   Returns zero and writes message on failure. */
int add_operators( struct operator *operators, struct environment *env, char *message );

/* Add the specified null-terminated constant array to env.
   Returns zero and writes message on failure. */
int add_constants( struct constant *constants, struct environment *env, char *message );

/* Parse the specified program file into env.
   Returns zero and writes up to 256 bytes to message on failure. */
int parse_tt_file( char *file_name, struct environment *env, char *message );

/* Parse the specified program text into env. Returns zero and writes message on failure. */
int parse_tt_program( char *program, struct string *file_name, struct environment *env, char *message );

/* Initialize expr to call the specified function when evaluated. */
void initialize_call_expr( struct function_expression *expr, struct function *func );

/* Evaluate the global-variable initialization expressions for env before program execution.
   Returns zero and assigns exception on failure. */
int initialize_globals( struct environment *env, struct variable *exception );

/* Decrement the reference-count of any referenced types,
   deallocate if necessary, and assign null to the specified variable.
   This must be called for all variables assigned during program execution. */
void dispose_variable( struct variable *var );

/* As dispose_variable(), but do not assign null for performance reasons.
   The resulting variable may contain an invalid pointer and should not be re-used. */
void dispose_temporary( struct variable *var );

/* Dispose a block statement. */
void dispose_block_statement( struct statement *this );

/* Deallocate the specified environment and all types referenced by it. */
void dispose_environment( struct environment *env );

/* Parse a statement that expects one or more expressions after the keyword. */
struct element* parse_expr_list_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev,
	enum result ( *execute )( struct statement *this, struct variables *vars, struct variable *result ),
	char *message );

/* Parse statements from the specified element using the specified keyword index.
   Writes up to 128 bytes to message on failure. */
void parse_keywords_indexed( struct keyword **index, struct element *elem,
	struct function *func, struct variables *vars, struct statement *stmt, char *message );

/* Validate the specified element against the specified syntax string.
   Returns the next element and writes up to 128 bytes to message on failure. */
struct element* validate_syntax( char *syntax, struct element *elem,
	struct element *key, struct environment *env, char *message );

/* Add the specified global declaration to the specified environment. */
struct string_list* add_decl( struct string *decl, int line, struct environment *env, char *message );

/* Parse a single expression and append it to prev.
   Returns the next element to be parsed and writes up to 128 bytes to message on failure. */
struct element* parse_expression( struct element *elem,
	struct function *func, struct variables *vars, struct expression *prev, char *message );

/* Parse and return a new function reference with the specified name from the specified element.
   The function body is not parsed by this function to allow for forward declarations.
   Returns NULL and writes up to 128 bytes to message on failure. */
struct function* parse_function( struct element *elem, char *name,
	struct function *parent, char *message );
	
/* Parse the body of a function returned by parse_function(). */
int parse_function_body( struct function *func, struct variables *vars, char *message );

/* Allocate and return a string of the specified length and reference count of 1. */
struct string* new_string( int length );

/* Allocate and return a new element with the specified string length. */
struct element* new_element( int str_len );

/* Allocate and return a new array with the specified size, associated string length and reference count of 1. */
struct array* new_array( struct environment *env, int length, int str_len );

/* Decrement the reference count of the specified value and deallocate if necessary. */
void unref_string( struct string *str );

/* Assign src variable to dest, managing reference counts. */
void assign_variable( struct variable *src, struct variable *dest );

/* Assign src variable to dest array at the specified index, managing reference counts. */
enum result assign_array_variable( struct variable *src, struct array *arr, int idx, struct variables *vars, struct expression *source );

/* Assign an exception with the specified error code and message to vars and return EXCEPTION. */
enum result throw( struct variables *vars, struct expression *source, int integer, const char *string );

/* Assign an uncatchable exception with the specified exit code and message to vars and return EXCEPTION. */
enum result throw_exit( struct variables *vars, int exit_code, const char *message );

/* Write the specified bytes as a string literal to output (if not null).
   The encoded length is returned. */
int write_byte_string( char *bytes, int count, char *output );

/* Evaluate an integer value using the index field of the specified expression. */
enum result evaluate_integer_literal_expression( struct expression *this,
	struct variables *vars, struct variable *result );

/* Evaluate a string reference associated with a string expression. */
enum result evaluate_string_literal_expression( struct expression *this,
	struct variables *vars, struct variable *result );

/* Return a new string reference from the specified null-terminated character array. */
struct string* new_string_value( char *source );

/* Load the specified portion of a file into buffer (if not null).
   Returns the number of bytes available or read from offset.
   Returns -1 and writes message on failure. */
long load_file( char *file_name, char *buffer, long offset, long count, char *message );

/* Unpack a 32-bit big-endian integer from str at the specified index. */
int unpack( char *str, int idx );

/* Return 1 if name is equivalent to the specified keyword. */
int is_keyword( char *name, char *key );

/* Return OKAY if var contains an instance of the specified structure.
   If vars is non-null a suitable exception is thrown for the specified source expression. */
enum result is_instance( struct variable *var, struct structure *type, struct variables *vars, struct expression *source );

/* Return 1 if the specified reference is an instance of the specified custom type. */
int is_custom_instance( struct string *str, struct custom_type *type );

/* Evaluate the specified expression into the specified result variable.
   Throws an exception if the value is not a reference. */
enum result evaluate_string( struct expression *expr, struct variables *vars, struct variable *result );

/* Evaluate the specified expression into the specified result variable.
   Throws an exception if the value is not an element reference or null (if allowed). */
enum result evaluate_element( struct expression *expr, struct variables *vars, struct variable *result, int allow_null );

/* Evaluate the specified expression into the specified integer result. */
enum result evaluate_integer( struct expression *expr, struct variables *vars, int *result );

/* Evaluate the specified expression into the specified result variable.
   Throws an exception if the value is not an instance of the specified custom type. */
enum result evaluate_custom( struct expression *expr, struct custom_type *type, struct variables *vars, struct variable *result );

#endif /* _TOWNTALK_H */
