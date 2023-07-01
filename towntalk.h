
#include "stdlib.h"

/* Towntalk (c)2023 Martin Cameron. */

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
	struct function *entry_point;
	struct worker *worker;
};

/* Reference-counted custom type. */
struct custom_type {
	struct string str;
	void ( *dispose )( struct string *this );
};

/* Reference-counted worker function. */
struct worker {
	struct custom_type custom;
	struct environment env;
	struct variable *args, result, exception;
	struct expression *parameters;
	char locked, worker_locked;
	struct array *strings;
	void *thread, *mutex;
	enum result ret;
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
int parse_tt_program( char *program, char *file_name, struct environment *env, char *message );

/* Initialize expr to call the specified function when evaluated. */
void initialize_call_expr( struct function_expression *expr, struct function *func );

/* Evaluate the global-variable initialization expressions for env before program execution.
   Returns zero and assigns exception on failure. */
int initialize_globals( struct environment *env, struct variable *exception );

/* Decrement the reference-count of any referenced types,
   deallocate if necessary, and assign null to the specified variable.
   This must be called for all variables assigned during program execution. */
void dispose_variable( struct variable *var );

/* Deallocate the specified environment and all types referenced by it. */
void dispose_environment( struct environment *env );

/* Parse a statement that expects one or more expressions after the keyword. */
struct element* parse_expr_list_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev,
	enum result ( *execute )( struct statement *this, struct variables *vars, struct variable *result ),
	char *message );

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
void assign_array_variable( struct variable *src, struct array *arr, int idx );

/* Assign an exception with the specified error code and message to vars and return EXCEPTION. */
enum result throw( struct variables *vars, struct expression *source, int integer, const char *string );

/* Assign an uncatchable exception with the specified exit code and message to vars and return EXCEPTION. */
enum result throw_exit( struct variables *vars, int exit_code, const char *message );

/* Write the specified bytes as a string literal to output (if not null).
   The encoded length is returned. */
int write_byte_string( char *bytes, int count, char *output );

/* Load the specified portion of a file into buffer (if not null).
   Returns the number of bytes available or read from offset.
   Returns -1 and writes message on failure. */
long load_file( char *file_name, char *buffer, long offset, long count, char *message );

/* Unpack a 32-bit big-endian integer from str at the specified index. */
int unpack( char *str, int idx );

/* Add thread-safe custom statements and operators to the specified worker.
   Returns 0 and assigns message on failure. */
int initialize_worker( struct worker *work, char *message );

/* Begin execution of the specified worker. Returns 0 on failure. */
int start_worker( struct worker *work );

/* Lock the specified worker mutex. Returns 0 on failure. */
int lock_worker( struct worker *work );

/* Unlock the specified worker mutex. Returns 0 on failure. */
int unlock_worker( struct worker *work );

/* Wait for the completion of the specified worker.
   If cancel is non-zero, the worker should be interrupted. */
void await_worker( struct worker *work, int cancel );

/* --- */
