
#include "stdlib.h"

/* Towntalk (c)2020 Martin Cameron. */

/* Return value of execute/evaluate functions. */
enum result {
	EXCEPTION, OKAY, RETURN, BREAK, CONTINUE
};

/* String list. */
struct string_list {
	char *value;
	struct string_list *next;
};

/* Reference-counted string. */
struct string {
	size_t reference_count;
	int length, line;
	char *string;
};

/* Reference-counted parse-tree element. */
struct element {
	struct string str;
	struct element *child, *next;
};

/* Reference-counted array. */
struct array {
	struct string str;
	int length;
	struct variable *array;
	struct array *prev, *next;
};

/* Reference-counted function reference. */
struct function_reference {
	struct string str;
	struct function_declaration *func;
};

/* Variable value. */
struct variable {
	int integer_value;
	struct string *string_value;
};

/* Function declaration list. */
struct function_declaration {
	char *name;
	int line, num_parameters, num_variables;
	struct function_reference ref;
	struct string file;
	struct element *elem;
	struct environment *env;
	struct string_list *variable_decls, *variable_decls_tail;
	struct statement *statements, *statements_tail;
	struct function_declaration *next;
};

/* Execution environment. */
struct environment {
	int argc;
	char **argv, interrupted;
	struct array arrays;
	struct keyword *statements;
	struct operator *operators;
	struct structure *structures;
	struct global_variable *constants, *constants_tail;
	struct global_variable *globals, *globals_tail;
	struct function_declaration *functions, *entry_points;
};

/* Struct declaration list.*/
struct structure {
	char *name;
	int length;
	struct string_list *fields, *fields_tail;
	struct structure *next;
};

/* Global variables. */
struct global_variable {
	char *name;
	struct variable value;
	struct expression *initializer;
	struct global_variable *next;
};

/* Expression list. */
struct expression {
	int line, index;
	struct global_variable *global;
	struct function_declaration *function;
	struct expression *parameters, *next;
	enum result ( *evaluate )( struct expression *this, struct variable *variables,
		struct variable *result, struct variable *exception );
};

/* Statement list. */
struct statement {
	int local;
	struct variable *global;
	struct expression *source, *destination, *index;
	struct statement *if_block, *else_block, *next;
	enum result ( *execute )( struct statement *this, struct variable *variables,
		struct variable *result, struct variable *exception );
};

/* Parser keyword. */
struct keyword {
	char *name, *syntax;
	/* Parse the current declaration into env, or statement into prev->next. */
	struct element* ( *parse )( struct element *elem, struct environment *env,
		struct function_declaration *func, struct statement *prev, char *message );
	struct keyword *next;
};

/* Parser operator. */
struct operator {
	char *name, oper;
	int num_operands;
	enum result ( *evaluate )( struct expression *this, struct variable *variables,
		struct variable *result, struct variable *exception );
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

/* Add a copy of the specified statement list to env. Returns zero and writes message on failure. */
int add_statements( struct keyword *statements, struct environment *env, char *message );

/* Add a copy of the specified operator list to env. Returns zero and writes message on failure. */
int add_operators( struct operator *operators, struct environment *env, char *message );

/* Add the specified constants to env. Returns zero and writes message on failure. */
int add_constants( struct constant *constants, struct environment *env, char *message );

/* Parse the specified program file into env.
   Returns zero and writes up to 256 bytes to message on failure. */
int parse_tt_file( char *file_name, struct environment *env, char *message );

/* Parse the specified program text into env. Returns zero and writes message on failure. */
int parse_tt_program( char *program, char *file_name, struct environment *env, char *message );

/* Initialize expr to execute the specified function when evaluated. */
void initialize_function_expr( struct expression *expr, struct function_declaration *func );

/* Evaluate the global-variable initialization expressions for env before program execution.
   Returns zero and assigns exception on failure. */
int initialize_globals( struct environment *env, struct variable *exception );

/* Decrement the reference-count of any types referenced by var, and deallocate if necessary.
   This must be called for all variables assigned during program execution. */
void dispose_variable( struct variable *var );

/* Deallocate the specified environment and all types referenced by it. */
void dispose_environment( struct environment *env );

/* Parse a statement that expects one or more expressions after the keyword. */
struct element* parse_expr_list_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev,
	enum result ( *execute )( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ), char *message );

/* Allocate and return a new statement. Returns NULL and writes message on failure. */
struct statement* new_statement( char *message );

/* Allocate and return a string of the specified length and reference count of 1. */
struct string* new_string_value( int length );

/* Allocate and return a new array of the specified number of elements and reference count of 1. */
struct array* new_array( struct environment *env, int length );

/* Decrement the reference count of the specified value and deallocate if necessary. */
void unref_string( struct string *str );

/* Assign src variable to dest, managing reference counts. */
void assign_variable( struct variable *src, struct variable *dest );

/* Assign the specified error code and message to the exception variable and return EXCEPTION. */
enum result throw( struct variable *exception, struct expression *source, int integer, const char *string );

/* Assign an uncatchable exception variable with the specified exit code and return EXCEPTION. */
enum result throw_exit( struct variable *exception, int exit_code );

/* Load the specified file into buffer (if not null) and returns the file length.
   Returns -1 and writes message on failure. */
long load_file( char *file_name, char *buffer, char *message );

/* Return the index of the last separator char encountered in str. */
int chop( char *str, const char *separators );

/* Unpack a 32-bit big-endian integer from str at the specified index. */
int unpack( char *str, int idx );

/* --- */
