
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

struct keyword statements[ 22 ];
struct operator operators[ 52 ];

/* Initialize env with the the standard statements, operators and constants.
   Returns zero and writes message on failure. */
int initialize_environment( struct environment *env, char *message );

/* Add the specified constants to env. Returns zero and writes message on failure. */
int add_constants( struct constant *constants, struct environment *env, char *message );

/* Parse the specified program file into env.
   Returns zero and writes up to 256 bytes to message on failure. */
int parse_tt_file( char *file_name, struct environment *env, char *message );

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

/* --- */
