
#include "towntalk.c"

int main( int argc, char **argv ) {
	int exit_code = EXIT_FAILURE;
	char *file_name, message[ 256 ] = "";
	struct environment *env;
	struct variable result, except;
	struct expression expr;
	/* Handle command-line.*/
	if( argc < 2 ) {
		fprintf( stderr, "Usage: %s program.tt [args]\n", argv[ 0 ] );
		return EXIT_FAILURE;
	}
	file_name = argv[ 1 ];
	/* Parse program file. */
	env = calloc( 1, sizeof( struct environment ) );
	if( env ) {
		env->argc = argc - 1;
		env->argv = &argv[ 1 ];
		if( add_constants( constants, env, message ) ) {
			env->statements = statements;
			env->operators = operators;
			if( parse_tt_file( file_name, env, message ) ) {
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
		}
		dispose_environment( env );
	} else {
		fputs( "Out of memory.\n", stderr );
	}
	return exit_code;
}
