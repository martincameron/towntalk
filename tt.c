
#include "errno.h"
#include "signal.h"
#include "stdio.h"
#include "string.h"

#include "towntalk.h"

static struct environment *env;

static void interrupt_handler( int signum ) {
	signal( signum, interrupt_handler );
	env->interrupted = 1;
	if( env->worker ) {
		/* Terminate current worker. */
		env->worker->env->interrupted = 1;
	}
}

int main( int argc, char **argv ) {
	int exit_code = EXIT_FAILURE;
	char *file_name, message[ 256 ] = "";
	struct variable result = { 0 }, except = { 0 };
	struct expression expr = { 0 };
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
		if( initialize_environment( env, message ) ) {
			if( parse_tt_file( file_name, env, message ) ) {
				if( env->entry_points ) {
					/* Install signal handler. */
					if( signal( SIGINT, interrupt_handler ) != SIG_ERR ) {
						/* Evaluate the last entry-point function. */
						initialize_call_expr( &expr, env->entry_points );
						if( initialize_globals( env, &except ) && expr.evaluate( &expr, NULL, &result, &except ) ) {
							exit_code = EXIT_SUCCESS;
						} else if( except.string_value && except.string_value->string == NULL ) {
							exit_code = except.integer_value;
						} else {
							fprintf( stderr, "Unhandled exception %d.\n", except.integer_value );
							if( except.string_value && except.string_value->string ) {
								fprintf( stderr, "%s\n", except.string_value->string );
							}
						}
						dispose_variable( &result );
						dispose_variable( &except );
					} else {
						fprintf( stderr, "Unable to install signal handler: %s\n", strerror( errno ) );
					}
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
