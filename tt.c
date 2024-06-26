
#include "errno.h"
#include "signal.h"
#include "stdio.h"
#include "string.h"

#if defined( MULTI_THREAD )
#include "pthread.h"
#endif

#include "worker.h"

#if defined( ASM_STATEMENT )
#include "ttasm.h"
#endif

#define MAX_STACK 1048576

static struct environment env;

static void interrupt_handler( int signum ) {
	signal( signum, interrupt_handler );
	env.interrupted = 1;
	if( env.worker ) {
		/* Terminate current worker. */
		( ( struct worker * ) env.worker )->env.interrupted = 1;
	}
}

#if defined( MULTI_THREAD )
void* worker_thread( void *data ) {
	struct variables vars = { 0 };
	struct function_expression expr = { 0 };
	struct worker *work = ( struct worker * ) data;
	vars.exception = &work->exception;
	vars.func = work->env.entry_point;
	initialize_entry_point( &expr, vars.func );
	expr.expr.parameters = work->parameters;
	work->ret = expr.expr.evaluate( &expr.expr, &vars, &work->result );
	return NULL;
}

/* Add thread-safe custom statements and operators to the specified worker.
   Returns 0 and assigns message on failure. */
int initialize_worker( struct worker *work, char *message ) {
#if defined( ASM_STATEMENT )
	return add_statements( asm_keyword, &work->env, message );
#else
	return 1;
#endif
}

/* Begin execution of the specified worker. Returns 0 on failure. */
int start_worker( struct worker *work ) {
	pthread_t *thread = calloc( 1, sizeof( pthread_t ) );
	pthread_mutex_t *mutex = calloc( 1, sizeof( pthread_mutex_t ) );
	work->mutex = mutex;
	work->thread = thread;
	if( mutex && pthread_mutex_init( mutex, NULL ) == 0 ) {
		if( thread && pthread_create( thread, NULL, worker_thread, work ) == 0 ) {
			return 1;
		} else {
			pthread_mutex_destroy( mutex );
		}
	}
	free( thread );
	work->thread = NULL;
	free( mutex );
	work->mutex = NULL;
	return 0;
}

/* Lock the specified worker mutex. Returns 0 on failure. */
int lock_worker( struct worker *work ) {
	return work->mutex == NULL || pthread_mutex_lock( ( pthread_mutex_t * ) work->mutex ) == 0;
}

/* Unlock the specified worker mutex. Returns 0 on failure. */
int unlock_worker( struct worker *work ) {
	return work->mutex == NULL || pthread_mutex_unlock( ( pthread_mutex_t * ) work->mutex ) == 0;
}

/* Wait for the completion of the specified worker.
   If cancel is non-zero, the worker should be interrupted. */
void await_worker( struct worker *work, int cancel ) {
	if( work->thread ) {
		if( cancel ) {
			work->env.interrupted = 1;
		}
		pthread_join( ( ( pthread_t * ) work->thread )[ 0 ], NULL );
		pthread_mutex_destroy( ( pthread_mutex_t * ) work->mutex );
		free( work->thread );
		free( work->mutex );
		work->thread = work->mutex = NULL;
	}
}
#endif

int main( int argc, char **argv ) {
	int exit_code = EXIT_FAILURE;
	char *file_name, message[ 256 ] = "";
	struct variable result = { 0 }, except = { 0 };
	struct function_expression expr = { 0 };
	struct variables vars = { 0 };
	struct function *func = NULL;
	struct array *arr;
	vars.exception = &except;
	/* Handle command-line.*/
	if( argc < 2 ) {
		fprintf( stderr, "Usage: %s program.tt [args]\n", argv[ 0 ] );
		return EXIT_FAILURE;
	}
	file_name = argv[ 1 ];
	/* Install signal handler. */
	if( signal( SIGINT, interrupt_handler ) != SIG_ERR ) {
		/* Parse program file. */
		if( initialize_environment( &env, MAX_STACK, message )
		&& initialize_worker_extension( &env, message )
#if defined( ASM_STATEMENT )
		&& add_statements( asm_keyword, &env, message )
#endif
		&& parse_tt_file( file_name, &env, message ) ) {
			env.argc = argc - 1;
			env.argv = &argv[ 1 ];
			if( env.entry_point ) {
				/* Evaluate the last entry-point function. */
				initialize_entry_point( &expr, env.entry_point );
				if( initialize_globals( &env, &except ) && expr.expr.evaluate( &expr.expr, &vars, &result ) ) {
					exit_code = EXIT_SUCCESS;
				} else if( except.string_value && except.string_value->type == EXIT ) {
					if( except.string_value->string ) {
						fputs( except.string_value->string, stderr );
						fputc( '\n', stderr );
					}
					exit_code = except.number_value;
				} else {
					fprintf( stderr, "Unhandled exception %d.\n", ( int ) except.number_value );
					if( except.string_value && except.string_value->string ) {
						if( except.string_value->type == ARRAY ) {
							arr = ( struct array * ) except.string_value;
							if( arr->length > 0 && arr->string_values && arr->string_values[ 0 ]->type == FUNCTION ) {
								/* Stack-trace. */
								func = ( struct function * ) arr->string_values[ 0 ];
							}
						}
						if( func ) {
							fprintf( stderr, "%s (on line %d of '%s')\n", arr->str.string, ( int ) arr->number_values[ 0 ], func->file->string );
						} else {
							fprintf( stderr, "%s\n", except.string_value->string );
						}
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
	} else {
		fprintf( stderr, "Unable to install signal handler: %s\n", strerror( errno ) );
	}
	dispose_environment( &env );
	return exit_code;
}
