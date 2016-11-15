
#include "towntalk.c"

#include "SDL/SDL.h"

static int execute_fxopen_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret;
	struct variable width = { 0, NULL }, height = { 0, NULL };
	struct expression *expr = this->source;
	ret = expr->evaluate( expr, variables, &width, exception );
	if( ret ) {
		expr = expr->next;
		ret = expr->evaluate( expr, variables, &height, exception );
		if( ret ) {
			if( SDL_SetVideoMode( width.integer_value, height.integer_value, 32, SDL_HWSURFACE ) == NULL ) {
				ret = throw( exception, this->source, 0, SDL_GetError() );
			}
			dispose_variable( &height );
		}
		dispose_variable( &width );
	}
	return ret;
}

static int execute_fxrect_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, idx = 0;
	struct SDL_Rect rect;
	struct variable params[ 5 ];
	struct expression *expr = this->source;
	memset( params, 0, 5 * sizeof( struct variable ) );
	ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
	expr = expr->next;
	while( ret && expr ) {
		ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
		expr = expr->next;
	}
	if( ret ) {
		rect.x = params[ 0 ].integer_value;
		rect.y = params[ 1 ].integer_value;
		rect.w = params[ 2 ].integer_value;
		rect.h = params[ 3 ].integer_value;
		if( SDL_FillRect( SDL_GetVideoSurface(), &rect, params[ 4 ].integer_value ) ) {
			ret = throw( exception, this->source, 0, SDL_GetError() );
		}
	}
	idx = 0;
	while( idx < 5 ) {
		dispose_variable( &params[ idx++ ] );
	}
	return ret;
}

static int execute_fxflip_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret = 1;
	if( SDL_Flip( SDL_GetVideoSurface() ) ) {
		ret = throw( exception, this->source, 0, SDL_GetError() );
	}
	return ret;
}

static struct element* parse_fxopen_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxopen_statement, message );
}

static struct element* parse_fxflip_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		stmt->execute = &execute_fxflip_statement;
		prev->next = stmt;
		next = next->next;
	}
	return next;
}

static struct element* parse_fxrect_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxrect_statement, message );
}

static struct keyword fxstatements[] = {
	{ "fxopen", "xx;", &parse_fxopen_statement, &fxstatements[ 1 ] },
	{ "fxflip", ";", &parse_fxflip_statement, &fxstatements[ 2 ] },
	{ "fxrect", "xxxxx;", &parse_fxrect_statement, statements }
};

int main( int argc, char **argv ) {
	int success, exit_code = EXIT_FAILURE;
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
		env->statements = fxstatements;
		env->operators = operators;
		success = parse_tt_file( file_name, env, message );
		if( success ) {
			if( env->entry_point ) {
				/* Initialize SDL. */
				success = SDL_Init( SDL_INIT_VIDEO );
				if( success == 0 ) {
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
					SDL_Quit();
				} else {
					fprintf( stderr, "Unable to initialise SDL: %s\n", SDL_GetError() );
				}
			} else {
				fprintf( stderr, "No programs found.\n" );
			}
		} else {
			fprintf( stderr, "%s\n", message );
		}
		dispose_environment( env );
	} else {
		fputs( "Out of memory.\n", stderr );
	}
	return exit_code;
}
