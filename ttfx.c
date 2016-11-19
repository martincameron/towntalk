
#include "towntalk.c"

#include "SDL.h"

#define MAX_SURFACES 16

struct fxenvironment {
	struct environment env;
	struct SDL_Surface *surfaces[ MAX_SURFACES ];
	SDL_TimerID timer;
	SDLKey key;
};

static Uint32 timer_callback( Uint32 interval, void *param ) {
	SDL_Event event;
	event.type = SDL_USEREVENT;
	event.user.code = 0;
	event.user.data1 = event.user.data2 = NULL;
	SDL_PushEvent( &event );
	return interval;
}

static void dispose_fxenvironment( struct fxenvironment *fxenv ) {
	int idx = 0;
	if( fxenv ) {
		while( idx < MAX_SURFACES ) {
			if( fxenv->surfaces[ idx ] ) {
				SDL_FreeSurface( fxenv->surfaces[ idx ] );
			}
			idx++;
		}
		if( fxenv->timer ) {
			SDL_RemoveTimer( fxenv->timer );
		}
		dispose_environment( ( struct environment * ) fxenv );
	}
}

static int execute_fxopen_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret;
	struct variable width = { 0, NULL }, height = { 0, NULL }, caption = { 0, NULL };
	struct expression *expr = this->source;
	ret = expr->evaluate( expr, variables, &width, exception );
	if( ret ) {
		expr = expr->next;
		ret = expr->evaluate( expr, variables, &height, exception );
		if( ret ) {
			expr = expr->next;
			ret = expr->evaluate( expr, variables, &caption, exception );
			if( ret ) {
				if( caption.element_value->string ) {
					SDL_WM_SetCaption( caption.element_value->string, "" );
				}
				if( SDL_SetVideoMode( width.integer_value, height.integer_value, 32, SDL_HWSURFACE ) == NULL ) {
					ret = throw( exception, this->source, 0, SDL_GetError() );
				}
				dispose_variable( &caption );
			}
			dispose_variable( &height );
		}
		dispose_variable( &width );
	}
	return ret;
}

static int execute_fxsurface_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, surf, width, height, len, idx = 0;
	struct variable params[ 4 ], *array;
	Uint32 *pixels;
	struct SDL_Surface *surface = NULL;
	struct expression *expr = this->source;
	struct fxenvironment *fxenv = ( struct fxenvironment * ) expr->function->env;
	memset( params, 0, 4 * sizeof( struct variable ) );
	ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
	expr = expr->next;
	while( ret && expr ) {
		ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
		expr = expr->next;
	}
	if( ret ) {
		surf = params[ 0 ].integer_value;
		if( surf >= 0 && surf < MAX_SURFACES ) {
			width = params[ 1 ].integer_value;
			height = params[ 2 ].integer_value;
			if( width > 0 && height > 0 ) {
				surface = SDL_CreateRGBSurface( SDL_HWSURFACE, width, height,
					32, 0xFF000000, 0x00FF0000, 0x0000FF00, 0x000000FF );
				if( surface ) {
					if( params[ 3 ].element_value ) {
						array = params[ 3 ].element_value->array;
						if( array ) {
							if( SDL_LockSurface( surface ) == 0 ) {
								idx = 0;
								len = params[ 3 ].element_value->length;
								if( len > width * height ) {
									len = width * height;
								}
								pixels = ( Uint32 * ) surface->pixels;
								while( idx < len ) {
									pixels[ idx ] = array[ idx ].integer_value;
									idx++;
								}
								SDL_UnlockSurface( surface );
							} else {
								ret = throw( exception, this->source, 0, SDL_GetError() );
							}
						} else {
							ret = throw( exception, this->source, 0, "Not an array." );
						}
					}
					if( ret ) {
						if( fxenv->surfaces[ surf ] ) {
							SDL_FreeSurface( fxenv->surfaces[ surf ] );
						}
						fxenv->surfaces[ surf ] = surface;
					} else {
						SDL_FreeSurface( surface );
					}
				} else {
					ret = throw( exception, this->source, 0, SDL_GetError() );
				}
			} else {
				ret = throw( exception, this->source, 0, "Invalid surface dimensions." );
			}
		} else {
			ret = throw( exception, this->source, surf, "Surface index out of bounds." );
		}
	}
	idx = 0;
	while( idx < 4 ) {
		dispose_variable( &params[ idx++ ] );
	}
	return ret;
}

static int execute_fxblit_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int ret, idx = 0;
	struct SDL_Rect src, dest;
	struct variable params[ 7 ];
	struct expression *expr = this->source;
	struct fxenvironment *fxenv = ( struct fxenvironment * ) expr->function->env;
	memset( params, 0, 7 * sizeof( struct variable ) );
	ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
	expr = expr->next;
	while( ret && expr ) {
		ret = expr->evaluate( expr, variables, &params[ idx++ ], exception );
		expr = expr->next;
	}
	if( ret ) {
		idx = params[ 0 ].integer_value;
		if( idx >= 0 && idx < MAX_SURFACES ) {
			src.x = params[ 1 ].integer_value;
			src.y = params[ 2 ].integer_value;
			src.w = params[ 3 ].integer_value;
			src.h = params[ 4 ].integer_value;
			dest.x = params[ 5 ].integer_value;
			dest.y = params[ 6 ].integer_value;
			if( SDL_BlitSurface( fxenv->surfaces[ idx ], &src, SDL_GetVideoSurface(), &dest ) ) {
				ret = throw( exception, this->source, 0, SDL_GetError() );
			}
		} else {
			ret = throw( exception, this->source, idx, "Surface index out of bounds." );
		}
	}
	idx = 0;
	while( idx < 7 ) {
		dispose_variable( &params[ idx++ ] );
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

static int execute_fxshow_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	SDL_UpdateRect( SDL_GetVideoSurface(), 0, 0, 0, 0 );
	return 1;
}

static int execute_fxsleep_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable millis = { 0, NULL };
	int ret = this->source->evaluate( this->source, variables, &millis, exception );
	if( ret && millis.integer_value > 0 ) {
		SDL_Delay( millis.integer_value );
	}
	return ret;
}

static int execute_fxtimer_statement( struct statement *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct variable millis = { 0, NULL };
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->source->function->env;
	int ret = this->source->evaluate( this->source, variables, &millis, exception );
	if( ret ) {
		if( millis.integer_value > 0 ) {
			fxenv->timer = SDL_AddTimer( millis.integer_value, timer_callback, fxenv );
			if( fxenv->timer == NULL ) {
				ret = throw( exception, this->source, millis.integer_value, "Unable to start timer." );
			}
		} else {
			if( SDL_RemoveTimer( fxenv->timer ) == 0 ) {
				ret = throw( exception, this->source, millis.integer_value, "Unable to stop timer." );
			}
		}
	}
	return ret;
}

static struct element* parse_fxopen_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxopen_statement, message );
}

static struct element* parse_fxshow_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	struct element *next = elem->next;
	struct statement *stmt = new_statement( message );
	if( stmt ) {
		stmt->execute = &execute_fxshow_statement;
		prev->next = stmt;
		next = next->next;
	}
	return next;
}

static struct element* parse_fxsurface_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxsurface_statement, message );
}

static struct element* parse_fxblit_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxblit_statement, message );
}

static struct element* parse_fxrect_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxrect_statement, message );
}

static struct element* parse_fxsleep_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxsleep_statement, message );
}

static struct element* parse_fxtimer_statement( struct element *elem, struct environment *env,
	struct function_declaration *func, struct statement *prev, char *message ) {
	return parse_expr_list_statement( elem, env, func, prev, &execute_fxtimer_statement, message );
}

static int evaluate_smillis_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = SDL_GetTicks();
	result->element_value = NULL;
	return 1;
}

static int evaluate_sfxwait_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	struct fxenvironment *fxenv = ( struct fxenvironment * ) this->function->env;
	int ret = 1;
	SDL_Event event;
	SDL_WaitEvent( &event );
	if( event.type == SDL_QUIT ) {
		ret = throw( exception, this, 0, NULL );
	} else {
		if( event.type == SDL_KEYDOWN || event.type == SDL_KEYUP ) {
			fxenv->key = event.key.keysym.sym;
		}
		dispose_variable( result );
		result->integer_value = event.type;
		result->element_value = NULL;
	}
	return ret;
}

static int evaluate_sxmouse_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	SDL_GetMouseState( &result->integer_value, NULL );
	result->element_value = NULL;
	return 1;
}

static int evaluate_symouse_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	SDL_GetMouseState( NULL, &result->integer_value );
	result->element_value = NULL;
	return 1;
}

static int evaluate_smousekey_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = SDL_GetMouseState( NULL, NULL );
	result->element_value = NULL;
	return 1;
}

static int evaluate_skeyboard_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = ( ( struct fxenvironment * ) this->function->env )->key;
	result->element_value = NULL;
	return 1;
}

static int evaluate_skeyshift_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	dispose_variable( result );
	result->integer_value = SDL_GetModState();
	result->element_value = NULL;
	return 1;
}

static struct constant fxconstants[] = {
	{ "FX_KEYDOWN", 2, NULL },
	{ "FX_KEYUP", 3, NULL },
	{ "FX_MOUSEMOTION", 4, NULL },
	{ "FX_MOUSEKEYDOWN", 5, NULL },
	{ "FX_MOUSEKEYUP", 6, NULL },
	{ "FX_TIMER", 24, NULL },
	{ NULL }
};

static struct operator fxoperators[] = {
	{ "$millis",'$', 0, &evaluate_smillis_expression, &fxoperators[ 1 ] },
	{ "$fxwait",'$', 0, &evaluate_sfxwait_expression, &fxoperators[ 2 ] },
	{ "$xmouse",'$', 0, &evaluate_sxmouse_expression, &fxoperators[ 3 ] },
	{ "$ymouse",'$', 0, &evaluate_symouse_expression, &fxoperators[ 4 ] },
	{ "$mousekey",'$', 0, &evaluate_smousekey_expression, &fxoperators[ 5 ] },
	{ "$keyboard",'$', 0, &evaluate_skeyboard_expression, &fxoperators[ 6 ] },
	{ "$keyshift",'$', 0, &evaluate_skeyshift_expression, operators }
};

static struct keyword fxstatements[] = {
	{ "fxopen", "xxx;", &parse_fxopen_statement, &fxstatements[ 1 ] },
	{ "fxshow", ";", &parse_fxshow_statement, &fxstatements[ 2 ] },
	{ "fxsurface", "xxxx;", &parse_fxsurface_statement, &fxstatements[ 3 ] },
	{ "fxblit", "xxxxxxx;", &parse_fxblit_statement, &fxstatements[ 4 ] },
	{ "fxrect", "xxxxx;", &parse_fxrect_statement, &fxstatements[ 5 ] },
	{ "fxsleep", "x;", &parse_fxsleep_statement, &fxstatements[ 6 ] },
	{ "fxtimer", "x;", &parse_fxtimer_statement, statements }
};

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
	env = calloc( 1, sizeof( struct fxenvironment ) );
	if( env ) {
		env->argc = argc - 1;
		env->argv = &argv[ 1 ];
		if( add_constants( fxconstants, env, message )
		&& add_constants( constants, env, message )  ) {
			env->statements = fxstatements;
			env->operators = fxoperators;
			if( parse_tt_file( file_name, env, message ) ) {
				if( env->entry_point ) {
					/* Initialize SDL. */
					if( SDL_Init( SDL_INIT_VIDEO | SDL_INIT_TIMER ) == 0 ) {
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
		}
		dispose_fxenvironment( ( struct fxenvironment * )env );
	} else {
		fputs( "Out of memory.\n", stderr );
	}
	return exit_code;
}
