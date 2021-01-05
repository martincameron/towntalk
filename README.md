# towntalk

A simple interpreted programming language with a C-like syntax,
fast integer arithmetic, managed strings, arrays, and exceptions.

An SDL extension is provided for basic input, 2D graphics, sound, and 
a limited form of multi-threading.

Not to be taken too seriously, but lightweight, portable and easy to extend.

Here's an example of a program with an upper-case function:

```
function upcase( str ) {
	var idx = 0, len = $len( str ), arr = $array( len );
	while <( idx len ) {
		var chr = $chr( str idx );
		if >( chr 95 ) {
			let chr = -( chr 32 );
		}
		let [ arr idx ] = chr;
		inc idx;
	}
	return $sub( arr, 0, len );
}

program hello {
	print upcase( "Hello, World!" );
}
```

Here's how you might add a native upper-case expression to an embedded program:

```C
#include "stdio.h"
#include "ctype.h"
#include "string.h"

#include "towntalk.h"

static enum result evaluate_upcase_expression( struct expression *this, struct variable *variables,
	struct variable *result, struct variable *exception ) {
	int idx, len;
	enum result ret;
	struct string *str;
	struct variable var = { 0 };
	ret = this->parameters->evaluate( this->parameters, variables, &var, exception );
	if( ret ) {
		if( var.string_value ) {
			str = new_string_value( strlen( var.string_value->string ) );
			if( str ) {
				for( idx = 0, len = var.string_value->length; idx < len; idx++ ) {
					str->string[ idx ] = toupper( var.string_value->string[ idx ] );
				}
				dispose_variable( result );
				result->integer_value = 0;
				result->string_value = str;
			} else {
				ret = throw( exception, this, 0, OUT_OF_MEMORY );
			}
		} else {
			ret = throw( exception, this, 0, "Not a string." );
		}
		dispose_variable( &var );
	}
	return ret;
}

int main( int argc, char **argv ) {
	int exit_code = EXIT_FAILURE;
	char message[ 256 ] = "";
	struct operator upcase = { "$upcase", '$', 1, evaluate_upcase_expression, NULL };
	struct environment env = { 0 };
	struct variable result = { 0 }, except = { 0 };
	struct expression expr = { 0 };
	if( initialize_environment( &env, message ) && add_operators( &upcase, &env, message )
		&& parse_tt_program( "program hello { print $upcase( \"Hello, World!\" ); } ", "hello", &env, message ) ) {
		initialize_call_expr( &expr, env.entry_point );
		if( initialize_globals( &env, &except ) && expr.evaluate( &expr, NULL, &result, &except ) ) {
			exit_code = EXIT_SUCCESS;
		} else if( except.string_value && except.string_value->type == EXIT ) {
			if( except.string_value->string ) {
				fputs( except.string_value->string, stderr );
				fputc( '\n', stderr );
			}
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
		fprintf( stderr, "%s\n", message );
	}
	dispose_environment( &env );
	return exit_code;
}
```

Cheers,
Martin
