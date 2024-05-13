# towntalk

A simple interpreted programming language with a C-like syntax,
fast arithmetic, managed strings, arrays, and exceptions.

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
#include "towntalk.h"

static enum result evaluate_upcase_expression( struct expression *this,
	struct variables *vars, struct variable *result ) {
	int idx, len;
	struct string *str;
	char *input, *output;
	struct variable var = { 0 };
	enum result ret = evaluate_string( this->parameters, vars, &var );
	if( ret ) {
		input = var.string_value->string;
		len = var.string_value->length;
		str = new_string( len );
		if( str ) {
			output = str->string;
			for( idx = 0; idx < len; idx++ ) {
				output[ idx ] = toupper( input[ idx ] );
			}
			result->string_value = str;
		} else {
			ret = throw_out_of_memory( vars, this );
		}
		dispose_variable( &var );
	}
	return ret;
}

int main( int argc, char **argv ) {
	int exit_code = EXIT_FAILURE;
	char message[ 256 ] = "";
	struct environment env = { 0 };
	struct string filename = { 1, "hello", 5, STRING };
	struct operator operators[ 2 ] = { { "$upcase", '$', 1, evaluate_upcase_expression, NULL }, { NULL } };
	struct variable result = { 0 }, except = { 0 };
	struct function_expression expr = { 0 };
	struct variables vars = { 0 };
	vars.exception = &except;
	if( initialize_environment( &env, 65536, message ) && add_operators( operators, &env, message )
		&& parse_tt_program( "program hello { print $upcase( \"Hello, World!\" ); } ", &filename, &env, message ) ) {
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
