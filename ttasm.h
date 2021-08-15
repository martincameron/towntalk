
struct element* parse_asm_statement( struct element *elem, struct environment *env,
	struct function *func, struct statement *prev, char *message );

struct keyword asm_keyword[] = {
	{ "asm", "{", parse_asm_statement, NULL },
	{ NULL }
};
