
struct element* parse_asm_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message );

struct keyword asm_keyword[] = {
	{ "asm", "{", parse_asm_statement, NULL },
	{ NULL }
};
