
#ifndef _TTASM_H
#define _TTASM_H 1

struct element* parse_asm_statement( struct element *elem,
	struct function *func, struct variables *vars, struct statement *prev, char *message );

struct keyword asm_keyword[] = {
	{ "asm", "{", parse_asm_statement, NULL },
	{ NULL }
};

#endif /* _TTASM_H */
