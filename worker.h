
#include "towntalk.h"

#ifndef _WORKER_H
#define _WORKER_H 1

/*
	Extension for multi-threaded subinterpreters.
*/

/* Reference-counted worker function. */
struct worker {
	struct custom_type custom;
	struct environment env;
	struct variable *args, result, exception;
	struct expression *parameters;
	char locked, worker_locked;
	struct array *strings;
	void *thread, *mutex;
	enum result ret;
};

int initialize_worker_extension( struct environment *env, char *message );

/* Add thread-safe custom statements and operators to the specified worker.
   Returns 0 and assigns message on failure. */
int initialize_worker( struct worker *work, char *message );

/* Begin execution of the specified worker. Returns 0 on failure. */
int start_worker( struct worker *work );

/* Lock the specified worker mutex. Returns 0 on failure. */
int lock_worker( struct worker *work );

/* Unlock the specified worker mutex. Returns 0 on failure. */
int unlock_worker( struct worker *work );

/* Wait for the completion of the specified worker.
   If cancel is non-zero, the worker should be interrupted. */
void await_worker( struct worker *work, int cancel );

#endif /* _WORKER_H */
