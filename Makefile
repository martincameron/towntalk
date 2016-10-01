
CC=gcc
CFLAGS=-ansi -pedantic -Wall -g

all: tt

clean:
	rm -f tt

tt: tt.c
	$(CC) $(CFLAGS) tt.c -o tt
