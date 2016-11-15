
CC=gcc
CFLAGS=-ansi -pedantic -Wall -g

all: tt

clean:
	rm -f tt ttfx

tt: tt.c towntalk.c
	$(CC) $(CFLAGS) tt.c -o tt

ttfx: ttfx.c towntalk.c
	$(CC) $(CFLAGS) ttfx.c -o ttfx `sdl-config --cflags --libs`
