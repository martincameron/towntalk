
CC=gcc
CFLAGS=-ansi -pedantic -Wall -g

all: tt

clean:
	rm -f tt ttfx mod2tmf

tt: tt.c towntalk.c
	$(CC) $(CFLAGS) tt.c -o tt

ttfx: ttfx.c towntalk.c
	$(CC) $(CFLAGS) ttfx.c -o ttfx `sdl-config --cflags --libs`

mod2tmf: mod2tmf.c
	$(CC) $(CFLAGS) mod2tmf.c -o mod2tmf

xmtotmf: xm2tmf.c
	$(CC) $(CFLAGS) xm2tmf.c -o xmtotmf
