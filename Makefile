
CC=gcc
CFLAGS=-std=c99 -Wall -g

all: tt

clean:
	rm -f tt ttfx xm2tmf

tt: tt.c towntalk.c
	$(CC) $(CFLAGS) tt.c -o tt

ttfx: ttfx.c towntalk.c
	$(CC) $(CFLAGS) ttfx.c -o ttfx `sdl-config --cflags --libs`

ttfx-midi: ttfx.c towntalk.c
	$(CC) $(CFLAGS) -DMIDI ttfx.c -o ttfx `sdl-config --cflags --libs` -lasound

xm2tmf: xm2tmf.c
	$(CC) $(CFLAGS) xm2tmf.c -o xm2tmf
