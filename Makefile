
CC=gcc
CFLAGS=-Wall -Og
ANSI_C=-ansi -pedantic

all: tt

clean:
	rm -f tt ttfx ttfx-midi ttfx-sdl1 xm2tmf

tt: tt.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) $(ANSI_C) tt.c towntalk.c -o tt

ttfx: ttfx.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) ttfx.c towntalk.c -o ttfx `sdl2-config --cflags --libs`

ttfx-midi: ttfx.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) -DALSA_MIDI ttfx.c towntalk.c -o ttfx-midi `sdl2-config --cflags --libs` -lasound

ttfx-sdl1: ttfx.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) ttfx.c towntalk.c -o ttfx-sdl1 `sdl-config --cflags --libs`

xm2tmf: xm2tmf.c
	$(CC) $(CFLAGS) $(ANSI_C) xm2tmf.c -o xm2tmf
