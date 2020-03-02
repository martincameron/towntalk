
CC=gcc
CFLAGS=-Wall -g
ANSI_C=-ansi -pedantic

all: tt

clean:
	rm -f tt ttfx ttfx2 ttfx-midi xm2tmf

tt: tt.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) $(ANSI_C) tt.c towntalk.c -o tt

ttfx: ttfx.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) ttfx.c towntalk.c -o ttfx `sdl-config --cflags --libs`

ttfx2: ttfx2.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) ttfx2.c towntalk.c -o ttfx2 `sdl2-config --cflags --libs`

ttfx-midi: ttfx.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) -DALSA_MIDI ttfx.c towntalk.c -o ttfx-midi `sdl-config --cflags --libs` -lasound

xm2tmf: xm2tmf.c
	$(CC) $(CFLAGS) $(ANSI_C) xm2tmf.c -o xm2tmf
