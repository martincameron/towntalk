
CC=gcc
CFLAGS=-Wall -g
ANSI_C=-ansi -pedantic

all: tt

clean:
	rm -f tt ttfx ttfx-midi xm2tmf

tt: tt.c towntalk.c
	$(CC) $(CFLAGS) $(ANSI_C) tt.c -o tt

ttfx: ttfx.c towntalk.c
	$(CC) $(CFLAGS) $(ANSI_C) ttfx.c -o ttfx `sdl-config --cflags --libs`

ttfx-midi: ttfx.c towntalk.c
	$(CC) $(CFLAGS) -DALSA_MIDI ttfx.c -o ttfx-midi `sdl-config --cflags --libs` -lasound

xm2tmf: xm2tmf.c
	$(CC) $(CFLAGS) $(ANSI_C) xm2tmf.c -o xm2tmf
