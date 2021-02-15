
CC=gcc
CFLAGS=-Wextra -Og
ANSI_C=-ansi -pedantic
PTHREADS=-pthread

all: tt

clean:
	rm -f tt ttmt ttfx ttfx-midi ttfx-sdl1 xm2tmf

tt: tt.c towntalk.c towntalk.h ttasm.c ttasm.h
	$(CC) $(CFLAGS) $(ANSI_C) -DASM_STATEMENT tt.c towntalk.c ttasm.c -o tt

ttmt: tt.c towntalk.c towntalk.h ttasm.c ttasm.h
	$(CC) $(CFLAGS) $(ANSI_C) $(PTHREADS) -DMULTI_THREAD -DASM_STATEMENT tt.c towntalk.c ttasm.c -o ttmt

ttfx: ttfx.c towntalk.c towntalk.h ttasm.c ttasm.h
	$(CC) $(CFLAGS) -DMULTI_THREAD -DASM_STATEMENT ttfx.c towntalk.c ttasm.c -o ttfx `sdl2-config --cflags --libs`

ttfx-midi: ttfx.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) -DALSA_MIDI ttfx.c towntalk.c -o ttfx-midi `sdl2-config --cflags --libs` -lasound

ttfx-sdl1: ttfx.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) ttfx.c towntalk.c -o ttfx-sdl1 `sdl-config --cflags --libs`

xm2tmf: xm2tmf.c
	$(CC) $(CFLAGS) $(ANSI_C) xm2tmf.c -o xm2tmf
