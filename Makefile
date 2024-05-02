
CC=gcc
CFLAGS=-Wextra -Wunused-variable -Og
ANSI_C=-ansi -pedantic
PTHREADS=-pthread

all: tt

clean:
	rm -f tt ttfp ttfx ttfx-midi ttfx-sdl1 xm2tmf

tt: tt.c towntalk.c towntalk.h optimizer.c worker.c worker.h ttasm.c ttasm.h
	$(CC) $(CFLAGS) $(ANSI_C) -DOPTIMIZER -DASM_STATEMENT tt.c towntalk.c optimizer.c worker.c ttasm.c -o tt

ttfp: tt.c towntalk.c towntalk.h optimizer.c worker.c worker.h ttasm.c ttasm.h
	$(CC) $(CFLAGS) $(ANSI_C) $(PTHREADS) -DMULTI_THREAD -DFLOATING_POINT -DOPTIMIZER -DASM_STATEMENT tt.c towntalk.c optimizer.c worker.c ttasm.c -o ttfp

ttfx: ttfx.c towntalk.c towntalk.h optimizer.c worker.c worker.h ttasm.c ttasm.h
	$(CC) $(CFLAGS) -DMULTI_THREAD -DOPTIMIZER -DASM_STATEMENT ttfx.c towntalk.c optimizer.c worker.c ttasm.c -o ttfx `sdl2-config --cflags --libs`

ttfx-midi: ttfx.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) -DALSA_MIDI ttfx.c towntalk.c -o ttfx-midi `sdl2-config --cflags --libs` -lasound

ttfx-sdl1: ttfx.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) ttfx.c towntalk.c -o ttfx-sdl1 `sdl-config --cflags --libs`

xm2tmf: xm2tmf.c
	$(CC) $(CFLAGS) $(ANSI_C) xm2tmf.c -o xm2tmf
