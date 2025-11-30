
CC=gcc
CFLAGS=-Wall -Og
ANSI_C=-ansi -pedantic
PTHREADS=-pthread

LIBM=-lm
LIBASOUND=-lasound
SDL1=`sdl-config --cflags --libs`
SDL2=`sdl2-config --cflags --libs`

all: tt

clean:
	rm -fv tt ttfp ttfx ttfx-midi ttfx-sdl1 xm2tmf *.gcov *.gcda *.gcno

tt: tt.c towntalk.c towntalk.h optimizer.c worker.c worker.h ttasm.c ttasm.h
	$(CC) $(CFLAGS) $(ANSI_C) -DOPTIMIZER -DASM_STATEMENT tt.c towntalk.c optimizer.c worker.c ttasm.c -o tt

ttfp: tt.c towntalk.c towntalk.h optimizer.c worker.c worker.h ttasm.c ttasm.h
	$(CC) $(CFLAGS) $(ANSI_C) $(PTHREADS) -DMULTI_THREAD -DFLOATING_POINT -DOPTIMIZER -DASM_STATEMENT tt.c towntalk.c optimizer.c worker.c ttasm.c -o ttfp $(LIBM)

ttfx: ttfx.c towntalk.c towntalk.h optimizer.c worker.c worker.h ttasm.c ttasm.h
	$(CC) $(CFLAGS) -DMULTI_THREAD -DFLOATING_POINT -DOPTIMIZER -DASM_STATEMENT ttfx.c towntalk.c optimizer.c worker.c ttasm.c -o ttfx $(SDL2) $(LIBM)

ttfx-midi: ttfx.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) -DALSA_MIDI ttfx.c towntalk.c -o ttfx-midi $(SDL2) $(LIBASOUND)

ttfx-sdl1: ttfx.c towntalk.c towntalk.h
	$(CC) $(CFLAGS) ttfx.c towntalk.c -o ttfx-sdl1 $(SDL1)

xm2tmf: xm2tmf.c
	$(CC) $(CFLAGS) $(ANSI_C) xm2tmf.c -o xm2tmf
