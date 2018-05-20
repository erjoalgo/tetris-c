ACLOCAL_AMFLAGS = -I m4

CC=gcc
AMLAGS=-Wall -W -Wextra -Ofast -DNDEBUG -funroll-all-loops --param max-unroll-times=200
OBJ_C=ai.c game.c grid.c block.c shape.c evolution.c

# OBJ_EXTRA=evolution.o tetris-ncurses.o tetris-play.o
# OBJ=ai.o game.o grid.o block.o shape.o

# all: tetris

# tetris: tetris.o $(OBJ) $(OBJ_EXTRA)
# 	$(CC) -o $@ $< $(OBJ) $(OBJ_EXTRA) $(CFLAGS) -lncurses

# tetris-prof: tetris.o $(OBJ)
# 	$(CC) -o $@ $< $(OBJ) $(CFLAGS) -lncurses

lib_LTLIBRARIES = libtetris.la
libtetris_la_SOURCES = $(OBJ_C)
# tetris_LDADD = libncurses


bin_PROGRAMS = tetris
tetris_SOURCES = tetris.c
tetris_LDADD = libtetris.la

# EXTRA_tetris_SOURCES = tetris-ncurses.c tetris-play.c

# if HAVE_LIBNCURSES
if USE_LIBNCURSES
tetris_SOURCES += tetris-play.c tetris-ncurses.c
endif

# TODO make
# AX_WITH_CURSES

# libtetris.so: $(OBJ) evolution.o
# 	$(CC) -shared -Wl,-soname,$@.1 -o $@ $(OBJ) evolution.o $(CFLAGS) -lc

# .c.o: $(DEPS)
# 	$(CC) -c -o $@ $< $(CFLAGS)

perf.data: FORCE tetris-prof
	perf record --call-graph dwarf -g ./tetris-prof ai -d 4 -m 1000

call.svg: perf.data
	perf script | gprof2dot -f perf | dot -Tsvg -o > $@
	firefox --new-tab $@

clean-local:
	rm -f *.o *.s *.so prof gmon.out call.svg tetris perf.data*

FORCE:

service: libtetris.so FORCE
	cd service; make
