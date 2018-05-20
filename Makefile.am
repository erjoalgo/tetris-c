ACLOCAL_AMFLAGS = -I m4

OBJ_C=ai.c game.c grid.c block.c shape.c evolution.c

lib_LTLIBRARIES = libtetris.la
libtetris_la_SOURCES = $(OBJ_C)


bin_PROGRAMS = tetris
tetris_SOURCES = tetris.c
tetris_LDADD = libtetris.la


# EXTRA_tetris_SOURCES = tetris-ncurses.c tetris-play.c
# if HAVE_LIBNCURSES
if USE_LIBNCURSES
tetris_SOURCES += tetris-play.c tetris-ncurses.c
endif

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
