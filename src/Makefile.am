ACLOCAL_AMFLAGS = -I m4

lib_LTLIBRARIES = libtetris.la
libtetris_la_SOURCES = ai.c game.c grid.c block.c shape.c evolution.c

bin_PROGRAMS = tetris
tetris_SOURCES = tetris.c
tetris_LDADD = libtetris.la

if USE_LIBNCURSES
# EXTRA_tetris_SOURCES = tetris-ncurses.c tetris-play.c
# if HAVE_LIBNCURSES
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
