CC=gcc
CFLAGS=-Wall -W -Wextra -Ofast -DNDEBUG -funroll-all-loops  --param max-unroll-times=200  # -fno-inline -fno-inline-functions  # -g -funroll-loops -fno-inline-functions #-fno-inline #-fno-omit-frame-pointer # -fverbose-asm -fpic
# CFLAGS=-Wall -W -Werror -Wextra -fno-inline -g -fno-inline-functions # -funroll-loops --param max-unroll-times=200 -fno-inline-functions #-fno-inline #-fno-omit-frame-pointer # -fverbose-asm -fpic
DEPS=tetris.h tetri_ai.h
OBJ=ai.o game.o grid.o block.o shape.o
OBJ_EXTRA=evolution.o tetris-ncurses.o tetris-play.o

all: tetris

tetris: tetris.o $(OBJ) $(OBJ_EXTRA)
	$(CC) -o $@ $< $(OBJ) $(OBJ_EXTRA) $(CFLAGS) -lncurses

tetris-prof: tetris.o $(OBJ)
	$(CC) -o $@ $< $(OBJ) $(CFLAGS) -lncurses

libtetris.so: tetris.o $(OBJ) evolution.o
	$(CC) -shared -Wl,-soname,$@.1 -o $@ $< $(OBJ) evolution.o $(CFLAGS)

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)



perf.data: FORCE tetris-prof
	perf record --call-graph dwarf -g ./tetris-prof ai -d 4 -m 1000

call.svg: perf.data
	perf script | gprof2dot -f perf | dot -Tsvg -o > $@
	firefox --new-tab $@

clean:
	rm -f *.o *.s *.so prof gmon.out call.svg tetris perf.data*

FORCE:
