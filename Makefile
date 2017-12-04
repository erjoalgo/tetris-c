CC=gcc
CFLAGS=-Wall -W -Werror -Wextra -DNDEBUG -Ofast #-g -funroll-loops --param max-unroll-times=200 -fno-inline-functions #-fno-inline #-fno-omit-frame-pointer # -fverbose-asm -fpic
DEPS=tetris.h tetri_ai.h
OBJ=ai.o game.o grid.o block.o shape.o evolution.o tetris-ncurses.o tetris-play.o

all: tetris

tetris: tetris.o $(OBJ)
	$(CC) -o $@ $< $(OBJ) $(CFLAGS) -lncurses

tetris-prof: tetris.o $(OBJ)
	$(CC) -o $@ $< $(OBJ) $(CFLAGS) -lncurses

libtetris.so: $(OBJ) evolution.o
	$(CC) -shared -o $@ $(OBJ)

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)



perf.data: FORCE tetris-prof
	perf record --call-graph dwarf -g ./tetris-prof ai

call.svg: perf.data
	perf script | gprof2dot -f perf | dot -Tsvg -o > $@
	firefox --new-tab $@

clean:
	rm -f *.o *.s *.so prof gmon.out call.svg tetris perf.data*

FORCE:
