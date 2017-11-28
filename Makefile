CC=gcc
CFLAGS=-Wall -W -Werror -Wextra -Ofast -ffast-math -DNDEBUG
DEPS=tetris.h tetri_ai.h
OBJ=ai.o game.o grid.o block.o shape.o evolution.o tetris-ncurses.o tetris-play.o

all: tetris

tetris: tetris.o $(OBJ)
	$(CC) -o $@ $< $(OBJ) $(CFLAGS) -lncurses

libtetris.so: $(OBJ) evolution.o
	$(CC) -shared -o $@ $(OBJ)

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

gmon.out: tetris
	./tetris

prof: gmon.out
	gprof tetris gmon.out> $@

call.svg: prof
	gprof2dot -f prof prof | dot -Tsvg -o > $@
	firefox --new-tab $@

clean:
	rm -f *.o *.s *.so prof gmon.out call.svg tetris
