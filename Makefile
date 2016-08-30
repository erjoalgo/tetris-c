CC=gcc
CFLAGS=-Wall -W -Werror -Wextra -Ofast -DNDEBUG -lm
DEPS=tetris.h tetri_ai.h
OBJ=ai.o game.o grid.o block.o shape.o

tetris: tetris.o $(OBJ) evolution.o
	$(CC) -o $@ $< $(OBJ) evolution.o $(CFLAGS)

tetris-play: tetris-play.o tetris-ncurses.o $(OBJ)
	$(CC) -o $@ $< tetris-ncurses.o $(OBJ) $(CFLAGS) -lncurses

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
	rm *o prof gmon.out call.svg
