CC=gcc
CFLAGS=-Wall -g -W -Werror -Wextra
DEPS=tetris.h
OBJ=ai.o game.o grid.o block.o shape.o

tetris: tetris.o $(OBJ)
	$(CC) -o $@ $< $(OBJ) $(CFLAGS)

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)


