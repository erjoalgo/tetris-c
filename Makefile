CC=gcc
CFLAGS=-Wall -g -W -Werror -Wextra
DEPS=tetris.h
OBJ=tetris.o ai.c game.c grid.o block.o shape.o

tetris: $(OBJ)
	$(CC) -o $@ $(OBJ) $(CFLAGS)

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)


