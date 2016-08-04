CC=gcc
CFLAGS=-Wall -g -W -Wall -Werror -Wextra
DEPS=tetris.h
OBJ=tetris.o grid.o block.o shape.o

tetris: $(OBJ)
	$(CC) -o $@ $(OBJ) $(CFLAGS)

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

grid.o: grid.c block.o $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

