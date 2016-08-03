CC=gcc
CFLAGS=-Wall -g
DEPS=tetris.h
OBJ=tetris.o grid.o block.o shape.o

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

grid.o: grid.c block.o $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

tetris: $(OBJ)
	$(CC) -o $@ $(OBJ) $(CFLAGS)
