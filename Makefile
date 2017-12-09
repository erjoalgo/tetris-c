CC=gcc
CFLAGS=-Wall -W -Wextra -Ofast -DNDEBUG -funroll-all-loops  --param max-unroll-times=200  # -fno-inline -fno-inline-functions  # -g -funroll-loops -fno-inline-functions #-fno-inline #-fno-omit-frame-pointer # -fverbose-asm -fpic
# CFLAGS=-Wall -W -Werror -Wextra -fno-inline -g -fno-inline-functions # -funroll-loops --param max-unroll-times=200 -fno-inline-functions #-fno-inline #-fno-omit-frame-pointer # -fverbose-asm -fpic
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


PROF_PARAMS=ai -d 4 -m 1000 -s1512799933
perf.data: FORCE tetris-prof
	perf record --call-graph dwarf -g ./tetris-prof $(PROF_PARAMS)

stat: FORCE tetris-prof
	perf stat ./tetris $(PROF_PARAMS)

time: FORCE tetris-prof
	time ./tetris $(PROF_PARAMS)

pprof.data: FORCE tetris-prof
	CPUPROFILE=$@ ./tetris $(PROF_PARAMS)

pprof.pg: tetris-prof pprof.data
	pprof $^ > $@

pprof.gv: tetris-prof pprof.data
	pprof --web $^

pprof.asm: tetris-prof pprof.data
	pprof --disasm='block_(add|remove)|drop|ai' $^ > $@
	less $@

call.svg: perf.data
	perf script | gprof2dot -f perf | dot -Tsvg -o > $@
	firefox --new-tab $@

clean:
	rm -f *.o *.s *.so prof gmon.out call.svg tetris perf.data*

FORCE:
