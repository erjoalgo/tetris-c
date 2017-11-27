#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include "tetris.h"
#include "tetris_ai.h"


void fatal(char* msg){
  printf("FATAL: %s", msg);
  exit(1);
}

int main(int argc, char** argv)
{
  SHAPES = shapes_read("shapes.in", &SHAPE_COUNT);
  int seed = time(NULL);
  printf( "seed %d \n", seed );
  srand(seed);
  if (argc<2)	{
    fatal("must provide subcommand");
  }else 	{
    char* opt = argv[1];
    if (!strcmp(argv[1], "ai"))	{
      int max_moves = -1;
      int depth = 1;
      int show_grid = 0;
      ai_run(max_moves, depth, show_grid);
    }else if (!strcmp(opt, "evolve"))	{
      evolution_test();
    }else if (!strcmp(opt, "test"))	{
      shape_test();
      grid_test();
      shape_stream_test();
    }else 	{
      printf( "unknown option: %s\n", opt );
      return 1;
    }
  }
  return 0;
}
