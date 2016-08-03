#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include "tetris.h"


void fatal(char* msg){
  printf("FATAL: %s", msg);
  exit(1);
}

int main(int argc, char* argv[])
{
  int shape_count;
  shape** shapes = shapes_read("shapes.in", &shape_count);
  int i;
  for ( i = 0; i < shape_count; i++ )	{
    printf( "shape %d/%d\n", i+1, shape_count );
    shape_print(shapes[i]);
  }
  grid* g = grid_new(19, 10);
  grid_print(g);
  srand(time(NULL));
  while (1)	{
    int r = rand()%shape_count;
    block* b = block_new(shapes[r]);
    grid_block_center_top(g, b);
    if (grid_block_intersects(g, b))	{
      break;
    }
    grid_block_add(g, b);
    grid_block_remove(g, b);
    grid_block_add(g, b);
    grid_print(g);
    grid_block_remove(g, b);
    grid_block_drop(g, b);
    grid_block_add(g, b);
    grid_print(g);
  }

  game_move moves[3];
  moves[0] = (game_move) { .shape = shapes[2], .rot = 0, .col = 0 };
  moves[1] = (game_move) { .shape = shapes[2], .rot = 0, .col = 4 };
  moves[2] = (game_move) { .shape = shapes[0], .rot = 0, .col = 8 };
  g = grid_new(19, 10);
  grid_apply_moves(g, moves, 3);
  grid_print(g);
  // clear_lines(g);
  return 0;
}

/* class Grid (object):

    def get (self, y, x):
        coords = [x,y]
        for block in self.virtualBlocks:
            for i in xrange (len (block)):
                if coords == block.ith (i):
                    return 1
        return self.grid[y][x]



*/
