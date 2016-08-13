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

int main()
{
  SHAPES = shapes_read("shapes.in", &SHAPE_COUNT);
  srand(time(NULL));
  shape_test();
  grid_test();
  shape_stream_test();
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
