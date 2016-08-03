#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "tetris.h"

grid* grid_new ( int height, int width )	{
  grid* g = malloc(sizeof(grid));
  g->height = height;
  g->width = width;
  g->rows = malloc(height*sizeof(*g->rows));
  int r;
  for ( r = 0; r < height; r++ )	{
    g->rows[r] = malloc(width*sizeof(*g->rows));
    memset(g->rows[r], 0, g->width*sizeof(*g->rows[r]));
  }

  g->relief = malloc(width*sizeof(g->relief));
  int c;
  for ( c = 0; c < g->width; c++ )	{
    g->relief[c] = -1;
  }

  g->row_fill_count = malloc(height*sizeof(g->row_fill_count));
  memset(g->row_fill_count, 0, g->width*sizeof(*g->row_fill_count));

  g->full_rows = malloc(height*sizeof(g->full_rows));

  g->virtual_blocks_c = 0;
  g->total_cleared_count = 0;
  g->last_cleared_count = 0;
  return g;
}

int grid_height_at_start_at ( grid* g, int x, int start_at )	{
  // return the largest y s.t. g.grid[x][y]==1,
  // or -1 if no such y exists
  int y;
  for ( y = start_at; y >= 0; y-- )	{
    if (g->rows[y][x] != 0)	{
      break;
    }
  }
  return y;
}

int grid_height_at ( grid* g, int x ){
  return grid_height_at_start_at(g, x, g->height-1);
}

void grid_block_set_color ( grid* g, block* b, int color )	{
  // add block, updating relief, row_fill_count, needs_clear
  int i = 0;
  coord c;
  for ( i = 0; i < b->shape->len; i++ )	{
    block_get(b, i, &c);
    int x = c[0];
    int y = c[1];
    g->rows[y][x] = color;
    if (color == 0)	{
      // TODO remove x, y refs
      g->row_fill_count[y] -= 1;
      if (g->relief[x] == y)	{
	g->relief[x] = grid_height_at_start_at(g, x, y-1);
      }
    }else 	{
      g->row_fill_count[y] += 1;
      if (g->row_fill_count[y] == g->width)	{
	g->full_rows[g->full_rows_count++] = y;
      }
      assert(g->relief[x] != y);
      if (g->relief[x]<y)	{
	g->relief[x] = y;
      }
    }
  }
}

void grid_block_add ( grid* g, block* b )	{
  grid_block_set_color(g, b, 1);
}

void grid_block_remove ( grid* g, block* b )	{
  grid_block_set_color(g, b, 0);
}

int cmp_rev (const void* a, const void* b  )	{
  // descending order
  int f = *((int*)a);
  int s = *((int*)b);
  if (f > s) return  -1;
  if (f < s) return 1;
  return 0;
}

int max(int* heights, int count){
  int mx = heights[0];
  int i;
  for ( i = 1; i < count; i++ )	{
    int curr = heights[i];
    mx = curr>mx?curr:mx;
  }
  return mx;
}

void clear_lines ( grid* g )	{
  if (g->full_rows_count == 0)	{
    return ;
  }
  assert(g->full_rows_count>0);
  int cleared_count = 0;
  int* cleared[g->full_rows_count];
  //smallest last. small values means near bottom of the grid
  // that is, descending order.
  // why did I pick descending order?
  qsort(g->full_rows, g->full_rows_count, sizeof(int), cmp_rev);
  // smallest
  int y = g->full_rows[g->full_rows_count-1];
  int ymax = max (g->relief, g->width);
  assert(ymax<g->height);
  assert(g->row_fill_count[y] == g->width);
  assert(g->full_rows[g->full_rows_count-1] == y);

  int nextNonFull = y+1;
  while (nextNonFull<=ymax)	{
    // copy next non-full row into y
    // y is either full or has already been copied by a lower y
    // if it is full, we zero it and save it for the end

    // find the next nonFull
    assert(nextNonFull<g->height);
    while (g->row_fill_count[nextNonFull] == g->width)	{
      nextNonFull++;
      // it should be (almost) impossible for the highest row to get full
      // however, it is still possible,eg if new shape exactly fits into top row
      // this could happen only intentionally
      // so for now ignore this rare edge case
      assert(nextNonFull<g->height);
    }
    if (nextNonFull>ymax)	{
      // there is no next non full to copy into a row below
      break;
    }
    // invariant: nextNonfull should be not full
    assert(g->row_fill_count[nextNonFull] != g->width);

    if (g->row_fill_count[y]==g->width) {
      // in this case, save row y for the end
      assert(g->full_rows[g->full_rows_count-1] == y);
      g->full_rows_count--;
      cleared[cleared_count++] = g->rows[y];
    }
    // reuse the row, no need to allocate new memory
    // copy next-non-full into y
    // y was previously a next-non-full and already copied
    // or y is full and we saved it
    g->rows[y] = g->rows[nextNonFull];
    // g->row_fill_count[y] must have already been used by some lower row
    // or it was a full row, and it is appened to cleared
    // cleared.length + ?  = y- ymin
    g->row_fill_count[y] = g->row_fill_count[nextNonFull];

    y++;
    nextNonFull ++;
  }
  // now there are left-over rows that were cleared
  // they need to be zeroed-out, and replaces into rows[y...ymax]
  g->total_cleared_count+=cleared_count;
  g->last_cleared_count=cleared_count;
  assert(cleared_count>0);
  while (cleared_count--)	{
    g->rows[y] = cleared[cleared_count];
    g->row_fill_count[y] = 0;
    memset(g->rows[y], 0, g->width*sizeof(*g->rows[y]));

    y++;
  }

  // now we need to update relief
  int i;
  for ( i = 0; i < g->width; i++ )	{
    g->relief[i] = grid_height_at_start_at(g, i, g->relief[i]);
  }
  // we should be done.
  // should assert consistency

}

void check_consistency ( grid* g )	{
  int i;
  for ( i = 0; i < g->width; i++ )	{
    assert(g->relief[i] == grid_height_at(g, i));
  }
  int r;
  for ( r = 0; r < g->height; r++ )	{
    int count = 0;
    int c;
    for ( c = 0; c < g->width; c++ )	{
      count += g->rows[r][c]?1:0;
    }
    assert(g->row_fill_count[r] == count);
  }
  for ( i = 0; i < g->full_rows_count; i++ )	{
    r = g->full_rows[i];
    assert(g->row_fill_count[r] == g->width);
  }
}

void add_virtual_block(grid* g, block* b){
  g->virtual_blocks[0] = b;
}

void remove_virtual_block(grid* g){
  g->virtual_blocks[0] = NULL;
}

int grid_equal(grid* a, grid* b){
  if (a->width != b->width || a->height != b->height)	{
    return 0;
  }
  int r,c;
  for ( r = 0; r < a->height; r++ )	{
    for ( c = 0; c < a->height; c++ )	{
      if (a->rows[r][c] != b->rows[r][c])	{
	return 0;
      }
    }
  }
  // TODO virtualblocks
  return 1;
}

int in_bounds ( grid* g, block* b )	{
  return extreme(b, LEFT)>=0 &&
    extreme(b, RIGHT)<g->width &&
    extreme(b, BOT)>=0 &&
    extreme(b, TOP)<g->height;
}

int intersects ( grid* g, block* b )	{
  assert(in_bounds(g, b));
  if (max(g->relief, g->width)<extreme(b, BOT))	{
    return 0;
  }
  int i;
  coord cr;
  for ( i = 0; i < b->shape->len; i++ )	{
    block_get(b, i, &cr);
    int r = cr[1];
    int c = cr[0];
    if (g->rows[r][c])	{
      return 1;
    }
  }
  return 0;
}

int block_valid ( grid* g, block* b )	{
  return in_bounds(g, b) && !intersects(g, b);
}

void block_center_top (grid* g, block* b){
  // assert(extreme(b, BOT) == 0); this makes no sense here
  int rot = b->rot;
  b->offset[1] = g->height - 1 - b->shape->rot_wh[rot][1];
  b->offset[0] = (g->width - b->shape->rot_wh[rot][0])/2;
  assert(in_bounds(g, b));
  assert(b->shape->max_dim_len<g->width);
}

void print_grid ( grid* g )	{
  printf( "\n" );
  int row, col;
  for ( row = g->height-1; row >= 0; row-- )	{
    // TODO how to print entire row of memory at once
    // TODO include virtual blocks
    for ( col = 0; col < g->width; col++ )	{
      printf("%c", g->rows[row][col]? '*' : ' ');
    }
    printf( "|\n" );
  }
  for ( col = 0; col < g->width; col++ )	{
    printf("%c", 'T');
  }
  printf( "\n" );
}

void grid_apply_moves ( grid* g, game_move* stream, int stream_count )	{
  int i;
  for ( i = 0; i < stream_count; i++ )	{
    game_move move = stream[i];
    block* b = block_new(move.shape);
    block_center_top(g, b);
    b->offset[0] = move.col;
    assert(block_valid(g, b));
    b->rot = move.rot;
    drop(g, b);
    grid_block_add(g, b);
    clear_lines(g);
  }
}

void print_arr ( int* arr, int len )	{
  printf( "[ " );
  int i;
  for ( i = 0; i < len; i++ )	{
    printf( "%d ", arr[i] );
  }
  printf( "]\n" );
}

void print_relief ( grid* g )	{
  int i;
  printf( "relief:   " );
  print_arr(g->relief, g->width);
  printf( "heigh at: " );
  for ( i = 0; i < g->width; i++ )	{
    printf( "%d ", grid_height_at(g, i) );
  }
  printf( "\n" );
}

void block_move_safe ( grid* g, block* b, int direction, int amount )	{
  move(b, direction, amount);
  if (!block_valid(g, b))	{
    move(b, direction, -amount);
  }
}

void block_rotate_safe ( grid* g, block* b, int amount )	{
  rotate(b, amount);
  if (!block_valid(g, b))	{
    rotate(b, -amount);
  }
}
