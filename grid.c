#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "tetris.h"
#define RAND(len) (rand()%(len))

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

void grid_set_color ( grid* g, int r, int c, int color )	{
  assert(!!g->rows[r][c] != !!color);
  g->rows[r][c] = color;
  if (color == 0)	{
    g->row_fill_count[r] -= 1;
    if (g->relief[c] == r)	{
      g->relief[c] = grid_height_at_start_at(g, c, r-1);
    }
  }else 	{
    g->row_fill_count[r] += 1;
    if (g->row_fill_count[r] == g->width)	{
      g->full_rows[g->full_rows_count++] = r;
    }
    assert(g->relief[c] != r);
    if (g->relief[c]<r)	{
      g->relief[c] = r;
    }
  }
}

void grid_block_set_color ( grid* g, block* b, int color )	{
  // add block, updating relief, row_fill_count, needs_clear
  int i = 0;
  coord cr;
  for ( i = 0; i < b->shape->len; i++ )	{
    block_get(b, i, &cr);
    int c = cr[0];
    int r = cr[1];
    grid_set_color(g, r, c, color);
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

int grid_clear_lines ( grid* g )	{
  if (g->full_rows_count == 0)	{
    return 0;
  }
  assert(g->full_rows_count>0);
  int expected_cleared_count = g->full_rows_count;
  int cleared_count = 0;
  int* cleared[g->full_rows_count];
  // smallest last. small values means near bottom of the grid
  // that is, descending order.
  // this is so we can just decrement the count to "pop" the smallest row
  qsort(g->full_rows, g->full_rows_count, sizeof(int), cmp_rev);
  // smallest full row
  int y = g->full_rows[g->full_rows_count-1];
  // largest occupied (full or non-full) row.
  int ymax = max (g->relief, g->width);
  assert(ymax<g->height);
  assert(g->row_fill_count[y] == g->width);
  assert(g->full_rows[g->full_rows_count-1] == y);

  int next_non_full = y+1;
  while (next_non_full<=ymax)	{
    // copy next non-full row into y, which is either full
    // or has already been copied into a lower y
    // if it is full, we zero it and save it for the end

    // find the next non-full
    assert(next_non_full<g->height);
    while (g->row_fill_count[next_non_full] == g->width)	{
      next_non_full++;
      // it should be (almost) impossible for the highest row to get full
      // however, it is still possible,eg if new shape exactly fits into top row
      // this could happen only intentionally
      // so for now ignore this rare edge case
      assert(next_non_full<g->height);
    }
    if (next_non_full>ymax)	{
      assert(next_non_full>=g->height ||
	     g->row_fill_count[next_non_full] == 0);
      // there is no next non full to copy into a row below
      break;
    }
    // invariant: next_non_full should be not full
    assert(g->row_fill_count[next_non_full] != g->width);

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
    g->rows[y] = g->rows[next_non_full];
    // g->row_fill_count[y] must have already been used by some lower row
    // or it was a full row, and it is appened to cleared
    // cleared.length + ?  = y- ymin
    g->row_fill_count[y] = g->row_fill_count[next_non_full];

    y++;
    next_non_full ++;
  }
  // now there might be left-over rows that were cleared
  // they need to be zeroed-out, and replaces into rows[y...ymax]
  assert(cleared_count+g->full_rows_count>0);
  assert(cleared_count+g->full_rows_count
	 == expected_cleared_count);
  g->total_cleared_count+=expected_cleared_count;
  g->last_cleared_count=expected_cleared_count;

  while (cleared_count+g->full_rows_count)	{
    g->rows[y] = g->full_rows_count?
      g->rows[g->full_rows[--g->full_rows_count]]:
      cleared[--cleared_count];
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
  return g->last_cleared_count;
}

void grid_assert_consistency ( grid* g )	{
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

  int* sorted_rows[g->height];
  memcpy(sorted_rows, g->rows, sizeof(*g->rows)*g->height);
  qsort(sorted_rows, g->height, sizeof(*g->rows), cmp_rev);
  for ( i = 1; i < g->height; i++ )	{
    assert(sorted_rows[i+1] != sorted_rows[i]);
  }

  int checked[g->height];
  memset(checked, 0, sizeof(checked));
  for ( i = 0; i < g->full_rows_count; i++ )	{
    r = g->full_rows[i];
    assert(g->row_fill_count[r] == g->width);
    assert(checked[r] == 0);
    checked[r] = 1;
  }

  for ( i = 0; i < g->height; i++ )	{
    if (!checked[i])	{
      assert(g->row_fill_count[i] != g->width);
    }
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

int grid_block_in_bounds ( grid* g, block* b )	{
  return block_extreme(b, LEFT)>=0 &&
    block_extreme(b, RIGHT)<g->width &&
    block_extreme(b, BOT)>=0 &&
    block_extreme(b, TOP)<g->height;
}

int grid_block_intersects ( grid* g, block* b )	{
  assert(grid_block_in_bounds(g, b));
  if (max(g->relief, g->width)<block_extreme(b, BOT))	{
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

int grid_block_valid ( grid* g, block* b )	{
  return grid_block_in_bounds(g, b) && !grid_block_intersects(g, b);
}

void grid_block_center_top (grid* g, block* b){
  // assert(extreme(b, BOT) == 0); this makes no sense here
  int rot = b->rot;
  b->offset[1] = g->height - 1 - b->shape->rot_wh[rot][1];
  b->offset[0] = (g->width - b->shape->rot_wh[rot][0])/2;
  assert(grid_block_in_bounds(g, b));
  assert(b->shape->max_dim_len<g->width);
}

int drop_amount ( grid* g, block* b )	{
  int i;
  int min_amnt = g->height-1;
  coord cr;
  for ( i = 0; i < b->shape->crust_len[b->rot][BOT]; i++ )	{
    block_crust_get(b, BOT, i, &cr);
    int c = cr[0];
    int r = cr[1];
    int amnt = r-(g->relief[c]+1);
    if (amnt<min_amnt)	{
      min_amnt = amnt;
    }
  }
  if (min_amnt<0)	{
    // relief can not help us, as we are under the relief
    min_amnt = 0;
    // assert(!intersects(g, b));
    int max_amnt = block_extreme(b, BOT);
    for ( min_amnt = 0; min_amnt<max_amnt; min_amnt++ )	{
      int next_amnt = min_amnt+1;
      for ( i = 0; i < b->shape->crust_len[b->rot][BOT]; i++ )	{
	block_crust_get(b, BOT, i, &cr);
	int r = cr[0];
	int c = cr[1];
	if (g->rows[r][c+next_amnt])	{
	  // break a;
	  goto a;
	}
      }
    }
  }
 a: return min_amnt;
}

void grid_block_drop ( grid* g, block* b )	{
  int amount = drop_amount(g, b);
  block_move(b, BOT, amount);
  // assert(block_valid(g, b));
}

void grid_print ( grid* g )	{
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
  static block* b = block_new(NULL);
  for ( i = 0; i < stream_count; i++ )	{
    game_move move = stream[i];
    block_init(b, move.shape);
    grid_block_center_top(g, b);
    b->offset[0] = move.col;
    assert(grid_block_valid(g, b));
    b->rot = move.rot;
    grid_block_drop(g, b);
    grid_block_add(g, b);
    grid_clear_lines(g);
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

void grid_block_move_safe ( grid* g, block* b, int direction, int amount )	{
  block_move(b, direction, amount);
  if (!grid_block_valid(g, b))	{
    block_move(b, direction, -amount);
  }
}

void grid_block_rotate_safe ( grid* g, block* b, int amount )	{
  block_rotate(b, amount);
  if (!grid_block_valid(g, b))	{
    block_rotate(b, -amount);
  }
}

void grid_test (  )	{
  block* b = block_new(NULL);
  grid* g = grid_new(GRID_HEIGHT, GRID_WIDTH);
  grid_assert_consistency(g);
  // test a simple flow until grid gets full
  while (1)	{
    block_init(b, SHAPES[RAND(SHAPE_COUNT)]);
    grid_block_center_top(g, b);
    if (grid_block_intersects(g, b))	{
      break;
    }
    grid_block_add(g, b);
    grid_assert_consistency(g);
    grid_block_remove(g, b);
    grid_assert_consistency(g);
    grid_block_add(g, b);
    grid_assert_consistency(g);
    grid_print(g);
    grid_block_remove(g, b);
    grid_assert_consistency(g);
    grid_block_drop(g, b);
    grid_assert_consistency(g);
    grid_block_add(g, b);
    grid_assert_consistency(g);
    grid_print(g);
  }

  // exercise clearing lines
  game_move moves[3];
  moves[0] = (game_move) { .shape = SHAPE_I, .rot = 0, .col = 0 };
  moves[1] = (game_move) { .shape = SHAPE_I, .rot = 0, .col = 4 };
  moves[2] = (game_move) { .shape = SHAPE_O, .rot = 0, .col = 8 };
  g = grid_new(GRID_HEIGHT, GRID_WIDTH);
  grid_apply_moves(g, moves, 3);
  grid_print(g);

  g = grid_new(GRID_HEIGHT, GRID_WIDTH);
  int MV_COUNT = 5;
  game_move moves[MV_COUNT];
  moves[0] = (game_move) { .shape = SHAPE_O, .rot = 0, .col = 0 };
  moves[1] = (game_move) { .shape = SHAPE_O, .rot = 0, .col = 2 };
  moves[2] = (game_move) { .shape = SHAPE_O, .rot = 0, .col = 4 };
  moves[3] = (game_move) { .shape = SHAPE_O, .rot = 0, .col = 6 };
  moves[4] = (game_move) { .shape = SHAPE_O, .rot = 0, .col = 8 };
  grid_apply_moves(g, moves, MV_COUNT);
  grid_print(g);
}
