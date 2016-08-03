#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "tetris.h"

block* block_new ( shape* s )	{
  block* b = malloc(sizeof(block));
  b->rot = 0;
  b->offset[0] = 0;
  b->offset[1] = 0;
  b->shape = s;
  return b;
}

void block_get ( block* b, int i, coord* result )	{
  int* rot = b->shape->rot[b->rot][i];
  (*result)[0] = rot[0] + b->offset[0];
  (*result)[1] = rot[1] + b->offset[1];
}

void block_crust_get ( block* b, direction d, int i, coord* result )	{
  int* crust = b->shape->crust[b->rot][d][i];
  // TODO make sure this is correct order
  (*result)[0] = crust[0] + b->offset[0];
  (*result)[1] = crust[1] + b->offset[1];
}

int extreme ( block* b, direction d )	{
  switch(d){
  case LEFT:
    // assert(min_dim(b->shape->rot[b->rot], b->shape->len, 0) == 0);
    return b->offset[0];
  case BOT:
    // assert(min_dim(b->shape->rot[b->rot], b->shape->len, 1) == 0);
    return b->offset[1];
  case RIGHT:
    return b->shape->rot_wh[b->rot][0] + b->offset[0] -1;
  case TOP:
    return b->shape->rot_wh[b->rot][1] + b->offset[1] -1;
  default:
    assert(0);return 0;
  }
};

void move ( block* b, direction d, int amount )	{
  int dim = (d == BOT || d == TOP)? 1 : 0;
  if (d == LEFT ||  d == BOT)	{
    amount*=-1;
  }
  b->offset[dim]+=amount;
}

void rotate ( block* b, int amount )	{
  int rot = b->shape->rot_count;
  b->rot = (b->rot+amount)%rot;
  if (b->rot<0)	{
    b->rot+=rot;
  }
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
    int max_amnt = extreme(b, BOT);
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

void drop ( grid* g, block* b )	{
  int amount = drop_amount(g, b);
  move(b, BOT, amount);
  // assert(block_valid(g, b));
}

void print_block ( block* b )	{
  int i;
  // TODO rename shape.count to shape.len
  coord rc;
  for ( i = 0; i < b->shape->len; i++ )	{
    block_get(b, i, &rc);
    printf( "[%d %d]", rc[0], rc[1] );
  }
  printf( "\n" );
}
