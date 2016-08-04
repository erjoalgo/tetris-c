#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "tetris.h"

void block_init ( block* b, shape* s )	{
  b->rot = 0;
  b->offset[0] = 0;
  b->offset[1] = 0;
  b->shape = s;
}

block* block_new ( shape* s )	{
  block* b = malloc(sizeof(block));
  block_init(b, s);
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

int block_extreme ( block* b, direction d )	{
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

void block_move ( block* b, direction d, int amount )	{
  int dim = (d == BOT || d == TOP)? 1 : 0;
  if (d == LEFT ||  d == BOT)	{
    amount*=-1;
  }
  b->offset[dim]+=amount;
}

void block_rotate ( block* b, int amount )	{
  int rot = b->shape->rot_count;
  b->rot = (b->rot+amount)%rot;
  if (b->rot<0)	{
    b->rot+=rot;
  }
}

void block_print ( block* b )	{
  int i;
  // TODO rename shape.count to shape.len
  coord rc;
  for ( i = 0; i < b->shape->len; i++ )	{
    block_get(b, i, &rc);
    printf( "[%d %d]", rc[0], rc[1] );
  }
  printf( "\n" );
}
