#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "tetris.h"

shape_stream* shape_stream_new ( int max_len )	{
  shape_stream* s = malloc(sizeof(*s));
  s->max_len = max_len;
  s->i = 0;
  s->is_defined = malloc(s->max_len*sizeof(*s->is_defined));
  memset(s->is_defined, 0, s->max_len*sizeof(*s->is_defined));
  s->stream = malloc(s->max_len*sizeof(*s->stream));
  return s;
}


shape* shape_stream_get_set ( shape_stream* stream, int idx,
			      shape* set_shape )	{
  assert(idx<stream->max_len);
  int i, pop = 0;
  if (idx == -1)	{
    idx = 0;
    pop = 1;
  }
  i = (stream->i+idx)%stream->max_len;
  if (!stream->is_defined[i] || set_shape != NULL)	{
    stream->stream[i] = set_shape == NULL?
      SHAPES[RAND(SHAPE_COUNT)] : set_shape;
    stream->is_defined[i] = 1;
  }
  if (pop)	{
    stream->is_defined[i] = 0;
    stream->i++;
  }
  return stream->stream[i];
}

shape* shape_stream_peek ( shape_stream* stream, int idx )	{
  return shape_stream_get_set(stream, idx, NULL);
}

shape* shape_stream_pop ( shape_stream* stream ) {
  return shape_stream_get_set(stream, -1, NULL);
}

void game_move_print ( game_move* gm )	{
  block b;
  b.shape = gm->shape;
  b.offset[0] = gm->col;
  b.offset[1] = 0;
  b.rot = gm->rot;
  block_print(&b);
}

void shape_stream_test (  )	{
  int max_len = 5;
  shape_stream* ss = shape_stream_new(max_len);
  int i;
  for ( i = 0; i < 10; i++ )	{
    assert(shape_stream_pop(ss) != NULL);
  }
  for ( i = 0; i < max_len; i++ )	{
    shape_stream_get_set(ss, i, SHAPES[i]);
    assert(shape_stream_peek(ss, i) == SHAPES[i]);
  }

  for ( i = 0; i < max_len; i++ )	{
    printf( "\n\n\n\npeek %d. actual: \n", i );
    shape_print(shape_stream_peek(ss, i), 1);
    printf( "\nexpected:\n" );
    shape_print(SHAPES[i], 1);
    assert(shape_stream_peek(ss, i) == SHAPES[i]);
  }

  for ( i = 0; i < max_len; i++ )	{
    assert(shape_stream_pop(ss) == SHAPES[i]);
  }
}

int game_cycle_next_move ( grid* g, block* b, shape_stream* ss ) {
  shape* s = shape_stream_pop(ss);
  block_init(b, s);
  return grid_block_center_elevate(g, b);
}
