#include <stdlib.h>
#include <assert.h>
#include "tetris.h"

shape_stream* shape_stream_new ( int max_len )	{
  shape_stream* s = malloc(sizeof(shape_stream*));
  s->is_defined = malloc(s->max_len*sizeof(*s->is_defined));
  s->max_len = max_len;
  s->i = 0;
  return s;
}


shape* shape_stream_peek ( shape_stream* stream, int idx )	{
  assert(idx<stream->max_len);
  int i, pop;
  if (idx == -1)	{
    idx = 0;
    pop = 1;
  }
  i = (stream->i+idx)%stream->max_len;
  if (!stream->is_defined[i])	{
    stream->stream[i] = SHAPES[RAND(SHAPE_COUNT)];
    stream->is_defined[i] = 1;
  }
  if (pop)	{
    stream->is_defined[i] = 0;
    stream->i++;
  }
  return stream->stream[i];
}

shape* shape_stream_pop ( shape_stream* stream ) {
  return shape_stream_peek(stream, -1);
}
