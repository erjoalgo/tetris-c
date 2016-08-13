#include <stdlib.h>
#include <assert.h>
#include "tetris.h"

shape_stream* shape_stream_new ( int max_len )	{
  shape_stream* s = malloc(sizeof(*s));
  s->is_defined = malloc(s->max_len*sizeof(*s->is_defined));
  s->max_len = max_len;
  s->i = 0;
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
