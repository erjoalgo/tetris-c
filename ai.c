#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <string.h>
#include <assert.h>
#include "tetris.h"
#include "tetris_ai.h"

#define MAX_MUTATION 2.5
#define MOST_NEG_DBL (-DBL_MAX)

/*typedef struct {
  double (*raw_value)(grid* g, double* ordered_raws);
  double weight;
} feature;

typedef *feature heuristic;*/

void feature_gaps ( grid* g, double* ordered_raws );
void feature_variance ( grid* g, double* ordered_raws );

void init_feat_names (  )	{
  feat_names[FEATIDX_RELIEF_MAX] = "RELIEF_MAX";
  feat_names[FEATIDX_RELIEF_AVG] = "RELIEF_AVG";
  feat_names[FEATIDX_RELIEF_VAR] = "RELIEF_VAR";
  feat_names[FEATIDX_GAPS] = "GAPS     ";
  // feat_names[FEATIDX_GAPS_EXP] = "GAPS_EXP     ";
  feat_names[FEATIDX_OBS] = "OBS     ";
  // feat_names[FEATIDX_OBS_EXP] = "OBS_EXP     ";
  feat_names[FEATIDX_ROWS_FULL_CTR] = "ROWS_FULL_CTR";
}

double grid_eval ( grid* g, double* weights )	{
  double raws[FEAT_COUNT];
  feature_variance(g, raws);
  feature_gaps(g, raws);
  double val = 0;
  int i;
  for ( i = 0; i < FEAT_COUNT; i++ )	{
    val += raws[i]*weights[i];
  }
  /*
  grid_print(g);
  for ( i = 0; i < FEAT_COUNT; i++ )	{
    printf( "%s\t\t%.2f\t%.2f\t%.2f\n", feat_names[i], raws[i], weights[i],
	    raws[i]*weights[i] );
  }
  printf( "TOTAL   \t\t\t\t%.2f\n", val );
  getchar();
  */
  return val;
}

static grid** grids;
static block** blocks;
static int grids_count = 0;

game_move* ai_best_move_rec ( grid* g, shape_stream* stream, double* weights,
		       int depth_left, double* result_value );

game_move* ai_best_move_rec ( grid* g, shape_stream* stream, double* w,
		      int depth_left, double* value )	{
  double best_score = MOST_NEG_DBL;
  game_move* best_move = malloc(sizeof(game_move));

  int depth = stream->max_len-depth_left-1;
  shape* s = shape_stream_peek(stream, depth);
  best_move->shape = s;
  block* b = blocks[depth_left];
  b->shape = s;
  // in cases when we need to clear lines
  grid* g_prime = grids[depth_left];//depth_left: 0...ss->max_len-1
  grid* g_rec;
  int max_rots = b->shape->rot_count;
  int r;
  for ( r = 0; r < max_rots; r++ )	{
      int c;
      int max_cols = g->width - b->shape->rot_wh[r][0] +1;
      for ( c = 0; c < max_cols; c++ )	{
	if (!grid_block_center_top(g, b))	{
	  continue;
	}
	assert(grid_block_valid(g, b));
	b->offset[0] = c;
	b->rot = r;
	if (grid_block_intersects(g, b))	{
	  continue;
	}
	grid_block_drop(g, b);
	grid_block_add(g, b);
	double curr;
	if (g->full_rows_count)	{
	  g_rec = g_prime;
	  // memcpy(g_rec, g, sizeof(grid));
	  grid_cpy(g_rec, g);
	  assert(grid_equal(g_rec, g));
	  grid_clear_lines(g_rec);
	}else 	{
	  g_rec = g;
	}
	if (depth_left)	{
	  ai_best_move_rec(g_rec, stream, w, depth_left-1, &curr);
	}else 	{
	  curr = grid_eval(g_rec, w);
	}
	if (curr>best_score)	{
	  best_score = curr;
	  best_move->rot = r;
	  best_move->col = c;
	}
	grid_block_remove(g, b);
      }
  }
  *value = best_score;
  return best_move;
}

game_move* ai_best_move ( grid* g, shape_stream* ss, double* w )	{
  if (grids_count<ss->max_len)	{
    grids = realloc(grids, ss->max_len*sizeof(*grids));
    blocks = realloc(blocks, ss->max_len*sizeof(*blocks));
    int i;
    for ( i = grids_count; i < ss->max_len; i++ )	{
      grids[i] = grid_new(g->height, g->width);
      blocks[i] = block_new(NULL);
    }
    grids_count = ss->max_len;
  }
  double best_value;
  game_move* best_move = ai_best_move_rec(g, ss, w, ss->max_len-1, &best_value);

  if (best_value == MOST_NEG_DBL)	{
    return NULL;
  }else 	{
    return best_move;
  }
}

void feature_variance ( grid* g, double* ordered_raws )	{
  double avg = 0, var = 0, max = 0;
  int width = g->width;
  int i;

  for ( i = 0; i < width; i++ )	{
    int curr = g->relief[i];
    if (curr>max)	{
      max = curr;
    }
    avg+=curr+1;
  }
  var = sqrt(var);//stdev
  avg/=width;
  for ( i = 0; i < g->width; i++ )	{
    double diff = avg-g->relief[i];
    var += diff*diff;
  }
  ordered_raws[FEATIDX_RELIEF_MAX] = max;
  ordered_raws[FEATIDX_RELIEF_AVG] = avg;
  ordered_raws[FEATIDX_RELIEF_VAR] = var;
  // return var;
}

void feature_gaps ( grid* g, double* ordered_raws )	{
  int gaps = 0, obs = 0, obs_exp = 0, gaps_exp = 0;
  int c;
  for ( c = 0; c < g->width; c++ )	{
    int r;
    // this is not row-major
    for ( r = g->relief[c]; r >= 0; r-- )	{
      if (!g->rows[r][c])	{
	gaps++;
	// penalize higher gaps more heavily
	// this encourages to fix gaps near the top first
	gaps_exp += 1<<r;
      }else 	{
	obs++;
	obs_exp += 1<<r;
      }
    }
  }
  ordered_raws[FEATIDX_GAPS] = gaps;
  // ordered_raws[FEATIDX_GAPS_EXP] = gaps_exp;
  // ordered_raws[FEATIDX_GAPS_EXP] = 0;
  ordered_raws[FEATIDX_OBS] = obs;
  // ordered_raws[FEATIDX_OBS_EXP] = obs_exp;
  // ordered_raws[FEATIDX_OBS_EXP] = 0;
  ordered_raws[FEATIDX_ROWS_FULL_CTR] = g->full_rows_count;
  // ordered_raws[FEATIDX_CLEARED_COUNT] = ;
}

void test_feature (  )	{
  grid* g = grid_new(GRID_HEIGHT, GRID_WIDTH);
  grid_set_color(g, 2, 0, 1);
  double raws[FEAT_COUNT];
  feature_gaps(g, raws);
  assert(raws[FEATIDX_GAPS] == 2);
  assert(raws[FEATIDX_OBS] == 0);
  grid_set_color(g, 4, 0, 1);
  assert(raws[FEATIDX_GAPS] == 3);
  assert(raws[FEATIDX_OBS] == 1);
}

void ai_init (  )	{
  double w[FEAT_COUNT] = {-0.80,-0.80,-0.18,-1.00,-1.00,19.94};;
  default_weights = malloc(sizeof(w));
  memcpy(default_weights, w, sizeof(w));
  init_feat_names();
}

void ai_test (  )	{
  grid* g = grid_new(GRID_HEIGHT, GRID_WIDTH);
  shape_stream* ss = shape_stream_new(3);
  g = grid_new(GRID_HEIGHT, GRID_WIDTH);
  double w[FEAT_COUNT];
  memcpy(w, default_weights, sizeof(w));
  game_move moves[1];
  int applied = 0, succ;
  while (1)	{
    grid_print(g);
    game_move* gm = ai_best_move(g, ss, w);
    if (gm == NULL)	{
      printf( "ai can't place move\n" );
      break;
    }
    moves[0] = *gm;
    game_move_print(gm);
    succ = grid_apply_moves(g, moves, 1);
    assert(succ);
    applied++;
    shape_stream_pop(ss);
  }
  (void)succ;
  printf( "%d moves applied, %d lines cleared\n",
	  applied, g->total_cleared_count );
}
