#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "tetris.h"
#include "tetris_ai.h"

#define MAX_MUTATION 1
#define MOVE_AHEAD_COUNT 3
#define AI_BROTHER_COUNT 3
#define ROUND_COUNT 5


typedef struct {
  double w[FEAT_COUNT];//weights
  int mutation;
  double mutation_amt;
} ai;

void mutate_ai ( ai* ai, double* parent_weights )	{
  memcpy(ai->w, parent_weights, FEAT_COUNT*sizeof(*ai->w));
  int r = RAND(FEAT_COUNT);
  // http://stackoverflow.com/questions/13408990
  // amount is in (-MAX_MUTATION .. MAX_MUTATION)
  double amount = (float)rand()/(float)
    (RAND_MAX/2*MAX_MUTATION) - MAX_MUTATION;
  ai->w[r] += amount;
  ai->mutation = r;
  ai->mutation_amt = amount;
}

void mutate_weights_test (  )	{
  double* w = malloc(FEAT_COUNT*sizeof(*w));
  ai ai;
  memset(ai.w, 0, FEAT_COUNT*sizeof(*w));
  mutate_ai(&ai, w);
  int i;
  int mutated_count = 0;
  for ( i = 0; i < FEAT_COUNT; i++ )	{
    mutated_count += ai.w[i] != 0;
    assert(abs(ai.w[i])<=MAX_MUTATION);
  }
}

static grid* grids[AI_BROTHER_COUNT];
int best_ai ( ai* ais, int ai_c, int* moves_survived )	{
  grid** g = grids;
  int i;
  for ( i = 0; i < ai_c; i++ )	{
    grid_reset(g[i]);
  }
  shape_stream* ss = shape_stream_new(MOVE_AHEAD_COUNT);
  memset(moves_survived, 0, ai_c*sizeof(*moves_survived));

  int ai_live_count = ai_c;//short-cut when only one left
  int move_count = 0;
  game_move moves[1];
  int winner = -1;
  while (ai_live_count)	{
    int i;
    for ( i = 0; i < ai_c; i++ )	{
      if (moves_survived[i])	{
	continue;
      }
      game_move* gm = ai_best_move(g[i], ss, ais[i].w);
      if (gm == NULL)	{
	assert(move_count);
	ai_live_count--;
	moves_survived[i] = move_count;
	if (!ai_live_count)	{
	  winner = i;
	  break;
	}else 	{
	  continue;
	}
      }
      moves[0] = *gm;
      int succ = grid_apply_moves(g[i], moves, 1);
      assert(succ);
      (void)succ;
    }
    shape_stream_pop(ss);
    move_count++;
  }
  if (moves_survived[0] == moves_survived[winner])	{
    winner = 0;
  }

  printf( "survived: { " );
  for ( i = 0; i < ai_c; i++ )	{
    printf( "%d%s", moves_survived[i],
	    i != ai_c-1? ", " : " }");
  }
  assert(winner != -1);
  printf( "\tmax: %d\twinner:\t%d\n", moves_survived[winner], winner );
  return winner;
}

void ai_print ( ai* a )	{
  int i;
  printf( "{ " );
  for ( i = 0; i < FEAT_COUNT; i++ )	{
    printf( "%.2f%s", a->w[i], i != FEAT_COUNT-1?", ":" }\n" );
  }
}

void breed_ai ( ai* initial, int max_rounds )	{
  int ai_c = AI_BROTHER_COUNT;
  ai ais[ai_c];//todo rename
  ais[0] = *initial;
  int survived[ai_c];
  while (max_rounds<0 || max_rounds--)	{
    int i;
    for ( i = 1; i < ai_c; i++ )	{
	mutate_ai(ais + i, ais[0].w);
	// ai_print(ais[i]);
      }

    int consistent_winner = -1;
    for ( i = 0; i < ROUND_COUNT; i++ )	{
      int winner = best_ai(ais, ai_c, survived);
      if (winner != 0 && consistent_winner == -1)	{
	consistent_winner = winner;
      }else if (winner == 0 || winner != consistent_winner)	{
	consistent_winner = -1;
	break;
      }
    }
    if (consistent_winner != -1)	{
      if (0 == consistent_winner)	{
	assert(0);
	printf( "INCUMBENT IS CONSISTENT WINNER\n" );
      }else 	{
	int mutation = ais[consistent_winner].mutation;
	double mutation_amt = ais[consistent_winner].mutation_amt;
	ai_print(ais + 0);
	ai_print(ais + consistent_winner);

	printf( "%s %s BY %.2f\n", feat_names[mutation],
		mutation_amt>0? "UP" : "DOWN",
		mutation_amt * (mutation_amt>0? 1 : -1) );
	// getchar();
      }
      ais[0] = ais[consistent_winner];
    }
    printf( "\n" );
  }
}

void evolution_init (  )	{
  int i;
  for ( i = 0; i < AI_BROTHER_COUNT; i++ )	{
    grids[i] = grid_new(GRID_HEIGHT/2, GRID_WIDTH);
  }
}

void evolution_test (  )	{
  ai_init();
  evolution_init();
  mutate_weights_test();
  double w[FEAT_COUNT];
  ai initial;
  memcpy(initial.w, default_weights, sizeof(w));
  ai_print(&initial);
  breed_ai (&initial, -1);
}
