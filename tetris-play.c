#include <ncurses.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include "tetris.h"
#include "tetris_ai.h"
#include "assert.h"

#define DELAY 300000


void human_play();
void ai_play(int depth, int delay_secs);

int main(int argc, char** argv)
{
  SHAPES = shapes_read("shapes.in", &SHAPE_COUNT);
  int seed = time(NULL);
  printf( "seed %d \n", seed );
  srand(seed);
  if (argc<2)	{
    printf("must provide subcommand");
    exit(1);
  }else 	{
    char* opt = argv[1];
    if (!strcmp(argv[1], "human"))	{
      human_play();
    }else if (!strcmp(opt, "ai"))	{
      ai_play(3, 1);
    }else 	{
      printf( "unknown option: %s\n", opt );
      return 1;
    }
  }
  return 0;
}

void human_play() {

  grid* g = grid_new(GRID_HEIGHT, GRID_WIDTH);
  block* b = block_new(NULL);

  ncurses_setup(g);
  ncurses_refresh();

  int dropped = 1, cleared = 0;
  int color;

  while(1) {
    if (dropped)	{
      color = RAND(SHAPE_COUNT);
      block_init(b, SHAPES[color]);
      grid_block_center_elevate(g, b);
      if (grid_block_intersects(g, b))	{
	mvprintw(g->height+1, 0, "game over!");
	break;
      }
      ncurses_block_print_shadow(b, 1, g);
      ncurses_refresh();
      dropped = 0;
    }else 	{
      int ch = getch();
      ncurses_block_print_shadow(b, 0, g);//delete block
      switch(ch){
      case KEY_LEFT: grid_block_move_safe(g, b, LEFT, 1); break;
      case KEY_RIGHT: grid_block_move_safe(g, b, RIGHT, 1); break;
      case KEY_DOWN: grid_block_move_safe(g, b, BOT, 1); break;
      case KEY_UP: grid_block_rotate_safe(g, b, 1); break;
      case ' ': dropped = 1; break;
      default: {
	int i;
	for ( i = 0; i < g->width; i++ )	{
	  if (ch == COL_SHORTCUT_KEYS[i])	{
	    grid_block_move_safe_to(g, b, i);
	    dropped = 1;
	    break;
	  }
	}
      }
      }
      if (dropped)	{
	grid_block_drop(g, b);
	grid_block_add(g, b);
	cleared = grid_clear_lines(g);
	if (cleared)	{
	  // need to repaint the whole grid
	  ncurses_grid_print(g);
	}
	ncurses_grid_print_fill_count(g);
      }else 	{
	cleared = 0;
      }
      if (!cleared)	{
	ncurses_block_print_shadow(b, 1, g);//repaint in new location
      }

      ncurses_refresh();
    }
  }

  endwin();
}

void sleep_secs (unsigned int secs) {
  // https://stackoverflow.com/questions/3930363/implement-time-delay-in-c
    unsigned int retTime = time(0) + secs;   // Get finishing time.
    while (time(0) < retTime);               // Loop until it arrives.
}

void ai_play(int depth, int delay_secs) {

  grid* g = grid_new(GRID_HEIGHT, GRID_WIDTH);
  shape_stream* ss = shape_stream_new(depth);
  ai_init();
  ncurses_setup(g);
  double* w = default_weights;
  assert(w);
  // double* w = get_default_weights();
  block bb;
  block* b = &bb;
  while (1)	{
    // show next block
    b->shape = shape_stream_peek(ss, 0);
    b->rot = 0;
    grid_block_center_elevate(g, b);
    ncurses_block_print(b, 1, g->height);
    ncurses_refresh();

    // compute next move. wait secs
    game_move* gm = ai_best_move(g, ss, w);
    shape_stream_pop(ss);
    if (gm == NULL) break;
    sleep_secs(delay_secs);

    ncurses_block_print(b, 0, g->height);//erase
    int succ = grid_block_apply_move(g, b, gm, 1);
    (void)succ;
    assert(succ);

    // unfortunately grid_block_apply_move also clears lines
    if (g->last_cleared_count)	{
      ncurses_grid_print(g);// repaint the whole grid
    }else 	{
      ncurses_block_print(b, 1, g->height);//leave painted until next clear
    }
    ncurses_refresh();
  }
  printf( "game over\n" );
  getch();
  endwin();
}
