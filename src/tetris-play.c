#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include "tetris.h"
#include "tetris_ai.h"
#include <ncurses.h>
#include <unistd.h>
#include <signal.h>

#define DELAY 300000
#define SEC 100000

void ui_play();// human or ai
void ui_play_ai(int depth, int delay_secs);// ai only
void sig_handler(int sig);
void toggle_ai ( );

typedef enum {MVLEFT, MVRIGHT, MVDOWN, DROP,
	      DROP_TO_ARG,
	      ROT_TO_ARG,
	      SWITCH_PLAYER,
	      ROTCW, ROTCCW, NONE} ui_move;

ui_move human_get_move ( grid* g, block* b, shape_stream* ss, int* arg )	{
  // provide the human's move as a mutation of b
  // return whether there was a drop request (i.e. ready for next block)
  // don't change the grid

  (void)ss;//could be used to display the future blocks
  (void)b;//human is seeing this on the screen, not needed
  int ch = getch();
  switch(ch){
  case KEY_LEFT: return MVLEFT;
  case KEY_RIGHT: return MVRIGHT;
  case KEY_DOWN: return MVDOWN;
  case KEY_UP: return ROTCW;
  case ' ': return DROP;
  case 112: return SWITCH_PLAYER;// letter 'p'
  default: {
    int i;
    for ( i = 0; i < g->width; i++ )	{
      if (ch == COL_SHORTCUT_KEYS[i])	{
	// b->offset[0] = i;
	// possible correction
	// b->offset[0] -= MAX(0, block_extreme(b, RIGHT) - (g->width-1));
	*arg = i;
	return DROP_TO_ARG;
      }
    }
    for ( i = 0; i < 4; i++ )	{
      if (ch == ROT_SHORTCUT_KEYS[i])	{
	*arg = i;
	return ROT_TO_ARG;
      }
    }
  }
  }
  return NONE;
}

game_move* gm;
ui_move ai_get_move ( grid* g, block* b, shape_stream* ss, int* arg)	{
  (void)arg;//ai will make moves one at a time
  if (gm == NULL)	{
    // new block. just display it
    gm = ai_best_move(g, ss, default_weights_cpy());
    return NONE;
  }else 	{
    // make moves one at a time. rotations first
    if (b->rot != gm->rot)	{
      int inc = (gm->rot-b->rot+4)%4;
      return inc<3? ROTCW: ROTCCW;
    }else if (b->offset[0] != gm->col)	{
      return gm->col > b->offset[0]? MVRIGHT: MVLEFT;
    }else 	{
      gm = NULL;
      return DROP;
    }
  }
}

int ai_playing = 1;

void ui_play() {
  signal(SIGINT, sig_handler);

  grid* g = grid_new(GRID_HEIGHT, GRID_WIDTH);
  block* b = block_new(NULL);

  ncurses_setup(g);
  ncurses_refresh();

  int dropped = 1;

  // allow ai to make moves for human
  shape_stream* ss = shape_stream_new(3);

  while(1) {
    if (dropped)	{
      // generate a new block
      shape_stream_pop(ss);
      block_init(b, shape_stream_peek(ss, 0));
      grid_block_center_elevate(g, b);
      if (grid_block_intersects(g, b))	{
	break;// cannot place new block. game over
      }
      ncurses_block_print_shadow(b, 1+ai_playing, g);
      if (ai_playing)	{
	usleep(.5*SEC);//brief pause to simulate 'ai thinking'
      }

      dropped = 0;
    }else	{
      int arg = 0; //optional 'prefix arg'
      ui_move move = ai_playing? ai_get_move(g, b, ss, &arg) :
	human_get_move(g, b, ss, &arg);

      if (ai_playing)	usleep(.7*SEC); // ai 'thinking'

      ncurses_block_print_shadow(b, 0, g);//unpaint old block

      switch(move){
      case MVLEFT:
      case MVRIGHT:
	grid_block_move_safe(g, b, move==MVLEFT? LEFT: RIGHT, 1); break;

      case MVDOWN: grid_block_move_safe(g, b, BOT, 1); break;

      case DROP_TO_ARG:  grid_block_move_safe_to(g, b, arg);
      case DROP: dropped = 1; break;

      case ROTCW:
      case ROTCCW:
	grid_block_rotate_safe(g, b, 1+2*(move == ROTCCW)); break;

      case ROT_TO_ARG:
	grid_block_rotate_safe(g, b, arg); break;

      case SWITCH_PLAYER: toggle_ai(); break;

      case NONE: break;
      }

      int cleared = 0;
      if (dropped)	{
	grid_block_drop(g, b);
	grid_block_add(g, b);
	cleared = grid_clear_lines(g);
      }
      if (cleared)	{
	// need to repaint the whole grid
	ncurses_grid_print(g);
      }else 	{
	//repaint in new location
	ncurses_block_print_shadow(b, 1+ai_playing*(!dropped), g);
      }
    }
    ncurses_grid_print_fill_count(g);
    ncurses_refresh();
  }

  mvprintw(g->height+1, 0, "game over!");
  endwin();
}

void ui_play_ai(int depth, int delay_secs) {

  grid* g = grid_new(GRID_HEIGHT, GRID_WIDTH);
  shape_stream* ss = shape_stream_new(depth);
  ncurses_setup(g);
  double* w = default_weights_cpy();
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
    usleep(delay_secs*SEC);

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

void toggle_ai (  )	{
  ai_playing = !ai_playing;
  gm = NULL;
}
int last_sig_secs = 0;
void  sig_handler(int sig)
{
  (void)sig;
  int now_secs = time(0);
  if (now_secs-last_sig_secs<=1)	{
    endwin();
    exit(0);
  }else 	{
    last_sig_secs = now_secs;
    toggle_ai();
  }
}
