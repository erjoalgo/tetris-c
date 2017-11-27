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
