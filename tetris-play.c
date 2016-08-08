#include <ncurses.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#include "tetris.h"

#define DELAY 300000

static char COL_SHORTCUT_KEYS[] = "1234qwersd";

int main() {

  srand(time(NULL));

  WINDOW* w = initscr();
  keypad(w, 1);
  noecho();
  curs_set(0);

  SHAPES = shapes_read("shapes.in", &SHAPE_COUNT);

  grid* g = grid_new(GRID_HEIGHT, GRID_WIDTH);
  block* b = block_new(NULL);
  ncurses_grid_print(g);//print empty grid
  // also print shortcut chars
  int col;
  for ( col = 0; col < g->width; col++ )	{
    mvaddch(g->height, col, COL_SHORTCUT_KEYS[col]);
  }
  int new_block = 1;
  int drop, cleared = 0;
  int color;

  while(1) {
    if (new_block)	{
      color = RAND(SHAPE_COUNT);
      block_init(b, SHAPES[color]);
      grid_block_center_top(g, b);
      if (grid_block_intersects(g, b))	{
	mvprintw(g->height+1, 0, "game over!");
	break;
      }
      ncurses_block_print(b, 1, g->height);
      refresh();
      new_block = 0;
    }else 	{
      int ch = wgetch(w);
      ncurses_block_print(b, 0, g->height);//delete block
      switch(ch){
      case KEY_LEFT: grid_block_move_safe(g, b, LEFT, 1); break;
      case KEY_RIGHT: grid_block_move_safe(g, b, RIGHT, 1); break;
      case KEY_DOWN: grid_block_move_safe(g, b, BOT, 1); break;
      case KEY_UP: grid_block_rotate_safe(g, b, 1); break;
      case ' ': {
	drop = 1;
	break;
      }
      default: {
	int i;
	for ( i = 0; i < g->width; i++ )	{
	  if (ch == COL_SHORTCUT_KEYS[i])	{
	    b->offset[0] = i;
	    drop = 1;
	    break;
	  }
	}
      }
      }
      if (drop)	{
	grid_block_drop(g, b);
	grid_block_add(g, b);
	cleared = grid_clear_lines(g);
	if (cleared)	{
	  // need to repaint the whole grid
	  ncurses_grid_print(g);
	}
	new_block = 1;
	drop = 0;
	ncurses_grid_print_fill_count(g);
      }else 	{
	cleared = 0;
      }
      if (!cleared)	{
	ncurses_block_print(b, 1, g->height);//repaint in new location
      }

      refresh();
    }
  }

  endwin();
  return 0;
}
