#include <ncurses.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#include "tetris.h"

#define DELAY 300000

static char COL_SHORTCUT_KEYS[] = "1234qwersd";

int TETROMINO_COLORS[] = {COLOR_RED, COLOR_GREEN,
		COLOR_YELLOW, COLOR_BLUE,
		COLOR_MAGENTA, COLOR_CYAN};

int main() {

  srand(time(NULL));

  WINDOW* w = initscr();
  keypad(w, 1);
  noecho();
  curs_set(0);

  start_color();
  SHAPES = shapes_read("shapes.in", &SHAPE_COUNT);

  int bg = !(use_default_colors() == OK)? COLOR_WHITE : -1;
  int i;
  for ( i = 0; i < SHAPE_COUNT; i++ )	{
    init_pair(i+1, TETROMINO_COLORS[i], bg);
  }
  init_pair(SHAPE_COUNT+1, bg, bg);

  grid* g = grid_new(GRID_HEIGHT, GRID_WIDTH);
  block* b = block_new(NULL);
  ncurses_grid_print(g);//print empty grid
  // also print shortcut chars
  int col;
  for ( col = 0; col < g->width; col++ )	{
    mvaddch(g->height, col, COL_SHORTCUT_KEYS[col]);
  }
  int dropped = 1, cleared = 0;
  int color;

  while(1) {
    if (dropped)	{
      color = RAND(SHAPE_COUNT);
      block_init(b, SHAPES[color]);
      grid_block_center_top(g, b);
      if (grid_block_intersects(g, b))	{
	mvprintw(g->height+1, 0, "game over!");
	break;
      }
      attron(COLOR_PAIR(color+1));
      attrset(COLOR_PAIR(color+1)|A_STANDOUT);
      attroff(A_BOLD);
      ncurses_block_print(b, 1, g->height);
      refresh();
      dropped = 0;
    }else 	{
      int ch = wgetch(w);
      attron(COLOR_PAIR(-1));
      attron(A_BOLD);
      attrset(COLOR_PAIR(-1)|A_STANDOUT);
      ncurses_block_print(b, 0, g->height);//delete block
      switch(ch){
      case KEY_LEFT: grid_block_move_safe(g, b, LEFT, 1); break;
      case KEY_RIGHT: grid_block_move_safe(g, b, RIGHT, 1); break;
      case KEY_DOWN: grid_block_move_safe(g, b, BOT, 1); break;
      case KEY_UP: grid_block_rotate_safe(g, b, 1); break;
      case ' ': {
	dropped = 1;
	break;
      }
      default: {
	int i;
	for ( i = 0; i < g->width; i++ )	{
	  if (ch == COL_SHORTCUT_KEYS[i])	{
	    b->offset[0] = i;
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
      attron(COLOR_PAIR(color+1));
      attrset(COLOR_PAIR(color+1)|A_STANDOUT);
      if (!cleared)	{
	ncurses_block_print(b, 1, g->height);//repaint in new location
      }

      refresh();
    }
  }

  endwin();
  return 0;
}
