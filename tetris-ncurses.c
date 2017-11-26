#include <ncurses.h>
#include "tetris.h"

const int EDGE=1;

WINDOW* win;
inline void ncurses_paint ( int r, int c, int on )	{

  r+=EDGE;
  c+=EDGE;

  int pair = 1+on;
  wattron(win, COLOR_PAIR(pair));
  mvwaddch(win, r,  c, ' ');
}

void ncurses_grid_print ( grid* g )	{
  int row, col;
  // char row_s[g->width+1];
  // row_s[g->width] = '|';
  for ( row = g->height-1; row >= 0; row-- )	{
    for ( col = 0; col < g->width; col++ )	{
      // row_s[col] = g->rows[row][col]? '█' : ' ';
      // row_s[col] = g->rows[row][col]? '*' : ' ';
      // mvprintw(g->height-1-row, col, g->rows[row][col]? "*" : " ");
      ncurses_paint(g->height-1-row, col, g->rows[row][col]);
    }
  }
  // row_s[g->width] = ' ';
  // mvprintw(g->height, col, row_s);
}

void ncurses_grid_print_fill_count ( grid* g )	{
  int row;
  for ( row = 0; row < g->height; row++ )	{
    mvwaddch(win, g->height-1-row+EDGE, g->width+1+EDGE,
	     '0'+g->row_fill_count[row]);
  }
  int col;
  for ( col = 0; col < g->width; col++ )	{
    int tall = g->relief[col];
    mvwaddch(win, EDGE+g->height+1, col+EDGE,
	    tall == -1? '-' :
	    tall<10?'0'+tall:
	    'A'+tall%10);
  }
}


void ncurses_block_print ( block* b, int color, int grid_height )	{
  int i;
  coord cr;
  for ( i = 0; i < b->shape->len; i++ )	{
    block_get(b, i, &cr);
    ncurses_paint(grid_height-1-cr[1], cr[0], color);
  }
}

void ncurses_setup ( grid* g )	{
  int startrc[] = {3,13};// somewhat center the grid

  initscr();// init the standard screen
  // allow EDGE units on each side for border
  win = newwin(g->height+EDGE*2, g->width+EDGE*2, startrc[0], startrc[1]);

  keypad(stdscr, 1); // without this, keypad keycodes are off
  noecho();// don't echo keypresses like ^[[A
  curs_set(0);// don't echo a blinking cursor
  // cbreak(); not sure

  start_color();// needed before any color-related calls
  int BG = COLOR_GREEN;
  int FG = COLOR_WHITE;
  int UNUSED = COLOR_BLUE;
  init_pair(1, UNUSED, BG);
  init_pair(2, UNUSED, FG);
  init_pair(3, FG, COLOR_BLACK);
  assume_default_colors(FG, BG);

  wattron(win, COLOR_PAIR(3));//border color
  // box(win, 0 , 0); not sure what this does
  wborder(win, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ');

  // print shortcut chars
  int col;
  for ( col = 0; col < g->width; col++ )	{
    mvwaddch(win, g->height+EDGE, col+EDGE, COL_SHORTCUT_KEYS[col]);
  }

  ncurses_refresh();
}

void ncurses_refresh (  )	{
  wrefresh(stdscr);
  wrefresh(win);
}
