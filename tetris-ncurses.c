#include <ncurses.h>
#include "tetris.h"

void ncurses_grid_print ( grid* g )	{
  int row, col;
  // char row_s[g->width+1];
  // row_s[g->width] = '|';
  for ( row = g->height-1; row >= 0; row-- )	{
    // TODO how to print entire row of memory at once
    // TODO include virtual blocks
    for ( col = 0; col < g->width; col++ )	{
      // row_s[col] = g->rows[row][col]? '█' : ' ';
      // row_s[col] = g->rows[row][col]? '*' : ' ';
      // mvprintw(g->height-1-row, col, g->rows[row][col]? "*" : " ");
      mvaddch(g->height-1-row, col, g->rows[row][col]? '*' : ' ');
    }
    mvaddch(g->height-1-row, g->width, '|');
  }
  
  // row_s[g->width] = ' ';
  // mvprintw(g->height, col, row_s);
}

void ncurses_grid_print_fill_count ( grid* g )	{
  int row;
  for ( row = 0; row < g->height; row++ )	{
    mvaddch(g->height-1-row, g->width+1, '0'+g->row_fill_count[row]);
  }
  int col;
  for ( col = 0; col < g->width; col++ )	{
    int tall = g->relief[col];
    mvaddch(g->height+1, col,
	    tall == -1? '-' :
	    tall<10?'0'+tall:
	    'A'+tall%10);
  }
}


void ncurses_block_print ( block* b, int color, int grid_height )	{
  // char* c = add? "█" : " ";
  char c = !!color? '*' : ' ';
  int i;
  // TODO rename shape.count to shape.len
  coord cr;
  for ( i = 0; i < b->shape->len; i++ )	{
    block_get(b, i, &cr);
    mvaddch(grid_height-1-cr[1], cr[0], c);
  }
}
