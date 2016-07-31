#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// typedef int[2] coord;
typedef int coord[2];
// typedef struct coord { int x[2]; } coord;

typedef struct {
  int rot_count;
  int rot_wh[4][2];
  int** crust[4][4];
  int crust_count[4][4];
  int len;
  int max_dim_len;
  int** rot[4];
} shape;

typedef struct {
  int offset[2];
  int rot;
  shape* shape;
} block;

block* block_new ( shape* s )	{
  block* b = malloc(sizeof(block));
  b->rot = 0;
  b->offset[0] = 0;
  b->offset[1] = 0;
  b->shape = s;
  return b;
}

typedef enum {BOT, LEFT, TOP, RIGHT} direction;

void block_get ( block* b, int i, coord* result )	{
  int* rot = b->shape->rot[b->rot][i];
  // TODO make sure this is correct order
  (*result)[0] = rot[0] + b->offset[0];
  // "sizeof will be wrong"?
  // http://stackoverflow.com/questions/4523497/typedef-fixed-length-array
  (*result)[1] = rot[1] + b->offset[1];
}

void block_crust_get ( block* b, direction d, int i, coord* result )	{
  int* crust = b->shape->crust[b->rot][d][i];
  // TODO make sure this is correct order
  (*result)[0] = crust[0] + b->offset[0];
  *result[1] = crust[1] + b->offset[1];
}


typedef struct {
  int** rows;
  int* relief;
  int* row_fill_count;
  int* full_rows;
  int full_rows_count;
  int height;
  int width;

  int virtual_blocks_c;
  int cleared_count;
  block** virtual_blocks;
} grid;



// TODO consistent function naming, prefix with grid_ or block_
grid* grid_new ( int height, int width )	{
  grid* g = malloc(sizeof(grid));
  g->height = height;
  g->width = width;
  g->rows = malloc(height*sizeof(*g->rows));
  int r;
  for ( r = 0; r < height; r++ )	{
    g->rows[r] = malloc(width*sizeof(*g->rows));
    memset(g->rows[r], 0, g->width*sizeof(*g->rows[r]));
  }

  g->relief = malloc(width*sizeof(g->relief));
  int c;
  for ( c = 0; c < g->width; c++ )	{
    g->relief[c] = -1;
  }

  g->row_fill_count = malloc(height*sizeof(g->row_fill_count));
  memset(g->row_fill_count, 0, g->width*sizeof(*g->row_fill_count));

  g->full_rows = malloc(height*sizeof(g->full_rows));

  g->virtual_blocks_c = 0;
  g->cleared_count = 0;
  return g;
}

int max(int* heights, int count){
  int mx = heights[0];
  int i;
  for ( i = 1; i < count; i++ )	{
    int curr = heights[i];
    mx = curr>mx?curr:mx;
  }
  return mx;
}

int max_dim(int* coords[2], int count, int dim) {
  int mx = coords[0][dim];
  int i;
  for ( i = 1; i < count; i++ )	{
    int curr = coords[i][dim];
    mx = curr>mx?curr:mx;
  }
  return mx;
}
// int min_dim(int count; int coords[count][2], int count, int dim) {
int min_dim(int* coords[2], int count, int dim) {
  int mn = coords[0][dim];
  int i;
  for ( i = 1; i < count; i++ )	{
    int curr = coords[i][dim];
    mn = curr<mn?curr:mn;
  }
  return mn;
}

void fatal(char* msg){
  printf("FATAL: %s", msg);
  exit(1);
}
int grid_height_at_start_at ( grid* g, int x, int start_at )	{
  // return the largest y s.t. g.grid[x][y]==1,
  // or -1 if no such y exists
  int y;
  for ( y = start_at; y >= 0; y-- )	{
    if (g->rows[x][y] != 0)	{
      break;
    }
  }
  return y;
}
int grid_height_at ( grid* g, int x ){
  return grid_height_at_start_at(g, x, g->height-1);
}



int grid_block_set_color ( grid* g, block* b, int color )	{
  // add block, updating relief, row_fill_count, needs_clear
  int i = 0;
  int delta = color == 0? -1 : 1;
  coord c;
  for ( i = 0; i < b->shape->len; i++ )	{
    block_get(b, i, &c);
    int x = c[0];
    int y = c[1];
    g->rows[x][y] = color;
    if (color == 0)	{
      g->row_fill_count -= 1;
      if (g->relief[x] == y)	{
	g->relief[x] = grid_height_at_start_at(g, x, y-1);
      }
    }else 	{
      g->row_fill_count[y] += 1;
      if (g->row_fill_count[y] == g->width)	{
	g->full_rows[g->full_rows_count++] = y;
      }
      assert(g->relief[x] != y);
      if (g->relief[x]<y)	{
	g->relief[x] = y;
      }
    }
  }
}



int grid_block_add ( grid* g, block* b )	{
  grid_block_set_color(g, b, 1);
}

int grid_block_remove ( grid* g, block* b )	{
  grid_block_set_color(g, b, 0);
}



int cmp_rev (const void* a, const void* b  )	{
  // descending order
  int f = *((int*)a);
  int s = *((int*)b);
  if (f > s) return  -1;
  if (f < s) return 1;
  return 0;
}

void clear_lines ( grid* g )	{
  if (g->full_rows_count == 0)	{
    return ;
  }
  assert(g->full_rows_count>0);
  int cleared_count = g->full_rows_count;
  int* cleared[g->full_rows_count];
  //smallest last. small values means near bottom of the grid
  // that is, descending order.
  // why did I pick descending order?
  qsort(g->full_rows, g->full_rows_count, sizeof(int), cmp_rev);
  int y = g->full_rows[g->full_rows_count-1];
  int ymax = max (g->relief, g->width);
  assert(ymax<g->height);
  int nextNonFull = y+1;
  while (nextNonFull<=ymax)	{
    // swap y with the next non-full

    // find the next nonFull
    while (g->row_fill_count[nextNonFull] == g->width)	{
      assert(nextNonFull<g->height);
      nextNonFull++;
      // it should be (almost) impossible for the highest row to get full
      // however, it is still possible,eg if new shape exactly fits into top row
      // this could happen only intentionally
      // so for now ignore this rare edge case
      assert(nextNonFull<g->height);
    }
    if (nextNonFull>ymax)	{
      break;
    }
    // invariant: nextNonfull should be full
    assert(g->row_fill_count[nextNonFull] == g->width);

    // if rows[y] is full
    if (g->row_fill_count[y]==g->width) {
      // y should be the lowest row that is full
      assert(g->full_rows[g->full_rows_count] == y);
      g->full_rows_count--;
      cleared[cleared_count++] = g->rows[y];
    }
    // reuse the row, no need to allocate new memory
    // swap y and next-non-full
    g->rows[y] = g->rows[nextNonFull];
    // g->row_fill_count[y] must have already been used by some lower row
    // or it was a full row, and it is appened to cleared
    // cleared.length + ?  = y- ymin
    g->row_fill_count[y] = g->row_fill_count[nextNonFull];


    y++;
    nextNonFull ++;
  }
  // now there are left-over rows that were cleared
  // they need to be zeroed-out, and replaces into rows[y...ymax]
  while (cleared_count)	{
    g->rows[y] = cleared[cleared_count];
    g->row_fill_count[y] = 0;
    int i;
    for ( i = 0; i < g->width; i++ )	{
      g->rows[y][i] = 0;
    }
    y++;
    cleared_count--;
  }

  // now we need to update relief
  int i;
  for ( i = 0; i < g->width; i++ )	{
    g->relief[i] = grid_height_at_start_at(g, i, g->relief[i]);
  }
  // we should be done.
  // should assert consistency

}

void add_virtual_block(grid* g, block* b){
  g->virtual_blocks[0] = b;
}

void remove_virtual_block(grid* g){
  g->virtual_blocks[0] = NULL;
}

int eql(grid* a, grid* b){
  if (a->width != b->width || a->height != b->height)	{
    return 0;
  }
  int r,c;
  for ( r = 0; r < a->height; r++ )	{
    for ( c = 0; c < a->height; c++ )	{
      if (a->rows[r][c] != b->rows[r][c])	{
	return 0;
      }
    }
  }
  // TODO virtualblocks
  return 1;
}



int extreme ( block* b, direction d )	{
  switch(d){
  case LEFT:
    assert(min_dim(b->shape->rot[b->rot], b->shape->len, 0) == 0);
    return b->offset[0];
  case BOT:
    assert(min_dim(b->shape->rot[b->rot], b->shape->len, 1) == 0);
    return b->offset[1];
  case RIGHT:
    return b->shape->rot_wh[b->rot][0] + b->offset[0];
  case TOP:
    return b->shape->rot_wh[b->rot][1] + b->offset[1];
  }
}

void move ( block* b, direction d, int amount )	{
  int dim = (d == BOT || d == TOP)? 1 : 0;
  if (d == LEFT ||  d == BOT)	{
    amount*=-1;
  }
  b->offset[dim]+=amount;
}

void rotate ( block* b, int amount )	{
  int rot = b->shape->rot_count;
  b->rot = (b->rot+amount)%rot;
  if (b->rot<0)	{
    b->rot+=rot;
  }
}


int intersects ( grid* g, block* b )	{
  assert(in_bounds(g, b));
  if (max(g->relief, g->width)<extreme(b, BOT))	{
    return 0;
  }
  int i;
  coord rc;
  for ( i = 0; i < b->shape->len; i++ )	{
    block_get(b, i, &rc);
    int r = rc[0];
    int c = rc[1];
    if (g->rows[r][c])	{
      return 1;
    }
  }
  return 0;
}

int in_bounds ( grid* g, block* b )	{
  return extreme(b, LEFT)>=0 &&
    extreme(b, RIGHT)<g->width &&
    extreme(b, BOT)>=0 &&
    extreme(b, TOP)<g->height;
}

int block_valid ( grid* g, block* b )	{
  return in_bounds(g, b) && !intersects(g, b);
}

void move_safe ( grid* g, block* b, int direction, int amount )	{
  move(b, direction, amount);
  if (!block_valid(g, b))	{
    move(b, direction, -amount);
  }
}

void rotate_safe ( grid* g, block* b, int amount )	{
  rotate(b, amount);
  if (!block_valid(g, b))	{
    rotate(b, -amount);
  }
}

int drop_amount ( grid* g, block* b )	{
  int i;
  int min_amnt = g->height-1;
  coord rc;
  for ( i = 0; i < b->shape->crust_count[b->rot][BOT]; i++ )	{
    block_crust_get(b, BOT, i, &rc);
    int r = rc[0];
    int c = rc[1];
    int amnt = c-g->relief[r];
    if (amnt<min_amnt)	{
      min_amnt = amnt;
    }
  }
  if (min_amnt<0)	{
    // relief can not help us, as we are under the relief
    min_amnt = 0;
    assert(!intersects(g, b));
    int max_amnt = extreme(b, BOT);
    for ( min_amnt = 0; min_amnt<max_amnt; min_amnt++ )	{
      int next_amnt = min_amnt+1;
      for ( i = 0; i < b->shape->crust_count[b->rot][BOT]; i++ )	{
	block_crust_get(b, BOT, i, &rc);
	int r = rc[0];
	int c = rc[1];
	if (g->rows[r][c+next_amnt])	{
	  // break a;
	  goto a;
	}
      }
    }
  }
 a: return min_amnt;
}


void drop ( grid* g, block* b )	{
  int amount = drop_amount(g, b);
  move(b, BOT, amount);
  assert(block_valid(g, b));
}

void block_center_top (grid* g, block* b){
  // assert(extreme(b, BOT) == 0); this makes no sense here
  int rot = b->rot;
  b->offset[1] = g->height - b->shape->rot_wh[rot][1];
  b->offset[0] = (g->width - b->shape->rot_wh[rot][0])/2;
  assert(in_bounds(g, b));
  assert(b->shape->max_dim_len<g->width);
}



typedef struct {
  shape* shape;
  int rot;//abs rot
  int col;
} game_move;

void print_grid ( grid* g )	{
  printf( "\n" );
  int row, col;
  for ( row = g->height-1; row >= 0; row-- )	{
    // TODO how to print entire row of memory at once
    // TODO include virtual blocks
    for ( col = 0; col < g->width; col++ )	{
      printf("%c", g->rows[row][col]? '*' : ' ');
    }
    printf( "\n" );
  }
  printf( "\n" );
}

void print_block ( block* b )	{
  int i;
  // TODO rename shape.count to shape.len
  coord rc;
  for ( i = 0; i < b->shape->len; i++ )	{
    block_get(b, i, &rc);
    printf( "[%d %d]", rc[1], rc[0] );
  }
}


void grid_apply_moves ( grid* g, game_move* stream, int stream_count )	{
  int i;
  for ( i = 0; i < stream_count; i++ )	{
    game_move move = stream[i];
    block* b = block_new(move.shape);
    b->offset[0] = move.col;
    b->offset[1] = g->height - b->shape->rot_wh[b->rot][1];
    assert(block_valid(g, b));
    b->rot = move.rot;
    drop(g, b);
    grid_block_add(g, b);
    clear_lines(g);
  }
}

void check_consistency ( grid* g )	{
  int i;
  for ( i = 0; i < g->width; i++ )	{
    assert(g->relief[i] == grid_height_at(g, i));
  }
  int r;
  for ( r = 0; r < g->height; r++ )	{
    int count = 0;
    int c;
    for ( c = 0; c < g->width; c++ )	{
      count += g->rows[r][c]?1:0;
    }
    assert(g->row_fill_count[r] == count);
  }
}


int cmp_coord (const void* a, const void* b  )	{
  int* A = *((int**)a);
  int* B = *((int**)b);
  int suma = A[0]+A[1];
  int sumb = B[0]+B[1];
  if ( suma != sumb)	{
    return sumb-suma;
  }else	{
    return B[0]-A[0];
  }
}
int max_ab ( int a, int b )	{
  return a>b?a:b;
}

shape* shape_new ( int** shape_rot, int shape_len )	{
  // shape_rot is one rotation of the shape
  shape* s = malloc(sizeof(shape));
  s->len = shape_len;

  // normalize to (0, 0)
  int extreme_left = min_dim(shape_rot, shape_len, 0);
  int extreme_bot = min_dim(shape_rot, shape_len, 1);

  // define all rotations
  s->rot[0] = malloc(shape_len * sizeof(*s->rot[0]));
  int i;
  // first rotation: normalize to (0, 0)
  for ( i = 0; i < shape_len; i++ )	{
    s->rot[0][i] = malloc(2*sizeof(*s->rot[0][i]));
    s->rot[0][i][0] = shape_rot[i][0] - extreme_left;;
    s->rot[0][i][1] = shape_rot[i][1] - extreme_bot;
  }
  s->max_dim_len = max_ab(max_dim(s->rot[0], shape_len, 0),
			  max_dim(s->rot[0], shape_len, 1)) + 1;
  // define 1-4 rotations
  int roti;
  for ( roti = 1; roti < 4; roti++ )	{
    s->rot[roti] = malloc(shape_len * sizeof(*s->rot[roti]));
    for ( i = 0; i < shape_len; i++ )	{
      s->rot[roti][i] = malloc(2*sizeof(*s->rot[roti][i]));
      s->rot[roti][i][0] = s->rot[roti-1][i][1];
      s->rot[roti][i][1] = s->max_dim_len - 1 - s->rot[roti-1][i][0];
    }
    // we need to normalize to detect uniqueness later
    extreme_left = min_dim(s->rot[roti], shape_len, 0);
    extreme_bot = min_dim(s->rot[roti], shape_len, 1);
    for ( i = 0; i < shape_len; i++ )	{
      s->rot[roti][i][0] -= extreme_left;
      s->rot[roti][i][1] -= extreme_bot;
    }
  }

  // initialize s->rot_wh
  for ( roti = 0; roti < 4; roti++ )	{
    s->rot_wh[roti][0] = max_dim(s->rot[roti], shape_len, 0);
    s->rot_wh[roti][1] = max_dim(s->rot[roti], shape_len, 1);
  }


  // determine number of unique rotations
  char rot_str[4][shape_len*2+1];
  s->rot_count = 0;
  for ( roti = 0; roti < 4; roti++ )	{
    rot_str[roti][shape_len*2] = '\0';
    qsort(s->rot[roti], shape_len, sizeof(int)*2, cmp_coord);
    for ( i = 0; i < shape_len; i++ )	{
      rot_str[roti][2*i] = '0' + s->rot[roti][i][0];
      rot_str[roti][2*i+1] = '0' + s->rot[roti][i][1];
    }
    for ( i = 0; i < roti; i++ )	{
      if (strcmp(rot_str[i], rot_str[roti]) == 0)	{
	goto a;
      }
    }
    s->rot_count++;
  }
 a: roti = 0;

  // define crusts
  for ( roti = 0; roti < 4; roti++ )	{
    int d;
    for ( d = 0; d < 4; d++ )	{

      int extremes[s->max_dim_len][2];//value, index
      int dim = (d == BOT || d == TOP)? 1 : 0;
      int keep_max = (d == TOP || d == RIGHT);
      for ( i = 0; i < s->max_dim_len; i++ )	{
	extremes[i][0] = -1;
      }
      int crust_count = 0;
      for ( i = 0; i < shape_len; i++ )	{
	int key = s->rot[roti][i][(dim+1)%2];
	int val = s->rot[roti][i][dim];
	int curr = extremes[key][0];
	int replace = curr == -1 ||
	  keep_max && val>curr ||
	  !keep_max && val<curr;
	if (curr == -1)	{
	  crust_count++;
	}
	if (replace)	{
	  extremes[key][0] = val;
	  extremes[key][1] = i;
	}
      }
      s->crust_count[roti][d] = crust_count;
      s->crust[roti][d] = malloc(crust_count*sizeof(*s->crust[roti]));
      int ii = 0;
      for ( i = 0; i < s->max_dim_len; i++ )	{
	if (extremes[i][0] != -1)	{
	  int index = extremes[i][1];
	  s->crust[roti][d][ii] = malloc(2*sizeof(*s->crust[roti][i]));
	  s->crust[roti][d][ii][0] = s->rot[roti][index][0];
	  s->crust[roti][d][ii][1] = s->rot[roti][index][1];
	  ii++;
	}
      }
    }
  }
  return s;
}

void print_coords ( char* grid, int rows, int  cols,
		    int* coords[2], int coords_len)	{
  int i;
  for ( i = 0; i < coords_len; i++ )	{
    int c = coords[i][0];
    int r = rows - 1 - coords[i][1];
    printf( "[%d %d] ", c, coords[i][1] );

    grid[r*cols+c] = '*';
  }
  printf("\n\n");
  printf(grid);
  for ( i = 0; i < coords_len; i++ )	{
    int c = coords[i][0];
    int r = rows - 1 - coords[i][1];
    grid[r*cols+c] = ' ';
  }
}


void print_shape ( shape* s )	{
  char grid[s->max_dim_len*(1+s->max_dim_len) + 1];
  int i;
  for ( i = 0; i < s->max_dim_len*(1+s->max_dim_len); i++ )	{
    grid[i] = (i+1)%(s->max_dim_len+1) ? ' ' : '\n';
  }
  grid[s->max_dim_len*(1+s->max_dim_len)] = '\0';

  int roti;
  for ( roti = 0; roti < s->rot_count; roti++ )	{
    printf( "\n\nrot %d\n", roti );
    print_coords(grid, s->max_dim_len,s->max_dim_len+1,
		 s->rot[roti], s->len);

    int d;
    for ( d = 0; d < 4; d++ )	{
      printf( "\nrot %d, crust ", roti);
      switch(d){
      case TOP: printf("TOP");break;
      case RIGHT: printf("RIGHT");break;
      case LEFT: printf("LEFT");break;
      case BOT: printf("BOT");break;
      }
      printf( "\n" );
      print_coords(grid, s->max_dim_len,s->max_dim_len+1,
		   s->crust[roti][d], s->crust_count[roti][d]);
    }
  }
}

shape** read_shapes ( char* file, int* shape_count)	{
  FILE* fh = fopen(file, "r");
  *shape_count = 0;
  shape** shapes = malloc(1*sizeof(shape*));
  while (!feof (fh))	{
    int len;
    fscanf(fh, "%d", &len);
    int i;
    // int rot[len][2];
    int** rot = malloc(len*sizeof(*rot));
    for ( i = 0; i < len; i++ )	{
      rot[i] = malloc(2*sizeof(*rot[i]));
      fscanf(fh, "%d", &rot[i][0]);
      fscanf(fh, "%d", &rot[i][1]);
    }
    shapes = realloc(shapes, (*shape_count+1)*sizeof(shape*));
    shapes[(*shape_count)++] = shape_new(rot, len);
  }
  return shapes;
}

int main(int argc, char* argv[])
{
  int x[] = {4,5,2,3,1,0,9,8,6,7};

  qsort (x, sizeof(x)/sizeof(*x), sizeof(*x), cmp_rev);
  int i;
  for (i = 0 ; i < 10 ; i++)
    printf ("%d ", x[i]);
  printf( "\n" );

  int shape_count;
  shape** shapes = read_shapes("shapes.in", &shape_count);
  for ( i = 0; i < shape_count; i++ )	{
    printf( "shape %d/%d\n", i+1, shape_count );
    print_shape(shapes[i]);
  }
  return 0;
}

/* class Grid (object):

    def __init__ (self, dim = None, repaint=False, trackHeuristicData = False, name="Unnamed grid", track_on_cells = False):
        dim = dim or (grid_height, 10)
        self.dim = dim
        self.grid = [[0]*dim[1] for _ in xrange(dim[0])]
        self.relief = [-1]*dim[1]
        self.virtualBlocks = []
        self.needClear = []
        self.height, self.width = dim
        self.rowFillC = [0]*self.height
        self.lastCleared = 0
        self.name = name
        self.totalCleared = 0
        self.repaint = repaint
        # self.on_cells = LLGrid(self.dim) if track_on_cells else None
        self.on_cells = [ LL(LL.NONE) for _ in xrange(self.height)] if track_on_cells else None

    def get (self, y, x):
        coords = [x,y]
        for block in self.virtualBlocks:
            for i in xrange (len (block)):
                if coords == block.ith (i):
                    return 1
        return self.grid[y][x]



*/
