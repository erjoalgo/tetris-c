#include <stdio.h>
#include <stdlib.h>



// typedef int[2] coord;
typedef int coord[2];
// typedef struct coord { int x[2]; } coord;

typedef struct {
  int** rots[2];
  int rots_count;
  int** crust[2];
  int* crust_count;
  int count;
} shape;

typedef struct {
  int offset[2];
  int rot;
  shape* shape;
} block;

typedef enum {BOT, LEFT, TOP, RIGHT} direction;

void block_get ( block* b, int i, coord* result )	{
  int* rot = b->shape->rots[b->rot][i];
  // TODO make sure this is correct order
  (*result)[0] = rot[0] + b->offset[0];
  // "sizeof will be wrong"?
  // http://stackoverflow.com/questions/4523497/typedef-fixed-length-array
  (*result)[1] = rot[1] + b->offset[1];
}

void block_crust_get ( block* b, direction d, int i, coord* result )	{
  int* crust = b->shape->crust[d][i];
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




grid* grid_new ( int width, int height )	{
  grid* g = malloc(sizeof(grid));
  g->rows = malloc(sizeof(int)*height);
  g->relief = malloc(sizeof(int)*width);
  g->row_fill_count = malloc(sizeof(int)*width);
  g->full_rows = malloc(sizeof(int)*width);
  int r;
  for ( r = 0; r < height; r++ )	{
    g->rows[r] = malloc(sizeof(int)*width);
  }
  return g;
}

int max(int* heights, int count){
  int mx = heights[0];
  int i;
  for ( i = 0; i < count; i++ )	{
    int curr = heights[i];
    mx = curr>mx?curr:mx;
  }
  return mx;
}
void fatal(char* msg){
  printf("FATAL: %s", msg);
  exit(1);
}
void assert(int val){
  if (!val)	{
    fatal("assertion error");
  }
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
  for ( i = 0; i < b->shape->count; i++ )	{
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
  coord c;
  int dim = (d == BOT || d == TOP)? 1 : 0;
  int i;
  block_crust_get(b, d, 0, &c);
  int mx = c[dim];
  for ( i = 1; i < b->shape->crust_count[d]; i++ )	{
    // int curr = b->crust[d][i][dim];
    block_crust_get(b, d, i, &c);
    int curr = c[dim];
    if (d == BOT || d == RIGHT)	{
      mx = curr > mx? curr: mx;
    }else 	{
      mx = curr < mx? curr: mx;
    }
  }
  return mx;
}

void move ( block* b, direction d, int amount )	{
  int dim = (d == BOT || d == TOP)? 1 : 0;
  if (d == LEFT ||  d == BOT)	{
    amount*=-1;
  }
  b->offset[dim]+=amount;
}

void rotate ( block* b, int amount )	{
  int rots = b->shape->rots_count;
  b->rot = (b->rot+amount)%rots;
  if (b->rot<0)	{
    b->rot+=rots;
  }
}


int intersects ( grid* g, block* b )	{
  assert(in_bounds(g, b));
  if (max(g->relief, g->width)<extreme(b, BOT))	{
    return 0;
  }
  int i;
  coord rc;
  for ( i = 0; i < b->shape->count; i++ )	{
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
  for ( i = 0; i < b->shape->crust_count[BOT]; i++ )	{
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
      for ( i = 0; i < b->shape->crust_count[BOT]; i++ )	{
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


typedef struct {
  int shape;
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

int main(int argc, char* argv[])
{
    int x[] = {4,5,2,3,1,0,9,8,6,7};

    qsort (x, sizeof(x)/sizeof(*x), sizeof(*x), cmp_rev);
    int i;
    for (i = 0 ; i < 10 ; i++)
        printf ("%d ", x[i]);

    printf( "bot %d", BOT );
    return 0;
}

/*
  class Block (object):

    def __init__ (self, model):
        self.offset = [0,0]
        self.rotation = 0
        self.model = model
        assert (assert2(isinstance (model, Shape), model.__class__))
        self.index = 0

    def __clone__ (self):
        return Block ()
        clone = Block (model)
        clone.offset = self.offset[:]
        clone.rotation = self.rotation
        clone.index = 0
        return clone
    
    def rotate (self, count):
        self.rotation = (self.rotation+count)%self.model.rotationC
        assert(assert2( self.model.rotations[self.rotation]))

    def next (self):
        #reusable iterator: can  only be iterated by one thread, atomically! use yield?
        if self.index < len( self.model.shape):
            ab = [self.model.rotations[self.rotation][self.index][0] + self.offset[0], self.model.rotations[self.rotation][self.index][1] + self.offset[1]]
            self.index+=1
            return tuple(ab)
            #return self.ith (self.index-1)
        else:
            self.index = 0
            raise StopIteration
        #check this. can not iterate concurrently!
        
    def ith (self, i):
        return map(operator.add, self.model.rotations[self.rotation][i], self.offset)

    def applyFunc (self, fun):
        index = 0
        rotation = self.model.rotations[self.rotation]
        xoff = self.offset[0]
        yoff = self.offset[1]
        while index < self.model.len:
            fun (rotation[index][0] + xoff, rotation[index][1] + yoff)
            index+=1

    def applyFuncOverCrust (self, direction, fun):
        if direction == BOT:
            crust = self.model.botCrust[self.rotation]
            length = self.model.botCrustLen[self.rotation]
        elif direction == TOP:
            crust = self.model.topCrust[self.rotation]
            length = self.model.topCrustLen[self.rotation]
        else:
            assert(assert2 (False, "Not implemented"))
        index =0
        while index < length:
            fun (crust[index][0] + self.offset[0], crust[index][1] + self.offset[1])
            index+=1
        
        """for index in xrange (length):
            fun (crust[index][0] + self.offset[0], crust[index][1] + self.offset[1])"""
            
    def __iter__ (self):
        return self
    def __len__ (self):
        return len (self.model.shape)


    def extremeOld (self, extreme):
        hori = extreme == LEFT or extreme == RIGHT
        fun = min if extreme == BOT or extreme == LEFT else max
        return fun(map (lambda(coords): coords[ 0 if hori else 1],self.model.rotations[self.rotation])) + self.offset[0 if hori else 1]

    def extreme (self, extreme):
        if extreme == LEFT:
            return self.offset[0]
        elif extreme == RIGHT:
            return self.offset[0]+self.model.rotationWH[self.rotation][0] - 1
        elif extreme == TOP:
            return self.offset[1] + self.model.rotationWH[self.rotation][1] - 1
        else:
            return self.offset[1]


    def __str__ (self):
        str = "[ "
        for i in xrange (len(self.model.shape)):
            #str += "[%d, %d] "%(a,b)
            str += "[%d, %d] "%tuple ( self.ith (i))
        str += "]"
        return str
    
    def move (self, direction, amount):
        self.offset[0 if direction == LEFT or direction == RIGHT else 1] += amount * (1 if direction == TOP or direction == RIGHT else -1)

    def botCrust (self):
        #return map(self.model.botCrust[self.rotation]
        return imap (lambda xy: map (operator.add, xy, self.offset),self.model.botCrust[self.rotation])
	*/





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

    def __getitem__ (self, index):
        return self.grid[index]

    def get (self, y, x):
        coords = [x,y]
        for block in self.virtualBlocks:
            for i in xrange (len (block)):
                if coords == block.ith (i):
                    return 1
        return self.grid[y][x]




    def centerTop (self, block):
        assert(assert2(block.model.squareC < self.width))
        assert(assert2(block.extreme (BOT)==0))
        block.offset[1] = self.height - block.model.rotationWH[block.rotation][1]
       #printt (block.offset)
        #block.move (TOP, self.height - block.extreme (TOP) - 1)
        block.offset[0] = (self.width - block.model.rotationWH[block.rotation][0])/2
        assert(assert2(self.inBounds (block), block, block.model.rotationWH[block.rotation]))

    def apply_move_stream ( self, stream ):
         count = 0
         for move in stream:
              count+=1
              (m, r, x) = move
              block = Block(Game.defaultShapeSet[m])
              block.rotation = r
              block.offset[0] = x
              block.offset[1] = self.height - block.model.rotationWH[block.rotation][1]
              drop_amount = self.drop(block)
              self.addBlock(block)
              self.checkClearing()
         # print "applied %d moves" % (count)

*/
