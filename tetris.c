#include <stdio.h>
#include <stdlib.h>


struct grid {
  int[][] rows;
  int[] relief;
  int[] row_fill_count;
  int[] full_rows;
  int full_rows_count;
  int height;
  int width;

  block*[] virtual_blocs;
  int cleared_count;
};

struct block {
  int[][] block;
  int rotations;
}


grid_new ( int width, int height )	{
  grid g;
  g.grid;
}


typedef struct grid grid;

int grid_height_at_start_at ( grid* g, int x, int start_at )	{
  // return the largest y s.t. g.grid[x][y]==1,
  // or -1 if no such y exists
  for ( y = start_at; y >= 0; y-- )	{
    if (g.grid[x][y] != 0)	{
      break;
    }
  }
  return y;
}
int grid_height_at ( grid* g, int x ){
  return grid_height_at_start_at(g, x, g.height-1);
}


int grid_block_set_color ( grid* g, block* b, int color )	{
  // add block, updating relief, row_fill_count, needs_clear
  int i = 0;
  int delta = color == 0? -1 : 1;
  for ( int i = 0; i < b.count; i++ )	{
    int x = b.block[i][0];
    int y = b.block[i][1];
    g.grid[x][y] = color;
    if (color == 0)	{
      g.row_fill_count -= 1;
      if (g.relief[x] == y)	{
	g.relief[x] = grid_height_at_start_at(g, x, y-1);
      }
    }else 	{
      g.row_fill_count[y] += 1;
      if (g.row_fill_count[y] == g.width)	{
	g.full_rows[g.full_rows_count++] = y;
      }
      assert(g.relief[x] != y);
      if (g.relief[x]<y)	{
	g.relief[x] = y;
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

void clear_lines ( grid* g )	{
  if (g.full_rows_count == 0)	{
    return ;
  }
  assert(g.full_rows_count>0);
  int cleared_count = g.full_rows_count;
  //smallest last. small values means near bottom of the grid
  // that is, descending order.
  // why did I pick descending order?
  sort(g.full_rows);
  int y = g.full_rows[g.full_rows_count-1];
  int ymax = max (self.relief);
  assert(ymax<g.height);
  int nextNonFull = y+1;
  while (nextNonFull<=ymax)	{
    // swap y with the next non-full

    // find the next nonFull
    while (g.row_fill_count[nextNonFull] == g.width)	{
      assert(nextNonFull<g.height);
      nextNonFull++;
      // it should be (almost) impossible for the highest row to get full
      // however, it is still possible,eg if new shape exactly fits into top row
      // this could happen only intentionally
      // so for now ignore this rare edge case
      assert(nextNonFull<g.height);
    }
    if (nextNonFull>ymax)	{
      break;
    }
    // invariant: nextNonfull should be full
    assert(g.row_full_count(nextNonFull) == g.width);

    // if rows[y] is full
    if (g.row_full_count[y]==g.width) {
      // y should be the lowest row that is full
      assert(g.full_rows[g.full_row_count] == y);
      g.full_row_count--;
      cleared[cleared_count++] = g.rows[y];
    }
    // reuse the row, no need to allocate new memory
    // swap y and next-non-full
    g.rows[y] = g.rows[nextNonFull];
    // g.row_fill_count[y] must have already been used by some lower row
    // or it was a full row, and it is appened to cleared
    // cleared.length + ?  = y- ymin
    g.row_fill_count[y] = self.row_fill_count[nextNonFull];


    y++;
    nextNonFull ++;
  }
  // now there are left-over rows that were cleared
  // they need to be zeroed-out, and replaces into rows[y...ymax]
  while (cleared_count)	{
    row[y] = cleared[cleared_count];
    g.row_fill_count[y] = 0;
    for ( int i = 0; i < g.width; i++ )	{
      row[y][0] = 0;
    }
    y++;
    cleared_count--;
  }

  // now we need to update relief
  for ( int i = 0; i < g.width; i++ )	{
    g.relief[i] = grid_height_at_start_at(g, i, g.relief[i]);
  }
  // we should be done.
  // should assert consistency

}

void print_grid ( grid g* )	{
  fprintf( "\n" );
  for ( int row = g.height-1; row >= 0; row-- )	{
    // TODO how to print entire row of memory at once
    // TODO include virtual blocks
    for ( int col = 0; col < g.width; col++ )	{
      printc(g.rows[row][col]? '*' : ' ');
    }
  }
  fprintf( "\n" );
}

void check_consistency ( grid g* )	{
  for ( int i = 0; i < g.width; i++ )	{
    assert(g.relief[i] == grid_height_at(g));
  }
  for ( int y = 0; y < g.height; y++ )	{
    int count = 0;
    for ( int x = 0; x < ; x++ )	{
      count += g.rows[y][x]?1:0;
    }
    assert(g.row_fill_count[y] == count);
  }
}




/*



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

    def addBlock (self, block):
        #for [x,y] in block:
        def addBlockFunc(x,y):
            assert(assert2(self.grid[y][x]==0))
            self.grid[y][x] = 1 #or color-specific number
            if self.relief[x] < y:
                self.relief[x] = y
                #if y > self.ymax:
                #self.ymax = y
            self.rowFillC[y]+=1
            if self.rowFillC[y]==self.width:
                self.needClear.append (y) #add sorted?
            #if trackHeuristicData:

        block.applyFunc ( addBlockFunc )
        if self.repaint:
            self.repaintInfo.cellModified (True, block)
        if self.on_cells:
             # block.applyFunc(lambda x, y: self.on_cells.add(Block.__encode_xy(x, y)))
             block.applyFunc(lambda x, y: self.on_cells[y].add(x))
        assert(self.consistency ())

    @staticmethod
    def __encode_xy ( x, y ):
         return y*width+x


    def addVirtualBlock (self, block, add = True):
        if add:
            self.virtualBlocks.append (block)
        else:
            self.virtualBlocks.remove (block)



    def __eq__ (self, other):
        #all (imap (equals, self.grid, other.grid))
        return all (imap (lambda y: self.grid[y]==other.grid[y], xrange (self.height)))

    def intersects (self, block):
        assert(assert2(self.inBounds (block), self, block))
        if max (self.relief) < block.extreme (BOT):
            return False

        for (a,b) in block:
            if self.grid[b][a] != 0:
                block.index = 0#ugly
                return True
        return False

    def inBounds (self, block):
        return block.extreme (TOP) < self.height and block.extreme (RIGHT) < self.width and block.extreme (LEFT) >= 0 and block.extreme (BOT) >= 0


    def valid (self, block):
        return self.inBounds (block) and not self.intersects (block)
    def moveSafe (self, block, direction, amount=1):
        assert(self.consistency ())

        block.move (direction, amount)
        if self.valid (block):
            return True
        else:
            #breakpoint2 ()
            block.move (-direction,amount)
            return False

    def rotateSafe (self, block, amount=1):
        block.rotate (amount)
        if self.valid (block):
            return True
        else:
            block.rotate (-amount)
            return False
    def drop (self, block):
        #amount  = min (imap (lambda xy: xy[1] - self.relief[xy[0]], block.botCrust ())) - 1
        self.needClear.append (self.height)#local variable amount being used inside needClear
        def maxAmount (x,y):
            currAmount = y - self.relief[x]
            if currAmount < self.needClear[-1]:
                self.needClear[-1] = currAmount

        block.applyFuncOverCrust (BOT, maxAmount )

        amount = self.needClear.pop () -1
        if amount<0:
            #relief can not help us, as we are under the relief
            amount = 0
            crust = list ( block.botCrust ())
            while True:
                if any (imap (lambda xy: (xy[1] - (amount+1)) < 0 or self.grid[xy[1]-(amount+1)][xy[0]] != 0, crust)):
                    #next is occupied
                    break
                else:
                    amount+=1
        #if amount !=0 and self.repaint:
            #self.repaintInfo.cellModified (True, block)
        block.move (BOT, amount)
        assert(self.consistency (True))
        return amount


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
