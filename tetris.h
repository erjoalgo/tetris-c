typedef struct {
  int rot_count;
  int rot_wh[4][2];
  int** crust[4][4];
  int crust_len[4][4];
  int len;
  int max_dim_len;
  int** rot[4];
} shape;

typedef struct {
  int offset[2];
  int rot;
  shape* shape;
} block;

typedef enum {BOT, LEFT, TOP, RIGHT} direction;

typedef struct {
  int** rows;
  int* relief;
  int* row_fill_count;
  int* full_rows;
  int full_rows_count;
  int height;
  int width;

  int virtual_blocks_c;
  int total_cleared_count;
  int last_cleared_count;
  block** virtual_blocks;
} grid;

typedef int coord[2];//TODO do away with this?

typedef struct {
  shape* shape;
  int rot;//abs rot
  int col;
} game_move;


grid* grid_new(int height, int width);
void grid_set_color ( grid* g, int r, int c, int color );
void grid_block_add(grid* g, block* b);
void grid_block_remove(grid* g, block* b);
void grid_block_center_top(grid* g, block* b);
int grid_block_valid(grid* g, block* b);
int grid_block_intersects(grid* g, block* b);
void grid_apply_moves( grid* g, game_move* stream, int stream_count );
void grid_block_drop(grid* g, block* b);
void grid_print(grid* g);


void shape_print(shape* s);
shape** shapes_read(char* file, int* shape_count);

//global shapes and count
shape** SHAPES;
int SHAPE_COUNT;

block* block_new(shape* s);
void block_init(block* b, shape* s);
void block_get(block* b, int i, coord* result);
void block_rotate(block* b, int amount);
void block_move ( block* b, direction d, int amount );
void block_crust_get ( block* b, direction d, int i, coord* result );
int block_extreme(block* b, direction d);

#define GRID_HEIGHT 19
#define GRID_WIDTH 10
