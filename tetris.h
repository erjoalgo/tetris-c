typedef enum {BOT, LEFT, TOP, RIGHT} direction;

typedef int coord[2];//TODO do away with this?

typedef struct {
  int rot_count;
  int rot_wh[4][2];
  int** crust[4][4];
  int crust_len[4][4];
  int len;
  int max_dim_len;
  int** rot[4];
} shape;
void shape_print(shape* s, int quiet);
shape** shapes_read(char* file, int* shape_count);
void shape_test();
//global shapes and count
shape** SHAPES;
int SHAPE_COUNT;
#define SHAPE_O SHAPES[0]
#define SHAPE_T SHAPES[1]
#define SHAPE_I SHAPES[2]
#define SHAPE_L SHAPES[3]
#define SHAPE_J SHAPES[4]
#define SHAPE_S SHAPES[5]
#define SHAPE_Z SHAPES[6]

typedef struct {
  int offset[2];
  int rot;
  shape* shape;
} block;
block* block_new(shape* s);
void block_init(block* b, shape* s);
void block_get(block* b, int i, coord* result);
void block_rotate(block* b, int amount);
void block_move ( block* b, direction d, int amount );
void block_crust_get ( block* b, direction d, int i, coord* result );
int block_extreme(block* b, direction d);
void block_print(block* b);


typedef struct {
  shape* shape;
  int rot;//abs rot
  int col;
} game_move;

void game_move_print(game_move* g);
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
grid* grid_new(int height, int width);
void grid_set_color ( grid* g, int r, int c, int color );
void grid_block_add(grid* g, block* b);
void grid_block_remove(grid* g, block* b);
int grid_block_center_top(grid* g, block* b);
int grid_block_valid(grid* g, block* b);
int grid_block_intersects(grid* g, block* b);
int grid_apply_moves( grid* g, game_move* stream, int stream_count );
void grid_block_drop(grid* g, block* b);
void grid_print(grid* g);
void grid_block_move_safe(grid* g, block* b, int direction, int amount);
void grid_block_rotate_safe(grid* g, block* b, int amount);
int grid_clear_lines(grid* g);
void grid_test();
#define GRID_HEIGHT 19
#define GRID_WIDTH 10




typedef struct{
  int max_len;
  int i;
  int* is_defined;
  shape** stream;
} shape_stream;
shape_stream* shape_stream_new ( int max_len );
shape* shape_stream_peek ( shape_stream* stream, int idx );
shape* shape_stream_pop ( shape_stream* stream );

#define RAND(len) (rand()%(len))

void ncurses_grid_print ( grid* g );
void ncurses_grid_print_fill_count ( grid* g );
void ncurses_block_print ( block* b, int color, int grid_height );

void shape_stream_test();
