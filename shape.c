#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "tetris.h"

int cmp_coord (const void* a, const void* b  )	{
  int* A = *((int**)a);
  int* B = *((int**)b);
  if (A[1] != B[1])	{
    return -(B[1]-A[1]);
  }else	{
    return A[0]-B[0];
  }
}

int max_dim(int** coords, int count, int dim) {
  int mx = coords[0][dim];
  int i;
  for ( i = 1; i < count; i++ )	{
    int curr = coords[i][dim];
    mx = curr>mx?curr:mx;
  }
  return mx;
}

// int min_dim(int count; int coords[count][2], int count, int dim) {
int min_dim(int** coords, int count, int dim) {
  int mn = coords[0][dim];
  int i;
  for ( i = 1; i < count; i++ )	{
    int curr = coords[i][dim];
    mn = curr<mn?curr:mn;
  }
  return mn;
}

int max_ab ( int a, int b )	{
  return a>b?a:b;
}

shape* shape_new ( int** shape_rot, int shape_len, int shape_id )	{
  // shape_rot is one rotation of the shape
  shape* s = malloc(sizeof(shape));
  s->len = shape_len;
  s->id = shape_id;

  // normalize to (0, 0)
  int extreme_left = min_dim(shape_rot, shape_len, 0);
  int extreme_bot = min_dim(shape_rot, shape_len, 1);

  // define all rotations
  s->rot[0] = malloc(shape_len * sizeof(*s->rot[0]));
  int i;
  // first rotation: normalize to (0, 0)
  for ( i = 0; i < shape_len; i++ )	{
    s->rot[0][i] = malloc(2*sizeof(*s->rot[0][i]));
    s->rot[0][i][0] = shape_rot[i][0] - extreme_left;
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
    s->rot_wh[roti][0] = max_dim(s->rot[roti], shape_len, 0) + 1;
    s->rot_wh[roti][1] = max_dim(s->rot[roti], shape_len, 1) + 1;
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
      int crust_len = 0;
      for ( i = 0; i < shape_len; i++ )	{
	int key = s->rot[roti][i][(dim+1)%2];
	int val = s->rot[roti][i][dim];
	int curr = extremes[key][0];
	int replace = curr == -1 ||
	  (keep_max && val>curr) ||
	  (!keep_max && val<curr);
	if (curr == -1)	{
	  crust_len++;
	}
	if (replace)	{
	  extremes[key][0] = val;
	  extremes[key][1] = i;
	}
      }
      s->crust_len[roti][d] = crust_len;
      s->crust[roti][d] = malloc(crust_len*sizeof(*s->crust[roti]));
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
      qsort(s->crust[roti][d], crust_len, sizeof(int)*2, cmp_coord);
    }
  }
  {
    if (s->len>MAX_BLOCK_LEN)	{
      printf( "shape len %d is greater than than MAX_BLOCK_LEN of %d\n",
	      s->len, MAX_BLOCK_LEN );
      return NULL;
    }
    int r, i, dim, dir;
    // initialize the flat, more efficient versions
    for ( r = 0; r < s->rot_count; r++ )	{
      for ( dim = 0; dim < 2; dim++ )	{
	for ( i = 0; i < s->len; i++ )	{
	  s->rot_flat[r][i][dim] = s->rot[r][i][dim];
	}
	for ( dir = 0; dir < 4; dir++ )	{
	  for ( i = 0; i < s->crust_len[r][dir]; i++ )	{
	    s->crust_flat[dir][r][i][dim] = s->crust[dir][r][i][dim];
	  }
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
  printf("%s\n", grid);
  for ( i = 0; i < coords_len; i++ )	{
    int c = coords[i][0];
    int r = rows - 1 - coords[i][1];
    grid[r*cols+c] = ' ';
  }
}

void shape_print ( shape* s, int quiet )	{
  char grid[s->max_dim_len*(1+s->max_dim_len) + 1];
  int i;
  for ( i = 0; i < s->max_dim_len*(1+s->max_dim_len); i++ )	{
    grid[i] = (i+1)%(s->max_dim_len+1) ? ' ' : '\n';
  }
  grid[s->max_dim_len*(1+s->max_dim_len)] = '\0';

  if (quiet)	{
    print_coords(grid, s->max_dim_len,s->max_dim_len+1,
		 s->rot[0], s->len);
    return ;
  }

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
		   s->crust[roti][d], s->crust_len[roti][d]);
    }
  }
}

shape** shapes_read ( char* file, int* shape_count)	{
  FILE* fh = fopen(file, "r");
  if (!fh) return NULL;
  *shape_count = 0;
  shape** shapes = malloc(1*sizeof(shape*));
  int id = 0;
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
    shapes[(*shape_count)++] = shape_new(rot, len, id++);
  }
  return shapes;
}

int shapes_init ( char* shapes_file )    {
  return !!(SHAPES = shapes_read(shapes_file, &SHAPE_COUNT));
}

void shape_test (  )	{
  int i;
  for ( i = 0; i < SHAPE_COUNT; i++ )	{
    printf( "shape %d/%d\n", i+1, SHAPE_COUNT );
    shape_print(SHAPES[i], 0);
  }
}

void shape_new_test (  )	{
  int shape_rot_spec[] = {1, 0 ,0, 1 ,1, 1 ,0, 2};
  int len = sizeof(shape_rot_spec)/sizeof(*shape_rot_spec)/2;
  int* shape_rot[len];
  int i;
  for ( i = 0; i < len; i++ )    {
    shape_rot[i] = malloc(2*sizeof(*shape_rot[i]));
    shape_rot[i][0] = shape_rot_spec[i*2];
    shape_rot[i][1] = shape_rot_spec[i*2+1];
  }
  shape* s = shape_new(shape_rot, len, 0);
  shape_print(s, 0);
  printf( "%s\n", shape_serialize(s) );
}

char* shape_serialize ( shape* s )    {
  char* buf = malloc(10000);
  char* CRUST_NAMES[4];
  CRUST_NAMES[TOP] = "top";
  CRUST_NAMES[RIGHT] = "right";
  CRUST_NAMES[LEFT] = "left";
  CRUST_NAMES[BOT] = "bot";

  int i = 0;
  int r, b, d;
  i = sprintf(buf, "{\n\t\"id\": %d,\n\t\"length\": %d,\n\t\"rot-count\": %d",
              s->id, s->len, s->rot_count);

  i+=sprintf(buf+i, ",\n\t\"rotation_configurations\": [");
  for ( r = 0; r < s->rot_count; r++ )    {
    i+=sprintf(buf+i, "%s\n\t\t[", r? ",": "");
    for ( b = 0; b < s->len; b++ )    {
      i+=sprintf(buf+i, "%s[%d, %d]", b? ", ": "", s->rot_flat[r][b][0], s->rot_flat[r][b][1]);
    }
    i+=sprintf(buf+i, "]");
  }
  i+=sprintf(buf+i, "\n\t]");

  i+=sprintf(buf+i, ",\n\t\"crust_configurations\": {");
  for ( d = 0; d < 4; d++ )    {
    char* crust_name = CRUST_NAMES[d];
    i+=sprintf(buf+i, "%s\n\t\t\"%s\": [", d? ",": "", crust_name);
    for ( r = 0; r < s->rot_count; r++ )    {
      i+=sprintf(buf+i, "%s\n\t\t\t[", r? ",": "");
      for ( b = 0; b < s->crust_len[r][d]; b++ )    {
        i+=sprintf(buf+i, "%s[%d, %d]", b? ", ":"",
                   s->crust[r][d][b][0], s->crust[r][d][b][1]);
      }
      i+=sprintf(buf+i, "]");
    }
    i+=sprintf(buf+i, "\n\t\t]");
  }
  i+=sprintf(buf+i, "\n\t}");
  i+=sprintf(buf+i, "\n}");
  return buf;
}

