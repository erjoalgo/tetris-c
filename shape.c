#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "tetris.h"

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
	  keep_max && val>curr ||
	  !keep_max && val<curr;
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
		   s->crust[roti][d], s->crust_len[roti][d]);
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
