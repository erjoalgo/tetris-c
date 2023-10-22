#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include "tetris.h"
#include "tetris_ai.h"

#include <unistd.h>

#ifndef DATADIR
#define DATADIR "."
#endif

#define FATAL(fmt, ...) { fprintf(stderr, fmt, ##__VA_ARGS__); exit(1); }

int main(int argc, char** argv)
{
  char* shapes_file =  DATADIR "/shapes.in";
  char* weights_file =  DATADIR "/default_weights.tsv";

  int seed = time(NULL);

  int depth = 3;
  int max_moves = 5000;
  int show_grid = 0;

  int c;
  while ((c = getopt (argc, argv, "d:m:hvs:S:w:")) != -1)
    switch (c)
      {
      case 'd':
	depth = atoi(optarg);
        break;
      case 'm':
	max_moves = atoi(optarg);
        break;
      case 's':
	seed = atoi(optarg);
        break;
      case 'S':
	shapes_file = optarg;
        break;
      case 'w':
	weights_file = optarg;
        break;
      case 'v':
	show_grid = 1;
        break;
      case 'h':
	printf ("usage: tetris play|ai|evolve|test\n");
	exit(0);
        break;
      case '?':
	FATAL("unknown flag %c", optopt);
      default:
	assert(0);
      }

  if (optind>=argc)	{
    FATAL("must provide command");
  }
  char* cmd = argv[optind];

  if (!shapes_init(shapes_file))	{
    FATAL("unable to open %s", shapes_file);
  }
  double* w = NULL;
  if (!(w = load_weights(weights_file)))    {
    printf("WARN: unable to load weights from %s. falling back to defaults\n", weights_file);
    w = default_weights_cpy();
  }

  printf( "seed %d \n", seed );
  srand(seed);

  if (!strcmp(cmd, "play"))	{
    #if defined(HAVE_LIBNCURSES)
    ui_play(w);
    #else
    printf( "not compiled with ncurses support\n" );
    #endif
  }else if (!strcmp(cmd, "ai"))	{
    int start = time(NULL);
    ai_run(max_moves, depth, show_grid, w);
    printf( "%ld secs elapsed\n", (time(NULL)-start) );
  }else if (!strcmp(cmd, "evolve"))	{
    evolution_test();
  }else if (!strcmp(cmd, "test"))	{
    shape_test();
    grid_test();
    shape_stream_test();
  }else 	{
    FATAL("unknown command: %s\n", cmd );
  }
  return 0;
}
