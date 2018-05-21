#define FEAT_COUNT 6

double* default_weights_cpy();

void ai_run ( int max_moves, int depth, int show_grid, double* w );

game_move* ai_best_move ( grid* g, shape_stream* ss, double* w );

char* feat_names[FEAT_COUNT];
void evolution_test();

void ui_play(double* w );
void ui_play_ai(int depth, int delay_secs, double* w);
