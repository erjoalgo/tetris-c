#define FEAT_COUNT 6

double* default_weights_cpy();
void ai_run ( int max_moves, int depth, int show_grid );
void ai_init();
game_move* ai_best_move ( grid* g, shape_stream* ss, double* w );

char* feat_names[FEAT_COUNT];
void evolution_test();

void ui_play();
void ui_play_ai(int depth, int delay_secs);
