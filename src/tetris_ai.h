#ifndef TETRIS_AI_H
#define TETRIS_AI_H

#define FEAT_COUNT 6

double* default_weights_cpy();

void ai_run ( int max_moves, int depth, int show_grid, double* w );

double* load_weights(char* ai_weights_file);

game_move* ai_best_move ( grid* g, shape_stream* ss, double* w );

extern const char* FEAT_NAMES[FEAT_COUNT];
void evolution_test();

void ui_play(double* w );
void ui_play_ai(int depth, int delay_secs, double* w);

#endif // TETRIS_AI_H
