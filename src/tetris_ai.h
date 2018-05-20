#define FEATIDX_RELIEF_MAX 0
#define FEATIDX_RELIEF_AVG 1
#define FEATIDX_RELIEF_VAR 2
#define FEATIDX_GAPS 3
#define FEATIDX_OBS 4
#define FEATIDX_DISCONT 5
// #define FEATIDX_GAPS_EXP 4
// #define FEATIDX_OBS_EXP 6
#define FEAT_COUNT 6

void ai_run ( int max_moves, int depth, int show_grid );
void ai_init();
double* default_weights;
game_move* ai_best_move ( grid* g, shape_stream* ss, double* w );

char* feat_names[FEAT_COUNT];
void evolution_test();

void ui_play();
void ui_play_ai(int depth, int delay_secs);
