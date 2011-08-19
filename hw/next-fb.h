typedef struct {
	DisplayState *ds;
	uint32_t base;
	uint32_t pitch;
	uint32_t cols;
	uint32_t rows;
	int invalidate;


} next_state_t;

void nextfb_draw_line(void *opaque, uint8_t *d, const uint8_t *s, int width, int pitch);

void nextfb_init(next_state_t *s);
