#include <inttypes.h>

#define XPRAM_SIZE 256

extern uint8_t XPRAM[XPRAM_SIZE];

void xpram_init(void);
void xpram_exit(void);
