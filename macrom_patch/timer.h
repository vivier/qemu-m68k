#include <inttypes.h>

void TimerReset(void);
void TimerInit(void);
void TimerExit(void);
int16_t InsTime(uint32_t tm, uint16_t trap);
int16_t RmvTime(uint32_t tm);
int16_t PrimeTime(uint32_t tm, int32_t time);
