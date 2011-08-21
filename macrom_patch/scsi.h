#include <inttypes.h>

#include "cpu.h"

int16_t SCSIReset(void);
int16_t SCSIGet(void);
int16_t SCSISelect(int id);
int16_t SCSICmd(int cmd_length, target_phys_addr_t cmd);
int16_t SCSIRead(uint32_t tib);
int16_t SCSIWrite(uint32_t tib);
int16_t SCSIComplete(uint32_t timeout, uint32_t message, uint32_t stat);
uint16_t SCSIStat(void);
int16_t SCSIMgrBusy(void);
