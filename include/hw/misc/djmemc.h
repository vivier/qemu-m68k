/*
 * djMEMC, macintosh memory and interrupt controller
 * (Quadra 610/650/800 & Centris 610/650)
 */

#ifndef HW_MISC_DJMEMC_H
#define HW_MISC_DJMEMC_H

#include "hw/sysbus.h"
#include "cpu.h"

#define DjMEMCMaxBanks    10

typedef struct DjMEMCState {
    SysBusDevice parent_obj;

    MemoryRegion mem_regs;

    /* Memory controller */
    uint32_t interleave;
    uint32_t bank[DjMEMCMaxBanks];
    uint32_t top;
    uint32_t config;
    uint32_t refresh_rate;

    /* interrupt controller */
    M68kCPU *cpu;
    uint8_t ipr;
} DjMEMCState;

#define TYPE_DJMEMC "djMEMC"
#define DJMEMC(obj) OBJECT_CHECK(DjMEMCState, (obj), TYPE_DJMEMC)

#endif
