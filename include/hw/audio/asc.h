/*
 *  Copyright (c) 2012-2018 Laurent Vivier <laurent@vivier.eu>
 *
 * This work is licensed under the terms of the GNU GPL, version 2 or later.
 * See the COPYING file in the top-level directory.
 *
 */

#ifndef HW_AUDIO_ASC_H
#define HW_AUDIO_ASC_H

#include "qemu/osdep.h"
#include "hw/sysbus.h"
#include "audio/audio.h"

enum {
    ASC_TYPE_ASC    = 0,  /* original discrete Apple Sound Chip */
    ASC_TYPE_EASC   = 1,  /* discrete Enhanced Apple Sound Chip */
    ASC_TYPE_V8     = 2,  /* ASC included in the V8 ASIC (LC/LCII) */
    ASC_TYPE_EAGLE  = 3,  /* ASC included in the Eagle ASIC (Classic II) */
    ASC_TYPE_SPICE  = 4,  /* ASC included in the Spice ASIC (Color Classic) */
    ASC_TYPE_SONORA = 5,  /* ASC included in the Sonora ASIC (LCIII) */
    ASC_TYPE_VASP   = 6,  /* ASC included in the VASP ASIC  (IIvx/IIvi) */
    ASC_TYPE_ARDBEG = 7   /* ASC included in the Ardbeg ASIC (LC520) */
};

typedef struct ASCState {
    SysBusDevice parent_obj;

    MemoryRegion mem_regs;
    QEMUSoundCard card;
    SWVoiceOut *channel;

    qemu_irq irq;

    uint8_t type;
    int a_wptr, a_rptr, a_cnt;
    int b_wptr, b_rptr, b_cnt;

    uint8_t *fifo;

    uint8_t regs[48];
} ASCState;

#define TYPE_ASC "apple-sound-chip"
#define ASC(obj) OBJECT_CHECK(ASCState, (obj), TYPE_ASC)

#endif
