#include <string.h>

#include "cdrom.h"
#include "xpram.h"

uint8_t XPRAM[XPRAM_SIZE];

/*
 *  Initialize XPRAM
 */

void xpram_init(void)
{
    int16_t i16;

    /* Load XPRAM from settings file */

#if 0
    xpram_load(); /* FIXME */
#endif

    /* Load XPRAM default values if signature not found */

    if (XPRAM[0x0c] != 0x4e || XPRAM[0x0d] != 0x75
     || XPRAM[0x0e] != 0x4d || XPRAM[0x0f] != 0x63) {
        /* Clear XPRAM */
        memset(XPRAM, 0, XPRAM_SIZE);
        XPRAM[0x0c] = 0x4e;    /* "NuMc" signature */
        XPRAM[0x0d] = 0x75;
        XPRAM[0x0e] = 0x4d;
        XPRAM[0x0f] = 0x63;
        /* InternalWaitFlags = DynWait
         * (don't wait for SCSI devices upon bootup)
         */
        XPRAM[0x01] = 0x80;
        XPRAM[0x10] = 0xa8;    /* Standard PRAM values */
        XPRAM[0x11] = 0x00;
        XPRAM[0x12] = 0x00;
        XPRAM[0x13] = 0x22;
        XPRAM[0x14] = 0xcc;
        XPRAM[0x15] = 0x0a;
        XPRAM[0x16] = 0xcc;
        XPRAM[0x17] = 0x0a;
        XPRAM[0x1c] = 0x00;
        XPRAM[0x1d] = 0x02;
        XPRAM[0x1e] = 0x63;
        XPRAM[0x1f] = 0x00;
        XPRAM[0x08] = 0x13;
        XPRAM[0x09] = 0x88;
        XPRAM[0x0a] = 0x00;
        XPRAM[0x0b] = 0xcc;
        XPRAM[0x76] = 0x00;    /* OSDefault = MacOS */
        XPRAM[0x77] = 0x01;
    }

    /* Set boot volume */
    i16 = 1; /* PrefsFindInt32("bootdrive"); */
    XPRAM[0x78] = i16 >> 8;
    XPRAM[0x79] = i16 & 0xff;
    i16 = CDROMRefNum; /* PrefsFindInt32("bootdriver");*/
    XPRAM[0x7a] = i16 >> 8;
    XPRAM[0x7b] = i16 & 0xff;
}

/*
 *  Deinitialize XPRAM
 */

void xpram_exit(void)
{
    /* Save XPRAM to settings file */

#if 0
    save_xpram(); /* FIXME */
#endif
}
