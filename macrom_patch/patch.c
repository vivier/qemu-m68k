#include <stdio.h>
#include <inttypes.h>

#include "macrom.h"

/* drivers */

#include "sony.h"
#include "disk.h"
#include "cdrom.h"
#include "slot.h"

/*
 *  Macros used to extract one of the 16-bit words from a 32-bit word value
 */

#define HiWord(X) (((X) >> 16) & 0xffff)
#define LoWord(X) ((X) & 0xffff)

// Construct four-character-code */

#define FOURCC(a,b,c,d) (((uint32)(a) << 24) | ((uint32)(b) << 16) | ((uint32)(c) << 8) | (uint32)(d))

/* 68k opcodes */

const uint16 M68K_ILLEGAL = 0x4afc;
const uint16 M68K_NOP = 0x4e71;
const uint16 M68K_RTS = 0x4e75;
const uint16 M68K_RTD = 0x4e74;
const uint16 M68K_RTR = 0x4e77;
const uint16 M68K_JMP = 0x4ef9;
const uint16 M68K_JMP_A0 = 0x4ed0;
const uint16 M68K_JSR = 0x4eb9;
const uint16 M68K_JSR_A0 = 0x4e90;

static struct {
    const char *name;
    int32_t id;
} mac_desc[]  = {
    { "Classic"                     , 1 },
    { "Mac XL"                      , 2 },
    { "Mac 512KE"                   , 3 },
    { "Mac Plus"                    , 4 },
    { "Mac SE"                      , 5 },
    { "Mac II"                      , 6 },
    { "Mac IIx"                     , 7 },
    { "Mac IIcx"                    , 8 },
    { "Mac SE/030"                  , 9 },
    { "Mac Portable"                , 10 },
    { "Mac IIci"                    , 11 },
    { "Mac IIfx"                    , 13 },
    { "Mac Classic"                 , 17 },
    { "Mac IIsi"                    , 18 },
    { "Mac LC"                      , 19 },
    { "Quadra 900"                  , 20 },
    { "PowerBook 170"               , 21 },
    { "Quadra 700"                  , 22 },
    { "Classic II"                  , 23 },
    { "PowerBook 100"               , 24 },
    { "PowerBook 140"               , 25 },
    { "Quadra 950"                  , 26 },
    { "Mac LCIII/Performa 450"      , 27 },
    { "PowerBook Duo 210"           , 29 },
    { "Centris 650"                 , 30 },
    { "PowerBook Duo 230"           , 32 },
    { "PowerBook 180"               , 33 },
    { "PowerBook 160"               , 34 },
    { "Quadra 800"                  , 35 },
    { "Quadra 650"                  , 36 },
    { "Mac LCII"                    , 37 },
    { "PowerBook Duo 250"           , 38 },
    { "Mac IIvi"                    , 44 },
    { "Mac IIvm/Performa 600"       , 45 },
    { "Mac IIvx"                    , 48 },
    { "Color Classic/Performa 250"  , 49 },
    { "PowerBook 165c"              , 50 },
    { "Centris 610"                 , 52 },
    { "Quadra 610"                  , 53 },
    { "PowerBook 145"               , 54 },
    { "Mac LC520"                   , 56 },
    { "Quadra/Centris 660AV"        , 60 },
    { "Performa 46x"                , 62 },
    { "PowerBook 180c"              , 71 },
    { "PowerBook 520/520c/540/540c" , 72 },
    { "PowerBook Duo 270c"          , 77 },
    { "Quadra 840AV"                , 78 },
    { "Performa 550"                , 80 },
    { "PowerBook 165"               , 84 },
    { "PowerBook 190"               , 85 },
    { "Mac TV"                      , 88 },
    { "Mac LC475/Performa 47x"      , 89 },
    { "Mac LC575"                   , 92 },
    { "Quadra 605"                  , 94 },
    { "Quadra 630"                  , 98 },
    { "Mac LC580"                   , 99 },
    { "PowerBook Duo 280"           , 102 },
    { "PowerBook Duo 280c"          , 103 },
    { "PowerBook 150"               , 115 },
    { "unknown"                     , -1 }
};

uint16_t rom_version;
/* FIXME: CPUState */
int cpu_type = 3; /* 68030 */
int fpu_type;
int TwentyFourBitAddressing ;

/* Global variables */

uint32_t UniversalInfo;           /* ROM offset of UniversalInfo */
uint32_t PutScrapPatch;           /* Mac address of PutScrap() patch */
uint32_t GetScrapPatch = 0;       /* Mac address of GetScrap() patch */
uint32_t ROMBreakpoint = 0;       /* ROM offset of breakpoint (0 = disabled, 0x2310 = CritError) */

static uint32_t sony_offset;              /* ROM offset of .Sony driver */
static uint32_t serd_offset;              /* ROM offset of SERD resource (serial drivers) */
static uint32_t microseconds_offset;      /* ROM offset of Microseconds() replacement routine */
static uint32_t debugutil_offset;         /* ROM offset of DebugUtil() replacement routine */

/*
 *  Search offset of A-Trap routine in ROM
 */

static uint32 find_rom_trap(uint8_t *rom, uint16 trap)
{
    uint8_t *bp = (uint8 *)(rom + ldl_p(rom + 0x22));
    uint16_t rom_trap = 0xa800;
    uint32_t ofs = 0;
    int i;

again:
    for (i = 0; i < 0x400; i++) {
        bool unimplemented = false;
        uint8_t b = *bp++;

        if (b == 0x80) {           /* Unimplemented trap */
            unimplemented = true;
        } else if (b == 0xff) {    /* Absolute address */
            ofs = ldl_p((uint32_t*)bp);
            bp += 4;
        } else if (b & 0x80) {    /* 1 byte offset */
            int16_t add = (b & 0x7f) << 1;
            if (!add)
                return 0;
            ofs += add;
        } else {                  /* 2 byte offset */
            int16_t add = ((b << 8) | *bp++) << 1;
            if (!add) {
                return 0;
            }
            ofs += add;
        }
        if (rom_trap == trap)
            return unimplemented ? 0 : ofs;
        rom_trap++;
    }
    rom_trap = 0xa000;
    goto again;
}

/*
 *  Search ROM for byte string, return ROM offset (or 0)
 */

static uint32_t find_rom_data(uint8_t *rom, uint32_t start, uint32_t end,
                              const uint8_t *data, uint32_t data_len)
{
        uint32_t ofs = start;

        while (ofs < end) {
                if (!memcmp((void *)(rom + ofs), data, data_len)) {
                        return ofs;
                }
                ofs++;
        }
        return 0;
}

/*
 *  Search ROM resource by type/ID, return ROM offset of resource data
 */

static uint32_t rsrc_ptr = 0;

static uint32_t find_rom_resource(uint8_t *rom, uint32_t s_type,
                                  int16_t s_id, bool cont)
{
    uint8_t *lp = rom + ldl_p(rom + 0x1a);

    if (!cont)
        rsrc_ptr = ldl_p(lp);
    else
        rsrc_ptr = ldl_p(rom + rsrc_ptr + 8);

    for (;;) {
        lp = rom + rsrc_ptr;
        uint32_t data = ldl_p(lp + 12);
        uint32_t type = ldl_p(lp + 16);
        int16_t id = lduw_p(lp + 20);

        if (type == s_type && id == s_id)
            return data;

        rsrc_ptr = ldl_p(lp + 8);
        if (!rsrc_ptr)
            break;
        }
        return 0;
}

/*
 *  Driver stubs
 */

/* Replacement for .Sony driver */

static const uint8_t sony_driver[] = {

    /* Driver header */

    SonyDriverFlags >> 8, SonyDriverFlags & 0xff,
    0, 0, 0, 0, 0, 0,
    0x00, 0x18,                            /* Open() offset */
    0x00, 0x1c,                            /* Prime() offset */
    0x00, 0x20,                            /* Control() offset */
    0x00, 0x2c,                            /* Status() offset */
    0x00, 0x52,                            /* Close() offset */
    0x05, 0x2e, 0x53, 0x6f, 0x6e, 0x79,    /* ".Sony" */

    /* Open() */

    M68K_EMUL_OP_SONY_OPEN >> 8, M68K_EMUL_OP_SONY_OPEN & 0xff,
    0x4e, 0x75,                            /*  rts */

    /* Prime() */

    M68K_EMUL_OP_SONY_PRIME >> 8, M68K_EMUL_OP_SONY_PRIME & 0xff,
    0x60, 0x0e,                            /*  bra IOReturn */

    /* Control() */

    M68K_EMUL_OP_SONY_CONTROL >> 8, M68K_EMUL_OP_SONY_CONTROL & 0xff,
    0x0c, 0x68, 0x00, 0x01, 0x00, 0x1a,    /*  cmp.w #1,$1a(a0) */
    0x66, 0x04,                            /*  bne IOReturn */
    0x4e, 0x75,                            /*  rts */

    /* Status() */

    M68K_EMUL_OP_SONY_STATUS >> 8, M68K_EMUL_OP_SONY_STATUS & 0xff,

    /* IOReturn */

    0x32, 0x28, 0x00, 0x06,                /*  move.w 6(a0),d1 */
    0x08, 0x01, 0x00, 0x09,                /*  btst #9,d1 */
    0x67, 0x0c,                            /*  beq 1 */
    0x4a, 0x40,                            /*  tst.w d0 */
    0x6f, 0x02,                            /*  ble 2 */
    0x42, 0x40,                            /*  clr.w d0 */
    0x31, 0x40, 0x00, 0x10,                /*2 move.w d0,$10(a0) */
    0x4e, 0x75,                            /*  rts */
    0x4a, 0x40,                            /*1 tst.w d0 */
    0x6f, 0x04,                            /*  ble 3 */
    0x42, 0x40,                            /*  clr.w d0 */
    0x4e, 0x75,                            /*  rts */
    0x2f, 0x38, 0x08, 0xfc,                /*3 move.l $8fc,-(sp) */
    0x4e, 0x75,                            /*  rts */

    /* Close() */

    0x70, 0xe8,                            /*  moveq #-24,d0 */
    0x4e, 0x75                             /*  rts */
};

/* Generic disk driver */

static const uint8_t disk_driver[] = {

    /* Driver header */

    DiskDriverFlags >> 8, DiskDriverFlags & 0xff,
    0, 0, 0, 0, 0, 0,
    0x00, 0x18,                            /* Open() offset */
    0x00, 0x1c,                            /* Prime() offset */
    0x00, 0x20,                            /* Control() offset */
    0x00, 0x2c,                            /* Status() offset */
    0x00, 0x52,                            /* Close() offset */
    0x05, 0x2e, 0x44, 0x69, 0x73, 0x6b,    /* ".Disk" */

    /* Open() */

    M68K_EMUL_OP_DISK_OPEN >> 8, M68K_EMUL_OP_DISK_OPEN & 0xff,
    0x4e, 0x75,                            /*  rts */

    /* Prime() */

    M68K_EMUL_OP_DISK_PRIME >> 8, M68K_EMUL_OP_DISK_PRIME & 0xff,
    0x60, 0x0e,                            /*  bra IOReturn */

    /* Control() */

    M68K_EMUL_OP_DISK_CONTROL >> 8, M68K_EMUL_OP_DISK_CONTROL & 0xff,
    0x0c, 0x68, 0x00, 0x01, 0x00, 0x1a,    /*  cmp.w #1,$1a(a0) */
    0x66, 0x04,                            /*  bne IOReturn */
    0x4e, 0x75,                            /*  rts */

    /* Status() */

    M68K_EMUL_OP_DISK_STATUS >> 8, M68K_EMUL_OP_DISK_STATUS & 0xff,

    /* IOReturn */

    0x32, 0x28, 0x00, 0x06,                /*  move.w 6(a0),d1 */
    0x08, 0x01, 0x00, 0x09,                /*  btst #9,d1 */
    0x67, 0x0c,                            /*  beq 1 */
    0x4a, 0x40,                            /*  tst.w d0 */
    0x6f, 0x02,                            /*  ble 2 */
    0x42, 0x40,                            /*  clr.w d0 */
    0x31, 0x40, 0x00, 0x10,                /*2 move.w d0,$10(a0) */
    0x4e, 0x75,                            /*  rts */
    0x4a, 0x40,                            /*1 tst.w d0 */
    0x6f, 0x04,                            /*  ble 3 */
    0x42, 0x40,                            /*  clr.w d0 */
    0x4e, 0x75,                            /*  rts */
    0x2f, 0x38, 0x08, 0xfc,                /*3 move.l $8fc,-(sp) */
    0x4e, 0x75,                            /*  rts */

    /* Close() */

    0x70, 0xe8,                            /*  moveq #-24,d0 */
    0x4e, 0x75                             /*  rts */
};

/* CD-ROM driver */

static const uint8_t cdrom_driver[] = {

    /* Driver header */

    CDROMDriverFlags >> 8, CDROMDriverFlags & 0xff, 0, 0, 0, 0, 0, 0,
    0x00, 0x1c,                            /* Open() offset */
    0x00, 0x20,                            /* Prime() offset */
    0x00, 0x24,                            /* Control() offset */
    0x00, 0x30,                            /* Status() offset */
    0x00, 0x56,                            /* Close() offset */
    0x08, 0x2e, 0x41, 0x70, 0x70, 0x6c, 0x65, 0x43, 0x44, 0x00,    /* ".AppleCD" */

    /* Open() */

    M68K_EMUL_OP_CDROM_OPEN >> 8, M68K_EMUL_OP_CDROM_OPEN & 0xff,
    0x4e, 0x75,                            /*  rts */

    /* Prime() */
    M68K_EMUL_OP_CDROM_PRIME >> 8, M68K_EMUL_OP_CDROM_PRIME & 0xff,
    0x60, 0x0e,                            /*  bra        IOReturn */

    /* Control() */
    M68K_EMUL_OP_CDROM_CONTROL >> 8, M68K_EMUL_OP_CDROM_CONTROL & 0xff,
    0x0c, 0x68, 0x00, 0x01, 0x00, 0x1a,    /*  cmp.w    #1,$1a(a0) */
    0x66, 0x04,                            /*  bne        IOReturn */
    0x4e, 0x75,                            /*  rts */

    /* Status() */
    M68K_EMUL_OP_CDROM_STATUS >> 8, M68K_EMUL_OP_CDROM_STATUS & 0xff,

    /* IOReturn */
    0x32, 0x28, 0x00, 0x06,                /*  move.w    6(a0),d1 */
    0x08, 0x01, 0x00, 0x09,                /*  btst        #9,d1 */
    0x67, 0x0c,                            /*  beq        1 */
    0x4a, 0x40,                            /*  tst.w    d0 */
    0x6f, 0x02,                            /*  ble        2 */
    0x42, 0x40,                            /*  clr.w    d0 */
    0x31, 0x40, 0x00, 0x10,                /*2 move.w    d0,$10(a0) */
    0x4e, 0x75,                            /*  rts */
    0x4a, 0x40,                            /*1 tst.w    d0 */
    0x6f, 0x04,                            /*  ble        3 */
    0x42, 0x40,                            /*  clr.w    d0 */
    0x4e, 0x75,                            /*  rts */
    0x2f, 0x38, 0x08, 0xfc,                /*3 move.l    $8fc,-(sp) */
    0x4e, 0x75,                            /*  rts */

    /* Close() */
    0x70, 0xe8,                            /*  moveq    #-24,d0 */
    0x4e, 0x75                            /*  rts */
};

static const uint8_t ain_driver[] = {    /* .AIn driver header */
    /* Driver header */
    0x4d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x18,                            /* Open() offset */
    0x00, 0x1e,                            /* Prime() offset */
    0x00, 0x24,                            /* Control() offset */
    0x00, 0x32,                            /* Status() offset */
    0x00, 0x38,                            /* Close() offset */
    0x04, 0x2e, 0x41, 0x49, 0x6e, 0x09,    /* ".AIn",9 */

    /* Open() */
    0x70, 0x00,                            /*  moveq    #0,d0 */
    M68K_EMUL_OP_SERIAL_OPEN >> 8, M68K_EMUL_OP_SERIAL_OPEN & 0xff,
    0x4e, 0x75,                            /*    rts */

    /* Prime() */
    0x70, 0x00,                            /*  moveq    #0,d0 */
    M68K_EMUL_OP_SERIAL_PRIME >> 8, M68K_EMUL_OP_SERIAL_PRIME & 0xff,
    0x60, 0x1a,                            /*    bra        IOReturn */

    /* Control() */
    0x70, 0x00,                            /*  moveq    #0,d0 */
    M68K_EMUL_OP_SERIAL_CONTROL >> 8, M68K_EMUL_OP_SERIAL_CONTROL & 0xff,
    0x0c, 0x68, 0x00, 0x01, 0x00, 0x1a,    /*    cmp.w    #1,$1a(a0) */
    0x66, 0x0e,                            /*    bne        IOReturn */
    0x4e, 0x75,                            /*    rts */

    /* Status() */
    0x70, 0x00,                            /*  moveq    #0,d0 */
    M68K_EMUL_OP_SERIAL_STATUS >> 8, M68K_EMUL_OP_SERIAL_STATUS & 0xff,
    0x60, 0x06,                            /*  bra IOReturn */

    /* Close() */
    0x70, 0x00,                            /*  moveq    #0,d0 */
    M68K_EMUL_OP_SERIAL_CLOSE >> 8, M68K_EMUL_OP_SERIAL_CLOSE & 0xff,
    0x4e, 0x75,                            /*    rts */

    /* IOReturn */
    0x32, 0x28, 0x00, 0x06,                /*    move.w    6(a0),d1 */
    0x08, 0x01, 0x00, 0x09,                /*    btst    #9,d1 */
    0x67, 0x0c,                            /*    beq        1 */
    0x4a, 0x40,                            /*    tst.w    d0 */
    0x6f, 0x02,                            /*    ble        2 */
    0x42, 0x40,                            /*    clr.w    d0 */
    0x31, 0x40, 0x00, 0x10,                /*2    move.w    d0,$10(a0) */
    0x4e, 0x75,                            /*    rts */
    0x4a, 0x40,                            /*1    tst.w    d0 */
    0x6f, 0x04,                            /*    ble        3 */
    0x42, 0x40,                            /*    clr.w    d0 */
    0x4e, 0x75,                            /*    rts */
    0x2f, 0x38, 0x08, 0xfc,                /*3    move.l    $8fc,-(a7) */
    0x4e, 0x75,                            /*    rts */
};

static const uint8_t aout_driver[] = {    /* .AOut driver header */
    /* Driver header */
    0x4e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x1a,                            /* Open() offset */
    0x00, 0x20,                            /* Prime() offset */
    0x00, 0x26,                            /* Control() offset */
    0x00, 0x34,                            /* Status() offset */
    0x00, 0x3a,                            /* Close() offset */
    0x05, 0x2e, 0x41, 0x4f, 0x75, 0x74, 0x09, 0x00,        /* ".AOut",9 */

    /* Open() */
    0x70, 0x01,                            /*  moveq    #1,d0 */
    M68K_EMUL_OP_SERIAL_OPEN >> 8, M68K_EMUL_OP_SERIAL_OPEN & 0xff,
    0x4e, 0x75,                            /*    rts */

    /* Prime() */
    0x70, 0x01,                            /*  moveq    #1,d0 */
    M68K_EMUL_OP_SERIAL_PRIME >> 8, M68K_EMUL_OP_SERIAL_PRIME & 0xff,
    0x60, 0x1a,                            /*    bra        IOReturn */

    /* Control() */
    0x70, 0x01,                            /*  moveq    #1,d0 */
    M68K_EMUL_OP_SERIAL_CONTROL >> 8, M68K_EMUL_OP_SERIAL_CONTROL & 0xff,
    0x0c, 0x68, 0x00, 0x01, 0x00, 0x1a,    /*    cmp.w    #1,$1a(a0) */
    0x66, 0x0e,                            /*    bne        IOReturn */
    0x4e, 0x75,                            /*    rts */

    /* Status() */
    0x70, 0x01,                            /*  moveq    #1,d0 */
    M68K_EMUL_OP_SERIAL_STATUS >> 8, M68K_EMUL_OP_SERIAL_STATUS & 0xff,
    0x60, 0x06,                            /*  bra IOReturn */

    /* Close() */
    0x70, 0x01,                            /*  moveq    #1,d0 */
    M68K_EMUL_OP_SERIAL_CLOSE >> 8, M68K_EMUL_OP_SERIAL_CLOSE & 0xff,
    0x4e, 0x75,                            /*    rts */

    /* IOReturn */
    0x32, 0x28, 0x00, 0x06,                /*    move.w    6(a0),d1 */
    0x08, 0x01, 0x00, 0x09,                /*    btst    #9,d1 */
    0x67, 0x0c,                            /*    beq        1 */
    0x4a, 0x40,                            /*    tst.w    d0 */
    0x6f, 0x02,                            /*    ble        2 */
    0x42, 0x40,                            /*    clr.w    d0 */
    0x31, 0x40, 0x00, 0x10,                /*2    move.w    d0,$10(a0) */
    0x4e, 0x75,                            /*    rts */
    0x4a, 0x40,                            /*1    tst.w    d0 */
    0x6f, 0x04,                            /*    ble        3 */
    0x42, 0x40,                            /*    clr.w    d0 */
    0x4e, 0x75,                            /*    rts */
    0x2f, 0x38, 0x08, 0xfc,                /*3    move.l    $8fc,-(a7) */
    0x4e, 0x75,                            /*    rts */
};

static const uint8_t bin_driver[] = {    /* .BIn driver header */
    /* Driver header */
    0x4d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x18,                            /* Open() offset */
    0x00, 0x1e,                            /* Prime() offset */
    0x00, 0x24,                            /* Control() offset */
    0x00, 0x32,                            /* Status() offset */
    0x00, 0x38,                            /* Close() offset */
    0x04, 0x2e, 0x42, 0x49, 0x6e, 0x09,    /* ".BIn",9 */

    /* Open() */
    0x70, 0x02,                            /*  moveq    #2,d0 */
    M68K_EMUL_OP_SERIAL_OPEN >> 8, M68K_EMUL_OP_SERIAL_OPEN & 0xff,
    0x4e, 0x75,                            /*    rts */

    /* Prime() */
    0x70, 0x02,                            /*  moveq    #2,d0 */
    M68K_EMUL_OP_SERIAL_PRIME >> 8, M68K_EMUL_OP_SERIAL_PRIME & 0xff,
    0x60, 0x1a,                            /*    bra        IOReturn */

    /* Control() */
    0x70, 0x02,                            /*  moveq    #2,d0 */
    M68K_EMUL_OP_SERIAL_CONTROL >> 8, M68K_EMUL_OP_SERIAL_CONTROL & 0xff,
    0x0c, 0x68, 0x00, 0x01, 0x00, 0x1a,    /*    cmp.w    #1,$1a(a0) */
    0x66, 0x0e,                            /*    bne        IOReturn */
    0x4e, 0x75,                            /*    rts */

    /* Status() */
    0x70, 0x02,                            /*  moveq    #2,d0 */
    M68K_EMUL_OP_SERIAL_STATUS >> 8, M68K_EMUL_OP_SERIAL_STATUS & 0xff,
    0x60, 0x06,                            /*  bra IOReturn */

    /* Close() */
    0x70, 0x02,                            /*  moveq    #2,d0 */
    M68K_EMUL_OP_SERIAL_CLOSE >> 8, M68K_EMUL_OP_SERIAL_CLOSE & 0xff,
    0x4e, 0x75,                            /*    rts */

    /* IOReturn */
    0x32, 0x28, 0x00, 0x06,                /*    move.w    6(a0),d1 */
    0x08, 0x01, 0x00, 0x09,                /*    btst    #9,d1 */
    0x67, 0x0c,                            /*    beq        1 */
    0x4a, 0x40,                            /*    tst.w    d0 */
    0x6f, 0x02,                            /*    ble        2 */
    0x42, 0x40,                            /*    clr.w    d0 */
    0x31, 0x40, 0x00, 0x10,                /*2    move.w    d0,$10(a0) */
    0x4e, 0x75,                            /*    rts */
    0x4a, 0x40,                            /*1    tst.w    d0 */
    0x6f, 0x04,                            /*    ble        3 */
    0x42, 0x40,                            /*    clr.w    d0 */
    0x4e, 0x75,                            /*    rts */
    0x2f, 0x38, 0x08, 0xfc,                /*3    move.l    $8fc,-(a7) */
    0x4e, 0x75,                            /*    rts */
};

static const uint8_t bout_driver[] = {    /* .BOut driver header */
    /* Driver header */
    0x4e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x1a,                            /* Open() offset */
    0x00, 0x20,                            /* Prime() offset */
    0x00, 0x26,                            /* Control() offset */
    0x00, 0x34,                            /* Status() offset */
    0x00, 0x3a,                            /* Close() offset */
    0x05, 0x2e, 0x42, 0x4f, 0x75, 0x74, 0x09, 0x00,        /* ".BOut",9 */

    /* Open() */
    0x70, 0x03,                            /*  moveq    #3,d0 */
    M68K_EMUL_OP_SERIAL_OPEN >> 8, M68K_EMUL_OP_SERIAL_OPEN & 0xff,
    0x4e, 0x75,                            /*    rts */

    /* Prime() */
    0x70, 0x03,                            /*  moveq    #3,d0 */
    M68K_EMUL_OP_SERIAL_PRIME >> 8, M68K_EMUL_OP_SERIAL_PRIME & 0xff,
    0x60, 0x1a,                            /*    bra        IOReturn */

    /* Control() */
    0x70, 0x03,                            /*  moveq    #3,d0 */
    M68K_EMUL_OP_SERIAL_CONTROL >> 8, M68K_EMUL_OP_SERIAL_CONTROL & 0xff,
    0x0c, 0x68, 0x00, 0x01, 0x00, 0x1a,    /*    cmp.w    #1,$1a(a0) */
    0x66, 0x0e,                            /*    bne        IOReturn */
    0x4e, 0x75,                            /*    rts */

    /* Status() */
    0x70, 0x03,                            /*  moveq    #3,d0 */
    M68K_EMUL_OP_SERIAL_STATUS >> 8, M68K_EMUL_OP_SERIAL_STATUS & 0xff,
    0x60, 0x06,                            /*  bra IOReturn */

    /* Close() */
    0x70, 0x03,                            /*  moveq    #3,d0 */
    M68K_EMUL_OP_SERIAL_CLOSE >> 8, M68K_EMUL_OP_SERIAL_CLOSE & 0xff,
    0x4e, 0x75,                            /*    rts */

    /* IOReturn */
    0x32, 0x28, 0x00, 0x06,                /*    move.w    6(a0),d1 */
    0x08, 0x01, 0x00, 0x09,                /*    btst    #9,d1 */
    0x67, 0x0c,                            /*    beq        1 */
    0x4a, 0x40,                            /*    tst.w    d0 */
    0x6f, 0x02,                            /*    ble        2 */
    0x42, 0x40,                            /*    clr.w    d0 */
    0x31, 0x40, 0x00, 0x10,                /*2    move.w    d0,$10(a0) */
    0x4e, 0x75,                            /*    rts */
    0x4a, 0x40,                            /*1    tst.w    d0 */
    0x6f, 0x04,                            /*    ble        3 */
    0x42, 0x40,                            /*    clr.w    d0 */
    0x4e, 0x75,                            /*    rts */
    0x2f, 0x38, 0x08, 0xfc,                /*3    move.l    $8fc,-(a7) */
    0x4e, 0x75,                            /*    rts */
};


/*
 *  ADBOp() patch
 */

static const uint8_t adbop_patch[] = {    /* Call ADBOp() completion procedure */
                                        /* The completion procedure may call ADBOp() again! */
    0x40, 0xe7,                /*    move    sr,-(sp) */
    0x00, 0x7c, 0x07, 0x00,    /*    ori        #$0700,sr */
    M68K_EMUL_OP_ADBOP >> 8, M68K_EMUL_OP_ADBOP & 0xff,
    0x48, 0xe7, 0x70, 0xf0,    /*    movem.l    d1-d3/a0-a3,-(sp) */
    0x26, 0x48,                /*    move.l    a0,a3 */
    0x4a, 0xab, 0x00, 0x04,    /*    tst.l    4(a3) */
    0x67, 0x00, 0x00, 0x18,    /*    beq        1 */
    0x20, 0x53,                /*    move.l    (a3),a0 */
    0x22, 0x6b, 0x00, 0x04,    /*    move.l    4(a3),a1 */
    0x24, 0x6b, 0x00, 0x08,    /*    move.l    8(a3),a2 */
    0x26, 0x78, 0x0c, 0xf8,    /*    move.l    $cf8,a3 */
    0x4e, 0x91,                /*    jsr        (a1) */
    0x70, 0x00,                /*    moveq    #0,d0 */
    0x60, 0x00, 0x00, 0x04,    /*    bra        2 */
    0x70, 0xff,                /*1    moveq    #-1,d0 */
    0x4c, 0xdf, 0x0f, 0x0e,    /*2    movem.l    (sp)+,d1-d3/a0-a3 */
    0x46, 0xdf,                /*    move    (sp)+,sr */
    0x4e, 0x75                /*    rts */
};

static char *p2c(uint8_t *pstring)
{
    static char cstring[256];
    int string_len, i;

    string_len = pstring[0];
    for (i = 0; i < string_len; i++) {
        cstring[i] = pstring[i + 1];
    }
    cstring[i] = 0;

    return cstring;
}

static void list_rom_resources(uint8_t *rom)
{
    uint8_t *lp;
    uint32_t rsrc_ptr;

    printf("ROM Resources:\n");
    printf("Offset\t Type\tID\tSize\tName\n");
    printf("------------------------------------------------\n");

    lp = rom + ldl_p((uint32_t*)(rom + 0x1a));
    rsrc_ptr = ldl_p((uint32_t*)lp);
    for (;;) {
        uint32_t data;

        lp = rom + rsrc_ptr;

        data = ldl_p((uint32_t*)(lp + 12));

        printf("%08x %c%c%c%c\t%d\t%d\t%s\n", data,
               (int)*(lp + 16), (int)*(lp + 17), (int)*(lp + 18),
               (int)*(lp + 19), lduw_p(lp + 20),
               ldl_p(rom + data - 8), p2c(lp + 23));

        rsrc_ptr = ldl_p((uint32_t*)(lp + 8));
        if (!rsrc_ptr)
            break;
    }
    printf("\n");
}

static void print_universal_info(uint8_t *rom, uint32_t info)
{
    uint8_t id = *(rom + info + 18);
    uint16_t hwcfg = lduw_p((uint16_t*)(rom + info + 16));
    uint16_t rom85 = lduw_p((uint16_t*)(rom + info + 20));
    const char *name = "unknown";
    int i;

    for (i = 0; mac_desc[i].id >= 0; i++)
        if (mac_desc[i].id == id + 6) {
            name = mac_desc[i].name;
            break;
        }

    printf("%08x %02x\t%04x\t%04x\t%s\n", info, id, hwcfg, rom85, name);
}

static void list_universal_infos(uint8_t *rom)
{
    uint32_t ofs = 0x3000;
    int i, q;

    for (i = 0; i < 0x2000; i += 2, ofs+=2) {
        if (ldl_p(rom + ofs) == 0xdc000505) {
            ofs -= 16;
            for (q = ofs; q > 0 && ldl_p(rom + q) != ofs - q; q-=4) ;
                if (q > 0) {
                    printf("Universal Table at %08x:\n", q);
                    printf("Offset\t ID\tHWCfg\tROM85\tModel\n");
                    printf("------------------------------------------------\n");
                    while ((ofs = ldl_p(rom + q))) {
                            print_universal_info(rom, ofs + q);
                            q += 4;
                }
            }
            break;
        }
    }
    printf("\n");
}

static void print_rom_info(uint8_t *rom)
{
    printf("\nROM Info:\n");
    printf("Checksum    : %08x\n", ldl_p(rom));
    printf("Version     : %04x\n", lduw_p(rom + 8));
    printf("Sub Version : %04x\n", lduw_p(rom + 18));
    printf("Resource Map: %08x\n", ldl_p(rom + 26));
    printf("Trap Tables : %08x\n\n", ldl_p(rom + 34));
    if (lduw_p(rom + 8) == MACOS_ROM_VERSION_32) {
            list_rom_resources(rom);
            list_universal_infos(rom);
    }
}


static int patch_rom_classic(uint8_t *rom)
{
    uint16_t *wp;
    uint32_t base;

    /* Don't jump into debugger (VIA line) */

    stw_p((uint16_t *)(rom + 0x1c40), 0x601e);

    /* Don't complain about incorrect ROM checksum */

    stw_p((uint16_t *)(rom + 0x1c6c), 0x7c00);

    /* Don't initialize IWM */

    wp = (uint16_t *)(rom + 0x50);
    stw_p(wp++, M68K_NOP);
    stw_p(wp, M68K_NOP);

    /* Skip startup sound */

    wp = (uint16_t *)(rom + 0x6a);
    stw_p(wp++, M68K_NOP);
    stw_p(wp, M68K_NOP);

    /* Don't loop in ADB init */

    stw_p((uint16_t *)(rom + 0x3364), M68K_NOP);

    /* Patch ClkNoMem */

    wp = (uint16_t *)(rom + 0xa2c0);
    stw_p(wp++, M68K_EMUL_OP_CLKNOMEM);
    stw_p(wp, 0x4ed5);            /* jmp    (a5) */

    /* Skip main memory test (not that it wouldn't pass, but it's faster that way) */

    wp = (uint16_t *)(rom + 0x11e);
    stw_p(wp++, M68K_NOP);
    stw_p(wp, M68K_NOP);

    /* Install our own drivers */

    wp = (uint16_t *)(rom + 0x3f82a);
    stw_p(wp++, M68K_EMUL_OP_INSTALL_DRIVERS);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp, M68K_NOP);

    /* Don't look for SCSI devices */

    stw_p((uint16_t *)(rom + 0xd5a), 0x601e);

    /* Replace .Sony driver */

    sony_offset = 0x34680;
    memcpy(rom + sony_offset, sony_driver, sizeof(sony_driver));

    /* Install .Disk and .AppleCD drivers */

    memcpy(rom + sony_offset + 0x100, disk_driver, sizeof(disk_driver));
    memcpy(rom + sony_offset + 0x200, cdrom_driver, sizeof(cdrom_driver));

    /* Copy icons to ROM */

    /* Sony Disk icon */

    SonyDiskIconAddr = MACROM_ADDR + sony_offset + 0x400;
    memcpy(rom + sony_offset + 0x400, SonyDiskIcon, sizeof(SonyDiskIcon));

    /* Sony Drive icon */

    SonyDriveIconAddr = MACROM_ADDR + sony_offset + 0x600;
    memcpy(rom + sony_offset + 0x600, SonyDriveIcon, sizeof(SonyDriveIcon));

    /* Disk icon */

    DiskIconAddr = MACROM_ADDR + sony_offset + 0x800;
    memcpy(rom + sony_offset + 0x800, DiskIcon, sizeof(DiskIcon));

    /* CDROM icon */

    CDROMIconAddr = MACROM_ADDR + sony_offset + 0xa00;
    memcpy(rom + sony_offset + 0xa00, CDROMIcon, sizeof(CDROMIcon));

    /* Install SERD patch and serial drivers */

    serd_offset = 0x31bae;

    stw_p((uint16_t *)(rom + serd_offset + 12), M68K_EMUL_OP_SERD);
    stw_p((uint16_t *)(rom + serd_offset + 14), M68K_RTS);

    memcpy(rom + serd_offset + 0x100, ain_driver, sizeof(ain_driver));
    memcpy(rom + serd_offset + 0x200, aout_driver, sizeof(aout_driver));
    memcpy(rom + serd_offset + 0x300, bin_driver, sizeof(bin_driver));
    memcpy(rom + serd_offset + 0x400, bout_driver, sizeof(bout_driver));

    /* Replace ADBOp() */

    memcpy(rom + 0x3880, adbop_patch, sizeof(adbop_patch));

    /* Replace Time Manager */

    wp = (uint16_t *)(rom + 0x1a95c);
    stw_p(wp++, M68K_EMUL_OP_INSTIME);
    stw_p(wp++, M68K_RTS);
    stw_p(wp++, 0x40e7);        /* move    sr,-(sp) */
    stw_p(wp++, 0x007c);        /* ori    #$0700,sr */
    stw_p(wp++, 0x0700);
    stw_p(wp++, M68K_EMUL_OP_RMVTIME);
    stw_p(wp++, 0x46df);        /* move    (sp)+,sr */
    stw_p(wp++, M68K_RTS);

    stw_p(wp++, 0x40e7);        /* move    sr,-(sp) */
    stw_p(wp++, 0x007c);        /* ori    #$0700,sr */
    stw_p(wp++, 0x0700);
    stw_p(wp++, M68K_EMUL_OP_PRIMETIME);
    stw_p(wp++, 0x46df);        /* move    (sp)+,sr */
    stw_p(wp++, M68K_RTS);

    microseconds_offset = 0x1a990;
    stw_p((uint16_t *)(rom + microseconds_offset), M68K_EMUL_OP_MICROSECONDS);
    stw_p((uint16_t *)(rom + microseconds_offset + 2), M68K_RTS);

    /* Replace DebugUtil */

    debugutil_offset = microseconds_offset + 4;
    stw_p((uint16_t *)(rom + debugutil_offset), M68K_EMUL_OP_DEBUGUTIL);
    stw_p((uint16_t *)(rom + debugutil_offset + 2), M68K_RTS);

    /* Replace SCSIDispatch() */

    wp = (uint16_t *)(rom + 0x1a206);
    stw_p(wp++, M68K_EMUL_OP_SCSI_DISPATCH);
    stw_p(wp++, 0x2e49);        /* move.l    a1,a7 */
    stw_p(wp, M68K_JMP_A0);

    /* Modify vCheckLoad() so we can patch resources */

    wp = (uint16_t *)(rom + 0xe740);
    stw_p(wp++, M68K_JMP);
    stw_p(wp++, ((MACROM_ADDR + sony_offset + 0x300) >> 16));
    stw_p(wp, ((MACROM_ADDR + sony_offset + 0x300) & 0xffff));

    wp = (uint16_t *)(rom + sony_offset + 0x300);
    stw_p(wp++, 0x2f03);        /* move.l    d3,-(sp) (save type) */
    stw_p(wp++, 0x2078);        /* move.l    $07f0,a0 */
    stw_p(wp++, 0x07f0);
    stw_p(wp++, M68K_JSR_A0);
    stw_p(wp++, 0x221f);        /* move.l    (sp)+,d1 (restore type) */
    stw_p(wp++, M68K_EMUL_OP_CHECKLOAD);
    stw_p(wp, M68K_RTS);

    /* Install PutScrap() patch for clipboard data exchange (the patch is activated by EMUL_OP_INSTALL_DRIVERS) */

    PutScrapPatch = MACROM_ADDR + sony_offset + 0xc00;
    base = MACROM_ADDR + 0x12794;

    wp = (uint16_t *)(rom + sony_offset + 0xc00);
    stw_p(wp++, M68K_EMUL_OP_PUT_SCRAP);
    stw_p(wp++, M68K_JMP);
    stw_p(wp++, base >> 16);
    stw_p(wp, base & 0xffff);

    /* Patch VIA interrupt handler */

    wp = (uint16_t *)(rom + 0x2b3a);    /* Level 1 handler */
    stw_p(wp++, 0x5888);        /* addq.l    #4,a0 */
    stw_p(wp++, 0x5888);        /* addq.l    #4,a0 */
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp, M68K_NOP);

    wp = (uint16_t *)(rom + 0x2be4);    /* 60Hz handler (handles everything) */
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_EMUL_OP_IRQ);
    stw_p(wp++, 0x4a80);        /* tst.l    d0 */
    stw_p(wp, 0x67f4);          /* beq        0x402be2 */

    return 0;
}

/* ROM patches for 32-bit clean Mac-II ROMs (version $067c) */

static int patch_rom_32(uint8_t *rom, int rom_size)
{
    uint16_t *wp;
    uint8_t *bp;
    uint32_t base;
    int i;
    uint32_t edisk_offset;
    static const uint8_t universal_dat[]    = { 0xdc, 0x00, 0x05, 0x05,
                                                0x3f, 0xff, 0x01, 0x00 };
    static const uint8_t memdisp_dat[]      = { 0x30, 0x3c, 0xa8, 0x9f,
                                                0xa7, 0x46, 0x30, 0x3c,
                                                0xa0, 0x5c, 0xa2, 0x47 };
    static const uint8_t init_asc_dat[]     = { 0x26, 0x68, 0x00, 0x30,
                                                0x12, 0x00, 0xeb, 0x01 };
    static const uint8_t via2b_dat[]        = { 0x20, 0x78, 0x0c, 0xec,
                                                0x11, 0x7c, 0x00, 0x90,
                                                0x00, 0x13, 0x4e, 0x75 };
    static const uint8_t bmove_dat[]        = { 0x20, 0x5f, 0x22, 0x5f,
                                                0x0c, 0x38, 0x00, 0x04,
                                                0x01, 0x2f };
    static const uint8_t ptest2_dat[]       = { 0x0c, 0x38, 0x00, 0x04,
                                                0x01, 0x2f, 0x6d, 0x54,
                                                0x48, 0xe7, 0xf8, 0x60 };
    static const uint8_t via2_dat[]         = { 0x20, 0x78, 0x0c, 0xec,
                                                0x11, 0x7c, 0x00, 0x90 };
    static const uint8_t frame_base_dat[]   = { 0x22, 0x78, 0x0d, 0xd8,
                                                0xd3, 0xe9, 0x00, 0x08 };
    static const uint8_t fix_memsize2_dat[] = { 0x22, 0x30, 0x81, 0xe2,
                                                0x0d, 0xdc, 0xff, 0xba,
                                                0xd2, 0xb0, 0x81, 0xe2,
                                                0x0d, 0xdc, 0xff, 0xec,
                                                0x21, 0xc1, 0x1e, 0xf8 };
    static const uint8_t nubus_dat[]        = { 0x45, 0xfa, 0x00, 0x0a,
                                                0x42, 0xa7, 0x10, 0x11 };
    static const uint8_t model_id2_dat[]    = { 0x45, 0xf9, 0x5f, 0xff,
                                                0xff, 0xfc, 0x20, 0x12 };
    static const uint8_t model_id_dat[]     = { 0x20, 0x7c, 0x5f, 0xff,
                                                0xff, 0xfc, 0x72, 0x07,
                                                0xc2, 0x90 };
    static const uint8_t init_scc_dat[]     = { 0x08, 0x38, 0x00, 0x01,
                                                0x0d, 0xd1, 0x67, 0x04 };
    static const uint8_t clk_no_mem_dat[]   = { 0x40, 0xc2, 0x00, 0x7c,
                                                0x07, 0x00, 0x48, 0x42 };
    static const uint8_t read_xpram3_dat[]  = { 0x48, 0xe7, 0xe0, 0x60,
                                                0x02, 0x01, 0x00, 0x70,
                                                0x0c, 0x01, 0x00, 0x20 };
    static const uint8_t read_xpram2_dat[]  = { 0x26, 0x4e, 0x08, 0x92,
                                                0x00, 0x02, 0xea, 0x59,
                                                0x02, 0x01, 0x00, 0x07,
                                                0x00, 0x01, 0x00, 0xb8 };
    static const uint8_t read_xpram_dat[]   = { 0x26, 0x4e, 0x41, 0xf9,
                                                0x50, 0xf0, 0x00, 0x00,
                                                0x08, 0x90, 0x00, 0x02 };
    static const uint8_t init_mmu3_dat[]    = { 0x0c, 0x2e, 0x00, 0x01,
                                                0xff, 0xe6, 0x66, 0x0c,
                                                0x4c, 0xed, 0x03, 0x87,
                                                0xff, 0xe8 };
    static const uint8_t init_mmu2_dat[]    = { 0x08, 0x06, 0x00, 0x0d,
                                                0x67 };
    static const uint8_t edisk_dat[]        = { 0xd5, 0xfc, 0x00, 0x01, 0x00,
                                                0x00, 0xb5, 0xfc, 0x00, 0xe0,
                                                0x00, 0x00 };

    /* Find UniversalInfo */

    base = find_rom_data(rom, 0x3400, 0x3c00, universal_dat, sizeof(universal_dat));
    if (base == 0) {
        return -1;
    }

    UniversalInfo = base - 0x10;

    /* Patch UniversalInfo (disable NuBus slots) */

    bp = rom + UniversalInfo +
         ldl_p(rom + UniversalInfo + 12);    /* nuBusInfoPtr */
    bp[0] = 0x03;
    for (i = 1; i < 16; i++) {
        bp[i] = 0x08;
    }

#if 0 /* FIXME */
    /* Set model ID from preferences */

    bp = rom + UniversalInfo + 18;        /* productKind */
    *bp = PrefsFindInt32("modelid");
#endif

    /* Make FPU optional */

    if (fpu_type == 0) {
        bp = rom + UniversalInfo + 22;    /* defaultRSRCs */
        *bp = 4;    /* FPU optional */
    }

    /* Install special reset opcode and jump
     * (skip hardware detection and tests)
     */

    base = MACROM_ADDR + 0xba;

    wp = (uint16_t *)(rom + 0x8c);
    stw_p(wp++, M68K_EMUL_OP_RESET);
    stw_p(wp++, M68K_JMP);
    stw_p(wp++, base >> 16);
    stw_p(wp, base & 0xffff);

    /* Don't GetHardwareInfo */

    wp = (uint16_t *)(rom + 0xc2);
    stw_p(wp++, M68K_NOP);
    stw_p(wp, M68K_NOP);

    /* Don't init VIAs */

    wp = (uint16_t *)(rom + 0xc6);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp, M68K_NOP);

    /* Fake CPU type test */

    wp = (uint16_t *)(rom + 0x7c0);
    stw_p(wp++, 0x7e00 + cpu_type);
    stw_p(wp, M68K_RTS);

    /* Don't clear end of BootGlobs upto end of RAM (address xxxx0000) */
    static const uint8_t clear_globs_dat[] = {0x42, 0x9a, 0x36, 0x0a, 0x66, 0xfa};
    base = find_rom_data(rom, 0xa00, 0xb00, clear_globs_dat, sizeof(clear_globs_dat));
    if (base) {        /* ROM15/20/22/23/26/27/32 */
        wp = (uint16_t *)(rom + base + 2);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
    }

    /* Patch InitMMU (no MMU present, don't choke on unknown CPU types) */
    if (rom_size <= 0x80000) {
        static const uint8_t init_mmu_dat[] = {0x0c, 0x47, 0x00, 0x03, 0x62, 0x00, 0xfe};
        if ((base = find_rom_data(rom, 0x4000, 0x50000, init_mmu_dat, sizeof(init_mmu_dat))) == 0) return -1;
    } else {
        static const uint8_t init_mmu_dat[] = {0x0c, 0x47, 0x00, 0x04, 0x62, 0x00, 0xfd};
        if ((base = find_rom_data(rom, 0x80000, 0x90000, init_mmu_dat, sizeof(init_mmu_dat))) == 0) return -1;
    }
    wp = (uint16_t *)(rom + base);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    wp++;
    stw_p(wp++, 0x7000);            /* moveq #0,d0 */
    stw_p(wp++, M68K_NOP);

    /* Patch InitMMU (no RBV present) */
    if (rom_size <= 0x80000) {
        base = find_rom_data(rom, 0x4000, 0x50000, init_mmu2_dat, sizeof(init_mmu2_dat));
    } else {
        base = find_rom_data(rom, 0x80000, 0x90000, init_mmu2_dat, sizeof(init_mmu2_dat));
    }
    if (base) {        /* ROM11/10/13/26 */
        bp = (uint8_t *)(rom + base + 4);
        *bp = 0x60;                        /* bra */
    }

    /* Patch InitMMU (don't init MMU) */
    if (rom_size <= 0x80000) {
        if ((base = find_rom_data(rom, 0x4000, 0x50000, init_mmu3_dat, sizeof(init_mmu3_dat))) == 0) return -1;
    } else {
        if ((base = find_rom_data(rom, 0x80000, 0x90000, init_mmu3_dat, sizeof(init_mmu3_dat))) == 0) return -1;
    }
    wp = (uint16_t *)(rom + base + 6);
    stw_p(wp++, M68K_NOP);

    /* Replace XPRAM routines */
    base = find_rom_data(rom, 0x40000, 0x50000, read_xpram_dat, sizeof(read_xpram_dat));
    if (base) {            /* ROM10 */
        wp = (uint16_t *)(rom + base);
        stw_p(wp++, M68K_EMUL_OP_READ_XPRAM);
        stw_p(wp++, 0x4ed6);        /* jmp    (a6) */
    }
    base = find_rom_data(rom, 0x40000, 0x50000, read_xpram2_dat, sizeof(read_xpram2_dat));
    if (base) {            /* ROM11 */
        wp = (uint16_t *)(rom + base);
        stw_p(wp++, M68K_EMUL_OP_READ_XPRAM);
        stw_p(wp++, 0x4ed6);        /* jmp    (a6) */
    }
    if (rom_size > 0x80000) {
        base = find_rom_data(rom, 0x80000, 0x90000, read_xpram3_dat, sizeof(read_xpram3_dat));
        if (base) {        /* ROM15 */
            wp = (uint16_t *)(rom + base);
            stw_p(wp++, M68K_EMUL_OP_READ_XPRAM2);
            stw_p(wp++, M68K_RTS);
        }
    }

    /* Patch ClkNoMem */
    base = find_rom_trap(rom, 0xa053);
    wp = (uint16_t *)(rom + base);
    if (lduw_p(wp) == 0x4ed5) {    /* ROM23/26/27/32 */
        if ((base = find_rom_data(rom, 0xb0000, 0xb8000, clk_no_mem_dat, sizeof(clk_no_mem_dat))) == 0) return -1;
    }
    wp = (uint16_t *)(rom + base);
    stw_p(wp++, M68K_EMUL_OP_CLKNOMEM);
    stw_p(wp++, 0x4ed5);            /* jmp    (a5) */

    /* Patch BootGlobs */
    wp = (uint16_t *)(rom + 0x10e);
    stw_p(wp++, M68K_EMUL_OP_PATCH_BOOT_GLOBS);
    stw_p(wp++, M68K_NOP);

    /* Don't init SCC */
    if ((base = find_rom_data(rom, 0xa00, 0xa80, init_scc_dat, sizeof(init_scc_dat))) == 0) return -1;
    wp = (uint16_t *)(rom + base);
    stw_p(wp++, M68K_RTS);

    /* Don't access 0x50f1a101 */
    wp = (uint16_t *)(rom + 0x4232);
    if (lduw_p(wp + 1) == 0x50f1 && lduw_p(wp + 2) == 0xa101) {    /* ROM32 */
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
    }

    /* Don't init IWM */
    wp = (uint16_t *)(rom + 0x9c0);
    stw_p(wp++, M68K_RTS);

    /* Don't init SCSI */
    wp = (uint16_t *)(rom + 0x9a0);
    stw_p(wp++, M68K_RTS);

    /* Don't init ASC */
    base = find_rom_data(rom, 0x4000, 0x5000, init_asc_dat, sizeof(init_asc_dat));
    if (base) {        /* ROM15/22/23/26/27/32 */
        wp = (uint16_t *)(rom + base);
        stw_p(wp++, 0x4ed6);        /* jmp    (a6) */
    }

    /* Don't EnableExtCache */
    wp = (uint16_t *)(rom + 0x190);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);

    /* Don't DisableIntSources */
    wp = (uint16_t *)(rom + 0x9f4c);
    stw_p(wp++, M68K_RTS);

    /* Fake CPU speed test (SetupTimeK) */
    /* *** increased jl : MacsBug uses TimeDBRA for kbd repeat timing */
    wp = (uint16_t *)(rom + 0x800);
    stw_p(wp++, 0x31fc);            /* move.w    #xxx,TimeDBRA */
    stw_p(wp++, 10000);
    stw_p(wp++, 0x0d00);
    stw_p(wp++, 0x31fc);            /* move.w    #xxx,TimeSCCDBRA */
    stw_p(wp++, 10000);
    stw_p(wp++, 0x0d02);
    stw_p(wp++, 0x31fc);            /* move.w    #xxx,TimeSCSIDBRA */
    stw_p(wp++, 10000);
    stw_p(wp++, 0x0b24);
    stw_p(wp++, 0x31fc);            /* move.w    #xxx,TimeRAMDBRA */
    stw_p(wp++, 10000);
    stw_p(wp++, 0x0cea);
    stw_p(wp++, M68K_RTS);

#if 0
    /* Move system zone to start of Mac RAM */
    wp = (uint16_t *)(rom + 0x50a);
    stw_p(wp++, HiWord(RAMBaseMac + 0x2000));
    stw_p(wp++, LoWord(RAMBaseMac + 0x2000));
    stw_p(wp++, HiWord(RAMBaseMac + 0x3800));
    stw_p(wp, LoWord(RAMBaseMac + 0x3800));
#endif

#if 0 /* FIXME */
    /* gb-- Temporary hack to get rid of crashes in Speedometer */
    wp = (uint16_t *)(rom + 0xdba2);
    if (lduw_p(wp) == 0x662c)        /* bne.b    #$2c */
        stw_p(wp++, 0x602c);        /* bra.b    #$2c */
#endif
    
    /* Don't write to VIA in InitTimeMgr */
    wp = (uint16_t *)(rom + 0xb0e2);
    stw_p(wp++, 0x4cdf);            /* movem.l    (sp)+,d0-d5/a0-a4 */
    stw_p(wp++, 0x1f3f);
    stw_p(wp++, M68K_RTS);

    /* Don't read ModelID from 0x5ffffffc */
    base = find_rom_data(rom, 0x40000, 0x50000, model_id_dat, sizeof(model_id_dat));
    if (base) {        /* ROM20 */
        wp = (uint16_t *)(rom + base + 8);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
    }

    /* Don't read ModelID from 0x5ffffffc */
    base = find_rom_data(rom, 0x4000, 0x5000, model_id2_dat, sizeof(model_id2_dat));
    if (base) {        /* ROM27/32 */
        wp = (uint16_t *)(rom + base + 6);
        stw_p(wp++, 0x7000);    /* moveq    #0,d0 */
        stw_p(wp++, 0xb040);    /* cmp.w    d0,d0 */
        stw_p(wp++, 0x4ed6);    /* jmp        (a6) */
    }

    /* Install slot ROM */
    if (!InstallSlotROM(rom, rom_size))
        return -1;

    /* Don't probe NuBus slots */
    base = find_rom_data(rom, 0x5000, 0x6000, nubus_dat, sizeof(nubus_dat));
    if (base) {        /* ROM10/11 */
        wp = (uint16_t *)(rom + base + 6);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
    }

    /* Don't EnableOneSecInts */
    static const uint8_t lea_dat[] = {0x41, 0xf9};
    if ((base = find_rom_data(rom, 0x226, 0x22a, lea_dat, sizeof(lea_dat))) == 0) return -1;
    wp = (uint16_t *)(rom + base);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);

    /* Don't EnableParityPatch/Enable60HzInts */
    if ((base = find_rom_data(rom, 0x230, 0x234, lea_dat, sizeof(lea_dat))) == 0) {
        wp = (uint16_t *)(rom + 0x230);
        if (lduw_p(wp) == 0x6100)    /* ROM11 */
            base = 0x230;
        else
            return -1;
    }
    wp = (uint16_t *)(rom + base);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);

    /* Compute boot stack pointer and fix logical/physical RAM size (CompBootStack) (must be done after InitMemMgr!) */
    wp = (uint16_t *)(rom + 0x490);
    stw_p(wp++, 0x2038);    /* move.l    $10c,d0 */
    stw_p(wp++, 0x010c);
    stw_p(wp++, 0xd0b8);    /* add.l    $2a6,d0 */
    stw_p(wp++, 0x02a6);
    stw_p(wp++, 0xe288);    /* lsr.l    #1,d0 */
    stw_p(wp++, 0x0880);    /* bclr        #0,d0 */
    stw_p(wp++, 0x0000);
    stw_p(wp++, 0x0440);    /* subi.w    #$400,d0 */
    stw_p(wp++, 0x0400);
    stw_p(wp++, 0x2040);    /* move.l    d0,a0 */
    stw_p(wp++, M68K_EMUL_OP_FIX_MEMSIZE);
    stw_p(wp++, M68K_RTS);

    base = find_rom_data(rom, 0x4c000, 0x4c080, fix_memsize2_dat, sizeof(fix_memsize2_dat));
    if (base) {        /* ROM15/22/23/26/27/32 */
        wp = (uint16_t *)(rom + base + 16);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
    }

    /* Don't open .Sound driver but install our own drivers */
    wp = (uint16_t *)(rom + 0x1142);
    stw_p(wp++, M68K_EMUL_OP_INSTALL_DRIVERS);

    /* Don't access SonyVars */
    wp = (uint16_t *)(rom + 0x1144);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    wp += 2;
    stw_p(wp++, M68K_NOP);

    /* Don't write to VIA in InitADB */
    wp = (uint16_t *)(rom + 0xa8a8);
    if (*wp == 0) {        /* ROM22/23/26/27/32 */
        wp = (uint16_t *)(rom + 0xb2c6a);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        wp = (uint16_t *)(rom + 0xb2d2e);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        wp += 2;
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
    } else {
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        wp = (uint16_t *)(rom + 0xa662);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
        wp += 2;
        stw_p(wp++, M68K_NOP);
        stw_p(wp++, M68K_NOP);
    }

    /* Don't EnableSlotInts */
    if ((base = find_rom_data(rom, 0x2ee, 0x2f2, lea_dat, sizeof(lea_dat))) == 0) return -1;
    wp = (uint16_t *)(rom + base);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);

    /* Don't mangle frame buffer base (GetDevBase) */
    wp = (uint16_t *)(rom + 0x5b78);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, 0x2401);        /* move.l    d1,d2 */
    stw_p(wp++, 0x605e);        /* bra        0x40805bde */

    /* Really don't mangle frame buffer base */
    if (rom_size > 0x80000) {
        base = find_rom_data(rom, 0x8c000, 0x8d000, frame_base_dat, sizeof(frame_base_dat));
        if (base) {        /* ROM22/23/26/27/32 */
            wp = (uint16_t *)(rom + base);
            stw_p(wp++, 0x2401);    /* move.l    d1,d2 */
            stw_p(wp++, M68K_RTS);
        }
    }

    /* Don't write to VIA2 */
    if ((base = find_rom_data(rom, 0xa000, 0xa400, via2_dat, sizeof(via2_dat))) == 0) return -1;
    wp = (uint16_t *)(rom + base + 4);
    stw_p(wp++, M68K_RTS);

    /* Don't write to VIA2, even on ROM20 */
    base = find_rom_data(rom, 0x40000, 0x44000, via2b_dat, sizeof(via2b_dat));
    if (base) {        /* ROM19/20 */
        wp = (uint16_t *)(rom + base + 4);
        stw_p(wp++, M68K_RTS);
    }

    /* Don't use PTEST instruction on 68040/060 */
    if (rom_size > 0x80000) {

        /* BlockMove() */
        base = find_rom_data(rom, 0x87000, 0x87800, bmove_dat, sizeof(bmove_dat));
        if (base) {        /* ROM15/22/23/26/27/32 */
            wp = (uint16_t *)(rom + base + 4);
            stw_p(wp++, M68K_EMUL_OP_BLOCK_MOVE);
            stw_p(wp++, 0x7000);
            stw_p(wp++, M68K_RTS);
        }

        /* SANE */
        base = find_rom_data(rom, 0, rom_size, ptest2_dat, sizeof(ptest2_dat));
        if (base) {        /* ROM15/20/22/23/26/27/32 */
            wp = (uint16_t *)(rom + base + 8);
            stw_p(wp++, M68K_NOP);
            stw_p(wp++, 0xf4f8);        /* cpusha    dc/ic */
            stw_p(wp++, M68K_NOP);
            stw_p(wp++, 0x7000);        /* moveq    #0,d0 */
            stw_p(wp++, M68K_RTS);
        }
    }

    /* Don't set MemoryDispatch() to unimplemented trap */

    base = find_rom_data(rom, 0x4f100, 0x4f180, memdisp_dat, sizeof(memdisp_dat));
    if (base) {    /* ROM15/22/23/26/27/32 */
        stw_p((uint16_t *)(rom + base + 10), M68K_NOP);
    }

    /* Patch .EDisk driver (don't scan for EDisks in the area ROMBase..0xe00000) */

    edisk_offset = find_rom_resource(rom, FOURCC('D','R','V','R'), 51, false);
    if (edisk_offset) {
        base = find_rom_data(rom, edisk_offset, edisk_offset + 0x10000,
                             edisk_dat, sizeof(edisk_dat));
        if (base) {
            wp = (uint16_t *)(rom + base + 8);
            stw_p(wp++, 0);
            stw_p(wp, 0);
        }
    }

    /* Replace .Sony driver */

    sony_offset = find_rom_resource(rom, FOURCC('D','R','V','R'), 4, false);
    memcpy(rom + sony_offset, sony_driver, sizeof(sony_driver));

    /* Install .Disk and .AppleCD drivers */

    memcpy(rom + sony_offset + 0x100, disk_driver, sizeof(disk_driver));
    memcpy(rom + sony_offset + 0x200, cdrom_driver, sizeof(cdrom_driver));

    /* Copy icons to ROM */

    SonyDiskIconAddr = MACROM_ADDR + sony_offset + 0x400;
    memcpy(rom + sony_offset + 0x400, SonyDiskIcon, sizeof(SonyDiskIcon));
    SonyDriveIconAddr = MACROM_ADDR + sony_offset + 0x600;
    memcpy(rom + sony_offset + 0x600, SonyDriveIcon, sizeof(SonyDriveIcon));
    DiskIconAddr = MACROM_ADDR + sony_offset + 0x800;
    memcpy(rom + sony_offset + 0x800, DiskIcon, sizeof(DiskIcon));
    CDROMIconAddr = MACROM_ADDR + sony_offset + 0xa00;
    memcpy(rom + sony_offset + 0xa00, CDROMIcon, sizeof(CDROMIcon));

    /* Install SERD patch and serial drivers */

    serd_offset = find_rom_resource(rom, FOURCC('S','E','R','D'), 0, false);

    wp = (uint16_t *)(rom + serd_offset + 12);
    stw_p(wp++, M68K_EMUL_OP_SERD);
    stw_p(wp, M68K_RTS);

    memcpy(rom + serd_offset + 0x100, ain_driver, sizeof(ain_driver));
    memcpy(rom + serd_offset + 0x200, aout_driver, sizeof(aout_driver));
    memcpy(rom + serd_offset + 0x300, bin_driver, sizeof(bin_driver));
    memcpy(rom + serd_offset + 0x400, bout_driver, sizeof(bout_driver));

    /* Replace ADBOp() */

    memcpy(rom + find_rom_trap(rom, 0xa07c), adbop_patch, sizeof(adbop_patch));

    /* Replace Time Manager (the Microseconds patch is
     * activated in InstallDrivers())
     */

    wp = (uint16_t *)(rom + find_rom_trap(rom, 0xa058));
    stw_p(wp++, M68K_EMUL_OP_INSTIME);
    stw_p(wp++, M68K_RTS);

    wp = (uint16_t *)(rom + find_rom_trap(rom, 0xa059));
    stw_p(wp++, 0x40e7);        /* move    sr,-(sp) */
    stw_p(wp++, 0x007c);        /* ori    #$0700,sr */
    stw_p(wp++, 0x0700);
    stw_p(wp++, M68K_EMUL_OP_RMVTIME);
    stw_p(wp++, 0x46df);        /* move    (sp)+,sr */
    stw_p(wp, M68K_RTS);

    wp = (uint16_t *)(rom + find_rom_trap(rom, 0xa05a));
    stw_p(wp++, 0x40e7);        /* move    sr,-(sp) */
    stw_p(wp++, 0x007c);        /* ori    #$0700,sr */
    stw_p(wp++, 0x0700);
    stw_p(wp++, M68K_EMUL_OP_PRIMETIME);
    stw_p(wp++, 0x46df);        /* move    (sp)+,sr */
    stw_p(wp++, M68K_RTS);

    microseconds_offset = (uint8_t *)wp - rom;
    stw_p(wp++, M68K_EMUL_OP_MICROSECONDS);
    stw_p(wp++, M68K_RTS);

    /* Replace DebugUtil */

    debugutil_offset = (uint8_t *)wp - rom;
    stw_p(wp++, M68K_EMUL_OP_DEBUGUTIL);
    stw_p(wp++, M68K_RTS);

    /* Replace SCSIDispatch() */

    wp = (uint16_t *)(rom + find_rom_trap(rom, 0xa815));
    stw_p(wp++,M68K_EMUL_OP_SCSI_DISPATCH);
    stw_p(wp++, 0x2e49);        /* move.l    a1,a7 */
    stw_p(wp++, M68K_JMP_A0);

    /* Modify vCheckLoad() so we can patch resources */

    wp = (uint16_t *)(rom + 0x1b8f4);
    stw_p(wp++, M68K_JMP);
    stw_p(wp++, (MACROM_ADDR + sony_offset + 0x300) >> 16);
    stw_p(wp, (MACROM_ADDR + sony_offset + 0x300) & 0xffff);

    wp = (uint16_t *)(rom + sony_offset + 0x300);
    stw_p(wp++, 0x2f03);        /* move.l    d3,-(sp) (save type) */
    stw_p(wp++, 0x2078);        /* move.l    $07f0,a0 */
    stw_p(wp++, 0x07f0);
    stw_p(wp++, M68K_JSR_A0);
    stw_p(wp++, 0x221f);        /* move.l    (sp)+,d1 (restore type) */
    stw_p(wp++, M68K_EMUL_OP_CHECKLOAD);
    stw_p(wp, M68K_RTS);

    /* Patch PowerOff() */

    wp = (uint16_t *)(rom + find_rom_trap(rom, 0xa05b));    /* PowerOff() */
    stw_p(wp, M68K_EMUL_OP_SHUTDOWN);

    /* Install PutScrap() patch for clipboard data exchange
     * (the patch is activated by EMUL_OP_INSTALL_DRIVERS)
     */

    PutScrapPatch = MACROM_ADDR + sony_offset + 0xc00;
    base = MACROM_ADDR + find_rom_trap(rom, 0xa9fe);

    wp = (uint16_t *)(rom + sony_offset + 0xc00);
    stw_p(wp++, M68K_EMUL_OP_PUT_SCRAP);
    stw_p(wp++, M68K_JMP);
    stw_p(wp++, base >> 16);
    stw_p(wp, base & 0xffff);

    /* Install GetScrap() patch for clipboard data exchange
     * (the patch is activated by EMUL_OP_INSTALL_DRIVERS)
     */

    GetScrapPatch = MACROM_ADDR + sony_offset + 0xd00;
    base = MACROM_ADDR + find_rom_trap(rom, 0xa9fd);

    wp = (uint16_t *)(rom + sony_offset + 0xd00);
    stw_p(wp++, M68K_EMUL_OP_GET_SCRAP);
    stw_p(wp++, M68K_JMP);
    stw_p(wp++, base >> 16);
    stw_p(wp, base & 0xffff);

    /* Look for double PACK 4 resources */

    base = find_rom_resource(rom, FOURCC('P','A','C','K'), 4, false);
    if (base == 0) {
        return -1;
    }

    base = find_rom_resource(rom, FOURCC('P','A','C','K'), 4, true);
    if (base  == 0 && fpu_type == 0) {
        fprintf(stderr, "WARNING: This ROM seems to require an FPU\n");
    }

    /* Patch VIA interrupt handler */

    wp = (uint16_t *)(rom + 0x9bc4);    /* Level 1 handler */
    stw_p(wp++, 0x7002);        /* moveq    #2,d0 (always 60Hz interrupt) */
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp, M68K_NOP);


    wp = (uint16_t *)(rom + 0xa296); /* 60Hz handler (handles everything) */
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_NOP);
    stw_p(wp++, M68K_EMUL_OP_IRQ);
    stw_p(wp++, 0x4a80);        /* tst.l    d0 */
    stw_p(wp, 0x67f4);        /* beq        0x4080a294 */

    return 0;
}

int macrom_patch(uint8_t *rom, int rom_size)
{
    int ret = -1;

    rom_version = lduw_p(rom + 8);

    print_rom_info(rom);

    switch (rom_version) {
    case MACOS_ROM_VERSION_CLASSIC:
        ret = patch_rom_classic(rom);
        break;
    case MACOS_ROM_VERSION_32:
        ret = patch_rom_32(rom, rom_size);
        break;
    }

    return ret;
}
