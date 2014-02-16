/*
 * QEMU Macintosh floppy disk controller emulator (SWIM)
 *
 * Copyright (c) 2014 Laurent Vivier <laurent@vivier.eu>
 *
 * This work is licensed under the terms of the GNU GPL, version 2.  See
 * the COPYING file in the top-level directory.
 *
 */

#include "qemu/osdep.h"
#include "hw/sysbus.h"

/* IWM registers */

#define IWM_PH0L                0
#define IWM_PH0H                1
#define IWM_PH1L                2
#define IWM_PH1H                3
#define IWM_PH2L                4
#define IWM_PH2H                5
#define IWM_PH3L                6
#define IWM_PH3H                7
#define IWM_MTROFF              8
#define IWM_MTRON               9
#define IWM_INTDRIVE            10
#define IWM_EXTDRIVE            11
#define IWM_Q6L                 12
#define IWM_Q6H                 13
#define IWM_Q7L                 14
#define IWM_Q7H                 15

/* SWIM registers */

#define SWIM_WRITE_DATA         0
#define SWIM_WRITE_MARK         1
#define SWIM_WRITE_CRC          2
#define SWIM_WRITE_PARAMETER    3
#define SWIM_WRITE_PHASE        4
#define SWIM_WRITE_SETUP        5
#define SWIM_WRITE_MODE0        6
#define SWIM_WRITE_MODE1        7

#define SWIM_READ_DATA          8
#define SWIM_READ_MARK          9
#define SWIM_READ_ERROR         10
#define SWIM_READ_PARAMETER     11
#define SWIM_READ_PHASE         12
#define SWIM_READ_SETUP         13
#define SWIM_READ_STATUS        14
#define SWIM_READ_HANDSHAKE     15

#define REG_SHIFT               9

#define MAX_FD                  2

typedef struct SWIMCtrl SWIMCtrl;

typedef struct FDrive {
    SWIMCtrl *swimctrl;
    BlockBackend *blk;
} FDrive;

#define SWIM_MODE_IWM  0
#define SWIM_MODE_SWIM 1

/* bits in phase register */

#define SWIM_SEEK_NEGATIVE   0x074
#define SWIM_STEP            0x071
#define SWIM_MOTOR_ON        0x072
#define SWIM_MOTOR_OFF       0x076
#define SWIM_INDEX           0x073
#define SWIM_EJECT           0x077
#define SWIM_SETMFM          0x171
#define SWIM_SETGCR          0x175
#define SWIM_RELAX           0x033
#define SWIM_LSTRB           0x008
#define SWIM_CA_MASK         0x077

/* Select values for swim_select and swim_readbit */

#define SWIM_READ_DATA_0     0x074
#define SWIM_TWOMEG_DRIVE    0x075
#define SWIM_SINGLE_SIDED    0x076
#define SWIM_DRIVE_PRESENT   0x077
#define SWIM_DISK_IN         0x170
#define SWIM_WRITE_PROT      0x171
#define SWIM_TRACK_ZERO      0x172
#define SWIM_TACHO           0x173
#define SWIM_READ_DATA_1     0x174
#define SWIM_MFM_MODE        0x175
#define SWIM_SEEK_COMPLETE   0x176
#define SWIM_ONEMEG_MEDIA    0x177

/* Bits in handshake register */

#define SWIM_MARK_BYTE       0x01
#define SWIM_CRC_ZERO        0x02
#define SWIM_RDDATA          0x04
#define SWIM_SENSE           0x08
#define SWIM_MOTEN           0x10
#define SWIM_ERROR           0x20
#define SWIM_DAT2BYTE        0x40
#define SWIM_DAT1BYTE        0x80

/* bits in setup register */

#define SWIM_S_INV_WDATA     0x01
#define SWIM_S_3_5_SELECT    0x02
#define SWIM_S_GCR           0x04
#define SWIM_S_FCLK_DIV2     0x08
#define SWIM_S_ERROR_CORR    0x10
#define SWIM_S_IBM_DRIVE     0x20
#define SWIM_S_GCR_WRITE     0x40
#define SWIM_S_TIMEOUT       0x80

/* bits in mode register */

#define SWIM_CLFIFO          0x01
#define SWIM_ENBL1           0x02
#define SWIM_ENBL2           0x04
#define SWIM_ACTION          0x08
#define SWIM_WRITE_MODE      0x10
#define SWIM_HEDSEL          0x20
#define SWIM_MOTON           0x80

struct SWIMCtrl {
    MemoryRegion iomem;
    FDrive drives[MAX_FD];
    int mode;
    /* IWM mode */
    int iwm_switch;
    int regs[8];
#define IWM_PH0   0
#define IWM_PH1   1
#define IWM_PH2   2
#define IWM_PH3   3
#define IWM_MTR   4
#define IWM_DRIVE 5
#define IWM_Q6    6
#define IWM_Q7    7
    uint8_t iwm_data;
    uint8_t iwm_mode;
    /* SWIM mode */
    uint8_t swim_phase;
    uint8_t swim_mode;
};

#define TYPE_SYSBUS_SWIM "sysbus-swim"
#define SYSBUS_SWIM(obj) OBJECT_CHECK(SWIMCtrlSysBus, (obj), TYPE_SYSBUS_SWIM)

typedef struct SWIMCtrlSysBus {
    /*< private >*/
    SysBusDevice parent_obj;
    /*< public >*/

    struct SWIMCtrl state;
} SWIMCtrlSysBus;

static void iwmctrl_write(void *opaque, hwaddr reg, uint64_t value,
                          unsigned size)
{
    SWIMCtrl *swimctrl = opaque;

    reg >>= REG_SHIFT;

    swimctrl->regs[reg >> 1] = reg & 1;

    if (swimctrl->regs[IWM_Q6] &&
        swimctrl->regs[IWM_Q7]) {
        if (swimctrl->regs[IWM_MTR]) {
            /* data register */
            swimctrl->iwm_data = value;
        } else {
            /* mode register */
            swimctrl->iwm_mode = value;
            /* detect sequence to switch from IWM mode to SWIM mode */
            switch (swimctrl->iwm_switch) {
            case 0:
                if (value == 0x57) {
                    swimctrl->iwm_switch++;
                }
                break;
            case 1:
                if (value == 0x17) {
                    swimctrl->iwm_switch++;
                }
                break;
            case 2:
                if (value == 0x57) {
                    swimctrl->iwm_switch++;
                }
                break;
            case 3:
                if (value == 0x57) {
                    swimctrl->mode = SWIM_MODE_SWIM;
                    swimctrl->iwm_switch = 0;
                }
                break;
            }
        }
    }
}

static uint64_t iwmctrl_read(void *opaque, hwaddr reg, unsigned size)
{
    SWIMCtrl *swimctrl = opaque;

    reg >>= REG_SHIFT;

    swimctrl->regs[reg >> 1] = reg & 1;

    return 0;
}

static void swimctrl_write(void *opaque, hwaddr reg, uint64_t value,
                           unsigned size)
{
    SWIMCtrl *swimctrl = opaque;

    if (swimctrl->mode == SWIM_MODE_IWM) {
        iwmctrl_write(opaque, reg, value, size);
        return;
    }

    reg >>= REG_SHIFT;

    switch (reg) {
    case SWIM_WRITE_PHASE:
        swimctrl->swim_phase = value;
        break;
    case SWIM_WRITE_MODE0:
        swimctrl->swim_mode &= ~value;
        break;
    case SWIM_WRITE_MODE1:
        swimctrl->swim_mode |= value;
        break;
    case SWIM_WRITE_DATA:
    case SWIM_WRITE_MARK:
    case SWIM_WRITE_CRC:
    case SWIM_WRITE_PARAMETER:
    case SWIM_WRITE_SETUP:
        break;
    }
}

static uint64_t swimctrl_read(void *opaque, hwaddr reg, unsigned size)
{
    SWIMCtrl *swimctrl = opaque;
    uint32_t value = 0;

    if (swimctrl->mode == SWIM_MODE_IWM) {
        return iwmctrl_read(opaque, reg, size);
    }

    reg >>= REG_SHIFT;

    switch (reg) {
    case SWIM_READ_PHASE:
        value = swimctrl->swim_phase;
        break;
    case SWIM_READ_HANDSHAKE:
        if (swimctrl->swim_phase == SWIM_DRIVE_PRESENT) {
            /* always answer "no drive present" */
            value = SWIM_SENSE;
        }
        break;
    case SWIM_READ_DATA:
    case SWIM_READ_MARK:
    case SWIM_READ_ERROR:
    case SWIM_READ_PARAMETER:
    case SWIM_READ_SETUP:
    case SWIM_READ_STATUS:
        break;
    }

    return value;
}

static const MemoryRegionOps swimctrl_mem_ops = {
    .write = swimctrl_write,
    .read = swimctrl_read,
    .endianness = DEVICE_NATIVE_ENDIAN,
};

static void sysbus_swim_initfn(Object *obj)
{
    SysBusDevice *sbd = SYS_BUS_DEVICE(obj);
    SWIMCtrlSysBus *sys = SYSBUS_SWIM(obj);
    SWIMCtrl *swimctrl = &sys->state;

    memory_region_init_io(&swimctrl->iomem, obj, &swimctrl_mem_ops, swimctrl,
                          "swim", 0x2000);
    sysbus_init_mmio(sbd, &swimctrl->iomem);
}

static Property sysbus_swim_properties[] = {
    DEFINE_PROP_DRIVE("driveA", SWIMCtrlSysBus, state.drives[0].blk),
    DEFINE_PROP_DRIVE("driveB", SWIMCtrlSysBus, state.drives[1].blk),
    DEFINE_PROP_END_OF_LIST(),
};

static void sysbus_swim_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);

    dc->props = sysbus_swim_properties;
    set_bit(DEVICE_CATEGORY_STORAGE, dc->categories);
}

static const TypeInfo sysbus_swim_info = {
    .name          = TYPE_SYSBUS_SWIM,
    .parent        = TYPE_SYS_BUS_DEVICE,
     .instance_size = sizeof(SWIMCtrlSysBus),
    .instance_init = sysbus_swim_initfn,
    .class_init    = sysbus_swim_class_init,
};

static void swim_register_types(void)
{
    type_register_static(&sysbus_swim_info);
}

type_init(swim_register_types)
