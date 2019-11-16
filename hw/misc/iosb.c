/*
 *
 */

#include "qemu/osdep.h"
#include "migration/vmstate.h"
#include "hw/sysbus.h"
#include "hw/misc/iosb.h"
#include "trace.h"

#define IOSB_SIZE       0x2000

#define Config     0
#define Config2    1
#define Sonic_SCSI 2
#define Revision   3
#define SCSI_ResID 4
#define Brightness 5
#define Timeout    6

static const VMStateDescription vmstate_IOSB = {
    .name = "IOSB",
    .version_id = 1,
    .minimum_version_id = 1,
    .fields      = (VMStateField[]) {
        VMSTATE_END_OF_LIST()
    }
};

static uint64_t IOSB_read(void *opaque, hwaddr addr,
                          unsigned size)
{
    /* IOSBState *s = opaque; */
    uint64_t value = 0;

    switch (addr >> 8) {
    case Config:
        value = 1; /* BCLK 33 MHz */
        break;
    default:
        break;
    }
    trace_IOSB_read((int)(addr >> 8), size, value);

    return value;
}

static void IOSB_write(void *opaque, hwaddr addr, uint64_t value,
                       unsigned size)
{
    /* IOSBState *s = opaque; */
    trace_IOSB_write((int)(addr >> 8), size, value);
}

static const MemoryRegionOps IOSB_mmio_ops = {
    .read = IOSB_read,
    .write = IOSB_write,
    .impl = {
        .min_access_size = 1,
        .max_access_size = 4,
    },
    .endianness = DEVICE_BIG_ENDIAN,
};

static void IOSB_init(Object *obj)
{
    IOSBState *s = IOSB(obj);
    SysBusDevice *sbd = SYS_BUS_DEVICE(obj);

    memory_region_init_io(&s->mem_regs, NULL, &IOSB_mmio_ops, s, "IOSB",
                          IOSB_SIZE);
    sysbus_init_mmio(sbd, &s->mem_regs);
}

static void IOSB_reset(DeviceState *d)
{
    /* IOSBState *s = IOSB(d); */
}

static void IOSB_class_init(ObjectClass *oc, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);

    dc->reset = IOSB_reset;
    dc->vmsd = &vmstate_IOSB;
}

static TypeInfo IOSB_info = {
    .name          = TYPE_IOSB,
    .parent        = TYPE_SYS_BUS_DEVICE,
    .instance_size = sizeof(IOSBState),
    .instance_init = IOSB_init,
    .class_init    = IOSB_class_init,
};

static void IOSB_register_types(void)
{
    type_register_static(&IOSB_info);
}

type_init(IOSB_register_types)
