/*
 * djMEMC, macintosh memory and interrupt controller
 * (Quadra 610/650/800 & Centris 610/650)
 *
 *    https://mac68k.info/wiki/display/mac68k/djMEMC+Information
 *
 * The djMEMC is an Apple custom integrated circuit chip that performs a
 * variety of functions (RAM management, clock generation, ...).
 * It receives interrupt requests from various devices, assign priority to
 * each, and asserts one or more interrupt line to the CPU.
 */

#include "qemu/osdep.h"
#include "migration/vmstate.h"
#include "hw/irq.h"
#include "hw/misc/djmemc.h"
#include "hw/qdev-properties.h"
#include "trace.h"
#include "hw/nmi.h"

#define DJMEMC_SIZE       0x2000

enum {
    InterleaveConf    = 0,
    Bank0Conf         = 1,
    Bank1Conf         = 2,
    Bank2Conf         = 3,
    Bank3Conf         = 4,
    Bank4Conf         = 5,
    Bank5Conf         = 6,
    Bank6Conf         = 7,
    Bank7Conf         = 8,
    Bank8Conf         = 9,
    Bank9Conf         = 10,
    MemTop            = 11,
    Config            = 12,
    Refresh           = 13,
};

static const VMStateDescription vmstate_djMEMC = {
    .name = "djMEMC",
    .version_id = 1,
    .minimum_version_id = 1,
    .fields      = (VMStateField[]) {
        VMSTATE_UINT32(interleave, DjMEMCState),
        VMSTATE_UINT32_ARRAY(bank, DjMEMCState, DjMEMCMaxBanks),
        VMSTATE_UINT32(top, DjMEMCState),
        VMSTATE_UINT32(config, DjMEMCState),
        VMSTATE_UINT32(refresh_rate, DjMEMCState),
        VMSTATE_END_OF_LIST()
    }
};

static uint64_t djMEMC_read(void *opaque, hwaddr addr,
                              unsigned size)
{
    DjMEMCState *s = opaque;
    uint64_t value = 0;

    switch (addr >> 2) {
    case InterleaveConf:
        value = s->interleave;
        break;
    case Bank0Conf...Bank9Conf:
        value = s->bank[(addr >> 2) - Bank0Conf];
        break;
    case MemTop:
        value = s->top;
        break;
    case Config:
        value = s->config;
        break;
    case Refresh:
        value = s->refresh_rate;
        break;
    }
    trace_djMEMC_read((int)(addr >> 2), size, value);

    return value;
}

static void djMEMC_write(void *opaque, hwaddr addr, uint64_t value,
                         unsigned size)
{
    DjMEMCState *s = opaque;
    trace_djMEMC_write((int)(addr >> 2), size, value);

    switch (addr >> 2) {
    case InterleaveConf:
        s->interleave = value;
        break;
    case Bank0Conf...Bank9Conf:
        s->bank[(addr >> 2) - Bank0Conf] = value;
        break;
    case MemTop:
        s->top = value;
        break;
    case Config:
        s->config = value;
        break;
    case Refresh:
        s->refresh_rate = value;
        break;
    }
}

static const MemoryRegionOps djMEMC_mmio_ops = {
    .read = djMEMC_read,
    .write = djMEMC_write,
    .impl = {
        .min_access_size = 4,
        .max_access_size = 4,
    },
    .endianness = DEVICE_BIG_ENDIAN,
};

static void djMEMC_set_irq(void *opaque, int irq, int level)
{
    DjMEMCState *s = opaque;
    int i;


    if (level) {
        s->ipr |= 1 << irq;
    } else {
        s->ipr &= ~(1 << irq);
    }

    for (i = 7; i >= 0; i--) {
        if ((s->ipr >> i) & 1) {
            m68k_set_irq_level(s->cpu, i + 1, i + 25);
            return;
        }
    }
    m68k_set_irq_level(s->cpu, 0, 0);
}

static void djMEMC_init(Object *obj)
{
    DjMEMCState *s = DJMEMC(obj);
    SysBusDevice *sbd = SYS_BUS_DEVICE(obj);

    memory_region_init_io(&s->mem_regs, NULL, &djMEMC_mmio_ops, s, "djMEMC",
                          DJMEMC_SIZE);
    sysbus_init_mmio(sbd, &s->mem_regs);

    qdev_init_gpio_in(DEVICE(obj), djMEMC_set_irq, 8);
    object_property_add_link(obj, "cpu", TYPE_M68K_CPU,
                             (Object **) &s->cpu,
                             qdev_prop_allow_set_link_before_realize,
                             0);
}

static void djMEMC_reset(DeviceState *d)
{
    DjMEMCState *s = DJMEMC(d);
    int i;

    s->interleave = 0;
    s->top = 0;
    s->refresh_rate = 0;
    s->config = 0;

    for (i = 0; i < DjMEMCMaxBanks; i++) {
        s->bank[i] = 0;
    }
}

static void djMEMC_nmi(NMIState *n, int cpu_index, Error **errp)
{
    djMEMC_set_irq(DJMEMC(n), 6, 1);
}

static void djMEMC_class_init(ObjectClass *oc, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);
    NMIClass *nc = NMI_CLASS(oc);

    dc->reset = djMEMC_reset;
    dc->vmsd = &vmstate_djMEMC;
    nc->nmi_monitor_handler = djMEMC_nmi;
}

static TypeInfo djMEMC_info = {
    .name          = TYPE_DJMEMC,
    .parent        = TYPE_SYS_BUS_DEVICE,
    .instance_size = sizeof(DjMEMCState),
    .instance_init = djMEMC_init,
    .class_init    = djMEMC_class_init,
    .interfaces = (InterfaceInfo[]) {
         { TYPE_NMI },
         { }
    },
};

static void djMEMC_register_types(void)
{
    type_register_static(&djMEMC_info);
}

type_init(djMEMC_register_types)
