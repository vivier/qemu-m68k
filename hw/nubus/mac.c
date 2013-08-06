#include "hw/sysbus.h"
#include "exec/address-spaces.h"
#include "hw/nubus/mac.h"

static void mac_nubus_slot_write(void *opaque, hwaddr addr, uint64_t val,
                                       unsigned int size)
{
}


static uint64_t mac_nubus_slot_read(void *opaque, hwaddr addr,
                                    unsigned int size)
{
    return 0;
}

static const MemoryRegionOps mac_nubus_slot_ops = {
    .read  = mac_nubus_slot_read,
    .write = mac_nubus_slot_write,
    .endianness = DEVICE_NATIVE_ENDIAN,
    .valid = {
        .min_access_size = 1,
        .max_access_size = 1,
    },
};

static void mac_nubus_super_slot_write(void *opaque, hwaddr addr, uint64_t val,
                                       unsigned int size)
{
}

static uint64_t mac_nubus_super_slot_read(void *opaque, hwaddr addr,
                                          unsigned int size)
{
    return 0;
}

static const MemoryRegionOps mac_nubus_super_slot_ops = {
    .read  = mac_nubus_super_slot_read,
    .write = mac_nubus_super_slot_write,
    .endianness = DEVICE_BIG_ENDIAN,
    .valid = {
        .min_access_size = 1,
        .max_access_size = 1,
    },
};

static int mac_nubus_initfn(SysBusDevice *dev)
{
    MacNubusState *s = DO_UPCAST(MacNubusState, sysbus_dev, dev);
    MemoryRegion *super_slot_io = g_malloc(sizeof(MemoryRegion));;
    MemoryRegion *slot_io = g_malloc(sizeof(MemoryRegion));

    memory_region_init_io(super_slot_io, NULL, &mac_nubus_super_slot_ops,
                          s, "nubus-super-slots",
                          NUBUS_SUPER_SLOT_NB * NUBUS_SUPER_SLOT_SIZE);

    memory_region_init_io(slot_io, NULL, &mac_nubus_slot_ops,
                          s, "nubus-slots",
                          NUBUS_SLOT_NB * NUBUS_SLOT_SIZE);

    sysbus_init_mmio(dev, super_slot_io);
    sysbus_init_mmio(dev, slot_io);

    s->bus = nubus_bus_new(DEVICE(s), super_slot_io, slot_io);

    //qemu_register_reset(nubus_reset, s);
    return 0;
}

static void mac_nubus_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    SysBusDeviceClass *k = SYS_BUS_DEVICE_CLASS(klass);

    k->init = mac_nubus_initfn;
    //dc->reset = mac_nubus_reset;
    //dc->vmsd = &vmstate_mac_nubus;
    //dc->props = mac_nubus_properties;
    dc->desc = "Nubus bridge";
}

static const TypeInfo mac_nubus_type_info = {
    .name          = TYPE_MAC_NUBUS_BRIDGE,
    .parent        = TYPE_NUBUS_BRIDGE,
    .instance_size = sizeof(MacNubusState),
    .class_init    = mac_nubus_class_init,
};


NubusBus *nubus_mac_new(hwaddr super_slot_base, hwaddr slot_base)
{
    DeviceState *dev;

    dev = qdev_create(NULL, TYPE_MAC_NUBUS_BRIDGE);
    qdev_init_nofail(dev);
    sysbus_mmio_map(SYS_BUS_DEVICE(dev), 0, super_slot_base);
    sysbus_mmio_map(SYS_BUS_DEVICE(dev), 1, slot_base);

    return MAC_NUBUS_BRIDGE(dev)->bus;
}

static void mac_nubus_register_types(void)
{
    type_register_static(&mac_nubus_type_info);
}

type_init(mac_nubus_register_types)
