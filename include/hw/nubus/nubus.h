#include "hw/qdev.h"
#include "exec/address-spaces.h"

#define NUBUS_SUPER_SLOT_SIZE 0x10000000U
#define NUBUS_SUPER_SLOT_NB   0x9

#define NUBUS_SLOT_SIZE       0x01000000
#define NUBUS_SLOT_NB         0xF

#define NUBUS_FIRST_SLOT      0x9
#define NUBUS_LAST_SLOT       0xF

#define TYPE_NUBUS_DEVICE "nubus-device"
#define NUBUS_DEVICE(obj) \
     OBJECT_CHECK(NubusDevice, (obj), TYPE_NUBUS_DEVICE)
#define NUBUS_DEVICE_CLASS(klass) \
     OBJECT_CLASS_CHECK(NubusDeviceClass, (klass), TYPE_NUBUS_DEVICE)
#define NUBUS_DEVICE_GET_CLASS(obj) \
     OBJECT_GET_CLASS(NubusDeviceClass, (obj), TYPE_NUBUS_DEVICE)

#define TYPE_NUBUS_BUS "nubus-bus"
#define NUBUS_BUS(obj) OBJECT_CHECK(NubusBus, (obj), TYPE_NUBUS_BUS)

#define TYPE_NUBUS_BRIDGE "nubus-bridge"
#define NUBUS_BRIDGE(obj) OBJECT_CHECK(NubusBridge, (obj), TYPE_NUBUS_BRIDGE)

typedef struct NubusDeviceClass {
    DeviceClass parent_class;
    int (*init)(NubusDevice *dev);
} NubusDeviceClass;

struct NubusBus {
    BusState qbus;
    int current_slot;
    MemoryRegion *super_slot_io;
    MemoryRegion *slot_io;
    qemu_irq *irqs;
};

struct NubusDevice {
    DeviceState qdev;

    int slot_nb;

    /* Format Block */

    MemoryRegion fblock_io;

    uint32_t rom_length;
    uint32_t rom_crc;
    uint8_t rom_rev;
    uint8_t rom_format;
    uint8_t byte_lanes;
    int32_t directory_offset;

    /* ROM */

    MemoryRegion rom_io;
    const uint8_t *rom;
};

NubusBus *nubus_bus_new(DeviceState *dev, MemoryRegion *super_slot_io,
                       MemoryRegion *slot_io);
NubusDevice *nubus_try_create(NubusBus *bus, const char *name);
NubusDevice *nubus_create_simple(NubusBus *bus, const char *name);
NubusDevice *nubus_create(NubusBus *bus, const char *name);
void nubus_add_slot_mmio(NubusDevice *dev, hwaddr offset,
                         MemoryRegion *subregion);
void nubus_add_super_slot_mmio(NubusDevice *dev, hwaddr offset,
                               MemoryRegion *subregion);
void nubus_register_rom(NubusDevice *dev, const uint8_t *rom, uint32_t size,
                        int revision, int format, uint8_t byte_lanes);

static inline NubusBus *nubus_bus_from_device(NubusDevice *d)
{
    return NUBUS_BUS(qdev_get_parent_bus(DEVICE(d)));
}
