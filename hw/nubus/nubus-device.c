/*
 * QEMU Macintosh Nubus
 *
 * Copyright (c) 2013 Laurent Vivier <Laurent@Vivier.EU>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include "qemu/osdep.h"
#include "hw/nubus/nubus.h"

/* The Format Block Structure */

#define FBLOCK_DIRECTORY_OFFSET 0
#define FBLOCK_LENGTH           4
#define FBLOCK_CRC              8
#define FBLOCK_REVISION_LEVEL   12
#define FBLOCK_FORMAT           13
#define FBLOCK_TEST_PATTERN     14
#define FBLOCK_RESERVED         18
#define FBLOCK_BYTE_LANES       19

#define FBLOCK_SIZE             20

#    define FBLOCK_PATTERN_VAL  0x5a932bc7

static uint64_t nubus_fblock_read(void *opaque, hwaddr addr, unsigned int size)
{
    NubusDevice *dev = opaque;
    uint64_t val;

#define BYTE(v,b) (((v) >> (24-8*(b))) & 0xff)
    switch(addr) {
    case FBLOCK_BYTE_LANES:
        val = dev->byte_lanes;
        val |= (val ^ 0xf) << 4;
        break;
    case FBLOCK_RESERVED:
        val = 0x00;
        break;
    case FBLOCK_TEST_PATTERN...FBLOCK_TEST_PATTERN + 3:
        val = BYTE(FBLOCK_PATTERN_VAL, addr - FBLOCK_TEST_PATTERN);
        break;
    case FBLOCK_FORMAT:
        val = dev->rom_format;
        break;
    case FBLOCK_REVISION_LEVEL:
        val = dev->rom_rev;
        break;
    case FBLOCK_CRC...FBLOCK_CRC + 3:
        val = BYTE(dev->rom_crc, addr - FBLOCK_CRC);
        break;
    case FBLOCK_LENGTH...FBLOCK_LENGTH + 3:
        val = BYTE(dev->rom_length, addr - FBLOCK_LENGTH);
        break;
    case FBLOCK_DIRECTORY_OFFSET...FBLOCK_DIRECTORY_OFFSET + 3:
        val = BYTE(dev->directory_offset, addr - FBLOCK_DIRECTORY_OFFSET);
        break;
    default:
        val = 0;
        break;
    }
    return val;
}

static void nubus_fblock_write(void *opaque, hwaddr addr, uint64_t val,
                               unsigned int size)
{
    /* READ-ONLY */
}

static const MemoryRegionOps nubus_format_block_ops = {
    .read = nubus_fblock_read,
    .write = nubus_fblock_write,
    .endianness = DEVICE_NATIVE_ENDIAN,
    .valid = {
        .min_access_size = 1,
        .max_access_size = 1,
    }
};

static void nubus_register_format_block(NubusDevice *dev)
{
    char fblock_name[27];
    NubusBus *bus = nubus_bus_from_device(dev);

    sprintf(fblock_name, "nubus-slot-%d-format-block", dev->slot_nb);

    hwaddr fblock_offset = (dev->slot_nb + 1) * NUBUS_SLOT_SIZE - FBLOCK_SIZE;
    memory_region_init_io(&dev->fblock_io, NULL, &nubus_format_block_ops,
                          dev, fblock_name, FBLOCK_SIZE);
    memory_region_add_subregion(bus->slot_io, fblock_offset,
                                &dev->fblock_io);
}

NubusDevice *nubus_create(NubusBus *bus, const char *name)
{
    DeviceState *dev;
    NubusDevice *d;

    if (!bus) {
        hw_error("Tried to create nubus device %s with no nubus bus present.",
                 name);
    }

    if (bus->current_slot < NUBUS_FIRST_SLOT ||
        bus->current_slot > NUBUS_LAST_SLOT) {
        fprintf(stderr, "Cannot register nubus card '%s', not enough slot\n",
                name);
        return NULL;
    }

    dev = qdev_create(&bus->qbus, name);

    d = NUBUS_DEVICE(dev);
    d->slot_nb = bus->current_slot++;
    nubus_register_format_block(d);

    return d;
}

NubusDevice *nubus_try_create(NubusBus *bus, const char *name)
{
    DeviceState *dev;
    NubusDevice *d;

    if (!bus) {
        hw_error("Tried to create nubus device %s with no nubus bus present.",
                 name);
    }

    if (bus->current_slot < NUBUS_FIRST_SLOT ||
        bus->current_slot > NUBUS_LAST_SLOT) {
        fprintf(stderr, "Cannot register nubus card '%s', not enough slot\n",
                name);
        return NULL;
    }

    dev = qdev_try_create(&bus->qbus, name);
    if (!dev) {
        return NULL;
    }

    d = NUBUS_DEVICE(dev);
    d->slot_nb = bus->current_slot++;
    nubus_register_format_block(d);

    return d;
}

NubusDevice *nubus_create_simple(NubusBus *bus, const char *name)
{
    NubusDevice *dev;

    dev = nubus_create(bus, name);
    qdev_init_nofail(&dev->qdev);

    return dev;
}

void nubus_add_slot_mmio(NubusDevice *dev,
                         hwaddr offset,
                         MemoryRegion *subregion)
{
    NubusBus *bus = nubus_bus_from_device(dev);
    hwaddr slot_offset = dev->slot_nb * NUBUS_SLOT_SIZE + offset;
    memory_region_add_subregion(bus->slot_io, slot_offset, subregion);
}

void nubus_add_super_slot_mmio(NubusDevice *dev,
                               hwaddr offset,
                               MemoryRegion *subregion)
{
    NubusBus *bus = nubus_bus_from_device(dev);
    hwaddr slot_offset = (dev->slot_nb - 6) * NUBUS_SUPER_SLOT_SIZE + offset;
    memory_region_add_subregion(bus->super_slot_io, slot_offset, subregion);
}

static void mac_nubus_rom_write(void *opaque, hwaddr addr, uint64_t val,
                                       unsigned int size)
{
}

static uint64_t mac_nubus_rom_read(void *opaque, hwaddr addr,
                                    unsigned int size)
{
    NubusDevice *dev = opaque;

    return dev->rom[addr];
}

static const MemoryRegionOps mac_nubus_rom_ops = {
    .read  = mac_nubus_rom_read,
    .write = mac_nubus_rom_write,
    .endianness = DEVICE_NATIVE_ENDIAN,
    .valid = {
        .min_access_size = 1,
        .max_access_size = 1,
    },
};


void nubus_register_rom(NubusDevice *dev, const uint8_t *rom, uint32_t size,
                        int revision, int format, uint8_t byte_lanes)
{
    NubusBus *bus = nubus_bus_from_device(dev);
    hwaddr rom_offset;
    char rom_name[18];

    /* FIXME : really compute CRC */
    dev->rom_length = 0;
    dev->rom_crc = 0;

    dev->rom_rev = revision;
    dev->rom_format = format;

    dev->byte_lanes = byte_lanes;
    dev->directory_offset = -size;

    /* ROM */

    dev->rom = rom;
    sprintf(rom_name, "nubus-slot-%d-rom", dev->slot_nb);
    memory_region_init_io(&dev->rom_io, NULL, &mac_nubus_rom_ops,
                          dev, rom_name, size);
    memory_region_set_readonly(&dev->rom_io, true);

    rom_offset = (dev->slot_nb + 1) * NUBUS_SLOT_SIZE - FBLOCK_SIZE +
                 dev->directory_offset;
    memory_region_add_subregion(bus->slot_io, rom_offset, &dev->rom_io);

}


static void nubus_device_init(Object *obj)
{
    //NubusDevice *dev = NUBUS_DEVICE(obj);
}

static int nubus_qdev_init(DeviceState *qdev)
{
    NubusDevice *dev = NUBUS_DEVICE(qdev);
    NubusDeviceClass *klass = NUBUS_DEVICE_GET_CLASS(dev);

    if (klass->init) {
        return klass->init(dev);
    }

    return 0;
}

static void nubus_device_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *k = DEVICE_CLASS(klass);
    k->init = nubus_qdev_init;
    k->bus_type = TYPE_NUBUS_BUS;
}

static const TypeInfo nubus_device_type_info = {
    .name = TYPE_NUBUS_DEVICE,
    .parent = TYPE_DEVICE,
    .instance_size = sizeof(NubusDevice),
    .instance_init = nubus_device_init,
    .abstract = true,
    .class_size = sizeof(NubusDeviceClass),
    .class_init = nubus_device_class_init,
};

static void nubus_register_types(void)
{
    type_register_static(&nubus_device_type_info);
}

type_init(nubus_register_types)
