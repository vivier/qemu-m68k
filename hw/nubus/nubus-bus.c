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

static NubusBus *nubus = NULL;

static void nubus_bus_initfn(Object *obj)
{
    NubusBus *bus = NUBUS_BUS(obj);;
    bus->current_slot = NUBUS_FIRST_SLOT;
}

static void nubus_bus_class_init(ObjectClass *klass, void *data)
{
    //BusClass *k = BUS_CLASS(klass);

    //k->print_dev = nubus_dev_print;
    //k->get_fw_dev_path = nubus_get_fw_dev_path;
}

static const TypeInfo nubus_bus_info = {
    .name = TYPE_NUBUS_BUS,
    .parent = TYPE_BUS,
    .instance_size = sizeof(NubusBus),
    .instance_init = nubus_bus_initfn,
    .class_init = nubus_bus_class_init,
};

NubusBus *nubus_bus_new(DeviceState *dev, MemoryRegion *super_slot_io,
                       MemoryRegion *slot_io)
{
    if (nubus) {
        fprintf(stderr, "Can't create a second Nubus bus\n");
        return NULL;
    }

    if (NULL == dev) {
        dev = qdev_create(NULL, "nubus-bridge");
        qdev_init_nofail(dev);
    }

    nubus = NUBUS_BUS(qbus_create(TYPE_NUBUS_BUS, dev, NULL));

    nubus->super_slot_io = super_slot_io;
    nubus->slot_io = slot_io;

    return nubus;
}

static void nubus_register_types(void)
{
    type_register_static(&nubus_bus_info);
}

type_init(nubus_register_types)
