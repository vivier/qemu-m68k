/*
 * QEMU ADB support
 *
 * Copyright (c) 2004 Fabrice Bellard
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
#include "hw/input/adb.h"

#define ADB_POLL_FREQ 50

/* Apple Macintosh Family Hardware Refenece
 * Table 19-10 ADB transaction states
 */

#define STATE_NEW	0
#define STATE_EVEN	1
#define STATE_ODD	2
#define STATE_IDLE	3

/* error codes */
#define ADB_RET_NOTPRESENT (-2)

/* The adb keyboard doesn't have every key imaginable */
#define NO_KEY 0xff

static void adb_device_reset(ADBDevice *d)
{
    qdev_reset_all(DEVICE(d));
}

int adb_request(ADBBusState *s, uint8_t *obuf, const uint8_t *buf, int len)
{
    ADBDevice *d;
    int devaddr, cmd, i;

    cmd = buf[0] & 0xf;
    if (cmd == ADB_BUSRESET) {
        for(i = 0; i < s->nb_devices; i++) {
            d = s->devices[i];
            adb_device_reset(d);
        }
        return 0;
    }
    devaddr = buf[0] >> 4;
    for(i = 0; i < s->nb_devices; i++) {
        d = s->devices[i];
        if (d->devaddr == devaddr) {
            ADBDeviceClass *adc = ADB_DEVICE_GET_CLASS(d);
            return adc->devreq(d, obuf, buf, len);
        }
    }
    return ADB_RET_NOTPRESENT;
}

int adb_poll(ADBBusState *s, uint8_t *obuf, uint16_t poll_mask)
{
    ADBDevice *d;
    int olen, i;
    uint8_t buf[1];

    olen = 0;
    for(i = 0; i < s->nb_devices; i++) {
        if (s->poll_index >= s->nb_devices)
            s->poll_index = 0;
        d = s->devices[s->poll_index];
        if ((1 << d->devaddr) & poll_mask) {
            buf[0] = ADB_READREG | (d->devaddr << 4);
            olen = adb_request(s, obuf + 1, buf, 1);
            /* if there is data, we poll again the same device */
            if (olen > 0) {
                obuf[0] = buf[0];
                olen++;
                break;
            }
        }
        s->poll_index++;
    }
    return olen;
}

int adb_via_poll(ADBBusState *adb, int state, uint8_t *data)
{
    if (state != STATE_IDLE)
        return 0;
    if (adb->data_in_size < adb->data_in_index)
	return 0;
    if (adb->data_out_index != 0)
        return 0;
    adb->data_in_index = 0;
    adb->data_out_index = 0;
    adb->data_in_size = adb_poll(adb, adb->data_in, 0xffff);
    if (adb->data_in_size) {
        *data = adb->data_in[adb->data_in_index++];
        qemu_irq_raise(adb->data_ready);
    }
    return adb->data_in_size;
}

int adb_send(ADBBusState *adb, int state, uint8_t data)
{
    switch(state) {
    case STATE_NEW:
        adb->data_out_index = 0;
        break;
    case STATE_EVEN:
        if ((adb->data_out_index & 1) == 0)
            return 0;
        break;
    case STATE_ODD:
        if (adb->data_out_index & 1)
            return 0;
        break;
    case STATE_IDLE:
        return 0;
    }
    adb->data_out[adb->data_out_index++] = data;
    qemu_irq_raise(adb->data_ready);
    return 1;
}

int adb_receive(ADBBusState *adb, int state, uint8_t *data)
{
    switch(state) {
    case STATE_NEW:
        return 0;
    case STATE_EVEN:
        if (adb->data_in_size <= 0) {
            qemu_irq_raise(adb->data_ready);
            return 0;
        }
        if (adb->data_in_index >= adb->data_in_size) {
            *data = 0;
            qemu_irq_raise(adb->data_ready);
            return 1;
        }
        if ((adb->data_in_index & 1) == 0)
            return 0;
        break;
    case STATE_ODD:
        if (adb->data_in_size <= 0) {
            qemu_irq_raise(adb->data_ready);
            return 0;
        }
        if (adb->data_in_index >= adb->data_in_size) {
            *data = 0;
            qemu_irq_raise(adb->data_ready);
            return 1;
        }
        if (adb->data_in_index & 1)
            return 0;
        break;
    case STATE_IDLE:
        if (adb->data_out_index == 0)
            return 0;
        adb->data_in_size = adb_request(adb, adb->data_in,
                                        adb->data_out, adb->data_out_index);
        adb->data_out_index = 0;
        adb->data_in_index = 0;
        if (adb->data_in_size < 0) {
            *data = 0xff;
            qemu_irq_raise(adb->data_ready);
            return -1;
        }
        if (adb->data_in_size == 0) {
            return 0;
        }
        break;
    }
    *data = adb->data_in[adb->data_in_index++];
    qemu_irq_raise(adb->data_ready);
    return 1;
}

static const TypeInfo adb_bus_type_info = {
    .name = TYPE_ADB_BUS,
    .parent = TYPE_BUS,
    .instance_size = sizeof(ADBBusState),
};

const VMStateDescription vmstate_adb_device = {
    .name = "adb_device",
    .version_id = 0,
    .minimum_version_id = 0,
    .fields = (VMStateField[]) {
        VMSTATE_INT32(devaddr, ADBDevice),
        VMSTATE_INT32(handler, ADBDevice),
        VMSTATE_END_OF_LIST()
    }
};

static void adb_device_realizefn(DeviceState *dev, Error **errp)
{
    ADBDevice *d = ADB_DEVICE(dev);
    ADBBusState *bus = ADB_BUS(qdev_get_parent_bus(dev));

    if (bus->nb_devices >= MAX_ADB_DEVICES) {
        return;
    }

    bus->devices[bus->nb_devices++] = d;
}

static void adb_device_class_init(ObjectClass *oc, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);

    dc->realize = adb_device_realizefn;
    dc->bus_type = TYPE_ADB_BUS;
}

static const TypeInfo adb_device_type_info = {
    .name = TYPE_ADB_DEVICE,
    .parent = TYPE_DEVICE,
    .instance_size = sizeof(ADBDevice),
    .abstract = true,
    .class_init = adb_device_class_init,
};

static void adb_register_types(void)
{
    type_register_static(&adb_bus_type_info);
    type_register_static(&adb_device_type_info);
}

type_init(adb_register_types)
