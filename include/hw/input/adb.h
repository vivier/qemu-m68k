/*
 * QEMU ADB emulation shared definitions and prototypes
 *
 * Copyright (c) 2004-2007 Fabrice Bellard
 * Copyright (c) 2007 Jocelyn Mayer
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

#ifndef ADB_H
#define ADB_H

#include "hw/qdev.h"

#define MAX_ADB_DEVICES 16

#define ADB_MAX_OUT_LEN 16

typedef struct ADBBusState ADBBusState;

/* ADB commands */

#define ADB_BUSRESET            0x00
#define ADB_FLUSH               0x01
#define ADB_WRITEREG            0x08
#define ADB_READREG             0x0c

/* ADB device commands */

#define ADB_CMD_SELF_TEST               0xff
#define ADB_CMD_CHANGE_ID               0xfe
#define ADB_CMD_CHANGE_ID_AND_ACT       0xfd
#define ADB_CMD_CHANGE_ID_AND_ENABLE    0x00

/* ADB default device IDs (upper 4 bits of ADB command byte) */

#define ADB_DEVID_DONGLE      1
#define ADB_DEVID_KEYBOARD    2
#define ADB_DEVID_MOUSE       3
#define ADB_DEVID_TABLET      4
#define ADB_DEVID_MODEM       5
#define ADB_DEVID_MISC        7

typedef struct ADBDevice ADBDevice;

/* buf = NULL means polling */
typedef int ADBDeviceRequest(ADBDevice *d, uint8_t *buf_out,
                              const uint8_t *buf, int len);

#define TYPE_ADB_DEVICE "adb-device"
#define ADB_DEVICE(obj) OBJECT_CHECK(ADBDevice, (obj), TYPE_ADB_DEVICE)

struct ADBDevice {
    /*< private >*/
    DeviceState parent_obj;
    /*< public >*/

    int devaddr;
    int handler;
};

#define ADB_DEVICE_CLASS(cls) \
    OBJECT_CLASS_CHECK(ADBDeviceClass, (cls), TYPE_ADB_DEVICE)
#define ADB_DEVICE_GET_CLASS(obj) \
    OBJECT_GET_CLASS(ADBDeviceClass, (obj), TYPE_ADB_DEVICE)

typedef struct ADBDeviceClass {
    /*< private >*/
    DeviceClass parent_class;
    /*< public >*/

    ADBDeviceRequest *devreq;
} ADBDeviceClass;

#define TYPE_ADB_BUS "apple-desktop-bus"
#define ADB_BUS(obj) OBJECT_CHECK(ADBBusState, (obj), TYPE_ADB_BUS)

struct ADBBusState {
    /*< private >*/
    BusState parent_obj;
    /*< public >*/

    ADBDevice *devices[MAX_ADB_DEVICES];
    int nb_devices;
    int poll_index;
    qemu_irq data_ready;
    int data_in_size;
    int data_in_index;
    int data_out_index;
    uint8_t data_in[128];
    uint8_t data_out[16];
};

extern const VMStateDescription vmstate_adb_device;

int adb_request(ADBBusState *s, uint8_t *buf_out,
                const uint8_t *buf, int len);
int adb_poll(ADBBusState *s, uint8_t *buf_out, uint16_t poll_mask);

#define TYPE_ADB_KEYBOARD "adb-keyboard"
#define TYPE_ADB_MOUSE "adb-mouse"

int adb_send(ADBBusState *adb, int state, uint8_t data);
int adb_receive(ADBBusState *adb, int state, uint8_t *data);
#endif /* ADB_H */
