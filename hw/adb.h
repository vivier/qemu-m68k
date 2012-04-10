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

#if !defined(__ADB_H__)
#define __ADB_H__

#define MAX_ADB_DEVICES 16

#define ADB_MAX_OUT_LEN 16

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

#define ADB_DONGLE      1
#define ADB_KEYBOARD    2
#define ADB_MOUSE       3
#define ADB_TABLET      4
#define ADB_MODEM       5
#define ADB_MISC        7


typedef struct ADBDevice ADBDevice;

/* buf = NULL means polling */
typedef int ADBDeviceRequest(ADBDevice *d, uint8_t *buf_out,
                              const uint8_t *buf, int len);
typedef int ADBDeviceReset(ADBDevice *d);

struct ADBDevice {
    struct ADBBusState *bus;
    int devaddr;
    int handler;
    ADBDeviceRequest *devreq;
    ADBDeviceReset *devreset;
    void *opaque;
};

typedef struct ADBBusState {
    ADBDevice devices[MAX_ADB_DEVICES];
    int nb_devices;
    int poll_index;
} ADBBusState;

int adb_request(ADBBusState *s, uint8_t *buf_out,
                const uint8_t *buf, int len);
int adb_poll(ADBBusState *s, uint8_t *buf_out);

ADBDevice *adb_register_device(ADBBusState *s, int devaddr,
                               ADBDeviceRequest *devreq,
                               ADBDeviceReset *devreset,
                               void *opaque);

extern ADBBusState adb_bus;
#endif /* !defined(__ADB_H__) */
