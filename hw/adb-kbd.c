/* Keyboard ADB device */

#include "hw.h"
#include "console.h"
#include "adb.h"
#include "adb-kbd.h"

typedef struct KBDState {
    uint8_t data[128];
    int rptr, wptr, count;
} KBDState;

static const uint8_t pc_to_adb_keycode[256] = {
  0, 53, 18, 19, 20, 21, 23, 22, 26, 28, 25, 29, 27, 24, 51, 48,
 12, 13, 14, 15, 17, 16, 32, 34, 31, 35, 33, 30, 36, 54,  0,  1,
  2,  3,  5,  4, 38, 40, 37, 41, 39, 50, 56, 42,  6,  7,  8,  9,
 11, 45, 46, 43, 47, 44,123, 67, 58, 49, 57,122,120, 99,118, 96,
 97, 98,100,101,109, 71,107, 89, 91, 92, 78, 86, 87, 88, 69, 83,
 84, 85, 82, 65,  0,  0, 10,103,111,  0,  0,110, 81,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0, 94,  0, 93,  0,  0,  0,  0,  0,  0,104,102,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 76,125,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,105,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0, 75,  0,  0,124,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,115, 62,116,  0, 59,  0, 60,  0,119,
 61,121,114,117,  0,  0,  0,  0,  0,  0,  0, 55,126,  0,127,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0, 95,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
};

static void adb_kbd_put_keycode(void *opaque, int keycode)
{
    ADBDevice *d = opaque;
    KBDState *s = d->opaque;

    if (s->count < sizeof(s->data)) {
        s->data[s->wptr] = keycode;
        if (++s->wptr == sizeof(s->data))
            s->wptr = 0;
        s->count++;
    }
}

static int adb_kbd_poll(ADBDevice *d, uint8_t *obuf)
{
    static int ext_keycode;
    KBDState *s = d->opaque;
    int adb_keycode, keycode;
    int olen;

    olen = 0;
    for(;;) {
        if (s->count == 0)
            break;
        keycode = s->data[s->rptr];
        if (++s->rptr == sizeof(s->data))
            s->rptr = 0;
        s->count--;

        if (keycode == 0xe0) {
            ext_keycode = 1;
        } else {
            if (ext_keycode)
                adb_keycode =  pc_to_adb_keycode[keycode | 0x80];
            else
                adb_keycode =  pc_to_adb_keycode[keycode & 0x7f];
            obuf[0] = adb_keycode | (keycode & 0x80);
            /* NOTE: could put a second keycode if needed */
            obuf[1] = 0xff;
            olen = 2;
            ext_keycode = 0;
            break;
        }
    }
    return olen;
}

static int adb_kbd_request(ADBDevice *d, uint8_t *obuf,
                           const uint8_t *buf, int len)
{
    KBDState *s = d->opaque;
    int cmd, reg, olen;

    if ((buf[0] & 0x0f) == ADB_FLUSH) {
        /* flush keyboard fifo */
        s->wptr = s->rptr = s->count = 0;
        return 0;
    }

    cmd = buf[0] & 0xc;
    reg = buf[0] & 0x3;
    olen = 0;
    switch(cmd) {
    case ADB_WRITEREG:
        switch(reg) {
        case 2:
            /* LED status */
            break;
        case 3:
            switch(buf[2]) {
            case ADB_CMD_SELF_TEST:
                break;
            case ADB_CMD_CHANGE_ID:
            case ADB_CMD_CHANGE_ID_AND_ACT:
            case ADB_CMD_CHANGE_ID_AND_ENABLE:
                d->devaddr = buf[1] & 0xf;
                break;
            default:
                /* XXX: check this */
                d->devaddr = buf[1] & 0xf;
                d->handler = buf[2];
                break;
            }
        }
        break;
    case ADB_READREG:
        switch(reg) {
        case 0:
            olen = adb_kbd_poll(d, obuf);
            break;
        case 1:
            break;
        case 2:
            obuf[0] = 0x00; /* XXX: check this */
            obuf[1] = 0x07; /* led status */
            olen = 2;
            break;
        case 3:
            obuf[0] = d->handler;
            obuf[1] = d->devaddr;
            olen = 2;
            break;
        }
        break;
    }
    return olen;
}

static const VMStateDescription vmstate_adb_kbd = {
    .name = "adb_kbd",
    .version_id = 1,
    .minimum_version_id = 1,
    .minimum_version_id_old = 1,
    .fields      = (VMStateField[]) {
        VMSTATE_BUFFER(data, KBDState),
        VMSTATE_INT32(rptr, KBDState),
        VMSTATE_INT32(wptr, KBDState),
        VMSTATE_INT32(count, KBDState),
        VMSTATE_END_OF_LIST()
    }
};

static int adb_kbd_reset(ADBDevice *d)
{
    KBDState *s = d->opaque;

    d->handler = 1;
    d->devaddr = ADB_KEYBOARD;
    memset(s, 0, sizeof(KBDState));

    return 0;
}

void adb_kbd_init(ADBBusState *bus)
{
    ADBDevice *d;
    KBDState *s;
    s = g_malloc0(sizeof(KBDState));
    d = adb_register_device(bus, ADB_KEYBOARD, adb_kbd_request,
                            adb_kbd_reset, s);
    qemu_add_kbd_event_handler(adb_kbd_put_keycode, d);
    vmstate_register(NULL, -1, &vmstate_adb_kbd, s);
}
