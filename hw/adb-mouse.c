/* Mouse ADB device */

#include "hw.h"
#include "console.h"
#include "adb.h"
#include "adb-mouse.h"

typedef struct MouseState {
    int buttons_state, last_buttons_state;
    int dx, dy, dz;
} MouseState;

static void adb_mouse_event(void *opaque,
                            int dx1, int dy1, int dz1, int buttons_state)
{
    ADBDevice *d = opaque;
    MouseState *s = d->opaque;

    s->dx += dx1;
    s->dy += dy1;
    s->dz += dz1;
    s->buttons_state = buttons_state;
}


static int adb_mouse_poll(ADBDevice *d, uint8_t *obuf)
{
    MouseState *s = d->opaque;
    int dx, dy;

    if (s->last_buttons_state == s->buttons_state &&
        s->dx == 0 && s->dy == 0)
        return 0;

    dx = s->dx;
    if (dx < -63)
        dx = -63;
    else if (dx > 63)
        dx = 63;

    dy = s->dy;
    if (dy < -63)
        dy = -63;
    else if (dy > 63)
        dy = 63;

    s->dx -= dx;
    s->dy -= dy;
    s->last_buttons_state = s->buttons_state;

    dx &= 0x7f;
    dy &= 0x7f;

    if (!(s->buttons_state & MOUSE_EVENT_LBUTTON))
        dy |= 0x80;
    if (!(s->buttons_state & MOUSE_EVENT_RBUTTON))
        dx |= 0x80;

    obuf[0] = dy;
    obuf[1] = dx;
    return 2;
}

static int adb_mouse_request(ADBDevice *d, uint8_t *obuf,
                             const uint8_t *buf, int len)
{
    MouseState *s = d->opaque;
    int cmd, reg, olen;

    if ((buf[0] & 0x0f) == ADB_FLUSH) {
        /* flush mouse fifo */
        s->buttons_state = s->last_buttons_state;
        s->dx = 0;
        s->dy = 0;
        s->dz = 0;
        return 0;
    }

    cmd = buf[0] & 0xc;
    reg = buf[0] & 0x3;
    olen = 0;
    switch(cmd) {
    case ADB_WRITEREG:
        switch(reg) {
        case 2:
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
                break;
            }
        }
        break;
    case ADB_READREG:
        switch(reg) {
        case 0:
            olen = adb_mouse_poll(d, obuf);
            break;
        case 1:
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

static int adb_mouse_reset(ADBDevice *d)
{
    MouseState *s = d->opaque;

    d->handler = 2;
    d->devaddr = ADB_MOUSE;
    memset(s, 0, sizeof(MouseState));

    return 0;
}

static const VMStateDescription vmstate_adb_mouse = {
    .name = "adb_mouse",
    .version_id = 1,
    .minimum_version_id = 1,
    .minimum_version_id_old = 1,
    .fields      = (VMStateField[]) {
        VMSTATE_INT32(buttons_state, MouseState),
        VMSTATE_INT32(last_buttons_state, MouseState),
        VMSTATE_INT32(dx, MouseState),
        VMSTATE_INT32(dy, MouseState),
        VMSTATE_INT32(dz, MouseState),
        VMSTATE_END_OF_LIST()
    }
};

void adb_mouse_init(ADBBusState *bus)
{
    ADBDevice *d;
    MouseState *s;

    s = g_malloc0(sizeof(MouseState));
    d = adb_register_device(bus, ADB_MOUSE, adb_mouse_request,
                            adb_mouse_reset, s);
    qemu_add_mouse_event_handler(adb_mouse_event, d, 0, "QEMU ADB Mouse");
    vmstate_register(NULL, -1, &vmstate_adb_mouse, s);
}
