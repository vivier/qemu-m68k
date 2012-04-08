/*
 * QEMU Motorola 680x0 Macintosh Video Card Emulation
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

#include "sysemu.h"
#include "hw.h"
#include "framebuffer.h"

#include "macfb.h"

struct MacfbState {
    DisplayState *ds;
    uint8_t palette[256 * 3];
};
typedef struct MacfbState MacfbState;

static void macfb_draw_line(void *opaque, uint8_t *d, const uint8_t *p,
                             int width, int pitch)
{
    MacfbState *s = opaque;
    int i, j;

    for (i = 0, j = 0; i < width * ((graphic_depth + 7) / 8); i++) {
        d[j++] = s->palette[p[i] * 3];
        d[j++] = s->palette[p[i] * 3 + 1];
        d[j++] = s->palette[p[i] * 3 + 2];
        j++;
    }
}

static void macfb_invalidate(void *opaque)
{
}

static void macfb_update(void *opaque)
{
    MacfbState *s = opaque;
    DisplaySurface *info = s->ds->surface;
    int first = 0;
    int last  = 0;
    int i;
    for (i = 0; i < 256; i++) {
        s->palette[i * 3] = 255 -i;
        s->palette[i * 3 + 1] = 255 - i;
        s->palette[i * 3 + 2] = 255 - i;
    }

    framebuffer_update_display(s->ds,
                               MACFB_VIDEO_BASE, 640, 480, 640, info->linesize,
                               0, 1, macfb_draw_line, s, &first, &last);
    dpy_update(s->ds, 0, 0, 640, 480);
}

void macfb_init(void)
{
    MacfbState *s;
    int videomem_index;

    s = (MacfbState *)g_malloc0(sizeof(MacfbState));

    s->ds = graphic_console_init(macfb_update,
                              macfb_invalidate,
                              NULL, NULL, s);
    qemu_console_resize(s->ds, 640, 480);


    videomem_index = qemu_ram_alloc(NULL,"mac-video.ram",640 * 480);

    cpu_register_physical_memory(MACFB_VIDEO_BASE, 640 * 480,
                                 videomem_index | IO_MEM_RAM);

    return;
}
