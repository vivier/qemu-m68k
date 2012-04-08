/*
 * QEMU Motorola 680x0 Macintosh Video Card Emulation
 *                 Copyright (c) 2012 Laurent Vivier
 *
 * some parts from QEMU G364 framebuffer Emulator.
 *                 Copyright (c) 2007-2011 Herve Poussineau
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
#include "sysbus.h"
#include "framebuffer.h"
#include "console.h"
#include "pixel_ops.h"

struct MacfbState {
    SysBusDevice busdev;
    MemoryRegion mem_vram;
    MemoryRegion mem_ctrl;
    DisplayState *ds;

    uint8_t *vram;
    uint32_t palette_current;
    uint8_t color_palette[256 * 3];
    uint32_t width, height; /* in pixels */
};
typedef struct MacfbState MacfbState;

#define MACFB_PAGE_SIZE 4096
#define VRAM_SIZE (1024 * 1024)

#define DAFB_RESET	0x200
#define DAFB_LUT	0x213

static inline int check_dirty(MacfbState *s, ram_addr_t page)
{
    return memory_region_get_dirty(&s->mem_vram, page, DIRTY_MEMORY_VGA);
}

static inline void reset_dirty(MacfbState *s,
                               ram_addr_t page_min, ram_addr_t page_max)
{
    memory_region_reset_dirty(&s->mem_vram,
                              page_min,
                              page_max + MACFB_PAGE_SIZE - page_min - 1,
                              DIRTY_MEMORY_VGA);
}

static void macfb_draw_graphic8(MacfbState *s)
{
    int i, w;
    uint8_t *vram;
    uint8_t *data_display, *dd;
    ram_addr_t page, page_min, page_max;
    int x, y;
    int xmin, xmax;
    int ymin, ymax;
    unsigned int (*rgb_to_pixel)(unsigned int r, unsigned int g, unsigned int b);

    switch (ds_get_bits_per_pixel(s->ds)) {
        case 8:
            rgb_to_pixel = rgb_to_pixel8;
            w = 1;
            break;
        case 15:
            rgb_to_pixel = rgb_to_pixel15;
            w = 2;
            break;
        case 16:
            rgb_to_pixel = rgb_to_pixel16;
            w = 2;
            break;
        case 32:
            rgb_to_pixel = rgb_to_pixel32;
            w = 4;
            break;
        default:
            hw_error("macfb: unknown host depth %d",
                     ds_get_bits_per_pixel(s->ds));
            return;
    }

    page = 0;
    page_min = (ram_addr_t)-1;
    page_max = 0;

    x = y = 0;
    xmin = s->width;
    xmax = 0;
    ymin = s->height;
    ymax = 0;

    vram= s->vram;
    /* XXX: out of range in vram? */
    data_display = dd = ds_get_data(s->ds);
    while (y < s->height) {
        if (check_dirty(s, page)) {
            if (y < ymin)
                ymin = ymax = y;
            if (page_min == (ram_addr_t)-1)
                page_min = page;
            page_max = page;
            if (x < xmin)
                xmin = x;
            for (i = 0; i < MACFB_PAGE_SIZE; i++) {
                uint8_t index;
                unsigned int color;
                /* normal area */
                index = *vram;
                color = (*rgb_to_pixel)(
                    s->color_palette[index * 3],
                    s->color_palette[index * 3 + 1],
                    s->color_palette[index * 3 + 2]);
                memcpy(dd, &color, w);
                dd += w;
                x++;
                vram++;
                if (x == s->width) {
                    xmax = s->width - 1;
                    y++;
                    if (y == s->height) {
                        ymax = s->height - 1;
                        goto done;
                    }
                    data_display = dd = data_display + ds_get_linesize(s->ds);
                    xmin = 0;
                    x = 0;
                }
            }
            if (x > xmax)
                xmax = x;
            if (y > ymax)
                ymax = y;
        } else {
            int dy;
            if (page_min != (ram_addr_t)-1) {
                reset_dirty(s, page_min, page_max);
                page_min = (ram_addr_t)-1;
                page_max = 0;
                dpy_update(s->ds, xmin, ymin, xmax - xmin + 1, ymax - ymin + 1);
                xmin = s->width;
                xmax = 0;
                ymin = s->height;
                ymax = 0;
            }
            x += MACFB_PAGE_SIZE;
            dy = x / s->width;
            x = x % s->width;
            y += dy;
            vram += MACFB_PAGE_SIZE;
            data_display += dy * ds_get_linesize(s->ds);
            dd = data_display + x * w;
        }
        page += MACFB_PAGE_SIZE;
    }

done:
    if (page_min != (ram_addr_t)-1) {
        dpy_update(s->ds, xmin, ymin, xmax - xmin + 1, ymax - ymin + 1);
        reset_dirty(s, page_min, page_max);
    }
}

static void macfb_invalidate(void *opaque)
{
    MacfbState *s = opaque;
    int i;

    for (i = 0; i < VRAM_SIZE; i += MACFB_PAGE_SIZE) {
        memory_region_set_dirty(&s->mem_vram, i);
    }
}

static void macfb_update(void *opaque)
{
    MacfbState *s = opaque;

    qemu_flush_coalesced_mmio_buffer();

    if (s->width == 0 || s->height == 0)
        return;

    if (s->width != ds_get_width(s->ds) || s->height != ds_get_height(s->ds)) {
        qemu_console_resize(s->ds, s->width, s->height);
    }

    macfb_draw_graphic8(s);
}

static void macfb_reset(DeviceState *d)
{
    MacfbState *s = container_of(d, MacfbState, busdev.qdev);
    int i;

    s->palette_current = 0;
    for (i = 0; i < 256; i++) {
        s->color_palette[i * 3] = 255 - i;
        s->color_palette[i * 3 + 1] = 255 - i;
        s->color_palette[i * 3 + 2] = 255 - i;
    }
    memset(s->vram, 0, VRAM_SIZE);
}

static uint64_t macfb_ctrl_read(void *opaque,
                                target_phys_addr_t addr,
                                unsigned int size)
{
    return 0;
}

static void macfb_ctrl_write(void *opaque,
                             target_phys_addr_t addr,
                             uint64_t val,
                             unsigned int size)
{
    MacfbState *s = opaque;
    switch(addr) {
    case DAFB_RESET:
        s->palette_current = 0;
        break;
    case DAFB_LUT:
        s->color_palette[s->palette_current++] = val;
        if (s->palette_current % 3)
            macfb_invalidate(s);
        break;
    }
}

static const MemoryRegionOps macfb_ctrl_ops = {
    .read = macfb_ctrl_read,
    .write = macfb_ctrl_write,
    .endianness = DEVICE_BIG_ENDIAN,
    .impl.min_access_size = 1,
    .impl.max_access_size = 4,
};

static int macfb_init(SysBusDevice *dev)
{
    MacfbState *s = FROM_SYSBUS(typeof(*s), dev);

    s->vram = g_malloc0(VRAM_SIZE);

    s->ds = graphic_console_init(macfb_update,
                                 macfb_invalidate,
                                 NULL, NULL, s);
    qemu_console_resize(s->ds, s->width, s->height);

    memory_region_init_ram_ptr(&s->mem_vram, &dev->qdev, "vram", VRAM_SIZE,
                               s->vram);
    memory_region_init_io(&s->mem_ctrl, &macfb_ctrl_ops, s, "ctrl", 0x1000);

    memory_region_set_coalescing(&s->mem_vram);
    sysbus_init_mmio_region(dev, &s->mem_vram);
    sysbus_init_mmio_region(dev, &s->mem_ctrl);

    return 0;
}

static int macfb_post_load(void *opaque, int version_id)
{
    macfb_invalidate(opaque);
    return 0;
}

static const VMStateDescription vmstate_macfb = {
    .name = "macfb",
    .version_id = 1,
    .minimum_version_id = 1,
    .minimum_version_id_old = 1,
    .post_load = macfb_post_load,
    .fields = (VMStateField[]) {
        VMSTATE_BUFFER_UNSAFE(color_palette, MacfbState, 0, 256 * 3),
        VMSTATE_UINT32(palette_current, MacfbState),
        VMSTATE_END_OF_LIST()
    }
};

static SysBusDeviceInfo macfb_info = {
    .init = macfb_init,
    .qdev.name = "macfb",
    .qdev.desc = "Macintosh 680x0 framebuffer",
    .qdev.size = sizeof(MacfbState),
    .qdev.vmsd  = &vmstate_macfb,
    .qdev.reset = macfb_reset,
    .qdev.props = (Property[]) {
        DEFINE_PROP_UINT32("width", MacfbState, width, 640),
        DEFINE_PROP_UINT32("height", MacfbState, height, 480),
        DEFINE_PROP_END_OF_LIST(),
    }
};

static void macfb_register(void)
{
    sysbus_register_withprop(&macfb_info);
}

device_init(macfb_register)
