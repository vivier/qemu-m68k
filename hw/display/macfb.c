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

#include "qemu/osdep.h"
#include "hw/sysbus.h"
#include "ui/console.h"
#include "ui/pixel_ops.h"
#include "hw/nubus/nubus.h"

#define VIDEO_BASE 0x00001000
#define DAFB_BASE  0x00800000

struct MacfbState {
    MemoryRegion mem_vram;
    MemoryRegion mem_ctrl;
    QemuConsole *con;

    uint8_t *vram;
    uint32_t palette_current;
    uint8_t color_palette[256 * 3];
    uint32_t width, height; /* in pixels */
    uint8_t depth;
};
typedef struct MacfbState MacfbState;

#define TYPE_MACFB "sysbus-macfb"
#define MACFB(obj) \
    OBJECT_CHECK(MacfbSysBusState, (obj), TYPE_MACFB)

#define MACFB_PAGE_SIZE 4096
#define MACFB_VRAM_SIZE (4 * 1024 * 1024)

#define DAFB_RESET	0x200
#define DAFB_LUT	0x213

#include "macfb-template.h"

static inline void reset_dirty(MacfbState *s,
                               ram_addr_t page_min, ram_addr_t page_max)
{
    memory_region_reset_dirty(&s->mem_vram,
                              page_min,
                              page_max + MACFB_PAGE_SIZE - page_min - 1,
                              DIRTY_MEMORY_VGA);
}

static void macfb_draw_graphic(MacfbState *s)
{
    DisplaySurface *surface = qemu_console_surface(s->con);
    ram_addr_t page, page_min, page_max;
    int y, ymin;
    int macfb_stride = (s->depth * s->width + 7) / 8;
    macfb_draw_line_func_t draw_line;

    if (s->depth > 24) {
        hw_error("macfb: unknown guest depth %d", s->depth);
        return;
    }
    if (surface_bits_per_pixel(surface) > 32) {
        hw_error("macfb: unknown host depth %d", surface_bits_per_pixel(surface));
        return;
    }
    draw_line = macfb_draw_line[s->depth - 1][surface_bits_per_pixel(surface) - 1];

    if (draw_line == NULL) {
        hw_error("macfb: unknown guest/host depth combination %d/%d", s->depth,
                 surface_bits_per_pixel(surface));
        return;
    }

    page_min = (ram_addr_t)-1;
    page_max = 0;
    ymin = -1;
    page = 0;
    for (y = 0; y < s->height; y++)
    {
        int update;

        update = memory_region_get_dirty(&s->mem_vram, page, macfb_stride,
                                         DIRTY_MEMORY_VGA);

        if (update) {
            uint8_t *data_display;

            data_display = surface_data(surface) + y * surface_stride(surface);
            draw_line(s, data_display, s->vram + page, s->width);

            if (ymin < 0) {
                ymin = y;
            }
            if (page_min == (ram_addr_t)-1) {
                page_min = page;
            }
            page_max = page + macfb_stride - 1;
        } else {
            if (ymin >= 0) {
                dpy_gfx_update(s->con, 0, ymin, s->width, y - ymin);
                ymin = -1;
            }
        }
        page += macfb_stride;
    }
    if (ymin >= 0) {
        dpy_gfx_update(s->con, 0, ymin, s->width, y - ymin);
    }
    if (page_min != (ram_addr_t)-1) {
        reset_dirty(s, page_min, page_max);
    }
}

static void macfb_invalidate_display(void *opaque)
{
    MacfbState *s = opaque;

    memory_region_set_dirty(&s->mem_vram, 0, MACFB_VRAM_SIZE);
}

static void macfb_update_display(void *opaque)
{
    MacfbState *s = opaque;
    DisplaySurface *surface = qemu_console_surface(s->con);

    qemu_flush_coalesced_mmio_buffer();

    if (s->width == 0 || s->height == 0)
        return;

    if (s->width != surface_width(surface) ||
        s->height != surface_height(surface)) {
        qemu_console_resize(s->con, s->width, s->height);
    }

    macfb_draw_graphic(s);
}

static void macfb_reset(MacfbState *s)
{
    int i;

    s->palette_current = 0;
    for (i = 0; i < 256; i++) {
        s->color_palette[i * 3] = 255 - i;
        s->color_palette[i * 3 + 1] = 255 - i;
        s->color_palette[i * 3 + 2] = 255 - i;
    }
    memset(s->vram, 0, MACFB_VRAM_SIZE);
    macfb_invalidate_display(s);
}

static uint64_t macfb_ctrl_read(void *opaque,
                                hwaddr addr,
                                unsigned int size)
{
    return 0;
}

static void macfb_ctrl_write(void *opaque,
                             hwaddr addr,
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
            macfb_invalidate_display(s);
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

static int macfb_post_load(void *opaque, int version_id)
{
    macfb_invalidate_display(opaque);
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

static const GraphicHwOps macfb_ops = {
    .invalidate = macfb_invalidate_display,
    .gfx_update = macfb_update_display,
};

static void macfb_init(DeviceState *dev, MacfbState *s)
{
    s->vram = g_malloc0(MACFB_VRAM_SIZE);

    s->con = graphic_console_init(dev, 0, &macfb_ops, s);

    memory_region_init_io(&s->mem_ctrl, NULL, &macfb_ctrl_ops, s, "macfb-ctrl",
                          0x1000);
    memory_region_init_ram_ptr(&s->mem_vram, NULL, "macfb-vram", MACFB_VRAM_SIZE,
                               s->vram);
    vmstate_register_ram(&s->mem_vram, dev);
    memory_region_set_coalescing(&s->mem_vram);
}

typedef struct {
    SysBusDevice busdev;
    MacfbState macfb;
} MacfbSysBusState;

typedef struct {
    NubusDevice busdev;
    MacfbState macfb;
} MacfbNubusState;

static int macfb_sysbus_init(SysBusDevice *dev)
{
    MacfbState *s =  &MACFB(dev)->macfb;

    macfb_init(DEVICE(dev), s);
    sysbus_init_mmio(dev, &s->mem_ctrl);
    sysbus_init_mmio(dev, &s->mem_vram);

    return 0;
}

const uint8_t macfb_rom[] = {
    255, 0, 0, 0,
};

static int macfb_nubus_init(NubusDevice *dev)
{
    MacfbState *s = &DO_UPCAST(MacfbNubusState, busdev, dev)->macfb;

    macfb_init(DEVICE(dev), s);
    nubus_add_slot_mmio(dev, DAFB_BASE, &s->mem_ctrl);
    nubus_add_slot_mmio(dev, VIDEO_BASE, &s->mem_vram);
    nubus_register_rom(dev, macfb_rom, sizeof(macfb_rom), 1, 9, 0xf);

    return 0;
}

static void macfb_sysbus_reset(DeviceState *d)
{
    MacfbSysBusState *s = MACFB(d);
    macfb_reset(&s->macfb);
}

static void macfb_nubus_reset(DeviceState *d)
{
    MacfbNubusState *s = DO_UPCAST(MacfbNubusState, busdev.qdev, d);
    macfb_reset(&s->macfb);
}

static Property macfb_sysbus_properties[] = {
    DEFINE_PROP_UINT32("width", MacfbSysBusState, macfb.width, 640),
    DEFINE_PROP_UINT32("height", MacfbSysBusState, macfb.height, 480),
    DEFINE_PROP_UINT8("depth", MacfbSysBusState, macfb.depth, 8),
    DEFINE_PROP_END_OF_LIST(),
};

static Property macfb_nubus_properties[] = {
    DEFINE_PROP_UINT32("width", MacfbNubusState, macfb.width, 640),
    DEFINE_PROP_UINT32("height", MacfbNubusState, macfb.height, 480),
    DEFINE_PROP_UINT8("depth", MacfbNubusState, macfb.depth, 8),
    DEFINE_PROP_END_OF_LIST(),
};

static void macfb_sysbus_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    SysBusDeviceClass *k = SYS_BUS_DEVICE_CLASS(klass);

    k->init = macfb_sysbus_init;
    dc->desc = "SysBus Macintosh framebuffer";
    dc->reset = macfb_sysbus_reset;
    dc->vmsd = &vmstate_macfb;
    dc->props = macfb_sysbus_properties;
}

static void macfb_nubus_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    NubusDeviceClass *k = NUBUS_DEVICE_CLASS(klass);

    k->init = macfb_nubus_init;
    dc->desc = "Nubus Macintosh framebuffer";
    dc->reset = macfb_nubus_reset;
    dc->vmsd = &vmstate_macfb;
    dc->props = macfb_nubus_properties;
}

static TypeInfo macfb_sysbus_info = {
    .name          = TYPE_MACFB,
    .parent        = TYPE_SYS_BUS_DEVICE,
    .instance_size = sizeof(MacfbSysBusState),
    .class_init    = macfb_sysbus_class_init,
};

static TypeInfo macfb_nubus_info = {
    .name          = "nubus-macfb",
    .parent        = TYPE_NUBUS_DEVICE,
    .instance_size = sizeof(MacfbNubusState),
    .class_init    = macfb_nubus_class_init,
};

static void macfb_register_types(void)
{
    type_register_static(&macfb_sysbus_info);
    type_register_static(&macfb_nubus_info);
}

type_init(macfb_register_types)
