/*
 * NeXT Cube/Staion Framebuffer Emulation
 *
 * Copyright (c) 2011 Bryce Lanham
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
#include "hw.h"
#include "loader.h"
#include "console.h"
#include "framebuffer.h"
#define BITS 8
#include "pixel_ops.h"
#include "next-fb.h"
/*
typedef struct NextVGAState {
    VGACommonState vga;
    target_phys_addr_t vram_base;
	};*/
//need a function to register the mm i/o for fb and to register vram

void nextfb_draw_line(void *opaque, uint8_t *d, const uint8_t *s, int width, int pitch)
{
    uint32_t pal[4] = {0xFFFFFFFF, 0xFFAAAAAA, 0xFF555555,0xFF000000};
    uint32_t*buf = qemu_malloc(1120*4);
    int i = 0;
    for(;i<280; i++)
    {
        int j=i*4;
        uint8_t src = s[i];
        buf[j+3]= pal[src & 0x3];
        src >>=2;
        buf[j+2]= pal[src & 0x3];
        src >>=2;
        buf[j+1]= pal[src & 0x3];
        src >>=2;
        buf[j+0]= pal[src & 0x3];
    }
	
    memcpy(d,buf,1120*4);
	free(buf);
}


static void nextfb_update(void * opaque)
{
	next_state_t *s = (next_state_t *)opaque;
	DisplaySurface *info = s->ds->surface;

	
	
	int dest_width = 4;
	int src_width;
	int first = 0;
	int last  = 0;
	src_width = s->cols = 1120;
	src_width = 288;
	dest_width = 1120;
  	dest_width = info->linesize;
	framebuffer_update_display(s->ds,
							0xB000000,1120,832,
							src_width,dest_width, 0,
							1,
							nextfb_draw_line,
							NULL,
							&first, &last);

	dpy_update(s->ds,0,0,1120,832);	
}

static void nextfb_invalidate(void *opaque)
{
}

void nextfb_init(next_state_t *s)
{
    s->ds = graphic_console_init(nextfb_update,nextfb_invalidate, NULL, NULL, s);
	qemu_console_resize(s->ds,1120,832);	
	s->cols = 1120;
	s->rows = 832;
	s->invalidate = 1;

    cpu_register_physical_memory(0xB000000, 0x1CB100, 
        qemu_ram_alloc(NULL,"next-vram.ram",0x1CB100) | IO_MEM_RAM);

}
