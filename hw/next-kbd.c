/*
 * QEMU NeXT Keyboard/Mouse emulation
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

/* This is admittedly hackish, but works well enough for basic input
   Mouse support will be added once we can boot something that needs
	the mouse. */ 

#include "hw.h"
#include "console.h"
#include "next-kbd.h"

/* debug NeXT keyboard */
//#define DEBUG_KBD

#ifdef DEBUG_KBD
#define DPRINTF(fmt, ...)                                       \
    do { printf("KBD: " fmt , ## __VA_ARGS__); } while (0);
#else
#define DPRINTF(fmt, ...)
#endif
/* follwoing defintions from next68k netbsd */
#define CSR_INT 0x00800000
#define CSR_DATA 0x00400000

#define KD_KEYMASK              0x007f
#define KD_DIRECTION            0x0080 /* pressed or released */
#define KD_CNTL                 0x0100
#define KD_LSHIFT               0x0200
#define KD_RSHIFT               0x0400
#define KD_LCOMM                0x0800
#define KD_RCOMM                0x1000
#define KD_LALT                 0x2000
#define KD_RALT                 0x4000
#define KD_VALID                0x8000 /* only set for scancode keys ? */
#define KD_MODS                 0x4f00

#define KBD_QUEUE_SIZE 256

typedef struct {
    uint8_t data[KBD_QUEUE_SIZE];
    int rptr, wptr, count;
} KBDQueue;


typedef struct KBDState {
    KBDQueue queue;
    uint8_t blank;
    int shift;//make this an unsigned short, and set it to the modifier value

} KBDState;
KBDState *kbd_env;
static void nextkbd_event(void *opaque, int ch);

static void queue_code(void *opaque, int code);
static uint32_t kbd_read_byte(void *opaque, target_phys_addr_t addr);
static uint32_t kbd_read_word(void *opaque, target_phys_addr_t addr);
static uint32_t kbd_read_long(void *opaque, target_phys_addr_t addr);

static void kbd_write_byte(void *opaque, target_phys_addr_t addr, uint32_t val);
static void kbd_write_word(void *opaque, target_phys_addr_t addr, uint32_t val);
static void kbd_write_long(void *opaque, target_phys_addr_t addr, uint32_t val);

static CPUWriteMemoryFunc *kbd_write[3] = {
    kbd_write_byte,
    kbd_write_word,
    kbd_write_long
};
static CPUReadMemoryFunc *kbd_read[3] = {
    kbd_read_byte,
    kbd_read_word,
    kbd_read_long
};




void nextkbd_init(void *opaque)
{
	KBDState *s = qemu_mallocz(sizeof(KBDState));
    
	s->shift = 0;
	
	int kbd_addr = cpu_register_io_memory(kbd_read,kbd_write, (void *)s,DEVICE_NATIVE_ENDIAN);
	
	cpu_register_physical_memory((uint32_t)0x200e000,0x1000,kbd_addr);//from netbsd's cpu.h

    qemu_add_kbd_event_handler(nextkbd_event, s);
}

static uint32_t kbd_read_byte(void *opaque, target_phys_addr_t addr)
{
    addr = addr & 0xe003;
    switch(addr)
    {
    	
        case 0xe000:
		return 0x80|0x20;
		
        case 0xe001:
		return 0x80|0x40|0x20|0x10;
		
        case 0xe002:
		return 0x40|0x10|0x2|0x1;

        default:
        DPRINTF("RB ADDR %x\n",addr);
        return 0;
    }
    return 0;
}
static uint32_t kbd_read_word(void *opaque, target_phys_addr_t addr)
{
    DPRINTF("RW ADDR %x\n",addr);
    return 0;
}
static uint32_t kbd_read_long(void *opaque, target_phys_addr_t addr)
{
    int key = 0;
    KBDState *s = (KBDState *)opaque;
    KBDQueue *q = &s->queue;
    switch(addr & 0xe00f)
	{
    	case 0xe000:
	//	fprintf(stderr,"KB L Read: @ %X\n",((CPUM68KState *)opaque)->pc);
		return 0xA0F09300;
        
        case 0xe008:
        /* get keycode from buffer */
        if(q->count > 0)
        {
            
        key = q->data[q->rptr];
        if (++q->rptr == KBD_QUEUE_SIZE)
            q->rptr = 0;

            q->count--;
            
          
            if(s->shift)
            key |= KD_LSHIFT;
            if(key & 0x80)
            return 0;
            else
            return 0x10000000 | KD_VALID | key;
        }
        else
            return 0;
    //    return 0x10009026;		
        
        default:
        DPRINTF("RL ADDR %x\n",addr);
        return 0;
    }
}

static void kbd_write_byte(void *opaque, target_phys_addr_t addr, uint32_t val)
{
    //DPRINTF("WB ADDR %x\n",addr);
}
static void kbd_write_word(void *opaque, target_phys_addr_t addr, uint32_t val)
{
    DPRINTF("WW ADDR %x\n",addr);
}
static void kbd_write_long(void *opaque, target_phys_addr_t addr, uint32_t val)
{
   // DPRINTF("WL ADDR %x\n",addr);
}


static void nextkbd_event(void *opaque, int ch)
{
    /*will want to set vars for caps/num lock*/
    /*if (ch & 0x80) -> key release */
   /* there's also e0 escaped scancodes that might need to be handled */ 
     DPRINTF("EVENT %X\n",ch);
 
    queue_code(opaque, ch);
   
}

static const unsigned char next_keycodes[128] = {
  0,    0x49, 0x4A,  0x4B,  0x4C,  0x4D,  0x50,  0x4F,  0x4E,  0x1E,  0x1F,  0x20,  0x1D,  0x1C, 0x1B,  0,
  0x42, 0x43, 0x44,  0x45,  0x48,  0x47,  0x46,  0x06,  0x07,  0x08,  0x0,  0,  0x2A,  0, 0x39,  0x3A,
  0x3B,  0x3C,  0x3D,  0x40,  0x3F,  0x3E,  0x2D,  0x2C,  0x2B,  0x26,  0,  0,  0x31,  0x32, 0x33,  0x34,
  0x35,  0x37,  0x36,  0x2e,  0x2f,  0x30,  0,  0,  0,  0x38,  0,  0,  0,  0, 0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0
};

static void queue_code(void *opaque, int code)
{

    KBDState *s = (KBDState *)opaque;
    KBDQueue *q = &s->queue;
    int key = code & 0x7F;
    int release = code & 0x80;
      if(code == 0x2A)
            {
                s->shift = 1;
                return;
            }
            else if(code == (0x2A | 0x80))
            {
                s->shift = 0;
                return;
            }




    if (q->count >= KBD_QUEUE_SIZE)
        return;
    
    q->data[q->wptr] = next_keycodes[key] | release;
    if (++q->wptr == KBD_QUEUE_SIZE)
        q->wptr = 0;
    q->count++;
    //s->update_irq(s->update_arg, 1);
    s->blank += 1;
}
