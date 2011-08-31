/*
 * QEMU m68k Macintosh VIA device support
 *
 * Copyright (c) 2011 Laurent Vivier
 *
 * Some parts from hw/cuda.c
 *
 * Copyright (c) 2004-2007 Fabrice Bellard
 * Copyright (c) 2007 Jocelyn Mayer
 *
 * some parts from linux-2.6.29, arch/m68k/include/asm/mac_via.h
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
#include "qemu-timer.h"
#include "mac_via.h"

/* debug VIA */
#if 0
#define DEBUG_VIA
#endif

#ifdef DEBUG_VIA
#define VIA_DPRINTF(fmt, ...)                                  \
    do { printf("VIA%d: " fmt , s->type, ## __VA_ARGS__); } while (0)
#else
#define VIA_DPRINTF(fmt, ...)
#endif

/*
 * Base addresses for the VIAs. There are two in every machine,
 */

#define VIA1_BASE (0x50F00000)
#define VIA2_BASE (0x50F02000)
#define VIA_SIZE  (0x00001FFF)

/*
 * Not all of these are true post MacII I think.
 * CSA: probably the ones CHRP marks as 'unused' change purposes
 * when the IWM becomes the SWIM.
 * http://www.rs6000.ibm.com/resource/technology/chrpio/via5.mak.html
 * ftp://ftp.austin.ibm.com/pub/technology/spec/chrp/inwork/CHRP_IORef_1.0.pdf
 *
 * also, http://developer.apple.com/technotes/hw/hw_09.html claims the
 * following changes for IIfx:
 * VIA1A_vSccWrReq not available and that VIA1A_vSync has moved to an IOP.
 * Also, "All of the functionality of VIA2 has been moved to other chips".
 */

#define VIA1A_vSccWrReq 0x80   /* SCC write. (input)
                                * [CHRP] SCC WREQ: Reflects the state of the
                                * Wait/Request pins from the SCC.
                                * [Macintosh Family Hardware]
                                * as CHRP on SE/30,II,IIx,IIcx,IIci.
                                * on IIfx, "0 means an active request"
                                */
#define VIA1A_vRev8     0x40   /* Revision 8 board ???
                                * [CHRP] En WaitReqB: Lets the WaitReq_L
                                * signal from port B of the SCC appear on
                                * the PA7 input pin. Output.
                                * [Macintosh Family] On the SE/30, this
                                * is the bit to flip screen buffers.
                                * 0=alternate, 1=main.
                                * on II,IIx,IIcx,IIci,IIfx this is a bit
                                * for Rev ID. 0=II,IIx, 1=IIcx,IIci,IIfx
                                */
#define VIA1A_vHeadSel  0x20   /* Head select for IWM.
                                * [CHRP] unused.
                                * [Macintosh Family] "Floppy disk
                                * state-control line SEL" on all but IIfx
                                */
#define VIA1A_vOverlay  0x10   /* [Macintosh Family] On SE/30,II,IIx,IIcx
                                * this bit enables the "Overlay" address
                                * map in the address decoders as it is on
                                * reset for mapping the ROM over the reset
                                * vector. 1=use overlay map.
                                * On the IIci,IIfx it is another bit of the
                                * CPU ID: 0=normal IIci, 1=IIci with parity
                                * feature or IIfx.
                                * [CHRP] En WaitReqA: Lets the WaitReq_L
                                * signal from port A of the SCC appear
                                * on the PA7 input pin (CHRP). Output.
                                * [MkLinux] "Drive Select"
                                *  (with 0x20 being 'disk head select')
                                */
#define VIA1A_vSync     0x08   /* [CHRP] Sync Modem: modem clock select:
                                * 1: select the external serial clock to
                                *    drive the SCC's /RTxCA pin.
                                * 0: Select the 3.6864MHz clock to drive
                                *    the SCC cell.
                                * [Macintosh Family] Correct on all but IIfx
                                */

/* Macintosh Family Hardware sez: bits 0-2 of VIA1A are volume control
 * on Macs which had the PWM sound hardware.  Reserved on newer models.
 * On IIci,IIfx, bits 1-2 are the rest of the CPU ID:
 * bit 2: 1=IIci, 0=IIfx
 * bit 1: 1 on both IIci and IIfx.
 * MkLinux sez bit 0 is 'burnin flag' in this case.
 * CHRP sez: VIA1A bits 0-2 and 5 are 'unused': if programmed as
 * inputs, these bits will read 0.
 */
#define VIA1A_vVolume   0x07    /* Audio volume mask for PWM */
#define VIA1A_CPUID0    0x02    /* CPU id bit 0 on RBV, others */
#define VIA1A_CPUID1    0x04    /* CPU id bit 0 on RBV, others */
#define VIA1A_CPUID2    0x10    /* CPU id bit 0 on RBV, others */
#define VIA1A_CPUID3    0x40    /* CPU id bit 0 on RBV, others */

/* Info on VIA1B is from Macintosh Family Hardware & MkLinux.
 * CHRP offers no info. */
#define VIA1B_vSound   0x80    /* Sound enable (for compatibility with
                                * PWM hardware) 0=enabled.
                                * Also, on IIci w/parity, shows parity error
                                * 0=error, 1=OK. */
#define VIA1B_vMystery 0x40    /* On IIci, parity enable. 0=enabled,1=disabled
                                * On SE/30, vertical sync interrupt enable.
                                * 0=enabled. This vSync interrupt shows up
                                * as a slot $E interrupt. */
#define VIA1B_vADBS2   0x20    /* ADB state input bit 1 (unused on IIfx) */
#define VIA1B_vADBS1   0x10    /* ADB state input bit 0 (unused on IIfx) */
#define VIA1B_vADBInt  0x08    /* ADB interrupt 0=interrupt (unused on IIfx)*/
#define VIA1B_vRTCEnb  0x04    /* Enable Real time clock. 0=enabled. */
#define VIA1B_vRTCClk  0x02    /* Real time clock serial-clock line. */
#define VIA1B_vRTCData 0x01    /* Real time clock serial-data line. */

/*
 *    VIA2 A register is the interrupt lines raised off the nubus
 *    slots.
 *      The below info is from 'Macintosh Family Hardware.'
 *      MkLinux calls the 'IIci internal video IRQ' below the 'RBV slot 0 irq.'
 *      It also notes that the slot $9 IRQ is the 'Ethernet IRQ' and
 *      defines the 'Video IRQ' as 0x40 for the 'EVR' VIA work-alike.
 *      Perhaps OSS uses vRAM1 and vRAM2 for ADB.
 */

#define VIA2A_vRAM1    0x80    /* RAM size bit 1 (IIci: reserved) */
#define VIA2A_vRAM0    0x40    /* RAM size bit 0 (IIci: internal video IRQ) */
#define VIA2A_vIRQE    0x20    /* IRQ from slot $E */
#define VIA2A_vIRQD    0x10    /* IRQ from slot $D */
#define VIA2A_vIRQC    0x08    /* IRQ from slot $C */
#define VIA2A_vIRQB    0x04    /* IRQ from slot $B */
#define VIA2A_vIRQA    0x02    /* IRQ from slot $A */
#define VIA2A_vIRQ9    0x01    /* IRQ from slot $9 */

/* RAM size bits decoded as follows:
 * bit1 bit0  size of ICs in bank A
 *  0    0    256 kbit
 *  0    1    1 Mbit
 *  1    0    4 Mbit
 *  1    1   16 Mbit
 */

/*
 *    Register B has the fun stuff in it
 */

#define VIA2B_vVBL    0x80    /* VBL output to VIA1 (60.15Hz) driven by
                               * timer T1.
                               * on IIci, parity test: 0=test mode.
                               * [MkLinux] RBV_PARODD: 1=odd,0=even. */
#define VIA2B_vSndJck 0x40    /* External sound jack status.
                               * 0=plug is inserted.  On SE/30, always 0 */
#define VIA2B_vTfr0   0x20    /* Transfer mode bit 0 ack from NuBus */
#define VIA2B_vTfr1   0x10    /* Transfer mode bit 1 ack from NuBus */
#define VIA2B_vMode32 0x08    /* 24/32bit switch - doubles as cache flush
                               * on II, AMU/PMMU control.
                               *   if AMU, 0=24bit to 32bit translation
                               *   if PMMU, 1=PMMU is accessing page table.
                               * on SE/30 tied low.
                               * on IIx,IIcx,IIfx, unused.
                               * on IIci/RBV, cache control. 0=flush cache.
                               */
#define VIA2B_vPower  0x04   /* Power off, 0=shut off power.
                              * on SE/30 this signal sent to PDS card.
                              */
#define VIA2B_vBusLk  0x02   /* Lock NuBus transactions, 0=locked.
                              * on SE/30 sent to PDS card.
                              */
#define VIA2B_vCDis   0x01   /* Cache control. On IIci, 1=disable cache card
                              * on others, 0=disable processor's instruction
                              * and data caches.
                              */

/* interrupt flags */

#define IRQ_SET         0x80

/* common */

#define VIA_IRQ_TIMER1      0x40
#define VIA_IRQ_TIMER2      0x20

/* VIA1 */

#define VIA1_IRQ_ADB_CLOCK  0x10
#define VIA1_IRQ_ADB_DATA   0x08
#define VIA1_IRQ_ADB_READY  0x04
#define VIA1_IRQ_VBLANK     0x02
#define VIA1_IRQ_ONE_SECOND 0x01

/* VIA2 */

#define VIA2_IRQ_ASC        0x10
#define VIA2_IRQ_SCSI       0x08
#define VIA2_IRQ_UNUSED     0x04
#define VIA2_IRQ_SLOT       0x02
#define VIA2_IRQ_SCSI_DATA  0x01

/* Apple sez: http://developer.apple.com/technotes/ov/ov_04.html
 * Another example of a valid function that has no ROM support is the use
 * of the alternate video page for page-flipping animation. Since there
 * is no ROM call to flip pages, it is necessary to go play with the
 * right bit in the VIA chip (6522 Versatile Interface Adapter).
 * [CSA: don't know which one this is, but it's one of 'em!]
 */

/*
 *    6522 registers - see databook.
 * CSA: Assignments for VIA1 confirmed from CHRP spec.
 */

/* partial address decode.  0xYYXX : XX part for RBV, YY part for VIA */
/* Note: 15 VIA regs, 8 RBV regs */

#define vBufB    0x0000  /* [VIA/RBV]  Register B */
#define vBufAH   0x0200  /* [VIA only] Buffer A, with handshake. DON'T USE! */
#define vDirB    0x0400  /* [VIA only] Data Direction Register B. */
#define vDirA    0x0600  /* [VIA only] Data Direction Register A. */
#define vT1CL    0x0800  /* [VIA only] Timer one counter low. */
#define vT1CH    0x0a00  /* [VIA only] Timer one counter high. */
#define vT1LL    0x0c00  /* [VIA only] Timer one latches low. */
#define vT1LH    0x0e00  /* [VIA only] Timer one latches high. */
#define vT2CL    0x1000  /* [VIA only] Timer two counter low. */
#define vT2CH    0x1200  /* [VIA only] Timer two counter high. */
#define vSR      0x1400  /* [VIA only] Shift register. */
#define vACR     0x1600  /* [VIA only] Auxilary control register. */
#define vPCR     0x1800  /* [VIA only] Peripheral control register. */
                         /*            CHRP sez never ever to *write* this.
                          *            Mac family says never to *change* this.
                          * In fact we need to initialize it once at start.
                          */
#define vIFR     0x1a00  /* [VIA/RBV]  Interrupt flag register. */
#define vIER     0x1c00  /* [VIA/RBV]  Interrupt enable register. */
#define vBufA    0x1e00  /* [VIA/RBV] register A (no handshake) */

typedef struct VIATimer {
    int index;
    uint16_t counter; /* Timer counter */
    uint16_t latch;   /* Timer latch */
    int64_t next_irq_time;
    QEMUTimer *timer;
} VIATimer;

typedef struct VIAState {
    int type; /* is VIA1 (1) or VIA2 (2) */
    /* VIA registers */
    uint8_t a;    /* data register A */
    uint8_t b;    /* data register B */
    uint8_t dira; /* data direction register A (1 = output) */
    uint8_t dirb; /* data direction register B (1 = output) */
    uint8_t pcr;  /* peripheral control register */
    uint8_t acr;  /* auxiliary control register */
    uint8_t ifr;  /* interrupt flag register */
    uint8_t ier;  /* interrupt enable register */
    uint8_t sr;   /* shift register */

    uint32_t tick_offset;

    uint8_t last_b;
    uint8_t data_out;
    int data_out_cnt;
    uint8_t data_in;
    uint8_t data_in_cnt;
    uint8_t cmd;
    int wprotect;
    int alt;

    VIATimer timers[2];

    qemu_irq irq;
} VIAState;

static VIAState via_state[2];

QEMUTimer *one_second_timer;
QEMUTimer *VBL_timer;

#define VIA_TIMER_FREQ (783360)

static int64_t get_next_irq_time(VIATimer *s, int64_t current_time)
{
    int64_t d, next_time;

    /* current counter value */
    d = muldiv64(current_time, VIA_TIMER_FREQ, get_ticks_per_sec());
    next_time = d + s->counter;
    next_time = muldiv64(next_time, get_ticks_per_sec(), VIA_TIMER_FREQ);
    if (next_time <= current_time) {
        next_time = current_time + 1;
    }
    return next_time;
}

#define T1MODE      0xc0
#define T1MODE_CONT 0x40

static void via_arm_timer(VIATimer *ti, int64_t current_time)
{
    if (!ti->timer) {
        return;
    }
    ti->next_irq_time = get_next_irq_time(ti, current_time);
    qemu_mod_timer(ti->timer, ti->next_irq_time);
}

static void via_timer_update(VIAState *s, VIATimer *ti,
                             int64_t current_time)
{
    if (!ti->timer) {
        return;
    }
    if (!(s->ier & VIA_IRQ_TIMER1) &&
        (s->acr & T1MODE) != T1MODE_CONT) {
        qemu_del_timer(ti->timer);
    } else {
        ti->counter = ti->latch;
        via_arm_timer(ti, current_time);
    }
}

static void via_update_irq(VIAState *s)
{
    if (s->ifr & s->ier) {
        qemu_irq_raise(s->irq);
    } else {
        qemu_irq_lower(s->irq);
    }
}

static void via_timer1(void *opaque)
{
    VIAState *s = opaque;
    VIATimer *ti = &s->timers[0];

    via_timer_update(s, ti, ti->next_irq_time);
    s->ifr |= VIA_IRQ_TIMER1;
    via_update_irq(s);
}

static void via1_VBL_update(VIAState *s)
{
    if (s->ifr & s->ier & VIA1_IRQ_VBLANK) { /* 60 Hz irq */
        qemu_mod_timer(VBL_timer,
                       (qemu_get_clock_ns(vm_clock) + 16630) / 16630 * 16630);
    } else {
        qemu_del_timer(VBL_timer);
    }
}

static void via1_VBL(void *opaque)
{
    VIAState *s = opaque;
    via1_VBL_update(s);
    s->ifr |= VIA1_IRQ_VBLANK;
    via_update_irq(s);
}

static void via1_one_second_update(VIAState *s)
{
    if (s->ifr & s->ier & VIA1_IRQ_ONE_SECOND) {
        qemu_mod_timer(one_second_timer,
                      (qemu_get_clock_ms(vm_clock) + 1000) / 1000 * 1000);
    } else {
        qemu_del_timer(one_second_timer);
    }
}

static void via1_one_second(void *opaque)
{
    VIAState *s = opaque;
    via1_one_second_update(s);
    s->ifr |= VIA1_IRQ_ONE_SECOND;
    via_update_irq(s);
}

static void via_irq_update(VIAState *s)
{
    switch (s->type) {
    case 1:
        via1_one_second_update(s);
        via1_VBL(s);
        break;
    case 2:
        break;
    }
}

#define RTC_OFFSET 2082844800
static uint8_t PRAM[256];

static void via1_rtc_update(VIAState *s)
{
    if (s->b & VIA1B_vRTCEnb) {
        return;
    }

    if (s->dirb & VIA1B_vRTCData) {
        /* send bits to the RTC */
        if (!(s->last_b & VIA1B_vRTCClk) && (s->b & VIA1B_vRTCClk)) {
            s->data_out <<= 1;
            s->data_out |= s->b & VIA1B_vRTCData;
            s->data_out_cnt++;
        }
    } else {
        /* receive bits from the RTC */
        if ((s->last_b & VIA1B_vRTCClk) &&
            !(s->b & VIA1B_vRTCClk) &&
            s->data_in_cnt) {
            s->b = (s->b & ~VIA1B_vRTCData) |
                   ((s->data_in >> 8) & VIA1B_vRTCData);
            s->data_in <<= 1;
            s->data_in_cnt--;
        }
    }

    if (s->data_out_cnt == 8) {
        s->data_out_cnt = 0;

        if (s->cmd == 0) {
            if (s->data_out & 0x80) {
                /* this is a read command */
                uint32_t time = s->tick_offset +
                               (qemu_get_clock_ns(vm_clock) /
                               get_ticks_per_sec());
                if (s->data_out == 0x81) {        /* seconds register 0 */
                    s->data_in = time & 0xff;
                    s->data_in_cnt = 8;
                } else if (s->data_out == 0x85) { /* seconds register 1 */
                    s->data_in = (time >> 8) & 0xff;
                    s->data_in_cnt = 8;
                } else if (s->data_out == 0x89) { /* seconds register 2 */
                    s->data_in = (time >> 16) & 0xff;
                    s->data_in_cnt = 8;
                } else if (s->data_out == 0x8d) { /* seconds register 3 */
                    s->data_in = (time >> 24) & 0xff;
                    s->data_in_cnt = 8;
                } else if ((s->data_out & 0xf3) == 0xa1) {
                    /* PRAM address 0x10 -> 0x13 */
                    int addr = (s->data_out >> 2) & 0x03;
                    s->data_in = PRAM[addr];
                    s->data_in_cnt = 8;
                } else if ((s->data_out & 0xf3) == 0xa1) {
                    /* PRAM address 0x00 -> 0x0f */
                    int addr = (s->data_out >> 2) & 0x0f;
                    s->data_in = PRAM[addr];
                    s->data_in_cnt = 8;
                } else if ((s->data_out & 0xf8) == 0xb8) {
                    /* extended memory designator and sector number */
                    s->cmd = s->data_out;
                }
            } else {
                /* this is a write command */
                s->cmd = s->data_out;
            }
        } else {
            if (s->cmd & 0x80) {
                if ((s->cmd & 0xf8) == 0xb8) {
                    /* extended memory designator and sector number */
                    int sector = s->cmd & 0x07;
                    int addr = (s->data_out >> 2) & 0x1f;

                    s->data_in = PRAM[sector * 8 + addr];
                    s->data_in_cnt = 8;
                }
            } else if (!s->wprotect) {
                /* this is a write command */
                if (s->alt != 0) {
                    /* extended memory designator and sector number */
                    int sector = s->cmd & 0x07;
                    int addr = (s->alt >> 2) & 0x1f;

                    PRAM[sector * 8 + addr] = s->data_out;

                    s->alt = 0;
                } else if (s->cmd == 0x01) { /* seconds register 0 */
                    /* FIXME */
                } else if (s->cmd == 0x05) { /* seconds register 1 */
                    /* FIXME */
                } else if (s->cmd == 0x09) { /* seconds register 2 */
                    /* FIXME */
                } else if (s->cmd == 0x0d) { /* seconds register 3 */
                    /* FIXME */
                } else if (s->cmd == 0x31) {
                    /* Test Register */
                } else if (s->cmd == 0x35) {
                    /* Write Protect register */
                    s->wprotect = s->data_out & 1;
                } else if ((s->cmd & 0xf3) == 0xa1) {
                    /* PRAM address 0x10 -> 0x13 */
                    int addr = (s->cmd >> 2) & 0x03;
                    PRAM[addr] = s->data_out;
                } else if ((s->cmd & 0xf3) == 0xa1) {
                    /* PRAM address 0x00 -> 0x0f */
                    int addr = (s->cmd >> 2) & 0x0f;
                    PRAM[addr] = s->data_out;
                } else if ((s->cmd & 0xf8) == 0xb8) {
                    /* extended memory designator and sector number */
                    s->alt = s->cmd;
                }
            }
        }
        s->data_out = 0;
    }
}

static void via_writeb(void *opaque, target_phys_addr_t addr, uint32_t val)
{
    VIAState *s = opaque;

    addr &= VIA_SIZE;

    switch (addr) {
    case vBufA: /* Buffer A */
        VIA_DPRINTF("writeb: vBufA = %02x\n", val);
        s->a = (s->a & ~s->dira) | (val & s->dira);
        break;
    case vBufB:  /* Register B */
        VIA_DPRINTF("writeb: vBufB = %02x\n", val);
        s->b = (s->b & ~s->dirb) | (val & s->dirb);
        if (s->type == 1) {
            via1_rtc_update(s);
            s->last_b = s->b;
        }
        break;
    case vDirA:  /* Data Direction Register A. */
        VIA_DPRINTF("writeb: vDirA = %02x\n", val);
        s->dira = val;
        break;
    case vDirB:  /* Data Direction Register B. */
        VIA_DPRINTF("writeb: vDirB = %02x\n", val);
        s->dirb = val;
        break;
    case vT1CL:  /* Timer one counter low. */
        VIA_DPRINTF("writeb: vT1CL = %02x\n", val);
        s->timers[0].counter = (s->timers[0].counter & 0xff00) | val;
        break;
    case vT1CH:  /* Timer one counter high. */
        VIA_DPRINTF("writeb: vT1CH = %02x\n", val);
        s->timers[0].counter = (s->timers[0].counter & 0x00ff) | (val << 8);
        via_arm_timer(&s->timers[0], qemu_get_clock_ns(vm_clock));
        break;
    case vT1LL:  /* Timer one latches low. */
        VIA_DPRINTF("writeb: vT1LL = %02x\n", val);
        s->timers[0].latch = (s->timers[0].latch & 0xff00) | val;
        break;
    case vT1LH:  /* Timer one latches high. */
        VIA_DPRINTF("writeb: vT1LH = %02x\n", val);
        s->timers[0].latch = (s->timers[0].latch & 0x00ff) | (val << 8);
        via_arm_timer(&s->timers[0], qemu_get_clock_ns(vm_clock));
        break;
    case vT2CL:  /* Timer two counter low. */
        VIA_DPRINTF("writeb: vT2CL = %02x\n", val);
        s->timers[1].counter = (s->timers[1].counter & 0xff00) | val;
        break;
    case vT2CH:  /* Timer two counter high. */
        VIA_DPRINTF("writeb: vT2CH = %02x\n", val);
        s->timers[1].counter = (s->timers[1].counter & 0x00ff) | (val << 8);
        via_arm_timer(&s->timers[1], qemu_get_clock_ns(vm_clock));
        break;
    case vSR:    /* Shift register. */
        VIA_DPRINTF("writeb: vSR = %02x\n", val);
        s->sr = val;
        break;
    case vACR:   /* Auxilary control register. */
        VIA_DPRINTF("writeb: vACR = %02x\n", val);
        s->acr = val;
        via_timer_update(s, &s->timers[0], qemu_get_clock_ns(vm_clock));
        break;
    case vPCR:   /* Peripheral control register. */
        VIA_DPRINTF("writeb: vPCR = %02x\n", val);
        s->pcr = val;
        break;
    case vIFR:   /* Interrupt flag register. */
        VIA_DPRINTF("writeb: vIFR = %02x\n", val);
        if (val & IRQ_SET) {
            /* set bits */
            s->ifr |= val & 0x7f;
        } else {
            /* clear bits */
            s->ifr &= ~val;
        }
        VIA_DPRINTF("            -> %02x\n", s->ifr);
        via_timer_update(s, &s->timers[0], qemu_get_clock_ns(vm_clock));
        via_irq_update(s);
        break;
    case vIER:   /* Interrupt enable register. */
        VIA_DPRINTF("writeb: vIER = %02x\n", val);
        if (val & IRQ_SET) {
            /* set bits */
            s->ier |= val & 0x7f;
        } else {
            /* clear bits */
            s->ier &= ~val;
        }
        VIA_DPRINTF("            -> %02x\n", s->ier);
        via_timer_update(s, &s->timers[0], qemu_get_clock_ns(vm_clock));
        via_irq_update(s);
        break;
    default:
        VIA_DPRINTF("writeb: addr 0x%08x val %02x\n", addr, val);
        break;
    }
}

static uint32_t via_readb(void *opaque, target_phys_addr_t addr)
{
    VIAState *s = opaque;
    uint32_t val;

    addr &= VIA_SIZE;

    switch (addr) {
    case vBufA: /* Buffer A */
        val = s->a;
        VIA_DPRINTF("readb:  vBufA = %02x\n", val);
        break;
    case vBufB:  /* Register B */
        val = s->b;
        VIA_DPRINTF("readb:  vBufB = %02x\n", val);
        break;
    case vDirA:  /* Data Direction Register A. */
        val = s->dira;
        VIA_DPRINTF("readb:  vDirA = %02x\n", val);
        break;
    case vDirB:  /* Data Direction Register B. */
        val = s->dirb;
        VIA_DPRINTF("readb:  vDirB = %02x\n", val);
        break;
    case vT1CL:  /* Timer one counter low. */
        val = s->timers[0].counter & 0x00ff;
        VIA_DPRINTF("readb:  vT1CL = %02x\n", val);
        break;
    case vT1CH:  /* Timer one counter high. */
        val = (s->timers[0].counter >> 8) & 0x00ff;
        VIA_DPRINTF("readb:  vT1CH = %02x\n", val);
        break;
    case vT1LL:  /* Timer one latches low. */
        val = s->timers[0].latch & 0x00ff;
        VIA_DPRINTF("readb:  vT1LL = %02x\n", val);
        break;
    case vT1LH:  /* Timer one latches high. */
        val = (s->timers[0].latch >> 8) & 0x00ff;
        VIA_DPRINTF("readb:  vT1LH = %02x\n", val);
        break;
    case vT2CL:  /* Timer two counter low. */
        val = s->timers[1].counter & 0x00ff;
        VIA_DPRINTF("readb:  vT2CL = %02x\n", val);
        break;
    case vT2CH:  /* Timer two counter high. */
        val = (s->timers[1].counter >> 8) & 0x00ff;
        VIA_DPRINTF("readb:  vT2CH = %02x\n", val);
        break;
    case vSR:    /* Shift register. */
        val = s->sr;
        VIA_DPRINTF("readb:  vSR = %02x\n", val);
        break;
    case vACR:   /* Auxilary control register. */
        val = s->acr;
        VIA_DPRINTF("readb:  vACR = %02x\n", val);
        break;
    case vPCR:   /* Peripheral control register. */
        val = s->pcr;
        VIA_DPRINTF("readb:  vPCR = %02x\n", val);
        break;
    case vIFR:   /* Interrupt flag register. */
        val = s->ifr | ((s->ifr & 0x7f) ? IRQ_SET : 0);
        VIA_DPRINTF("readb:  vIFR = %02x\n", val);
        break;
    case vIER:   /* Interrupt enable register. */
        val = s->ier | IRQ_SET;
        VIA_DPRINTF("readb:  vIER = %02x\n", val);
        break;
    default:
        val = 0;
        VIA_DPRINTF("readb:  addr 0x%08x val ??\n", addr);
        break;
    }
    return val;
}

static void via_writew(void *opaque, target_phys_addr_t addr, uint32_t val)
{
}

static void via_writel(void *opaque, target_phys_addr_t addr, uint32_t val)
{
}

static uint32_t via_readw(void *opaque, target_phys_addr_t addr)
{
    return 0;
}

static uint32_t via_readl(void *opaque, target_phys_addr_t addr)
{
    return 0;
}

static CPUWriteMemoryFunc * const via_write[] = {
    &via_writeb,
    &via_writew,
    &via_writel,
};

static CPUReadMemoryFunc * const via_read[] = {
    &via_readb,
    &via_readw,
    &via_readl,
};

static void via_reset(void *opaque)
{
    /* FIXME */
}

void mac_via_init(qemu_irq via1_irq, qemu_irq via2_irq)
{
    struct tm tm;
    int via_mem_index[2];
    VIAState *s;

    s = &via_state[0];

    one_second_timer = qemu_new_timer_ms(vm_clock, via1_one_second, s);
    VBL_timer = qemu_new_timer_ns(vm_clock, via1_VBL, s);

    qemu_get_timedate(&tm, 0);
    s->tick_offset = (uint32_t)mktimegm(&tm) + RTC_OFFSET;

    s->type = 1;
    s->irq = via1_irq;
    s->timers[0].index = 0;
    s->timers[0].timer = qemu_new_timer_ns(vm_clock, via_timer1, s);
    s->timers[1].index = 1;
    via_mem_index[0] = cpu_register_io_memory(via_read, via_write,
                                              s, DEVICE_NATIVE_ENDIAN);
    s = &via_state[1];
    s->type = 2;
    s->irq = via1_irq;
    s->timers[0].index = 0;
    s->timers[0].timer = qemu_new_timer_ns(vm_clock, via_timer1, s);
    s->timers[1].index = 1;
    via_mem_index[1] = cpu_register_io_memory(via_read, via_write,
                                              s, DEVICE_NATIVE_ENDIAN);

    cpu_register_physical_memory(VIA1_BASE, VIA_SIZE, via_mem_index[0]);
    cpu_register_physical_memory(VIA2_BASE, VIA_SIZE, via_mem_index[1]);
    /* FIXME: vmstate_register() */
    qemu_register_reset(via_reset, &via_state[0]);
    qemu_register_reset(via_reset, &via_state[1]);
}
