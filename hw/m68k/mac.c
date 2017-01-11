/*
 * QEMU Motorla 680x0 Macintosh hardware System Emulator
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
#include "sysemu/sysemu.h"
#include "cpu.h"
#include "hw/hw.h"
#include "hw/boards.h"
#include "elf.h"
#include "hw/loader.h"
#include "hw/display/framebuffer.h"
#include "ui/console.h"
#include "exec/address-spaces.h"
#include "hw/char/escc.h"
#include "hw/sysbus.h"
#include "hw/scsi/esp.h"
#include "bootinfo.h"
#include "hw/misc/mac_via.h"
#include "hw/input/adb.h"
#include "hw/audio/asc.h"
#include "hw/nubus/mac.h"
#include "net/net.h"

#define MACROM_ADDR     0x40000000
#define MACROM_SIZE     0x00100000

/*
 *              .ident          = MAC_MODEL_Q800,
 *              .name           = "Quadra 800",
 *              .adb_type       = MAC_ADB_II,
 *              .via_type       = MAC_VIA_QUADRA,
 *              .scsi_type      = MAC_SCSI_QUADRA,
 *              .scc_type       = MAC_SCC_QUADRA,
 *              .ether_type     = MAC_ETHER_SONIC,
 *              .nubus_type     = MAC_NUBUS
 */

#define MACROM_FILENAME "MacROM.bin"

#define Q800_MACHINE_ID 35
#define Q800_CPU_ID (1<<2)
#define Q800_FPU_ID (1<<2)
#define Q800_MMU_ID (1<<2)

#define MACH_MAC        3
#define Q800_MAC_CPU_ID 2

#define VIA_BASE              0x50f00000
#define SONIC_PROM_BASE       0x50f08000
#define SONIC_BASE            0x50f0a000
#define SCC_BASE              0x50f0c020
#define ESP_BASE              0x50f10000
#define ESP_PDMA              0x50f10100
#define ASC_BASE              0x50F14000
#define SWIM_BASE             0x50F1E000
#define NUBUS_SUPER_SLOT_BASE 0x60000000
#define NUBUS_SLOT_BASE       0xf0000000

/* the video base, whereas it a Nubus address,
 * is needed by the kernel to have early display and
 * thus provided by the bootloader
 */
#define VIDEO_BASE            0xf9001000

#define MAC_CLOCK  3686418 //783300

typedef struct {
    M68kCPU *cpu;
    uint8_t ipr;
} q800_glue_state_t;

static void q800_glue_set_irq(void *opaque, int irq, int level)
{
    int i;

    q800_glue_state_t *s = opaque;

    if (level) {
        s->ipr |= 1 << irq;
    } else {
        s->ipr &= ~(1 << irq);
    }

    for (i = 7; i >= 0; i--) {
        if ((s->ipr >> i) & 1) {
            m68k_set_irq_level(s->cpu, i + 1, i + 25);
            return;
        }
    }
    m68k_set_irq_level(s->cpu, 0, 0);
}

static void main_cpu_reset(void *opaque)
{
    M68kCPU *cpu = opaque;
    CPUState *cs = CPU(cpu);

    cpu_reset(cs);
    cpu->env.aregs[7] = ldl_phys(cs->as, 0);
    cpu->env.pc = ldl_phys(cs->as, 4);
}

static void q800_init(MachineState *machine)
{
    M68kCPU *cpu = NULL;
    int linux_boot;
    int32_t kernel_size;
    uint64_t elf_entry;
    char *filename;
    int bios_size;
    ram_addr_t initrd_base;
    int32_t initrd_size;
    MemoryRegion *rom;
    MemoryRegion *ram;
    ram_addr_t ram_size = machine->ram_size;
    const char *kernel_filename = machine->kernel_filename;
    const char *initrd_filename = machine->initrd_filename;
    const char *kernel_cmdline = machine->kernel_cmdline;
    const char *cpu_model = machine->cpu_model;
    q800_glue_state_t *s;
    qemu_irq *pic;
    hwaddr parameters_base;
    CPUState *cs;
    DeviceState *dev;
    DeviceState *via_dev;
    SysBusDevice *sysbus;
    BusState *adb_bus;
    qemu_irq  esp_reset_irq, esp_dma_enable;
    NubusBus *nubus;
    NubusDevice *nubus_dev;
    DriveInfo *fds[2];

    linux_boot = (kernel_filename != NULL);

    /* RAM */

    ram = g_malloc(sizeof (*ram));
    memory_region_init_ram(ram, NULL, "m68k_mac.ram", ram_size, &error_abort);
    memory_region_add_subregion(get_system_memory(), 0, ram);

    /* ROM */

    rom = g_malloc(sizeof(*rom));
    memory_region_init_ram(rom, NULL, "m68k_mac.rom", MACROM_SIZE,
                           &error_abort);
    memory_region_set_readonly(rom, true);
    memory_region_add_subregion(get_system_memory(), MACROM_ADDR, rom);

    /* Load MacROM binary */

    if (bios_name == NULL) {
        bios_name = MACROM_FILENAME;
    }
    filename = qemu_find_file(QEMU_FILE_TYPE_BIOS, bios_name);

    if (filename) {
        bios_size = load_image_targphys(filename, MACROM_ADDR, MACROM_SIZE);
        g_free(filename);
    } else {
        bios_size = -1;
    }

    if (!linux_boot) {
        if (bios_size < 0 || bios_size > MACROM_SIZE) {
            hw_error("qemu: could not load MacROM '%s'\n", bios_name);
            exit(1);
        }
    }

    /* init CPUs */
    if (cpu_model == NULL) {
        cpu_model = "m68040";
    }
    cpu = cpu_m68k_init(cpu_model);
    if (!cpu) {
            hw_error("qemu: unable to find m68k CPU definition\n");
            exit(1);
    }
    qemu_register_reset(main_cpu_reset, cpu);

    /* Glue */

    s = (q800_glue_state_t *)g_malloc0(sizeof(q800_glue_state_t));
    s->cpu = cpu;
    pic = qemu_allocate_irqs(q800_glue_set_irq, s, 6);

    /* VIA */

    via_dev = qdev_create(NULL, TYPE_MAC_VIA);
    qdev_init_nofail(via_dev);
    sysbus = SYS_BUS_DEVICE(via_dev);
    sysbus_mmio_map(sysbus, 0, VIA_BASE);
    sysbus_connect_irq(sysbus, 0, pic[0]);
    sysbus_connect_irq(sysbus, 1, pic[1]);

    adb_bus = qdev_get_child_bus(via_dev, "adb.0");
    dev = qdev_create(adb_bus, TYPE_ADB_KEYBOARD);
    qdev_init_nofail(dev);
    dev = qdev_create(adb_bus, TYPE_ADB_MOUSE);
    qdev_init_nofail(dev);

    /* MACSONIC */

    if (nb_nics != 1) {
        hw_error("Q800 needs a dp83932 ethernet interfaces");
    }
    if (!nd_table[0].model) {
        nd_table[0].model = g_strdup("dp83932");
    }
    if (strcmp(nd_table[0].model, "dp83932") != 0) {
        hw_error("Q800 needs a dp83932 ethernet interfaces");
    } else {
        /* MacSonic driver needs an Apple MAC address
         * Valid prefix are:
         * 00:05:02 Apple
         * 00:80:19 Dayna Communications, Inc.
         * 00:A0:40 Apple
         * 08:00:07 Apple
         * (Q800 use the last one)
         */
        nd_table[0].macaddr.a[0] = 0x08;
        nd_table[0].macaddr.a[1] = 0x00;
        nd_table[0].macaddr.a[2] = 0x07;
    }
    qemu_check_nic_model(&nd_table[0], "dp83932");
    dev = qdev_create(NULL, "dp8393x");
    qdev_set_nic_properties(dev, &nd_table[0]);
    qdev_prop_set_uint8(dev, "it_shift", 2);
    qdev_prop_set_bit(dev, "big_endian", true);
    qdev_prop_set_ptr(dev, "dma_mr", get_system_memory());
    qdev_init_nofail(dev);
    sysbus = SYS_BUS_DEVICE(dev);
    sysbus_mmio_map(sysbus, 0, SONIC_BASE);
    sysbus_mmio_map(sysbus, 1, SONIC_PROM_BASE);
    sysbus_connect_irq(sysbus, 0, pic[2]);

    /* SCC */

    dev = qdev_create(NULL, "escc");
    qdev_prop_set_uint32(dev, "disabled", 0);
    qdev_prop_set_uint32(dev, "frequency", MAC_CLOCK);
    qdev_prop_set_uint32(dev, "it_shift", 1);
    qdev_prop_set_uint32(dev, "reg_bit", 1);
    qdev_prop_set_chr(dev, "chrA", serial_hds[0]);
    qdev_prop_set_chr(dev, "chrB", serial_hds[1]);
    qdev_prop_set_uint32(dev, "chnBtype", 0);
    qdev_prop_set_uint32(dev, "chnAtype", 0);
    qdev_init_nofail(dev);
    sysbus = SYS_BUS_DEVICE(dev);
    sysbus_connect_irq(sysbus, 0, pic[3]);
    sysbus_connect_irq(sysbus, 1, pic[3]);
    sysbus_mmio_map(sysbus, 0, SCC_BASE);

    /* SCSI */

    esp_init_pdma(ESP_BASE, 4, ESP_PDMA,
                  qdev_get_gpio_in(via_dev, VIA2_IRQ_SCSI_BIT),
                  qdev_get_gpio_in(via_dev, VIA2_IRQ_SCSI_DATA_BIT),
                  &esp_reset_irq, &esp_dma_enable);

    /* Apple Sound Chip */

    dev = qdev_create(NULL, "apple-sound-chip");
    qdev_prop_set_uint8(dev, "asctype", ASC_TYPE_ASC);
    qdev_init_nofail(dev);
    sysbus_mmio_map(SYS_BUS_DEVICE(dev), 0, ASC_BASE);
    sysbus_connect_irq(SYS_BUS_DEVICE(dev), 0,
                       qdev_get_gpio_in(via_dev, VIA2_IRQ_ASC_BIT));

    /* SWIM floppy controller */

    if (drive_get_max_bus(IF_FLOPPY) >= 2) {
        fprintf(stderr, "qemu: too many floppy drives\n");
        exit(1);
    }
    fds[0] = drive_get(IF_FLOPPY, 0, 0);
    fds[1] = drive_get(IF_FLOPPY, 0, 1);

    dev = qdev_create(NULL, "sysbus-swim");
    if (fds[0]) {
        qdev_prop_set_drive(dev, "driveA", blk_by_legacy_dinfo(fds[0]),
                            &error_fatal);
    }
    if (fds[1]) {
        qdev_prop_set_drive(dev, "driveB", blk_by_legacy_dinfo(fds[1]),
                            &error_fatal);
    }
    qdev_init_nofail(dev);
    sysbus_mmio_map(SYS_BUS_DEVICE(dev), 0, SWIM_BASE);

    /* NuBus */

    nubus = nubus_mac_new(NUBUS_SUPER_SLOT_BASE, NUBUS_SLOT_BASE);

    /* framebuffer in nubus slot #9 */

    nubus_dev = nubus_create(nubus, "nubus-macfb");
    qdev_prop_set_uint32(&nubus_dev->qdev, "width", graphic_width);
    qdev_prop_set_uint32(&nubus_dev->qdev, "height", graphic_height);
    qdev_prop_set_uint8(&nubus_dev->qdev, "depth", graphic_depth);
    qdev_init_nofail(&nubus_dev->qdev);

    cs = CPU(cpu);
    if (linux_boot) {
        uint64_t high;
        kernel_size = load_elf(kernel_filename, NULL, NULL,
                               &elf_entry, NULL, &high, 1,
                               EM_68K, 0, 0);
        if (kernel_size < 0) {
            hw_error("qemu: could not load kernel '%s'\n",
                      kernel_filename);
            exit(1);
        }
        stl_phys(cs->as, 4, elf_entry); /* reset initial PC */
        parameters_base = (high + 1) & ~1;

        BOOTINFO1(cs->as, parameters_base, BI_MACHTYPE, MACH_MAC);
        BOOTINFO1(cs->as, parameters_base, BI_FPUTYPE, Q800_FPU_ID);
        BOOTINFO1(cs->as, parameters_base, BI_MMUTYPE, Q800_MMU_ID);
        BOOTINFO1(cs->as, parameters_base, BI_CPUTYPE, Q800_CPU_ID);
        BOOTINFO1(cs->as, parameters_base, BI_MAC_CPUID, Q800_MAC_CPU_ID);
        BOOTINFO1(cs->as, parameters_base, BI_MAC_MODEL, Q800_MACHINE_ID);
        BOOTINFO1(cs->as, parameters_base,
                  BI_MAC_MEMSIZE, ram_size >> 20); /* in MB */
        BOOTINFO2(cs->as, parameters_base, BI_MEMCHUNK, 0, ram_size);
        BOOTINFO1(cs->as, parameters_base, BI_MAC_VADDR, VIDEO_BASE);
        BOOTINFO1(cs->as, parameters_base, BI_MAC_VDEPTH, graphic_depth);
        BOOTINFO1(cs->as, parameters_base, BI_MAC_VDIM,
                  (graphic_height << 16) | graphic_width);
        BOOTINFO1(cs->as, parameters_base, BI_MAC_VROW,
                  (graphic_width * graphic_depth + 7) / 8);
        BOOTINFO1(cs->as, parameters_base, BI_MAC_SCCBASE, SCC_BASE);

        if (kernel_cmdline) {
            BOOTINFOSTR(cs->as, parameters_base, BI_COMMAND_LINE,
                        kernel_cmdline);
        }

        /* load initrd */
        if (initrd_filename) {
           initrd_size = get_image_size(initrd_filename);
            if (initrd_size < 0) {
                hw_error("qemu: could not load initial ram disk '%s'\n",
                         initrd_filename);
                exit(1);
            }

            initrd_base = (ram_size - initrd_size) & TARGET_PAGE_MASK;
            load_image_targphys(initrd_filename, initrd_base,
                                ram_size - initrd_base);
            BOOTINFO2(cs->as, parameters_base, BI_RAMDISK, initrd_base,
                      initrd_size);
        } else {
            initrd_base = 0;
            initrd_size = 0;
        }
        BOOTINFO0(cs->as, parameters_base, BI_LAST);
    } else {
        uint8_t *ptr;
        ptr = rom_ptr(MACROM_ADDR);
        stl_phys(cs->as, 0, ldl_p(ptr));    /* reset initial SP */
        stl_phys(cs->as, 4,
                 MACROM_ADDR + ldl_p(ptr + 4)); /* reset initial PC */
    }
}

static void q800_machine_init(MachineClass *mc)
{
    mc->desc = "Macintosh Quadra 800";
    mc->init = q800_init;
    mc->max_cpus = 1;
    mc->is_default = 1;
    mc->block_default_type = IF_SCSI;
}

DEFINE_MACHINE("q800", q800_machine_init)
