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
#include "bootinfo.h"
#include "hw/misc/mac_via.h"
#include "hw/input/adb.h"

#define MACROM_ADDR     0x800000
#define MACROM_SIZE     0x100000


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

#define VIA_BASE   0x50f00000
#define SCC_BASE   0x50f0c020
#define VIDEO_BASE 0xf9001000

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
    cpu_reset(CPU(cpu));
    cpu->env.aregs[7] = ldl_phys(0);
    cpu->env.pc = ldl_phys(4);
}

static void q800_init(QEMUMachineInitArgs *args)
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
    ram_addr_t ram_size = args->ram_size;
    const char *kernel_filename = args->kernel_filename;
    const char *initrd_filename = args->initrd_filename;
    const char *kernel_cmdline = args->kernel_cmdline;
    const char *cpu_model = args->cpu_model;
    q800_glue_state_t *s;
    qemu_irq *pic;
    hwaddr parameters_base;
    DeviceState *dev;
    SysBusDevice *sysbus;
    BusState *adb_bus;

    linux_boot = (kernel_filename != NULL);

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

    ram = g_malloc(sizeof (*ram));
    memory_region_init_ram(ram, NULL, "m68k_mac.ram", ram_size);
    memory_region_add_subregion(get_system_memory(), 0, ram);

    /* Glue */

    s = (q800_glue_state_t *)g_malloc0(sizeof(q800_glue_state_t));
    s->cpu = cpu;
    pic = qemu_allocate_irqs(q800_glue_set_irq, s, 6);

    /* VIA */

    dev = qdev_create(NULL, TYPE_MAC_VIA);
    qdev_init_nofail(dev);
    sysbus = SYS_BUS_DEVICE(dev);
    sysbus_mmio_map(sysbus, 0, VIA_BASE);
    sysbus_connect_irq(sysbus, 0, pic[0]);
    sysbus_connect_irq(sysbus, 1, pic[1]);

    adb_bus = qdev_get_child_bus(dev, "adb.0");
    dev = qdev_create(adb_bus, TYPE_ADB_KEYBOARD);
    qdev_init_nofail(dev);
    dev = qdev_create(adb_bus, TYPE_ADB_MOUSE);
    qdev_init_nofail(dev);

    /* SCC */

    escc_init(SCC_BASE, pic[3], pic[3], serial_hds[0],
              serial_hds[1], MAC_CLOCK, 0);

    if (linux_boot) {
        uint64_t high;
        kernel_size = load_elf(kernel_filename, NULL, NULL,
                               &elf_entry, NULL, &high, 1,
                               ELF_MACHINE, 0);
        if (kernel_size < 0) {
            hw_error("qemu: could not load kernel '%s'\n",
                      kernel_filename);
            exit(1);
        }
        stl_phys(4, elf_entry); /* reset initial PC */
        parameters_base = (high + 1) & ~1;
        
        BOOTINFO1(parameters_base, BI_MACHTYPE, MACH_MAC);
        BOOTINFO1(parameters_base, BI_FPUTYPE, Q800_FPU_ID);
        BOOTINFO1(parameters_base, BI_MMUTYPE, Q800_MMU_ID);
        BOOTINFO1(parameters_base, BI_CPUTYPE, Q800_CPU_ID);
        BOOTINFO1(parameters_base, BI_MAC_CPUID, Q800_MAC_CPU_ID);
        BOOTINFO1(parameters_base, BI_MAC_MODEL, Q800_MACHINE_ID);
        BOOTINFO1(parameters_base, BI_MAC_MEMSIZE, ram_size >> 20); /* in MB */
        BOOTINFO2(parameters_base, BI_MEMCHUNK, 0, ram_size);
        BOOTINFO1(parameters_base, BI_MAC_VADDR, VIDEO_BASE);
        BOOTINFO1(parameters_base, BI_MAC_VDEPTH, graphic_depth);
        BOOTINFO1(parameters_base, BI_MAC_VDIM, (480 << 16) | 640);
        BOOTINFO1(parameters_base, BI_MAC_VROW,
                     640 * ((graphic_depth  + 7) / 8));
        BOOTINFO1(parameters_base, BI_MAC_SCCBASE, SCC_BASE);

        if (kernel_cmdline) {
            BOOTINFOSTR(parameters_base, BI_COMMAND_LINE, kernel_cmdline);
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
            BOOTINFO2(parameters_base, BI_RAMDISK, initrd_base, initrd_size);
        } else {
            initrd_base = 0;
            initrd_size = 0;
        }
        BOOTINFO0(parameters_base, BI_LAST);
    } else {
        /* allocate and load BIOS */
        rom = g_malloc(sizeof(*rom));
        memory_region_init_ram(rom, NULL, "m68k_mac.rom", MACROM_SIZE);
        if (bios_name == NULL) {
            bios_name = MACROM_FILENAME;
        }
        filename = qemu_find_file(QEMU_FILE_TYPE_BIOS, bios_name);
        memory_region_set_readonly(rom, true);
        memory_region_add_subregion(get_system_memory(), MACROM_ADDR, rom);

        /* Load MacROM binary */
        if (filename) {
            bios_size = load_image_targphys(filename, MACROM_ADDR, MACROM_SIZE);
            g_free(filename);
        } else {
            bios_size = -1;
        }
        if (bios_size < 0 || bios_size > MACROM_SIZE) {
            hw_error("qemu: could not load MacROM '%s'\n", bios_name);
            exit(1);
        }
    }
}

static QEMUMachine q800_machine = {
    .name = "q800",
    .desc = "Macintosh Quadra 800",
    .init = q800_init,
    .max_cpus = 1,
    .is_default = 1,
};

static void q800_machine_init(void)
{
    qemu_register_machine(&q800_machine);
}

machine_init(q800_machine_init);
