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

#include "sysemu.h"
#include "cpu.h"
#include "hw.h"
#include "boards.h"
#include "elf.h"
#include "loader.h"
#include "framebuffer.h"
#include "console.h"
#include "exec-memory.h"
#include "escc.h"
#include "mac_via.h"
#include "sysbus.h"

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
#define DAFB_BASE  0xf9800200

#define MAC_CLOCK  3686418 //783300

struct bi_record {
    uint16_t tag;        /* tag ID */
    uint16_t size;       /* size of record */
    uint32_t data[0];    /* data */
};

/* machine independent tags */

#define BI_LAST         0x0000 /* last record */
#define BI_MACHTYPE     0x0001 /* machine type (u_long) */
#define BI_CPUTYPE      0x0002 /* cpu type (u_long) */
#define BI_FPUTYPE      0x0003 /* fpu type (u_long) */
#define BI_MMUTYPE      0x0004 /* mmu type (u_long) */
#define BI_MEMCHUNK     0x0005 /* memory chunk address and size */
                               /* (struct mem_info) */
#define BI_RAMDISK      0x0006 /* ramdisk address and size */
                               /* (struct mem_info) */
#define BI_COMMAND_LINE 0x0007 /* kernel command line parameters */
                               /* (string) */

/*  Macintosh-specific tags (all u_long) */

#define BI_MAC_MODEL    0x8000  /* Mac Gestalt ID (model type) */
#define BI_MAC_VADDR    0x8001  /* Mac video base address */
#define BI_MAC_VDEPTH   0x8002  /* Mac video depth */
#define BI_MAC_VROW     0x8003  /* Mac video rowbytes */
#define BI_MAC_VDIM     0x8004  /* Mac video dimensions */
#define BI_MAC_VLOGICAL 0x8005  /* Mac video logical base */
#define BI_MAC_SCCBASE  0x8006  /* Mac SCC base address */
#define BI_MAC_BTIME    0x8007  /* Mac boot time */
#define BI_MAC_GMTBIAS  0x8008  /* Mac GMT timezone offset */
#define BI_MAC_MEMSIZE  0x8009  /* Mac RAM size (sanity check) */
#define BI_MAC_CPUID    0x800a  /* Mac CPU type (sanity check) */
#define BI_MAC_ROMBASE  0x800b  /* Mac system ROM base address */

/*  Macintosh hardware profile data */

#define BI_MAC_VIA1BASE 0x8010  /* Mac VIA1 base address (always present) */
#define BI_MAC_VIA2BASE 0x8011  /* Mac VIA2 base address (type varies) */
#define BI_MAC_VIA2TYPE 0x8012  /* Mac VIA2 type (VIA, RBV, OSS) */
#define BI_MAC_ADBTYPE  0x8013  /* Mac ADB interface type */
#define BI_MAC_ASCBASE  0x8014  /* Mac Apple Sound Chip base address */
#define BI_MAC_SCSI5380 0x8015  /* Mac NCR 5380 SCSI (base address, multi) */
#define BI_MAC_SCSIDMA  0x8016  /* Mac SCSI DMA (base address) */
#define BI_MAC_SCSI5396 0x8017  /* Mac NCR 53C96 SCSI (base address, multi) */
#define BI_MAC_IDETYPE  0x8018  /* Mac IDE interface type */
#define BI_MAC_IDEBASE  0x8019  /* Mac IDE interface base address */
#define BI_MAC_NUBUS    0x801a  /* Mac Nubus type (none, regular, pseudo) */
#define BI_MAC_SLOTMASK 0x801b  /* Mac Nubus slots present */
#define BI_MAC_SCCTYPE  0x801c  /* Mac SCC serial type (normal, IOP) */
#define BI_MAC_ETHTYPE  0x801d  /* Mac builtin ethernet type (Sonic, MACE */
#define BI_MAC_ETHBASE  0x801e  /* Mac builtin ethernet base address */
#define BI_MAC_PMU      0x801f  /* Mac power management / poweroff hardware */
#define BI_MAC_IOP_SWIM 0x8020  /* Mac SWIM floppy IOP */
#define BI_MAC_IOP_ADB  0x8021  /* Mac ADB IOP */

#define BOOTINFO0(base, id) \
    do { \
        stw_phys(base, id); \
        base += 2; \
        stw_phys(base, sizeof(struct bi_record)); \
        base += 2; \
    } while (0)

#define BOOTINFO1(base, id, value) \
    do { \
        stw_phys(base, id); \
        base += 2; \
        stw_phys(base, sizeof(struct bi_record) + 4); \
        base += 2; \
        stl_phys(base, value); \
        base += 4; \
    } while (0)

#define BOOTINFO2(base, id, value1, value2) \
    do { \
        stw_phys(base, id); \
        base += 2; \
        stw_phys(base, sizeof(struct bi_record) + 8); \
        base += 2; \
        stl_phys(base, value1); \
        base += 4; \
        stl_phys(base, value2); \
        base += 4; \
    } while (0)

#define BOOTINFOSTR(base, id, string) \
    do { \
        int i; \
        stw_phys(base, id); \
        base += 2; \
        stw_phys(base, (sizeof(struct bi_record) + strlen(string) + 2) & ~1); \
        base += 2; \
        for (i = 0; string[i]; i++) { \
            stb_phys(base++, string[i]); \
        } \
        stb_phys(base++, 0); \
        base = (parameters_base + 1) & ~1; \
    } while (0)

typedef struct {
    CPUState *env;
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
            m68k_set_irq_level(s->env, i + 1, i + 25);
            return;
        }
    }
    m68k_set_irq_level(s->env, 0, 0);
}

static void main_cpu_reset(void *opaque)
{
    CPUState *env = opaque;
    cpu_reset(env);
}

static void q800_init(ram_addr_t ram_size,
                       const char *boot_device,
                       const char *kernel_filename,
                       const char *kernel_cmdline,
                       const char *initrd_filename,
                       const char *cpu_model)
{
    CPUState *env = NULL;
    int linux_boot;
    int32_t kernel_size;
    uint64_t elf_entry;
    char *filename;
    int bios_size;
    ram_addr_t initrd_base;
    int32_t initrd_size;
    MemoryRegion *rom;
    MemoryRegion *ram;
    MemoryRegion *escc_mem;
    MemoryRegion *via_mem;
    q800_glue_state_t *s;
    qemu_irq *pic;
    target_phys_addr_t parameters_base;
    DeviceState *dev;
    SysBusDevice *sysbus;

    linux_boot = (kernel_filename != NULL);

    /* init CPUs */
    if (cpu_model == NULL) {
        cpu_model = "m68040";
    }
    env = cpu_init(cpu_model);
    if (!env) {
            hw_error("qemu: unable to find m68k CPU definition\n");
            exit(1);
    }
    qemu_register_reset((QEMUResetHandler *)&main_cpu_reset, env);

    ram = g_malloc(sizeof (*ram));
    memory_region_init_ram(ram, NULL, "m68k_mac.ram", ram_size);
    memory_region_add_subregion(get_system_memory(), 0, ram);

    /* Glue */

    s = (q800_glue_state_t *)g_malloc0(sizeof(q800_glue_state_t));
    s->env = env;
    pic = qemu_allocate_irqs(q800_glue_set_irq, s, 6);

    /* VIA */

    via_mem = mac_via_init(pic[0], pic[1]);
    memory_region_add_subregion(get_system_memory(), VIA_BASE, via_mem);

    /* SCC */

    escc_mem = escc_init(SCC_BASE, pic[3], pic[3], serial_hds[0],
                         serial_hds[1], MAC_CLOCK, 1, 1);

    /* framebuffer */

    dev = qdev_create(NULL, "macfb");
    qdev_init_nofail(dev);
    sysbus = sysbus_from_qdev(dev);
    sysbus_mmio_map(sysbus, 0, VIDEO_BASE);
    sysbus_mmio_map(sysbus, 1, DAFB_BASE);

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
        BOOTINFO1(parameters_base, BI_MAC_VDEPTH, 8);
        BOOTINFO1(parameters_base, BI_MAC_VDIM, (480 << 16) | 640);
        BOOTINFO1(parameters_base, BI_MAC_VROW,
                     640 * ((8  + 7) / 8));
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
