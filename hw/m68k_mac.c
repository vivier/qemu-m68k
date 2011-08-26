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
#include "escc.h"

#define MACROM_ADDR     0x800000
#define MACROM_SIZE     0x100000
#define MACROM_FILENAME "MacROM.bin"

#define Q800_MACHINE_ID 35
#define Q800_CPU_ID (1<<2)
#define Q800_FPU_ID (1<<2)
#define Q800_MMU_ID (1<<2)

#define MACH_MAC        3 

#define VIA1_BASE 0x50f00000
#define VIA2_BASE 0x50f02000
#define SCC_BASE  0x50f0c020
#define VIDEO_BASE 0xf9001000
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

typedef struct {
    DisplayState *ds;
} q800_state_t;

static q800_state_t q800_state;

static uint8_t palette[256 * 3];
static void q800fb_draw_line(void *opaque, uint8_t *d, const uint8_t *s,
                             int width, int pitch)
{
    int i, j;

   for (i = 0, j = 0; i < width * ((graphic_depth + 7) / 8); i++) {
        d[j++] = palette[s[i] * 3];
        d[j++] = palette[s[i] * 3 + 1];
        d[j++] = palette[s[i] * 3 + 2];
        j++;
    }
}

static void q800fb_invalidate(void *opaque)
{
}

static void q800fb_update(void *opaque)
{
    q800_state_t *s = (q800_state_t *)opaque;
    DisplaySurface *info = s->ds->surface;
    int first = 0;
    int last  = 0;
    int i;
    for (i = 0; i < 256; i++) {
        palette[i * 3] = palette[i * 3 + 1] = palette[i * 3 + 2] = 255 - i;
    }

    framebuffer_update_display(s->ds,
                               VIDEO_BASE, 640, 480, 640, info->linesize,
                               0, 1, q800fb_draw_line, NULL, &first, &last);
    dpy_update(s->ds, 0, 0, 640, 480);
}

static void q800fb_init(q800_state_t *s)
{
    int videomem_index;

    s->ds = graphic_console_init(q800fb_update,
                                 q800fb_invalidate,
                                 NULL, NULL, s);
    qemu_console_resize(s->ds, 640, 480);


    videomem_index = qemu_ram_alloc(NULL,"q800-video.ram",640 * 480);

    cpu_register_physical_memory(VIDEO_BASE, 640 * 480,
                                 videomem_index | IO_MEM_RAM);
}

static void via_set_irq(void *opaque, int irq, int level)
{
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
    ram_addr_t ram_offset;
    int32_t kernel_size;
    uint64_t elf_entry;
    ram_addr_t bios_offset;
    char *filename;
    int bios_size;
    uint32_t initrd_base;
    int32_t initrd_size;
    int escc_mem_index;
    qemu_irq *pic;
    target_phys_addr_t parameters_base;
    int i;
#if 0
    qemu_irq **heathrow_irqs;
    int i;
    uint32_t kernel_base;
    int32_t initrd_size;
    PCIBus *pci_bus;
    MacIONVRAMState *nvr;
    int pic_mem_index, nvram_mem_index, dbdma_mem_index, cuda_mem_index;
    int ide_mem_index[2];
    uint16_t ppc_boot_device;
    DriveInfo * hd[MAX_IDE_BUS * MAX_IDE_DEVS];
    void *fw_cfg;
    void *dbdma;
#endif

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

    ram_offset = qemu_ram_alloc(NULL, "m68k_mac.ram", ram_size);
    cpu_register_physical_memory(0, ram_size, ram_offset | IO_MEM_RAM);

    q800fb_init(&q800_state);
    graphic_depth = 8;

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
        
        stw_phys(parameters_base, BI_MACHTYPE);
        parameters_base += 2;
        stw_phys(parameters_base, sizeof(struct bi_record) + 4);
        parameters_base += 2;
        stl_phys(parameters_base, MACH_MAC);
        parameters_base += 4;

        stw_phys(parameters_base, BI_FPUTYPE);
        parameters_base += 2;
        stw_phys(parameters_base, sizeof(struct bi_record) + 4);
        parameters_base += 2;
        stl_phys(parameters_base, Q800_FPU_ID);
        parameters_base += 4;

        stw_phys(parameters_base, BI_MMUTYPE);
        parameters_base += 2;
        stw_phys(parameters_base, sizeof(struct bi_record) + 4);
        parameters_base += 2;
        stl_phys(parameters_base, Q800_MMU_ID);
        parameters_base += 4;

        stw_phys(parameters_base, BI_CPUTYPE);
        parameters_base += 2;
        stw_phys(parameters_base, sizeof(struct bi_record) + 4);
        parameters_base += 2;
        stl_phys(parameters_base, Q800_CPU_ID);
        parameters_base += 4;

        stw_phys(parameters_base, BI_MAC_VADDR);
        parameters_base += 2;
        stw_phys(parameters_base, sizeof(struct bi_record) + 4);
        parameters_base += 2;
        stl_phys(parameters_base, VIDEO_BASE);
        parameters_base += 4;

        stw_phys(parameters_base, BI_MAC_VDEPTH);
        parameters_base += 2;
        stw_phys(parameters_base, sizeof(struct bi_record) + 4);
        parameters_base += 2;
        stl_phys(parameters_base, graphic_depth);
        parameters_base += 4;

        stw_phys(parameters_base, BI_MAC_VDIM);
        parameters_base += 2;
        stw_phys(parameters_base, sizeof(struct bi_record) + 4);
        parameters_base += 2;
        stl_phys(parameters_base, (480 << 16) | 640);
        parameters_base += 4;

        stw_phys(parameters_base, BI_MAC_VROW);
        parameters_base += 2;
        stw_phys(parameters_base, sizeof(struct bi_record) + 4);
        parameters_base += 2;
	stl_phys(parameters_base, 640 * ((graphic_depth  + 7)/ 8));
        parameters_base += 4;

        stw_phys(parameters_base, BI_MAC_SCCBASE);
        parameters_base += 2;
        stw_phys(parameters_base, sizeof(struct bi_record) + 4);
        parameters_base += 2;
        stl_phys(parameters_base, SCC_BASE);
        parameters_base += 4;

        if (kernel_cmdline) {
            stw_phys(parameters_base, BI_COMMAND_LINE);
            parameters_base += 2;
            stw_phys(parameters_base, sizeof(struct bi_record) +
                     strlen(kernel_cmdline) + 1);
            parameters_base += 2;
            for (i = 0; kernel_cmdline[i]; i++) {
               stb_phys(parameters_base + i, kernel_cmdline[i]);
            }
            stb_phys(parameters_base + i, 0);
            parameters_base = parameters_base + strlen(kernel_cmdline) + 1;
            parameters_base = (parameters_base + 1) & ~1;
        }

        /* load initrd */
        if (initrd_filename) {
            initrd_base = parameters_base + 12 + 4; /* BI_RAMDISK + BI_LAST */
            initrd_size = load_image_targphys(initrd_filename, initrd_base, 
                                              ram_size - initrd_base);
            if (initrd_size < 0) {
                hw_error("qemu: could not load initial ram disk '%s'\n",
                         initrd_filename);
                exit(1);
            }
            stw_phys(parameters_base, BI_RAMDISK);
            parameters_base += 2;
            stw_phys(parameters_base, 8);
            parameters_base += 2;
            stl_phys(parameters_base, initrd_base);
            parameters_base += 4;
            stl_phys(parameters_base, initrd_size);
            parameters_base += 4;
        } else {
            initrd_base = 0;
            initrd_size = 0;
        }
        stw_phys(parameters_base, BI_LAST);
        parameters_base += 2;
        stw_phys(parameters_base, 0);
        parameters_base += 2;
    } else {
        /* allocate and load BIOS */
        bios_offset = qemu_ram_alloc(NULL, "m68k_mac.rom", MACROM_SIZE);
        if (bios_name == NULL) {
            bios_name = MACROM_FILENAME;
        }
        filename = qemu_find_file(QEMU_FILE_TYPE_BIOS, bios_name);
        cpu_register_physical_memory(MACROM_ADDR, MACROM_SIZE,
                                     bios_offset | IO_MEM_ROM);

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


#if 0
    /* XXX: we register only 1 output pin for heathrow PIC */
    heathrow_irqs = qemu_mallocz(smp_cpus * sizeof(qemu_irq *));
    heathrow_irqs[0] =
        qemu_mallocz(smp_cpus * sizeof(qemu_irq)*1);
    /* Connect the heathrow PIC outputs to the 6xx bus */
    for (i = 0; i < smp_cpus; i++) {
        switch (PPC_INPUT(env)) {
        case PPC_FLAGS_INPUT_6xx:
            heathrow_irqs[i] = heathrow_irqs[0] + (i * 1);
            heathrow_irqs[i][0] =
                ((qemu_irq *)env->irq_inputs)[PPC6xx_INPUT_INT];
            break;
        default:
            hw_error("Bus model not supported on OldWorld Mac machine\n");
        }
    }
#endif

    pic = qemu_allocate_irqs(via_set_irq, NULL, 14);
    escc_mem_index = escc_init(SCC_BASE, pic[1], pic[2], serial_hds[0],
                               serial_hds[1], MAC_CLOCK, 0);

#if 0
    /* cuda also initialize ADB */
    cuda_init(&cuda_mem_index, pic[0x12]);

    adb_kbd_init(&adb_bus);
    adb_mouse_init(&adb_bus);

    macio_init(pci_bus, PCI_DEVICE_ID_APPLE_343S1201, 1, pic_mem_index,
               dbdma_mem_index, cuda_mem_index, nvr, 2, ide_mem_index,
               escc_mem_index);
#endif
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
