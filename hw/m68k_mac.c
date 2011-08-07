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
#include "macrom_patch/macrom.h"

#define MACROM_FILENAME "MacROM.bin"

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
    uint8_t *rom;
#if 0
    qemu_irq *pic, **heathrow_irqs;
    int i;
    uint32_t kernel_base, initrd_base;
    int32_t initrd_size;
    PCIBus *pci_bus;
    MacIONVRAMState *nvr;
    int pic_mem_index, nvram_mem_index, dbdma_mem_index, cuda_mem_index;
    int escc_mem_index, ide_mem_index[2];
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

    if (linux_boot) {

        kernel_size = load_elf(kernel_filename, NULL, NULL,
                               &elf_entry, NULL, NULL, 1,
                               ELF_MACHINE, 0);
        if (kernel_size < 0) {
            hw_error("qemu: could not load kernel '%s'\n",
                      kernel_filename);
            exit(1);
        }
        env->pc = elf_entry;
#if 0
        /* load initrd */
        if (initrd_filename) {
            initrd_base = INITRD_LOAD_ADDR;
            initrd_size = load_image_targphys(initrd_filename, initrd_base,
                                              ram_size - initrd_base);
            if (initrd_size < 0) {
                hw_error("qemu: could not load initial ram disk '%s'\n",
                         initrd_filename);
                exit(1);
            }
        } else {
            initrd_base = 0;
            initrd_size = 0;
        }
#endif
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
            qemu_free(filename);
        } else {
            bios_size = -1;
        }
        if (bios_size < 0 || bios_size > MACROM_SIZE) {
            hw_error("qemu: could not load MacROM '%s'\n", bios_name);
            exit(1);
        }
        if (semihosting_enabled) {
            rom = rom_ptr(MACROM_ADDR);
            macrom_patch(rom, bios_size);
            stl_phys(0, ldl_p(rom));	/* reset initial SP */
            stl_phys(4, MACROM_ADDR + ldl_p(rom + 4)); /* reset initial PC */
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

    escc_mem_index = escc_init(0x80013000, pic[0x0f], pic[0x10], serial_hds[0],
                               serial_hds[1], ESCC_CLOCK, 4);

    /* cuda also initialize ADB */
    cuda_init(&cuda_mem_index, pic[0x12]);

    adb_kbd_init(&adb_bus);
    adb_mouse_init(&adb_bus);

    macio_init(pci_bus, PCI_DEVICE_ID_APPLE_343S1201, 1, pic_mem_index,
               dbdma_mem_index, cuda_mem_index, nvr, 2, ide_mem_index,
               escc_mem_index);

    if (graphic_depth != 15 && graphic_depth != 32 && graphic_depth != 8) {
        graphic_depth = 15;
    }
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
