/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 *
 * Virt system Controller
 */

#include "qemu/osdep.h"
#include "qemu/datadir.h"
#include "hw/qdev-properties.h"
#include "hw/sysbus.h"
#include "hw/loader.h"
#include "hw/boards.h"
#include "migration/vmstate.h"
#include "qemu/log.h"
#include "trace.h"
#include "elf.h"
#include "sysemu/runstate.h"
#include "hw/misc/virt_ctrl.h"

enum {
    REG_FEATURES = 0x00,
    REG_CMD      = 0x04,
    REG_PARAM    = 0x08,
};

#define FEAT_POWER_CTRL 0x00000001
#define FEAT_FW_CTRL    0x00000002

#define FEAT_SUPPORTED (FEAT_POWER_CTRL | FEAT_FW_CTRL)

enum {
    /* Power Control */
    CMD_NOOP,
    CMD_RESET,
    CMD_HALT,
    CMD_PANIC,
    /* Firmware Control */
    CMD_FW_MACHINE_ID,
    CMD_FW_LOAD,
    CMD_FW_RAMSIZE,
    CMD_FW_QEMU_VERSION
};

enum {
   FW_M68K,
};

static uint32_t param;

#define RESULT_ERROR (-1)

static uint32_t fw_load_m68k(VirtCtrlState *s)
{
    char *elf_filename, *ramfs_filename;
    int32_t kernel_size;
    uint64_t elf_entry, high;
    int32_t ramfs_size;
    ram_addr_t ramfs_base;
    void *ram_ptr;

    elf_filename = qemu_find_file(QEMU_FILE_TYPE_BIOS, s->fw_elf);
    if (elf_filename == NULL) {
        error_report("Cannot find %s", s->fw_elf);
        return RESULT_ERROR;
    }
    ramfs_filename = qemu_find_file(QEMU_FILE_TYPE_BIOS, s->fw_ramfs);
    if (ramfs_filename == NULL) {
        error_report("Cannot find %s", s->fw_ramfs);
        return RESULT_ERROR;
    }

    kernel_size = load_elf_ram(elf_filename, NULL, NULL, NULL,
                               &elf_entry, NULL, &high, NULL, 1,
                               EM_68K, 0, 0, NULL, false);
    if (kernel_size < 0) {
        error_report("could not load kernel '%s'", elf_filename);
        return RESULT_ERROR;
    }

    ramfs_size = get_image_size(ramfs_filename);
    if (ramfs_size < 0) {
        error_report("could not load initial ram disk '%s'",
                     ramfs_filename);
        return RESULT_ERROR;
    }

    ram_ptr = memory_region_get_ram_ptr(s->machine->ram);

    ramfs_base = (s->machine->ram_size - ramfs_size) & ~0xfff;
    load_image_size(ramfs_filename, ram_ptr + ramfs_base, ramfs_size);

    high = (high + 1) & ~1;
    *(uint32_t *)(ram_ptr + high) = cpu_to_be32(elf_entry);
    *(uint32_t *)(ram_ptr + high + 4) = cpu_to_be32(ramfs_base);
    *(uint32_t *)(ram_ptr + high + 8) = cpu_to_be32(ramfs_size);

    return high;
}

static uint64_t virt_ctrl_read(void *opaque, hwaddr addr, unsigned size)
{
    VirtCtrlState *s = opaque;
    uint64_t value = 0;

    switch (addr) {
    case REG_FEATURES:
        value = FEAT_SUPPORTED;
        break;
    case REG_PARAM:
        value = param;
        break;
    default:
        qemu_log_mask(LOG_UNIMP,
                      "%s: unimplemented register read 0x%02"HWADDR_PRIx"\n",
                      __func__, addr);
        break;
    }

    trace_virt_ctrl_read(s, addr, size, value);

    return value;
}

static void virt_ctrl_write(void *opaque, hwaddr addr, uint64_t value,
                            unsigned size)
{
    VirtCtrlState *s = opaque;

    trace_virt_ctrl_write(s, addr, size, value);

    switch (addr) {
    case REG_CMD:
        switch (value) {
        case CMD_NOOP:
            break;
        case CMD_RESET:
            qemu_system_reset_request(SHUTDOWN_CAUSE_GUEST_RESET);
            break;
        case CMD_HALT:
            qemu_system_shutdown_request(SHUTDOWN_CAUSE_GUEST_SHUTDOWN);
            break;
        case CMD_PANIC:
            qemu_system_shutdown_request(SHUTDOWN_CAUSE_GUEST_PANIC);
            break;
        case CMD_FW_LOAD:
            switch (param) {
            case FW_M68K:
                param = fw_load_m68k(s);
                break;
            default:
                qemu_log_mask(LOG_UNIMP, "%s: unimplemented FW type %d\n",
                              __func__, param);
                break;
            }
            break;
        case CMD_FW_RAMSIZE:
            param = s->machine->ram_size;
            break;
        case CMD_FW_QEMU_VERSION:
            param = (QEMU_VERSION_MAJOR << 24) | (QEMU_VERSION_MINOR << 16) |
                    (QEMU_VERSION_MICRO << 8);
            break;
        case CMD_FW_MACHINE_ID:
            param = 0;
            break;
        }
        break;
    case REG_PARAM:
        param = value;
        break;
    default:
        qemu_log_mask(LOG_UNIMP,
                      "%s: unimplemented register write 0x%02"HWADDR_PRIx"\n",
                      __func__, addr);
        break;
    }
}

static const MemoryRegionOps virt_ctrl_ops = {
    .read = virt_ctrl_read,
    .write = virt_ctrl_write,
    .endianness = DEVICE_NATIVE_ENDIAN,
    .valid.max_access_size = 4,
    .impl.max_access_size = 4,
};

static void virt_ctrl_reset(DeviceState *dev)
{
    VirtCtrlState *s = VIRT_CTRL(dev);

    trace_virt_ctrl_reset(s);
}

static void virt_ctrl_realize(DeviceState *dev, Error **errp)
{
    VirtCtrlState *s = VIRT_CTRL(dev);

    trace_virt_ctrl_instance_init(s);

    memory_region_init_io(&s->iomem, OBJECT(s), &virt_ctrl_ops, s,
                          "virt-ctrl", 0x100);
}

static const VMStateDescription vmstate_virt_ctrl = {
    .name = "virt-ctrl",
    .version_id = 1,
    .minimum_version_id = 1,
    .fields = (const VMStateField[]) {
        VMSTATE_UINT32(irq_enabled, VirtCtrlState),
        VMSTATE_END_OF_LIST()
    }
};

static Property virt_ctl_properties[] = {
    DEFINE_PROP_LINK("machine", VirtCtrlState, machine,
                     TYPE_MACHINE, MachineState *),
    DEFINE_PROP_STRING("fw.elf", VirtCtrlState, fw_elf),
    DEFINE_PROP_STRING("fw.ramfs", VirtCtrlState, fw_ramfs),
    DEFINE_PROP_END_OF_LIST(),
};

static void virt_ctrl_instance_init(Object *obj)
{
    SysBusDevice *dev = SYS_BUS_DEVICE(obj);
    VirtCtrlState *s = VIRT_CTRL(obj);

    trace_virt_ctrl_instance_init(s);

    sysbus_init_mmio(dev, &s->iomem);
    sysbus_init_irq(dev, &s->irq);
}

static void virt_ctrl_class_init(ObjectClass *oc, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(oc);

    dc->reset = virt_ctrl_reset;
    dc->realize = virt_ctrl_realize;
    dc->vmsd = &vmstate_virt_ctrl;

    device_class_set_props(dc, virt_ctl_properties);
}

static const TypeInfo virt_ctrl_info = {
    .name = TYPE_VIRT_CTRL,
    .parent = TYPE_SYS_BUS_DEVICE,
    .class_init = virt_ctrl_class_init,
    .instance_init = virt_ctrl_instance_init,
    .instance_size = sizeof(VirtCtrlState),
};

static void virt_ctrl_register_types(void)
{
    type_register_static(&virt_ctrl_info);
}

type_init(virt_ctrl_register_types)
