void dp83932_init(NICInfo *nd, hwaddr base, hwaddr rombase,
                  int it_shift, int regs_offset,
                  MemoryRegion *address_space,
                  qemu_irq irq, void* mem_opaque,
                  void (*memory_rw)(void *opaque, hwaddr addr, uint8_t *buf, int len, int is_write));

