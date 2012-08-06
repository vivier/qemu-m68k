void dp83932_init(NICInfo *nd, target_phys_addr_t base, int it_shift,
                  MemoryRegion *address_space,
                  qemu_irq irq, void* mem_opaque,
                  void (*memory_rw)(void *opaque, target_phys_addr_t addr,
                  uint8_t *buf, int len, int is_write));
