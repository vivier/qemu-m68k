/* mac_via.c */

/* VIA1 */

#define VIA1_IRQ_ONE_SECOND_BIT 0
#define VIA1_IRQ_VBLANK_BIT     1
#define VIA1_IRQ_ADB_READY_BIT  2
#define VIA1_IRQ_ADB_DATA_BIT   3
#define VIA1_IRQ_ADB_CLOCK_BIT  4

#define VIA1_IRQ_NB             5

/* VIA2 */

#define VIA2_IRQ_SCSI_DATA_BIT  0
#define VIA2_IRQ_SLOT_BIT       1
#define VIA2_IRQ_UNUSED_BIT     2
#define VIA2_IRQ_SCSI_BIT       3
#define VIA2_IRQ_ASC_BIT        4 

#define VIA2_IRQ_NB             5

void *mac_via_init(qemu_irq via1_irq, qemu_irq via2_irq,
                   qemu_irq **via1_irqs, qemu_irq **via2_irqs);
