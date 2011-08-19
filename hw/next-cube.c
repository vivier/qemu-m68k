/*
 * Next Cube System Driver
 * Copyright (c) 2011 Bryce Lanham
 *
 * Based on dummy_m68k.c Copyright (c) 2007 CodeSourcery.
 *
 * Scr2 code from Previous, used under the GPL
 *
 * This code is licensed under the GPL
 */


#include "hw.h"
#include "next-fb.h"
#include "next-kbd.h"
#include "next-net.h"

#include "monitor.h"
#include "sysemu.h"
#include "boards.h"
#include "console.h"
#include "loader.h"//probably not needed
#include "elf.h"
#include "esp.h" //SCSI ESP should work out of the box
#include "escc.h" //ZILOG 8530 Serial Emulation
#define ENTRY 0x0100001e

/* Board init.  */
#define ROM_FILE "rom66.bin"
/* these need to be in machine state */
uint32_t scr1 = 0;
uint32_t scr2 = 0;
uint32_t int_status = 4;//from previous
uint32_t int_mask = 0;
uint32_t event_test = 0;
/* Thanks to NeXT forums for this */
uint8_t rtc_ram3[32]={
0x94,0x0f,0x40,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0xfb,0x6d,0x00,0x00,0x7B,0x00,
0x00,0x00,0x65,0x6e,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x50,0x13
};
uint8_t *rtc_ram;
uint8_t rtc_ram2[32]={
0x94,0x0f,0x40,0x03,0x00,0x00,0x00,0x00,
0x00,0x00,0xfb,0x6d,0x00,0x00,0x4b,0x00,
0x41,0x00,0x20,0x00,0x00,0x00,0x00,0x00,
0x00,0x00,0x00,0x00,0x00,0x00,0x84,0x7e,
};

next_state_t next_state;

static uint32_t mmio_readb(void*opaque, target_phys_addr_t addr);
static uint32_t mmio_readw(void*opaque, target_phys_addr_t addr);
static uint32_t mmio_readl(void*opaque, target_phys_addr_t addr);

static void mmio_writeb(void*opaque, target_phys_addr_t addr, uint32_t val);
static void mmio_writew(void*opaque, target_phys_addr_t addr, uint32_t val);
static void mmio_writel(void*opaque, target_phys_addr_t addr, uint32_t val);
static CPUReadMemoryFunc *mmio_read[3] = {
	mmio_readb,
	mmio_readw,
	mmio_readl
};

static CPUWriteMemoryFunc *mmio_write[3] = {
	mmio_writeb,
	mmio_writew,
	mmio_writel
};
static uint32_t mmio_readb(void*opaque, target_phys_addr_t addr)
{
	switch(addr)
	{
		case 0xc002:
		return (scr1 >> 8) & 0xFF;
		case 0xc0008:
		return 0xff;//hack to hide memory error
		default:
	        fprintf(stderr,"MMIO Read B @ %x\n",addr);
        return 0x0;
    }
}
static uint32_t mmio_readw(void*opaque, target_phys_addr_t addr)
{
	switch(addr)
	{
        default:
	    fprintf(stderr,"MMIO Read W @ %x\n",addr);
        return 0x0;
    }
}

static uint32_t mmio_readl(void*opaque, target_phys_addr_t addr)
{

	switch(addr)
	{
        case 0x7000:
	//	fprintf(stderr,"INTSTAT\n");
        return int_status;
	
    	case 0x7800:
		//fprintf(stderr,"INTMASK\n");
		return int_mask;
	
    	case 0xc000:
	//	fprintf(stderr, "SCR1 Read: @ %X\n",((CPUM68KState *)opaque)->pc);
		return scr1;
	
    	case 0xd000:
	//	fprintf(stderr, "SCR2 Read: @ %X %X\n",((CPUM68KState *)opaque)->pc,scr2);
		return scr2;
		
		case 0xc0000:
		return 0x01;	
       	
		case 0xc0034:
		return 0x560;
 		
		default:
		fprintf(stderr,"MMIO Read L @ %x\n",addr);
		return 0x0;
	}
}
static void mmio_writeb(void*opaque, target_phys_addr_t addr, uint32_t val)
{
	
	switch(addr)
	{
		default:
		fprintf(stderr,"MMIO Write B @ %x with %x\n",addr,val);
	}

}
static void mmio_writew(void*opaque, target_phys_addr_t addr, uint32_t val)
{
	fprintf(stderr,"MMIO Write W\n"	);
}
int led = 0;

static void mmio_writel(void*opaque, target_phys_addr_t addr, uint32_t val)
{
	static int phase = 0;
	static uint8_t old_scr2;
	static uint8_t rtc_command = 0;
	static uint8_t rtc_value = 0;
	static uint8_t rtc_status = 0x90;
	static uint8_t rtc_return = 0;
	uint8_t scr2_2; 
    switch(addr)
	{
		case 0x10:
        break;
        case 0x7000:
		fprintf(stderr,"INT Status old: %x new: %x\n",int_status,val);
        int_status = val;
		break;
		case 0x7800:
		fprintf(stderr,"INT Status old: %x new: %x\n",int_mask,val);
		int_mask  = val;
		break;
		case 0xc000:
		fprintf(stderr, "SCR1 Write: %x @ %X\n",val,((CPUM68KState *)opaque)->pc);
		break;
		case 0xd000:
		//old_scr2 = val;
		scr2_2 = (val >> 8) & 0xFF;
			if(val &0x1)
		{	
			printf("fault!\n");
			led++;
			if(led == 10)
			{
				fprintf(stderr,"LED flashing, possible fault, pausing emulation\n");
				led = 0;
				vm_stop(VMSTOP_DEBUG);
			}

		}
		
		if (scr2_2& 0x1) {
		//	fprintf(stderr,"RTC %x phase %i\n",scr2_2,phase);
			if (phase==-1) phase=0;
				// if we are in going down clock... do something
				#define SCR2_RTCLK 0x2
				#define SCR2_RTDATA 0x4
				if (((old_scr2&SCR2_RTCLK)!=(scr2_2&SCR2_RTCLK)) && ((scr2_2&SCR2_RTCLK)==0) ) {
					if (phase<8)
						rtc_command=(rtc_command<<1)|((scr2_2&SCR2_RTDATA)?1:0);
					if ((phase>=8) && (phase<16)) {
						rtc_value=(rtc_value<<1)|((scr2_2&SCR2_RTDATA)?1:0);
			
					// if we read RAM register, output RT_DATA bit
					if (rtc_command<=0x1F) {
						scr2_2=scr2_2&(~SCR2_RTDATA);
					if (rtc_ram[rtc_command]&(0x80>>(phase-8)))
						scr2_2 |=SCR2_RTDATA;
				
					rtc_return=(rtc_return<<1)|((scr2_2&SCR2_RTDATA)?1:0);
				}
					// read the status 0x30
					if (rtc_command==0x30) {
						scr2_2=scr2_2&(~SCR2_RTDATA);
						// for now status = 0x98 (new rtc + FTU)
						if (rtc_status&(0x80>>(phase-8)))
							scr2_2|=SCR2_RTDATA;
					
						rtc_return=(rtc_return<<1)|((scr2_2&SCR2_RTDATA)?1:0);
					}
					// read the status 0x31
					if (rtc_command==0x31) {
						scr2_2=scr2_2&(~SCR2_RTDATA);
					// for now 0x00
					if (0x00&(0x80>>(phase-8)))
						scr2_2|=SCR2_RTDATA;
					rtc_return=(rtc_return<<1)|((scr2_2&SCR2_RTDATA)?1:0);
				}
			
				if ((rtc_command>=0x20) && (rtc_command<=0x2F)) {
					scr2_2=scr2_2&(~SCR2_RTDATA);
					// for now 0x00
					if (0x00&(0x80>>(phase-8)))
						scr2_2|=SCR2_RTDATA;
					rtc_return=(rtc_return<<1)|((scr2_2&SCR2_RTDATA)?1:0);
				}
			
			}
		
			phase++;
				if (phase==16) {
		//	fprintf(stderr,"SCR2 RTC command complete %x %x %x at PC=$%08x\n",
		//	rtc_command,rtc_value,rtc_return,0);
						if ((rtc_command>=0x80) && (rtc_command<=0x9F))
                        {
							rtc_ram[rtc_command-0x80]=rtc_value;
                            #ifdef READ_RTC
							FILE *fp = fopen("rtc.ram","wb+");
                            int ret = fwrite(rtc_ram,1,32,fp);
							if(ret != 32)
									abort();
                            fclose(fp);
							#endif
                        }
						// write to x30 register
						if (rtc_command==0xB1) {
						// clear FTU
						if (rtc_value & 0x04) {
							rtc_status=rtc_status&(~0x18);
							int_status=int_status&(~0x04);
						}
					}		
				}
			}
		} else {
						// else end or abort
						phase=-1;
						rtc_command=0;
						rtc_value=0;
		}
		scr2 = val & 0xFFFF00FF;
		scr2 |= scr2_2<< 8;	
		old_scr2 = scr2_2;

		break;
		case 0xc0034:
	//	if(val == 0x90000560)
	//	vm_stop(VMSTOP_DEBUG);
		default:
			fprintf(stderr,"MMIO Write l @ %x with %x\n",addr,val);

	}
}


static uint32_t scr_readb(void*opaque, target_phys_addr_t addr);
static uint32_t scr_readw(void*opaque, target_phys_addr_t addr);
static uint32_t scr_readl(void*opaque, target_phys_addr_t addr);
static CPUReadMemoryFunc *scr_read[3] = {
	scr_readb,
	scr_readw,
	scr_readl

};

static void scr_writeb(void*opaque, target_phys_addr_t addr, uint32_t value);
static void scr_writew(void*opaque, target_phys_addr_t addr, uint32_t value);
static void scr_writel(void*opaque, target_phys_addr_t addr, uint32_t value);
static CPUWriteMemoryFunc * const scr_write[3] = {
	scr_writeb,
	scr_writew,
	scr_writel
};

static uint32_t scr_readb(void*opaque, target_phys_addr_t addr)
{
 //   CPUState *s = (CPUState *)opaque;
    switch(addr)
    {
        
        case 0x14000: case 0x14005:
        fprintf(stderr,"SCSI read b\n");
        return 0x0;
        
        case 0x14104://FDD
      //  return 0x0;
        
        case 0x14108:
        return 0x0;
        
        case 0x18001: 
	//	fprintf(stderr, "SCC @ %X\n",((CPUM68KState *)opaque)->pc);
        return 0;
        case 0x1a000:
    //    return 0;
        case 0x1a001: 
     //   return 0;
        case 0x1a002: 
       // return 0;
        case 0x1a003: 
     //   fprintf(stderr,"event #%x @ %x\n",addr &0x3,s->pc);
       // event_test++;
      // if(event_test == 200) event_test = 0;
        return event_test; 
        default:
        fprintf(stderr,"BMAP Read B @ %x\n",addr);
        return 0;
    }
    return 0;
}
static uint32_t scr_readw(void*opaque, target_phys_addr_t addr)
{
	fprintf(stderr,"S Read W @ %x\n",addr);
	return 0;
}
static uint32_t scr_readl(void*opaque, target_phys_addr_t addr)
{
	fprintf(stderr,"SRead L @ %x\n",addr);
	return 0;
}
static void scr_writeb(void*opaque, target_phys_addr_t addr, uint32_t value)
{
    switch(addr)
    {
        case 0x10000: break;//Screen brightness
        case 0x18000:
        case 0x18001:
        case 0x18004:
        break;
    	default:
        fprintf(stderr,"BMAP Write B @ %x with %x\n",addr,value);
    }
}
static void scr_writew(void*opaque, target_phys_addr_t addr, uint32_t value)
{
	fprintf(stderr,"SWrite w @ %x with %x\n",addr,value);
}
static void scr_writel(void*opaque, target_phys_addr_t addr, uint32_t value)
{

	fprintf(stderr,"SWrite l @ %x with %x\n",addr,value);
}

/* need to make more defines, put them into a header */
#define RAM_SIZE 0x4000000

void serial_irq(void *opaque, int n, int level);
static void next_cube_init(ram_addr_t ram_size,
                     const char *boot_device,
                     const char *kernel_filename, const char *kernel_cmdline,
                     const char *initrd_filename, const char *cpu_model)
{
    /* Initialize the cpu core */ 
    CPUState *env = cpu_init("m68040");
    if (env == NULL) {
        fprintf(stderr, "Unable to find m68k CPU definition\n");
        exit(1);
    }
    
    /* Initialize CPU registers.  */
    env->vbr = 0;
    env->pc  = 0x100001e; //technically should read vector
    env->sr  = 0x2700;
    
    /* Set internal registers to initial values */
	scr1 = 0x01000220;
    scr2 = 0x00ff8880;
    
    int_mask = 0x88027640; 
    int_status= 0x200;
    
    /* Load RTC ram,  needs to be in a function probably */
    { 
	   	rtc_ram = malloc(32);
       	#ifdef LOAD_RTC
        FILE *fp = fopen("rtc.ram","rb");
        if(fread(rtc_ram,1,32,fp) != 32)
            abort();
        fclose(fp);
		#endif
        memcpy(rtc_ram,rtc_ram2,32);
    }


    /* 64MB RAM starting at 0x4000000  */
    cpu_register_physical_memory(0x4000000, RAM_SIZE,
        qemu_ram_alloc(NULL, "next-cube.ram", RAM_SIZE) | IO_MEM_RAM);
  
	/* Framebuffer */
    nextfb_init(&next_state);
 
    /* MMIO */
	cpu_register_physical_memory((uint32_t)0x2000000,0xD0000,
        cpu_register_io_memory(mmio_read,mmio_write, (void *)env,DEVICE_NATIVE_ENDIAN));    
    
    /* BMAP */ //acts as a catch-all for now
    cpu_register_physical_memory((uint32_t)0x2100000,0x3A7FF,
        cpu_register_io_memory(scr_read,scr_write, (void *)env,DEVICE_NATIVE_ENDIAN));
    
    /* KBD */
    nextkbd_init((void *)env);

    /* Serial */
   	//CharDriverState *console = text_console_init(NULL);
  	//qemu_irq *serial = qemu_allocate_irqs(serial_irq, env, 2);
  	//escc_init(0x2118000, serial[0], serial[1],
    //        console, NULL,   (9600*384),0);

  	
    /* Load ROM here */	 
    if(get_image_size(ROM_FILE) != 0x20000)
    {
        fprintf(stderr,"Failed to load rom file!\n");
        exit(1);
    }
    
    rom_add_file_fixed(ROM_FILE,0x1000000,0);
    rom_add_file_fixed(ROM_FILE,0x000000,1);
	cpu_register_physical_memory((uint32_t)0x1000000, 0x20000,
        qemu_ram_alloc(NULL, "next.rom", 0x20000) | IO_MEM_ROM);
		cpu_register_physical_memory((uint32_t)0x000000, 0x20000,
        qemu_ram_alloc(NULL, "nex.rom", 0x20000) | IO_MEM_ROM);

	/* Ethernet */
    nextnet_init((void *)env);

    
}

void serial_irq(void *opaque, int n, int level)
{
  //  fprintf(stderr,"IRQQQQQ\n");
 //   int_status |= 0xFFFFFF00;
//	int_status |= 1<<17;
//CPUM68KState *env = (CPUM68KState *)opaque;
//env->exception_index = 10;
//m68k_set_irq_level((CPUM68KState *)opaque, 5,29);//25);

}
static QEMUMachine next_machine = {
    .name = "next-cube",
    .desc = "NeXT Cube",
    .init = next_cube_init,
};

static void next_machine_init(void)
{
    qemu_register_machine(&next_machine);
}

machine_init(next_machine_init);
