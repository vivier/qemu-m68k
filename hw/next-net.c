/*
 * QEMU NeXT Network (MB8795) emulation
 *
 * Copyright (c) 2011 Bryce Lanham
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
#include "sysemu.h"//only needed for vm_stop
#include "hw.h"
#include "net.h"
#include "next-net.h"
#include "sysbus.h"
/* debug NeXT ethernet */
#define DEBUG_NET

#ifdef DEBUG_NET
#define DPRINTF(fmt, ...)                                       \
    do { printf("NET: " fmt , ## __VA_ARGS__); } while (0);
#else
#define DPRINTF(fmt, ...)
#endif
    
/* IRQs should be moved to header later */
#define TX_I_DMA 0
#define RX_I_DMA 1
#define TX_I 	 2
#define RX_I     3
/* names could be better */
typedef struct NextDMA {
	uint32_t csr;
	uint32_t savedbase;
	uint32_t savedlimit;
	
    uint32_t baser;
    uint32_t base;
    uint32_t limit;
    uint32_t chainbase;
    uint32_t chainlimit;
	uint32_t basew;

	
} NextDMA;

typedef struct NextNetState {
    uint8_t mac[6];
    
    qemu_irq *irq;
    
    NICState *nic;
    NICConf c;
	
    NextDMA tx_dma;
	uint8_t tx_stat;
	uint8_t tx_mask;
    uint8_t tx_mode;
	
    NextDMA rx_dma;
    uint8_t rx_stat;
    uint8_t rx_mask;
    uint8_t rx_mode;

    uint8_t rst_mode;

} NextNetState;

NextNetState nextnet_state;

void *env_g;

void nextnet_irq(void *opaque, int n, int level);
static int nextnet_can_rx(VLANClientState *nc);
static ssize_t nextnet_rx(VLANClientState *nc, const uint8_t *buf, size_t size);
static void nextnet_cleanup(VLANClientState *nc);

static NetClientInfo nextnet_info = {
    .type = NET_CLIENT_TYPE_NIC,
    .size = sizeof(NICState),
    .can_receive = nextnet_can_rx,
    .receive = nextnet_rx,
    .cleanup = nextnet_cleanup,
};
/* these need to be somewhere else */
extern uint32_t int_status;//should be set in a global irq handler
extern uint32_t int_mask;
extern uint32_t event_test;

void nextnet_reset(NextNetState *s);
void nextnet_reset(NextNetState *s)
{


}

static uint32_t net_readb(void*opaque, target_phys_addr_t addr);
static uint32_t net_readw(void*opaque, target_phys_addr_t addr);
static uint32_t net_readl(void*opaque, target_phys_addr_t addr);
static CPUReadMemoryFunc *net_read[3] = {
	net_readb,
	net_readw,
	net_readl
};

static void net_writeb(void*opaque, target_phys_addr_t addr, uint32_t value);
static void net_writew(void*opaque, target_phys_addr_t addr, uint32_t value);
static void net_writel(void*opaque, target_phys_addr_t addr, uint32_t value);
static CPUWriteMemoryFunc * const net_write[3] = {
	net_writeb,
	net_writew,
	net_writel
};


void nextnet_init(void *opaque)
{
    NextNetState *s = &nextnet_state;
	CPUState *env = (CPUState *)opaque; 
    env_g = opaque;//used this as a global during testing, should be removed
    
	/*register device register space */
    cpu_register_physical_memory((uint32_t)0x2106000,0x1000,
        cpu_register_io_memory(net_read,net_write, (void *)&nextnet_state,DEVICE_NATIVE_ENDIAN));
  cpu_register_physical_memory((uint32_t)0x2006000,0x1000,
        cpu_register_io_memory(net_read,net_write, (void *)&nextnet_state,DEVICE_NATIVE_ENDIAN));


    /* and ethernet control/status registers *///including DMA for now, will seperate out later
    cpu_register_physical_memory((uint32_t)0x2000110,0x4400,
        cpu_register_io_memory(net_read,net_write, (void *)&nextnet_state,DEVICE_NATIVE_ENDIAN));

    /* connect to virtual lan */
    s->c.vlan = qemu_find_vlan(0,1); 
    
    /* register nic */
    s->nic = qemu_new_nic(&nextnet_info,&s->c, "NeXT MB8795\0", NULL, s);

    /* this needs to be read, will have to load rom file i guess */
    uint8_t mac[6] = {0,0,0xf,0,0xf3,0x2};
    memcpy(&s->mac,mac,6); 	
    
	/* allocate TX/RX and DMA irqs - will be global later */
    s->irq = qemu_allocate_irqs(nextnet_irq, env, 4);
    
    /* this sets the nic's info */
    qemu_format_nic_info_str(&s->nic->nc, mac);


}
/* It is likely that all register reads are bytes, while all CSR r/w are longs */
static uint32_t net_readb(void*opaque, target_phys_addr_t addr)
{
    NextNetState *s = (NextNetState *)opaque;
    
//	CPUState *env = (CPUState *)env_g; 
    switch(addr)
    {
        case 0x6000://TXSTAT
            // DPRINTF("TXSTAT \tRead\n");
            return s->tx_stat;
        
        case 0x6001:
            DPRINTF("TXMASK \tRead\n");
            return s->tx_mask;

        case 0x6002:
          //  DPRINTF("RXSTAT \tRead  %x @ %x\n",s->rx_stat,env->pc);
            return s->rx_stat;

        case 0x6003:
          //  DPRINTF("RXMASK \tRead\n");
            return s->rx_mask;

        case 0x6004:
            DPRINTF("TXMODE \tRead\n");
            return s->tx_mode;
        
        case 0x6005:
          //  DPRINTF("RXMODE \tRead\n");
            return s->rx_mode;
        
        case 0x6006:
            DPRINTF("RSTMODE \tRead\n");
            return s->rst_mode;
        
        default:
            fprintf(stderr,"NET Read B @ %x\n",addr);
            return 0;
    }
}
static uint32_t net_readw(void*opaque, target_phys_addr_t addr)
{
	fprintf(stderr,"S Read W @ %x\n",addr);
	return 0;
}
static uint32_t net_readl(void*opaque, target_phys_addr_t addr)
{
    NextNetState *s = (NextNetState *)opaque;
	CPUState *env = (CPUState *)env_g; 
	switch(addr)
    {
        case 0x110:

            //DPRINTF("TXCSR Read\n");
            return s->tx_dma.csr;
        case 0x4100: 
            fprintf(stderr,"SAVEDBASE Read\n");
            return s->tx_dma.savedbase;
		case 0x4104: 
            fprintf(stderr,"SAVELIMIT Read\n");
			return s->tx_dma.savedlimit;
		case 0x4114: 
            fprintf(stderr,"TXLIMIT Read\n");
			return s->tx_dma.limit;
		case 0x4310:
            fprintf(stderr,"TXBASE Read\n");
			/* FUTURE :return nextdma_read(device, addr); */	
			return s->tx_dma.basew;

		case 0x150:
           // fprintf(stderr,"RXCSR Read %x\n",s->rx_dma.csr);
            return s->rx_dma.csr;
        
		case 0x4140:
			return s->rx_dma.savedbase;
        case 0x4144:
            DPRINTF("SAVELIMIT %x @ %x\n",s->rx_dma.savedlimit,env->pc);
            return s->rx_dma.savedlimit;
		
        default:
        fprintf(stderr,"NET Read l @ %x\n",addr);
        return 0;
    }
}
#define NET_TXSTAT_CLEAR 0xFF
#define NET_RXSTAT_CLEAR 0xFF
static void net_writeb(void*opaque, target_phys_addr_t addr, uint32_t value)
{
//	CPUState *env = (CPUState *)env_g; 
    NextNetState *s = (NextNetState *)opaque;
    switch(addr)
    {
    	case 0x6000:
            DPRINTF("TXSTAT \tWrite: %x\n",value);
            if(value == NET_TXSTAT_CLEAR)
               s->tx_stat = 0x80;
            else
                s->tx_stat = value;
            break;
        case 0x6001:
            DPRINTF("TXMASK \tWrite: %x\n",value);
            s->tx_mask = value;
            break;
        case 0x6002:
         //   DPRINTF("RXSTAT \tWrite: %x @ %x\n",value,env->pc);
            if(value == NET_RXSTAT_CLEAR)
                s->rx_stat = 0;
            else
                s->rx_stat = value;
            break;
        case 0x6003:
        //    DPRINTF("RXMASK \tWrite: %x\n",value);
            s->rx_mask = value;
            break;
        case 0x6004:
            DPRINTF("TXMODE \tWrite: %x\n",value);
            s->tx_mode = value;
            break;
        case 0x6005:
          //  DPRINTF("RXMODE \tWrite: %x\n",value);
            s->rx_mode = value;
            break;
        case 0x6006:
            DPRINTF("RSTMODE \tWrite: %x\n",value);
            s->rst_mode = value;
            break;
        case 0x600d:
        s->mac[(addr&0xF)-8] = value;         
        DPRINTF("Set MAC ADDR %.2x:%.2x:%.2x:%.2x:%.2x:%.2x\n",
            s->mac[0],s->mac[1],s->mac[2],s->mac[3],s->mac[4],s->mac[5]);
        qemu_macaddr_default_if_unset((MACAddr *)&s->mac);
        break;
        case 0x6008:case 0x6009:case 0x600a: case 0x600b: case 0x600c:
        s->mac[(addr&0xF)-8] = value;         
        break; 
        case 0x6010:case 0x6011: case 0x6012: case 0x6013: case 0x6014:
     //   break;
        default:
        fprintf(stderr,"NET Write B @ %x with %x\n",addr,value);
    }
}
static void net_writew(void*opaque, target_phys_addr_t addr, uint32_t value)
{
	fprintf(stderr,"NET W w @ %x with %x\n",addr,value);
}
#define DMA_ENABLE		0x01000000
#define DMA_SUPDATE		0x02000000
#define DMA_COMPLETE	0x08000000

#define DMA_M2DEV       0x0
#define DMA_SETENABLE   0x00010000
#define DMA_SETSUPDATE  0x00020000
#define DMA_DEV2M		0x00040000
#define DMA_CLRCOMPLETE 0x00080000
#define DMA_RESET		0x00100000
static void net_writel(void*opaque, target_phys_addr_t addr, uint32_t value)
{
	static int tx_count = 0;
    NextNetState *s = (NextNetState *)opaque;
	switch(addr)
    {
        case 0x110:
        {
				
								
				if(value & DMA_SETENABLE)
				{
                tx_count++;
			//	if(tx_count % 4) return;
				size_t len = (0xFFFFFFF & s->tx_dma.limit) - s->tx_dma.base;
              DPRINTF("TXDMA ENABLE: %x len: %zu\n",s->tx_dma.base, len);
				DPRINTF("TX Enable\n");
                uint8_t buf[1600];//needs to be in dma struct?
                cpu_physical_memory_read(s->tx_dma.base, buf, len); 
                
				qemu_send_packet(&s->nic->nc, buf,len);   
               	s->tx_dma.csr |= DMA_COMPLETE | DMA_SUPDATE;
			    s->tx_stat =  0x80;
			//	if(tx_count > 1510) vm_stop(VMSTOP_DEBUG);
				
    			qemu_set_irq(s->irq[TX_I_DMA],3);

				}
				if(value & DMA_SETSUPDATE)
					s->tx_dma.csr |= DMA_SUPDATE;	
		
				if(value & DMA_CLRCOMPLETE)
					s->tx_dma.csr &= ~DMA_COMPLETE;

				if(value & DMA_RESET)
					s->tx_dma.csr &= ~(DMA_COMPLETE | DMA_SUPDATE | DMA_ENABLE);	        
		}
        break;
        
        case 0x4100: 
        DPRINTF("Write l @ %x with %x\n",addr,value);
			s->tx_dma.savedbase = value;
		    break;
		
		case 0x4104: 
        DPRINTF("Write l @ %x with %x\n",addr,value);
			s->tx_dma.savedlimit = value;
		    break;
		case 0x4110:
        DPRINTF("Write l @ %x with %x\n",addr,value);
			s->tx_dma.base = value;
			break;	
        case 0x4114: 
        DPRINTF("Write l @ %x with %x\n",addr,value);
			s->tx_dma.limit = value;
		    break;
		
        case 0x4310:
        DPRINTF("Write l @ %x with %x\n",addr,value);
			s->tx_dma.base = value;
			/* FUTURE :nextdma_write(device, addr, value); */	
            break;

        case 0x150:
            	if(value & DMA_DEV2M)
                {
                    DPRINTF("RX Dev to Memory\n");
				}	
				
                if(value & DMA_SETENABLE)
                    s->rx_dma.csr |= DMA_ENABLE;
                
				if(value & DMA_SETSUPDATE)
					s->rx_dma.csr |= DMA_SUPDATE;	
		
				if(value & DMA_CLRCOMPLETE)
					s->rx_dma.csr &= ~DMA_COMPLETE;

				if(value & DMA_RESET)
					s->rx_dma.csr &= ~(DMA_COMPLETE | DMA_SUPDATE | DMA_ENABLE);				
				//
	
				DPRINTF("RXCSR \tWrite: %x\n",value);
	        	break;
        
        case 0x4150:
            
       // DPRINTF("Write l @ %x with %x\n",addr,value);
            s->rx_dma.base = value;
		//	s->rx_dma.savedbase = value;
            break;

        case 0x4154:
            s->rx_dma.limit = value;
       // DPRINTF("Write l @ %x with %x\n",addr,value);
            break;

        case 0x4158:
            s->rx_dma.chainbase = value;
       // DPRINTF("Write l @ %x with %x\n",addr,value);
            break;
    
        case 0x415c:
            s->rx_dma.chainlimit = value;
       // DPRINTF("Write l @ %x with %x\n",addr,value);
            //DPRINTF("Pointer write %x w %x\n",addr,value);
            break;
        default:
        DPRINTF("Write l @ %x with %x\n",addr,value);
    }

}

static int nextnet_can_rx(VLANClientState *nc)
{
    NextNetState *s = DO_UPCAST(NICState, nc, nc)->opaque;
    if(s->rx_mode & 0x3)
		return 1;
	else
		return -1;
}

static ssize_t nextnet_rx(VLANClientState *nc, const uint8_t *buf, size_t size)
{
	NextNetState *s = DO_UPCAST(NICState, nc, nc)->opaque;
    
	DPRINTF("received packet %zu\n",size);

	/* Ethernet DMA is supposedly 32 byte aligned */	
	if((size % 32) != 0)
	{
		size -= size % 32;
		size += 32;
	}
	
	/* write the packet into memory */
	cpu_physical_memory_write(s->rx_dma.base,buf,size);
	
	/* saved limit is checked to calculate packet size
		by both the rom and netbsd */ 
	s->rx_dma.savedlimit = (s->rx_dma.base + size);
	s->rx_dma.savedbase = (s->rx_dma.base);
	
	/*32 bytes under savedbase seems to be some kind of register
	of which the purpose is unknown as of yet*/
	//stl_phys(s->rx_dma.base-32,0xFFFFFFFF);
	
	if((s->rx_dma.csr & DMA_SUPDATE)){	
		s->rx_dma.base = s->rx_dma.chainbase;
		s->rx_dma.limit = s->rx_dma.chainlimit;
	}
	//we received a packet
    s->rx_stat = 0x80;
	
	//Set dma registers and raise an irq
	s->rx_dma.csr |= DMA_COMPLETE; //DON'T CHANGE THIS!!!!
   	qemu_set_irq(s->irq[RX_I_DMA],6);
    
	return size;
}

static void nextnet_cleanup(VLANClientState *nc)
{
}

/* level and vector values taken from Plan 9 source */
void nextnet_irq(void *opaque, int n, int level)
{
    CPUM68KState *s = (CPUM68KState *)opaque;
    switch(n)
	{
		case TX_I:
			int_status = 1<<10;
			m68k_set_irq_level(s,3,27);
			break;
		
		case RX_I:
			int_status = 1<<9;
			m68k_set_irq_level(s,3,27);
			break;
		
		case TX_I_DMA:
			int_status = 1<<28;
			m68k_set_irq_level(s,6,30);
            break;
		
		case RX_I_DMA:
			int_status = 1<<27;
			m68k_set_irq_level(s,6,30);
			break;
	}

	
}		
