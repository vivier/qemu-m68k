#include <stdio.h>

#include "cpu.h"
#include "dyngen-exec.h"
#include "softmmu_exec.h"
#include "macrom.h"

#define RAMBaseMac 0

/*
 * MacOS reset
 */

static int do_simcall_reset(CPUState *env)
{
    int i;
    uint32 boot_globs = RAMBaseMac + ram_size - 0x1c;
#if 0
    TimerReset();
    EtherReset();
    AudioReset();
#endif

    /* Create BootGlobs at top of memory */

    for (i = 0; i < 1024; i++) {
        stl_phys(ram_size - 4096 + i * 4, 0);
    }

    stl_phys(boot_globs + 0x00, RAMBaseMac);   /* First RAM bank */
    stl_phys(boot_globs + 0x04, ram_size);
    stl_phys(boot_globs + 0x08, 0xffffffff);   /* End of bank table */
    stl_phys(boot_globs + 0x0c, 0);

    /* Setup registers for boot routine */

    env->dregs[0] = ldl_phys(MACROM_ADDR + UniversalInfo + 0x18);      /* AddrMapFlags */
    env->dregs[1] = ldl_phys(MACROM_ADDR + UniversalInfo + 0x1c);      /* UnivROMFlags */
    env->dregs[2] = ldl_phys(MACROM_ADDR + UniversalInfo + 0x10);      /* HWCfgFlags/IDs */
    if (fpu_type)
        env->dregs[2] |= 0x10000000;                             /* Set FPU flag if FPU present */
    else
        env->dregs[2] &= 0xefffffff;                             /* Clear FPU flag if no FPU present */
    env->aregs[0] = MACROM_ADDR + UniversalInfo +
                    ldl_phys(MACROM_ADDR + UniversalInfo);    /* AddrMap */
    env->aregs[1] = MACROM_ADDR + UniversalInfo;           /* UniversalInfo */
    env->aregs[6] = boot_globs;                                  /* BootGlobs */
    env->aregs[7] = RAMBaseMac + 0x10000;                        /* Boot stack */

    return 0;
}

int do_macrom_simcall(CPUState *env)
{
    int ret = -1;
    int simcall;

    simcall = lduw_code(env->pc);

    switch(simcall) {
    case M68K_EMUL_OP_RESET:
        ret = do_simcall_reset(env);
        break;
    }

    if (ret == 0) {
        env->pc += 2;
    }

    return ret;
}
