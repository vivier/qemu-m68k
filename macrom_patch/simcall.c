#include <stdio.h>

#include "cpu.h"
#include "dyngen-exec.h"
#include "softmmu_exec.h"
#include "macrom.h"
#include "utils.h"
#include "xpram.h"
#include "timer.h"

#define RAMBaseMac 0

/*
 * MacOS reset
 */

static int do_simcall_reset(CPUState *env)
{
    int i;
    uint32 boot_globs = RAMBaseMac + ram_size - 0x1c;

    TimerReset();
#if 0
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

/*
 * Clock/PRAM operations
 */

static int do_simcall_clknomem(CPUState *env)
{
    int localtalk, is_read;
    uint8_t reg;
    uint32_t t;
    uint8_t b;

    is_read = env->dregs[1] & 0x80;

    if ((env->dregs[1] & 0x78) == 0x38) {

        /* XPRAM */

        reg = ((env->dregs[1] << 5) & 0xe0) |
              ((env->dregs[1] >> 10) & 0x1f);

        if (is_read) {

            env->dregs[2] = XPRAM[reg];

            /* LocalTalk enabled? */

            localtalk = !(XPRAM[0xe0] || XPRAM[0xe1]);

            switch (reg) {
            case 0x08:
                if (rom_version != MACOS_ROM_VERSION_32) {
                    env->dregs[2] &= 0xf8;
                }
                break;
            case 0x8a: /* 32bit mode is always enabled */
                env->dregs[2] |= 0x05;
                break;

            /* Disable LocalTalk (use EtherTalk instead) */

            case 0xe0:
                if (localtalk) {
                    env->dregs[2] = 0x00;
                }
                break;
            case 0xe1:
                if (localtalk) {
                    env->dregs[2] = 0xf1;
                }
                break;
            case 0xe2:
                if (localtalk) {
                    env->dregs[2] = 0x00;
                }
                break;
            case 0xe3:
                if (localtalk) {
                    env->dregs[2] = 0x0a;
                }
                break;
            }
        } else { /* !is_read */
            if (reg == 0x8a && !TwentyFourBitAddressing) {
                /* 32bit mode is always enabled if possible */
                env->dregs[2] |= 0x05;
            }
            XPRAM[reg] = env->dregs[2];
        }
    } else {
        /* PRAM, RTC and other clock registers */

        reg = (env->dregs[1] >> 2) & 0x1f;

        if (reg >= 0x10 || (reg >= 0x08 && reg < 0x0c)) {
            if (is_read) {
                env->dregs[2] = XPRAM[reg];
            } else {
                XPRAM[reg] = env->dregs[2];
            }
        } else if (reg < 0x08 && is_read) {
            t = convert_to_mac_time(time(NULL));
            b = t;
            switch (reg & 3) {
            case 1:
                b = t >> 8;
                break;
            case 2:
                b = t >> 16;
                break;
            case 3:
                b = t >> 24;
                break;
            }
            env->dregs[2] = b;
        }
    }
    env->dregs[0] = 0;
    env->dregs[1] = env->dregs[2];

    return 0;
}

static int do_simcall_patch_boot_globs(CPUState *env)
{
    stl_phys(env->aregs[4] - 20, ram_size); /* MemTop */
    stb_phys(env->aregs[4] - 26, 0); /* No MMU */
    stb_phys(env->aregs[4] - 25,
             ldub_phys(env->aregs[4] - 25) | 1); /* No MMU */
    env->aregs[6] = ram_size; /* MemTop */

    return 0;
}

static int do_simcall_fix_memsize(CPUState *env)
{
    uint32_t diff;

    /* Difference between logical and physical size */

    diff = ldl_phys(0x1ef8) - ldl_phys(0x1ef4);

    stl_phys(0x1ef8, ram_size);
    stl_phys(0x1ef4, ram_size - diff);

    return 0;
}

static int do_simcall_instime(CPUState *env)
{
    env->dregs[0] = InsTime(env->aregs[0], env->dregs[1]);

    return 0;
}

static int do_simcall_rmvtime(CPUState *env)
{
    env->dregs[0] = RmvTime(env->aregs[0]);

    return 0;
}

static int do_simcall_primetime(CPUState *env)
{
    env->dregs[0] = PrimeTime(env->aregs[0], env->dregs[0]);

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
    case M68K_EMUL_OP_CLKNOMEM:
        ret = do_simcall_clknomem(env);
        break;
    case M68K_EMUL_OP_PATCH_BOOT_GLOBS:
        ret = do_simcall_patch_boot_globs(env);
        break;
    case M68K_EMUL_OP_FIX_MEMSIZE:
        ret = do_simcall_fix_memsize(env);
        break;
    case M68K_EMUL_OP_INSTIME:
        ret = do_simcall_instime(env);
        break;
    case M68K_EMUL_OP_RMVTIME:
        ret = do_simcall_rmvtime(env);
        break;
    case M68K_EMUL_OP_PRIMETIME:
        ret = do_simcall_primetime(env);
        break;
    }

    if (ret == 0) {
        env->pc += 2;
    }

    return ret;
}
