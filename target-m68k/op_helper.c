/*
 *  M68K helper routines
 *
 *  Copyright (c) 2007 CodeSourcery
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */
#include "cpu.h"
#include "helper.h"

#if defined(CONFIG_USER_ONLY)

void m68k_cpu_do_interrupt(CPUState *cs)
{
    M68kCPU *cpu = M68K_CPU(cs);
    CPUM68KState *env = &cpu->env;

    env->exception_index = -1;
}

void do_interrupt_m68k_hardirq(CPUM68KState *env)
{
}

#define ldub_data ldub
#define lduw_data lduw
#define ldl_data ldl
#define stb_data stb
#define stw_data stw
#define stl_data stl
#else

extern int semihosting_enabled;

#include "exec/softmmu_exec.h"

#define MMUSUFFIX _mmu

#define SHIFT 0
#include "exec/softmmu_template.h"

#define SHIFT 1
#include "exec/softmmu_template.h"

#define SHIFT 2
#include "exec/softmmu_template.h"

#define SHIFT 3
#include "exec/softmmu_template.h"

/* Try to fill the TLB and return an exception if error. If retaddr is
   NULL, it means that the function was called in C code (i.e. not
   from generated code or from helper.c) */
void tlb_fill(CPUM68KState *env, target_ulong addr, int is_write, int mmu_idx,
              uintptr_t retaddr)
{
    int ret;

    ret = cpu_m68k_handle_mmu_fault(env, addr, is_write, mmu_idx);
    if (unlikely(ret)) {
        if (retaddr) {
            /* now we have a real cpu fault */
            cpu_restore_state(env, retaddr);
        }
        cpu_loop_exit(env);
    }
}

static void do_rte(CPUM68KState *env)
{
    uint32_t sp;

    sp = env->aregs[7];
    if (m68k_feature(env, M68K_FEATURE_M68000)) {
        uint16_t fmt;
throwaway:
        env->sr = cpu_lduw_kernel(env, sp);
        sp += 2;
        env->pc = cpu_ldl_kernel(env, sp);
        sp += 4;
        if (m68k_feature(env, M68K_FEATURE_QUAD_MULDIV)) {
            /*  all excepte 68000 */
            fmt = cpu_lduw_kernel(env, sp);
            sp += 2;
            switch (fmt >> 12) {
            case 0:
                break;
            case 1:
                env->aregs[7] = sp;
                m68k_switch_sp(env);
                goto throwaway;
            case 2:
            case 3:
                sp += 4;
                break;
            case 4:
                sp += 8;
                break;
            case 7:
                sp += 52;
                break;
            }
        }
    } else {
        uint32_t fmt;
        fmt = cpu_ldl_kernel(env, sp);
        env->pc = cpu_ldl_kernel(env, sp + 4);
        sp |= (fmt >> 28) & 3;
        env->sr = fmt & 0xffff;
        sp += 8;
    }
    env->aregs[7] = sp;
    m68k_switch_sp(env);

    /* restore CC state */
    env->cc_op = CC_OP_FLAGS;
    env->cc_dest = env->sr & 0xf;
    env->cc_x = (env->sr >> 4) & 1;
}

static inline void do_stack_frame(CPUM68KState *env, uint32_t *sp,
                                  uint16_t format, uint16_t sr,
                                  uint32_t addr, uint32_t retaddr)
{
    switch (format) {
    case 4:
        *sp -= 4;
        cpu_stl_kernel(env, *sp, env->pc);
        *sp -= 4;
        cpu_stl_kernel(env, *sp, addr);
        break;
    case 3:
    case 2:
        *sp -= 4;
        cpu_stl_kernel(env, *sp, addr);
        break;
    }
    *sp -= 2;
    cpu_stw_kernel(env, *sp, (format << 12) + (env->exception_index << 2));
    *sp -= 4;
    cpu_stl_kernel(env, *sp, retaddr);
    *sp -= 2;
    cpu_stw_kernel(env, *sp, sr);
}

static void do_interrupt_all(CPUM68KState *env, int is_hw)
{
    CPUState *cs;
    uint32_t sp;
    uint32_t fmt;
    uint32_t retaddr;
    uint32_t vector;
    int oldsr;

    fmt = 0;
    retaddr = env->pc;

    if (env->cc_op != CC_OP_FLAGS) {
        env->cc_dest = cpu_m68k_flush_flags(env, env->cc_op, env->cc_src,
                                            env->cc_dest);
        env->cc_op = CC_OP_FLAGS;
    }

    if (!is_hw) {
        switch (env->exception_index) {
        case EXCP_RTE:
            /* Return from an exception.  */
            do_rte(env);
            return;
        case EXCP_HALT_INSN:
            if (semihosting_enabled
                    && (env->sr & SR_S) != 0
                    && (env->pc & 3) == 0
                    && cpu_lduw_code(env, env->pc - 4) == 0x4e71
                    && cpu_ldl_code(env, env->pc) == 0x4e7bf000) {
                env->pc += 4;
                do_m68k_semihosting(env, env->dregs[0]);
                return;
            }
            cs = CPU(m68k_env_get_cpu(env));
            cs->halted = 1;
            env->exception_index = EXCP_HLT;
            cpu_loop_exit(env);
            return;
        }
    }

    env->sr = (env->sr & 0xffe0) | ((env->cc_x & 1) << 4) |
              (env->cc_dest & 0xf);

    vector = env->exception_index << 2;

    sp = env->aregs[7];

    if (qemu_loglevel_mask(CPU_LOG_INT)) {
        static int count = 0;
        static const char *name;
        name = "Unassigned";
        switch(env->exception_index) {
        case 0: name = "Reset Interrupt SP"; break;
        case 1: name = "Reset PC"; break;
        case 2: name = "Access Fault"; break;
        case 3: name = "Address Error"; break;
        case 4: name = "Illegal Instruction"; break;
        case 5: name = "Divide by Zero"; break;
        case 6: name = "CHK/CHK2"; break;
        case 7: name = "FTRAPcc, TRAPcc, TRAPV"; break;
        case 8: name = "Privilege Violation"; break;
        case 9: name = "Trace"; break;
        case 10: name = "A-Line"; break;
        case 11: name = "F-Line"; break;
        case 13: name = "Copro Protocol Violation"; break; /* 68020/030 only */
        case 14: name = "Format Error"; break;
        case 15: name = "Unitialized Interruot"; break;

        case 24: name = "Spurious Interrupt"; break;
        case 25: name = "Level 1 Interrupt"; break;
        case 26: name = "Level 2 Interrupt"; break;
        case 27: name = "Level 3 Interrupt"; break;
        case 28: name = "Level 4 Interrupt"; break;
        case 29: name = "Level 5 Interrupt"; break;
        case 30: name = "Level 6 Interrupt"; break;
        case 31: name = "Level 7 Interrupt"; break;

        case 32: name = "TRAP #0"; break;
        case 33: name = "TRAP #1"; break;
        case 34: name = "TRAP #2"; break;
        case 35: name = "TRAP #3"; break;
        case 36: name = "TRAP #4"; break;
        case 37: name = "TRAP #5"; break;
        case 38: name = "TRAP #6"; break;
        case 39: name = "TRAP #7"; break;
        case 40: name = "TRAP #8"; break;
        case 41: name = "TRAP #9"; break;
        case 42: name = "TRAP #10"; break;
        case 43: name = "TRAP #11"; break;
        case 44: name = "TRAP #12"; break;
        case 45: name = "TRAP #13"; break;
        case 46: name = "TRAP #14"; break;
        case 47: name = "TRAP #15"; break;

        case 48: name = "FP Branch/Set on unordered condition"; break;
        case 49: name = "FP Inexact Result"; break;
        case 50: name = "FP Divide by Zero"; break;
        case 51: name = "FP Underflow"; break;
        case 52: name = "FP Operand Error"; break;
        case 53: name = "FP Overflow"; break;
        case 54: name = "FP Signaling NAN"; break;
        case 55: name = "FP Unimplemented Data Type"; break;

        case 56: name = "MMU Configuration Error"; break; /* 68030/68851 only */
        case 57: name = "MMU Illegal Operation"; break; /* 68851 only */
        case 58: name = "MMU Access Level Violation"; break; /* 68851 only */

        case 64 ... 255: name = "User Defined Vector"; break;
        }
        qemu_log("INT %6d: %s(%#x) pc=%08x sp=%08x sr=%04x\n",
                 ++count, name, vector, env->pc, sp, env->sr);
    }

    /*
     * MC68040UM/AD,  chapter 9.3.10
     */

    /* "the processor first make an internal copy" */
    oldsr = env->sr;
    /* "set the mode to supervisor" */
    env->sr |= SR_S;
    /* "suppress tracing" */
    env->sr &= ~SR_T;
    /* "sets the processor interrupt mask" */
    if (is_hw) {
        env->sr |= (env->sr & ~SR_I) | (env->pending_level << SR_I_SHIFT);
    }

    m68k_switch_sp(env);
    sp = env->aregs[7];

    if (m68k_feature(env, M68K_FEATURE_M68000)) {
        sp &= ~1;
        if (env->exception_index == 2) {
            static int mmu_fault = 0;
            if (mmu_fault) {
                cpu_abort(env, "DOUBLE MMU FAULT\n");
            }
            mmu_fault = 1;
            sp -= 4;
            cpu_stl_kernel(env, sp, 0); /* push data 3 */
            sp -= 4;
            cpu_stl_kernel(env, sp, 0); /* push data 2 */
            sp -= 4;
            cpu_stl_kernel(env, sp, 0); /* push data 1 */
            sp -= 4;
            cpu_stl_kernel(env, sp, 0); /* write back 1 / push data 0 */
            sp -= 4;
            cpu_stl_kernel(env, sp, 0); /* write back 1 address */
            sp -= 4;
            cpu_stl_kernel(env, sp, 0); /* write back 2 data */
            sp -= 4;
            cpu_stl_kernel(env, sp, 0); /* write back 2 address */
            sp -= 4;
            cpu_stl_kernel(env, sp, 0); /* write back 3 data */
            sp -= 4;
            cpu_stl_kernel(env, sp, env->mmu.ar); /* write back 3 address */
            sp -= 4;
            cpu_stl_kernel(env, sp, env->mmu.ar); /* fault address */
            sp -= 2;
            cpu_stw_kernel(env, sp, 0); /* write back 1 status */
            sp -= 2;
            cpu_stw_kernel(env, sp, 0); /* write back 2 status */
            sp -= 2;
            cpu_stw_kernel(env, sp, 0); /* write back 3 status */
            sp -= 2;
            cpu_stw_kernel(env, sp, env->mmu.ssw); /* special status word */
            sp -= 4;
            cpu_stl_kernel(env, sp, env->mmu.ar); /* effective address */
            do_stack_frame(env, &sp, 7, oldsr, 0, retaddr);
            mmu_fault = 0;
            if (qemu_loglevel_mask(CPU_LOG_INT)) {
                qemu_log("            ssw:  %08x ea:   %08x sfc:  %d    dfc: %d\n",
                         env->mmu.ssw, env->mmu.ar, env->sfc, env->dfc);
            }
        } else if (env->exception_index == 3) {
            do_stack_frame(env, &sp, 2, oldsr, 0, retaddr);
        } else if (env->exception_index == 4 ||
                   env->exception_index == 5 ||
                   env->exception_index == 6 ||
                   env->exception_index == 7 ||
                   env->exception_index == 9) {
            /* FIXME: addr is not only env->pc */
            do_stack_frame(env, &sp, 2, oldsr, env->pc, retaddr);
        } else if (is_hw && oldsr & SR_M && env->exception_index >= 24
                         && env->exception_index < 32) {
            do_stack_frame(env, &sp, 0, oldsr, 0, retaddr);
            oldsr = env->sr;
            env->sr &= ~SR_M;
            env->aregs[7] = sp;
            m68k_switch_sp(env);
            sp = env->aregs[7] & ~1;
            do_stack_frame(env, &sp, 1, oldsr, 0, retaddr);
        } else {
            do_stack_frame(env, &sp, 0, oldsr, 0, retaddr);
        }
    } else {
        fmt |= 0x40000000;
        fmt |= (sp & 3) << 28;
        fmt |= vector << 16;
        fmt |= oldsr;

        sp &= ~3;
        sp -= 4;
        cpu_stl_kernel(env, sp, retaddr);
        sp -= 4;
        cpu_stl_kernel(env, sp, fmt);
    }

    env->aregs[7] = sp;
    /* Jump to vector.  */
    env->pc = cpu_ldl_kernel(env, env->vbr + vector);
}

void m68k_cpu_do_interrupt(CPUState *cs)
{
    M68kCPU *cpu = M68K_CPU(cs);
    CPUM68KState *env = &cpu->env;

    do_interrupt_all(env, 0);
}

void do_interrupt_m68k_hardirq(CPUM68KState *env)
{
    do_interrupt_all(env, 1);
}

void m68k_cpu_unassigned_access(CPUState *cs, hwaddr addr,
                           bool is_write, bool is_exec, int is_asi, unsigned size)
{
    M68kCPU *cpu = M68K_CPU(cs);
    CPUM68KState *env = &cpu->env;
#ifdef DEBUG_UNASSIGNED
    qemu_log_mask(CPU_LOG_INT, "Unassigned " TARGET_FMT_plx " wr=%d exe=%d\n",
             addr, is_write, is_exec);
#endif
    if (env == NULL) {
        /* when called from gdb, env is NULL */
        return;
    }

    env->mmu.mmusr = 0;
    env->mmu.ssw |= M68K_ATC_040;
    /* FIXME: manage MMU table access error */
    if (env->sr & SR_S) /* SUPERVISOR */
        env->mmu.ssw |= 4;
    if (is_exec) /* instruction or data */
        env->mmu.ssw |= 2;
    else
        env->mmu.ssw |= 1;
    switch (size) {
    case 1:
        env->mmu.ssw |= M68K_BA_SIZE_BYTE;
        break;
    case 2:
        env->mmu.ssw |= M68K_BA_SIZE_WORD;
        break;
    case 4:
        env->mmu.ssw |= M68K_BA_SIZE_LONG;
        break;
    }

    if (!is_write) {
        env->mmu.ssw |= M68K_RW_040;
    }

    env->mmu.ar = addr;

    env->exception_index = EXCP_ACCESS;
    cpu_loop_exit(env);
}
#endif

static void raise_exception(CPUM68KState *env, int tt)
{
    env->exception_index = tt;
    cpu_loop_exit(env);
}

/* load from a bitfield */

uint64_t HELPER(bitfield_load)(CPUM68KState *env, uint32_t addr, uint32_t offset,
                               uint32_t width)
{
    uint64_t bitfield = 0;
    int size;

    size = (offset + width + 7) >> 3;
    switch(size) {
    case 1:
        bitfield = cpu_ldub_data(env, addr);
        bitfield <<= 56;
        break;
    case 2:
        bitfield = cpu_lduw_data(env, addr);
        bitfield <<= 48;
        break;
    case 3:
        bitfield = cpu_lduw_data(env, addr);
        bitfield <<= 8;
        bitfield |= cpu_ldub_data(env, addr + 2);
        bitfield <<= 40;
        break;
    case 4:
        bitfield = cpu_ldl_data(env, addr);
        bitfield <<= 32;
        break;
    case 5:
        bitfield = cpu_ldl_data(env, addr);
        bitfield <<= 8;
        bitfield |= cpu_ldub_data(env, addr + 4);
        bitfield <<= 24;
        break;
    }

    return bitfield;
}

/* store to a bitfield */

void HELPER(bitfield_store)(CPUM68KState *env, uint32_t addr, uint32_t offset,
                            uint32_t width, uint64_t bitfield)
{
    int size;

    size = (offset + width + 7) >> 3;
    switch(size) {
    case 1:
        cpu_stb_data(env, addr, bitfield >> 56);
        break;
    case 2:
        cpu_stw_data(env, addr, bitfield >> 48);
        break;
    case 3:
        cpu_stw_data(env, addr, bitfield >> 48);
        cpu_stb_data(env, addr + 2, bitfield >> 40);
        break;
    case 4:
        cpu_stl_data(env, addr, bitfield >> 32);
        break;
    case 5:
        cpu_stl_data(env, addr, bitfield >> 32);
        cpu_stb_data(env, addr + 4, bitfield >> 24);
        break;
    }
}

void HELPER(raise_exception)(CPUM68KState *env, uint32_t tt)
{
    raise_exception(env, tt);
}

void HELPER(divu)(CPUM68KState *env, uint32_t word)
{
    uint32_t num;
    uint32_t den;
    uint32_t quot;
    uint32_t rem;
    uint32_t flags;

    num = env->div1;
    den = env->div2;
    /* ??? This needs to make sure the throwing location is accurate.  */
    if (den == 0) {
        raise_exception(env, EXCP_DIV0);
    }
    quot = num / den;
    rem = num % den;
    flags = 0;
    /* Avoid using a PARAM1 of zero.  This breaks dyngen because it uses
       the address of a symbol, and gcc knows symbols can't have address
       zero.  */
    if (word && quot > 0xffff) {
	/* real 68040 keep Z and N on overflow,
         * whereas documentation says "undefined"
         */
        flags |= CCF_V | (env->cc_dest & (CCF_Z|CCF_N));
    } else {
        if (quot == 0)
            flags |= CCF_Z;
        else if ((int16_t)quot < 0)
            flags |= CCF_N;
    }

    env->div1 = quot;
    env->div2 = rem;
    env->cc_dest = flags;
}

void HELPER(divs)(CPUM68KState *env, uint32_t word)
{
    int32_t num;
    int32_t den;
    int32_t quot;
    int32_t rem;
    int32_t flags;

    num = env->div1;
    den = env->div2;
    if (den == 0) {
        raise_exception(env, EXCP_DIV0);
    }
    quot = num / den;
    rem = num % den;
    flags = 0;
    if (word && quot != (int16_t)quot) {
	/* real 68040 keep Z and N on overflow,
         * whereas documentation says "undefined"
         */
        flags |= CCF_V | (env->cc_dest & (CCF_Z|CCF_N));
    } else {
        if (quot == 0)
            flags |= CCF_Z;
        else if ((int16_t)quot < 0)
            flags |= CCF_N;
    }

    env->div1 = quot;
    env->div2 = rem;
    env->cc_dest = flags;
}

void HELPER(divu64)(CPUM68KState *env)
{
    uint32_t num;
    uint32_t den;
    uint64_t quot;
    uint32_t rem;
    uint32_t flags;
    uint64_t quad;

    num = env->div1;
    den = env->div2;
    /* ??? This needs to make sure the throwing location is accurate.  */
    if (den == 0)
        raise_exception(env, EXCP_DIV0);
    quad = num | ((uint64_t)env->quadh << 32);
    quot = quad / den;
    rem = quad % den;
    if (quot > 0xffffffffULL) {
        flags = (env->cc_dest & ~ CCF_C) | CCF_V;
    } else {
        flags = 0;
        if (quot == 0)
            flags |= CCF_Z;
        else if ((int32_t)quot < 0)
            flags |= CCF_N;
        env->div1 = quot;
        env->quadh = rem;
    }
    env->cc_dest = flags;
}

void HELPER(divs64)(CPUM68KState *env)
{
    uint32_t num;
    int32_t den;
    int64_t quot;
    int32_t rem;
    int32_t flags;
    int64_t quad;

    num = env->div1;
    den = env->div2;
    if (den == 0)
        raise_exception(env, EXCP_DIV0);
    quad = num | ((int64_t)env->quadh << 32);
    quot = quad / (int64_t)den;
    rem = quad % (int64_t)den;

    if ((quot & 0xffffffff80000000ULL) &&
        (quot & 0xffffffff80000000ULL) != 0xffffffff80000000ULL) {
	flags = (env->cc_dest & ~ CCF_C) | CCF_V;
    } else {
        flags = 0;
        if (quot == 0)
	    flags |= CCF_Z;
        else if ((int32_t)quot < 0)
	    flags |= CCF_N;
        env->div1 = quot;
        env->quadh = rem;
    }
    env->cc_dest = flags;
}

uint32_t HELPER(mulu32_cc)(CPUM68KState *env, uint32_t op1, uint32_t op2)
{
    uint64_t res = (uint32_t)op1 * op2;
    uint32_t flags;

    flags = 0;
    if (res >> 32)
       flags |= CCF_V;
    if ((uint32_t)res == 0)
       flags |= CCF_Z;
    if ((int32_t)res < 0)
       flags |= CCF_N;
    env->cc_dest = flags;

    return res;
}

uint32_t HELPER(muls32_cc)(CPUM68KState *env, uint32_t op1, uint32_t op2)
{
    int64_t res = (int32_t)op1 * (int32_t)op2;
    uint32_t flags;

    flags = 0;
    if (res != (int64_t)(int32_t)res)
       flags |= CCF_V;
    if ((uint32_t)res == 0)
       flags |= CCF_Z;
    if ((int32_t)res < 0)
       flags |= CCF_N;
    env->cc_dest = flags;

    return res;
}

uint32_t HELPER(mulu64)(CPUM68KState *env, uint32_t op1, uint32_t op2)
{
    uint64_t res = (uint64_t)op1 * op2;
    uint32_t flags;

    env->quadh = res >> 32;
    flags = 0;
    if (res == 0)
       flags |= CCF_Z;
    if ((int64_t)res < 0)
       flags |= CCF_N;
    env->cc_dest = flags;

    return res;
}

uint32_t HELPER(muls64)(CPUM68KState *env, uint32_t op1, uint32_t op2)
{
    int64_t res = (uint64_t)(int32_t)op1 * (int32_t)op2;
    uint32_t flags;

    env->quadh = res >> 32;
    flags = 0;
    if (res == 0)
       flags |= CCF_Z;
    if (res < 0)
       flags |= CCF_N;
    env->cc_dest = flags;

    return res;
}
