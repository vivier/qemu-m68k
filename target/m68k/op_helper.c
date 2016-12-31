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
#include "qemu/osdep.h"
#include "cpu.h"
#include "exec/helper-proto.h"
#include "exec/exec-all.h"
#include "exec/cpu_ldst.h"
#include "exec/semihost.h"

#if defined(CONFIG_USER_ONLY)

void m68k_cpu_do_interrupt(CPUState *cs)
{
    cs->exception_index = -1;
}

static inline void do_interrupt_m68k_hardirq(CPUM68KState *env)
{
}

#else

extern void qemu_system_reset_request(void);

/* Try to fill the TLB and return an exception if error. If retaddr is
   NULL, it means that the function was called in C code (i.e. not
   from generated code or from helper.c) */
void tlb_fill(CPUState *cs, target_ulong addr, int size,
              MMUAccessType access_type, int mmu_idx, uintptr_t retaddr)
{
    int ret;

    ret = m68k_cpu_handle_mmu_fault(cs, addr, size, access_type, mmu_idx);
    if (unlikely(ret)) {
        if (retaddr) {
            /* now we have a real cpu fault */
            cpu_restore_state(cs, retaddr);
        }
        cpu_loop_exit(cs);
    }
}

static void do_rte(CPUM68KState *env)
{
    uint32_t sp;
    uint16_t sr;

    sp = env->aregs[7];
    if (m68k_feature(env, M68K_FEATURE_M68000)) {
        uint16_t fmt;
throwaway:
        sr = cpu_lduw_kernel(env, sp);
        sp += 2;
        env->pc = cpu_ldl_kernel(env, sp);
        sp += 4;
        if (m68k_feature(env, M68K_FEATURE_QUAD_MULDIV)) {
            /*  all except 68000 */
            fmt = cpu_lduw_kernel(env, sp);
            sp += 2;
            switch (fmt >> 12) {
            case 0:
                break;
            case 1:
                env->aregs[7] = sp;
                helper_set_sr(env, sr);
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
        sr = fmt;
        sp += 8;
    }
    env->aregs[7] = sp;
    helper_set_sr(env, sr);
}

static inline void do_stack_frame(CPUM68KState *env, uint32_t *sp,
                                  uint16_t format, uint16_t sr,
                                  uint32_t addr, uint32_t retaddr)
{
    CPUState *cs = CPU(m68k_env_get_cpu(env));
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
    cpu_stw_kernel(env, *sp, (format << 12) + (cs->exception_index << 2));
    *sp -= 4;
    cpu_stl_kernel(env, *sp, retaddr);
    *sp -= 2;
    cpu_stw_kernel(env, *sp, sr);
}

static void do_interrupt_all(CPUM68KState *env, int is_hw)
{
    CPUState *cs = CPU(m68k_env_get_cpu(env));
    uint32_t sp;
    uint32_t fmt;
    uint32_t retaddr;
    uint32_t vector;
    uint16_t sr, oldsr;

    fmt = 0;
    retaddr = env->pc;

    if (!is_hw) {
        switch (cs->exception_index) {
        case EXCP_RTE:
            /* Return from an exception.  */
            do_rte(env);
            return;
        case EXCP_HALT_INSN:
            if (semihosting_enabled()
                    && (env->sr & SR_S) != 0
                    && (env->pc & 3) == 0
                    && cpu_lduw_code(env, env->pc - 4) == 0x4e71
                    && cpu_ldl_code(env, env->pc) == 0x4e7bf000) {
                env->pc += 4;
                do_m68k_semihosting(env, env->dregs[0]);
                return;
            }
            cs->halted = 1;
            cs->exception_index = EXCP_HLT;
            cpu_loop_exit(cs);
            return;
        }
        if (cs->exception_index >= EXCP_TRAP0
            && cs->exception_index <= EXCP_TRAP15) {
            /* Move the PC after the trap instruction.  */
            retaddr += 2;
        }
    }

    vector = cs->exception_index << 2;

    if (qemu_loglevel_mask(CPU_LOG_INT)) {
        static int count = 0;
        static const char *name;
        name = "Unassigned";
        switch(cs->exception_index) {
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
                 ++count, name, vector, env->pc, env->aregs[7], env->sr);
    }

    /*
     * MC68040UM/AD,  chapter 9.3.10
     */

    sr = env->sr | cpu_m68k_get_ccr(env);
    /* "the processor first make an internal copy" */
    oldsr = sr;
    /* "set the mode to supervisor" */
    sr |= SR_S;
    /* "suppress tracing" */
    sr &= ~SR_T;
    /* "sets the processor interrupt mask" */
    if (is_hw) {
        sr |= (env->sr & ~SR_I) | (env->pending_level << SR_I_SHIFT);
    }
    helper_set_sr(env, sr);
    sp = env->aregs[7];

    if (m68k_feature(env, M68K_FEATURE_M68000)) {
        sp &= ~1;
        if (cs->exception_index == 2) {
            static int mmu_fault = 0;
            if (mmu_fault) {
                cpu_abort(cs, "DOUBLE MMU FAULT\n");
            }
            /*
             * horrible hack to reset macintosh Quadra 800.
             * To reset, linux calls ROM entry point 0x4080000a
             * as we have no ROM, this fails. We try to trap this here
             */
            if (env->mmu.ar == 0x4080000a &&
                (env->mmu.ssw & M68K_TM_040_SUPER)) {
                qemu_system_reset_request();
                return;
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
        } else if (cs->exception_index == 3) {
            do_stack_frame(env, &sp, 2, oldsr, 0, retaddr);
        } else if (cs->exception_index == 4 ||
                   cs->exception_index == 5 ||
                   cs->exception_index == 6 ||
                   cs->exception_index == 7 ||
                   cs->exception_index == 9) {
            /* FIXME: addr is not only env->pc */
            do_stack_frame(env, &sp, 2, oldsr, env->pc, retaddr);
        } else if (is_hw && oldsr & SR_M && cs->exception_index >= 24
                         && cs->exception_index < 32) {
            do_stack_frame(env, &sp, 0, oldsr, 0, retaddr);
            oldsr = sr;
            env->aregs[7] = sp;
            helper_set_sr(env, sr &= ~SR_M);
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

static inline void do_interrupt_m68k_hardirq(CPUM68KState *env)
{
    do_interrupt_all(env, 1);
}
#endif

bool m68k_cpu_exec_interrupt(CPUState *cs, int interrupt_request)
{
    M68kCPU *cpu = M68K_CPU(cs);
    CPUM68KState *env = &cpu->env;

    if (interrupt_request & CPU_INTERRUPT_HARD
        && (((env->sr & SR_I) >> SR_I_SHIFT) < env->pending_level
            || env->pending_level == 7)) {
        /* Real hardware gets the interrupt vector via an IACK cycle
           at this point.  Current emulated hardware doesn't rely on
           this, so we provide/save the vector when the interrupt is
           first signalled.  */
        cs->exception_index = env->pending_vector;
        do_interrupt_m68k_hardirq(env);
        return true;
    }
    return false;
}

static void raise_exception_ra(CPUM68KState *env, int tt, uintptr_t raddr)
{
    CPUState *cs = CPU(m68k_env_get_cpu(env));

    cs->exception_index = tt;
    cpu_loop_exit_restore(cs, raddr);
}

static void raise_exception(CPUM68KState *env, int tt)
{
    raise_exception_ra(env, tt, 0);
}

void HELPER(raise_exception)(CPUM68KState *env, uint32_t tt)
{
    raise_exception(env, tt);
}

void HELPER(divuw)(CPUM68KState *env, int destr, uint32_t den)
{
    uint32_t num = env->dregs[destr];
    uint32_t quot, rem;

    if (den == 0) {
        raise_exception_ra(env, EXCP_DIV0, GETPC());
    }
    quot = num / den;
    rem = num % den;

    env->cc_c = 0; /* always cleared, even if overflow */
    if (quot > 0xffff) {
        env->cc_v = -1;
        /* real 68040 keeps N and unset Z on overflow,
         * whereas documentation says "undefined"
         */
        env->cc_z = 1;
        return;
    }
    env->dregs[destr] = deposit32(quot, 16, 16, rem);
    env->cc_z = (int16_t)quot;
    env->cc_n = (int16_t)quot;
    env->cc_v = 0;
}

void HELPER(divsw)(CPUM68KState *env, int destr, int32_t den)
{
    int32_t num = env->dregs[destr];
    uint32_t quot, rem;

    if (den == 0) {
        raise_exception_ra(env, EXCP_DIV0, GETPC());
    }
    quot = num / den;
    rem = num % den;

    env->cc_c = 0; /* always cleared, even if overflow */
    if (quot != (int16_t)quot) {
        env->cc_v = -1;
        /* nothing else is modified */
        /* real 68040 keeps N and unset Z on overflow,
         * whereas documentation says "undefined"
         */
        env->cc_z = 1;
        return;
    }
    env->dregs[destr] = deposit32(quot, 16, 16, rem);
    env->cc_z = (int16_t)quot;
    env->cc_n = (int16_t)quot;
    env->cc_v = 0;
}

void HELPER(divul)(CPUM68KState *env, int numr, int regr, uint32_t den)
{
    uint32_t num = env->dregs[numr];
    uint32_t quot, rem;

    if (den == 0) {
        raise_exception_ra(env, EXCP_DIV0, GETPC());
    }
    quot = num / den;
    rem = num % den;

    env->cc_c = 0;
    env->cc_z = quot;
    env->cc_n = quot;
    env->cc_v = 0;

    if (m68k_feature(env, M68K_FEATURE_CF_ISA_A)) {
        if (numr == regr) {
            env->dregs[numr] = quot;
        } else {
            env->dregs[regr] = rem;
        }
    } else {
        env->dregs[regr] = rem;
        env->dregs[numr] = quot;
    }
}

void HELPER(divsl)(CPUM68KState *env, int numr, int regr, int32_t den)
{
    int32_t num = env->dregs[numr];
    int32_t quot, rem;

    if (den == 0) {
        raise_exception_ra(env, EXCP_DIV0, GETPC());
    }
    quot = num / den;
    rem = num % den;

    env->cc_c = 0;
    env->cc_z = quot;
    env->cc_n = quot;
    env->cc_v = 0;

    if (m68k_feature(env, M68K_FEATURE_CF_ISA_A)) {
        if (numr == regr) {
            env->dregs[numr] = quot;
        } else {
            env->dregs[regr] = rem;
        }
    } else {
        env->dregs[regr] = rem;
        env->dregs[numr] = quot;
    }
}

void HELPER(divull)(CPUM68KState *env, int numr, int regr, uint32_t den)
{
    uint64_t num = deposit64(env->dregs[numr], 32, 32, env->dregs[regr]);
    uint64_t quot;
    uint32_t rem;

    if (den == 0) {
        raise_exception_ra(env, EXCP_DIV0, GETPC());
    }
    quot = num / den;
    rem = num % den;

    env->cc_c = 0; /* always cleared, even if overflow */
    if (quot > 0xffffffffULL) {
        env->cc_v = -1;
        /* real 68040 keeps N and unset Z on overflow,
         * whereas documentation says "undefined"
         */
        env->cc_z = 1;
        return;
    }
    env->cc_z = quot;
    env->cc_n = quot;
    env->cc_v = 0;

    /*
     * If Dq and Dr are the same, the quotient is returned.
     * therefore we set Dq last.
     */

    env->dregs[regr] = rem;
    env->dregs[numr] = quot;
}

void HELPER(divsll)(CPUM68KState *env, int numr, int regr, int32_t den)
{
    int64_t num = deposit64(env->dregs[numr], 32, 32, env->dregs[regr]);
    int64_t quot;
    int32_t rem;

    if (den == 0) {
        raise_exception_ra(env, EXCP_DIV0, GETPC());
    }
    quot = num / den;
    rem = num % den;

    env->cc_c = 0; /* always cleared, even if overflow */
    if (quot != (int32_t)quot) {
        env->cc_v = -1;
        /* real 68040 keeps N and unset Z on overflow,
         * whereas documentation says "undefined"
         */
        env->cc_z = 1;
        return;
    }
    env->cc_z = quot;
    env->cc_n = quot;
    env->cc_v = 0;

    /*
     * If Dq and Dr are the same, the quotient is returned.
     * therefore we set Dq last.
     */

    env->dregs[regr] = rem;
    env->dregs[numr] = quot;
}

void HELPER(cas2w)(CPUM68KState *env, uint32_t regs, uint32_t a1, uint32_t a2)
{
    uint32_t Dc1 = extract32(regs, 9, 3);
    uint32_t Dc2 = extract32(regs, 6, 3);
    uint32_t Du1 = extract32(regs, 3, 3);
    uint32_t Du2 = extract32(regs, 0, 3);
    int16_t c1 = env->dregs[Dc1];
    int16_t c2 = env->dregs[Dc2];
    int16_t u1 = env->dregs[Du1];
    int16_t u2 = env->dregs[Du2];
    int16_t l1, l2;
    uintptr_t ra = GETPC();

    if (parallel_cpus) {
        /* Tell the main loop we need to serialize this insn.  */
        cpu_loop_exit_atomic(ENV_GET_CPU(env), ra);
    } else {
        /* We're executing in a serial context -- no need to be atomic.  */
        l1 = cpu_lduw_data_ra(env, a1, ra);
        l2 = cpu_lduw_data_ra(env, a2, ra);
        if (l1 == c1 && l2 == c2) {
            cpu_stw_data_ra(env, a1, u1, ra);
            cpu_stw_data_ra(env, a2, u2, ra);
        }
    }

    if (c1 != l1) {
        env->cc_n = l1;
        env->cc_v = c1;
    } else {
        env->cc_n = l2;
        env->cc_v = c2;
    }
    env->cc_op = CC_OP_CMPW;
    env->dregs[Dc1] = deposit32(env->dregs[Dc1], 0, 16, l1);
    env->dregs[Dc2] = deposit32(env->dregs[Dc2], 0, 16, l2);
}

void HELPER(cas2l)(CPUM68KState *env, uint32_t regs, uint32_t a1, uint32_t a2)
{
    uint32_t Dc1 = extract32(regs, 9, 3);
    uint32_t Dc2 = extract32(regs, 6, 3);
    uint32_t Du1 = extract32(regs, 3, 3);
    uint32_t Du2 = extract32(regs, 0, 3);
    uint32_t c1 = env->dregs[Dc1];
    uint32_t c2 = env->dregs[Dc2];
    uint32_t u1 = env->dregs[Du1];
    uint32_t u2 = env->dregs[Du2];
    uint32_t l1, l2;
    uintptr_t ra = GETPC();
#if defined(CONFIG_ATOMIC64) && !defined(CONFIG_USER_ONLY)
    int mmu_idx = cpu_mmu_index(env, 0);
    TCGMemOpIdx oi;
#endif

    if (parallel_cpus) {
        /* We're executing in a parallel context -- must be atomic.  */
#ifdef CONFIG_ATOMIC64
        uint64_t c, u, l;
        if ((a1 & 7) == 0 && a2 == a1 + 4) {
            c = deposit64(c2, 32, 32, c1);
            u = deposit64(u2, 32, 32, u1);
#ifdef CONFIG_USER_ONLY
            l = helper_atomic_cmpxchgq_be(env, a1, c, u);
#else
            oi = make_memop_idx(MO_BEQ, mmu_idx);
            l = helper_atomic_cmpxchgq_be_mmu(env, a1, c, u, oi, ra);
#endif
            l1 = l >> 32;
            l2 = l;
        } else if ((a2 & 7) == 0 && a1 == a2 + 4) {
            c = deposit64(c1, 32, 32, c2);
            u = deposit64(u1, 32, 32, u2);
#ifdef CONFIG_USER_ONLY
            l = helper_atomic_cmpxchgq_be(env, a2, c, u);
#else
            oi = make_memop_idx(MO_BEQ, mmu_idx);
            l = helper_atomic_cmpxchgq_be_mmu(env, a2, c, u, oi, ra);
#endif
            l2 = l >> 32;
            l1 = l;
        } else
#endif
        {
            /* Tell the main loop we need to serialize this insn.  */
            cpu_loop_exit_atomic(ENV_GET_CPU(env), ra);
        }
    } else {
        /* We're executing in a serial context -- no need to be atomic.  */
        l1 = cpu_ldl_data_ra(env, a1, ra);
        l2 = cpu_ldl_data_ra(env, a2, ra);
        if (l1 == c1 && l2 == c2) {
            cpu_stl_data_ra(env, a1, u1, ra);
            cpu_stl_data_ra(env, a2, u2, ra);
        }
    }

    if (c1 != l1) {
        env->cc_n = l1;
        env->cc_v = c1;
    } else {
        env->cc_n = l2;
        env->cc_v = c2;
    }
    env->cc_op = CC_OP_CMPL;
    env->dregs[Dc1] = l1;
    env->dregs[Dc2] = l2;
}

struct bf_data {
    uint32_t addr;
    uint32_t bofs;
    uint32_t blen;
    uint32_t len;
};

static struct bf_data bf_prep(uint32_t addr, int32_t ofs, uint32_t len)
{
    int bofs, blen;

    /* Bound length; map 0 to 32.  */
    len = ((len - 1) & 31) + 1;

    /* Note that ofs is signed.  */
    addr += ofs / 8;
    bofs = ofs % 8;
    if (bofs < 0) {
        bofs += 8;
        addr -= 1;
    }

    /* Compute the number of bytes required (minus one) to
       satisfy the bitfield.  */
    blen = (bofs + len - 1) / 8;

    /* Canonicalize the bit offset for data loaded into a 64-bit big-endian
       word.  For the cases where BLEN is not a power of 2, adjust ADDR so
       that we can use the next power of two sized load without crossing a
       page boundary, unless the field itself crosses the boundary.  */
    switch (blen) {
    case 0:
        bofs += 56;
        break;
    case 1:
        bofs += 48;
        break;
    case 2:
        if (addr & 1) {
            bofs += 8;
            addr -= 1;
        }
        /* fallthru */
    case 3:
        bofs += 32;
        break;
    case 4:
        if (addr & 3) {
            bofs += 8 * (addr & 3);
            addr &= -4;
        }
        break;
    default:
        g_assert_not_reached();
    }

    return (struct bf_data){
        .addr = addr,
        .bofs = bofs,
        .blen = blen,
        .len = len,
    };
}

static uint64_t bf_load(CPUM68KState *env, uint32_t addr, int blen,
                        uintptr_t ra)
{
    switch (blen) {
    case 0:
        return cpu_ldub_data_ra(env, addr, ra);
    case 1:
        return cpu_lduw_data_ra(env, addr, ra);
    case 2:
    case 3:
        return cpu_ldl_data_ra(env, addr, ra);
    case 4:
        return cpu_ldq_data_ra(env, addr, ra);
    default:
        g_assert_not_reached();
    }
}

static void bf_store(CPUM68KState *env, uint32_t addr, int blen,
                     uint64_t data, uintptr_t ra)
{
    switch (blen) {
    case 0:
        cpu_stb_data_ra(env, addr, data, ra);
        break;
    case 1:
        cpu_stw_data_ra(env, addr, data, ra);
        break;
    case 2:
    case 3:
        cpu_stl_data_ra(env, addr, data, ra);
        break;
    case 4:
        cpu_stq_data_ra(env, addr, data, ra);
        break;
    default:
        g_assert_not_reached();
    }
}

uint32_t HELPER(bfexts_mem)(CPUM68KState *env, uint32_t addr,
                            int32_t ofs, uint32_t len)
{
    uintptr_t ra = GETPC();
    struct bf_data d = bf_prep(addr, ofs, len);
    uint64_t data = bf_load(env, d.addr, d.blen, ra);

    return (int64_t)(data << d.bofs) >> (64 - d.len);
}

uint64_t HELPER(bfextu_mem)(CPUM68KState *env, uint32_t addr,
                            int32_t ofs, uint32_t len)
{
    uintptr_t ra = GETPC();
    struct bf_data d = bf_prep(addr, ofs, len);
    uint64_t data = bf_load(env, d.addr, d.blen, ra);

    /* Put CC_N at the top of the high word; put the zero-extended value
       at the bottom of the low word.  */
    data <<= d.bofs;
    data >>= 64 - d.len;
    data |= data << (64 - d.len);

    return data;
}

uint32_t HELPER(bfins_mem)(CPUM68KState *env, uint32_t addr, uint32_t val,
                           int32_t ofs, uint32_t len)
{
    uintptr_t ra = GETPC();
    struct bf_data d = bf_prep(addr, ofs, len);
    uint64_t data = bf_load(env, d.addr, d.blen, ra);
    uint64_t mask = -1ull << (64 - d.len) >> d.bofs;

    data = (data & ~mask) | (((uint64_t)val << (64 - d.len)) >> d.bofs);

    bf_store(env, d.addr, d.blen, data, ra);

    /* The field at the top of the word is also CC_N for CC_OP_LOGIC.  */
    return val << (32 - d.len);
}

uint32_t HELPER(bfchg_mem)(CPUM68KState *env, uint32_t addr,
                           int32_t ofs, uint32_t len)
{
    uintptr_t ra = GETPC();
    struct bf_data d = bf_prep(addr, ofs, len);
    uint64_t data = bf_load(env, d.addr, d.blen, ra);
    uint64_t mask = -1ull << (64 - d.len) >> d.bofs;

    bf_store(env, d.addr, d.blen, data ^ mask, ra);

    return ((data & mask) << d.bofs) >> 32;
}

uint32_t HELPER(bfclr_mem)(CPUM68KState *env, uint32_t addr,
                           int32_t ofs, uint32_t len)
{
    uintptr_t ra = GETPC();
    struct bf_data d = bf_prep(addr, ofs, len);
    uint64_t data = bf_load(env, d.addr, d.blen, ra);
    uint64_t mask = -1ull << (64 - d.len) >> d.bofs;

    bf_store(env, d.addr, d.blen, data & ~mask, ra);

    return ((data & mask) << d.bofs) >> 32;
}

uint32_t HELPER(bfset_mem)(CPUM68KState *env, uint32_t addr,
                           int32_t ofs, uint32_t len)
{
    uintptr_t ra = GETPC();
    struct bf_data d = bf_prep(addr, ofs, len);
    uint64_t data = bf_load(env, d.addr, d.blen, ra);
    uint64_t mask = -1ull << (64 - d.len) >> d.bofs;

    bf_store(env, d.addr, d.blen, data | mask, ra);

    return ((data & mask) << d.bofs) >> 32;
}

uint32_t HELPER(bfffo_reg)(uint32_t n, uint32_t ofs, uint32_t len)
{
    return (n ? clz32(n) : len) + ofs;
}

uint64_t HELPER(bfffo_mem)(CPUM68KState *env, uint32_t addr,
                           int32_t ofs, uint32_t len)
{
    uintptr_t ra = GETPC();
    struct bf_data d = bf_prep(addr, ofs, len);
    uint64_t data = bf_load(env, d.addr, d.blen, ra);
    uint64_t mask = -1ull << (64 - d.len) >> d.bofs;
    uint64_t n = (data & mask) << d.bofs;
    uint32_t ffo = helper_bfffo_reg(n >> 32, ofs, d.len);

    /* Return FFO in the low word and N in the high word.
       Note that because of MASK and the shift, the low word
       is already zero.  */
    return n | ffo;
}
