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

/* Try to fill the TLB and return an exception if error. If retaddr is
   NULL, it means that the function was called in C code (i.e. not
   from generated code or from helper.c) */
void tlb_fill(CPUState *cs, target_ulong addr, int is_write, int mmu_idx,
              uintptr_t retaddr)
{
    int ret;

    ret = m68k_cpu_handle_mmu_fault(cs, addr, is_write, mmu_idx);
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
    uint32_t fmt;

    sp = env->aregs[7];
    fmt = cpu_ldl_kernel(env, sp);
    env->pc = cpu_ldl_kernel(env, sp + 4);
    sp |= (fmt >> 28) & 3;
    env->aregs[7] = sp + 8;

    helper_set_sr(env, fmt);
}

static void do_interrupt_all(CPUM68KState *env, int is_hw)
{
    CPUState *cs = CPU(m68k_env_get_cpu(env));
    uint32_t sp;
    uint32_t fmt;
    uint32_t retaddr;
    uint32_t vector;

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

    fmt |= 0x40000000;
    fmt |= vector << 16;
    fmt |= env->sr;
    fmt |= cpu_m68k_get_ccr(env);

    env->sr |= SR_S;
    if (is_hw) {
        env->sr = (env->sr & ~SR_I) | (env->pending_level << SR_I_SHIFT);
        env->sr &= ~SR_M;
    }
    m68k_switch_sp(env);
    sp = env->aregs[7];
    fmt |= (sp & 3) << 28;

    /* ??? This could cause MMU faults.  */
    sp &= ~3;
    sp -= 4;
    cpu_stl_kernel(env, sp, retaddr);
    sp -= 4;
    cpu_stl_kernel(env, sp, fmt);
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
        && ((env->sr & SR_I) >> SR_I_SHIFT) < env->pending_level) {
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

static void raise_exception(CPUM68KState *env, int tt)
{
    CPUState *cs = CPU(m68k_env_get_cpu(env));

    cs->exception_index = tt;
    cpu_loop_exit(cs);
}

void HELPER(raise_exception)(CPUM68KState *env, uint32_t tt)
{
    raise_exception(env, tt);
}

/* load from a bitfield */

uint64_t HELPER(bitfield_load)(CPUM68KState *env, uint32_t addr,
                               uint32_t offset, uint32_t width)
{
    uint64_t bitfield = 0;
    int size;

    size = (offset + width + 7) >> 3;
    switch (size) {
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
    switch (size) {
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
