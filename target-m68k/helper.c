/*
 *  m68k op helpers
 *
 *  Copyright (c) 2006-2007 CodeSourcery
 *  Written by Paul Brook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "cpu.h"
#include "exec/gdbstub.h"

#include "exec/helper-proto.h"

#define SIGNBIT (1u << 31)

typedef struct M68kCPUListState {
    fprintf_function cpu_fprintf;
    FILE *file;
} M68kCPUListState;

/* modulo 33 table */
const uint8_t rox32_table[64] = {
    0, 1, 2, 3, 4, 5, 6, 7,
    8, 9,10,11,12,13,14,15,
   16,17,18,19,20,21,22,23,
   24,25,26,27,28,29,30,31,
   32, 0, 1, 2, 3, 4, 5, 6,
    7, 8, 9,10,11,12,13,14,
   15,16,17,18,19,20,21,22,
   23,24,25,26,27,28,29,30,
};

/* modulo 17 table */
const uint8_t rox16_table[64] = {
    0, 1, 2, 3, 4, 5, 6, 7,
    8, 9,10,11,12,13,14,15,
   16, 0, 1, 2, 3, 4, 5, 6,
    7, 8, 9,10,11,12,13,14,
   15,16, 0, 1, 2, 3, 4, 5,
    6, 7, 8, 9,10,11,12,13,
   14,15,16, 0, 1, 2, 3, 4,
    5, 6, 7, 8, 9,10,11,12,
};

/* modulo 9 table */
const uint8_t rox8_table[64] = {
    0, 1, 2, 3, 4, 5, 6, 7,
    8, 0, 1, 2, 3, 4, 5, 6,
    7, 8, 0, 1, 2, 3, 4, 5,
    6, 7, 8, 0, 1, 2, 3, 4,
    5, 6, 7, 8, 0, 1, 2, 3,
    4, 5, 6, 7, 8, 0, 1, 2,
    3, 4, 5, 6, 7, 8, 0, 1,
    2, 3, 4, 5, 6, 7, 8, 0,
};

static gint m68k_cpu_list_compare(gconstpointer a, gconstpointer b)
{
    ObjectClass *class_a = (ObjectClass *)a;
    ObjectClass *class_b = (ObjectClass *)b;
    const char *name_a, *name_b;

    name_a = object_class_get_name(class_a);
    name_b = object_class_get_name(class_b);
    if (strcmp(name_a, "any-" TYPE_M68K_CPU) == 0) {
        return 1;
    } else if (strcmp(name_b, "any-" TYPE_M68K_CPU) == 0) {
        return -1;
    } else {
        return strcasecmp(name_a, name_b);
    }
}

static void m68k_cpu_list_entry(gpointer data, gpointer user_data)
{
    ObjectClass *c = data;
    CPUListState *s = user_data;
    const char *typename;
    char *name;

    typename = object_class_get_name(c);
    name = g_strndup(typename, strlen(typename) - strlen("-" TYPE_M68K_CPU));
    if (strcmp(name, TARGET_DEFAULT_CPU) == 0) {
        (*s->cpu_fprintf)(s->file, " >");
    } else {
        (*s->cpu_fprintf)(s->file, "  ");
    }
    (*s->cpu_fprintf)(s->file, "%s\n",
                      name);
    g_free(name);
}

void m68k_cpu_list(FILE *f, fprintf_function cpu_fprintf)
{
    CPUListState s = {
        .file = f,
        .cpu_fprintf = cpu_fprintf,
    };
    GSList *list;

    list = object_class_get_list(TYPE_M68K_CPU, false);
    list = g_slist_sort(list, m68k_cpu_list_compare);
    g_slist_foreach(list, m68k_cpu_list_entry, &s);
    g_slist_free(list);
}

static int fpu_gdb_get_reg(CPUM68KState *env, uint8_t *mem_buf, int n)
{
    if (n < 8) {
        stfq_p(mem_buf, env->fregs[n]);
        return 8;
    }
    if (n < 11) {
        /* FP control registers (not implemented)  */
        memset(mem_buf, 0, 4);
        return 4;
    }
    return 0;
}

static int fpu_gdb_set_reg(CPUM68KState *env, uint8_t *mem_buf, int n)
{
    if (n < 8) {
        env->fregs[n] = ldfq_p(mem_buf);
        return 8;
    }
    if (n < 11) {
        /* FP control registers (not implemented)  */
        return 4;
    }
    return 0;
}

M68kCPU *cpu_m68k_init(const char *cpu_model)
{
    M68kCPU *cpu;
    CPUM68KState *env;
    ObjectClass *oc;

    oc = cpu_class_by_name(TYPE_M68K_CPU, cpu_model);
    if (oc == NULL) {
        return NULL;
    }
    cpu = M68K_CPU(object_new(object_class_get_name(oc)));
    env = &cpu->env;

    register_m68k_insns(env);

    object_property_set_bool(OBJECT(cpu), true, "realized", NULL);

    return cpu;
}

void m68k_cpu_init_gdb(M68kCPU *cpu)
{
    CPUState *cs = CPU(cpu);
    CPUM68KState *env = &cpu->env;

    if (m68k_feature(env, M68K_FEATURE_CF_FPU)) {
        gdb_register_coprocessor(cs, fpu_gdb_get_reg, fpu_gdb_set_reg,
                                 11, "cf-fp.xml", 18);
    }
    /* TODO: Add [E]MAC registers.  */
}

void cpu_m68k_flush_flags(CPUM68KState *env, int cc_op)
{
    M68kCPU *cpu = m68k_env_get_cpu(env);
    int flags;
    uint32_t src;
    uint32_t dest;
    uint32_t tmp;

#define HIGHBIT(type) (1u << (sizeof(type) * 8 - 1))

#define SET_NZ(x, type) do { \
    if ((type)(x) == 0) \
        flags |= CCF_Z; \
    else if ((type)(x) < 0) \
        flags |= CCF_N; \
    } while (0)

#define SET_FLAGS_SUB(type, utype) do { \
    SET_NZ(dest, type); \
    tmp = dest + src; \
    if ((utype) tmp < (utype) src) \
        flags |= CCF_C; \
    if (HIGHBIT(type) & (tmp ^ dest) & (tmp ^ src)) \
        flags |= CCF_V; \
    } while (0)

#define SET_FLAGS_ADD(type, utype) do { \
    SET_NZ(dest, type); \
    if ((utype) dest < (utype) src) \
        flags |= CCF_C; \
    tmp = dest - src; \
    if (HIGHBIT(type) & (src ^ dest) & ~(tmp ^ src)) \
        flags |= CCF_V; \
    } while (0)

#define SET_FLAGS_ADDX(type, utype) do { \
    SET_NZ(dest, type); \
    if ((utype) dest <= (utype) src) \
        flags |= CCF_C; \
    tmp = dest - src - 1; \
    if (HIGHBIT(type) & (src ^ dest) & ~(tmp ^ src)) \
        flags |= CCF_V; \
    } while (0)

#define SET_FLAGS_SUBX(type, utype) do { \
    SET_NZ(dest, type); \
    tmp = dest + src + 1; \
    if ((utype) tmp <= (utype) src) \
        flags |= CCF_C; \
    if (HIGHBIT(type) & (tmp ^ dest) & (tmp ^ src)) \
        flags |= CCF_V; \
    } while (0)

#define SET_FLAGS_SHIFT(type) do { \
    SET_NZ(dest, type); \
    if (src) \
        flags |= CCF_C; \
    } while(0)

    flags = 0;
    src = env->cc_src;
    dest = env->cc_dest;
    switch (cc_op) {
    case CC_OP_FLAGS:
        flags = dest;
        break;
    case CC_OP_LOGICB:
        SET_NZ(dest, int8_t);
        goto set_x;
        break;
    case CC_OP_LOGICW:
        SET_NZ(dest, int16_t);
        goto set_x;
        break;
    case CC_OP_LOGIC:
        SET_NZ(dest, int32_t);
set_x:
        if (env->cc_x && m68k_feature(env, M68K_FEATURE_M68000)) {
            /* Unlike m68k, coldfire always clears the overflow bit.  */
            flags |= CCF_X;
        }
        break;
    case CC_OP_ADDB:
        SET_FLAGS_ADD(int8_t, uint8_t);
        break;
    case CC_OP_ADDW:
        SET_FLAGS_ADD(int16_t, uint16_t);
        break;
    case CC_OP_ADD:
        SET_FLAGS_ADD(int32_t, uint32_t);
        break;
    case CC_OP_SUBB:
        SET_FLAGS_SUB(int8_t, uint8_t);
        break;
    case CC_OP_SUBW:
        SET_FLAGS_SUB(int16_t, uint16_t);
        break;
    case CC_OP_SUB:
        SET_FLAGS_SUB(int32_t, uint32_t);
        break;
    case CC_OP_ADDXB:
        SET_FLAGS_ADDX(int8_t, uint8_t);
        break;
    case CC_OP_ADDXW:
        SET_FLAGS_ADDX(int16_t, uint16_t);
        break;
    case CC_OP_ADDX:
        SET_FLAGS_ADDX(int32_t, uint32_t);
        break;
    case CC_OP_SUBXB:
        SET_FLAGS_SUBX(int8_t, uint8_t);
        break;
    case CC_OP_SUBXW:
        SET_FLAGS_SUBX(int16_t, uint16_t);
        break;
    case CC_OP_SUBX:
        SET_FLAGS_SUBX(int32_t, uint32_t);
        break;
    case CC_OP_SHIFTB:
        SET_FLAGS_SHIFT(int8_t);
	break;
    case CC_OP_SHIFTW:
        SET_FLAGS_SHIFT(int16_t);
	break;
    case CC_OP_SHIFT:
        SET_FLAGS_SHIFT(int32_t);
        break;
    default:
        cpu_abort(CPU(cpu), "Bad CC_OP %d", cc_op);
    }
    env->cc_op = CC_OP_FLAGS;
    env->cc_dest = flags;
}

void HELPER(movec)(CPUM68KState *env, uint32_t reg, uint32_t val)
{
    M68kCPU *cpu = m68k_env_get_cpu(env);

    switch (reg) {
    case 0x02: /* CACR */
        env->cacr = val;
        m68k_switch_sp(env);
        break;
    case 0x04: case 0x05: case 0x06: case 0x07: /* ACR[0-3] */
        /* TODO: Implement Access Control Registers.  */
        break;
    case 0x801: /* VBR */
        env->vbr = val;
        break;
    /* TODO: Implement control registers.  */
    default:
        cpu_abort(CPU(cpu), "Unimplemented control register write 0x%x = 0x%x\n",
                  reg, val);
    }
}

void HELPER(set_macsr)(CPUM68KState *env, uint32_t val)
{
    uint32_t acc;
    int8_t exthigh;
    uint8_t extlow;
    uint64_t regval;
    int i;
    if ((env->macsr ^ val) & (MACSR_FI | MACSR_SU)) {
        for (i = 0; i < 4; i++) {
            regval = env->macc[i];
            exthigh = regval >> 40;
            if (env->macsr & MACSR_FI) {
                acc = regval >> 8;
                extlow = regval;
            } else {
                acc = regval;
                extlow = regval >> 32;
            }
            if (env->macsr & MACSR_FI) {
                regval = (((uint64_t)acc) << 8) | extlow;
                regval |= ((int64_t)exthigh) << 40;
            } else if (env->macsr & MACSR_SU) {
                regval = acc | (((int64_t)extlow) << 32);
                regval |= ((int64_t)exthigh) << 40;
            } else {
                regval = acc | (((uint64_t)extlow) << 32);
                regval |= ((uint64_t)(uint8_t)exthigh) << 40;
            }
            env->macc[i] = regval;
        }
    }
    env->macsr = val;
}

void m68k_switch_sp(CPUM68KState *env)
{
    int new_sp;

    env->sp[env->current_sp] = env->aregs[7];
    new_sp = (env->sr & SR_S && env->cacr & M68K_CACR_EUSP)
             ? M68K_SSP : M68K_USP;
    env->aregs[7] = env->sp[new_sp];
    env->current_sp = new_sp;
}

#if defined(CONFIG_USER_ONLY)

int m68k_cpu_handle_mmu_fault(CPUState *cs, vaddr address, int rw,
                              int mmu_idx)
{
    M68kCPU *cpu = M68K_CPU(cs);

    cs->exception_index = EXCP_ACCESS;
    cpu->env.mmu.ar = address;
    return 1;
}

#else

/* MMU */

/* TODO: This will need fixing once the MMU is implemented.  */
hwaddr m68k_cpu_get_phys_page_debug(CPUState *cs, vaddr addr)
{
    return addr;
}

int m68k_cpu_handle_mmu_fault(CPUState *cs, vaddr address, int rw,
                              int mmu_idx)
{
    int prot;

    address &= TARGET_PAGE_MASK;
    prot = PAGE_READ | PAGE_WRITE | PAGE_EXEC;
    tlb_set_page(cs, address, address, prot, mmu_idx, TARGET_PAGE_SIZE);
    return 0;
}

/* Notify CPU of a pending interrupt.  Prioritization and vectoring should
   be handled by the interrupt controller.  Real hardware only requests
   the vector when the interrupt is acknowledged by the CPU.  For
   simplicitly we calculate it when the interrupt is signalled.  */
void m68k_set_irq_level(M68kCPU *cpu, int level, uint8_t vector)
{
    CPUState *cs = CPU(cpu);
    CPUM68KState *env = &cpu->env;

    env->pending_level = level;
    env->pending_vector = vector;
    if (level) {
        cpu_interrupt(cs, CPU_INTERRUPT_HARD);
    } else {
        cpu_reset_interrupt(cs, CPU_INTERRUPT_HARD);
    }
}

#endif

uint32_t HELPER(bitrev)(uint32_t x)
{
    x = ((x >> 1) & 0x55555555u) | ((x << 1) & 0xaaaaaaaau);
    x = ((x >> 2) & 0x33333333u) | ((x << 2) & 0xccccccccu);
    x = ((x >> 4) & 0x0f0f0f0fu) | ((x << 4) & 0xf0f0f0f0u);
    return bswap32(x);
}

uint32_t HELPER(ff1)(uint32_t x)
{
    int n;
    for (n = 32; x; n--)
        x >>= 1;
    return n;
}

uint32_t HELPER(bfffo)(uint32_t arg, uint32_t width)
{
    int n;
    uint32_t mask;
    mask = 0x80000000;
    for (n = 0; n < width; n++) {
       if (arg & mask)
           break;
       mask >>= 1;
    }
    return n;
}

uint32_t HELPER(rol32)(uint32_t val, uint32_t shift)
{
    uint32_t result;
    if (shift == 0 || shift == 32)
        return val;
    result = (val << shift) | (val >> (32 - shift));
    return result;
}

uint32_t HELPER(ror32)(uint32_t val, uint32_t shift)
{
    uint32_t result;
    if (shift == 0 || shift == 32)
        return val;
    result = (val >> shift) | (val << (32 - shift));
    return result;
}

uint32_t HELPER(sats)(uint32_t val, uint32_t ccr)
{
    /* The result has the opposite sign to the original value.  */
    if (ccr & CCF_V)
        val = (((int32_t)val) >> 31) ^ SIGNBIT;
    return val;
}

uint32_t HELPER(subx_cc)(CPUM68KState *env, uint32_t op1, uint32_t op2)
{
    uint32_t res;
    uint32_t old_flags;

    old_flags = env->cc_dest;
    if (env->cc_x) {
        env->cc_x = (op1 <= op2);
        env->cc_op = CC_OP_SUBX;
        res = op1 - (op2 + 1);
    } else {
        env->cc_x = (op1 < op2);
        env->cc_op = CC_OP_SUB;
        res = op1 - op2;
    }
    env->cc_dest = res;
    env->cc_src = op2;
    cpu_m68k_flush_flags(env, env->cc_op);
    /* !Z is sticky.  */
    env->cc_dest &= (old_flags | ~CCF_Z);
    return res;
}

uint32_t HELPER(addx_cc)(CPUM68KState *env, uint32_t op1, uint32_t op2)
{
    uint32_t res;
    uint32_t old_flags;

    old_flags = env->cc_dest;
    if (env->cc_x) {
        res = op1 + op2 + 1;
        env->cc_x = (res <= op2);
        env->cc_op = CC_OP_ADDX;
    } else {
        res = op1 + op2;
        env->cc_x = (res < op2);
        env->cc_op = CC_OP_ADD;
    }
    env->cc_dest = res;
    env->cc_src = op2;
    cpu_m68k_flush_flags(env, env->cc_op);
    /* !Z is sticky.  */
    env->cc_dest &= (old_flags | ~CCF_Z);
    return res;
}

uint32_t HELPER(xflag_lt)(uint32_t a, uint32_t b)
{
    return a < b;
}

void HELPER(set_sr)(CPUM68KState *env, uint32_t val)
{
    env->sr = val & 0xffff;
    m68k_switch_sp(env);
}

#define HELPER_SHL(type, bits) \
uint32_t HELPER(glue(glue(shl, bits),_cc))(CPUM68KState *env, uint32_t val, uint32_t shift) \
{ \
    type result; \
    uint32_t cf; \
    shift &= 63; \
    if (shift == 0) { \
        result = (type)val; \
        cf = env->cc_src & CCF_C; \
    } else if (shift < bits) { \
        result = (type)val << shift; \
        cf = ((type)val >> (bits - shift)) & 1; \
    } else if (shift == bits) { \
        result = 0; \
        cf = val & 1; \
    } else /* shift > bits */ { \
        result = 0; \
        cf = 0; \
    } \
    env->cc_src = cf; \
    env->cc_x = (cf != 0); \
    env->cc_dest = result; \
    return result; \
}

HELPER_SHL(uint8_t, 8)
HELPER_SHL(uint16_t, 16)
HELPER_SHL(uint32_t, 32)

#define HELPER_SHR(type, bits) \
uint32_t HELPER(glue(glue(shr, bits), _cc))(CPUM68KState *env, uint32_t val, uint32_t shift) \
{ \
    type result; \
    uint32_t cf; \
    shift &= 63; \
    if (shift == 0) { \
        result = (type)val; \
        cf = env->cc_src & CCF_C; \
    } else if (shift < bits) { \
        result = (type)val >> shift; \
        cf = ((type)val >> (shift - 1)) & 1; \
    } else if (shift == bits) { \
        result = 0; \
        cf = (type)val >> (bits - 1); \
    } else /* shift > bits */ { \
        result = 0; \
        cf = 0; \
    } \
    env->cc_src = cf; \
    env->cc_x = (cf != 0); \
    env->cc_dest = result; \
    return result; \
}

HELPER_SHR(uint8_t, 8)
HELPER_SHR(uint16_t, 16)
HELPER_SHR(uint32_t, 32)

#define HELPER_SAR(type, bits) \
uint32_t HELPER(glue(glue(sar, bits), _cc))(CPUM68KState *env, uint32_t val, uint32_t shift)					\
{ \
    type result; \
    uint32_t cf; \
    shift &= 63; \
    if (shift == 0) { \
        result = (type)val; \
        cf = (env->cc_src & CCF_C) != 0; \
    } else if (shift < bits) { \
        result = (type)val >> shift; \
        cf = ((type)val >> (shift - 1)) & 1; \
    } else /* shift >= bits */ { \
        result = (type)val >> (bits - 1); \
        cf = (type)val >> (bits - 1); \
    } \
    env->cc_src = cf; \
    env->cc_x = cf; \
    env->cc_dest = result; \
    return result; \
}

HELPER_SAR(int8_t, 8)
HELPER_SAR(int16_t, 16)
HELPER_SAR(int32_t, 32)

#define HELPER_ROL(type, bits) \
uint32_t HELPER(glue(glue(rol,bits),_cc))(CPUM68KState *env, uint32_t val, uint32_t shift) \
{ \
    type result; \
    uint32_t flags; \
    int count = shift & (bits - 1); \
    if (count) \
       result = ((type)val << count) | ((type)val >> (bits - count)); \
    else \
       result = (type)val; \
    flags = 0; \
    if (result == 0) \
       flags |= CCF_Z; \
    if (result & (1 << (bits - 1))) \
       flags |= CCF_N; \
    if (shift && result & 1) \
       flags |= CCF_C; \
    env->cc_dest = flags; \
    return result; \
}

HELPER_ROL(uint8_t, 8)
HELPER_ROL(uint16_t, 16)
HELPER_ROL(uint32_t, 32)

#define HELPER_ROR(type, bits) \
uint32_t HELPER(glue(glue(ror,bits),_cc))(CPUM68KState *env, uint32_t val, uint32_t shift) \
{ \
    type result; \
    uint32_t flags; \
    int count = shift & (bits - 1); \
    if (count) \
       result = ((type)val >> count) | ((type)val << (bits - count)); \
    else \
       result = (type)val; \
    flags = 0; \
    if (result == 0) \
       flags |= CCF_Z; \
    if (result & (1 << (bits - 1))) \
       flags |= CCF_N; \
    if (shift && result & (1 << (bits - 1))) \
       flags |= CCF_C; \
    env->cc_dest = flags; \
    return result; \
}

HELPER_ROR(uint8_t, 8)
HELPER_ROR(uint16_t, 16)
HELPER_ROR(uint32_t, 32)

#define HELPER_ROXR(type, bits) \
uint32_t HELPER(glue(glue(roxr,bits),_cc))(CPUM68KState *env, uint32_t val, uint32_t shift) \
{ \
    type result; \
    uint32_t flags; \
    int count = shift; \
    if (bits == 8) count = rox8_table[count]; \
    if (bits == 16) count = rox16_table[count]; \
    if (bits == 32) count = rox32_table[count]; \
    if (count) { \
       result = ((type)val >> count) | ((type)env->cc_x << (bits - count)); \
       if (count > 1) \
           result |= (type)val << (bits + 1 - count); \
       env->cc_x = ((type)val >> (count - 1)) & 1; \
    } else \
       result = (type)val; \
    flags = 0; \
    if (result == 0) \
       flags |= CCF_Z; \
    if (result & (1 << (bits - 1))) \
       flags |= CCF_N; \
    if (env->cc_x) \
       flags |= CCF_C; \
    env->cc_dest = flags; \
    return result; \
}

HELPER_ROXR(uint8_t, 8)
HELPER_ROXR(uint16_t, 16)
HELPER_ROXR(uint32_t, 32)

#define HELPER_ROXL(type, bits) \
uint32_t HELPER(glue(glue(roxl,bits),_cc))(CPUM68KState *env, uint32_t val, uint32_t shift) \
{ \
    type result; \
    uint32_t flags; \
    int count; \
    count = shift; \
    if (bits == 8) count = rox8_table[count]; \
    if (bits == 16) count = rox16_table[count]; \
    if (bits == 32) count = rox32_table[count]; \
    if (count) { \
       result = ((type)val << count) | ((type)env->cc_x << (count - 1)); \
       if (count > 1) \
           result |= (type)val >> (bits + 1 - count); \
       env->cc_x = ((type)val >> (bits - count)) & 1; \
    } else \
       result = (type)val; \
    flags = 0; \
    if (result == 0) \
       flags |= CCF_Z; \
    if (result & (1 << (bits - 1))) \
       flags |= CCF_N; \
    if (env->cc_x) \
       flags |= CCF_C; \
    env->cc_dest = flags; \
    return result; \
}

HELPER_ROXL(uint8_t, 8)
HELPER_ROXL(uint16_t, 16)
HELPER_ROXL(uint32_t, 32)

/* FPU helpers.  */

static const floatx80 fpu_rom[128] = {
    [0x00] = { .high = 0x4000, .low = 0xc90fdaa22168c235 },	/* Pi */

    [0x0b] = { .high = 0x3ffd, .low = 0x9a209a84fbcff798 },	/* Log10(2) */
    [0x0c] = { .high = 0x4000, .low = 0xadf85458a2bb4a9a },	/* e        */
    [0x0d] = { .high = 0x3fff, .low = 0xb8aa3b295c17f0bc },	/* Log2(e)  */
    [0x0e] = { .high = 0x3ffd, .low = 0xde5bd8a937287195 },	/* Log10(e) */
    [0x0f] = { .high = 0x0000, .low = 0x0000000000000000 },	/* Zero     */

    [0x30] = { .high = 0x3ffe, .low = 0xb17217f7d1cf79ac },	/* ln(2)    */
    [0x31] = { .high = 0x4000, .low = 0x935d8dddaaa8ac17 },	/* ln(10)   */
    [0x32] = { .high = 0x3fff, .low = 0x8000000000000000 },	/* 10^0     */
    [0x33] = { .high = 0x4002, .low = 0xa000000000000000 },	/* 10^1     */
    [0x34] = { .high = 0x4005, .low = 0xc800000000000000 },	/* 10^2     */
    [0x35] = { .high = 0x400c, .low = 0x9c40000000000000 },	/* 10^4     */
    [0x36] = { .high = 0x4019, .low = 0xbebc200000000000 },	/* 10^8     */
    [0x37] = { .high = 0x4034, .low = 0x8e1bc9bf04000000 },	/* 10^16    */
    [0x38] = { .high = 0x4069, .low = 0x9dc5ada82b70b59e },	/* 10^32    */
    [0x39] = { .high = 0x40d3, .low = 0xc2781f49ffcfa6d5 },	/* 10^64    */
    [0x3a] = { .high = 0x41a8, .low = 0x93ba47c980e98ce0 },	/* 10^128   */
    [0x3b] = { .high = 0x4351, .low = 0xaa7eebfb9df9de8e },	/* 10^256   */
    [0x3c] = { .high = 0x46a3, .low = 0xe319a0aea60e91c7 },	/* 10^512   */
    [0x3d] = { .high = 0x4d48, .low = 0xc976758681750c17 },	/* 10^1024  */
    [0x3e] = { .high = 0x5a92, .low = 0x9e8b3b5dc53d5de5 },	/* 10^2048  */
    [0x3f] = { .high = 0x7525, .low = 0xc46052028a20979b },	/* 10^4096  */
};

float64 HELPER(const_f64)(CPUM68KState *env, uint32_t offset)
{
    return floatx80_to_float64(fpu_rom[offset], &env->fp_status);
}

uint32_t HELPER(f64_to_i32)(CPUM68KState *env, float64 val)
{
    return float64_to_int32(val, &env->fp_status);
}

float32 HELPER(f64_to_f32)(CPUM68KState *env, float64 val)
{
    return float64_to_float32(val, &env->fp_status);
}

float64 HELPER(i32_to_f64)(CPUM68KState *env, uint32_t val)
{
    return int32_to_float64(val, &env->fp_status);
}

float64 HELPER(f32_to_f64)(CPUM68KState *env, float32 val)
{
    return float32_to_float64(val, &env->fp_status);
}

float64 HELPER(iround_f64)(CPUM68KState *env, float64 val)
{
    return float64_round_to_int(val, &env->fp_status);
}

float64 HELPER(itrunc_f64)(CPUM68KState *env, float64 val)
{
    return float64_trunc_to_int(val, &env->fp_status);
}

float64 HELPER(sqrt_f64)(CPUM68KState *env, float64 val)
{
    return float64_sqrt(val, &env->fp_status);
}

float64 HELPER(abs_f64)(float64 val)
{
    return float64_abs(val);
}

float64 HELPER(chs_f64)(float64 val)
{
    return float64_chs(val);
}

float64 HELPER(add_f64)(CPUM68KState *env, float64 a, float64 b)
{
    return float64_add(a, b, &env->fp_status);
}

float64 HELPER(sub_f64)(CPUM68KState *env, float64 a, float64 b)
{
    return float64_sub(a, b, &env->fp_status);
}

float64 HELPER(mul_f64)(CPUM68KState *env, float64 a, float64 b)
{
    return float64_mul(a, b, &env->fp_status);
}

float64 HELPER(div_f64)(CPUM68KState *env, float64 a, float64 b)
{
    return float64_div(a, b, &env->fp_status);
}

float64 HELPER(sub_cmp_f64)(CPUM68KState *env, float64 a, float64 b)
{
    /* ??? This may incorrectly raise exceptions.  */
    /* ??? Should flush denormals to zero.  */
    float64 res;
    res = float64_sub(a, b, &env->fp_status);
    if (float64_is_quiet_nan(res)) {
        /* +/-inf compares equal against itself, but sub returns nan.  */
        if (!float64_is_quiet_nan(a)
            && !float64_is_quiet_nan(b)) {
            res = float64_zero;
            if (float64_lt_quiet(a, res, &env->fp_status))
                res = float64_chs(res);
        }
    }
    return res;
}

uint32_t HELPER(compare_f64)(CPUM68KState *env, float64 val)
{
    return float64_compare_quiet(val, float64_zero, &env->fp_status);
}

/* MAC unit.  */
/* FIXME: The MAC unit implementation is a bit of a mess.  Some helpers
   take values,  others take register numbers and manipulate the contents
   in-place.  */
void HELPER(mac_move)(CPUM68KState *env, uint32_t dest, uint32_t src)
{
    uint32_t mask;
    env->macc[dest] = env->macc[src];
    mask = MACSR_PAV0 << dest;
    if (env->macsr & (MACSR_PAV0 << src))
        env->macsr |= mask;
    else
        env->macsr &= ~mask;
}

uint64_t HELPER(macmuls)(CPUM68KState *env, uint32_t op1, uint32_t op2)
{
    int64_t product;
    int64_t res;

    product = (uint64_t)op1 * op2;
    res = (product << 24) >> 24;
    if (res != product) {
        env->macsr |= MACSR_V;
        if (env->macsr & MACSR_OMC) {
            /* Make sure the accumulate operation overflows.  */
            if (product < 0)
                res = ~(1ll << 50);
            else
                res = 1ll << 50;
        }
    }
    return res;
}

uint64_t HELPER(macmulu)(CPUM68KState *env, uint32_t op1, uint32_t op2)
{
    uint64_t product;

    product = (uint64_t)op1 * op2;
    if (product & (0xffffffull << 40)) {
        env->macsr |= MACSR_V;
        if (env->macsr & MACSR_OMC) {
            /* Make sure the accumulate operation overflows.  */
            product = 1ll << 50;
        } else {
            product &= ((1ull << 40) - 1);
        }
    }
    return product;
}

uint64_t HELPER(macmulf)(CPUM68KState *env, uint32_t op1, uint32_t op2)
{
    uint64_t product;
    uint32_t remainder;

    product = (uint64_t)op1 * op2;
    if (env->macsr & MACSR_RT) {
        remainder = product & 0xffffff;
        product >>= 24;
        if (remainder > 0x800000)
            product++;
        else if (remainder == 0x800000)
            product += (product & 1);
    } else {
        product >>= 24;
    }
    return product;
}

void HELPER(macsats)(CPUM68KState *env, uint32_t acc)
{
    int64_t tmp;
    int64_t result;
    tmp = env->macc[acc];
    result = ((tmp << 16) >> 16);
    if (result != tmp) {
        env->macsr |= MACSR_V;
    }
    if (env->macsr & MACSR_V) {
        env->macsr |= MACSR_PAV0 << acc;
        if (env->macsr & MACSR_OMC) {
            /* The result is saturated to 32 bits, despite overflow occurring
               at 48 bits.  Seems weird, but that's what the hardware docs
               say.  */
            result = (result >> 63) ^ 0x7fffffff;
        }
    }
    env->macc[acc] = result;
}

void HELPER(macsatu)(CPUM68KState *env, uint32_t acc)
{
    uint64_t val;

    val = env->macc[acc];
    if (val & (0xffffull << 48)) {
        env->macsr |= MACSR_V;
    }
    if (env->macsr & MACSR_V) {
        env->macsr |= MACSR_PAV0 << acc;
        if (env->macsr & MACSR_OMC) {
            if (val > (1ull << 53))
                val = 0;
            else
                val = (1ull << 48) - 1;
        } else {
            val &= ((1ull << 48) - 1);
        }
    }
    env->macc[acc] = val;
}

void HELPER(macsatf)(CPUM68KState *env, uint32_t acc)
{
    int64_t sum;
    int64_t result;

    sum = env->macc[acc];
    result = (sum << 16) >> 16;
    if (result != sum) {
        env->macsr |= MACSR_V;
    }
    if (env->macsr & MACSR_V) {
        env->macsr |= MACSR_PAV0 << acc;
        if (env->macsr & MACSR_OMC) {
            result = (result >> 63) ^ 0x7fffffffffffll;
        }
    }
    env->macc[acc] = result;
}

void HELPER(mac_set_flags)(CPUM68KState *env, uint32_t acc)
{
    uint64_t val;
    val = env->macc[acc];
    if (val == 0) {
        env->macsr |= MACSR_Z;
    } else if (val & (1ull << 47)) {
        env->macsr |= MACSR_N;
    }
    if (env->macsr & (MACSR_PAV0 << acc)) {
        env->macsr |= MACSR_V;
    }
    if (env->macsr & MACSR_FI) {
        val = ((int64_t)val) >> 40;
        if (val != 0 && val != -1)
            env->macsr |= MACSR_EV;
    } else if (env->macsr & MACSR_SU) {
        val = ((int64_t)val) >> 32;
        if (val != 0 && val != -1)
            env->macsr |= MACSR_EV;
    } else {
        if ((val >> 32) != 0)
            env->macsr |= MACSR_EV;
    }
}

void HELPER(flush_flags)(CPUM68KState *env, uint32_t cc_op)
{
    cpu_m68k_flush_flags(env, cc_op);
}

uint32_t HELPER(get_macf)(CPUM68KState *env, uint64_t val)
{
    int rem;
    uint32_t result;

    if (env->macsr & MACSR_SU) {
        /* 16-bit rounding.  */
        rem = val & 0xffffff;
        val = (val >> 24) & 0xffffu;
        if (rem > 0x800000)
            val++;
        else if (rem == 0x800000)
            val += (val & 1);
    } else if (env->macsr & MACSR_RT) {
        /* 32-bit rounding.  */
        rem = val & 0xff;
        val >>= 8;
        if (rem > 0x80)
            val++;
        else if (rem == 0x80)
            val += (val & 1);
    } else {
        /* No rounding.  */
        val >>= 8;
    }
    if (env->macsr & MACSR_OMC) {
        /* Saturate.  */
        if (env->macsr & MACSR_SU) {
            if (val != (uint16_t) val) {
                result = ((val >> 63) ^ 0x7fff) & 0xffff;
            } else {
                result = val & 0xffff;
            }
        } else {
            if (val != (uint32_t)val) {
                result = ((uint32_t)(val >> 63) & 0x7fffffff);
            } else {
                result = (uint32_t)val;
            }
        }
    } else {
        /* No saturation.  */
        if (env->macsr & MACSR_SU) {
            result = val & 0xffff;
        } else {
            result = (uint32_t)val;
        }
    }
    return result;
}

uint32_t HELPER(get_macs)(uint64_t val)
{
    if (val == (int32_t)val) {
        return (int32_t)val;
    } else {
        return (val >> 61) ^ ~SIGNBIT;
    }
}

uint32_t HELPER(get_macu)(uint64_t val)
{
    if ((val >> 32) == 0) {
        return (uint32_t)val;
    } else {
        return 0xffffffffu;
    }
}

uint32_t HELPER(get_mac_extf)(CPUM68KState *env, uint32_t acc)
{
    uint32_t val;
    val = env->macc[acc] & 0x00ff;
    val = (env->macc[acc] >> 32) & 0xff00;
    val |= (env->macc[acc + 1] << 16) & 0x00ff0000;
    val |= (env->macc[acc + 1] >> 16) & 0xff000000;
    return val;
}

uint32_t HELPER(get_mac_exti)(CPUM68KState *env, uint32_t acc)
{
    uint32_t val;
    val = (env->macc[acc] >> 32) & 0xffff;
    val |= (env->macc[acc + 1] >> 16) & 0xffff0000;
    return val;
}

void HELPER(set_mac_extf)(CPUM68KState *env, uint32_t val, uint32_t acc)
{
    int64_t res;
    int32_t tmp;
    res = env->macc[acc] & 0xffffffff00ull;
    tmp = (int16_t)(val & 0xff00);
    res |= ((int64_t)tmp) << 32;
    res |= val & 0xff;
    env->macc[acc] = res;
    res = env->macc[acc + 1] & 0xffffffff00ull;
    tmp = (val & 0xff000000);
    res |= ((int64_t)tmp) << 16;
    res |= (val >> 16) & 0xff;
    env->macc[acc + 1] = res;
}

void HELPER(set_mac_exts)(CPUM68KState *env, uint32_t val, uint32_t acc)
{
    int64_t res;
    int32_t tmp;
    res = (uint32_t)env->macc[acc];
    tmp = (int16_t)val;
    res |= ((int64_t)tmp) << 32;
    env->macc[acc] = res;
    res = (uint32_t)env->macc[acc + 1];
    tmp = val & 0xffff0000;
    res |= (int64_t)tmp << 16;
    env->macc[acc + 1] = res;
}

void HELPER(set_mac_extu)(CPUM68KState *env, uint32_t val, uint32_t acc)
{
    uint64_t res;
    res = (uint32_t)env->macc[acc];
    res |= ((uint64_t)(val & 0xffff)) << 32;
    env->macc[acc] = res;
    res = (uint32_t)env->macc[acc + 1];
    res |= (uint64_t)(val & 0xffff0000) << 16;
    env->macc[acc + 1] = res;
}

/* load from a bitfield */

uint64_t HELPER(bitfield_load)(uint32_t addr, uint32_t offset, uint32_t width)
{
    uint8_t data[8];
    uint64_t bitfield;
    int size;
    int i;

    size = (offset + width + 7) >> 3;
#if defined(CONFIG_USER_ONLY)
    cpu_memory_rw_debug(NULL, (target_ulong)addr, data, size, 0);
#else
    cpu_physical_memory_rw(addr, data, size, 0);
#endif

    bitfield = data[0];
    for (i = 1; i < 8; i++)
        bitfield = (bitfield << 8) | data[i];

    return bitfield;
}

/* store to a bitfield */

void HELPER(bitfield_store)(uint32_t addr, uint32_t offset, uint32_t width,
                            uint64_t bitfield)
{
    uint8_t data[8];
    int size;
    int i;

    size = (offset + width + 7) >> 3;

    for (i = 0; i < 8; i++) {
        data[7 - i] = bitfield;
        bitfield >>= 8;
    }

#if defined(CONFIG_USER_ONLY)
    cpu_memory_rw_debug(NULL, (target_ulong)addr, data, size, 1);
#else
    cpu_physical_memory_rw(addr, data, size, 1);
#endif
}

uint32_t HELPER(abcd_cc)(CPUM68KState *env, uint32_t src, uint32_t dest)
{
    uint16_t hi, lo;
    uint16_t res;
    uint32_t flags;

    flags = env->cc_dest;
    flags &= ~(CCF_C|CCF_X);

    lo = (src & 0x0f) + (dest & 0x0f);
    if (env->cc_x)
        lo ++;
    hi = (src & 0xf0) + (dest & 0xf0);

    res = hi + lo;
    if (lo > 9)
        res += 0x06;

    /* C and X flags: set if decimal carry, cleared otherwise */

    if ((res & 0x3F0) > 0x90) {
        res += 0x60;
        flags |= CCF_C|CCF_X;
    }

    /* Z flag: cleared if nonzero */

    if (res & 0xff)
        flags &= ~CCF_Z;

    dest = (dest & 0xffffff00) | (res & 0xff);

    env->cc_x = (flags & CCF_X) != 0;
    env->cc_dest = flags;

    return dest;
}

uint32_t HELPER(sbcd_cc)(CPUM68KState *env, uint32_t src, uint32_t dest)
{
    uint16_t hi, lo;
    uint16_t res;
    uint32_t flags;
    int bcd = 0, carry = 0;

    flags = env->cc_dest;
    flags &= ~(CCF_C|CCF_X);

    if (env->cc_x)
        carry = 1;

    lo = (dest & 0x0f) - (src & 0x0f) - carry;
    hi = (dest & 0xf0) - (src & 0xf0);

    res = hi + lo;
    if (lo & 0xf0) {
        res -= 0x06;
        bcd = 0x06;
    }

    if ((((dest & 0xff) - (src & 0xff) - carry) & 0x100) > 0xff) {
        res -= 0x60;
    }

    /* C and X flags: set if decimal carry, cleared otherwise */

    if ((((dest & 0xff) - (src & 0xff) - (bcd + carry)) & 0x300) > 0xff) {
        flags |= CCF_C|CCF_X;
    }

    /* Z flag: cleared if nonzero */

    if (res & 0xff)
        flags &= ~CCF_Z;

    dest = (dest & 0xffffff00) | (res & 0xff);

    env->cc_x = (flags & CCF_X) != 0;
    env->cc_dest = flags;

    return dest;
}

void m68k_cpu_exec_enter(CPUState *cs)
{
    M68kCPU *cpu = M68K_CPU(cs);
    CPUM68KState *env = &cpu->env;

    env->cc_op = CC_OP_FLAGS;
    env->cc_dest = env->sr & 0xf;
    env->cc_x = (env->sr >> 4) & 1;
}

void m68k_cpu_exec_exit(CPUState *cs)
{
    M68kCPU *cpu = M68K_CPU(cs);
    CPUM68KState *env = &cpu->env;

    cpu_m68k_flush_flags(env, env->cc_op);
    env->cc_op = CC_OP_FLAGS;
    env->sr = (env->sr & 0xffe0) | env->cc_dest | (env->cc_x << 4);
}
