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

#include "cpu.h"
#include "exec/gdbstub.h"

#include "helper.h"
#include <math.h>

#if 0
#define DBG_FPUH(...) do { fprintf(stderr, "0x%08x: ", env->pc); fprintf(stderr, __VA_ARGS__); } while(0)
#define DBG_FPU(...) do { fprintf(stderr, __VA_ARGS__); } while(0)
#else
#define DBG_FPUH(...)
#define DBG_FPU(...)
#endif
static inline float FLOAT(float32 x)
{
    return *(float *)&x;
}
static inline double DOUBLE(float64 x)
{
    return *(double *)&x;
}

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

static int cf_fpu_gdb_get_reg(CPUM68KState *env, uint8_t *mem_buf, int n)
{
    if (n < 8) {
        float_status s;
        stfq_p(mem_buf, floatx80_to_float64(env->fregs[n].d, &s));
        return 8;
    }
    if (n < 11) {
        /* FP control registers (not implemented)  */
        memset(mem_buf, 0, 4);
        return 4;
    }
    return 0;
}

static int cf_fpu_gdb_set_reg(CPUM68KState *env, uint8_t *mem_buf, int n)
{
    if (n < 8) {
        float_status s;
        env->fregs[n].d = float64_to_floatx80(ldfq_p(mem_buf), &s);
        return 8;
    }
    switch (n) {
        case 8:
            env->fpcr = ldl_be_p(mem_buf);
            return 4;
        case 9:
            env->fpsr = ldl_be_p(mem_buf);
            return 4;
        case 10:
            return 4;
    }
    return 0;
}

static int m68k_fpu_gdb_get_reg(CPUM68KState *env, uint8_t *mem_buf, int n)
{
    if (n < 8) {
        stw_be_p(mem_buf, env->fregs[n].l.upper);
        memset(mem_buf + 2, 0, 2);
        stq_be_p(mem_buf + 4, env->fregs[n].l.lower);
        return 12;
    }
    switch (n) {
        case 8:
            stl_be_p(mem_buf, env->fpcr);
            return 4;
        case 9:
            stl_be_p(mem_buf, env->fpsr);
            return 4;
        case 10:
            memset(mem_buf, 0, 4);
            return 4;
    }
    return 0;
}

static int m68k_fpu_gdb_set_reg(CPUM68KState *env, uint8_t *mem_buf, int n)
{
    if (n < 8) {
        env->fregs[n].l.upper = lduw_be_p(mem_buf);
        env->fregs[n].l.lower = ldq_be_p(mem_buf + 4);
        return 12;
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
        gdb_register_coprocessor(cs, cf_fpu_gdb_get_reg, cf_fpu_gdb_set_reg,
                                 11, "cf-fp.xml", 18);
    } else if ( m68k_feature (env, M68K_FEATURE_FPU)) {
        gdb_register_coprocessor(cs, m68k_fpu_gdb_get_reg,
                                 m68k_fpu_gdb_set_reg, 11, "m68k-fp.xml", 18);
    }
    /* TODO: Add [E]MAC registers.  */
}

void cpu_m68k_flush_flags(CPUM68KState *env, int cc_op)
{
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
    flags |= src; \
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
        cpu_abort(env, "Bad CC_OP %d", cc_op);
    }
    env->cc_op = CC_OP_FLAGS;
    env->cc_dest = flags;
}

void HELPER(movec_to)(CPUM68KState * env, uint32_t reg, uint32_t val)
{
    switch (reg) {
    /* MC680[1234]0 */
    case M68K_CR_SFC:
        env->sfc = val & 7;
        return;
    case M68K_CR_DFC:
        env->dfc = val & 7;
        return;
    case M68K_CR_VBR:
        env->vbr = val;
        return;
    /* MC680[234]0 */
    case M68K_CR_CACR:
        env->cacr = val;
        m68k_switch_sp(env);
        return;
    /* MC680[34]0 */
    case M68K_CR_TC:
        env->mmu.tcr = val;
        return;
    case M68K_CR_MMUSR:
        env->mmu.mmusr = val;
        return;
    case M68K_CR_SRP:
        env->mmu.srp = val;
        return;
    case M68K_CR_URP:
        env->mmu.urp = val;
        return;
    case M68K_CR_USP:
        env->sp[M68K_USP] = val;
        return;
    case M68K_CR_MSP:
        env->sp[M68K_SSP] = val;
        return;
    case M68K_CR_ISP:
        env->sp[M68K_ISP] = val;
        return;
    /* MC68040/MC68LC040 */
    case M68K_CR_ITT0:
        env->mmu.ittr0 = val;
        return;
    case M68K_CR_ITT1:
         env->mmu.ittr1 = val;
        return;
    case M68K_CR_DTT0:
        env->mmu.dttr0 = val;
        return;
    case M68K_CR_DTT1:
        env->mmu.dttr1 = val;
        return;
    }
    cpu_abort(env, "Unimplemented control register write 0x%x = 0x%x\n",
              reg, val);
}

uint32_t HELPER(movec_from)(CPUM68KState * env, uint32_t reg)
{
    switch (reg) {
    /* MC680[1234]0 */
    case M68K_CR_SFC:
        return env->sfc;
    case M68K_CR_DFC:
        return env->dfc;
    case M68K_CR_VBR:
        return env->vbr;
    /* MC680[234]0 */
    case M68K_CR_CACR:
        return env->cacr;
    /* MC680[34]0 */
    case M68K_CR_TC:
        return env->mmu.tcr;
    case M68K_CR_MMUSR:
        return env->mmu.mmusr;
    case M68K_CR_SRP:
        return env->mmu.srp;
    case M68K_CR_USP:
        return env->sp[M68K_USP];
    case M68K_CR_MSP:
        return env->sp[M68K_SSP];
    case M68K_CR_ISP:
        return env->sp[M68K_ISP];
    /* MC68040/MC68LC040 */
    case M68K_CR_URP:
        return env->mmu.urp;
    case M68K_CR_ITT0:
        return env->mmu.ittr0;
    case M68K_CR_ITT1:
        return env->mmu.ittr1;
    case M68K_CR_DTT0:
        return env->mmu.dttr0;
    case M68K_CR_DTT1:
        return env->mmu.dttr1;
    }
    cpu_abort(env, "Unimplemented control register read 0x%x\n",
              reg);
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
    if (env->sr & SR_S) {
        if (env->sr & SR_M) {
            new_sp = M68K_SSP;
        } else {
            new_sp = M68K_ISP;
        }
    } else {
        new_sp = M68K_USP;
    }
    env->aregs[7] = env->sp[new_sp];
    env->current_sp = new_sp;
}

#if defined(CONFIG_USER_ONLY)

int cpu_m68k_handle_mmu_fault (CPUM68KState *env, target_ulong address, int rw,
                               int mmu_idx)
{
    env->exception_index = EXCP_ACCESS;
    env->mmu.ar = address;
    return 1;
}

#else

/* MMU: 68040 only */

static void print_address_zone(FILE *f, fprintf_function cpu_fprintf,
                               uint32_t logical, uint32_t physical,
                               uint32_t size, int attr)
{
    cpu_fprintf(f, "%08x - %08x -> %08x - %08x %c ",
                logical, logical + size - 1,
                physical, physical + size - 1,
                attr & 4 ? 'W' : '-');
    size >>= 10;
    if (size < 1024) {
        cpu_fprintf(f, "(%d KiB)\n", size);
    } else {
        size >>= 10;
        if (size < 1024) {
            cpu_fprintf(f, "(%d MiB)\n", size);
        } else {
            size >>= 10;
            cpu_fprintf(f, "(%d GiB)\n", size);
        }
    }
}

static void dump_address_map(FILE *f, fprintf_function cpu_fprintf,
                             CPUM68KState *env, uint32_t root_pointer)
{
    int i, j, k;
    int tic_size, tic_shift;
    uint32_t tib_mask;
    uint32_t tia, tib, tic;
    uint32_t logical = 0xffffffff, physical = 0xffffffff;
    uint32_t first_logical = 0xffffffff, first_physical = 0xffffffff;
    uint32_t last_logical, last_physical;
    int32_t size;
    int last_attr = -1, attr = -1;

    if (env->mmu.tcr & 0x4000) {
        /* 8k page */
        tic_size = 32;
        tic_shift = 13;
        tib_mask = 0xffffff80;
    } else {
        /* 4k page */
        tic_size = 64;
        tic_shift = 12;
        tib_mask = 0xffffff00;
    }
    for (i = 0; i < 128; i++) {
        tia = ldl_phys(root_pointer + i * 4);
        if ((tia & 2) == 0) /* INVALID */
            continue;
        for (j = 0; j < 128; j++) {
            tib = ldl_phys((tia & 0xffffff00) + j * 4);
            if ((tib & 2) == 0) /* INVALID */
                continue;

            for (k = 0; k < tic_size; k++) {
                tic = ldl_phys((tib & tib_mask) + k * 4);
                if ((tic & 3) == 0 || (tic & 3) == 3)
                    continue;
                if ((tic & 3) == 2) {
                    tic = ldl_phys(tic & ~3);
                }

                last_logical = logical;
                logical = (i << 25) | (j << 18) | (k << tic_shift);

                last_physical = physical;
                physical = tic & ~((1 << tic_shift) - 1);

                last_attr = attr;
                attr = tic & ((1 << tic_shift) - 1);

                if ((logical != (last_logical + (1 << tic_shift))) ||
                    (physical != (last_physical + (1 << tic_shift))) ||
                    (attr & 4) != (last_attr & 4)) {

                    if (first_logical != 0xffffffff) {
                        size = last_logical + (1 << tic_shift) -
                               first_logical;
                        print_address_zone(f, cpu_fprintf, first_logical,
                                           first_physical, size, last_attr);
                    }
                    first_logical = logical;
                    first_physical = physical;
                }
            }
        }
    }
    if (first_logical != logical || (attr & 4) != (last_attr & 4)) {
        size = logical + (1 << tic_shift) - first_logical;
        print_address_zone(f, cpu_fprintf, first_logical, first_physical, size,
                           last_attr);
    }
}

#define DUMP_CACHEFLAGS(a) \
    switch (a) { \
    case 0: /* cachable, write-through */ \
        cpu_fprintf(f, "T"); \
        break; \
    case 1: /* cachable, copyback */ \
        cpu_fprintf(f, "C"); \
        break; \
    case 2: /* noncachable, serialized */ \
        cpu_fprintf(f, "S"); \
        break; \
    case 3: /* noncachable */ \
        cpu_fprintf(f, "N"); \
        break; \
    }

static void dump_ttr(FILE *f, fprintf_function cpu_fprintf, uint32_t ttr)
{
    if (((ttr >> 15) & 1) == 0) {
        cpu_fprintf(f, "disabled\n");
        return;
    }
    cpu_fprintf(f, "Base: 0x%08x Mask: 0x%08x Control: ",
                ttr & 0xff000000, (ttr << 8) & 0xff000000);
    switch ((ttr >> 13) & 3) {
    case 0:
        cpu_fprintf(f, "U");
        break;
    case 1:
        cpu_fprintf(f, "S");
        break;
    default:
        cpu_fprintf(f, "*");
        break;
    }
    DUMP_CACHEFLAGS(ttr);
    if ((ttr >> 2) & 1) {
        cpu_fprintf(f, "R");
    } else {
        cpu_fprintf(f, "W");
    }
    cpu_fprintf(f, " U: %d\n", (ttr >> 8) & 3);
}

void dump_mmu(FILE *f, fprintf_function cpu_fprintf, CPUM68KState *env)
{
    if ((env->mmu.tcr & 0x8000) == 0) {
        cpu_fprintf(f, "Translation disabled\n");
        return;
    }
    cpu_fprintf(f, "Page Size: ");
    if (env->mmu.tcr & 0x4000) {
        cpu_fprintf(f, "8kB\n");
    } else {
        cpu_fprintf(f, "4kB\n");
    }
    
    cpu_fprintf(f, "MMUSR: ");
    if (env->mmu.mmusr & M68K_MMU_B_040) {
        cpu_fprintf(f, "BUS ERROR\n");
    } else {
        cpu_fprintf(f, "Phy=%08x Flags: ", env->mmu.mmusr & 0xfffff000);
        /* flags found on the page descriptor */
        if (env->mmu.mmusr & M68K_MMU_G_040) {
            cpu_fprintf(f, "G"); /* Global */
        } else {
            cpu_fprintf(f, ".");
        }
        if (env->mmu.mmusr & M68K_MMU_S_040) {
            cpu_fprintf(f, "S"); /* Supervisor */
        } else {
            cpu_fprintf(f, ".");
        }
        if (env->mmu.mmusr & M68K_MMU_M_040) {
            cpu_fprintf(f, "M"); /* Modified */
        } else {
            cpu_fprintf(f, ".");
        }
        if (env->mmu.mmusr & M68K_MMU_WP_040) {
            cpu_fprintf(f, "W"); /* Write protect */
        } else {
            cpu_fprintf(f, ".");
        }
        if (env->mmu.mmusr & M68K_MMU_T_040) {
            cpu_fprintf(f, "T"); /* Transparent */
        } else {
            cpu_fprintf(f, ".");
        }
        if (env->mmu.mmusr & M68K_MMU_R_040) {
            cpu_fprintf(f, "R"); /* Resident */
        } else {
            cpu_fprintf(f, ".");
        }
        cpu_fprintf(f, " Cache: ");
        DUMP_CACHEFLAGS(env->mmu.mmusr);
        cpu_fprintf(f, " U: %d\n", (env->mmu.mmusr >> 8) & 3);
        cpu_fprintf(f, "\n");
    }
    
    cpu_fprintf(f, "ITTR0: ");
    dump_ttr(f, cpu_fprintf, env->mmu.ittr0);
    cpu_fprintf(f, "ITTR1: ");
    dump_ttr(f, cpu_fprintf, env->mmu.ittr1);
    cpu_fprintf(f, "DTTR0: ");
    dump_ttr(f, cpu_fprintf, env->mmu.dttr0);
    cpu_fprintf(f, "DTTR1: ");
    dump_ttr(f, cpu_fprintf, env->mmu.dttr1);

    cpu_fprintf(f, "SRP: 0x%08x\n", env->mmu.srp);
    dump_address_map(f, cpu_fprintf, env, env->mmu.srp);

    cpu_fprintf(f, "URP: 0x%08x\n", env->mmu.urp);
    dump_address_map(f, cpu_fprintf, env, env->mmu.urp);
}

static int check_TTR(uint32_t ttr, hwaddr *physical, int *prot,
                     target_ulong addr, int access_type)
{
    uint32_t base, mask;

    /* check if transparent translation is enabled */

    if ((ttr & (1 << 15)) == 0) {
        return 0;
    }

    /* check mode access */

    if ((ttr & (1 << 14)) == 0) {
        if (ttr & (1 << 13)) {
            /* match only if supervisor */
            if ((access_type & ACCESS_SUPER) == 0) {
                return 0;
            }
        } else {
            /* match only if user */
            if ((access_type & ACCESS_SUPER) != 0) {
                return 0;
            }
        }
    }

    /* check address matching */

    base = ttr & 0xff000000;
    mask = ((ttr << 8) & 0xff000000) ^ 0xff000000;

    if ((addr & mask) != (base & mask)) {
        return 0;
    }

    *physical = addr;
    *prot = PAGE_READ | PAGE_EXEC;
    if ((ttr & (1 << 2)) == 0) {
        *prot |= PAGE_WRITE;
    }

    return 1;
}

#define GET_ENTRY() \
do { \
    next = ldl_phys(entry); \
    if ((next & 2) == 0) { \
        return -1; /* INVALID */ \
    } \
    if ((next & (1 << 3)) == 0) { \
        stl_phys(entry, next | (1 << 3)); \
    } \
    if (next & (1 << 2)) { \
        /* WRITE PROTECTED */ \
        if (access_type & ACCESS_PTEST) { \
            env->mmu.mmusr |= M68K_MMU_WP_040; \
        } \
        *prot &= ~PAGE_WRITE; \
        if (access_type & ACCESS_STORE) { \
            return -1; \
        } \
    } \
} while (0)

static int get_physical_address(CPUM68KState *env, hwaddr *physical,
                                int *prot, target_ulong address,
                                int access_type, target_ulong *page_size)
{
    uint32_t tia;
    uint32_t tib;
    uint32_t tic;
    uint32_t page_offset;
    uint32_t entry;
    uint32_t next;

    /* Transparent Translation (physical = logical) */

    if (access_type & ACCESS_CODE) {
        if (check_TTR(env->mmu.ittr0, physical, prot, address, access_type)) {
            goto TTR_exit;
        }
        if (check_TTR(env->mmu.ittr1, physical, prot, address, access_type)) {
            goto TTR_exit;
        }
    } else {
        if (check_TTR(env->mmu.dttr0, physical, prot, address, access_type)) {
            goto TTR_exit;
        }
        if (check_TTR(env->mmu.dttr1, physical, prot, address, access_type)) {
TTR_exit:
            if (access_type & ACCESS_PTEST) {
                /* Transparent Translation Register bit */
                env->mmu.mmusr = M68K_MMU_T_040 | M68K_MMU_R_040;
            }
            *page_size = TARGET_PAGE_SIZE;
            return 0;
        }
    }

    *prot = PAGE_READ | PAGE_WRITE;
    if (access_type & ACCESS_CODE) {
        *prot |= PAGE_EXEC;
    }
    if (access_type & ACCESS_SUPER) {
        next = env->mmu.srp;
    } else {
        next = env->mmu.urp;
    }

    tia = (address >> 23) & 0x1fc;
    entry = (next & ~0x1ff) | tia;
    GET_ENTRY();

    tib = (address >> 16) & 0x1fc;
    entry = (next & ~0x1ff) | tib;
    GET_ENTRY();

    if (env->mmu.tcr & 0x4000) {
        /* 8 kB page */
        tic = (address >> 11) & 0x7c;
        entry = (next & ~0x7f) | tic;
    } else {
        /* 4 kB page */
        tic = (address >> 10) & 0xfc;
        entry = (next & ~0xff) | tic;
    }

     next = ldl_phys(entry);

    if ((next & 3) == 0) {
        return -1;
    }
    if ((next & 3) == 2) {
        /* INDIRECT */
        entry = next & ~3;
        next = ldl_phys(entry);
    }
    if (!(access_type & ACCESS_PTEST)) {
        if (access_type & ACCESS_STORE) {
            if (next & (1 << 2)) { /* WRITE PROTECTED */
                if ((next & (1 << 3)) == 0) {
                    stl_phys(entry, next | (1 << 3));
                }
            } else if ((next & ((1 << 4) | (1 << 3))) != ((1 << 4) | (1 << 3))) {
                    stl_phys(entry, next | (1 << 4) | (1 << 3));
            }
        } else {
            if ((next & (1 << 3)) == 0) {
                stl_phys(entry, next | (1 << 3));
            }
        }
    } else {
        env->mmu.mmusr |= next & M68K_MMU_SR_MASK_040;
    }

    if (env->mmu.tcr & 0x4000) {
        *page_size = 8192;
        page_offset = address & 0x1fff;
        *physical = (next & ~0x1fff) + page_offset;
    } else {
        *page_size = 4096;
        page_offset = address & 0x0fff;
        *physical = (next & ~0x0fff) + page_offset;
    }

    if (access_type & ACCESS_PTEST) {
        env->mmu.mmusr |= *physical & 0xfffff000;
        env->mmu.mmusr |= M68K_MMU_R_040;
    }

    if (next & (1 << 2)) {
        /* WRITE PROTECTED */
        *prot &= ~PAGE_WRITE;
        if (access_type & ACCESS_STORE) {
            return -1;
        }
    }
    if (next & (1 << 7)) {
        /* SUPERVISOR */
        if ((access_type & ACCESS_SUPER) == 0) {
            return -1;
        }
    }

    return 0;
}

hwaddr m68k_cpu_get_phys_page_debug(CPUState *cs, vaddr addr)
{
    M68kCPU *cpu = M68K_CPU(cs);
    CPUM68KState *env = &cpu->env;
    hwaddr phys_addr;
    int prot;
    int access_type;
    target_ulong page_size;

    if ((env->mmu.tcr & (1 << 15)) == 0) {
        /* MMU disabled */
        return addr;
    }

    access_type = ACCESS_INT;
    if (env->sr & SR_S)
        access_type |= ACCESS_SUPER;
    if (get_physical_address(env, &phys_addr, &prot,
                             addr, access_type, &page_size) != 0) {
        return -1;
    }
    return phys_addr;
}

int cpu_m68k_handle_mmu_fault (CPUM68KState *env, target_ulong address, int rw,
                               int mmu_idx)
{
    hwaddr physical;
    int prot;
    int access_type;
    int ret;
    target_ulong page_size;

    if ((env->mmu.tcr & (1 << 15)) == 0) {
        /* MMU disabled */
        tlb_set_page(env, address & TARGET_PAGE_MASK,
                     address & TARGET_PAGE_MASK,
                     PAGE_READ | PAGE_WRITE | PAGE_EXEC,
                     mmu_idx, TARGET_PAGE_SIZE);
        return 0;
    }

    if (rw == 2) {
        access_type = ACCESS_CODE;
        rw = 0;
    } else {
        access_type = ACCESS_INT;
        if (rw) {
            access_type |= ACCESS_STORE;
        }
    }

    if (mmu_idx != MMU_USER_IDX) {
        access_type |= ACCESS_SUPER;
    }

    ret = get_physical_address(env, &physical, &prot,
                               address, access_type, &page_size);
    if (ret == 0) {
        tlb_set_page(env, address & TARGET_PAGE_MASK,
                     physical & TARGET_PAGE_MASK,
                     prot, mmu_idx, page_size);
        return 0;
    }
    /* page fault */
    env->mmu.ssw = M68K_ATC_040;
    switch (env->data_size) {
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
    if (access_type & ACCESS_SUPER) {
        env->mmu.ssw |= 4;
    }
    if ((access_type & ACCESS_CODE) == 0) {
        env->mmu.ssw |= 2;
    } else {
        env->mmu.ssw |= 1;
    }
    if (!(access_type & ACCESS_STORE)) {
        env->mmu.ssw |= M68K_RW_040;
    }
    env->mmu.ar = address;
    env->exception_index = EXCP_ACCESS;
    return 1;
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

uint32_t HELPER(subx8_cc)(CPUM68KState *env, uint32_t op1, uint32_t op2)
{
    uint8_t res;
    uint32_t old_flags;

    old_flags = env->cc_dest;
    if (env->cc_x) {
        env->cc_x = ((uint8_t)op1 <= (uint8_t)op2);
        env->cc_op = CC_OP_SUBXB;
        res = (uint8_t)op1 - ((uint8_t)op2 + 1);
    } else {
        env->cc_x = ((uint8_t)op1 < (uint8_t)op2);
        env->cc_op = CC_OP_SUBB;
        res = (uint8_t)op1 - (uint8_t)op2;
    }
    env->cc_dest = res;
    env->cc_src = (uint8_t)op2;
    cpu_m68k_flush_flags(env, env->cc_op);
    /* !Z is sticky.  */
    env->cc_dest &= (old_flags | ~CCF_Z);
    return (op1 & 0xffffff00) | res;
}

uint32_t HELPER(subx16_cc)(CPUM68KState *env, uint32_t op1, uint32_t op2)
{
    uint16_t res;
    uint32_t old_flags;

    old_flags = env->cc_dest;
    if (env->cc_x) {
        env->cc_x = ((uint16_t)op1 <= (uint16_t)op2);
        env->cc_op = CC_OP_SUBXW;
        res = (uint16_t)op1 - ((uint16_t)op2 + 1);
    } else {
        env->cc_x = ((uint16_t)op1 < (uint16_t)op2);
        env->cc_op = CC_OP_SUBW;
        res = (uint16_t)op1 - (uint16_t)op2;
    }
    env->cc_dest = res;
    env->cc_src = (uint16_t)op2;
    cpu_m68k_flush_flags(env, env->cc_op);
    /* !Z is sticky.  */
    env->cc_dest &= (old_flags | ~CCF_Z);
    return (op1 & 0xffff0000) | res;
}

uint32_t HELPER(subx32_cc)(CPUM68KState *env, uint32_t op1, uint32_t op2)
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

uint32_t HELPER(addx8_cc)(CPUM68KState *env, uint32_t op1, uint32_t op2)
{
    uint8_t res;
    uint32_t old_flags;

    old_flags = env->cc_dest;
    if (env->cc_x) {
        res = (uint8_t)op1 + (uint8_t)op2 + 1;
        env->cc_x = (res <= (uint8_t)op2);
        env->cc_op = CC_OP_ADDXB;
    } else {
        res = (uint8_t)op1 + (uint8_t)op2;
        env->cc_x = (res < (uint8_t)op2);
        env->cc_op = CC_OP_ADDB;
    }
    env->cc_dest = res;
    env->cc_src = (uint8_t)op2;
    cpu_m68k_flush_flags(env, env->cc_op);
    /* !Z is sticky.  */
    env->cc_dest &= (old_flags | ~CCF_Z);
    return (op1 & 0xffffff00) | res;
}

uint32_t HELPER(addx16_cc)(CPUM68KState *env, uint32_t op1, uint32_t op2)
{
    uint16_t res;
    uint32_t old_flags;

    old_flags = env->cc_dest;
    if (env->cc_x) {
        res = (uint16_t)op1 + (uint16_t)op2 + 1;
        env->cc_x = (res <= (uint16_t)op2);
        env->cc_op = CC_OP_ADDXW;
    } else {
        res = (uint16_t)op1 + (uint16_t)op2;
        env->cc_x = (res < (uint16_t)op2);
        env->cc_op = CC_OP_ADDW;
    }
    env->cc_dest = res;
    env->cc_src = (uint16_t)op2;
    cpu_m68k_flush_flags(env, env->cc_op);
    /* !Z is sticky.  */
    env->cc_dest &= (old_flags | ~CCF_Z);
    return (op1 & 0xffff0000) | res;
}

uint32_t HELPER(addx32_cc)(CPUM68KState *env, uint32_t op1, uint32_t op2)
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

uint32_t HELPER(xflag_lt_i8)(uint32_t a, uint32_t b)
{
    return (uint8_t)a < (uint8_t)b;
}

uint32_t HELPER(xflag_lt_i16)(uint32_t a, uint32_t b)
{
    return (uint16_t)a < (uint16_t)b;
}

uint32_t HELPER(xflag_lt_i32)(uint32_t a, uint32_t b)
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
        cf = 0; \
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
    env->cc_src = cf ? CCF_C : 0; \
    if (shift) env->cc_x = (cf != 0); \
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
        cf = 0; \
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
    env->cc_src = cf ? CCF_C : 0; \
    if (shift) env->cc_x = (cf != 0); \
    env->cc_dest = result; \
    return result; \
}

HELPER_SHR(uint8_t, 8)
HELPER_SHR(uint16_t, 16)
HELPER_SHR(uint32_t, 32)

#define HELPER_SAL(type, bits) \
uint32_t HELPER(glue(glue(sal, bits),_cc))(CPUM68KState *env, uint32_t val, uint32_t shift) \
{ \
    type result; \
    uint32_t cf; \
    uint32_t vf; \
    uint32_t m; \
    shift &= 63; \
    if (shift == 0) { \
        vf = 0; \
    } else if (shift < bits) { \
        m = ((1llu << (shift + 1)) - 1) << (bits - shift - 1); \
        vf = (val & m) != m && (val & m) != 0; \
    } else {\
        m = (1llu << bits) - 1; \
        vf = (val & m) != 0; \
    }\
    if (shift == 0) { \
        result = (type)val; \
        cf = 0; \
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
    env->cc_src = (cf ? CCF_C : 0) | (vf ? CCF_V : 0); \
    if (shift) env->cc_x = (cf != 0); \
    env->cc_dest = result; \
    return result; \
}

HELPER_SAL(int8_t, 8)
HELPER_SAL(int16_t, 16)
HELPER_SAL(int32_t, 32)

#define HELPER_SAR(type, bits) \
uint32_t HELPER(glue(glue(sar, bits), _cc))(CPUM68KState *env, uint32_t val, uint32_t shift)					\
{ \
    type result; \
    uint32_t cf; \
    shift &= 63; \
    if (shift == 0) { \
        result = (type)val; \
        cf = 0; \
    } else if (shift < bits) { \
        result = (type)val >> shift; \
        cf = ((type)val >> (shift - 1)) & 1; \
    } else /* shift >= bits */ { \
        result = (type)val >> (bits - 1); \
        cf = (type)val >> (bits - 1); \
    } \
    env->cc_src = cf ? CCF_C : 0; \
    if (shift) env->cc_x = (cf != 0); \
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
       if (count == bits)\
           result = ((type)env->cc_x << (bits - count));\
       else \
           result = ((type)val >> count) | ((type)env->cc_x << (bits - count));\
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
       if (count == bits) \
           result = ((type)env->cc_x << (count - 1)); \
       else \
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
    [0x00] = floatx80_pi,                                       /* Pi */

    [0x0b] = { .high = 0x3ffd, .low = 0x9a209a84fbcff798ULL },  /* Log10(2) */
    [0x0c] = floatx80_e,                                        /* e        */
    [0x0d] = floatx80_log2e,                                    /* Log2(e)  */
    [0x0e] = { .high = 0x3ffd, .low = 0xde5bd8a937287195ULL },  /* Log10(e) */
    [0x0f] = floatx80_zero,                                     /* Zero     */

    [0x30] = floatx80_ln2,                                      /* ln(2)    */
    [0x31] = { .high = 0x4000, .low = 0x935d8dddaaa8ac17ULL },  /* ln(10)   */
    [0x32] = floatx80_one,                                      /* 10^0     */
    [0x33] = floatx80_10,                                       /* 10^1     */
    [0x34] = { .high = 0x4005, .low = 0xc800000000000000ULL },  /* 10^2     */
    [0x35] = { .high = 0x400c, .low = 0x9c40000000000000ULL },  /* 10^4     */
    [0x36] = { .high = 0x4019, .low = 0xbebc200000000000ULL },  /* 10^8     */
    [0x37] = { .high = 0x4034, .low = 0x8e1bc9bf04000000ULL },  /* 10^16    */
    [0x38] = { .high = 0x4069, .low = 0x9dc5ada82b70b59eULL },  /* 10^32    */
    [0x39] = { .high = 0x40d3, .low = 0xc2781f49ffcfa6d5ULL },  /* 10^64    */
    [0x3a] = { .high = 0x41a8, .low = 0x93ba47c980e98ce0ULL },  /* 10^128   */
    [0x3b] = { .high = 0x4351, .low = 0xaa7eebfb9df9de8eULL },  /* 10^256   */
    [0x3c] = { .high = 0x46a3, .low = 0xe319a0aea60e91c7ULL },  /* 10^512   */
    [0x3d] = { .high = 0x4d48, .low = 0xc976758681750c17ULL },  /* 10^1024  */
    [0x3e] = { .high = 0x5a92, .low = 0x9e8b3b5dc53d5de5ULL },  /* 10^2048  */
    [0x3f] = { .high = 0x7525, .low = 0xc46052028a20979bULL },  /* 10^4096  */
};

static inline floatx80 FP0_to_floatx80(CPUM68KState *env)
{
    floatx80 res;

    res.high = env->fp0h;
    res.low = env->fp0l;

    return res;
}

static inline void floatx80_to_FP0(CPUM68KState *env, floatx80 res)
{
    env->fp0h = res.high;
    env->fp0l = res.low;
}

static inline void floatx80_to_FP1(CPUM68KState *env, floatx80 res)
{
    env->fp1h = res.high;
    env->fp1l = res.low;
}

static inline int32_t FP0_to_int32(CPUM68KState *env)
{
    return env->fp0h;
}

static inline void int32_to_FP0(CPUM68KState *env, int32_t val)
{
    env->fp0h = val;
}

static inline float32 FP0_to_float32(CPUM68KState *env)
{
    return *(float32*)&env->fp0h;
}

static inline void float32_to_FP0(CPUM68KState *env, float32 val)
{

    env->fp0h = *(uint32_t*)&val;
}

static inline float64 FP0_to_float64(CPUM68KState *env)
{
    return *(float64*)&env->fp0l;
}

static inline void float64_to_FP0(CPUM68KState *env, float64 val)
{
    env->fp0l = *(uint64_t*)&val;
}

static inline floatx80 FP1_to_floatx80(CPUM68KState *env)
{
    floatx80 res;

    res.high = env->fp1h;
    res.low = env->fp1l;

    return res;
}

static inline long double floatx80_to_ldouble(floatx80 val)
{
	if (floatx80_is_infinity(val)) {
		if (floatx80_is_neg(val)) {
			return -__builtin_infl();
		}
		return __builtin_infl();
	}
	if (floatx80_is_any_nan(val)) {
		char low[20];
		sprintf(low, "0x%016"PRIx64, val.low);

		return nanl(low);
	}

	return *(long double *)&val;
}

static inline floatx80 ldouble_to_floatx80(long double val)
{
	floatx80 res;

	if (isinf(val)) {
		res.high = floatx80_default_nan.high;
		res.low = 0;
	}
	if (isinf(val) < 0) {
		res.high |= 0x8000;
	}
	if (isnan(val)) {
		res.high = floatx80_default_nan.high;
		res.low = *(uint64_t*)((char *)&val + 4);
	}
	return *(floatx80*)&val;
}

void HELPER(const_FP0)(CPUM68KState *env, uint32_t offset)
{
    env->fp0h = fpu_rom[offset].high;
    env->fp0l = fpu_rom[offset].low;
    DBG_FPUH("ROM[0x%02x] %"PRIxFPH" %"PRIxFPL" %.17Lg\n",
            offset, env->fp0h, env->fp0l, floatx80_to_ldouble(FP0_to_floatx80(env)));
}

static inline void restore_precision_mode(CPUM68KState *env)
{
    int rounding_precision;

    rounding_precision = (env->fpcr >> 6) & 0x03;

    switch (rounding_precision) {
    case 0: /* extended */
        set_floatx80_rounding_precision(80, &env->fp_status);
        break;
    case 1: /* single */
        set_floatx80_rounding_precision(32, &env->fp_status);
        break;
    case 2: /* double */
        set_floatx80_rounding_precision(64, &env->fp_status);
        break;
    case 3: /* reserved */
    default:
        break;
    }
}

static inline void restore_rounding_mode(CPUM68KState *env)
{
    int rounding_mode;

    rounding_mode = (env->fpcr >> 4) & 0x03;

    switch (rounding_mode) {
    case 0: /* round to nearest */
        set_float_rounding_mode(float_round_nearest_even, &env->fp_status);
        break;
    case 1: /* round to zero */
        set_float_rounding_mode(float_round_to_zero, &env->fp_status);
        break;
    case 2: /* round toward minus infinity */
        set_float_rounding_mode(float_round_down, &env->fp_status);
        break;
    case 3: /* round toward positive infinity */
        set_float_rounding_mode(float_round_up, &env->fp_status);
        break;
    }
}

void HELPER(set_fpcr)(CPUM68KState *env, uint32_t val)
{
    DBG_FPUH("set_fpcr %04x\n", val);

    env->fpcr = val & 0xffff;

    restore_precision_mode(env);
    restore_rounding_mode(env);
}

void HELPER(exts32_FP0)(CPUM68KState *env)
{
    floatx80 res;

    DBG_FPUH("exts32_FP0 %d", FP0_to_int32(env));

    res = int32_to_floatx80(FP0_to_int32(env), &env->fp_status);

    DBG_FPU(" = %Lg\n", floatx80_to_ldouble(res));
    floatx80_to_FP0(env, res);
}

void HELPER(extf32_FP0)(CPUM68KState *env)
{
    floatx80 res;

    DBG_FPUH("extf32_FP0 %f %08x", FLOAT(FP0_to_float32(env)),
                                   FP0_to_int32(env));
    res = float32_to_floatx80(FP0_to_float32(env), &env->fp_status);
    DBG_FPU(" = %Lg\n", floatx80_to_ldouble(res));

    floatx80_to_FP0(env, res);
}

void HELPER(extf64_FP0)(CPUM68KState *env)
{
    floatx80 res;
    uint64_t val;

    val = FP0_to_float64(env);
    DBG_FPUH("extf64_FP0 0x%016"PRIx64", %g", val, *(double*)&val);
    res = float64_to_floatx80(val, &env->fp_status);
    DBG_FPU(" = %Lg\n", floatx80_to_ldouble(res));

    floatx80_to_FP0(env, res);
}

void HELPER(extp96_FP0)(CPUM68KState *env)
{
}

void HELPER(reds32_FP0)(CPUM68KState *env)
{
    floatx80 val;
    int32_t res;

    val = FP0_to_floatx80(env);
    DBG_FPUH("reds32_FP0 %Lg (%08x %016"PRIx64")",
	      floatx80_to_ldouble(val), env->fp0h, env->fp0l);
    res = floatx80_to_int32(val, &env->fp_status);
    DBG_FPU(" = %d\n", res);

    int32_to_FP0(env, res);
}

void HELPER(redf32_FP0)(CPUM68KState *env)
{
    floatx80 val;
    float32 res;

    val = FP0_to_floatx80(env);
    DBG_FPUH("redf32_FP0 %Lg", floatx80_to_ldouble(val));
    res = floatx80_to_float32(val, &env->fp_status);
    DBG_FPU(" = %f\n", FLOAT(res));

    float32_to_FP0(env, res);
}

void HELPER(redf64_FP0)(CPUM68KState *env)
{
    floatx80 val;
    float64 res;

    val = FP0_to_floatx80(env);
    DBG_FPUH("redf64_FP0 %Lg", floatx80_to_ldouble(val));
    res = floatx80_to_float64(val, &env->fp_status);
    DBG_FPU(" = %g\n", *(double*)&res);

    float64_to_FP0(env, res);
}

void HELPER(redp96_FP0)(CPUM68KState *env)
{
    DBG_FPUH("redp96_FP0\n");
}

void HELPER(iround_FP0)(CPUM68KState *env)
{
    floatx80 res;

    res = FP0_to_floatx80(env);

    DBG_FPUH("iround_FP0 %Lg", floatx80_to_ldouble(res));

    res = floatx80_round_to_int(res, &env->fp_status);

    DBG_FPU(" = %Lg\n", floatx80_to_ldouble(res));

    floatx80_to_FP0(env, res);
}

void HELPER(sinh_FP0)(CPUM68KState *env)
{
    floatx80 res;
    long double val;

    res = FP0_to_floatx80(env);
    val = floatx80_to_ldouble(res);

    DBG_FPUH("sinh_FP0 %Lg", val);
    val = sinhl(val);
    DBG_FPU(" = %Lg", val);
    res = ldouble_to_floatx80(val);
    floatx80_to_FP0(env, res);
}

void HELPER(itrunc_FP0)(CPUM68KState *env)
{
    floatx80 res;

    res = FP0_to_floatx80(env);
    DBG_FPUH("itrunc_FP0 %Lg", floatx80_to_ldouble(res));

    set_float_rounding_mode(float_round_to_zero, &env->fp_status);
    res = floatx80_round_to_int(res, &env->fp_status);
    restore_rounding_mode(env);

    DBG_FPU(" = %Lg\n", floatx80_to_ldouble(res));

    floatx80_to_FP0(env, res);
}

void HELPER(sqrt_FP0)(CPUM68KState *env)
{
    floatx80 res;

    res = FP0_to_floatx80(env);
    DBG_FPUH("sqrt_FP0 %Lg", floatx80_to_ldouble(res));
    res = floatx80_sqrt(res, &env->fp_status);
    DBG_FPU("  = %Lg\n", floatx80_to_ldouble(res));

    floatx80_to_FP0(env, res);
}

void HELPER(lognp1_FP0)(CPUM68KState *env)
{
    floatx80 val;
    long double res;

    val = FP0_to_floatx80(env);
    DBG_FPUH("lognp1_FP0 %Lg", floatx80_to_ldouble(val));
    res = logl(floatx80_to_ldouble(val) + 1.0);
    DBG_FPU(" = %Lg\n", res);

    floatx80_to_FP0(env, ldouble_to_floatx80(res));
}

void HELPER(ln_FP0)(CPUM68KState *env)
{
    floatx80 val;
    long double res;

    val = FP0_to_floatx80(env);
    DBG_FPUH("ln_FP0 %Lg", floatx80_to_ldouble(val));
    res = logl(floatx80_to_ldouble(val));
    DBG_FPU(" = %Lg\n", res);

    floatx80_to_FP0(env, ldouble_to_floatx80(res));
}

void HELPER(log10_FP0)(CPUM68KState *env)
{
    floatx80 val;
    long double res;

    val = FP0_to_floatx80(env);
    DBG_FPUH("log10_FP0 %Lg", floatx80_to_ldouble(val));
    res = log10l(floatx80_to_ldouble(val));
    DBG_FPU(" = %Lg\n", res);

    floatx80_to_FP0(env, ldouble_to_floatx80(res));
}

void HELPER(atan_FP0)(CPUM68KState *env)
{
    floatx80 res;
    long double val;

    res = FP0_to_floatx80(env);
    val = floatx80_to_ldouble(res);

    DBG_FPUH("atan_FP0 %Lg", val);
    val = atanl(val);
    DBG_FPU(" = %Lg", val);
    res = ldouble_to_floatx80(val);
    floatx80_to_FP0(env, res);
}

void HELPER(asin_FP0)(CPUM68KState *env)
{
    floatx80 res;
    long double val;

    res = FP0_to_floatx80(env);
    val = floatx80_to_ldouble(res);

    DBG_FPUH("asin_FP0 %Lg", val);
    val = asinl(val);
    DBG_FPU(" = %Lg", val);
    res = ldouble_to_floatx80(val);
    floatx80_to_FP0(env, res);
}

void HELPER(atanh_FP0)(CPUM68KState *env)
{
    floatx80 res;
    long double val;

    res = FP0_to_floatx80(env);
    val = floatx80_to_ldouble(res);

    DBG_FPUH("atanh_FP0 %Lg", val);
    val = atanhl(val);
    DBG_FPU(" = %Lg", val);
    res = ldouble_to_floatx80(val);
    floatx80_to_FP0(env, res);
}

void HELPER(sin_FP0)(CPUM68KState *env)
{
    floatx80 res;
    long double val;

    res = FP0_to_floatx80(env);
    val = floatx80_to_ldouble(res);

    DBG_FPUH("sin_FP0 %Lg", val);
    val = sinl(val);
    DBG_FPU(" = %Lg", val);
    res = ldouble_to_floatx80(val);
    floatx80_to_FP0(env, res);
}

void HELPER(tanh_FP0)(CPUM68KState *env)
{
    floatx80 res;
    long double val;

    res = FP0_to_floatx80(env);
    val = floatx80_to_ldouble(res);

    DBG_FPUH("tanh_FP0 %Lg", val);
    val = tanhl(val);
    DBG_FPU(" = %Lg", val);
    res = ldouble_to_floatx80(val);
    floatx80_to_FP0(env, res);
}

void HELPER(tan_FP0)(CPUM68KState *env)
{
    floatx80 res;
    long double val;

    res = FP0_to_floatx80(env);
    val = floatx80_to_ldouble(res);

    DBG_FPUH("tan_FP0 %Lg", val);
    val = tanl(val);
    DBG_FPU(" = %Lg", val);
    res = ldouble_to_floatx80(val);
    floatx80_to_FP0(env, res);
}

void HELPER(exp_FP0)(CPUM68KState *env)
{
    floatx80 f;
    long double res;

    f = FP0_to_floatx80(env);

    DBG_FPUH("exp_FP0 %Lg", floatx80_to_ldouble(f));

    res = expl(floatx80_to_ldouble(f));

    DBG_FPU(" = %Lg\n", res);
    floatx80_to_FP0(env, ldouble_to_floatx80(res));
}

void HELPER(exp2_FP0)(CPUM68KState *env)
{
    floatx80 f;
    long double res;

    f = FP0_to_floatx80(env);

    DBG_FPUH("exp2_FP0 %Lg", floatx80_to_ldouble(f));

    res = exp2l(floatx80_to_ldouble(f));

    DBG_FPU(" = %Lg\n", res);
    floatx80_to_FP0(env, ldouble_to_floatx80(res));
}

void HELPER(exp10_FP0)(CPUM68KState *env)
{
    floatx80 res;
    long double val;

    res = FP0_to_floatx80(env);
    val = floatx80_to_ldouble(res);

    DBG_FPUH("exp2_FP0 %Lg", val);
    val = exp10l(val);
    DBG_FPU(" = %Lg", val);
    res = ldouble_to_floatx80(val);
    floatx80_to_FP0(env, res);
}

void HELPER(abs_FP0)(CPUM68KState *env)
{
    floatx80 res;

    res = FP0_to_floatx80(env);
    DBG_FPUH("abs_FP0 %Lg", floatx80_to_ldouble(res));
    res = floatx80_abs(res);
    DBG_FPU(" = %Lg\n", floatx80_to_ldouble(res));

    floatx80_to_FP0(env, res);
}

void HELPER(cosh_FP0)(CPUM68KState *env)
{
    floatx80 res;
    long double val;

    res = FP0_to_floatx80(env);
    val = floatx80_to_ldouble(res);

    DBG_FPUH("cosh_FP0 %Lg", val);
    val = coshl(val);
    DBG_FPU(" = %Lg", val);
    res = ldouble_to_floatx80(val);
    floatx80_to_FP0(env, res);
}

void HELPER(chs_FP0)(CPUM68KState *env)
{
    floatx80 res;

    res = FP0_to_floatx80(env);
    DBG_FPUH("chs_FP0 %Lg", floatx80_to_ldouble(res));
    res = floatx80_chs(res);
    DBG_FPU(" = %Lg\n", floatx80_to_ldouble(res));

    floatx80_to_FP0(env, res);
}

void HELPER(acos_FP0)(CPUM68KState *env)
{
    floatx80 res;
    long double val;

    res = FP0_to_floatx80(env);
    val = floatx80_to_ldouble(res);

    DBG_FPUH("acos_FP0 %Lg", val);
    val = acosl(val);
    DBG_FPU(" = %Lg", val);
    res = ldouble_to_floatx80(val);
    floatx80_to_FP0(env, res);
}

void HELPER(cos_FP0)(CPUM68KState *env)
{
    floatx80 res;
    long double val;

    res = FP0_to_floatx80(env);
    val = floatx80_to_ldouble(res);

    DBG_FPUH("cos_FP0 %Lg", val);
    val = cosl(val);
    DBG_FPU(" = %Lg", val);
    res = ldouble_to_floatx80(val);
    floatx80_to_FP0(env, res);
}

void HELPER(getexp_FP0)(CPUM68KState *env)
{
    int32_t exp;
    floatx80 res;

    DBG_FPUH("getexp_FP0 %Lg", floatx80_to_ldouble(FP0_to_floatx80(env)));

    DBG_FPU(" fp0h 0x%08x fp0l 0x%016" PRIx64, env->fp0h, env->fp0l);

    exp = (env->fp0h & 0x7fff) - 0x3fff;

    res = int32_to_floatx80(exp, &env->fp_status);

    DBG_FPU(" = %Lg", floatx80_to_ldouble(res));
    floatx80_to_FP0(env, res);
}

void HELPER(scale_FP0_FP1)(CPUM68KState *env)
{
    int32_t scale;
    int32_t exp;

    DBG_FPUH("scale_FP0 %Lg", floatx80_to_ldouble(FP0_to_floatx80(env)));

    DBG_FPU(" fp0h 0x%08x fp0l 0x%016" PRIx64, env->fp0h, env->fp0l);

    scale = floatx80_to_int32(FP0_to_floatx80(env), &env->fp_status);

    exp = (env->fp1h & 0x7fff) + scale;

    env->fp0h = (env->fp1h & 0x8000) | (exp & 0x7fff);
    env->fp0l = env->fp1l;
    DBG_FPU(" = %Lg", floatx80_to_ldouble(FP0_to_floatx80(env)));
}

void HELPER(add_FP0_FP1)(CPUM68KState *env)
{
    floatx80 res;

    DBG_FPUH("add_FP0_FP1 %Lg %Lg", floatx80_to_ldouble(FP0_to_floatx80(env)),
            floatx80_to_ldouble(FP1_to_floatx80(env)));
    res = floatx80_add(FP0_to_floatx80(env), FP1_to_floatx80(env),
                      &env->fp_status);
    DBG_FPU(" = %Lg\n", floatx80_to_ldouble(res));

    floatx80_to_FP0(env, res);
}

void HELPER(sub_FP0_FP1)(CPUM68KState *env)
{
    floatx80 res;

    DBG_FPUH("sub_FP0 %Lg %Lg", floatx80_to_ldouble(FP0_to_floatx80(env)),
            floatx80_to_ldouble(FP1_to_floatx80(env)));
    res = floatx80_sub(FP1_to_floatx80(env), FP0_to_floatx80(env),
                       &env->fp_status);
    DBG_FPU(" = %Lg\n", floatx80_to_ldouble(res));

    floatx80_to_FP0(env, res);
}

void HELPER(mul_FP0_FP1)(CPUM68KState *env)
{
    floatx80 res;

    DBG_FPUH("mul_FP0_FP1 %Lg %Lg",
            floatx80_to_ldouble(FP0_to_floatx80(env)), floatx80_to_ldouble(FP1_to_floatx80(env)));
    res = floatx80_mul(FP0_to_floatx80(env), FP1_to_floatx80(env),
                       &env->fp_status);
    DBG_FPU(" = %Lg\n", floatx80_to_ldouble(res));

    floatx80_to_FP0(env, res);
}

void HELPER(div_FP0_FP1)(CPUM68KState *env)
{
    floatx80 res;

    DBG_FPUH("div_FP0_FP1 %Lg %Lg",
            floatx80_to_ldouble(FP0_to_floatx80(env)), floatx80_to_ldouble(FP1_to_floatx80(env)));
    res = floatx80_div(FP1_to_floatx80(env), FP0_to_floatx80(env),
                       &env->fp_status);
    DBG_FPU(" = %Lg\n", floatx80_to_ldouble(res));

    floatx80_to_FP0(env, res);
}

void HELPER(mod_FP0_FP1)(CPUM68KState *env)
{
    floatx80 res;
    long double src, dst;

    src = floatx80_to_ldouble(FP0_to_floatx80(env));
    dst = floatx80_to_ldouble(FP1_to_floatx80(env));

    DBG_FPUH("mod_FP0_FP1 %Lg %Lg", src, dst);
    dst = fmodl(dst, src);
    DBG_FPU(" = %Lg\n", dst);

    res = ldouble_to_floatx80(dst);
    floatx80_to_FP0(env, res);
}

void HELPER(sincos_FP0_FP1)(CPUM68KState *env)
{
    floatx80 res;
    long double val, valsin, valcos;

    res = FP0_to_floatx80(env);
    val = floatx80_to_ldouble(res);

    DBG_FPUH("sincos_FP0 %Lg", val);
    sincosl(val, &valsin, &valcos);
    DBG_FPU(" = %Lg, %Lg", valsin, valcos);
    res = ldouble_to_floatx80(valsin);
    floatx80_to_FP0(env, res);
    res = ldouble_to_floatx80(valcos);
    floatx80_to_FP1(env, res);
}

static void set_fpcc(CPUM68KState *env, floatx80 val)
{
    uint32_t fpcc = 0;

    if (floatx80_is_any_nan(val)) {
        fpcc |= FCCF_A;
    }
    if (floatx80_is_infinity(val)) {
        fpcc |= FCCF_I;
    }
    if (floatx80_is_neg(val)) {
        fpcc |= FCCF_N;
    }
    if (floatx80_is_zero(val)) {
        fpcc |= FCCF_Z;
    }

    DBG_FPU("FPCC 0x%02x %c%c%c%c\n", fpcc >> FCCF_SHIFT,
            fpcc & FCCF_N ? 'N' : '-',
            fpcc & FCCF_Z ? 'Z' : '-',
            fpcc & FCCF_I ? 'I' : '-',
            fpcc & FCCF_A ? 'A' : '-');
    env->fpsr = (env->fpsr & ~FCCF_MASK) | fpcc;
}

void HELPER(fcmp_FP0_FP1)(CPUM68KState *env)
{
    /* ??? This may incorrectly raise exceptions.  */
    /* ??? Should flush denormals to zero.  */
    floatx80 res;
    DBG_FPU("cmp_FP0_FP1 %Lg %Lg\n", floatx80_to_ldouble(FP0_to_floatx80(env)),
            floatx80_to_ldouble(FP1_to_floatx80(env)));
    res = floatx80_sub(FP1_to_floatx80(env), FP0_to_floatx80(env),
                       &env->fp_status);
    if (floatx80_is_any_nan(res)) {
        /* +/-inf compares equal against itself, but sub returns nan.  */
        if (!floatx80_is_any_nan(FP0_to_floatx80(env))
            && !floatx80_is_any_nan(FP1_to_floatx80(env))) {
            res = floatx80_zero;
            if (floatx80_lt_quiet(FP1_to_floatx80(env), res, &env->fp_status))
                res = floatx80_chs(res);
        }
    }

    set_fpcc(env, res);
}

void HELPER(compare_FP0)(CPUM68KState *env)
{
    DBG_FPU("compare_FP0 %Lg\n", floatx80_to_ldouble(FP0_to_floatx80(env)));
    set_fpcc(env, FP0_to_floatx80(env));
}

void HELPER(update_fpsr)(CPUM68KState *env)
{
    int eflags;

    DBG_FPU("update_fpsr");

    env->fpsr &= 0xff00;

    eflags = get_float_exception_flags(&env->fp_status);

    if (eflags & float_flag_invalid) {
        env->fpsr |= 0x0080; 
        env->fpsr |= 0x2000; 
    }
    if (eflags & float_flag_divbyzero) {
        env->fpsr |= 0x0010; 
        env->fpsr |= 0x0400;
    }
    if (eflags & float_flag_overflow) {
        env->fpsr |= 0x0040; 
        env->fpsr |= 0x1000;
    }
    if (eflags & float_flag_underflow) {
        env->fpsr |= 0x0020; 
        env->fpsr |= 0x0800;
    }
    if (eflags & float_flag_inexact) {
        env->fpsr |= 0x0008; 
        env->fpsr |= 0x0200;
    }
    set_float_exception_flags(0, &env->fp_status);
}

void HELPER(fmovem)(CPUM68KState *env, uint32_t opsize, uint32_t mode, uint32_t mask)
{
    fprintf(stderr, "MISSING HELPER fmovem\n");
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

#if !defined(CONFIG_USER_ONLY)
void HELPER(ptest)(CPUM68KState * env, uint32_t addr, uint32_t is_write)
{
    hwaddr physical;
    int access_type;
    int prot;
    int ret;
    target_ulong page_size;

    access_type = ACCESS_PTEST;
    access_type |= (env->dfc & 4) ? ACCESS_SUPER : 0;
    if ((env->dfc & 3) == 2) {
        access_type |= ACCESS_CODE;
    }

    env->mmu.mmusr = 0;
    env->mmu.ssw = 0;
    ret = get_physical_address(env, &physical, &prot, addr,
                               access_type, &page_size);
    if (ret == 0) {
        tlb_set_page(env, addr & TARGET_PAGE_MASK,
                     physical & TARGET_PAGE_MASK,
                     prot, access_type & ACCESS_SUPER ?
                     MMU_KERNEL_IDX : MMU_USER_IDX, page_size);
    }
}

void HELPER(pflush)(CPUM68KState * env, uint32_t addr, uint32_t opmode)
{
    switch (opmode) {
    case 0: /* Flush page entry if not global */
    case 1: /* Flush page entry */
        tlb_flush_page(env, addr);
        break;
    case 2: /* Flush all except global entries */
        tlb_flush(env, 0);
        break;
    case 3: /* Flush all entries */
        tlb_flush(env, 1);
        break;
    }
}

void HELPER(reset)(CPUM68KState * env)
{
    /* FIXME: reset all except CPU */
}
#endif
