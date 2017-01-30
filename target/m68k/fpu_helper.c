/*
 *  m68k FPU helpers
 *
 *  Copyright (c) 2006-2007 CodeSourcery
 *  Written by Paul Brook
 *  Copyright (c) 2011-2016 Laurent Vivier
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
#include "exec/helper-proto.h"
#include "exec/exec-all.h"

static const floatx80 fpu_rom[128] = {
    [0x00] = floatx80_pi,                                   /* Pi */
    [0x0b] = make_floatx80(0x3ffd, 0x9a209a84fbcff798ULL),  /* Log10(2) */
    [0x0c] = make_floatx80(0x4000, 0xadf85458a2bb4a9aULL),  /* e        */
    [0x0d] = make_floatx80(0x3fff, 0xb8aa3b295c17f0bcULL),  /* Log2(e)  */
    [0x0e] = make_floatx80(0x3ffd, 0xde5bd8a937287195ULL),  /* Log10(e) */
    [0x0f] = floatx80_zero,                                 /* Zero     */
    [0x30] = floatx80_ln2,                                  /* ln(2)    */
    [0x31] = make_floatx80(0x4000, 0x935d8dddaaa8ac17ULL),  /* ln(10)   */
    [0x32] = floatx80_one,                                  /* 10^0     */
    [0x33] = make_floatx80(0x4002, 0xa000000000000000ULL),  /* 10^1     */
    [0x34] = make_floatx80(0x4005, 0xc800000000000000ULL),  /* 10^2     */
    [0x35] = make_floatx80(0x400c, 0x9c40000000000000ULL),  /* 10^4     */
    [0x36] = make_floatx80(0x4019, 0xbebc200000000000ULL),  /* 10^8     */
    [0x37] = make_floatx80(0x4034, 0x8e1bc9bf04000000ULL),  /* 10^16    */
    [0x38] = make_floatx80(0x4069, 0x9dc5ada82b70b59eULL),  /* 10^32    */
    [0x39] = make_floatx80(0x40d3, 0xc2781f49ffcfa6d5ULL),  /* 10^64    */
    [0x3a] = make_floatx80(0x41a8, 0x93ba47c980e98ce0ULL),  /* 10^128   */
    [0x3b] = make_floatx80(0x4351, 0xaa7eebfb9df9de8eULL),  /* 10^256   */
    [0x3c] = make_floatx80(0x46a3, 0xe319a0aea60e91c7ULL),  /* 10^512   */
    [0x3d] = make_floatx80(0x4d48, 0xc976758681750c17ULL),  /* 10^1024  */
    [0x3e] = make_floatx80(0x5a92, 0x9e8b3b5dc53d5de5ULL),  /* 10^2048  */
    [0x3f] = make_floatx80(0x7525, 0xc46052028a20979bULL),  /* 10^4096  */
};

static floatx80 FP0_to_floatx80(CPUM68KState *env)
{
    return (floatx80){ .low = env->fp0l, .high = env->fp0h };
}

static void floatx80_to_FP0(CPUM68KState *env, floatx80 res)
{
    env->fp0l = res.low;
    env->fp0h = res.high;
}

static int32_t FP0_to_int32(CPUM68KState *env)
{
    return env->fp0h;
}

static void int32_to_FP0(CPUM68KState *env, int32_t val)
{
    env->fp0h = val;
}

static float32 FP0_to_float32(CPUM68KState *env)
{
    return *(float32 *)&env->fp0h;
}

static void float32_to_FP0(CPUM68KState *env, float32 val)
{
    env->fp0h = *(uint32_t *)&val;
}

static float64 FP0_to_float64(CPUM68KState *env)
{
    return *(float64 *)&env->fp0l;
}
static void float64_to_FP0(CPUM68KState *env, float64 val)
{
    env->fp0l = *(uint64_t *)&val;
}

static floatx80 FP1_to_floatx80(CPUM68KState *env)
{
    return (floatx80){ .low = env->fp1l, .high = env->fp1h };
}

void HELPER(exts32_FP0)(CPUM68KState *env)
{
    floatx80 res;

    res = int32_to_floatx80(FP0_to_int32(env), &env->fp_status);

    floatx80_to_FP0(env, res);
}

void HELPER(extf32_FP0)(CPUM68KState *env)
{
    floatx80 res;

    res = float32_to_floatx80(FP0_to_float32(env), &env->fp_status);

    floatx80_to_FP0(env, res);
}

void HELPER(extf64_FP0)(CPUM68KState *env)
{
    floatx80 res;

    res = float64_to_floatx80(FP0_to_float64(env), &env->fp_status);

    floatx80_to_FP0(env, res);
}

void HELPER(reds32_FP0)(CPUM68KState *env)
{
    int32_t res;

    res = floatx80_to_int32(FP0_to_floatx80(env), &env->fp_status);

    int32_to_FP0(env, res);
}

void HELPER(redf32_FP0)(CPUM68KState *env)
{
    float32 res;

    res = floatx80_to_float32(FP0_to_floatx80(env), &env->fp_status);

    float32_to_FP0(env, res);
}

void HELPER(redf64_FP0)(CPUM68KState *env)
{
    float64 res;

    res = floatx80_to_float64(FP0_to_floatx80(env), &env->fp_status);

    float64_to_FP0(env, res);
}

void HELPER(iround_FP0)(CPUM68KState *env)
{
    floatx80 res;

    res = floatx80_round_to_int(FP0_to_floatx80(env), &env->fp_status);

    floatx80_to_FP0(env, res);
}

static void m68k_restore_precision_mode(CPUM68KState *env)
{
    switch (env->fpcr & FPCR_PREC_MASK) {
    case FPCR_PREC_X: /* extended */
        set_floatx80_rounding_precision(80, &env->fp_status);
        break;
    case FPCR_PREC_S: /* single */
        set_floatx80_rounding_precision(32, &env->fp_status);
        break;
    case FPCR_PREC_D: /* double */
        set_floatx80_rounding_precision(64, &env->fp_status);
        break;
    case FPCR_PREC_U: /* undefined */
    default:
        break;
    }
}

static void cf_restore_precision_mode(CPUM68KState *env)
{
    if (env->fpcr & FPCR_PREC_S) { /* single */
        set_floatx80_rounding_precision(32, &env->fp_status);
    } else { /* double */
        set_floatx80_rounding_precision(64, &env->fp_status);
    }
}

static void restore_rounding_mode(CPUM68KState *env)
{
    switch (env->fpcr & FPCR_RND_MASK) {
    case FPCR_RND_N: /* round to nearest */
        set_float_rounding_mode(float_round_nearest_even, &env->fp_status);
        break;
    case FPCR_RND_Z: /* round to zero */
        set_float_rounding_mode(float_round_to_zero, &env->fp_status);
        break;
    case FPCR_RND_M: /* round toward minus infinity */
        set_float_rounding_mode(float_round_down, &env->fp_status);
        break;
    case FPCR_RND_P: /* round toward positive infinity */
        set_float_rounding_mode(float_round_up, &env->fp_status);
        break;
    }
}

static void set_fpsr_exception(CPUM68KState *env)
{
    uint32_t fpsr = 0;
    int flags;

    flags = get_float_exception_flags(&env->fp_status);
    if (flags == 0) {
        return;
    }
    set_float_exception_flags(0, &env->fp_status);

    if (flags & float_flag_invalid) {
        fpsr |= FPSR_AE_IOP;
    }
    if (flags & float_flag_divbyzero) {
        fpsr |= FPSR_AE_DZ;
    }
    if (flags & float_flag_overflow) {
        fpsr |= FPSR_AE_OVFL;
    }
    if (flags & float_flag_underflow) {
        fpsr |= FPSR_AE_UNFL;
    }
    if (flags & float_flag_inexact) {
        fpsr |= FPSR_AE_INEX;
    }

    env->fpsr = (env->fpsr & ~FPSR_AE_MASK) | fpsr;
}

static void fpu_exception(CPUM68KState *env, uint32_t exception)
{
    CPUState *cs = CPU(m68k_env_get_cpu(env));

    env->fpsr = (env->fpsr & ~FPSR_ES_MASK) | exception;
    if (env->fpcr & exception) {
        switch (exception) {
        case FPSR_ES_BSUN:
            cs->exception_index = EXCP_FP_BSUN;
            break;
        case FPSR_ES_SNAN:
            cs->exception_index = EXCP_FP_SNAN;
            break;
        case FPSR_ES_OPERR:
            cs->exception_index = EXCP_FP_OPERR;
            break;
        case FPSR_ES_OVFL:
            cs->exception_index = EXCP_FP_OVFL;
            break;
        case FPSR_ES_UNFL:
            cs->exception_index = EXCP_FP_UNFL;
            break;
        case FPSR_ES_DZ:
            cs->exception_index = EXCP_FP_DZ;
            break;
        case FPSR_ES_INEX:
        case FPSR_ES_INEX2:
            cs->exception_index = EXCP_FP_INEX;
            break;
        }
        cpu_loop_exit_restore(cs, GETPC());
    }
}

void cpu_m68k_set_fpcr(CPUM68KState *env, uint32_t val)
{
    env->fpcr = val & 0xffff;

    if (m68k_feature(env, M68K_FEATURE_CF_FPU)) {
        cf_restore_precision_mode(env);
    } else {
        m68k_restore_precision_mode(env);
    }
    restore_rounding_mode(env);
}

void HELPER(set_fpcr)(CPUM68KState *env, uint32_t val)
{
    cpu_m68k_set_fpcr(env, val);
}

void HELPER(itrunc_FP0)(CPUM68KState *env)
{
    floatx80 res;

    set_float_rounding_mode(float_round_to_zero, &env->fp_status);
    res = floatx80_round_to_int(FP0_to_floatx80(env), &env->fp_status);
    restore_rounding_mode(env);

    floatx80_to_FP0(env, res);
}

void HELPER(sqrt_FP0)(CPUM68KState *env)
{
    floatx80 res;

    res = floatx80_sqrt(FP0_to_floatx80(env), &env->fp_status);

    floatx80_to_FP0(env, res);
}

void HELPER(abs_FP0)(CPUM68KState *env)
{
    floatx80 res;

    res = floatx80_abs(FP0_to_floatx80(env));

    floatx80_to_FP0(env, res);
}

void HELPER(chs_FP0)(CPUM68KState *env)
{
    floatx80 res;

    res = floatx80_chs(FP0_to_floatx80(env));

    floatx80_to_FP0(env, res);
}

void HELPER(add_FP0_FP1)(CPUM68KState *env)
{
    floatx80 res;

    res = floatx80_add(FP0_to_floatx80(env), FP1_to_floatx80(env),
                      &env->fp_status);

    floatx80_to_FP0(env, res);
}

void HELPER(sub_FP0_FP1)(CPUM68KState *env)
{
    floatx80 res;

    res = floatx80_sub(FP1_to_floatx80(env), FP0_to_floatx80(env),
                       &env->fp_status);

    floatx80_to_FP0(env, res);
}

void HELPER(mul_FP0_FP1)(CPUM68KState *env)
{
    floatx80 res;

    res = floatx80_mul(FP0_to_floatx80(env), FP1_to_floatx80(env),
                       &env->fp_status);

    floatx80_to_FP0(env, res);
}

void HELPER(div_FP0_FP1)(CPUM68KState *env)
{
    floatx80 res;

    res = floatx80_div(FP1_to_floatx80(env), FP0_to_floatx80(env),
                       &env->fp_status);

    floatx80_to_FP0(env, res);
}

static int float_comp_to_cc(int float_compare)
{
    switch (float_compare) {
    case float_relation_equal:
        return FPSR_CC_Z;
    case float_relation_less:
        return FPSR_CC_N;
    case float_relation_unordered:
        return FPSR_CC_A;
    case float_relation_greater:
        return 0;
    default:
        g_assert_not_reached();
    }
}

void HELPER(cmp_FP0_FP1)(CPUM68KState *env)
{
    floatx80 fp0 = FP0_to_floatx80(env);
    floatx80 fp1 = FP1_to_floatx80(env);
    int flags, float_compare;

    float_compare = floatx80_compare(fp1, fp0, &env->fp_status);
    env->fpsr = (env->fpsr & ~FPSR_CC_MASK) | float_comp_to_cc(float_compare);

    flags = get_float_exception_flags(&env->fp_status);
    if (flags & float_flag_invalid) {
        fpu_exception(env, FPSR_ES_OPERR);
   }
   set_fpsr_exception(env);
}

void HELPER(tst_FP0)(CPUM68KState *env)
{
    uint32_t fpsr = 0;
    floatx80 val = FP0_to_floatx80(env);

    if (floatx80_is_neg(val)) {
        fpsr |= FPSR_CC_N;
    }

    if (floatx80_is_any_nan(val)) {
        fpsr |= FPSR_CC_A;
    } else if (floatx80_is_infinity(val)) {
        fpsr |= FPSR_CC_I;
    } else if (floatx80_is_zero(val)) {
        fpsr |= FPSR_CC_Z;
    }
    env->fpsr = (env->fpsr & ~FPSR_CC_MASK) | fpsr;

    set_fpsr_exception(env);
}

void HELPER(update_fpstatus)(CPUM68KState *env)
{
    int flags = get_float_exception_flags(&env->fp_status);

    if (env->fpsr & FPSR_AE_IOP) {
        flags |= float_flag_invalid;
    } else {
        flags &= ~float_flag_invalid;
    }
    if (env->fpsr & FPSR_AE_DZ) {
        flags |= float_flag_divbyzero;
    } else {
        flags &= ~float_flag_divbyzero;
    }
    if (env->fpsr & FPSR_AE_OVFL) {
        flags |= float_flag_overflow;
    } else {
        flags &= ~float_flag_overflow;
    }
    if (env->fpsr & FPSR_AE_UNFL) {
        flags |= float_flag_underflow;
    } else {
        flags &= ~float_flag_underflow;
    }
    if (env->fpsr & FPSR_AE_INEX) {
        flags |= float_flag_inexact;
    } else {
        flags &= ~float_flag_inexact;
    }

    set_float_exception_flags(flags, &env->fp_status);
}

void HELPER(fmovem)(CPUM68KState *env, uint32_t opsize,
                    uint32_t mode, uint32_t mask)
{
    fprintf(stderr, "MISSING HELPER fmovem\n");
}

void HELPER(const_FP0)(CPUM68KState *env, uint32_t offset)
{
    env->fp0l = fpu_rom[offset].low;
    env->fp0h = fpu_rom[offset].high;
}

void HELPER(getexp_FP0)(CPUM68KState *env)
{
    int32_t exp;
    floatx80 res;

    res = FP0_to_floatx80(env);
    if (floatx80_is_zero_or_denormal(res) || floatx80_is_any_nan(res) ||
        floatx80_is_infinity(res)) {
        return;
    }
    exp = (env->fp0h & 0x7fff) - 0x3fff;

    res = int32_to_floatx80(exp, &env->fp_status);

    floatx80_to_FP0(env, res);
}

void HELPER(getman_FP0)(CPUM68KState *env)
{
    floatx80 res;
    res = int64_to_floatx80(env->fp0l, &env->fp_status);
    floatx80_to_FP0(env, res);
}

void HELPER(scale_FP0_FP1)(CPUM68KState *env)
{
    int32_t scale;
    int32_t exp;

    scale = floatx80_to_int32(FP0_to_floatx80(env), &env->fp_status);

    exp = (env->fp1h & 0x7fff) + scale;

    env->fp0h = (env->fp1h & 0x8000) | (exp & 0x7fff);
    env->fp0l = env->fp1l;
}

static void make_quotient(CPUM68KState *env, floatx80 val)
{
    uint32_t quotient = floatx80_to_int32(val, &env->fp_status);
    uint32_t sign = (quotient >> 24) & 0x80;
    quotient = sign | (quotient & 0x7f);
    env->fpsr = (env->fpsr & ~FPSR_QT_MASK) | (quotient << FPSR_QT_SHIFT);
}

void HELPER(mod_FP0_FP1)(CPUM68KState *env)
{
    floatx80 res;

    res = floatx80_rem(FP1_to_floatx80(env), FP0_to_floatx80(env),
                       &env->fp_status);
    make_quotient(env, res);

    floatx80_to_FP0(env, res);
}
