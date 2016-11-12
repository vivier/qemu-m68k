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
    int float_compare;

    float_compare = floatx80_compare(fp1, fp0, &env->fp_status);
    env->fpsr = (env->fpsr & ~FPSR_CC_MASK) | float_comp_to_cc(float_compare);
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
}
