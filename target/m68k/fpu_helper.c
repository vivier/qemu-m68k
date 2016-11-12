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

void HELPER(itrunc_FP0)(CPUM68KState *env)
{
    floatx80 res;

    res = floatx80_round_to_int(FP0_to_floatx80(env), &env->fp_status);

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

void HELPER(cmp_FP0_FP1)(CPUM68KState *env)
{
    floatx80 fp0 = FP0_to_floatx80(env);
    floatx80 fp1 = FP1_to_floatx80(env);
    floatx80 res;

    res = floatx80_sub(fp0, fp1, &env->fp_status);
    if (floatx80_is_quiet_nan(res, &env->fp_status)) {
        /* +/-inf compares equal against itself, but sub returns nan.  */
        if (!floatx80_is_quiet_nan(fp0, &env->fp_status)
            && !floatx80_is_quiet_nan(fp1, &env->fp_status)) {
            res = floatx80_zero;
            if (floatx80_lt_quiet(fp0, res, &env->fp_status)) {
                res = floatx80_chs(res);
            }
        }
    }

    floatx80_to_FP0(env, res);
}

uint32_t HELPER(compare_FP0)(CPUM68KState *env)
{
    floatx80 fp0 = FP0_to_floatx80(env);
    return floatx80_compare_quiet(fp0, floatx80_zero, &env->fp_status);
}
