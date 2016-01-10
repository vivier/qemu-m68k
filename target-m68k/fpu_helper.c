/*
 * FPU helpers.
 *
 * Copyright (c) 2011-2015 Laurent Vivier
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

#include <math.h>

#if 0
#define DBG_FPUH(...) do { \
    fprintf(stderr, "0x%08x: ", env->pc); \
    fprintf(stderr, __VA_ARGS__); \
} while (0)
#define DBG_FPU(...) do { \
    fprintf(stderr, __VA_ARGS__); \
} while (0)
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
    return *(float32 *)&env->fp0h;
}

static inline void float32_to_FP0(CPUM68KState *env, float32 val)
{

    env->fp0h = *(uint32_t *)&val;
}

static inline float64 FP0_to_float64(CPUM68KState *env)
{
    return *(float64 *)&env->fp0l;
}

static inline void float64_to_FP0(CPUM68KState *env, float64 val)
{
    env->fp0l = *(uint64_t *)&val;
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
                res.high = floatx80_default_nan(NULL).high;
                res.low = 0;
        }
        if (isinf(val) < 0) {
                res.high |= 0x8000;
        }
        if (isnan(val)) {
                res.high = floatx80_default_nan(NULL).high;
                res.low = *(uint64_t *)((char *)&val + 4);
        }
        return *(floatx80 *)&val;
}

void HELPER(const_FP0)(CPUM68KState *env, uint32_t offset)
{
    env->fp0h = fpu_rom[offset].high;
    env->fp0l = fpu_rom[offset].low;
    DBG_FPUH("ROM[0x%02x] %"PRIxFPH" %"PRIxFPL" %.17Lg\n",
             offset, env->fp0h, env->fp0l,
             floatx80_to_ldouble(FP0_to_floatx80(env)));
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
    DBG_FPUH("extf64_FP0 0x%016"PRIx64", %g",
             val, *(double *)&val);
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
    DBG_FPU(" = %g\n", *(double *)&res);

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
             floatx80_to_ldouble(FP0_to_floatx80(env)),
             floatx80_to_ldouble(FP1_to_floatx80(env)));
    res = floatx80_mul(FP0_to_floatx80(env), FP1_to_floatx80(env),
                       &env->fp_status);
    DBG_FPU(" = %Lg\n", floatx80_to_ldouble(res));

    floatx80_to_FP0(env, res);
}

void HELPER(div_FP0_FP1)(CPUM68KState *env)
{
    floatx80 res;

    DBG_FPUH("div_FP0_FP1 %Lg %Lg",
             floatx80_to_ldouble(FP0_to_floatx80(env)),
             floatx80_to_ldouble(FP1_to_floatx80(env)));
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
            if (floatx80_lt_quiet(FP1_to_floatx80(env),
                                  res, &env->fp_status)) {
                res = floatx80_chs(res);
            }
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
}

void HELPER(fmovem)(CPUM68KState *env, uint32_t opsize,
                    uint32_t mode, uint32_t mask)
{
    fprintf(stderr, "MISSING HELPER fmovem\n");
}
