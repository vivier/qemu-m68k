/*
 * QEMU monitor
 *
 * Copyright (c) 2003-2004 Fabrice Bellard
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
#include "qemu/osdep.h"
#include "cpu.h"
#include "monitor/monitor.h"
#include "monitor/hmp-target.h"
#include "hmp.h"

const MonitorDef monitor_defs[] = {
    { "d0", offsetof(CPUM68KState, dregs[0]) },
    { "d1", offsetof(CPUM68KState, dregs[1]) },
    { "d2", offsetof(CPUM68KState, dregs[2]) },
    { "d3", offsetof(CPUM68KState, dregs[3]) },
    { "d4", offsetof(CPUM68KState, dregs[4]) },
    { "d5", offsetof(CPUM68KState, dregs[5]) },
    { "d6", offsetof(CPUM68KState, dregs[6]) },
    { "d7", offsetof(CPUM68KState, dregs[7]) },
    { "a0", offsetof(CPUM68KState, aregs[0]) },
    { "a1", offsetof(CPUM68KState, aregs[1]) },
    { "a2", offsetof(CPUM68KState, aregs[2]) },
    { "a3", offsetof(CPUM68KState, aregs[3]) },
    { "a4", offsetof(CPUM68KState, aregs[4]) },
    { "a5", offsetof(CPUM68KState, aregs[5]) },
    { "a6", offsetof(CPUM68KState, aregs[6]) },
    { "a7", offsetof(CPUM68KState, aregs[7]) },
    { "pc", offsetof(CPUM68KState, pc) },
    { "sr", offsetof(CPUM68KState, sr) },
    { "ssp", offsetof(CPUM68KState, sp[0]) },
    { "usp", offsetof(CPUM68KState, sp[1]) },
    { "isp", offsetof(CPUM68KState, sp[2]) },
    { NULL },
};

const MonitorDef *target_monitor_defs(void)
{
    return monitor_defs;
}
