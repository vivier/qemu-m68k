/*
 *  timer.c - Time Manager emulation
 *
 *  Basilisk II (C) 1997-2005 Christian Bauer
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*
 *  SEE ALSO
 *    Inside Macintosh: Processes, chapter 3 "Time Manager"
 *    Technote 1063: "Inside Macintosh: Processes: Time Manager Addenda"
 */

#include <stdio.h>
#include <sys/time.h>

#include "cpu.h"
#include "timer.h"
#include "macos_util.h"


/* Set this to 1 to enable TMQueue management (doesn't work) */

#define TM_QUEUE 0


/* Definitions for Time Manager */

enum {    /* TMTask struct */
    tmAddr = 6,
    tmCount = 10,
    tmWakeUp = 14,
    tmReserved = 18
};


/* Array of additional info for each installed TMTask */

struct TMDesc {
    uint32_t task;         /* Mac address of associated TMTask */
    struct timeval wakeup; /* Time this task is scheduled for execution */
    int in_use;            /* Flag: descriptor in use */
};

#define NUM_DESCS 64        /* Maximum number of descriptors */
static struct TMDesc desc[NUM_DESCS];

static void timer_mac2host_time(struct timeval *res, int32_t mactime)
{
    if (mactime > 0) {
        /* Time in milliseconds */
        res->tv_sec = mactime / 1000;
        res->tv_usec = (mactime % 1000) * 1000;
    } else {
        /* Time in negative microseconds */
        res->tv_sec = -mactime / 1000000;
        res->tv_usec = -mactime % 1000000;
    }
}

static int32_t timer_host2mac_time(struct timeval hosttime)
{
    if (hosttime.tv_sec < 0) {
        return 0;
    } else {
        uint64_t t = (uint64_t)hosttime.tv_sec * 1000000 + hosttime.tv_usec;
        if (t > 0x7fffffff) {
            return t / 1000;
        } else {
            return -t;
        }
    }
}

/*
 *  Allocate descriptor for given TMTask in list
 */

static int alloc_desc(uint32_t tm)
{
    int i;

    /* Search for first free descriptor */
    for (i = 0; i < NUM_DESCS; i++) {
        if (!desc[i].in_use) {
            desc[i].task = tm;
            desc[i].in_use = true;
            return i;
        }
    }
    return -1;
}


/*
 *  Free descriptor in list
 */

static inline void free_desc(int i)
{
    desc[i].in_use = false;
}


/*
 *  Find descriptor associated with given TMTask
 */

static inline int find_desc(uint32_t tm)
{
    int i;

    for (i = 0; i < NUM_DESCS; i++) {
        if (desc[i].in_use && desc[i].task == tm) {
            return i;
        }
    }
    return -1;
}


/*
 *  Enqueue task in Time Manager queue
 */

static void enqueue_tm(uint32_t tm)
{
#if TM_QUEUE
    uint32_t tm_var = ldl_phys(0xb30);
    stl_phys(tm + qLink, ldl_phys(tm_var));
    stl_phys(tm_var, tm);
#endif
}


/*
 *  Remove task from Time Manager queue
 */

static void dequeue_tm(uint32_t tm)
{
#if TM_QUEUE
    uint32_t p = ldl_phys(0xb30);
    while (p) {
        uint32_t next = ldl_phys(p + qLink);
        if (next == tm) {
            stl_phys(p + qLink, ldl_phys(next + qLink));
            return;
        }
    }
#endif
}


/*
 *  Initialize Time Manager
 */

void TimerInit(void)
{
    int i;

    /* Mark all descriptors as inactive */
    for (i = 0; i < NUM_DESCS; i++) {
        free_desc(i);
    }
}


/*
 *  Exit Time Manager
 */

void TimerExit(void)
{
}


/*
 *  Emulator reset, remove all timer tasks
 */

void TimerReset(void)
{
    int i;

    /* Mark all descriptors as inactive */
    for (i = 0; i < NUM_DESCS; i++) {
        free_desc(i);
    }
}


/*
 *  Insert timer task
 */

int16_t InsTime(uint32_t tm, uint16_t trap)
{
    stw_phys(tm + qType, (lduw_phys(tm + qType) & 0x1fff) |
                         ((trap << 4) & 0x6000));
    if (find_desc(tm) >= 0)
        printf("WARNING: InsTime(): Task re-inserted\n");
    else {
        int i = alloc_desc(tm);
        if (i < 0) {
            printf("FATAL: InsTime(): No free Time Manager descriptor\n");
        }
    }
    return 0;
}


/*
 *  Remove timer task
 */

int16_t RmvTime(uint32_t tm)
{
    /* Find descriptor */
    int i = find_desc(tm);
    if (i < 0) {
        printf("WARNING: RmvTime(%08x): Descriptor not found\n", tm);
        return 0;
    }

    /* Task active? */
    if (lduw_phys(tm + qType) & 0x8000) {

        /* Yes, make task inactive and remove it from the Time Manager queue */
        stw_phys(tm + qType, lduw_phys(tm + qType) & 0x7fff);
        dequeue_tm(tm);

        /* Compute remaining time */
        struct timeval remaining, current;
        gettimeofday(&current, NULL);
        timersub(&desc[i].wakeup, &current, &remaining);
        stl_phys(tm + tmCount, timer_host2mac_time(remaining));
    } else
        stl_phys(tm + tmCount, 0);

    /* Free descriptor */
    free_desc(i);
    return 0;
}


/*
 *  Start timer task
 */

int16_t PrimeTime(uint32_t tm, int32_t time)
{
    /* Find descriptor */
    int i = find_desc(tm);
    if (i < 0) {
        printf("FATAL: PrimeTime(): Descriptor not found\n");
        return 0;
    }

    /* Extended task? */
    if (lduw_phys(tm + qType) & 0x4000) {

        /* Convert delay time */
        struct timeval delay;
        timer_mac2host_time(&delay, time);

        /* Yes, tmWakeUp set? */
        if (ldl_phys(tm + tmWakeUp)) {

            /*!! PrimeTime(0) means continue previous delay */
            /* (save wakeup time in RmvTime?) */
            if (time == 0) {
                printf("WARNING: Unsupported PrimeTime(0)\n");
            }

            /* Yes, calculate wakeup time relative to last scheduled time */
            struct timeval wakeup;
            timeradd(&desc[i].wakeup, &delay, &wakeup);
            desc[i].wakeup = wakeup;

        } else {

            /* No, calculate wakeup time relative to current time */
            struct timeval now;
            gettimeofday(&now, NULL);
            timeradd(&now, &delay, &desc[i].wakeup);
        }

        /* Set tmWakeUp to indicate that task was scheduled */
        stl_phys(tm + tmWakeUp, 0x12345678);

    } else {

        /* Not extended task, calculate wakeup time relative to current time */
        struct timeval delay;
        timer_mac2host_time(&delay, time);
        gettimeofday(&desc[i].wakeup, NULL);
        timeradd(&desc[i].wakeup, &delay, &desc[i].wakeup);
    }

    /* Make task active and enqueue it in the Time Manager queue */
    stw_phys(tm + qType, lduw_phys(tm + qType) | 0x8000);
    enqueue_tm(tm);
    return 0;
}

#if 0
/*
 *  Timer interrupt function (executed as part of 60Hz interrupt)
 */

void TimerInterrupt(void)
{
    /* Look for active TMTasks that have expired */
    int i;
    struct timeval now;
    gettimeofday(&now, NULL);
    for (i = 0; i < NUM_DESCS; i++) {
        if (desc[i].in_use) {
            uint32_t tm = desc[i].task;
            if ((lduw_phys(tm + qType) & 0x8000) &&
                timer_cmp_time(desc[i].wakeup, now) < 0) {

                /* Found one, mark as inactive and remove it from the Time
                 * Manager queue
                 */
                stw_phys(tm + qType, lduw_phys(tm + qType) & 0x7fff);
                dequeue_tm(tm);

                /* Call timer function */
                uint32_t addr = ldl_phys(tm + tmAddr);
                if (addr) {
                    M68kRegisters r;
                    r.a[0] = addr;
                    r.a[1] = tm;
                    Execute68k(addr, &r);
                }
            }
        }
    }
}
#endif
