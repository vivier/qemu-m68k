/*
 *  scsi.cpp - SCSI Manager
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
 *    Inside Macintosh: Devices, chapter 3 "SCSI Manager"
 *    Technote DV 24: "Fear No SCSI"
 */

#include <stdio.h>
#include <string.h>

#include "scsi.h"

/* Error codes */
enum {
    scCommErr = 2,
    scArbNBErr,
    scBadParmsErr,
    scPhaseErr,
    scCompareErr,
    scMgrBusyErr,
    scSequenceErr,
    scBusTOErr,
    scComplPhaseErr
};

/* TIB opcodes */
enum {
    scInc = 1,
    scNoInc,
    scAdd,
    scMove,
    scLoop,
    scNop,
    scStop,
    scComp
};

/* Logical SCSI phases */
enum {
    PH_FREE,        /* Bus free */
    PH_ARBITRATED,    /* Bus arbitrated (after SCSIGet()) */
    PH_SELECTED,    /* Target selected (after SCSISelect()) */
    PH_TRANSFER        /* Command sent (after SCSICmd()) */
};

/* Global variables */
static int target_id;                   /* ID of active target */
static int phase;                       /* Logical SCSI phase */
static uint16_t fake_status;            /* Faked 5830 status word */
static bool reading;                    /* Flag: reading from device */

#define SG_TABLE_SIZE 1024
static int sg_index; /* Index of first unused entry in S/G table */
/* Scatter/gather table data pointer (host address space) */
static uint8_t *sg_ptr[SG_TABLE_SIZE];
/* Scatter/gather table data length */
static uint32_t sg_len[SG_TABLE_SIZE];
static uint32_t sg_total_length;       /* Total data length */


/*
 *  Execute TIB, constructing S/G table
 */

static int16_t exec_tib(uint32_t tib)
{
    for (;;) {

        /* Read next opcode and parameters */
        uint16_t cmd = lduw_phys(tib); tib += 2;
        uint32_t ptr = ldl_phys(tib); tib += 4;
        uint32_t len = ldl_phys(tib); tib += 4;

        switch (cmd) {
        case scInc:
            stl_phys(tib - 8, ptr + len);
        case scNoInc:
            if ((sg_index > 0) && (Mac2HostAddr(ptr) ==
                sg_ptr[sg_index-1] + sg_len[sg_index-1])) {
                sg_len[sg_index-1] += len; /* Merge to previous entry */
            } else {
                if (sg_index == SG_TABLE_SIZE) {
                    ErrorAlert(GetString(STR_SCSI_SG_FULL_ERR));
                    return -108;
                }
                sg_ptr[sg_index] = Mac2HostAddr(ptr); /* Create new entry */
                sg_len[sg_index] = len;
                sg_index++;
            }
            sg_total_length += len;
            break;

        case scAdd:
            stl_phys(ptr, ldl_phys(ptr) + len);
            break;

        case scMove:
            stl_phys(len, ldl_phys(ptr));
            break;

        case scLoop:
            stl_phys(tib - 4, len - 1);
            if (len - 1 > 0) {
                tib += (int32)ptr - 10;
            }
            break;

        case scNop:
            break;

        case scStop:
            return 0;

        case scComp:
            printf("WARNING: Unimplemented scComp opcode\n");
            return scCompareErr;

        default:
            printf("FATAL: Unrecognized TIB opcode %d\n", cmd);
            return scBadParmsErr;
        }
    }
}


/*
 *  Reset SCSI bus
 */

int16_t SCSIReset(void)
{
    phase = PH_FREE;
    fake_status = 0x0000;    /* Bus free */
    sg_index = 0;
    target_id = 8;
    return 0;
}


/*
 *  Arbitrate bus
 */

int16_t SCSIGet(void)
{
    if (phase != PH_FREE) {
        return scMgrBusyErr;
    }

    phase = PH_ARBITRATED;
    fake_status = 0x0040;    /* Bus busy */
    reading = false;
    sg_index = 0;            /* Flush S/G table */
    sg_total_length = 0;
    return 0;
}


/*
 *  Select SCSI device
 */

int16_t SCSISelect(int id)
{
    if (phase != PH_ARBITRATED) {
        return scSequenceErr;
    }

    /* ID valid? */
    if (id >= 0 && id <= 7) {
        target_id = id;

        /* Target present? */
        if (scsi_is_target_present(target_id)) {
            phase = PH_SELECTED;
            fake_status = 0x006a;  /* Target selected, command phase */
            return 0;
        }
    }

    /* Error */
    phase = PH_FREE;
    fake_status = 0x0000;        /* Bus free */
    return scCommErr;
}


/*
 *  Send SCSI command
 */

int16_t SCSICmd(int cmd_length, uint8_t *cmd)
{
    if (phase != PH_SELECTED) {
        return scPhaseErr;
    }

    /* Commdn length valid? */
    if (cmd_length != 6 && cmd_length != 10 && cmd_length != 12) {
        return scBadParmsErr;
    }

    /* Set command, extract LUN */
    scsi_set_cmd(cmd_length, cmd);

    /* Extract LUN, set target */
    if (!scsi_set_target(target_id, (cmd[1] >> 5) & 7)) {
        phase = PH_FREE;
        fake_status = 0x0000;    /* Bus free */
        return scCommErr;
    }

    phase = PH_TRANSFER;
    fake_status = 0x006e;        /* Target selected, data phase */
    return 0;
}


/*
 *  Read data
 */

int16_t SCSIRead(uint32_t tib)
{
    if (phase != PH_TRANSFER) {
        return scPhaseErr;
    }

    /* Execute TIB, fill S/G table */
    reading = true;
    return exec_tib(tib);
}


/*
 *  Write data
 */

int16_t SCSIWrite(uint32_t tib)
{
    if (phase != PH_TRANSFER) {
        return scPhaseErr;
    }

    /* Execute TIB, fill S/G table */
    return exec_tib(tib);
}


/*
 *  Wait for command completion (we're actually doing everything in here...)
 */

int16_t SCSIComplete(uint32_t timeout, uint32_t message, uint32_t stat)
{
    stw_phys(message, 0);
    if (phase != PH_TRANSFER) {
        return scPhaseErr;
    }

    /* Send command, process S/G table */
    uint16_t scsi_stat = 0;
    bool success = scsi_send_cmd(sg_total_length, reading, sg_index, sg_ptr,
                                 sg_len, &scsi_stat, timeout);
    stw_phys(stat, scsi_stat);

    /* Complete command */
    phase = PH_FREE;
    fake_status = 0x0000;    /* Bus free */
    return success ? 0 : scCommErr;
}


/*
 *  Get bus status
 */

uint16_t SCSIStat(void)
{
    return fake_status;
}


/*
 *  SCSI Manager busy?
 */

int16_t SCSIMgrBusy(void)
{
    return phase != PH_FREE;
}
