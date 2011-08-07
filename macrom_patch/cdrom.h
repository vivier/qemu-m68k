/*
 *  cdrom.h - CD-ROM driver
 *
 *  Copied from Basilisk II (C) 1997-2005 Christian Bauer
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

#ifndef CDROM_H
#define CDROM_H

#include <inttypes.h>

#define CDROMRefNum -62                   /* RefNum of driver */
#define CDROMDriverFlags 0x6d04           /* Driver flags */

extern const uint8_t CDROMIcon[258];      /* Icon data (copied to ROM by PatchROM()) */

extern uint32_t CDROMIconAddr;            /* Icon address (Mac address space, set by PatchROM()) */

extern void CDROMInit(void);
extern void CDROMExit(void);

extern void CDROMInterrupt(void);

extern int CDROMMountVolume(void *fh);

extern int16_t CDROMOpen(uint32_t pb, uint32_t dce);
extern int16_t CDROMPrime(uint32_t pb, uint32_t dce);
extern int16_t CDROMControl(uint32_t pb, uint32_t dce);
extern int16_t CDROMStatus(uint32_t pb, uint32_t dce);

#endif
