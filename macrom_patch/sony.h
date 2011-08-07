/*
 *  sony.h - Replacement .Sony driver (floppy drives)
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

#ifndef SONY_H
#define SONY_H

#include <inttypes.h>

#define SonyRefNum -5                        /* RefNum of driver */
#define SonyDriverFlags 0x6f00               /* Driver flags */

extern const uint8_t SonyDiskIcon[258];      /* Icon data (copied to ROM by PatchROM()) */
extern const uint8_t SonyDriveIcon[258];

extern uint32_t SonyDiskIconAddr;            /* Icon addresses (Mac address space, set by PatchROM()) */
extern uint32_t SonyDriveIconAddr;

extern void SonyInit(void);
extern void SonyExit(void);

extern void SonyInterrupt(void);

extern int SonyMountVolume(void *fh);

extern int16_t SonyOpen(uint32_t pb, uint32_t dce);
extern int16_t SonyPrime(uint32_t pb, uint32_t dce);
extern int16_t SonyControl(uint32_t pb, uint32_t dce);
extern int16_t SonyStatus(uint32_t pb, uint32_t dce);

#endif
