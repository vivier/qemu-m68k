/*
 * qemu-wrapper
 *
 * Copyright (c) 2011 Laurent Vivier
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
 *  along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

/*
 * HOWTO
 *
 * for instance, for m68k target.
 *
 * copy qemu-wrapper and qemu-m68 into the m68k filesystem:
 *
 *   cd m68k-linux-user
 *   sudo cp qemu-m68k qemu-wrapper /m68k/usr/bin/qemu-wrapper
 *
 * update binfmts:
 *
 * update-binfmts --install m68k /usr/bin/qemu-wrapper \
 *                 --magic \
 * \x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x04 \
 *                 --mask \
 * \xff\xff\xff\xff\xff\xff\xfe\xfe\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfb\xff\xff
 *
 * chroot the m68k filesystem:
 *
 * sudo QEMU_CPU=m68020 chroot /m68k
 *
 *                ******** IMPORTANT NOTE ********
 *
 * qemu-m68k and qemu-wrapper must be linked staticaly:
 *
 *   ./configure --target-list=m68k-linux-user --static
 *
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#include "config-target.h"

int main(int argc, char **argv, char **envp) {
	char *wrapper[argc + 7];
	int current = 0;
	char *cpu, *debug, *port;

	wrapper[current] = argv[0];
	current++;

	cpu = getenv("QEMU_CPU");
	if (cpu) {
		wrapper[current] = (char*)"-cpu";
		current++;
		wrapper[current] = cpu;
		current++;
	}

	debug = getenv("QEMU_DEBUG");
	if (debug) {
		wrapper[current] = (char*)"-d";
		current++;
		wrapper[current] = debug;
		current++;
	}
	unsetenv("QEMU_DEBUG");

	port = getenv("QEMU_GDB");
	if (port) {
		wrapper[current] = (char*)"-g";
		current++;
		wrapper[current] = port;
		current++;
	}
	unsetenv("QEMU_GDB");

	memcpy(&wrapper[current], &argv[1], sizeof(*argv) * (argc - 1));
	current += argc - 1;

	wrapper[current] = NULL;

	return execve("/usr/bin/qemu-" TARGET_ARCH, wrapper, envp);
}
