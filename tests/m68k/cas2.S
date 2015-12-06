	.include "trap.i"

	.data
dest1:	.long 0x5555eeee
dest2:	.long 0x6666ffff

	.text
	.globl _start
_start:
	move.l #0xaaaa1111, %d0
	move.l #0xbbbb2222, %d1
	move.l #0xcccc3333, %d2
	move.l #0xdddd4444, %d3
	lea dest1, %a0
	lea dest2, %a1

	/* %d0 != dest1 -> %d0 = dest1, %d1 = dest2 */

	cas2.w %d0:%d1,%d2:%d3,(%a0):(%a1)

        /* %d0 = 0xaaaa5555, %d1 = 0xbbbb6666 */

	move.l #0xaaaa1111, %d0

	/* %d0 != dest1 -> %d0 = dest1, %d1 = dest2 */

	cas2.w %d0:%d1,%d2:%d3,(%a0):(%a1)

        /* %d0 = 0xaaaa5555, %d1 = 0xbbbb6666 */

	move.l #0xbbbb2222, %d1

        /* %d0 == dest1, %d1 != dest2 -> %d0 = dest1, %d1 = dest2 */

	cas2.w %d0:%d1,%d2:%d3,(%a0):(%a1)

	/* %d0 = 0xaaaa5555, %d1 = 0xcccc6666 */

	/* %d0 == dest1, %d1 == dest2 -> dest1 = %d2, dest2 = %d3 */

	cas2.w %d0:%d1,%d2:%d3,(%a0):(%a1)
	exit 0
