	.include "trap.i"

	.data
dest:	.word 0xdead

	.text
	.globl _start
_start:
	move.l #0x1111dead, %d0	/* compare operand */
	move.l #0x2222beef, %d1	/* update operand */
	cas.w %d0, %d1, dest	/* dest == compare -> dest = update */
	cas.w %d0, %d1, dest	/* dest != compare -> compare = destination */
	exit 0
