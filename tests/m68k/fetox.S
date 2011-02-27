	.include "trap.i"

	.text
	.globl _start
_start:
	fmove.l #1,%fp0		// 1
	fetox.x %fp0,%fp0	// e
	exit 0
