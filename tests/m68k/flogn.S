	.include "trap.i"

	.text
	.globl _start
_start:
	fmovecr.x #0x0C,%fp0	// e
	flogn.x %fp0,%fp0	// 1
	exit 0
