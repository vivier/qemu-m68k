	.include "trap.i"

	.text
	.globl _start
_start:
	fmove.l #2, %fp0
	fmovecr.x #0x00,%fp1
	fscale.x %fp0, %fp1
	exit 0
