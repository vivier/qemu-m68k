	.include "trap.i"

	.text
	.globl _start
_start:
	fmove.l #4096, %fp0
	fgetexp.x %fp0, %fp1
	exit 0
