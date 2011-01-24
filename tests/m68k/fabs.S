	.include "trap.i"

	.text
	.globl _start
_start:
	fabs.b #-55, %fp0
	fmove.l #-1024, %fp1
	fabs.x %fp1
	exit 0
