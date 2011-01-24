	.include "trap.i"

	.text
	.globl _start
_start:
	fmove.b #255, %fp0
	fmove.w #1023, %fp1
	fmul.x  %fp1, %fp0
	exit 0
