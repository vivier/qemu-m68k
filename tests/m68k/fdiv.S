	.include "trap.i"

	.text
	.globl _start
_start:
# Pi
	fmove.b #10, %fp0
	fmove.b #3, %fp1
	fdiv.x  %fp1, %fp0
	exit 0
