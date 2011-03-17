	.include "trap.i"

	.data
X:	.long 0x7ffe0000, 0x80000000, 0x00000000

	.text
	.globl _start
_start:
	fmove.x X, %fp0
	fmove.x X, %fp1
	fadd.x %fp0, %fp1
	fmul.l #2,%fp0
	fcmp.x %fp0, %fp1
	fbeq a
	nop
a:
	exit 0
