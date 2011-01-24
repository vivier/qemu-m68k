	.include "trap.i"

	.text
	.globl _start
_start:
# Pi
	fmovecr.x #0x00, %fp0
	fmove.b   #3, %fp1
# 0.1415926535897932
	fsub.x      %fp1, %fp0
	exit 0
