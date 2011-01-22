	.include "trap.i"

	.data
pi:	.long 0x40000000
	.long 0xc90fdaa2
	.long 0x2168C235
ln2:	.long 0x3ffe0000
	.long 0xb17217f7
	.long 0xd1cf79ac

	.text
	.globl _start
_start:
	lea pi,%a0
	fmovem.x (%a0), %fp2-%fp3
	fmovem.x (%a0)+, %fp0-%fp1
	exit 0
