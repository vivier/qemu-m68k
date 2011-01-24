	.include "trap.i"

	.data
tmp:	.long 0x88776655
pi:	.long 0x40000000, 0xc90fdaa2, 0x2168C235
store:   .long 0, 0, 0

	.text
	.globl _start
_start:
	lea pi,%a0
	move.l (%a0), %d0
	fmove.x (%a0), %fp4
	fmove.b %fp4, store
	fmove.w %fp4, store
	fmove.l %fp4, store
	fmove.s %fp4, store
	fmove.d %fp4, store
	fmove.x %fp4, store
	# Dn

	move.l #-1, %d3
	fmove.b %d3, %fp0
	fmove.w %d3, %fp1
	fmove.l %d3, %fp2
	fmove.s %d3, %fp3

	move.l #1, %d1
	fmove.b %d1, %fp0
	fmove.w %d1, %fp1
	fmove.l %d1, %fp2
	fmove.s %d1, %fp3

	move.l #0x11223344, %d1
	fmove.b %d1, %fp1
	fmove.w %d1, %fp2
	fmove.l %d1, %fp3
	fmove.s %d1, %fp4

	# (A0)

	lea tmp,%a0
	fmove.b (%a0), %fp0
	fmove.w (%a0), %fp1
	fmove.l (%a0), %fp2
	fmove.l (%a0), %fp3
	lea pi,%a0
	fmove.x (%a0), %fp4

	# immediate values

	fmove.b #0xFF,%fp0
	fmove.w #0xFABC,%fp1
	fmove.l #0xFABCDEFA,%fp2
	fmove.s #0xDEADBEAF,%fp3
	fmove.d #0f4.29497E+09,%fp4
	fmove.x #0xFABCDEFADEADBEAF12345678,%fp5
	fmove.p #0xFABCDEFADEADBEAF12345678,%fp6

	exit 0
