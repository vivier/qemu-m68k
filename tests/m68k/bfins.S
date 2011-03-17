	.include "trap.i"

	.data
.A:	.long 0
	.text
	.globl _start
_start:
	move.l #0,%d1
	move.l #1,%d0
	bfins %d0,%d1,4,4
	move.l #3,%d0
	bfins %d0,%d1,8,2
	move.l #0,%d0
	bfins %d0,%d1,8,16

	move.l #1,%d0
	lea .A,%a0
	bfins %d0,(%a0),4,4
	move.l #3,%d0
	bfins %d0,(%a0),8,2
	move.l #0,%d0
	bfins %d0,(%a0),8,16
	exit 0
