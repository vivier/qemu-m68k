	.include "trap.i"

	.text
	.globl _start
_start:
	fmovecr.x #0x00,%fp0
	fmovecr.x #0x0B,%fp0
	fmovecr.x #0x0C,%fp0
	fmovecr.x #0x0D,%fp0
	fmovecr.x #0x0E,%fp0
	fmovecr.x #0x0F,%fp0
	fmovecr.x #0x30,%fp0
	fmovecr.x #0x31,%fp0
	fmovecr.x #0x32,%fp0
	fmovecr.x #0x33,%fp0
	fmovecr.x #0x34,%fp0
	fmovecr.x #0x35,%fp0
	fmovecr.x #0x36,%fp0
	fmovecr.x #0x37,%fp0
	fmovecr.x #0x38,%fp0
	fmovecr.x #0x39,%fp0
	fmovecr.x #0x3A,%fp0
	fmovecr.x #0x3B,%fp0
	fmovecr.x #0x3c,%fp0
	fmovecr.x #0x3d,%fp0
	fmovecr.x #0x3e,%fp0
	fmovecr.x #0x3f,%fp0

	exit 0
