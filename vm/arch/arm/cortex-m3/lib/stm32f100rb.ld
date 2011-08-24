MEMORY
{
	vectors (RX) : ORIGIN = 0x08000000, LENGTH = 0x100
	flash (RX)   : ORIGIN = 0x08000100, LENGTH = 0x20000 - 0x100
	ram (RWX)    : ORIGIN = 0x20000000, LENGTH = 0x2000
}

INCLUDE common.ld

SECTIONS
{
	__stack_end__ = 0x20000000 + 0x2000;
}