.cpu cortex-m3
.syntax unified
.thumb

.section .vectors, "xa", %progbits
.globl __vector_table
.type __vector_table, %function
__vector_table:
	.word	__stack_end__ /* initial SP value */
	.word	_startup      /* next instruction in Thumb mode, fetch from flash */

.text
.globl _startup
.type  _startup, %function
_startup:
	/* Clear BSS */
	eor	r0, r0
	ldr	r2, =__bss_begin__
	ldr	r3, =__bss_end__

0:
	str	r0, [r2], #4

	cmp	r3, r2
	bhi	0b

	/* Jump to C code */
	bl	main

	/* Halt */
0:	b	0b

.type __aeabi_unwind_cpp_pr0, %function
.globl __aeabi_unwind_cpp_pr0
__aeabi_unwind_cpp_pr0:
	bx	lr
