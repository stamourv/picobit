OUTPUT_FORMAT(elf32-littlearm)
ENTRY(_startup)

SECTIONS {
	.vectors : {
		*(.vectors)
	} > vectors

	.text : {
		*(.text)
		
		. = ALIGN(4);
		
		*(.rodata)
		
		. = ALIGN(4);
		
		__text_end__ = .;
	} > flash

	.bss : {
		__stack_begin__ = .;

		. += 0x100;

		__stack_end__ = .;

		__bss_begin__ = .;

		*(.bss)
		*(COMMON)

		. = ALIGN(4);

		__bss_end__ = .;

		__picobit_heap = .;
	} > ram

	/DISCARD/ : {
		*(.comment)
		*(.ARM.attributes)
	}
}
