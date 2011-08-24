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

	. = ALIGN(4);

	.bss : {
		__bss_begin__ = .;

		*(.bss)

		. = ALIGN(4);

		__bss_end__ = .;

		__picobit_ram = .;
	} > ram

	/DISCARD/ : {
		*(.comment)
		*(.ARM.attributes)
	}
}
