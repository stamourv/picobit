#ifndef PICOBIT_ARCH_ARM_CORTEX_M3_MEMORY_H
#define PICOBIT_ARCH_ARM_CORTEX_M3_MEMORY_H

#define CODE_START 0x8000

extern uint8 *__picobit_ram, *__picobit_rom;

#define ram_get(a) __picobit_ram[a]
#define ram_set(a,x) __picobit_ram[a] = (x)

#define rom_get(a) __picobit_rom[a]

#endif