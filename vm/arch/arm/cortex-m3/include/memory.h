#ifndef PICOBIT_ARCH_ARM_CORTEX_M3_MEMORY_H
#define PICOBIT_ARCH_ARM_CORTEX_M3_MEMORY_H

#define CODE_START 0x8000

extern uint8 __picobit_ram;

#define ram_get(a) ((uint8*)&__picobit_ram)[a]
#define ram_set(a,x) ((uint8*)&__picobit_ram)[a] = (x)

#define rom_get(a) (((uint8*) 0)[a])

#endif
