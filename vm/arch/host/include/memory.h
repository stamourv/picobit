#ifndef PICOBIT_ARCH_HOST_MEMORY_H
#define PICOBIT_ARCH_HOST_MEMORY_H

#define CODE_START 0x8000

extern uint8 ram_mem[];
#define ram_get(a) ram_mem[a]
#define ram_set(a,x) ram_mem[a] = (x)

#define ROM_BYTES 8192
extern uint8 rom_mem[];
#define rom_get(a) (rom_mem[a-CODE_START])

#endif