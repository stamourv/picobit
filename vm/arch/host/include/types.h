#ifndef ARCH_HOST_TYPES_H
#define ARCH_HOST_TYPES_H

#include <stdint.h>

/** Define Picobit basic types **/

typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;

/** Define Picobit complex types **/

typedef uint8 word;

typedef uint16 ram_addr;
typedef uint16 rom_addr;

/* Pointers are 13 bits */
typedef uint16 obj;

#endif