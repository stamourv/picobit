#ifndef PICOBIT_VM_H
#define PICOBIT_VM_H

#include <generated/autoconf.h>

#include <arch/types.h>

#include <heap.h>
#include <object.h>

/* root set */
obj arg1;
obj arg2;
obj arg3;
obj arg4;
obj cont;
obj env;

/* interpreter variables */
rom_addr pc;
uint8 glovars;
rom_addr entry;
uint8 bytecode;
uint8 bytecode_hi4;
uint8 bytecode_lo4;
uint16 a1;
uint16 a2;
uint16 a3;

#ifdef CONFIG_ERROR_HANDLING
#define ERROR(prim, msg) error (prim, msg)
#define TYPE_ERROR(prim, type) type_error (prim, type)
void error (char *prim, char *msg);
void type_error (char *prim, char *type);
#else
#define ERROR(prim, msg) halt_with_error()
#define TYPE_ERROR(prim, type) halt_with_error()
#endif

extern char* prim_name[];

#endif
