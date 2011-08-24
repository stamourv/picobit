#ifndef PICOBIT_PICOBIT_H
#define PICOBIT_PICOBIT_H

#include <generated/autoconf.h>

#include <arch/types.h>

/* Picobit complex types */

typedef uint8 word;

typedef uint16 ram_addr;
typedef uint16 rom_addr;

typedef uint16 obj; /* Only 13 bits are used in pointers */

#if defined(__GNUC__)
#define NORETURN __attribute__((noreturn))
#else
#define NORETURN
#endif /* defined(__GNUC__) */

#include <heap.h>
#include <object.h>

/* GC roots set. */

obj arg1;
obj arg2;
obj arg3;
obj arg4;
obj cont;
obj env;

/* Interpreter variables. */

rom_addr pc, entry;
uint8 glovars;

#ifdef CONFIG_ERROR_HANDLING

#define ERROR(prim, msg) error (prim, msg)
#define TYPE_ERROR(prim, type) type_error (prim, type)
void error (char *prim, char *msg) NORETURN;
void type_error (char *prim, char *type) NORETURN;

#else

void halt_with_error () NORETURN;
#define ERROR(prim, msg) halt_with_error()
#define TYPE_ERROR(prim, type) halt_with_error()

#endif /* CONFIG_ERROR_HANDLING */

#ifdef CONFIG_DEBUG_STRINGS
extern char* prim_name[];
#endif /* CONFIG_DEBUG_STRINGS */

#endif
