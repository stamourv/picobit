#ifndef PICOBIT_DEBUG_H
#define PICOBIT_DEBUG_H

#ifdef CONFIG_DEBUG
#include <stdio.h>

#define IF_TRACE(x) x
#define IF_GC_TRACE(x) x
#else
#define IF_TRACE(x)
#define IF_GC_TRACE(x)
#endif

#ifdef CONFIG_DEBUG_STRINGS
void show_type (obj o);
void show_state (rom_addr pc);
#else
#define show_type(o)
#define show_state(pc)
#endif

#endif
