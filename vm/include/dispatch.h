#ifndef PICOBIT_DISPATCH_H
#define PICOBIT_DISPATCH_H

#include <arch/types.h>
#include <heap.h>

#define FETCH_NEXT_BYTECODE() bytecode = rom_get (pc++)

#define PUSH_CONSTANT1     0x0
#define PUSH_CONSTANT2     0x1
#define PUSH_STACK1        0x2
#define PUSH_STACK2        0x3
#define PUSH_GLOBAL        0x4
#define SET_GLOBAL         0x5
#define CALL               0x6
#define JUMP               0x7
#define JUMP_TOPLEVEL_REL4 0x8
#define GOTO_IF_FALSE_REL4 0x9
#define PUSH_CONSTANT_LONG 0xa
#define LABEL_INSTR        0xb

#define PRIM1              0xc
#define PRIM2              0xd
#define PRIM3              0xe
#define PRIM4              0xf

void push_arg1 ();
obj pop ();
void pop_procedure ();
uint8 handle_arity_and_rest_param (uint8 na);
void build_env (uint8 na);
void save_cont ();
void interpreter ();

#endif
