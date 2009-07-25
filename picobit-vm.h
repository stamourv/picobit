/* file: "picobit-vm.h" */

/*
 * Copyright 2004-2009 by Marc Feeley and Vincent St-Amour, All Rights Reserved.
 */

#ifndef PICOBIT_VM_H
#define PICOBIT_VM_H

#define DEBUG_not
#define DEBUG_GC_not
#define INFINITE_PRECISION_BIGNUMS

/*---------------------------------------------------------------------------*/

// types

#ifndef SIXPIC
// these types are already defined in SIXPIC
typedef char int8;
typedef short int16;
typedef long int32;
typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned long uint32;
#endif

typedef uint8 word;

typedef uint16 ram_addr;
typedef uint16 rom_addr;

// pointers are 13 bits
typedef uint16 obj;

/*---------------------------------------------------------------------------*/

// environment

#ifdef PICOBOARD2
#define ROBOT
#endif

#ifdef HI_TECH_C
#define ROBOT
#endif

#ifndef ROBOT
#define WORKSTATION
#endif


#ifdef HI_TECH_C

#include <pic18.h>

static volatile near uint8 FW_VALUE_UP       @ 0x33;
static volatile near uint8 FW_VALUE_HI       @ 0x33;
static volatile near uint8 FW_VALUE_LO       @ 0x33;

#define ACTIVITY_LED1_LAT LATB
#define ACTIVITY_LED1_BIT 5
#define ACTIVITY_LED2_LAT LATB
#define ACTIVITY_LED2_BIT 4
static volatile near bit ACTIVITY_LED1 @ ((unsigned)&ACTIVITY_LED1_LAT*8)+ACTIVITY_LED1_BIT;
static volatile near bit ACTIVITY_LED2 @ ((unsigned)&ACTIVITY_LED2_LAT*8)+ACTIVITY_LED2_BIT;

#endif


#ifdef WORKSTATION

#include <stdio.h>
#include <stdlib.h>

#ifdef NETWORKING
#include <pcap.h>
#define MAX_PACKET_SIZE BUFSIZ
#define PROMISC 1
#define TO_MSEC 1
char errbuf[PCAP_ERRBUF_SIZE];
pcap_t *handle;
#define INTERFACE "eth0"
char buf [MAX_PACKET_SIZE]; // buffer for writing
#endif

#ifdef _WIN32

#include <sys/types.h>
#include <sys/timeb.h>
#include <conio.h>

#else

#include <sys/time.h>

#endif

#endif

/*---------------------------------------------------------------------------*/

// miscellaneous definitions
// TODO put at the end ?

// TODO these 2 are only used in negp, use them elsewhere ?
#define true  1
#define false 0

#define CODE_START 0x8000

/*---------------------------------------------------------------------------*/

// debugging

#ifdef DEBUG
#define IF_TRACE(x) x
#define IF_GC_TRACE(x) x
#else
#define IF_TRACE(x)
#define IF_GC_TRACE(x)
#endif

/*---------------------------------------------------------------------------*/

// error handling

#ifdef PICOBOARD2
#define ERROR(prim, msg) halt_with_error()
#define TYPE_ERROR(prim, type) halt_with_error()
#endif

#ifdef WORKSTATION
#define ERROR(prim, msg) error (prim, msg)
#define TYPE_ERROR(prim, type) type_error (prim, type)
void error (char *prim, char *msg);
void type_error (char *prim, char *type);
#endif

/*---------------------------------------------------------------------------*/

// address space layout
// TODO document each zone, also explain that since vector space is in ram, it uses the ram primitives

#define MAX_VEC_ENCODING 2047
#define MIN_VEC_ENCODING 1280
#define VEC_BYTES ((MAX_VEC_ENCODING - MIN_VEC_ENCODING + 1)*4)
// if the pic has less than 8k of memory, start vector space lower

#define MAX_RAM_ENCODING 1279
#define MIN_RAM_ENCODING 512
#define RAM_BYTES ((MAX_RAM_ENCODING - MIN_RAM_ENCODING + 1)*4)

#define MIN_FIXNUM_ENCODING 3
#define MIN_FIXNUM -1
#define MAX_FIXNUM 255
#define MIN_ROM_ENCODING (MIN_FIXNUM_ENCODING + MAX_FIXNUM - MIN_FIXNUM + 1)

#define OBJ_TO_RAM_ADDR(o,f) ((((o) - MIN_RAM_ENCODING) << 2) + (f))
#define OBJ_TO_ROM_ADDR(o,f) ((((o) - MIN_ROM_ENCODING) << 2) + (CODE_START + 4 + (f)))

#ifdef PICOBOARD2
#ifdef SIXPIC
#define ram_get(a) *(a+0x200)
#define ram_set(a,x) *(a+0x200) = (x)
#else
#define ram_get(a) *(uint8*)(a+0x200)
#define ram_set(a,x) *(uint8*)(a+0x200) = (x)
#endif
#endif
#ifdef WORKSTATION
uint8 ram_mem[RAM_BYTES + VEC_BYTES];
#define ram_get(a) ram_mem[a]
#define ram_set(a,x) ram_mem[a] = (x)
#endif

#ifdef  PICOBOARD2
#ifndef SIXPIC
// provided by SIXPIC
uint8 rom_get (rom_addr a){
  return *(rom uint8*)a;
}
#endif
#endif

#ifdef WORKSTATION
#define ROM_BYTES 8192
uint8 rom_mem[ROM_BYTES] =
  {
#define RED_GREEN
#define PUTCHAR_LIGHT_not
#ifdef RED_GREEN
    0xFB, 0xD7, 0x03, 0x00, 0x00, 0x00, 0x00, 0x32
    , 0x03, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00
    , 0x08, 0x50, 0x80, 0x16, 0xFE, 0xE8, 0x00, 0xFC
    , 0x32, 0x80, 0x2D, 0xFE, 0xFC, 0x31, 0x80, 0x43
    , 0xFE, 0xFC, 0x33, 0x80, 0x2D, 0xFE, 0xFC, 0x31
    , 0x80, 0x43, 0xFE, 0x90, 0x16, 0x01, 0x20, 0xFC
    , 0x32, 0xE3, 0xB0, 0x37, 0x09, 0xF3, 0xFF, 0x20
    , 0xFC, 0x33, 0xE3, 0xB0, 0x40, 0x0A, 0xF3, 0xFF
    , 0x08, 0xF3, 0xFF, 0x01, 0x40, 0x21, 0xD1, 0x00
    , 0x02, 0xC0, 0x4C, 0x71, 0x01, 0x20, 0x50, 0x90
    , 0x51, 0x00, 0xF1, 0x40, 0xD8, 0xB0, 0x59, 0x90
    , 0x51, 0x00, 0xFF
#endif
#ifdef PUTCHAR_LIGHT
    0xFB, 0xD7, 0x00, 0x00, 0x80, 0x08, 0xFE, 0xE8
    , 0x00, 0xF6, 0xF5, 0x90, 0x08
#endif
  };
uint8 rom_get (rom_addr a) {
  return rom_mem[a-CODE_START];
}
#endif

#define RAM_GET_FIELD0_MACRO(o) ram_get (OBJ_TO_RAM_ADDR(o,0))
#define RAM_SET_FIELD0_MACRO(o,val) ram_set (OBJ_TO_RAM_ADDR(o,0), val)
#define ROM_GET_FIELD0_MACRO(o) rom_get (OBJ_TO_ROM_ADDR(o,0))

#define RAM_GET_GC_TAGS_MACRO(o) (RAM_GET_FIELD0_MACRO(o) & 0x60)
#define RAM_GET_GC_TAG0_MACRO(o) (RAM_GET_FIELD0_MACRO(o) & 0x20)
#define RAM_GET_GC_TAG1_MACRO(o) (RAM_GET_FIELD0_MACRO(o) & 0x40)
#define RAM_SET_GC_TAGS_MACRO(o,tags)                                      \
  (RAM_SET_FIELD0_MACRO(o,(RAM_GET_FIELD0_MACRO(o) & 0x9f) | (tags)))
#define RAM_SET_GC_TAG0_MACRO(o,tag)                                    \
  RAM_SET_FIELD0_MACRO(o,(RAM_GET_FIELD0_MACRO(o) & 0xdf) | (tag))
#define RAM_SET_GC_TAG1_MACRO(o,tag)                                    \
  RAM_SET_FIELD0_MACRO(o,(RAM_GET_FIELD0_MACRO(o) & 0xbf) | (tag))

#define RAM_GET_FIELD1_MACRO(o) ram_get (OBJ_TO_RAM_ADDR(o,1))
#define RAM_GET_FIELD2_MACRO(o) ram_get (OBJ_TO_RAM_ADDR(o,2))
#define RAM_GET_FIELD3_MACRO(o) ram_get (OBJ_TO_RAM_ADDR(o,3))
#define RAM_SET_FIELD1_MACRO(o,val) ram_set (OBJ_TO_RAM_ADDR(o,1), val)
#define RAM_SET_FIELD2_MACRO(o,val) ram_set (OBJ_TO_RAM_ADDR(o,2), val)
#define RAM_SET_FIELD3_MACRO(o,val) ram_set (OBJ_TO_RAM_ADDR(o,3), val)
#define ROM_GET_FIELD1_MACRO(o) rom_get (OBJ_TO_ROM_ADDR(o,1))
#define ROM_GET_FIELD2_MACRO(o) rom_get (OBJ_TO_ROM_ADDR(o,2))
#define ROM_GET_FIELD3_MACRO(o) rom_get (OBJ_TO_ROM_ADDR(o,3))

word ram_get_gc_tags (obj o) { return RAM_GET_GC_TAGS_MACRO(o); }
word ram_get_gc_tag0 (obj o) { return RAM_GET_GC_TAG0_MACRO(o); }
word ram_get_gc_tag1 (obj o) { return RAM_GET_GC_TAG1_MACRO(o); }
void ram_set_gc_tags (obj o, word tags) { RAM_SET_GC_TAGS_MACRO(o, tags); }
void ram_set_gc_tag0 (obj o, word tag) { RAM_SET_GC_TAG0_MACRO(o,tag); }
void ram_set_gc_tag1 (obj o, word tag) { RAM_SET_GC_TAG1_MACRO(o,tag); }
word ram_get_field0 (obj o) { return RAM_GET_FIELD0_MACRO(o); }
word ram_get_field1 (obj o) { return RAM_GET_FIELD1_MACRO(o); }
word ram_get_field2 (obj o) { return RAM_GET_FIELD2_MACRO(o); }
word ram_get_field3 (obj o) { return RAM_GET_FIELD3_MACRO(o); }
void ram_set_field0 (obj o, word val) { RAM_SET_FIELD0_MACRO(o,val); }
void ram_set_field1 (obj o, word val) { RAM_SET_FIELD1_MACRO(o,val); }
void ram_set_field2 (obj o, word val) { RAM_SET_FIELD2_MACRO(o,val); }
void ram_set_field3 (obj o, word val) { RAM_SET_FIELD3_MACRO(o,val); }
word rom_get_field0 (obj o) { return ROM_GET_FIELD0_MACRO(o); }
word rom_get_field1 (obj o) { return ROM_GET_FIELD1_MACRO(o); }
word rom_get_field2 (obj o) { return ROM_GET_FIELD2_MACRO(o); }
word rom_get_field3 (obj o) { return ROM_GET_FIELD3_MACRO(o); }

obj ram_get_car (obj o);
obj rom_get_car (obj o);
obj ram_get_cdr (obj o);
obj rom_get_cdr (obj o);
void ram_set_car (obj o, obj val);
void ram_set_cdr (obj o, obj val);

obj ram_get_entry (obj o);
obj rom_get_entry (obj o);

obj get_global (uint8 i);
void set_global (uint8 i, obj o);


/*---------------------------------------------------------------------------*/

/*
  OBJECT ENCODING:

  #f           0
  #t           1
  ()           2
  fixnum n     MIN_FIXNUM -> 3 ... MAX_FIXNUM -> 3 + (MAX_FIXNUM-MIN_FIXNUM)
  rom object   4 + (MAX_FIXNUM-MIN_FIXNUM) ... MIN_RAM_ENCODING-1
  ram object   MIN_RAM_ENCODING ... MAX_RAM_ENCODING
  u8vector     MIN_VEC_ENCODING ... 8191

  layout of memory allocated objects:

  Gs represent mark bits used by the gc

  ifdef INFINITE_PRECISION_BIGNUMS
  bignum n     0GG***** **next** hhhhhhhh llllllll  (16 bit digit)
  TODO what to do with the gc tags for the bignums ? will this work ?
  
  ifndef INFINITE_PRECISION_BIGNUMS
  bignum n     00000000 uuuuuuuu hhhhhhhh llllllll  (24 bit signed integer)
  TODO doesn't work properly for the moment. only 16 bits are usable now

  pair         1GGaaaaa aaaaaaaa 000ddddd dddddddd
  a is car
  d is cdr
  gives an address space of 2^13 * 4 = 32k divided between simple objects,
  rom, ram and vectors

  symbol       1GG00000 00000000 00100000 00000000

  string       1GG***** *chars** 01000000 00000000

  u8vector     1GGxxxxx xxxxxxxx 011yyyyy yyyyyyyy
  x is length of the vector, in bytes (stored raw, not encoded as an object)
  y is pointer to the elements themselves (stored in vector space)

  closure      01Gaaaaa aaaaaaaa aaaxxxxx xxxxxxxx
  0x5ff<a<0x4000 is entry
  x is pointer to environment
  the reason why the environment is on the cdr (and the entry is split on 3
  bytes) is that, when looking for a variable, a closure is considered to be a
  pair. The compiler adds an extra offset to any variable in the closure's
  environment, so the car of the closure (which doesn't really exist) is never
  checked, but the cdr is followed to find the other bindings
  
  continuation 1GGxxxxx xxxxxxxx 100yyyyy yyyyyyyy
  x is parent continuation
  y is pointer to the second half, which is a closure (contains env and entry)
  
  An environment is a list of objects built out of pairs.  On entry to
  a procedure the environment is the list of parameters to which is
  added the environment of the closure being called.

  The first byte at the entry point of a procedure gives the arity of
  the procedure:

  n = 0 to 127    -> procedure has n parameters (no rest parameter)
  n = -128 to -1  -> procedure has -n parameters, the last is
  a rest parameter
*/

#define OBJ_FALSE 0
#define OBJ_TRUE  1
#define encode_bool(x) (x)

#define OBJ_NULL  2

// fixnum definitions in picobit-vm.h , address space layout section

#define ENCODE_FIXNUM(n) ((n) + (MIN_FIXNUM_ENCODING - MIN_FIXNUM))
#define DECODE_FIXNUM(o) ((o) - (MIN_FIXNUM_ENCODING - MIN_FIXNUM))

#define IN_VEC(o) ((o) >= MIN_VEC_ENCODING)
#define IN_RAM(o) (!IN_VEC(o) && ((o) >= MIN_RAM_ENCODING))
#define IN_ROM(o) (!IN_VEC(o) && !IN_RAM(o) && ((o) >= MIN_ROM_ENCODING))

// bignum first byte : 00Gxxxxx
#define BIGNUM_FIELD0 0
#define RAM_BIGNUM(o) ((ram_get_field0 (o) & 0xc0) == BIGNUM_FIELD0)
#define ROM_BIGNUM(o) ((rom_get_field0 (o) & 0xc0) == BIGNUM_FIELD0)

// composite first byte : 1GGxxxxx
#define COMPOSITE_FIELD0 0x80
#define RAM_COMPOSITE(o) ((ram_get_field0 (o) & 0x80) == COMPOSITE_FIELD0)
#define ROM_COMPOSITE(o) ((rom_get_field0 (o) & 0x80) == COMPOSITE_FIELD0)

// pair third byte : 000xxxxx
#define PAIR_FIELD2 0
#define RAM_PAIR(o) (RAM_COMPOSITE (o) && ((ram_get_field2 (o) & 0xe0) == PAIR_FIELD2))
#define ROM_PAIR(o) (ROM_COMPOSITE (o) && ((rom_get_field2 (o) & 0xe0) == PAIR_FIELD2))

// symbol third byte : 001xxxxx
#define SYMBOL_FIELD2 0x20
#define RAM_SYMBOL(o) (RAM_COMPOSITE (o) && ((ram_get_field2 (o) & 0xe0) == SYMBOL_FIELD2))
#define ROM_SYMBOL(o) (ROM_COMPOSITE (o) && ((rom_get_field2 (o) & 0xe0) == SYMBOL_FIELD2))

// string third byte : 010xxxxx
#define STRING_FIELD2 0x40
#define RAM_STRING(o) (RAM_COMPOSITE (o) && ((ram_get_field2 (o) & 0xe0) == STRING_FIELD2))
#define ROM_STRING(o) (ROM_COMPOSITE (o) && ((rom_get_field2 (o) & 0xe0) == STRING_FIELD2))

// vector third byte : 011xxxxx
#define VECTOR_FIELD2 0x60
#define RAM_VECTOR(o) (RAM_COMPOSITE (o) && ((ram_get_field2 (o) & 0xe0) == VECTOR_FIELD2))
#define ROM_VECTOR(o) (ROM_COMPOSITE (o) && ((rom_get_field2 (o) & 0xe0) == VECTOR_FIELD2))

// continuation third byte : 100xxxxx
#define CONTINUATION_FIELD2 0x80
#define RAM_CONTINUATION(o) (RAM_COMPOSITE (o) && ((ram_get_field2 (o) & 0xe0) == CONTINUATION_FIELD2))
#define ROM_CONTINUATION(o) (ROM_COMPOSITE (o) && ((rom_get_field2 (o) & 0xe0) == CONTINUATION_FIELD2))

// closure first byte : 01Gxxxxx
#define CLOSURE_FIELD0 0x40
#define RAM_CLOSURE(o) ((ram_get_field0 (o) & 0xc0) == CLOSURE_FIELD0)
#define ROM_CLOSURE(o) ((rom_get_field0 (o) & 0xc0) == CLOSURE_FIELD0)

/*---------------------------------------------------------------------------*/

// bignum definitions

#ifdef INFINITE_PRECISION_BIGNUMS

#define digit_width 16

typedef obj integer;
typedef uint16 digit; // TODO why these ? adds to the confusion
typedef uint32 two_digit;

#define obj_eq(x,y) ((x) == (y))
#define integer_hi_set(x,y) ram_set_car (x, y)

#define ZERO ENCODE_FIXNUM(0)
#define NEG1 (ZERO-1)
#define POS1 (ZERO+1)

integer make_integer (digit lo, integer hi);
integer integer_hi (integer x);
digit integer_lo (integer x);

integer norm (obj prefix, integer n);
uint8 negp (integer x);
uint8 cmp (integer x, integer y);
uint16 integer_length (integer x);
integer shr (integer x);
integer negative_carry (integer carry);
integer shl (integer x);
integer shift_left (integer x, uint16 n);
integer add (integer x, integer y);
integer invert (integer x);
integer sub (integer x, integer y);
integer neg (integer x);
integer scale (digit n, integer x);
integer mulnonneg (integer x, integer y);
integer divnonneg (integer x, integer y);
  
uint16 decode_int (obj o);
obj encode_int (uint16 n);

#endif

/*---------------------------------------------------------------------------*/

// garbage collector

// TODO explain what each tag means, with 1-2 mark bits
#define GC_TAG_0_LEFT   (1<<5)
#define GC_TAG_1_LEFT   (2<<5)
#define GC_TAG_UNMARKED (0<<5)

/* Number of object fields of objects in ram */
#define HAS_2_OBJECT_FIELDS(visit) (RAM_PAIR(visit) || RAM_CONTINUATION(visit))
#ifdef INFINITE_PRECISION_BIGNUMS
#define HAS_1_OBJECT_FIELD(visit)  (RAM_COMPOSITE(visit) \
				    || RAM_CLOSURE(visit) || RAM_BIGNUM(visit))
#else
#define HAS_1_OBJECT_FIELD(visit)  (RAM_COMPOSITE(visit) || RAM_CLOSURE(visit))
#endif
// all composites except pairs and continuations have 1 object field

#define NIL OBJ_FALSE

obj free_list; /* list of unused cells */
obj free_list_vec; /* list of unused cells in vector space */

obj arg1; /* root set */
obj arg2;
obj arg3;
obj arg4;
obj arg5;
obj cont;
obj env;

uint8 na; /* interpreter variables */
rom_addr pc;
uint8 glovars;
rom_addr entry;
uint8 bytecode;
uint8 bytecode_hi4;
uint8 bytecode_lo4;
uint16 a1;
uint16 a2;
uint16 a3;

/*---------------------------------------------------------------------------*/

// primitives

#ifdef WORKSTATION
char *prim_name[64];
#endif

void prim_numberp ();
void prim_add ();
void prim_mul ();
void prim_div ();
void prim_rem ();
void prim_neg ();
void prim_eq ();
void prim_lt ();
void prim_gt ();
// TODO we have extra primitives, pring back geq, leq, and put them in a sensible place in the primitives
void prim_ior ();
void prim_xor ();

void prim_pairp ();
obj cons (obj car, obj cdr);
void prim_cons ();
void prim_car ();
void prim_cdr ();
void prim_set_car ();
void prim_set_cdr ();
void prim_nullp ();

void prim_u8vectorp ();
void prim_make_u8vector ();
void prim_u8vector_ref ();
void prim_u8vector_set ();
void prim_u8vector_length ();
void prim_u8vector_copy ();

void prim_eqp ();
void prim_not ();
void prim_symbolp ();
void prim_stringp ();
void prim_string2list ();
void prim_list2string ();
void prim_booleanp ();

#ifdef WORKSTATION
void show (obj o);
void print (obj o);
#endif
void prim_print ();
uint32 read_clock ();
void prim_clock ();
void prim_motor ();
void prim_led ();
void prim_led2_color ();
void prim_getchar_wait ();
void prim_putchar ();
void prim_beep ();
void prim_adc ();
void prim_sernum ();

void prim_network_init ();
void prim_network_cleanup ();
void prim_receive_packet_to_u8vector ();
void prim_send_packet_from_u8vector ();

/*---------------------------------------------------------------------------*/

// dispatch

#define FETCH_NEXT_BYTECODE() bytecode = rom_get (pc++)

#define BEGIN_DISPATCH()                        \
  dispatch:                                     \
  IF_TRACE(show_state (pc));                    \
  FETCH_NEXT_BYTECODE();                        \
  bytecode_hi4 = bytecode & 0xf0;               \
  bytecode_lo4 = bytecode & 0x0f;               \
  switch (bytecode_hi4 >> 4) {

#define END_DISPATCH() }

#define CASE(opcode) case (opcode):;

#define DISPATCH(); goto dispatch;

#define PUSH_CONSTANT1     0x0
#define PUSH_CONSTANT2     0x1
#define PUSH_STACK1        0x2
#define PUSH_STACK2        0x3
#define PUSH_GLOBAL        0x4
#define SET_GLOBAL         0x5
#define CALL               0x6
#define JUMP               0x7
#define LABEL_INSTR        0x8
#define PUSH_CONSTANT_LONG 0x9

#define FREE1              0xa
#define FREE2              0xb

#define PRIM1              0xc
#define PRIM2              0xd
#define PRIM3              0xe
#define PRIM4              0xf

#define PUSH_ARG1() push_arg1 ()
#define POP() pop()

void push_arg1 ();
obj pop ();
void pop_procedure ();
void handle_arity_and_rest_param ();
void build_env ();
void save_cont ();
void interpreter ();

/*---------------------------------------------------------------------------*/

// debugging functions

#ifdef WORKSTATION
void show_type (obj o);
void show_state (rom_addr pc);
#endif

/*---------------------------------------------------------------------------*/

#endif

