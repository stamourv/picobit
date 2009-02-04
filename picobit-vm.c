/* file: "picobit-vm.c" */

/*
 * Copyright 2008 by Marc Feeley and Vincent St-Amour, All Rights Reserved.
 *
 * History:
 *
 *   15/08/2004  Release of version 1
 *   06/07/2008  Modified for PICOBOARD2_R3
 *   18/07/2008  Modified to use new object representation
 *   17/12/2008  Release of version 2
 */

#define DEBUG_not
#define DEBUG_GC_not
// TODO once this is stable, put as default
#define INFINITE_PRECISION_BIGNUMS_not

/*---------------------------------------------------------------------------*/

typedef char int8;
typedef short int16;
typedef long int32;
typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned long uint32;

#define true  1
#define false 0

typedef uint8 boolean;
// TODO was signed, preventive change

/*---------------------------------------------------------------------------*/


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
#include <pcap.h>

// for libpcap

#define MAX_PACKET_SIZE BUFSIZ
#define PROMISC 1
#define TO_MSEC 1

char errbuf[PCAP_ERRBUF_SIZE];
pcap_t *handle;

#define INTERFACE "eth0"

char buf [MAX_PACKET_SIZE]; // buffer for writing


#ifdef _WIN32
#include <sys/types.h>
#include <sys/timeb.h>
#include <conio.h>
#else
#include <sys/time.h>
#endif

#endif


/*---------------------------------------------------------------------------*/

#define WORD_BITS 8

#define CODE_START 0x5000

#ifdef DEBUG
#define IF_TRACE(x) x
#define IF_GC_TRACE(x) x
#else
#define IF_TRACE(x)
#define IF_GC_TRACE(x)
#endif

/*---------------------------------------------------------------------------*/


#ifdef PICOBOARD2

#define ERROR(prim, msg) halt_with_error()
#define TYPE_ERROR(prim, type) halt_with_error()

#endif


#ifdef WORKSTATION

#define ERROR(prim, msg) error (prim, msg)
#define TYPE_ERROR(prim, type) type_error (prim, type)

void error (char *prim, char *msg)
{
  printf ("ERROR: %s: %s\n", prim, msg);
  exit (1);
}

void type_error (char *prim, char *type)
{
  printf ("ERROR: %s: An argument of type %s was expected\n", prim, type);
  exit (1);
}

#endif


/*---------------------------------------------------------------------------*/

#if WORD_BITS <= 8
typedef uint8 word;
#else
typedef uint16 word;
#endif

typedef uint16 ram_addr;
typedef uint16 rom_addr;

typedef uint16 obj;

#ifdef INFINITE_PRECISION_BIGNUMS

#define digit_width 16

typedef obj integer;
typedef uint16 digit; // TODO why this ? adds to the confusion
typedef uint32 two_digit;

#endif

/*---------------------------------------------------------------------------*/

#define MAX_VEC_ENCODING 8191
#define MIN_VEC_ENCODING 4096
#define VEC_BYTES ((MAX_VEC_ENCODING - MIN_VEC_ENCODING + 1)*4)
// TODO this is new. if the pic has less than 8k of memory, start this lower
// TODO the pic actually has 2k, so change these
// TODO we'd only actually need 1024 or so for ram and vectors, since we can't address more. this gives us a lot of rom space

#define MAX_RAM_ENCODING 4095
#define MIN_RAM_ENCODING 512
#define RAM_BYTES ((MAX_RAM_ENCODING - MIN_RAM_ENCODING + 1)*4)
// TODO watch out if we address more than what the PIC actually has

#if WORD_BITS == 8
#define OBJ_TO_VEC_ADDR(o,f) (((ram_addr)((uint16)(o) - MIN_RAM_ENCODING) << 2) + (f))
#define OBJ_TO_RAM_ADDR(o,f) (((ram_addr)((uint16)(o) - MIN_RAM_ENCODING) << 2) + (f))
#define OBJ_TO_ROM_ADDR(o,f) (((rom_addr)((uint16)(o) - MIN_ROM_ENCODING) << 2) + (CODE_START + 4 + (f)))
#endif

#ifdef PICOBOARD2

#define ram_get(a) *(uint8*)(a+0x200)
#define ram_set(a,x) *(uint8*)(a+0x200) = (x)
#endif


#ifdef WORKSTATION

uint8 ram_mem[RAM_BYTES + VEC_BYTES];

#define ram_get(a) ram_mem[a]
#define ram_set(a,x) ram_mem[a] = (x)

#endif


/*---------------------------------------------------------------------------*/

#ifdef PICOBOARD2

uint8 rom_get (rom_addr a)
{
  return *(rom uint8*)a;
}

#endif


#ifdef WORKSTATION

#define ROM_BYTES 8192
// TODO the new pics have 32k, change this ? minus the vm size, firmware ?

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

uint8 rom_get (rom_addr a)
{
  return rom_mem[a-CODE_START];
}

#endif

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

  G's represent mark bits used by the gc

  ifdef INFINITE_PRECISION_BIGNUMS
  bignum n     0GG***** **next** hhhhhhhh llllllll  (16 bit digit)
  TODO make sure this works with the "new" object representation, that the first 3 bits are enough to spot bignums, quick check of the bignum predicate indicates this would work, now implement this pointer FOOBIGNUM
  TODO what to do with the gc tags for the bignums ? will this work ?
  
  ifndef INFINITE_PRECISION_BIGNUMS
  bignum n     0000000 uuuuuuuu hhhhhhhh llllllll  (24 bit signed integer)

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
#define encode_bool(x) ((obj)(x))

#define OBJ_NULL  2

#define MIN_FIXNUM_ENCODING 3
// TODO change these ? were -5 and 40, with the new bignums, the needs for these might change
#define MIN_FIXNUM -1
// TODO FOOBIGNUMS, was 0, but -1 needed to be a fixnum for the algos to work
#define MAX_FIXNUM 255
#define MIN_ROM_ENCODING (MIN_FIXNUM_ENCODING+MAX_FIXNUM-MIN_FIXNUM+1)

#define ENCODE_FIXNUM(n) ((obj)(n) + (MIN_FIXNUM_ENCODING - MIN_FIXNUM))
#define DECODE_FIXNUM(o) ((int32)(o) - (MIN_FIXNUM_ENCODING - MIN_FIXNUM))

// TODO why this ifdef ?
#if WORD_BITS == 8
#define IN_VEC(o) ((o) >= MIN_VEC_ENCODING)
#define IN_RAM(o) (!IN_VEC(o) && ((o) >= MIN_RAM_ENCODING))
#define IN_ROM(o) (!IN_VEC(o) && !IN_RAM(o) && ((o) >= MIN_ROM_ENCODING))
#endif

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

#if WORD_BITS == 8
#define RAM_GET_FIELD1_MACRO(o) ram_get (OBJ_TO_RAM_ADDR(o,1))
#define RAM_GET_FIELD2_MACRO(o) ram_get (OBJ_TO_RAM_ADDR(o,2))
#define RAM_GET_FIELD3_MACRO(o) ram_get (OBJ_TO_RAM_ADDR(o,3))
#define RAM_SET_FIELD1_MACRO(o,val) ram_set (OBJ_TO_RAM_ADDR(o,1), val)
#define RAM_SET_FIELD2_MACRO(o,val) ram_set (OBJ_TO_RAM_ADDR(o,2), val)
#define RAM_SET_FIELD3_MACRO(o,val) ram_set (OBJ_TO_RAM_ADDR(o,3), val)
#define ROM_GET_FIELD1_MACRO(o) rom_get (OBJ_TO_ROM_ADDR(o,1))
#define ROM_GET_FIELD2_MACRO(o) rom_get (OBJ_TO_ROM_ADDR(o,2))
#define ROM_GET_FIELD3_MACRO(o) rom_get (OBJ_TO_ROM_ADDR(o,3))
#define VEC_GET_BYTE0_MACRO(o) ram_get (OBJ_TO_RAM_ADDR(o,0))
#define VEC_GET_BYTE1_MACRO(o) ram_get (OBJ_TO_RAM_ADDR(o,1))
#define VEC_GET_BYTE2_MACRO(o) ram_get (OBJ_TO_RAM_ADDR(o,2))
#define VEC_GET_BYTE3_MACRO(o) ram_get (OBJ_TO_RAM_ADDR(o,3))
#define VEC_SET_BYTE0_MACRO(o,val) ram_set (OBJ_TO_RAM_ADDR(o,0), val)
#define VEC_SET_BYTE1_MACRO(o,val) ram_set (OBJ_TO_RAM_ADDR(o,1), val)
#define VEC_SET_BYTE2_MACRO(o,val) ram_set (OBJ_TO_RAM_ADDR(o,2), val)
#define VEC_SET_BYTE3_MACRO(o,val) ram_set (OBJ_TO_RAM_ADDR(o,3), val)
#endif

uint8 ram_get_gc_tags (obj o) { return RAM_GET_GC_TAGS_MACRO(o); }
uint8 ram_get_gc_tag0 (obj o) { return RAM_GET_GC_TAG0_MACRO(o); }
uint8 ram_get_gc_tag1 (obj o) { return RAM_GET_GC_TAG1_MACRO(o); }
void ram_set_gc_tags  (obj o, uint8 tags) { RAM_SET_GC_TAGS_MACRO(o, tags); }
void ram_set_gc_tag0 (obj o, uint8 tag) { RAM_SET_GC_TAG0_MACRO(o,tag); }
void ram_set_gc_tag1 (obj o, uint8 tag) { RAM_SET_GC_TAG1_MACRO(o,tag); }
uint8 ram_get_field0 (obj o) { return RAM_GET_FIELD0_MACRO(o); }
word ram_get_field1 (obj o) { return RAM_GET_FIELD1_MACRO(o); }
word ram_get_field2 (obj o) { return RAM_GET_FIELD2_MACRO(o); }
word ram_get_field3 (obj o) { return RAM_GET_FIELD3_MACRO(o); }
word ram_get_fieldn (obj o, uint8 n)
{
  switch (n)
    {
    case 0: return ram_get_field0 (o);
    case 1: return ram_get_field1 (o);
    case 2: return ram_get_field2 (o);
    case 3: return ram_get_field3 (o);
    }
}
void ram_set_field0 (obj o, uint8 val) { RAM_SET_FIELD0_MACRO(o,val); }
void ram_set_field1 (obj o, word val) { RAM_SET_FIELD1_MACRO(o,val); }
void ram_set_field2 (obj o, word val) { RAM_SET_FIELD2_MACRO(o,val); }
void ram_set_field3 (obj o, word val) { RAM_SET_FIELD3_MACRO(o,val); }
void ram_set_fieldn (obj o, uint8 n, word val)
{
  switch (n)
    {
    case 0: ram_set_field0 (o, val); break;
    case 1: ram_set_field1 (o, val); break;
    case 2: ram_set_field2 (o, val); break;
    case 3: ram_set_field3 (o, val); break;
    }  
}
uint8 rom_get_field0 (obj o) { return ROM_GET_FIELD0_MACRO(o); }
word rom_get_field1 (obj o) { return ROM_GET_FIELD1_MACRO(o); }
word rom_get_field2 (obj o) { return ROM_GET_FIELD2_MACRO(o); }
word rom_get_field3 (obj o) { return ROM_GET_FIELD3_MACRO(o); }
/* word vec_get_byte0 (obj o) { return VEC_GET_BYTE0_MACRO(o); } */
/* word vec_get_byte1 (obj o) { return VEC_GET_BYTE1_MACRO(o); } */
/* word vec_get_byte2 (obj o) { return VEC_GET_BYTE2_MACRO(o); } */
/* word vec_get_byte3 (obj o) { return VEC_GET_BYTE3_MACRO(o); } */
/* word vec_set_byte0 (obj o, word val) { VEC_SET_BYTE0_MACRO(o,val); } */
/* word vec_set_byte1 (obj o, word val) { VEC_SET_BYTE1_MACRO(o,val); } */
/* word vec_set_byte2 (obj o, word val) { VEC_SET_BYTE2_MACRO(o,val); } */
/* word vec_set_byte3 (obj o, word val) { VEC_SET_BYTE3_MACRO(o,val); } */

obj get_field0 (obj o) // TODO these are not used yet, will they be useful at all ?
{
  if (IN_RAM(o))
    return ram_get_field0 (o);
  else
    return rom_get_field0 (o);
}
obj get_field1 (obj o)
{
  if (IN_RAM(o))
    return ram_get_field1 (o);
  else
    return rom_get_field1 (o);
}
obj get_field2 (obj o)
{
  if (IN_RAM(o))
    return ram_get_field2 (o);
  else
    return rom_get_field2 (o);
}
obj get_field3 (obj o)
{
  if (IN_RAM(o))
    return ram_get_field3 (o);
  else
    return rom_get_field3 (o);
}


obj ram_get_car (obj o)
{ return ((ram_get_field0 (o) & 0x1f) << 8) | ram_get_field1 (o); }
obj rom_get_car (obj o)
{ return ((rom_get_field0 (o) & 0x1f) << 8) | rom_get_field1 (o); }
obj ram_get_cdr (obj o)
{ return ((ram_get_field2 (o) & 0x1f) << 8) | ram_get_field3 (o); }
obj rom_get_cdr (obj o)
{ return ((rom_get_field2 (o) & 0x1f) << 8) | rom_get_field3 (o); }
obj get_car (obj o)
{
  if (IN_RAM(o))
    return ram_get_car (o);
  else
    return rom_get_car (o);
}
obj get_cdr (obj o)
{
  if (IN_RAM(o))
    return ram_get_cdr (o);
  else
    return rom_get_cdr (o);
}

void ram_set_car (obj o, obj val)
{
  ram_set_field0 (o, (val >> 8) | (ram_get_field0 (o) & 0xe0));
  ram_set_field1 (o, val & 0xff);
}
void ram_set_cdr (obj o, obj val)
{
  ram_set_field2 (o, (val >> 8) | (ram_get_field2 (o) & 0xe0));
  ram_set_field3 (o, val & 0xff);
}

obj ram_get_entry (obj o)
{
  return (((ram_get_field0 (o) & 0x1f) << 11)
	  | (ram_get_field1 (o) << 3)
	  | (ram_get_field2 (o) >> 5));
}
obj rom_get_entry (obj o)
{
  return (((rom_get_field0 (o) & 0x1f) << 11)
	  | (rom_get_field1 (o) << 3)
	  | (rom_get_field2 (o) >> 5));
}
obj get_entry (obj o)
{
  if (IN_RAM(o))
    return ram_get_entry (o);
  else
    return rom_get_entry (o);
}


obj get_global (uint8 i)
// globals occupy the beginning of ram, with 2 globals per word
{
  if (i & 1)
    return ram_get_cdr (MIN_RAM_ENCODING + (i / 2));
  else
    return ram_get_car (MIN_RAM_ENCODING + (i / 2));
}

void set_global (uint8 i, obj o)
{
  if (i & 1)
    ram_set_cdr (MIN_RAM_ENCODING + (i / 2), o);
  else
    ram_set_car (MIN_RAM_ENCODING + (i / 2), o);
}

#ifdef WORKSTATION
void show_type (obj o) // for debugging purposes
  {
    printf("%d : ", o);
    if (o == OBJ_FALSE) printf("#f");
    else if (o == OBJ_TRUE) printf("#t");
    else if (o == OBJ_NULL) printf("()");
    else if (o < MIN_ROM_ENCODING) printf("fixnum");
    else if (IN_RAM (o))
      {
	if (RAM_BIGNUM(o)) printf("ram bignum");
	else if (RAM_PAIR(o)) printf("ram pair");
	else if (RAM_SYMBOL(o)) printf("ram symbol");
	else if (RAM_STRING(o)) printf("ram string");
	else if (RAM_VECTOR(o)) printf("ram vector");
	else if (RAM_CONTINUATION(o)) printf("ram continuation");
	else if (RAM_CLOSURE(o)) printf("ram closure");
      }
    else // ROM
      {
	if (ROM_BIGNUM(o)) printf("rom bignum");
	else if (ROM_PAIR(o)) printf("rom pair");
	else if (ROM_SYMBOL(o)) printf("rom symbol");
	else if (ROM_STRING(o)) printf("rom string");
	else if (ROM_VECTOR(o)) printf("rom vector");
	else if (ROM_CONTINUATION(o)) printf("rom continuation");
	else if (RAM_CLOSURE(o)) printf("rom closure");
      }
    printf("\n");
  }
#endif


/*---------------------------------------------------------------------------*/

/* Interface to GC */

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

/*---------------------------------------------------------------------------*/

/* Garbage collector */

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
int32 a1;
int32 a2;
int32 a3;

void init_ram_heap (void)
{
  uint8 i;
  obj o = MAX_RAM_ENCODING;

  free_list = 0;

  while (o > (MIN_RAM_ENCODING + (glovars + 1) / 2))
    // we don't want to add globals to the free list, and globals occupy the
    // beginning of memory at the rate of 2 globals per word (car and cdr)
    {
      ram_set_gc_tags (o, GC_TAG_UNMARKED);
      ram_set_car (o, free_list);
      free_list = o;
      o--;
    }

  free_list_vec = MIN_VEC_ENCODING;
  ram_set_car (free_list_vec, 0);
  // each node of the free list must know the free length that follows it
  // this free length is stored in words, not in bytes
  // if we did count in bytes, the number might need more than 13 bits
  ram_set_cdr (free_list_vec, VEC_BYTES / 4);
  
  for (i=0; i<glovars; i++)
    set_global (i, OBJ_FALSE);

  arg1 = OBJ_FALSE;
  arg2 = OBJ_FALSE;
  arg3 = OBJ_FALSE;
  arg4 = OBJ_FALSE;
  cont = OBJ_FALSE;
  env  = OBJ_NULL;
}


void mark (obj temp)
{  
  /* mark phase */
  
  obj stack;
  obj visit;

  if (IN_RAM(temp))
    {
      visit = NIL;

    push:

      stack = visit;
      visit = temp;
      
      IF_GC_TRACE(printf ("push   stack=%d  visit=%d (tag=%d)\n", stack, visit, ram_get_gc_tags (visit)>>5));
      
      if ((HAS_1_OBJECT_FIELD (visit) && ram_get_gc_tag0 (visit))
	  || (HAS_2_OBJECT_FIELDS (visit)
	      && (ram_get_gc_tags (visit) != GC_TAG_UNMARKED)))
	IF_GC_TRACE(printf ("case 1\n"));
      else
        {
          if (HAS_2_OBJECT_FIELDS(visit)) // pairs and continuations
            {
              IF_GC_TRACE(printf ("case 2\n"));

            visit_field2:

              temp = ram_get_cdr (visit);

              if (IN_RAM(temp))
                {
                  IF_GC_TRACE(printf ("case 3\n"));
                  ram_set_gc_tags (visit, GC_TAG_1_LEFT);
                  ram_set_cdr (visit, stack);
                  goto push;
                }

              IF_GC_TRACE(printf ("case 4\n"));

              goto visit_field1;
            }

          if (HAS_1_OBJECT_FIELD(visit))
            {
              IF_GC_TRACE(printf ("case 5\n"));

            visit_field1:

	      if (RAM_CLOSURE(visit)) // closures have the pointer in the cdr
		temp = ram_get_cdr (visit);
	      else
		temp = ram_get_car (visit);

              if (IN_RAM(temp))
                {
                  IF_GC_TRACE(printf ("case 6\n"));
                  ram_set_gc_tag0 (visit, GC_TAG_0_LEFT);
		  if (RAM_CLOSURE(visit))
		    ram_set_cdr (visit, stack);
		  else 
		    ram_set_car (visit, stack);		  

                  goto push;
                }

              IF_GC_TRACE(printf ("case 7\n"));
            }
          else
            IF_GC_TRACE(printf ("case 8\n"));

          ram_set_gc_tag0 (visit, GC_TAG_0_LEFT);
        }

    pop:

      IF_GC_TRACE(printf ("pop    stack=%d  visit=%d (tag=%d)\n", stack, visit, ram_get_gc_tags (visit)>>6));
      
      if (stack != NIL)
        {
	  if (HAS_2_OBJECT_FIELDS(stack) && ram_get_gc_tag1 (stack))
	    {
              IF_GC_TRACE(printf ("case 9\n"));
	      
              temp = ram_get_cdr (stack);  /* pop through cdr */
              ram_set_cdr (stack, visit);
              visit = stack;
              stack = temp;

	      ram_set_gc_tag1(visit, GC_TAG_UNMARKED);
	      // we unset the "1-left" bit
	      
              goto visit_field1;
            }

	  if (RAM_CLOSURE(stack))
	    // closures have one object field, but it's in the cdr
	    {
	      IF_GC_TRACE(printf ("case 10\n"));

              temp = ram_get_cdr (stack);  /* pop through cdr */
              ram_set_cdr (stack, visit);
              visit = stack;
              stack = temp;

              goto pop;
	    }

          IF_GC_TRACE(printf ("case 11\n"));

          temp = ram_get_car (stack);  /* pop through car */
          ram_set_car (stack, visit);
          visit = stack;
          stack = temp;

          goto pop;
        }
    }
}

#ifdef DEBUG_GC
int max_live = 0;
#endif

void sweep (void)
{
  /* sweep phase */

#ifdef DEBUG_GC
  int n = 0;
#endif

  obj visit = MAX_RAM_ENCODING;

  free_list = 0;

  while (visit >= (MIN_RAM_ENCODING + ((glovars + 1) / 2)))
    // we don't want to sweep the global variables area
    {
      if ((RAM_COMPOSITE(visit)
	   && (ram_get_gc_tags (visit) == GC_TAG_UNMARKED)) // 2 mark bit
	  || !(ram_get_gc_tags (visit) & GC_TAG_0_LEFT)) // 1 mark bit
	/* unmarked? */
        {
	  if (RAM_VECTOR(visit))
	    // when we sweep a vector, we also have to sweep its contents
	    {
	      obj o = ram_get_cdr (visit);
	      uint16 i = ram_get_car (visit); // number of elements
	      ram_set_car (o, free_list_vec);
	      ram_set_cdr (o, (i + 3) / 4); // free length, in words
	      free_list_vec = o;
	      // TODO merge free spaces
	    }
          ram_set_car (visit, free_list);
          free_list = visit;
        }
      else
        {
	  if (RAM_COMPOSITE(visit))
	    ram_set_gc_tags (visit, GC_TAG_UNMARKED);
	  else // only 1 mark bit to unset
	    ram_set_gc_tag0 (visit, GC_TAG_UNMARKED);
#ifdef DEBUG_GC
          n++;
#endif
        }
      visit--;
    }

#ifdef DEBUG_GC
  if (n > max_live)
    {
      max_live = n;
      printf ("**************** memory needed = %d\n", max_live+1);
      fflush (stdout);
    }
#endif
}

void gc (void)
{
  uint8 i;

  IF_TRACE(printf("\nGC BEGINS\n"));

  IF_GC_TRACE(printf("arg1\n"));
  mark (arg1);
  IF_GC_TRACE(printf("arg2\n"));
  mark (arg2);
  IF_GC_TRACE(printf("arg3\n"));
  mark (arg3);
  IF_GC_TRACE(printf("arg4\n"));
  mark (arg4);
  IF_GC_TRACE(printf("arg5\n"));
  mark (arg5);
  IF_GC_TRACE(printf("cont\n"));
  mark (cont);
  IF_GC_TRACE(printf("env\n"));
  mark (env);

  IF_GC_TRACE(printf("globals\n"));
  for (i=0; i<glovars; i++)
    mark (get_global (i));

  sweep ();
}

obj alloc_ram_cell (void)
{
  obj o;

#ifdef DEBUG_GC
  gc ();
#endif

  if (free_list == 0)
    {
#ifndef DEBUG_GC
      gc ();
      if (free_list == 0)
#endif
        ERROR("alloc_ram_cell", "memory is full");
    }

  o = free_list;
  
  free_list = ram_get_car (o);

  return o;
}

obj alloc_ram_cell_init (uint8 f0, uint8 f1, uint8 f2, uint8 f3)
{
  obj o = alloc_ram_cell ();

  ram_set_field0 (o, f0);
  ram_set_field1 (o, f1);
  ram_set_field2 (o, f2);
  ram_set_field3 (o, f3);

  return o;
}

obj alloc_vec_cell (uint16 n)
{
  obj o = free_list_vec;
  obj prec = 0;
  uint8 gc_done = 0;

#ifdef DEBUG_GC
  gc ();
  gc_done = 1;
#endif

  while ((ram_get_cdr (o) * 4) < n) // free space too small
    {
      if (o == 0) // no free space, or none big enough
	{
	  if (gc_done) // we gc'd, but no space is big enough for the vector
	    ERROR("alloc_vec_cell", "no room for vector");
#ifndef DEBUG_GC
	  gc ();
	  gc_done = 1;
#endif
	  o = free_list_vec;
	  prec = 0;
	  continue;
	} // TODO merge adjacent free spaces, maybe compact ?
      prec = o;
      o = ram_get_car (o);
    }

  // case 1 : the new vector fills every free word advertized, we remove the
  //  node from the free list
  if (((ram_get_cdr(o) * 4) - n) < 4)
    {
      if (prec)
	ram_set_car (prec, ram_get_car (o));
      else
	free_list_vec = ram_get_car (o);
    }
  // case 2 : there is still some space left in the free section, create a new
  //  node to represent this space
  else
    {
      obj new_free = o + (n + 3)/4;
      if (prec)
	ram_set_car (prec, new_free);
      else
	free_list_vec = new_free;
      ram_set_car (new_free, ram_get_car (o));
      ram_set_cdr (new_free, ram_get_cdr (o) - (n + 3)/4);
    }
  
  return o;
}

/*---------------------------------------------------------------------------*/

#ifdef INFINITE_PRECISION_BIGNUMS

// TODO FOOBIGNUMS this was taken from the bignum code, see if it works
int8 decode_int8 (obj o) // TODO never used except in decode_int
{ // TODO really fishy, to use only 8 bits this way...
  int8 result;
  if (o < MIN_FIXNUM_ENCODING)
    TYPE_ERROR("decode_int8.0", "integer");

  if (o <= (MIN_FIXNUM_ENCODING + (MAX_FIXNUM - MIN_FIXNUM)))
    return DECODE_FIXNUM(o);

  if (IN_RAM(o))
    {
      if (!RAM_BIGNUM(o))
	TYPE_ERROR("decode_int8.1", "integer");
      return ram_get_field3 (o);
    }
  else if (IN_ROM(o))
    {
      if (!ROM_BIGNUM(o))
	TYPE_ERROR("decode_int8.2", "integer");
      return rom_get_field3 (o);
    }
  else
    TYPE_ERROR("decode_int8.3", "integer");
}
// TODO how could this possibly work ? it does not consider other fields, same for encoding, get to the bottom of this

int32 decode_int (obj o)
{
  return decode_int8 (o); // TODO FOOBAR clearly wrong, is it used ?
}


obj encode_int (int32 n) // TODO never used in the bignum code
{
  if (n >= MIN_FIXNUM && n <= MAX_FIXNUM){
    return ENCODE_FIXNUM(n);
  }
  
  // TODO FOOBIGNUMS since we encode 0 here, and it's 00..0 we don't need to or with the 1st byte for the pointer, what happens with negative numbers, however ?
  return alloc_ram_cell_init (BIGNUM_FIELD0, ENCODE_FIXNUM(0), n >> 8, n);
}

#else

int32 decode_int (obj o)
{
  uint8 u;
  uint8 h;
  uint8 l;

  if (o < MIN_FIXNUM_ENCODING)
    TYPE_ERROR("decode_int.0", "integer");

  if (o <= (MIN_FIXNUM_ENCODING + (MAX_FIXNUM - MIN_FIXNUM)))
    return DECODE_FIXNUM(o);

  if (IN_RAM(o))
    {
      if (!RAM_BIGNUM(o))
        TYPE_ERROR("decode_int.1", "integer");

      u = ram_get_field1 (o);
      h = ram_get_field2 (o);
      l = ram_get_field3 (o);
    }
  else if (IN_ROM(o))
    {
      if (!ROM_BIGNUM(o))
        TYPE_ERROR("decode_int.2", "integer");

      u = rom_get_field1 (o);
      h = rom_get_field2 (o);
      l = rom_get_field3 (o);
    }
  else
    TYPE_ERROR("decode_int.3", "integer");

  if (u >= 128) // TODO FOOBIGNUMS uhh, what's that again ? is here since the beginning
    return ((int32)((((int16)u - 256) << 8) + h) << 8) + l;

  return ((int32)(((int16)u << 8) + h) << 8) + l;
}

obj encode_int (int32 n)
{
  if (n >= MIN_FIXNUM && n <= MAX_FIXNUM)
    return ENCODE_FIXNUM(n);

  return alloc_ram_cell_init (BIGNUM_FIELD0, n >> 16, n >> 8, n);
}

#endif

/*---------------------------------------------------------------------------*/

#ifdef WORKSTATION

void show (obj o)
{
#if 0
  printf ("[%d]", o);
#endif

  if (o == OBJ_FALSE)
    printf ("#f");
  else if (o == OBJ_TRUE)
    printf ("#t");
  else if (o == OBJ_NULL)
    printf ("()");
  else if (o <= (MIN_FIXNUM_ENCODING + (MAX_FIXNUM - MIN_FIXNUM)))
    printf ("%d", DECODE_FIXNUM(o));
  else
    {
      uint8 in_ram;

      if (IN_RAM(o))
        in_ram = 1;
      else
        in_ram = 0;

      if ((in_ram && RAM_BIGNUM(o)) || (!in_ram && ROM_BIGNUM(o))) // TODO FIX for new bignums
	printf ("%d", decode_int (o));
      else if ((in_ram && RAM_COMPOSITE(o)) || (!in_ram && ROM_COMPOSITE(o)))
        {
	  obj car;
	  obj cdr;

	  if ((in_ram && RAM_PAIR(o)) || (!in_ram && ROM_PAIR(o)))
	    {	      
	      if (in_ram)
		{
		  car = ram_get_car (o);
		  cdr = ram_get_cdr (o);
		}
	      else
		{
		  car = rom_get_car (o);
		  cdr = rom_get_cdr (o);
		}

	      printf ("(");
	      
	    loop:
	      
	      show (car);

	      if (cdr == OBJ_NULL)
		printf (")");
	      else if ((IN_RAM(cdr) && RAM_PAIR(cdr))
		       || (IN_ROM(cdr) && ROM_PAIR(cdr)))
		{
		  if (IN_RAM(cdr))
		    {
		      car = ram_get_car (cdr);
		      cdr = ram_get_cdr (cdr);
		    }
		  else
		    {
		      car = rom_get_car (cdr);
		      cdr = rom_get_cdr (cdr);
		    }
		  
		  printf (" ");
		  goto loop;
		}
	      else
		{
		  printf (" . ");
		  show (cdr);
		  printf (")");
		}
	    }
       	  else if ((in_ram && RAM_SYMBOL(o)) || (!in_ram && ROM_SYMBOL(o)))
	    printf ("#<symbol>");
	  else if ((in_ram && RAM_STRING(o)) || (!in_ram && ROM_STRING(o)))
	    printf ("#<string>");
	  else if ((in_ram && RAM_VECTOR(o)) || (!in_ram && ROM_VECTOR(o)))
	    printf ("#<vector %d>", o);
	  else
	    {
	      printf ("(");
	      car = ram_get_car (o);
	      cdr = ram_get_cdr (o);
	      // ugly hack, takes advantage of the fact that pairs and
	      // continuations have the same layout
	      goto loop;
	    }
        }
      else // closure
        {
          obj env;
          rom_addr pc;

          if (IN_RAM(o))
            env = ram_get_cdr (o);
          else
            env = rom_get_cdr (o);

          if (IN_RAM(o))
	    pc = ram_get_entry (o);
          else
	    pc = rom_get_entry (o);

          printf ("{0x%04x ", pc);
          show (env);
          printf ("}");
        }
    }

  fflush (stdout);
}

void show_state (rom_addr pc)
{
  printf("\n");
  printf ("pc=0x%04x bytecode=0x%02x env=", pc, rom_get (pc));
  show (env);
  printf (" cont=");
  show (cont);
  printf ("\n");
  fflush (stdout);
}

void print (obj o)
{
  show (o);
  printf ("\n");
  fflush (stdout);
}

#endif

/*---------------------------------------------------------------------------*/

/* Integer operations */

// TODO FOOBIGNUMS big pasted and NOT CHECKED section here
#ifdef INFINITE_PRECISION_BIGNUMS

#define obj_eq(x,y) ((x) == (y))

#define integer_hi_set(x,y) ram_set_car (x, y)
 // TODO FOOBIGNUMS won't work, I think, will erase next pointer (or set it only in part) ACTUALLY, this is probably supposed to change the pointer. changed field1, npw changes the whole car

#define ZERO ENCODE_FIXNUM(0)
#define NEG1 (ZERO-1)
#define POS1 (ZERO+1)

/* integer fixnum (uint8 n) // TODO this used to be a signed int, but broke everything. probably should be removed */
/* { */
/*   return ENCODE_FIXNUM (n); */
/* } */ // TODO if no ill effect is detected without this, remove it

// TODO this integer type is a mess, it should be obj, for clarity
integer make_integer (digit lo, integer hi) // TODO BAD, should use encode_int instead
{
  // TODO could this be fixed by a call to encode_int ?
  /*   if(!hi && lo <= MAX_FIXNUM) // TODO dependent on the current fixnum range, which starts at 0, fix this */ // TODO would this even be useful ? don't the math routines already revert to fixnums if needed ? or norm does it ?
/*     return ENCODE_FIXNUM(lo); */
  // TODO won't work, and the bignum functions are unaware of fixnums
  return alloc_ram_cell_init (BIGNUM_FIELD0 | (hi >> 8), hi, lo >> 8, lo); // TODO hi should always be a 13-bit pointer, to avoid clobbering the bignum field
}

integer integer_hi (integer x) // TODO should be used for decoding
{
  if (IN_RAM(x))
    return ram_get_car (x); // TODO was field1
  else if (IN_ROM(x))
    return rom_get_car (x); // TODO was field1
  else if (x < (MIN_FIXNUM_ENCODING - MIN_FIXNUM))
    return NEG1; /* negative fixnum */
  else
    return ZERO; /* nonnegative fixnum */
}

digit integer_lo (integer x)
{
  if (IN_RAM(x))
    return (((digit)ram_get_field2 (x)) << 8) + ram_get_field3 (x);
  else if (IN_ROM(x))
    return (((digit)rom_get_field2 (x)) << 8) + rom_get_field3 (x);
  else
    return DECODE_FIXNUM(x);
}

integer norm (obj prefix, integer n)
{
  /* norm(prefix,n) returns a normalized integer whose value is the
     integer n prefixed with the digits in prefix (a list of digits) */

  while (prefix != NIL)
    {
      digit d = integer_lo (prefix);
      obj temp = prefix;

      prefix = integer_hi (temp);

      if (obj_eq (n, ZERO))
        {
          if (d <= MAX_FIXNUM)
            {
              n = ENCODE_FIXNUM ((uint8)d); // TODO was fixnum, but was useless and broke stuff
              continue; // TODO with cast to unsigned, will it work for negative numbers ?
            }
        }
      else if (obj_eq (n, NEG1))
        {
          if (d >= (1<<digit_width) + MIN_FIXNUM)
            {
              n = ENCODE_FIXNUM ((uint8)(d - (1<<digit_width))); // TODO same
              continue;
            }
        }

      integer_hi_set (temp, n);
      n = temp;
    }

  return n;
}

boolean negp (integer x)
{
  /* negp(x) returns true iff x is negative */

  do
    {
      x = integer_hi (x);
      if (obj_eq (x, ZERO)) return false;
     } while (!obj_eq (x, NEG1));

  return true;
}

int8 cmp (integer x, integer y)
{
  /* cmp(x,y) return -1 when x<y, 1 when x>y, and 0 when x=y */

  int8 result = 0;
  digit xlo;
  digit ylo;

  for (;;)
    {
      if (obj_eq (x, ZERO) || obj_eq (x, NEG1))
        {
          if (!obj_eq (x, y))
            { if (negp (y)) result = 1; else result = -1; }
          break;
        }

      if (obj_eq (y, ZERO) || obj_eq (y, NEG1))
        {
          if (negp (x)) result = -1; else result = 1;
          break;
        }

      xlo = integer_lo (x);
      ylo = integer_lo (y);
      x = integer_hi (x);
      y = integer_hi (y);
      if (xlo != ylo)
        { if (xlo < ylo) result = -1; else result = 1; }
    }
  return result;
}

uint16 integer_length (integer x)
{
  /* integer_length(x) returns the number of bits in the binary
     representation of the nonnegative integer x */

  uint16 result = 0;
  integer next;
  digit d;

  while (!obj_eq ((next = integer_hi (x)), ZERO)) // TODO what happens if it ends with -1 ?
    {
      result += digit_width;
      x = next;
    }

  d = integer_lo (x);

  while (d > 0)
    {
      result++;
      d >>= 1;
    }

  return result;
}

integer shr (integer x)
{
  /* shr(x) returns the integer x shifted one bit to the right */

  obj result = NIL;
  digit d;

  for (;;)
    {
      if (obj_eq (x, ZERO) || obj_eq (x, NEG1))
        {
          result = norm (result, x);
          break;
        }

      d = integer_lo (x);
      x = integer_hi (x);
      result = make_integer ((d >> 1) |
                             ((integer_lo (x) & 1) ? (1<<(digit_width-1)) : 0),
                             result);
    }

  return result;
}

integer negative_carry (integer carry)
{
  if (carry)
    return NEG1;
  else
    return ZERO;
}

integer shl (integer x)
{
  /* shl(x) returns the integer x shifted one bit to the left */

  integer negc = ZERO; /* negative carry */
  integer temp;
  obj result = NIL;
  digit d;

  // TODO problem seems to occur when we pass from i=16 to i=17
/*   printf("SHL-before: lo=%d ; hi=%d ", integer_lo(x), integer_hi(x)); // TODO seems everything becomes 0 when we'd need 2 blocks... */
/*   p(x); */
/*   printf("\n"); */
  
  for (;;)
    {
      // TODO lo for 1st iteration is what's at address 0, not important
/*       printf("SHL-loop: negc : %d ; lo-r : %d ; hi-r : %d ; result ", negc, integer_lo(result), integer_hi(result)); // TODO qqch devient negaif, prob de signed, je crois, on a -262144, trouver ce que c'est en unsigned */
/*       p(result); */
/*       printf(" ; lo-x : %d ; hi-x : %d ; x ", integer_lo(x), integer_hi(x)); */
/*       p(x); */
      // TODO ok, both x and result are 0 (result point to NIL, x to 0), find which is supposed to be the msb, and give it its right value

      if (obj_eq (x, negc))
        {
          result = norm (result, x); // TODO see what happens in here, might be the cause of our problems
          break;
        }
      // TODO ok, so norm does nothing in the 1st iteration, for the case i from 16 to 17
/*       printf("SHL-loop2: lo-r : %d ; hi-r : %d ; result ", integer_lo(result), integer_hi(result)); // TODO qqch devient negaif, prob de signed, je crois, on a -262144, trouver ce que c'est en unsigned */
/*       p(result); */
/*       printf(" ; lo-x : %d ; hi-x : %d ; x ", integer_lo(x), integer_hi(x)); */
/*       p(x); */

      d = integer_lo (x);
      x = integer_hi (x);
      temp = negc;
      negc = negative_carry (d & (1<<(digit_width-1))); // TODO right side is constant, and sixpic has no constant folding
      //      printf("negc = %d\ntemp = %d (3 is NEG1, 4 is ZERO)\nd = %d\nmask = %d\ncarry = %d\n", negc, temp, d, (1 << (digit_width-1)), (d & (1<<(digit_width-1))));
      result = make_integer ((d << 1) | obj_eq (temp, NEG1), result);
      // TODO OK, we shift left because this is shl, it is supposed to... but the high bit is junked ?
    }

/*   printf("SHL-end: lo-r : %d ; hi-r : %d ; result ", integer_lo(result), integer_hi(result)); */
/*   p(result); */
/*   printf("\n"); */
  
  return result;
}

integer shift_left (integer x, uint16 n) // TODO have the primitves been changed for this and right ?
{
  /* shift_left(x,n) returns the integer x shifted n bits to the left */

  if (obj_eq (x, ZERO))
    return x;

  while (n & (digit_width-1))
    {
      x = shl (x);
      n--;
    }

  while (n > 0)
    {
      x = make_integer (0, x);
      n -= digit_width;
    }

  return x;
}

integer add (integer x, integer y)
{
  /* add(x,y) returns the sum of the integers x and y */

  integer negc = ZERO; /* negative carry */
  obj result = NIL; /* nil terminated for the norm function */
  digit dx;
  digit dy;

  for (;;)
    {
      if (obj_eq (x, negc))
        {
          result = norm (result, y);
          break;
        }

      if (obj_eq (y, negc))
        {
          result = norm (result, x);
          break;
        }

      dx = integer_lo (x);
      dy = integer_lo (y);
      dx = dx + dy; /* may wrap around */

      if (obj_eq (negc, ZERO))
        negc = negative_carry (dx < dy);
      else
        {
          dx++; /* may wrap around */
          negc = negative_carry (dx <= dy);
        }

      x = integer_hi (x);
      y = integer_hi (y);

      result = make_integer (dx, result);
    }

  return result;
}

integer invert (integer x)
{
  if (obj_eq (x, ZERO))
    return NEG1;
  else
    return ZERO;
}

integer sub (integer x, integer y)
{
  /* sub(x,y) returns the difference of the integers x and y */
  integer negc = NEG1; /* negative carry */
  obj result = NIL;
  digit dx;
  digit dy;

  for (;;)
    {
      if (obj_eq (x, negc) && (obj_eq (y, ZERO) || obj_eq (y, NEG1)))
        {
          result = norm (result, invert (y));
          break;
        }

      if (obj_eq (y, invert (negc)))
        {
          result = norm (result, x);
          break;
        }

      dx = integer_lo (x);
      dy = ~integer_lo (y);
      dx = dx + dy; /* may wrap around */

      if (obj_eq (negc, ZERO))
        negc = negative_carry (dx < dy);
      else
        {
          dx++; /* may wrap around */
          negc = negative_carry (dx <= dy);
        }

      x = integer_hi (x);
      y = integer_hi (y);

      result = make_integer (dx, result);
    }

  return result;
}

integer neg (integer x)
{
  /* neg(x) returns the integer -x */

  return sub (ZERO, x);
}

integer scale (digit n, integer x)
{
  /* scale(n,x) returns the integer n*x */

  obj result;
  digit carry;
  two_digit m;

  if ((n == 0) || obj_eq (x, ZERO))
    return ZERO;

  if (n == 1)
    return x;

  result = NIL;
  carry = 0;

  for (;;)
    {
      if (obj_eq (x, ZERO))
        {
          if (carry <= MAX_FIXNUM)
            result = norm (result, ENCODE_FIXNUM ((uint8)carry)); // TODO was fixnum, and int8 (signed)
          else
            result = norm (result, make_integer (carry, ZERO));
          break;
        }

      if (obj_eq (x, NEG1))
        {
          carry = carry - n;
          if (carry >= ((1<<digit_width) + MIN_FIXNUM))
            result = norm (result, ENCODE_FIXNUM ((uint8)carry)); // TODO was fixnum, and int8 (signed)
          else
            result = norm (result, make_integer (carry, NEG1));
          break;
        }

      m = (two_digit)integer_lo (x) * n + carry;

      x = integer_hi (x);
      carry = m >> digit_width;
      result = make_integer ((digit)m, result);
    }

  return result;
}

integer mulnonneg (integer x, integer y)
{
  /* mulnonneg(x,y) returns the product of the integers x and y
     where x is nonnegative */

  obj result = NIL;
  integer s = scale (integer_lo (x), y);

  for (;;)
    {
      result = make_integer (integer_lo (s), result);
      s = integer_hi (s);
      x = integer_hi (x);

      if (obj_eq (x, ZERO))
        break;

      s = add (s, scale (integer_lo (x), y));
    }

  return norm (result, s);
}

integer mul (integer x, integer y)
{
  /* mul(x,y) returns the product of the integers x and y */

  if (negp (x))
    return neg (mulnonneg (neg (x), y));
  else
    return mulnonneg (x, y);
}

integer divnonneg (integer x, integer y)
{
  /* divnonneg(x,y) returns the quotient and remainder of
     the integers x and y where x and y are nonnegative */

  integer result = ZERO;
  uint16 lx = integer_length (x);
  uint16 ly = integer_length (y);

  if (lx >= ly)
    {
      lx = lx - ly;

      y = shift_left (y, lx);

      do
        {
          result = shl (result);
          if (cmp (x, y) >= 0)
            {
              x = sub (x, y);
              result = add (POS1, result);
            }
          y = shr (y);
        } while (lx-- != 0);
    }

  return result;
}


void p (integer n)
{
  long long x; // TODO long long is 32 bits here, what about on a 64 bit machine ?
  x = ((long long)integer_lo (integer_hi (integer_hi (integer_hi (n))))<<48)+
    ((long long)integer_lo (integer_hi (integer_hi (n)))<<32)+
    ((long long)integer_lo (integer_hi (n))<<16)+
    (long long)integer_lo (n);
  printf ("%lld ", x);
  // TODO test for hex output, to avoid signedness problems
/*   printf("%x%x%x%x\n", // TODO prob, if a lower part is 0, will show 0, not 0000 */
/* 	 integer_lo (integer_hi (integer_hi (integer_hi (n)))), */
/* 	 integer_lo (integer_hi (integer_hi (n))), */
/* 	 integer_lo (integer_hi (n)), */
/* 	 integer_lo (n)); */
}

integer enc (long long n) // TODO used only for debugging
{
  integer result = NIL;

  while (n != 0 && n != -1)
    {
      result = make_integer ((digit)n, result);
      n >>= digit_width;
    }

  if (n < 0)
    return norm (result, NEG1);
  else
    return norm (result, ZERO);
}

void test (void) // TODO still in use ? no, but useful for tests
{
  integer min2;
  integer min1;
  integer zero;
  integer one;
  integer two;
  integer three;
  integer four;

  zero = make_integer (0x0000, 0);
  min1 = make_integer (0xffff, 0);
  integer_hi_set (zero, ZERO);
  integer_hi_set (min1, NEG1);

  min2 = make_integer (0xfffe, NEG1);
  one  = make_integer (0x0001, ZERO);
  two  = make_integer (0x0002, ZERO);
  three= make_integer (0x0003, ZERO);
  four = make_integer (0x0004, ZERO);

  //#if 0
  if (negp (ZERO)) printf ("zero is negp\n"); // should not show
  if (negp (NEG1)) printf ("min1 is negp\n");

  printf ("cmp(5,5) = %d\n",cmp (make_integer (5, ZERO), make_integer (5, ZERO)));
  printf ("cmp(2,5) = %d\n",cmp (make_integer (2, ZERO), make_integer (5, ZERO)));
  printf ("cmp(5,2) = %d\n",cmp (make_integer (5, ZERO), make_integer (2, ZERO)));

  printf ("cmp(-5,-5) = %d\n",cmp (make_integer (-5, NEG1), make_integer (-5, NEG1)));
  printf ("cmp(-2,-5) = %d\n",cmp (make_integer (-2, NEG1), make_integer (-5, NEG1)));
  printf ("cmp(-5,-2) = %d\n",cmp (make_integer (-5, NEG1), make_integer (-2, NEG1)));

  printf ("cmp(-5,65533) = %d\n",cmp (make_integer (-5, NEG1), make_integer (65533, ZERO)));
  printf ("cmp(-5,2)     = %d\n",cmp (make_integer (-5, NEG1), make_integer (2, ZERO)));
  printf ("cmp(5,-65533) = %d\n",cmp (make_integer (5, ZERO), make_integer (-65533, NEG1)));
  printf ("cmp(5,-2)     = %d\n",cmp (make_integer (5, ZERO), make_integer (-2, NEG1)));

  printf ("integer_length(0) = %d\n", integer_length (ZERO)); // these return the number of bits necessary to encode
  printf ("integer_length(1) = %d\n", integer_length (make_integer (1, ZERO)));
  printf ("integer_length(2) = %d\n", integer_length (make_integer (2, ZERO)));
  printf ("integer_length(3) = %d\n", integer_length (make_integer (3, ZERO)));
  printf ("integer_length(4) = %d\n", integer_length (make_integer (4, ZERO)));
  printf ("integer_length(65536 + 4) = %d\n", integer_length (make_integer (4, make_integer (1, ZERO))));


  printf ("1 = %d\n", one); // TODO these show the address, useful ?
  printf ("2 = %d\n", two);
  printf ("4 = %d\n", four);
  printf ("norm(2) = %d\n", norm (make_integer (0, make_integer (2, NIL)), ZERO)); // TODO these show the fixnum address (6 and 7), so it seems to be working
  printf ("norm(2) = %d\n", norm (make_integer (0, make_integer (2, NIL)), ZERO));
  printf ("norm(3) = %d\n", norm (make_integer (0, make_integer (3, NIL)), ZERO));
  printf ("norm(3) = %d\n", norm (make_integer (0, make_integer (3, NIL)), ZERO));

  printf ("shl(1) = %d\n", shl (one)); // TODO fixnums, again
  printf ("shl(2) = %d\n", shl (two));

  {
    integer n = one;
    int i;
    for (i=1; i<=34; i++)
      {
	printf("\nloop-1 : i=%d len=%d ", i, integer_length(n));
        p (n);
        n = shl(n);
      }
/*     for (i=1; i<=35; i++) */
/*       { */
/*         p (n); */
/*         n = shr(n); */
/*       } */ // TODO later
  }

  {
    integer n = shift_left (four, 5);
    int i;

/*     for (i=0; i<=14; i++) */
/*       { */
/*         p (shift_left (n, i*4)); */
/*       } */ // TODO later
  }

/*   p (add (enc (32768), enc (32768))); */
/*   p (add (enc (32768+(65536*65535LL)), enc (32768))); */

/*   p (sub (enc (32768), enc (-32768))); */
/*   p (sub (enc (32768+(65536*65535LL)), enc (-32768))); */

/*   p (sub (enc (32768), enc (32769))); */

/*   p (mul (enc (123456789), enc (1000000000))); */
/*   p (mul (enc (123456789), enc (-1000000000))); */
/*   p (mul (enc (-123456789), enc (1000000000))); */
/*   p (mul (enc (-123456789), enc (-1000000000))); */

/*   //#endif */

/*   p (divnonneg (enc (10000000-1), enc (500000))); */ // TODO later

  printf ("done\n");

  exit (0);
}

#endif

// TODO FOOBIGNUMS end pasted section

void prim_numberp (void)
{
  if (arg1 >= MIN_FIXNUM_ENCODING
      && arg1 <= (MIN_FIXNUM_ENCODING + (MAX_FIXNUM - MIN_FIXNUM)))
    arg1 = OBJ_TRUE;
  else
    {
      if (IN_RAM(arg1))
        arg1 = encode_bool (RAM_BIGNUM(arg1));
      else if (IN_ROM(arg1))
        arg1 = encode_bool (ROM_BIGNUM(arg1));
      else
        arg1 = OBJ_FALSE;
    }
}

void decode_2_int_args (void) // TODO fix for bignums ?
{
  a1 = decode_int (arg1); // TODO all math primitives call it, even for bignums, this is probably what causes problems, maybe not, since the primitives don't use a1 or a2, but rather arg1 and arg2
  a2 = decode_int (arg2);
}

void prim_add (void)
{
#ifdef INFINITE_PRECISION_BIGNUMS
  arg1 = add (arg1, arg2);
#else
  decode_2_int_args ();
  arg1 = encode_int (a1 + a2);
#endif
  arg2 = OBJ_FALSE;
}

void prim_sub (void)
{
#ifdef INFINITE_PRECISION_BIGNUMS
  arg1 = sub (arg1, arg2);
#else
  decode_2_int_args ();
  arg1 = encode_int (a1 - a2);
#endif
  arg2 = OBJ_FALSE;
}

void prim_mul (void)
{
#ifdef INFINITE_PRECISION_BIGNUMS
  arg1 = mul (arg1, arg2);
#else
  decode_2_int_args ();
  arg1 = encode_int (a1 * a2);
#endif
  arg2 = OBJ_FALSE;
}

void prim_div (void)
{
  decode_2_int_args (); // TODO useless work in the case of bignums, move in the else, but make sure that an error message is written even with bignums
  if (a2 == 0)
    ERROR("quotient", "divide by 0");
#ifdef INFINITE_PRECISION_BIGNUMS
  arg1 = ZERO;
#else
  arg1 = encode_int (a1 / a2);
#endif
  arg2 = OBJ_FALSE;
}

void prim_rem (void)
{
  decode_2_int_args (); // TODO same as div
  if (a2 == 0)
    ERROR("remainder", "divide by 0");
#ifdef INFINITE_PRECISION_BIGNUMS
  arg1 = ZERO;
#else
  arg1 = encode_int (a1 % a2);
#endif
  arg2 = OBJ_FALSE;
}

void prim_neg (void)
{
#ifdef INFINITE_PRECISION_BIGNUMS
  arg1 = neg (arg1);
#else
  a1 = decode_int (arg1);
  arg1 = encode_int (- a1);
#endif
}

void prim_eq (void)
{
#ifdef INFINITE_PRECISION_BIGNUMS
  arg1 = encode_bool(cmp (arg1, arg2) == 0);
#else
  decode_2_int_args ();
  arg1 = encode_bool(a1 == a2);
#endif
  arg2 = OBJ_FALSE;
}

void prim_lt (void)
{
#ifdef INFINITE_PRECISION_BIGNUMS
  arg1 = encode_bool(cmp (arg1, arg2) < 0);
#else
  decode_2_int_args ();
  arg1 = encode_bool(a1 < a2);
#endif
  arg2 = OBJ_FALSE;
}

void prim_gt (void)
{
#ifdef INFINITE_PRECISION_BIGNUMS
  arg1 = encode_bool(cmp (arg1, arg2) > 0);
#else
  decode_2_int_args ();
  arg1 = encode_bool(a1 > a2);
#endif
  arg2 = OBJ_FALSE;
}

void prim_ior (void) // TODO FOOBIGNUMS these have not been implemented with bignums, do it
{
  decode_2_int_args (); // TODO is the function call overhead worth it ?
  arg1 = encode_int (a1 | a2);
  arg2 = OBJ_FALSE;
}

void prim_xor (void)
{
  decode_2_int_args (); // TODO is the function call overhead worth it ?
  arg1 = encode_int (a1 ^ a2);
  arg2 = OBJ_FALSE;
}


/*---------------------------------------------------------------------------*/

/* List operations */

void prim_pairp (void)
{
  if (IN_RAM(arg1))
    arg1 = encode_bool (RAM_PAIR(arg1));
  else if (IN_ROM(arg1))
    arg1 = encode_bool (ROM_PAIR(arg1));
  else
    arg1 = OBJ_FALSE;
}

obj cons (obj car, obj cdr)
{
  return alloc_ram_cell_init (COMPOSITE_FIELD0 | (car >> 8),
			      car & 0xff,
			      PAIR_FIELD2 | (cdr >> 8),
			      cdr & 0xff);
}

void prim_cons (void)
{
  arg1 = cons (arg1, arg2);
  arg2 = OBJ_FALSE;
}

void prim_car (void)
{
  if (IN_RAM(arg1))
    {
      if (!RAM_PAIR(arg1))
        TYPE_ERROR("car.0", "pair");
      arg1 = ram_get_car (arg1);
    }
  else if (IN_ROM(arg1))
    {
      if (!ROM_PAIR(arg1))
        TYPE_ERROR("car.1", "pair");
      arg1 = rom_get_car (arg1);
    }
  else
    {
      TYPE_ERROR("car.2", "pair");
    }
}

void prim_cdr (void)
{
  if (IN_RAM(arg1))
    {
      if (!RAM_PAIR(arg1))
        TYPE_ERROR("cdr.0", "pair");
      arg1 = ram_get_cdr (arg1);
    }
  else if (IN_ROM(arg1))
    {
      if (!ROM_PAIR(arg1))
        TYPE_ERROR("cdr.1", "pair");
      arg1 = rom_get_cdr (arg1);
    }
  else
    {
      TYPE_ERROR("cdr.2", "pair");
    }
}

void prim_set_car (void)
{
  if (IN_RAM(arg1))
    {
      if (!RAM_PAIR(arg1))
        TYPE_ERROR("set-car!.0", "pair");

      ram_set_car (arg1, arg2);
      arg1 = OBJ_FALSE;
      arg2 = OBJ_FALSE;
    }
  else
    {
      TYPE_ERROR("set-car!.1", "pair");
    }
}

void prim_set_cdr (void)
{
  if (IN_RAM(arg1))
    {
      if (!RAM_PAIR(arg1))
        TYPE_ERROR("set-cdr!.0", "pair");

      ram_set_cdr (arg1, arg2);
      arg1 = OBJ_FALSE;
      arg2 = OBJ_FALSE;
    }
  else
    {
      TYPE_ERROR("set-cdr!.1", "pair");
    }
}

void prim_nullp (void)
{
  arg1 = encode_bool (arg1 == OBJ_NULL);
}

/*---------------------------------------------------------------------------*/

/* Vector operations */

void prim_u8vectorp (void)
{
  if (IN_RAM(arg1))
    arg1 = encode_bool (RAM_VECTOR(arg1));
  else if (IN_ROM(arg1))
    arg1 = encode_bool (ROM_VECTOR(arg1));
  else
    arg1 = OBJ_FALSE;
}

void prim_make_u8vector (void)
{
  decode_2_int_args (); // arg1 is length, arg2 is contents
  // TODO adapt for the new bignums
  if (a2 > 255)
    ERROR("make-u8vector", "byte vectors can only contain bytes");
    
  arg3 = alloc_vec_cell (a1);
  arg1 = alloc_ram_cell_init (COMPOSITE_FIELD0 | (a1 >> 8),
			      a1 & 0xff,
			      VECTOR_FIELD2 | (arg3 >> 8),
			      arg3 & 0xff);

  a1 = (a1 + 3) / 4; // actual length, in words
  while (a1--)
    {
      ram_set_field0 (arg3, a2);
      ram_set_field1 (arg3, a2);
      ram_set_field2 (arg3, a2);
      ram_set_field3 (arg3, a2);
      arg3++;
    }
}

void prim_u8vector_ref (void)
{
  a2 = decode_int (arg2);
  // TODO adapt for the new bignums
  if (IN_RAM(arg1))
    {
      if (!RAM_VECTOR(arg1))
	TYPE_ERROR("u8vector-ref.0", "vector");
      if ((ram_get_car (arg1) <= a2) || (a2 < 0))
	ERROR("u8vector-ref.0", "vector index invalid");
      arg1 = ram_get_cdr (arg1);
    }
  else if (IN_ROM(arg1))
    {
      if (!ROM_VECTOR(arg1))
	TYPE_ERROR("u8vector-ref.1", "vector");
      if ((rom_get_car (arg1) <= a2) || (a2 < 0))
	ERROR("u8vector-ref.1", "vector index invalid");
      arg1 = rom_get_cdr (arg1);
    }
  else
    TYPE_ERROR("u8vector-ref.2", "vector");

  if (IN_VEC(arg1))
    {
      arg1 += (a2 / 4);
      a2 %= 4;

      arg1 = encode_int (ram_get_fieldn (arg1, a2));
    }
  else // rom vector, stored as a list
    {
      while (a2--)
	arg1 = rom_get_cdr (arg1);

      // the contents are already encoded as fixnums
      arg1 = rom_get_car (arg1);
    }

  arg2 = OBJ_FALSE;
  arg3 = OBJ_FALSE;
  arg4 = OBJ_FALSE;
}

void prim_u8vector_set (void)
{ // TODO a lot in common with ref, abstract that
  a2 = decode_int (arg2); // TODO adapt for bignums
  a3 = decode_int (arg3);

  if (a3 > 255)
    ERROR("u8vector-set!", "byte vectors can only contain bytes");
  
  if (IN_RAM(arg1))
    {
      if (!RAM_VECTOR(arg1))
	TYPE_ERROR("u8vector-set!.0", "vector");
      if ((ram_get_car (arg1) <= a2) || (a2 < 0))
	ERROR("u8vector-set!", "vector index invalid");
      arg1 = ram_get_cdr (arg1);
    }
  else
    TYPE_ERROR("u8vector-set!.1", "vector");
  
  arg1 += (a2 / 4);
  a2 %= 4;

  ram_set_fieldn (arg1, a2, a3);

  arg1 = OBJ_FALSE;
  arg2 = OBJ_FALSE;
  arg3 = OBJ_FALSE;
}

void prim_u8vector_length (void)
{
  if (IN_RAM(arg1))
    {
      if (!RAM_VECTOR(arg1))
	TYPE_ERROR("u8vector-length.0", "vector");
      arg1 = encode_int (ram_get_car (arg1));
    }
  else if (IN_ROM(arg1))
    {
      if (!ROM_VECTOR(arg1))
	TYPE_ERROR("u8vector-length.1", "vector");
      arg1 = encode_int (rom_get_car (arg1));
    }
  else
    TYPE_ERROR("u8vector-length.2", "vector");
}

void prim_u8vector_copy (void)
{
  // arg1 is source, arg2 is source-start, arg3 is target, arg4 is target-start
  // arg5 is number of bytes to copy

  a1 = decode_int (arg2); // TODO adapt for bignums
  a2 = decode_int (arg4);
  a3 = decode_int (arg5);

  // case 1 : ram to ram
  if (IN_RAM(arg1) && IN_RAM(arg3))
    {
      if (!RAM_VECTOR(arg1) || !RAM_VECTOR(arg3))
	TYPE_ERROR("u8vector-copy!.0", "vector");
      if ((ram_get_car (arg1) < (a1 + a3)) || (a1 < 0) ||
	  (ram_get_car (arg3) < (a2 + a3)) || (a2 < 0))
	ERROR("u8vector-copy!.0", "vector index invalid");

      // position to the start
      arg1 = ram_get_cdr (arg1);
      arg1 += (a1 / 4);
      a1 %= 4;
      arg3 = ram_get_cdr (arg3);
      arg3 += (a2 / 4);
      a2 %= 4;

      // copy
      while (a3--)
	{
	  ram_set_fieldn (arg3, a2, ram_get_fieldn (arg1, a1));
	  
	  a1++;
	  arg1 += (a1 / 4);
	  a1 %= 4; // TODO merge with the previous similar block ?
	  a2++;
	  arg3 += (a2 / 4);
	  a2 %= 4;
	}
    }
  // case 2 : rom to ram
  else if (IN_ROM(arg1) && IN_RAM(arg3))
    {
      if (!ROM_VECTOR(arg1) || !RAM_VECTOR(arg3))
	TYPE_ERROR("u8vector-copy!.1", "vector");
      if ((rom_get_car (arg1) < (a1 + a3)) || (a1 < 0) ||
	  (ram_get_car (arg3) < (a2 + a3)) || (a2 < 0))
	ERROR("u8vector-copy!.1", "vector index invalid");

      arg1 = rom_get_cdr (arg1);
      while (a1--)
	arg1 = rom_get_cdr (arg1);

      arg3 = ram_get_cdr (arg3);
      arg3 += (a2 / 4);
      a2 %= 4;

      while (a3--)
	{
	  ram_set_fieldn (arg3, a2, decode_int (rom_get_car (arg1)));

	  arg1 = rom_get_cdr (arg1);
	  a2++;
	  arg3 += (a2 / 4);
	  a2 %= 4; // TODO very similar to the other case
	}
    }
  else
    TYPE_ERROR("u8vector-copy!.2", "vector");
    
  arg1 = OBJ_FALSE;
  arg2 = OBJ_FALSE;
  arg3 = OBJ_FALSE;
  arg4 = OBJ_FALSE;
  arg5 = OBJ_FALSE;
}

/*---------------------------------------------------------------------------*/

/* Miscellaneous operations */

void prim_eqp (void)
{
  arg1 = encode_bool (arg1 == arg2);
  arg2 = OBJ_FALSE;
}

void prim_not (void)
{
  arg1 = encode_bool (arg1 == OBJ_FALSE);
}

void prim_symbolp (void)
{
  if (IN_RAM(arg1))
    arg1 = encode_bool (RAM_SYMBOL(arg1));
  else if (IN_ROM(arg1))
    arg1 = encode_bool (ROM_SYMBOL(arg1));
  else
    arg1 = OBJ_FALSE;
}

void prim_stringp (void)
{
  if (IN_RAM(arg1))
    arg1 = encode_bool (RAM_STRING(arg1));
  else if (IN_ROM(arg1))
    arg1 = encode_bool (ROM_STRING(arg1));
  else
    arg1 = OBJ_FALSE;
}

void prim_string2list (void)
{
  if (IN_RAM(arg1))
    {
      if (!RAM_STRING(arg1))
        TYPE_ERROR("string->list.0", "string");

      arg1 = ram_get_car (arg1);
    }
  else if (IN_ROM(arg1))
    {
      if (!ROM_STRING(arg1))
        TYPE_ERROR("string->list.1", "string");

      arg1 = rom_get_car (arg1);
    }
  else
    TYPE_ERROR("string->list.2", "string");
}

void prim_list2string (void)
{
  arg1 = alloc_ram_cell_init (COMPOSITE_FIELD0 | ((arg1 & 0x1f00) >> 8),
			      arg1 & 0xff,
			      STRING_FIELD2,
			      0);
}

void prim_booleanp (void)
{
  arg1 = encode_bool (arg1 < 2);
}


/*---------------------------------------------------------------------------*/

/* Robot specific operations */


void prim_print (void)
{
#ifdef PICOBOARD2
#endif

#ifdef WORKSTATION

  print (arg1);

#endif

  arg1 = OBJ_FALSE;
}


int32 read_clock (void)
{
  int32 now = 0;

#ifdef PICOBOARD2

  now = from_now( 0 );

#endif

#ifdef WORKSTATION

#ifdef _WIN32

  static int32 start = 0;
  struct timeb tb;

  ftime (&tb);

  now = tb.time * 1000 + tb.millitm;
  if (start == 0)
    start = now;
  now -= start;

#else

  static int32 start = 0;
  struct timeval tv;

  if (gettimeofday (&tv, NULL) == 0)
    {
      now = tv.tv_sec * 1000 + tv.tv_usec / 1000;
      if (start == 0)
        start = now;
      now -= start;
    }

#endif

#endif

  return now;
}


void prim_clock (void)
{
  arg1 = encode_int (read_clock ());
}


void prim_motor (void)
{
  decode_2_int_args (); // TODO fix for bignums

  if (a1 < 1 || a1 > 2 || a2 < -100 || a2 > 100)
    ERROR("motor", "argument out of range");

#ifdef PICOBOARD2

  MOTOR_set( a1, a2 );

#endif

#ifdef WORKSTATION

  printf ("motor %d -> power=%d\n", a1, a2);
  fflush (stdout);

#endif

  arg1 = OBJ_FALSE;
  arg2 = OBJ_FALSE;
}


void prim_led (void)
{
  decode_2_int_args (); // TODO fix for bignums
  a3 = decode_int (arg3);

  if (a1 < 1 || a1 > 3 || a2 < 0 || a3 < 0)
    ERROR("led", "argument out of range");

#ifdef PICOBOARD2

  LED_set( a1, a2, a3 );

#endif

#ifdef WORKSTATION

  printf ("led %d -> duty=%d period=%d\n", a1, a2, a3 );
  fflush (stdout);

#endif

  arg1 = OBJ_FALSE;
  arg2 = OBJ_FALSE;
  arg3 = OBJ_FALSE;
}


void prim_led2_color (void)
{
  a1 = decode_int (arg1); // TODO fix for bignums

  if (a1 < 0 || a1 > 1)
    ERROR("led2-colors", "argument out of range");

#ifdef PICOBOARD2

  LED2_color_set( a1 );

#endif

#ifdef WORKSTATION

  printf ("led2-color -> %s\n", (a1==0)?"green":"red");
  fflush (stdout);

#endif

  arg1 = OBJ_FALSE;
}


void prim_getchar_wait (void)
{
  decode_2_int_args(); // TODO fix for bignums
  a1 = read_clock () + a1;

  if (a1 < 0 || a2 < 1 || a2 > 3)
    ERROR("getchar-wait", "argument out of range");

#ifdef PICOBOARD2

  arg1 = OBJ_FALSE;

  {
    serial_port_set ports;
    ports = serial_rx_wait_with_timeout( a2, a1 );
    if (ports != 0)
      arg1 = encode_int (serial_rx_read( ports ));
  }

#endif

#ifdef WORKSTATION

#ifdef _WIN32

  arg1 = OBJ_FALSE;

  do
    {
      if (_kbhit ())
        {
          arg1 = encode_int (_getch ());
          break;
        }
    } while (read_clock () < a1);


#else

  arg1 = encode_int (getchar ());

#endif

#endif
}


void prim_putchar (void)
{
  decode_2_int_args ();

  if (a1 < 0 || a1 > 255 || a2 < 1 || a2 > 3)
    ERROR("putchar", "argument out of range");

#ifdef PICOBOARD2

  serial_tx_write( a2, a1 );

#endif

#ifdef WORKSTATION

  putchar (a1);
  fflush (stdout);

#endif

  arg1 = OBJ_FALSE;
  arg2 = OBJ_FALSE;
}


void prim_beep (void)
{
  decode_2_int_args (); // TODO fix for bignums

  if (a1 < 1 || a1 > 255 || a2 < 0)
    ERROR("beep", "argument out of range");

#ifdef PICOBOARD2

  beep( a1, from_now( a2 ) );

#endif

#ifdef WORKSTATION

  printf ("beep -> freq-div=%d duration=%d\n", a1, a2 );
  fflush (stdout);

#endif

  arg1 = OBJ_FALSE;
  arg2 = OBJ_FALSE;
}


void prim_adc (void)
{
  short x;

  a1 = decode_int (arg1); // TODO fix for bignums

  if (a1 < 1 || a1 > 3)
    ERROR("adc", "argument out of range");

#ifdef PICOBOARD2

  x = adc( a1 );

#endif

#ifdef WORKSTATION

  x = read_clock () & 255;

  if (x > 127) x = 256 - x;

  x += 200;

#endif

  arg1 = encode_int (x);
}


void prim_dac (void) // TODO not used
{
  a1 = decode_int (arg1); // TODO fix for bignums

  if (a1 < 0 || a1 > 255)
    ERROR("dac", "argument out of range");

#ifdef PICOBOARD2

  dac( a1 );

#endif

#ifdef WORKSTATION

  printf ("dac -> %d\n", a1 );
  fflush (stdout);

#endif

  arg1 = OBJ_FALSE;
}


void prim_sernum (void)
{
  short x;

#ifdef PICOBOARD2

  x = serial_num ();

#endif

#ifdef WORKSTATION

  x = 0;

#endif

  arg1 = encode_int (x);
}


/*---------------------------------------------------------------------------*/
// networking, currently works only on workstations

#ifdef WORKSTATION

void prim_network_init (void)
{ // TODO maybe put in the initialization of the vm
  handle= pcap_open_live(INTERFACE, MAX_PACKET_SIZE, PROMISC, TO_MSEC, errbuf);
  if (handle == NULL)
    ERROR("network-init", "interface not responding");
}

void prim_network_cleanup (void)
{ // TODO maybe put in halt ?
  pcap_close(handle);
}

void prim_receive_packet_to_u8vector (void)
{
  // arg1 is the vector in which to put the received packet
  if (!RAM_VECTOR(arg1))
    TYPE_ERROR("receive-packet-to-u8vector", "vector");
  
  // receive the packet in the buffer
  struct pcap_pkthdr header;
  const u_char *packet;

  packet = pcap_next(handle, &header);

  if (packet == NULL)
    header.len = 0;

  if (ram_get_car (arg1) < header.len)
    ERROR("receive-packet-to-u8vector", "packet longer than vector");
    
  if (header.len > 0) // we have received a packet, write it in the vector
    {
      arg2 = rom_get_cdr (arg1);
      arg1 = header.len; // we return the length of the received packet
      a1 = 0;

      while (a1 < arg1)
	{
	  ram_set_fieldn (arg2, a1 % 4, (char)packet[a1]);
	  a1++;
	  arg2 += (a1 % 4) ? 0 : 1;
	}

      arg2 = OBJ_FALSE;
    }
  else // no packet to be read
    arg1 = OBJ_FALSE;
}

void prim_send_packet_from_u8vector (void)
{
  // arg1 is the vector which contains the packet to be sent
  // arg2 is the length of the packet
  // TODO only works with ram vectors for now
  if (!RAM_VECTOR(arg1))
    TYPE_ERROR("send-packet-from-vector!", "vector");
  a2 = decode_int (arg2); // TODO fix for bignums
  a1 = 0; 
  
  // TODO test if the length of the packet is longer than the length of the vector
  if (ram_get_car (arg1) < a2)
    ERROR("send-packet-from-u8vector", "packet cannot be longer than vector");

  arg1 = ram_get_cdr (arg1);
  
  // copy the packet to the output buffer
  while (a1 < a2)
    buf[a1] = ram_get_fieldn (arg1, a1 % 4);
  // TODO maybe I could just give pcap the pointer to the memory BREGG

  if (pcap_sendpacket(handle, buf, a2) < 0) // TODO an error has occurred, can we reuse the interface ?
    arg1 = OBJ_FALSE;
  else
    arg1 = OBJ_TRUE;

  arg2 = OBJ_FALSE;
}

#endif

/*---------------------------------------------------------------------------*/

#ifdef WORKSTATION

int hidden_fgetc (FILE *f)
{
  int c = fgetc (f);
#if 0
  printf ("{%d}",c);
  fflush (stdout);
#endif
  return c;
}

#define fgetc(f) hidden_fgetc(f)

void write_hex_nibble (int n)
{
  putchar ("0123456789ABCDEF"[n]);
}

void write_hex (uint8 n)
{
  write_hex_nibble (n >> 4);
  write_hex_nibble (n & 0x0f);
}

int hex (int c)
{
  if (c >= '0' && c <= '9')
    return (c - '0');

  if (c >= 'A' && c <= 'F')
    return (c - 'A' + 10);

  if (c >= 'a' && c <= 'f')
    return (c - 'a' + 10);

  return -1;
}

int read_hex_byte (FILE *f)
{
  int h1 = hex (fgetc (f));
  int h2 = hex (fgetc (f));

  if (h1 >= 0 && h2 >= 0)
    return (h1<<4) + h2;

  return -1;
}

int read_hex_file (char *filename)
{
  int c;
  FILE *f = fopen (filename, "r");
  int result = 0;
  int len;
  int a, a1, a2;
  int t;
  int b;
  int i;
  uint8 sum;
  int hi16 = 0;

  for (i=0; i<ROM_BYTES; i++)
    rom_mem[i] = 0xff;

  if (f != NULL)
    {
      while ((c = fgetc (f)) != EOF)
        {
          if ((c == '\r') || (c == '\n'))
            continue;

          if (c != ':' ||
              (len = read_hex_byte (f)) < 0 ||
              (a1 = read_hex_byte (f)) < 0 ||
              (a2 = read_hex_byte (f)) < 0 ||
              (t = read_hex_byte (f)) < 0)
            break;

          a = (a1 << 8) + a2;

          i = 0;
          sum = len + a1 + a2 + t;

          if (t == 0)
            {
            next0:

              if (i < len)
                {
                  unsigned long adr = ((unsigned long)hi16 << 16) + a - CODE_START;

                  if ((b = read_hex_byte (f)) < 0)
                    break;

                  if (adr >= 0 && adr < ROM_BYTES)
                    rom_mem[adr] = b;

                  a = (a + 1) & 0xffff;
                  i++;
                  sum += b;

                  goto next0;
                }
            }
          else if (t == 1)
            {
              if (len != 0)
                break;
            }
          else if (t == 4)
            {
              if (len != 2)
                break;

              if ((a1 = read_hex_byte (f)) < 0 ||
                  (a2 = read_hex_byte (f)) < 0)
                break;

              sum += a1 + a2;

              hi16 = (a1<<8) + a2;
            }
          else
            break;

          if ((b = read_hex_byte (f)) < 0)
            break;

          sum = -sum;

          if (sum != b)
            {
              printf ("*** HEX file checksum error (expected 0x%02x)\n", sum);
              break;
            }

          c = fgetc (f);

          if ((c != '\r') && (c != '\n'))
            break;

          if (t == 1)
            {
              result = 1;
              break;
            }
        }

      if (result == 0)
        printf ("*** HEX file syntax error\n");

      fclose (f);
    }

  return result;
}

#endif

/*---------------------------------------------------------------------------*/

#define FETCH_NEXT_BYTECODE() bytecode = rom_get (pc++)

#define BEGIN_DISPATCH()                        \
  dispatch:                                     \
  IF_TRACE(show_state (pc));                    \
  FETCH_NEXT_BYTECODE();                        \
  bytecode_hi4 = bytecode & 0xf0;               \
  bytecode_lo4 = bytecode & 0x0f;               \
  switch (bytecode_hi4 >> 4) {

#define END_DISPATCH() }

#define CASE(opcode) case (opcode>>4):;

#define DISPATCH(); goto dispatch;

#if 0
#define pc FSR1
#define sp FSR2
#define bytecode TABLAT
#define bytecode_hi4 WREG
#endif

#define PUSH_CONSTANT1     0x00
#define PUSH_CONSTANT2     0x10
#define PUSH_STACK1        0x20
#define PUSH_STACK2        0x30
#define PUSH_GLOBAL        0x40
#define SET_GLOBAL         0x50
#define CALL               0x60
#define JUMP               0x70
#define LABEL_INSTR        0x80
#define PUSH_CONSTANT_LONG 0x90

#define FREE1              0xa0
#define FREE2              0xb0

#define PRIM1              0xc0
#define PRIM2              0xd0
#define PRIM3              0xe0
#define PRIM4              0xf0

#ifdef WORKSTATION

char *prim_name[64] =
  {
    "prim #%number?",
    "prim #%+",
    "prim #%-",
    "prim #%*",
    "prim #%quotient",
    "prim #%remainder",
    "prim #%neg",
    "prim #%=",
    "prim #%<",
    "prim #%ior",
    "prim #%>",
    "prim #%xor",
    "prim #%pair?",
    "prim #%cons",
    "prim #%car",
    "prim #%cdr",
    "prim #%set-car!",
    "prim #%set-cdr!",
    "prim #%null?",
    "prim #%eq?",
    "prim #%not",
    "prim #%get-cont",
    "prim #%graft-to-cont",
    "prim #%return-to-cont",
    "prim #%halt",
    "prim #%symbol?",
    "prim #%string?",
    "prim #%string->list",
    "prim #%list->string",
    "prim #%make-u8vector",
    "prim #%u8vector-ref",
    "prim #%u8vector-set!",
    "prim #%print",
    "prim #%clock",
    "prim #%motor",
    "prim #%led",
    "prim #%led2-color",
    "prim #%getchar-wait",
    "prim #%putchar",
    "prim #%beep",
    "prim #%adc",
    "prim #%u8vector?",
    "prim #%sernum",
    "prim #%u8vector-length",
    "prim #%u8vector-copy!",
    "shift",
    "pop",
    "return",
    "prim #%boolean?",
    "prim #%network-init",
    "prim #%network-cleanup",
    "prim #%receive-packet-to-u8vector",
    "prim #%send-packet-from-u8vector",
    "prim 53",
    "prim 54",
    "prim 55",
    "prim 56",
    "prim 57",
    "prim 58",
    "prim 59",
    "prim 60",
    "prim 61",
    "prim 62",
    "prim 63"
  };

#endif

#define PUSH_ARG1() push_arg1 ()
#define POP() pop()

void push_arg1 (void)
{
  env = cons (arg1, env);
  arg1 = OBJ_FALSE;
}

obj pop (void)
{
  obj o = ram_get_car (env);
  env = ram_get_cdr (env);
  return o;
}

void pop_procedure (void)
{
  arg1 = POP();
  
  if (IN_RAM(arg1))
    {      
      if (!RAM_CLOSURE(arg1))
	TYPE_ERROR("pop_procedure.0", "procedure");
      
      entry = ram_get_entry (arg1) + CODE_START;
    }
  else if (IN_ROM(arg1))
    {      
      if (!ROM_CLOSURE(arg1))
        TYPE_ERROR("pop_procedure.1", "procedure");

      entry = rom_get_entry (arg1) + CODE_START;
    }
  else
    TYPE_ERROR("pop_procedure.2", "procedure");
}

void handle_arity_and_rest_param (void)
{
  uint8 np;

  np = rom_get (entry++);

  if ((np & 0x80) == 0)
    {
      if (na != np)
        ERROR("handle_arity_and_rest_param.0", "wrong number of arguments");
    }
  else
    {
      np = ~np;

      if (na < np)
        ERROR("handle_arity_and_rest_param.1", "wrong number of arguments");

      arg3 = OBJ_NULL;

      while (na > np)
        {
          arg4 = POP();

          arg3 = cons (arg4, arg3);
          arg4 = OBJ_FALSE;

          na--;
        }

      arg1 = cons (arg3, arg1);
      arg3 = OBJ_FALSE;
    }
}

void build_env (void)
{
  while (na != 0)
    {
      arg3 = POP();

      arg1 = cons (arg3, arg1);

      na--;
    }

  arg3 = OBJ_FALSE;
}

void save_cont (void)
{
  // the second half is a closure
  arg3 = alloc_ram_cell_init (CLOSURE_FIELD0 | (pc >> 11),
			      (pc >> 3) & 0xff,
			      ((pc & 0x0007) << 5) | (env >> 8),
			      env & 0xff);
  cont = alloc_ram_cell_init (COMPOSITE_FIELD0 | (cont >> 8),
                              cont & 0xff,
			      CONTINUATION_FIELD2 | (arg3 >> 8),
                              arg3 & 0xff);
  arg3 = OBJ_FALSE;
}

void interpreter (void)
{
  pc = (CODE_START + 4) + ((rom_addr)rom_get (CODE_START+2) << 2);

  glovars = rom_get (CODE_START+3); // number of global variables

  init_ram_heap ();
  
  BEGIN_DISPATCH();

  /***************************************************************************/
  CASE(PUSH_CONSTANT1);

  IF_TRACE(printf("  (push-constant "); show (bytecode_lo4); printf (")\n"));

  arg1 = bytecode_lo4;

  PUSH_ARG1();

  DISPATCH();

  /***************************************************************************/
  CASE(PUSH_CONSTANT2);

  IF_TRACE(printf("  (push-constant "); show (bytecode_lo4+16); printf (")\n"));
  arg1 = bytecode_lo4+16;

  PUSH_ARG1();

  DISPATCH();

  /***************************************************************************/
  CASE(PUSH_STACK1);

  IF_TRACE(printf("  (push-stack %d)\n", bytecode_lo4));

  arg1 = env;

  while (bytecode_lo4 != 0)
    {
      arg1 = ram_get_cdr (arg1);
      bytecode_lo4--;
    }

  arg1 = ram_get_car (arg1);

  PUSH_ARG1();

  DISPATCH();

  /***************************************************************************/
  CASE(PUSH_STACK2);

  IF_TRACE(printf("  (push-stack %d)\n", bytecode_lo4+16));

  bytecode_lo4 += 16;

  arg1 = env;

  while (bytecode_lo4 != 0)
    {
      arg1 = ram_get_cdr (arg1);
      bytecode_lo4--;
    }

  arg1 = ram_get_car (arg1);

  PUSH_ARG1();

  DISPATCH();

  /***************************************************************************/
  CASE(PUSH_GLOBAL);

  IF_TRACE(printf("  (push-global %d)\n", bytecode_lo4));

  arg1 = get_global (bytecode_lo4);

  PUSH_ARG1();

  DISPATCH();

  /***************************************************************************/
  CASE(SET_GLOBAL);

  IF_TRACE(printf("  (set-global %d)\n", bytecode_lo4));

  set_global (bytecode_lo4, POP());

  DISPATCH();

  /***************************************************************************/
  CASE(CALL);

  IF_TRACE(printf("  (call %d)\n", bytecode_lo4));

  na = bytecode_lo4;

  pop_procedure ();
  handle_arity_and_rest_param ();
  build_env ();
  save_cont ();

  env = arg1;
  pc = entry;

  arg1 = OBJ_FALSE;

  DISPATCH();

  /***************************************************************************/
  CASE(JUMP);

  IF_TRACE(printf("  (jump %d)\n", bytecode_lo4));

  na = bytecode_lo4;

  pop_procedure ();
  handle_arity_and_rest_param ();
  build_env ();

  env = arg1;
  pc = entry;

  arg1 = OBJ_FALSE;

  DISPATCH();

  /***************************************************************************/
  CASE(LABEL_INSTR);

  switch (bytecode_lo4)
    {
    case 0: // call-toplevel
      FETCH_NEXT_BYTECODE();  
      arg2 = bytecode;
      
      FETCH_NEXT_BYTECODE();
      
      IF_TRACE(printf("  (call-toplevel 0x%04x)\n",
		      ((arg2 << 8) | bytecode) + CODE_START));
      
      entry = (arg2 << 8) + bytecode + CODE_START;
      arg1 = OBJ_NULL;
      
      na = rom_get (entry++);
      
      build_env ();
      save_cont ();

      env = arg1;
      pc = entry;
      
      arg1 = OBJ_FALSE;
      arg2 = OBJ_FALSE;
      
      break;
      
    case 1: // jump-toplevel
      FETCH_NEXT_BYTECODE();  
      arg2 = bytecode;
      
      FETCH_NEXT_BYTECODE();
      
      IF_TRACE(printf("  (jump-toplevel 0x%04x)\n",
		      ((arg2 << 8) | bytecode) + CODE_START));
      
      entry = (arg2 << 8) + bytecode + CODE_START;
      arg1 = OBJ_NULL;
      
      na = rom_get (entry++);
      
      build_env ();
      
      env = arg1;
      pc = entry;
      
      arg1 = OBJ_FALSE;
      arg2 = OBJ_FALSE;
      
      break;
      
    case 2: // goto
      FETCH_NEXT_BYTECODE();
      arg2 = bytecode;
      
      FETCH_NEXT_BYTECODE();
      
      IF_TRACE(printf("  (goto 0x%04x)\n",
		      (arg2 << 8) + bytecode + CODE_START));
  
      pc = (arg2 << 8) + bytecode + CODE_START;
      
      break;

    case 3: // goto-if-false
      FETCH_NEXT_BYTECODE();
      arg2 = bytecode;
      
      FETCH_NEXT_BYTECODE();
      
      IF_TRACE(printf("  (goto-if-false 0x%04x)\n",
		      (arg2 << 8) + bytecode + CODE_START));
      
      if (POP() == OBJ_FALSE)
	pc = (arg2 << 8) + bytecode + CODE_START;

      break;

    case 4: // closure
      FETCH_NEXT_BYTECODE();
      arg2 = bytecode;
      
      FETCH_NEXT_BYTECODE();
      
      IF_TRACE(printf("  (closure 0x%04x)\n", (arg2 << 8) | bytecode));
      
      arg3 = POP(); // env
      
      entry = (arg2 << 8) | bytecode;
      
      arg1 = alloc_ram_cell_init (CLOSURE_FIELD0 | (arg2 >> 3),
				  ((arg2 & 0x07) << 5) | (bytecode >> 3),
				  ((bytecode &0x07) <<5) |((arg3 &0x1f00) >>8),
				  arg3 & 0xff);
      
      PUSH_ARG1();
      
      arg2 = OBJ_FALSE;
      arg3 = OBJ_FALSE;
      
      break;

    case 5: // call-toplevel-short
      FETCH_NEXT_BYTECODE(); // TODO the short version have a lot in common with the long ones, abstract ?
      // TODO short instructions don't work at the moment
      IF_TRACE(printf("  (call-toplevel-short 0x%04x)\n",
		      pc + bytecode + CODE_START));
      
      entry = pc + bytecode + CODE_START;
      arg1 = OBJ_NULL;
      
      na = rom_get (entry++);
      
      build_env ();
      save_cont ();

      env = arg1;
      pc = entry;
      
      arg1 = OBJ_FALSE;
      
      break;
      
    case 6: // jump-toplevel-short
      FETCH_NEXT_BYTECODE();
      
      IF_TRACE(printf("  (jump-toplevel-short 0x%04x)\n",
		      pc + bytecode + CODE_START));
      
      entry = pc + bytecode + CODE_START;
      arg1 = OBJ_NULL;
      
      na = rom_get (entry++);
      
      build_env ();
      
      env = arg1;
      pc = entry;
      
      arg1 = OBJ_FALSE;
      
      break;
      
    case 7: // goto-short
      FETCH_NEXT_BYTECODE();
      
      IF_TRACE(printf("  (goto-short 0x%04x)\n", pc + bytecode + CODE_START));
  
      pc = pc + bytecode + CODE_START;
      
      break;
      
    case 8: // goto-if-false-short
      FETCH_NEXT_BYTECODE();
      
      IF_TRACE(printf("  (goto-if-false-short 0x%04x)\n",
		      pc + bytecode + CODE_START));
      
      if (POP() == OBJ_FALSE)
	pc = pc + bytecode + CODE_START;
      
      break;
      
    case 9: // closure-short
      FETCH_NEXT_BYTECODE();
      
      IF_TRACE(printf("  (closure-short 0x%04x)\n", pc + bytecode));
      
      arg3 = POP(); // env
      
      entry = pc + bytecode;
      
      arg1 = alloc_ram_cell_init (CLOSURE_FIELD0 | (arg2 >> 3),
				  ((arg2 & 0x07) << 5) | (bytecode >> 3),
				  ((bytecode &0x07) <<5) |((arg3 &0x1f00) >>8),
				  arg3 & 0xff);
      
      PUSH_ARG1();
      
      arg3 = OBJ_FALSE;
      
      break;
      
#if 0
    case 10:
      break;
    case 11:
      break;
    case 12:
      break;
    case 13:
      break;
#endif
    case 14: // push_global [long]
      FETCH_NEXT_BYTECODE();
      
      IF_TRACE(printf("  (push-global [long] %d)\n", bytecode));
      
      arg1 = get_global (bytecode);
      
      PUSH_ARG1();
      
      break;
      
    case 15: // set_global [long]
      FETCH_NEXT_BYTECODE();
      
      IF_TRACE(printf("  (set-global [long] %d)\n", bytecode));
      
      set_global (bytecode, POP());
      
      break;
    }

  DISPATCH();

  /***************************************************************************/
  CASE(PUSH_CONSTANT_LONG);

  /* push-constant [long] */

  FETCH_NEXT_BYTECODE();

  IF_TRACE(printf("  (push [long] 0x%04x)\n", (bytecode_lo4 << 8) + bytecode));

  arg1 = (bytecode_lo4 << 8) | bytecode;
  PUSH_ARG1();
  
  DISPATCH();

  /***************************************************************************/
  CASE(FREE1); // FREE

  DISPATCH();

  /***************************************************************************/
  CASE(FREE2); // FREE

  DISPATCH();

  /***************************************************************************/
  CASE(PRIM1);

  IF_TRACE(printf("  (%s)\n", prim_name[bytecode_lo4]));

  switch (bytecode_lo4)
    {
    case 0:
      arg1 = POP();  prim_numberp ();  PUSH_ARG1();  break;
    case 1:
      arg2 = POP();  arg1 = POP();  prim_add ();      PUSH_ARG1();  break;
    case 2:
      arg2 = POP();  arg1 = POP();  prim_sub ();      PUSH_ARG1();  break;
    case 3:
      arg2 = POP();  arg1 = POP();  prim_mul ();      PUSH_ARG1();  break;
    case 4:
      arg2 = POP();  arg1 = POP();  prim_div ();      PUSH_ARG1();  break;
    case 5:
      arg2 = POP();  arg1 = POP();  prim_rem ();      PUSH_ARG1();  break;
    case 6:
      arg1 = POP();  prim_neg ();      PUSH_ARG1();  break;
    case 7:
      arg2 = POP();  arg1 = POP();  prim_eq ();       PUSH_ARG1();  break;
    case 8:
      arg2 = POP();  arg1 = POP();  prim_lt ();       PUSH_ARG1();  break;
    case 9:
      arg2 = POP(); arg1 = POP(); prim_ior (); PUSH_ARG1(); break;
    case 10:
      arg2 = POP();  arg1 = POP();  prim_gt ();       PUSH_ARG1();  break;
    case 11:
      arg2 = POP(); arg1 = POP(); prim_xor (); PUSH_ARG1(); break;
    case 12:
      arg1 = POP();  prim_pairp ();    PUSH_ARG1();  break;
    case 13:
      arg2 = POP();  arg1 = POP();  prim_cons ();     PUSH_ARG1();  break;
    case 14:
      arg1 = POP();  prim_car ();      PUSH_ARG1();  break;
    case 15:
      arg1 = POP();  prim_cdr ();      PUSH_ARG1();  break;
    }

  DISPATCH();

  /***************************************************************************/
  CASE(PRIM2);

  IF_TRACE(printf("  (%s)\n", prim_name[bytecode_lo4+16]));

  switch (bytecode_lo4)
    {
    case 0:
      arg2 = POP();  arg1 = POP();  prim_set_car ();                break;
    case 1:
      arg2 = POP();  arg1 = POP();  prim_set_cdr ();                break;
    case 2:
      arg1 = POP();  prim_nullp ();    PUSH_ARG1();  break;
    case 3:
      arg2 = POP();  arg1 = POP();  prim_eqp ();      PUSH_ARG1();  break;
    case 4:
      arg1 = POP();  prim_not ();      PUSH_ARG1();  break;
    case 5:
      /* prim #%get-cont */
      arg1 = cont;
      PUSH_ARG1();
      break;
    case 6:
      /* prim #%graft-to-cont */

      arg1 = POP(); /* thunk to call */
      cont = POP(); /* continuation */

      PUSH_ARG1();

      na = 0;

      pop_procedure ();
      handle_arity_and_rest_param ();
      build_env ();

      env = arg1;
      pc = entry;

      arg1 = OBJ_FALSE;

      break;
    case 7:
      /* prim #%return-to-cont */

      arg1 = POP(); /* value to return */
      cont = POP(); /* continuation */

      arg2 = ram_get_cdr (cont);
      
      pc = ram_get_entry (arg2);

      env = ram_get_cdr (arg2);
      cont = ram_get_car (cont);

      PUSH_ARG1();
      arg2 = OBJ_FALSE;

      break;
    case 8:
      /* prim #%halt */
      return;
    case 9:
      /* prim #%symbol? */
      arg1 = POP();  prim_symbolp ();  PUSH_ARG1();  break;
    case 10:
      /* prim #%string? */
      arg1 = POP();  prim_stringp ();  PUSH_ARG1();  break;
    case 11:
      /* prim #%string->list */
      arg1 = POP();  prim_string2list ();  PUSH_ARG1();  break;
    case 12:
      /* prim #%list->string */
      arg1 = POP();  prim_list2string ();  PUSH_ARG1();  break;
    case 13:
      /* prim #%make-u8vector */
      arg2 = POP(); arg1 = POP(); prim_make_u8vector (); PUSH_ARG1(); break;
    case 14:
      /* prim #%u8vector-ref */
      arg2 = POP(); arg1 = POP(); prim_u8vector_ref (); PUSH_ARG1(); break;
    case 15:
      /* prim #%u8vector-set! */
      arg3 = POP(); arg2 = POP(); arg1 = POP(); prim_u8vector_set (); break;
    }

  DISPATCH();

  /***************************************************************************/
  CASE(PRIM3);

  IF_TRACE(printf("  (%s)\n", prim_name[bytecode_lo4+32]));

  switch (bytecode_lo4)
    {
    case 0:
      /* prim #%print */
      arg1 = POP();
      prim_print ();
      break;
    case 1:
      /* prim #%clock */
      prim_clock ();  PUSH_ARG1();  break;
    case 2:
      /* prim #%motor */
      arg2 = POP();  arg1 = POP();  prim_motor ();  break;
    case 3:
      /* prim #%led */
      arg3 = POP();  arg2 = POP();  arg1 = POP();  prim_led ();  ;break;
    case 4:
      /* prim #%led2-color */
      arg1 = POP();  prim_led2_color ();  break;
    case 5:
      /* prim #%getchar-wait */
      arg2 = POP();  arg1 = POP();  prim_getchar_wait ();  PUSH_ARG1();  break;
    case 6:
      /* prim #%putchar */
      arg2 = POP();  arg1 = POP();  prim_putchar ();  break;
    case 7:
      /* prim #%beep */
      arg2 = POP();  arg1 = POP();  prim_beep ();  break;
    case 8:
      /* prim #%adc */
      arg1 = POP();  prim_adc ();  PUSH_ARG1();  break;
    case 9:
      /* prim #%u8vector? */
      arg1 = POP(); prim_u8vectorp (); PUSH_ARG1(); break;
    case 10:
      /* prim #%sernum */
      prim_sernum ();  PUSH_ARG1();  break;
    case 11:
      /* prim #%u8vector-length */
      arg1 = POP(); prim_u8vector_length (); PUSH_ARG1(); break;
    case 12:
      /* prim #%u8vector-copy! */
      arg5 = POP(); arg4 = POP(); arg3 = POP(); arg2 = POP(); arg1 = POP();
      prim_u8vector_copy (); break;
      break;
    case 13:
      /* shift */
      arg1 = POP();
      POP();
      PUSH_ARG1();
      break;
    case 14:
      /* pop */
      POP();
      break;
    case 15:
      /* return */
      arg1 = POP();
      arg2 = ram_get_cdr (cont);
      pc = ram_get_entry (arg2);
      env = ram_get_cdr (arg2);
      cont = ram_get_car (cont);
      PUSH_ARG1();
      arg2 = OBJ_FALSE;
      break;
    }

  DISPATCH();

  /***************************************************************************/

  CASE(PRIM4);

  IF_TRACE(printf("  (%s)\n", prim_name[bytecode_lo4]));
  
  switch (bytecode_lo4)
    {
    case 0:
      /* prim #%boolean? */
      arg1 = POP(); prim_booleanp (); PUSH_ARG1(); break;
    case 1:
      /* prim #%network-init */
      prim_network_init (); break;
    case 2:
      /* prim #%network-cleanup */
      prim_network_cleanup (); break;
    case 3:
      /* prim #%receive-packet-to-u8vector */
      arg1 = POP(); prim_receive_packet_to_u8vector (); PUSH_ARG1(); break;
    case 4:
      /* prim #%send-packet-from-u8vector */
      arg2 = POP(); arg1 = POP(); prim_send_packet_from_u8vector ();
      PUSH_ARG1(); break;
    case 5:
      break;
    case 6:
      break;
    case 7:
      break;
    case 8:
      break;
    case 9:
      break;
    case 10:
      break;
    case 11:
      break;
    case 12:
      break;
    case 13:
      break;
    case 14:
      break;
    case 15:
      break;
    }
  
  DISPATCH();

  /***************************************************************************/
  
  END_DISPATCH();
}

/*---------------------------------------------------------------------------*/

#ifdef WORKSTATION

void usage (void)
{
  printf ("usage: sim file.hex\n");
  exit (1);
}

int main (int argc, char *argv[])
{
  int errcode = 1;
  rom_addr rom_start_addr = 0;

  test(); // TODO arithmetic test
  
  if (argc > 1 && argv[1][0] == '-' && argv[1][1] == 's')
    {
      int h1;
      int h2;
      int h3;
      int h4;

      if ((h1 = hex (argv[1][2])) < 0 ||
          (h2 = hex (argv[1][3])) < 0 ||
          (h3 = hex (argv[1][4])) != 0 ||
          (h4 = hex (argv[1][5])) != 0 ||
          argv[1][6] != '\0')
        usage ();

      rom_start_addr = (h1 << 12) | (h2 << 8) | (h3 << 4) | h4;

      argv++;
      argc--;
    }

#ifdef DEBUG
  printf ("Start address = 0x%04x\n", rom_start_addr + CODE_START);
#endif

  if (argc != 2)
    usage ();

  if (!read_hex_file (argv[1]))
    printf ("*** Could not read hex file \"%s\"\n", argv[1]);
  else
    {
      int i;

      if (rom_get (CODE_START+0) != 0xfb ||
          rom_get (CODE_START+1) != 0xd7)
        printf ("*** The hex file was not compiled with PICOBIT\n");
      else
        {
#if 0
          for (i=0; i<8192; i++)
            if (rom_get (i) != 0xff)
              printf ("rom_mem[0x%04x] = 0x%02x\n", i, rom_get (i));
#endif

          interpreter ();

#ifdef DEBUG_GC
          printf ("**************** memory needed = %d\n", max_live+1);
#endif
        }
    }

  return errcode;
}

#endif

/*---------------------------------------------------------------------------*/
