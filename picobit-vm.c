/* file: "picobit-vm.c" */

/*
 * Copyright 2004 by Marc Feeley, All Rights Reserved.
 *
 * History:
 *
 *   15/08/2004  Release of version 1
 *   6/07/2008  Modified for PICOBOARD2_R3
 */

#define DEBUG_not
#define DEBUG_GC_not

/*---------------------------------------------------------------------------*/

typedef char int8;
typedef short int16;
typedef long int32;
typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned long uint32;

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

#define GLOVARS 16

#ifdef DEBUG
#define IF_TRACE(x) x
#define IF_GC_TRACE(x)
#else
#define IF_TRACE(x)
#define IF_GC_TRACE(x)
#endif

/*---------------------------------------------------------------------------*/


#ifdef PICOBOARD2

#define ERROR(msg) halt_with_error()
#define TYPE_ERROR(type) halt_with_error()

#endif


#ifdef WORKSTATION

#define ERROR(msg) error (msg)
#define TYPE_ERROR(type) type_error (type)

void error (char *msg)
{
  printf ("ERROR: %s\n", msg);
  exit (1);
}

void type_error (char *type)
{
  printf ("ERROR: An argument of type %s was expected\n", type);
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

/*---------------------------------------------------------------------------*/

#define MIN_RAM_ENCODING 128
#define MAX_RAM_ENCODING 8192
// TODO some space in rom is not used, use for fixnums ?
#define RAM_BYTES ((MAX_RAM_ENCODING - MIN_RAM_ENCODING + 1)*4)

// TODO change if we change the proportion of rom and ram addresses
#if WORD_BITS == 8
#define OBJ_TO_RAM_ADDR(o,f) (((ram_addr)((uint16)(o) - MIN_RAM_ENCODING) << 2) + (f))
#define OBJ_TO_ROM_ADDR(o,f) (((rom_addr)((uint8)(o) - MIN_ROM_ENCODING) << 2) + (CODE_START + 4 + (f)))
#endif


#ifdef PICOBOARD2

#if 0
#pragma udata picobit_heap=0x200
uint8 ram_mem[RAM_BYTES];
#pragma udata
#endif

#define ram_get(a) *(uint8*)(a+0x200)
#define ram_set(a,x) *(uint8*)(a+0x200) = (x)

#endif


#ifdef WORKSTATION

uint8 ram_mem[RAM_BYTES];

#define ram_get(a) ram_mem[a]
#define ram_set(a,x) ram_mem[a] = (x)

#endif


/*---------------------------------------------------------------------------*/

#ifdef PICOBOARD2

#if WORD_BITS == 8
#endif

uint8 rom_get (rom_addr a)
{
  return *(rom uint8*)a;
}

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

uint8 rom_get (rom_addr a)
{
  return rom_mem[a-CODE_START];
}

#endif

obj globals[GLOVARS];

/*---------------------------------------------------------------------------*/

/*
  OBJECT ENCODING:

  #f           0
  #t           1
  ()           2
  fixnum n     MIN_FIXNUM -> 3 ... MAX_FIXNUM -> 3 + (MAX_FIXNUM-MIN_FIXNUM)
  TODO do we want 0..127 as fixnums ? would reduce number of ra/om objects
  rom object   4 + (MAX_FIXNUM-MIN_FIXNUM) ... MIN_RAM_ENCODING-1
  ram object   MIN_RAM_ENCODING ... 4095 TODO was 255, now we have 12 bits

  layout of memory allocated objects:

  G's represent mark bits used by the gc TODO change GC, and does not use the same bits

  bignum n     00G00000 uuuuuuuu hhhhhhhh llllllll  (24 bit signed integer)
  TODO we could have 29-bit integers
  
  pair         1GGaaaaa aaaaaaaa 000ddddd dddddddd
  TODO was 00000010 aaaaaaaa aaaadddd dddddddd
  a is car
  d is cdr
  gives an address space of 2^13 * 4 = 32k (not all of it is for RAM, though)

  symbol       1GG00000 00000000 00100000 00000000

  string       1GG***** *chars** 01000000 00000000

  vector       1GG***** *elems** 01100000 00000000 TODO not used yet

  closure      01Gxxxxx xxxxxxxx aaaaaaaa aaaaaaaa TODO OLD
  closure      01Gaaaaa aaaaaaaa aaaxxxxx xxxxxxxx
  0x5ff<a<0x4000 is entry
  x is pointer to environment
  the reason why the environment is on the cdr (and the entry is split on 3
  bytes) is that, when looking for a variable, a closure is considered to be a
  pair. The compiler adds an extra offset to any variable in the closure's
  environment, so the car of the closure (which doesn't really exist) is never
  checked, but the cdr is followed to find the other bindings
  
  continuation 01Gxxxxx xxxxxxxx aaaaaaaa aaaaaaaa  0x5ff<a<0x4000 is pc TODO old
  continuation 01Gxxxxx xxxxxxxx 100yyyyy yyyyyyyy
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
#define OBJ_NULL  2

#define MIN_FIXNUM_ENCODING 3
#define MIN_FIXNUM (-5)
#define MAX_FIXNUM 40
#define MIN_ROM_ENCODING (MIN_FIXNUM_ENCODING+MAX_FIXNUM-MIN_FIXNUM+1)

#define ENCODE_FIXNUM(n) ((obj)(n) + (MIN_FIXNUM_ENCODING - MIN_FIXNUM))
#define DECODE_FIXNUM(o) ((int32)(o) - (MIN_FIXNUM_ENCODING - MIN_FIXNUM))

#if WORD_BITS == 8
#define IN_RAM(o) ((o) >= MIN_RAM_ENCODING)
#define IN_ROM(o) ((int8)(o) >= MIN_ROM_ENCODING)
#endif

// bignum first byte : 00G00000
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
#endif

#if WORD_BITS == 10
#define RAM_GET_FIELD1_MACRO(o)                                         \
  (ram_get (OBJ_TO_RAM_ADDR(o,1)) + ((RAM_GET_FIELD0_MACRO(o) & 0x03)<<8))
#define RAM_GET_FIELD2_MACRO(o)                                         \
  (ram_get (OBJ_TO_RAM_ADDR(o,2)) + ((RAM_GET_FIELD0_MACRO(o) & 0x0c)<<6))
#define RAM_GET_FIELD3_MACRO(o)                                         \
  (ram_get (OBJ_TO_RAM_ADDR(o,3)) + ((RAM_GET_FIELD0_MACRO(o) & 0x30)<<4))
#define RAM_SET_FIELD1_MACRO(o,val)                                     \
  do {                                                                  \
    ram_set (OBJ_TO_RAM_ADDR(o,1), (val) & 0xff);                       \
    RAM_SET_FIELD0_MACRO(o,(RAM_GET_FIELD0_MACRO(o) & 0xfc) + (((val) >> 8) & 0x03)); \
  } while (0)
#define RAM_SET_FIELD2_MACRO(o,val)                                     \
  do {                                                                  \
    ram_set (OBJ_TO_RAM_ADDR(o,2), (val) & 0xff);                       \
    RAM_SET_FIELD0_MACRO(o,(RAM_GET_FIELD0_MACRO(o) & 0xf3) + (((val) >> 6) & 0x0c)); \
  } while (0)
#define RAM_SET_FIELD3_MACRO(o,val)                                     \
  do {                                                                  \
    ram_set (OBJ_TO_RAM_ADDR(o,3), (val) & 0xff);                       \
    RAM_SET_FIELD0_MACRO(o,(RAM_GET_FIELD0_MACRO(o) & 0xcf) + (((val) >> 4) & 0x30)); \
  } while (0)
#define ROM_GET_FIELD1_MACRO(o)                                         \
  (rom_get (OBJ_TO_ROM_ADDR(o,1)) + ((ROM_GET_FIELD0_MACRO(o) & 0x03)<<8))
#define ROM_GET_FIELD2_MACRO(o)                                         \
  (rom_get (OBJ_TO_ROM_ADDR(o,2)) + ((ROM_GET_FIELD0_MACRO(o) & 0x0c)<<6))
#define ROM_GET_FIELD3_MACRO(o)                                         \
  (rom_get (OBJ_TO_ROM_ADDR(o,3)) + ((ROM_GET_FIELD0_MACRO(o) & 0x30)<<4))
#endif

uint8 ram_get_gc_tags (obj o) { return RAM_GET_GC_TAGS_MACRO(o); }
void ram_set_gc_tags  (obj o, uint8 tags) { RAM_SET_GC_TAGS_MACRO(o, tags); }
void ram_set_gc_tag0 (obj o, uint8 tag) { RAM_SET_GC_TAG0_MACRO(o,tag); }
void ram_set_gc_tag1 (obj o, uint8 tag) { RAM_SET_GC_TAG1_MACRO(o,tag); }
uint8 ram_get_field0 (obj o) { return RAM_GET_FIELD0_MACRO(o); }
word ram_get_field1 (obj o) { return RAM_GET_FIELD1_MACRO(o); } // TODO used to return obj, which used to be the same as words
word ram_get_field2 (obj o) { return RAM_GET_FIELD2_MACRO(o); }
word ram_get_field3 (obj o) { return RAM_GET_FIELD3_MACRO(o); }
void ram_set_field0 (obj o, uint8 val) { RAM_SET_FIELD0_MACRO(o,val); }
void ram_set_field1 (obj o, word val) { RAM_SET_FIELD1_MACRO(o,val); }
void ram_set_field2 (obj o, word val) { RAM_SET_FIELD2_MACRO(o,val); }
void ram_set_field3 (obj o, word val) { RAM_SET_FIELD3_MACRO(o,val); }
uint8 rom_get_field0 (obj o) { return ROM_GET_FIELD0_MACRO(o); }
word rom_get_field1 (obj o) { return ROM_GET_FIELD1_MACRO(o); }
word rom_get_field2 (obj o) { return ROM_GET_FIELD2_MACRO(o); }
word rom_get_field3 (obj o) { return ROM_GET_FIELD3_MACRO(o); }

obj ram_get_car (obj o)
{ return ((ram_get_field0 (o) & 0x1f) << 8) | ram_get_field1 (o); }
obj rom_get_car (obj o)
{ return ((rom_get_field0 (o) & 0x1f) << 8) | rom_get_field1 (o); }
obj ram_get_cdr (obj o)
{ return ((ram_get_field2 (o) & 0x1f) << 8) | ram_get_field3 (o); }
obj rom_get_cdr (obj o)
{ return ((rom_get_field2 (o) & 0x1f) << 8) | rom_get_field3 (o); }
void ram_set_car (obj o, obj val)
{
  ram_set_field0 (o, ((val & 0x1f00) >> 8) | (ram_get_field0 (o) & 0xc0));
  ram_set_field1 (o, val & 0xff);
}
void ram_set_cdr (obj o, obj val)
{
  ram_set_field2 (o, ((val & 0x1f00) >> 8) | (ram_get_field2 (o) & 0xc0));
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

obj get_global (uint8 i)
{
  return globals[i];
}

void set_global (uint8 i, obj o)
{
  globals[i] = o;
}

/*---------------------------------------------------------------------------*/

/* Interface to GC */

/* GC tags are in the top 2 bits of field 0 */
#define GC_TAG_0_LEFT   (1<<5)
// TODO was 3<<5, changed to play nice with procedures and bignums, but should actually set only this bit, not clear the other
#define GC_TAG_1_LEFT   (2<<5)
#define GC_TAG_UNMARKED (0<<5)  /* must be 0 */ // TODO FOOBAR is it ok ? eevn for bignums ?

/* Number of object fields of objects in ram */
#define HAS_2_OBJECT_FIELDS(visit) (RAM_PAIR(visit) || RAM_CONTINUATION(visit))
#define HAS_1_OBJECT_FIELD(visit)  (RAM_COMPOSITE(visit) || RAM_CLOSURE(visit))
// TODO now we consider that all composites have at least 1 field, even symbols, as do procedures. no problem for symbols, since the car is always #f (make sure)
// TODO if we ever have true bignums, bignums will have 1 object field

#define NIL OBJ_FALSE

/*---------------------------------------------------------------------------*/

/* Garbage collector */

obj free_list; /* list of unused cells */

obj arg1; /* root set */
obj arg2;
obj arg3;
obj arg4;
obj cont;
obj env;

uint8 na; /* interpreter variables */ // TODO what's na ?
rom_addr pc;
rom_addr entry;
uint8 bytecode;
uint8 bytecode_hi4;
uint8 bytecode_lo4;
obj second_half; /* the second half of continuations */
int32 a1;
int32 a2;
int32 a3;

void init_ram_heap (void)
{
  uint8 i;
  obj o = MAX_RAM_ENCODING;

  free_list = 0;

  while (o >= MIN_RAM_ENCODING)
    {
      ram_set_gc_tags (o, GC_TAG_UNMARKED);
      ram_set_car (o, free_list);
      free_list = o;
      o--;
    }

  for (i=0; i<GLOVARS; i++)
    set_global (i, OBJ_FALSE);

  arg1 = OBJ_FALSE;
  arg2 = OBJ_FALSE;
  arg3 = OBJ_FALSE;
  arg4 = OBJ_FALSE;
  cont = OBJ_FALSE;
  env  = OBJ_NULL;
  second_half = OBJ_FALSE;
}

void mark (obj temp)
{  
  /* mark phase */
  
  obj stack; // TODO do we need a stack ? since we have 0-1-2 children, we could do deutsche schorr waite
  obj visit;

  if (IN_RAM(temp))
    {
      visit = NIL;

    push:

      stack = visit;
      visit = temp;

      IF_GC_TRACE(printf ("push   stack=%d (tag=%d)  visit=%d (tag=%d)\n", stack, ram_get_gc_tags (stack)>>6, visit, ram_get_gc_tags (visit)>>6));

      /*
       * Four cases are possible:
       *
       * A)
       *                          stack  visit tag F1  F2  F3
       *                           NIL    |   +---+---+---+---+
       *                                  +-> | ? |   |   |   |
       *                                      +---+---+---+---+
       *
       * B)
       *          tag F1  F2  F3  stack  visit tag F1  F2  F3
       *         +---+---+---+---+   |    |   +---+---+---+---+
       *         | 1 |   |   |   | <-+    +-> | ? |   |   |   |
       *         +---+---+---+-|-+            +---+---+---+---+
       *     <-----------------+
       *
       * C)
       *          tag F1  F2  F3  stack  visit tag F1  F2  F3
       *         +---+---+---+---+   |    |   +---+---+---+---+
       *         | 2 |   |   |   | <-+    +-> | ? |   |   |   |
       *         +---+---+-|-+---+            +---+---+---+---+
       *     <-------------+
       *
       * D)
       *          tag F1  F2  F3  stack  visit tag F1  F2  F3
       *         +---+---+---+---+   |    |   +---+---+---+---+
       *         | 3 |   |   |   | <-+    +-> | ? |   |   |   |
       *         +---+-|-+---+---+            +---+---+---+---+
       *     <---------+
       */
      // TODO since no-one has 3 fields anymore, not really 4 cases ?
      
      //      if (ram_get_gc_tags (visit) != GC_TAG_UNMARKED) // TODO always matches procedures, WRONG, maybe check only the right gc bit ?/
      if (ram_get_gc_tags (visit) & 0x2f) // TODO we check only the last gc bit
	IF_GC_TRACE(printf ("case 1\n")); // TODO are there cases where checking only the last gc bit is wrong ?
      // TODO FOOBAR ok, with our new way, what do we check here ?
      else
        {
          if (HAS_2_OBJECT_FIELDS(visit))
            {
              IF_GC_TRACE(printf ("case 5\n"));
	      // TODO we don't have cases 2-4 anymore

            visit_field2:

              temp = ram_get_cdr (visit);

              if (IN_RAM(temp))
                {
                  IF_GC_TRACE(printf ("case 6\n"));
                  ram_set_gc_tags (visit, GC_TAG_1_LEFT);
                  ram_set_cdr (visit, stack);
                  goto push;
                }

              IF_GC_TRACE(printf ("case 7\n"));

              goto visit_field1;
            }

          if (HAS_1_OBJECT_FIELD(visit))
            {
              IF_GC_TRACE(printf ("case 8\n"));

            visit_field1:

              temp = ram_get_car (visit);

              if (IN_RAM(temp))
                {
                  IF_GC_TRACE(printf ("case 9\n"));
                  ram_set_gc_tag0 (visit, GC_TAG_0_LEFT); // TODO changed, now we only set bit 0, we don't change bit 1, since some objets have only 1 mark bit
                  ram_set_car (visit, stack);
                  goto push;
                }

              IF_GC_TRACE(printf ("case 10\n"));
            }
          else
            IF_GC_TRACE(printf ("case 11\n"));

          ram_set_gc_tag0 (visit, GC_TAG_0_LEFT); // TODO changed, same as above
        }

    pop:

      IF_GC_TRACE(printf ("pop    stack=%d (tag=%d)  visit=%d (tag=%d)\n", stack, ram_get_gc_tags (stack)>>6, visit, ram_get_gc_tags (visit)>>6));

      if (stack != NIL)
        {
          if (ram_get_gc_tags (stack) == GC_TAG_1_LEFT) // TODO FOOBAR, this is always true for closures that have not been marked, can such an object get here ? probably not, since when a procedure is popped, it has already been visited, so will be at 0 left
            {
              IF_GC_TRACE(printf ("case 13\n"));

              temp = ram_get_cdr (stack);  /* pop through field 2 */
              ram_set_cdr (stack, visit);
              visit = stack;
              stack = temp;

              goto visit_field1;
            }

          IF_GC_TRACE(printf ("case 14\n"));

          temp = ram_get_car (stack);  /* pop through field 1 */
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

  while (visit >= MIN_RAM_ENCODING)
    {
      if ((RAM_COMPOSITE(visit) && (ram_get_gc_tags (visit) == GC_TAG_UNMARKED)) || (ram_get_gc_tags (visit) & GC_TAG_0_LEFT)) /* unmarked? */
	// TODO now we check only 1 bit if the object has only 1 mark bit
        {
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

  mark (arg1);
  mark (arg2);
  mark (arg3);
  mark (arg4);
  mark (cont);
  mark (env);

  for (i=0; i<GLOVARS; i++)
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
        ERROR("memory is full");
    }

  o = free_list;

  free_list = ram_get_field1 (o);

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

/*---------------------------------------------------------------------------*/

int32 decode_int (obj o)
{
  uint8 u;
  uint8 h;
  uint8 l;

  if (o < MIN_FIXNUM_ENCODING)
    TYPE_ERROR("integer");

  if (o <= (MIN_FIXNUM_ENCODING + (MAX_FIXNUM - MIN_FIXNUM)))
    return DECODE_FIXNUM(o);

  if (IN_RAM(o))
    {
      if (!RAM_BIGNUM(o))
        TYPE_ERROR("integer");

      u = ram_get_field1 (o);
      h = ram_get_field2 (o);
      l = ram_get_field3 (o);
    }
  else if (IN_ROM(o))
    {
      if (!ROM_BIGNUM(o))
        TYPE_ERROR("integer");

      u = rom_get_field1 (o);
      h = rom_get_field2 (o);
      l = rom_get_field3 (o);
    }
  else
    TYPE_ERROR("integer");

  if (u >= 128)
    return ((int32)((((int16)u - 256) << 8) + h) << 8) + l;

  return ((int32)(((int16)u << 8) + h) << 8) + l;
}

obj encode_int (int32 n)
{
  if (n >= MIN_FIXNUM && n <= MAX_FIXNUM)
    return ENCODE_FIXNUM(n);

  return alloc_ram_cell_init (BIGNUM_FIELD0, n >> 16, n >> 8, n);
}

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

      if ((in_ram && RAM_BIGNUM(o)) || (!in_ram && ROM_BIGNUM(o)))
	printf ("%d", decode_int (o));
      else if ((in_ram && RAM_COMPOSITE(o)) || (!in_ram && ROM_COMPOSITE(o)))
        {
	  obj car;
	  obj cdr;

	  if (in_ram && RAM_PAIR(o))
	    {
	      car = ram_get_car (o);
	      cdr = ram_get_cdr (o);
	      printf ("(");

	    loop_ram:
	      show (car);

	      if (cdr == OBJ_NULL)
		printf (")");
	      else if (RAM_PAIR(cdr))
		{
		  car = ram_get_car (cdr);
		  cdr = ram_get_cdr (cdr);

		  printf (" ");
		  goto loop_ram;
		}
	      else
		{
		  printf (" . ");
		  show (cdr);
		  printf (")");
		}
	    }
	  else if (!in_ram && ROM_PAIR(o))
	    {
	      car = rom_get_car (o);
	      cdr = rom_get_cdr (o);
	      printf ("(");
	    loop_rom:
	      show (car);

	      if (cdr == OBJ_NULL)
		printf (")");
	      else if (ROM_PAIR(cdr))
		{
		  car = rom_get_car (cdr);
		  cdr = rom_get_cdr (cdr);

		  printf (" ");
		  goto loop_rom;
		}
	      else // TODO lots of repetition
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
	    printf ("#<vector>");
	  else
	    {
	      printf ("(");
	      car = ram_get_car (o);
	      cdr = ram_get_cdr (o);
	      goto loop_ram; // TODO ugly hack, takes advantage of the fact that pairs and continuations have the same layout
	    }
        }
      else // closure
        {
          obj env;
          /* obj parent_cont; */
          rom_addr pc;

          if (IN_RAM(o)) // TODO can closures be in rom ? I don't think so
            env = ram_get_cdr (o); // TODO was car, but representation changed
          else
            env = rom_get_cdr (o);

          /* if (IN_RAM(o)) */
          /*   parent_cont = ram_get_field2 (o); */
          /* else */
          /*   parent_cont = rom_get_field2 (o); */

          if (IN_RAM(o))
	    pc = ram_get_entry (o);
          else
	    pc = rom_get_entry (o);

          printf ("{0x%04x ", pc);
          show (env);
          /* printf (" "); */
          /* show (parent_cont); */
          printf ("}");
	  /* printf ("#<procedure>"); */
        }
    }

  fflush (stdout);
}

void show_state (rom_addr pc)
{
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

#define encode_bool(x) ((obj)(x))

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

void decode_2_int_args (void)
{
  a1 = decode_int (arg1);
  a2 = decode_int (arg2);
}

void prim_add (void)
{
  decode_2_int_args ();
  arg1 = encode_int (a1 + a2);
  arg2 = OBJ_FALSE;
}

void prim_sub (void)
{
  decode_2_int_args ();
  arg1 = encode_int (a1 - a2);
  arg2 = OBJ_FALSE;
}

void prim_mul (void)
{
  decode_2_int_args ();
  arg1 = encode_int (a1 * a2);
  arg2 = OBJ_FALSE;
}

void prim_div (void)
{
  decode_2_int_args ();
  if (a2 == 0)
    ERROR("divide by 0");
  arg1 = encode_int (a1 / a2);
  arg2 = OBJ_FALSE;
}

void prim_rem (void)
{
  decode_2_int_args ();
  if (a2 == 0)
    ERROR("divide by 0");
  arg1 = encode_int (a1 % a2);
  arg2 = OBJ_FALSE;
}

void prim_neg (void)
{
  a1 = decode_int (arg1);
  arg1 = encode_int (- a1);
}

void prim_eq (void)
{
  decode_2_int_args ();
  arg1 = encode_bool (a1 == a2);
  arg2 = OBJ_FALSE;
}

void prim_lt (void)
{
  decode_2_int_args ();
  arg1 = encode_bool (a1 < a2);
  arg2 = OBJ_FALSE;
}

void prim_gt (void)
{
  decode_2_int_args ();
  arg1 = encode_bool (a1 > a2);
  arg2 = OBJ_FALSE;
}

void prim_ior (void)
{
  a1 = decode_int (arg1);
  a2 = decode_int (arg2);
  arg1 = encode_int (a1 | a2);
  arg2 = OBJ_FALSE;
}

void prim_xor (void)
{
  a1 = decode_int (arg1);
  a2 = decode_int (arg2);
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
  return alloc_ram_cell_init (COMPOSITE_FIELD0 | ((car & 0x1f00) >> 8),
			      car & 0xff,
			      PAIR_FIELD2 | ((cdr & 0x1f00) >> 8),
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
        TYPE_ERROR("pair");
      arg1 = ram_get_car (arg1);
    }
  else if (IN_ROM(arg1))
    {
      if (!ROM_PAIR(arg1))
        TYPE_ERROR("pair");
      arg1 = rom_get_car (arg1);
    }
  else
    {
      TYPE_ERROR("pair");
    }
}

void prim_cdr (void)
{
  if (IN_RAM(arg1))
    {
      if (!RAM_PAIR(arg1))
        TYPE_ERROR("pair");
      arg1 = ram_get_cdr (arg1);
    }
  else if (IN_ROM(arg1))
    {
      if (!ROM_PAIR(arg1))
        TYPE_ERROR("pair");
      arg1 = rom_get_cdr (arg1);
    }
  else
    {
      TYPE_ERROR("pair");
    }
}

void prim_set_car (void)
{
  if (IN_RAM(arg1))
    {
      if (!RAM_PAIR(arg1))
        TYPE_ERROR("pair");

      ram_set_car (arg1, arg2);
      arg1 = OBJ_FALSE;
      arg2 = OBJ_FALSE;
    }
  else
    {
      TYPE_ERROR("pair");
    }
}

void prim_set_cdr (void)
{
  if (IN_RAM(arg1))
    {
      if (!RAM_PAIR(arg1))
        TYPE_ERROR("pair");

      ram_set_cdr (arg1, arg2);
      arg1 = OBJ_FALSE;
      arg2 = OBJ_FALSE;
    }
  else
    {
      TYPE_ERROR("pair");
    }
}

void prim_nullp (void)
{
  arg1 = encode_bool (arg1 == OBJ_NULL);
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
        TYPE_ERROR("string");

      arg1 = ram_get_car (arg1);
    }
  else if (IN_ROM(arg1))
    {
      if (!ROM_STRING(arg1))
        TYPE_ERROR("string");

      arg1 = rom_get_car (arg1);
    }
  else
    TYPE_ERROR("string");
}

 void prim_list2string (void)
{
  arg1 = alloc_ram_cell_init (COMPOSITE_FIELD0 | ((arg1 & 0x1f00) >> 8),
			      arg1 & 0xff,
			      STRING_FIELD2,
			      0);
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
  decode_2_int_args ();

  if (a1 < 1 || a1 > 2 || a2 < -100 || a2 > 100)
    ERROR("argument out of range to procedure \"motor\"");

#ifdef PICOBOARD2

  fw_motor ();

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
  decode_2_int_args ();
  a3 = decode_int (arg3);

  if (a1 < 1 || a1 > 3 || a2 < 0 || a3 < 0)
    ERROR("argument out of range to procedure \"led\"");

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
  a1 = decode_int (arg1);

  if (a1 < 0 || a1 > 1)
    ERROR("argument out of range to procedure \"led2-color\"");

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
  decode_2_int_args();
  a1 = read_clock () + a1;

  if (a1 < 0 || a2 < 1 || a2 > 3)
    ERROR("argument out of range to procedure \"getchar-wait\"");

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
    ERROR("argument out of range to procedure \"putchar\"");

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
  decode_2_int_args ();

  if (a1 < 1 || a1 > 255 || a2 < 0)
    ERROR("argument out of range to procedure \"beep\"");

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

  a1 = decode_int (arg1);

  if (a1 < 1 || a1 > 3)
    ERROR("argument out of range to procedure \"adc\"");

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


void prim_dac (void)
{
  a1 = decode_int (arg1);

  if (a1 < 0 || a1 > 255)
    ERROR("argument out of range to procedure \"dac\"");

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

#define PUSH_CONSTANT1 0x00
#define PUSH_CONSTANT2 0x10
#define PUSH_STACK1    0x20
#define PUSH_STACK2    0x30
#define PUSH_GLOBAL    0x40
#define SET_GLOBAL     0x50
#define CALL           0x60
#define JUMP           0x70
#define CALL_TOPLEVEL  0x80
#define JUMP_TOPLEVEL  0x90
#define GOTO           0xa0
#define GOTO_IF_FALSE  0xb0
#define CLOSURE        0xc0
#define PRIM1          0xd0
#define PRIM2          0xe0
#define PRIM3          0xf0

#ifdef WORKSTATION

char *prim_name[48] =
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
    "prim #%prim29",
    "prim #%prim30",
    "prim #%prim31",
    "prim #%print",
    "prim #%clock",
    "prim #%motor",
    "prim #%led",
    "prim #%led2-color",
    "prim #%getchar-wait",
    "prim #%putchar",
    "prim #%beep",
    "prim #%adc",
    "prim #%dac",
    "prim #%sernum",
    "prim #%prim43",
    "push-constant [long]",
    "shift",
    "pop",
    "return",
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
      if (RAM_CONTINUATION(arg1))
	ERROR("continuation in pop_procedure"); // TODO this might be legitimate, but for now, we can't do this. if this error comes up, fix this function so it can handle continuations
      
      if (!RAM_CLOSURE(arg1))
	TYPE_ERROR("procedure");
      
      entry = ram_get_entry (arg1) + CODE_START; // FOO all addresses in the bytecode should be from 0, not from CODE_START, should be fixed everywhere, but might not be
    }
  else if (IN_ROM(arg1))
    {
      if (ROM_CONTINUATION(arg1))
	ERROR("continuation in pop_procedure"); // TODO same as above
      
      if (!ROM_CLOSURE(arg1))
        TYPE_ERROR("procedure");

      entry = rom_get_entry (arg1) + CODE_START;
    }
  else
    TYPE_ERROR("procedure");
}

void handle_arity_and_rest_param (void)
{
  uint8 np;

  np = rom_get (entry++);

  if ((np & 0x80) == 0)
    {
      if (na != np)
        ERROR("wrong number of arguments");
    }
  else
    {
      np = ~np;

      if (na < np)
        ERROR("wrong number of arguments");

      arg3 = OBJ_NULL;

      while (na > np)
        {
          arg4 = POP();

          arg3 = cons (arg4, arg3);
          arg4 = OBJ_FALSE;

          na--;
        }

      arg1 = cons (arg3, arg1);
      arg3 = OBJ_FALSE; // TODO changed nothing with the new new closures, everything looks ok
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

  arg3 = OBJ_FALSE; // TODO changed nothing here either
}

void save_cont (void)
{ // BARF probably a problem here
  // the second half is a closure
  /* second_half = alloc_ram_cell_init (CLOSURE_FIELD0 | ((pc & 0xf800) >> 11), */
  /* 				     (pc & 0x07f8) >> 3, */
  /* 				     ((pc & 0x0007) << 5) | (env >> 8), */
  /* 				     env & 0xff); */
  second_half = alloc_ram_cell_init (CLOSURE_FIELD0 | (pc >> 11),
				     (pc >> 3) & 0xff, // BREGG
				     ((pc & 0x0007) << 5) | (env >> 8),
				     env & 0xff);
  // BREGG problem is, we add the start twice, in get entry, and somewhere else, but pc doesn't have it initially
  cont = alloc_ram_cell_init (COMPOSITE_FIELD0 | (cont >> 8),
                              cont & 0xff,
			      CONTINUATION_FIELD2 | (second_half >> 8),
                              second_half & 0xff);
}

void interpreter (void)
{
  init_ram_heap ();

  pc = (CODE_START + 4) + ((rom_addr)rom_get (CODE_START+2) << 2);

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

  arg1 = ram_get_car (arg1); // TODO BARF what to do if we want to get something in the env of a continuation ? will it happen, or only when called, when it becomes a simple closure ? if only when a closure, we're fine, I guess, since 1 is added to the offset by the compiler to skip the closure

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

  pop_procedure (); // TODO FOOBAR can we call a continuation ? if so, fix pop_procedure
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
  CASE(CALL_TOPLEVEL);

  FETCH_NEXT_BYTECODE();  
  second_half = bytecode; // TODO make sure second_half is not already in use
  
  FETCH_NEXT_BYTECODE();

  IF_TRACE(printf("  (call-toplevel 0x%04x)\n", ((second_half << 8) | bytecode) + CODE_START));

  entry = (second_half << 8) + bytecode + CODE_START; // TODO FOOBAR we have the last 4 bits of the opcode free, to do pretty much anything
  arg1 = OBJ_NULL;

  na = rom_get (entry++);

  build_env ();
  save_cont ();

  env = arg1;
  pc = entry;

  arg1 = OBJ_FALSE;

  DISPATCH();

  /***************************************************************************/
  CASE(JUMP_TOPLEVEL);

  FETCH_NEXT_BYTECODE();  
  second_half = bytecode; // TODO make sure second_half is not already in use
  
  FETCH_NEXT_BYTECODE();

  IF_TRACE(printf("  (jump-toplevel 0x%04x)\n", ((second_half << 8) | bytecode) + CODE_START));

  entry = (second_half << 8) + bytecode + CODE_START; // TODO this is a common pattern
  arg1 = OBJ_NULL;

  na = rom_get (entry++);

  build_env ();

  env = arg1;
  pc = entry;

  arg1 = OBJ_FALSE;

  DISPATCH();

  /***************************************************************************/
  CASE(GOTO);

  FETCH_NEXT_BYTECODE();
  second_half = bytecode;

  FETCH_NEXT_BYTECODE();
  
  // TODO goto's use 12-bit addresses, unlike calls and jumps, which use 16, is it ok ?
  // actually, the compiler gives them 16 bit addresses now, it seems
  // that means we have even more free instructions, but that now even gotos are on 3 bytes
  IF_TRACE(printf("  (goto 0x%04x)\n", ((rom_addr)(bytecode_lo4 + (CODE_START >> 8)) << 8) + bytecode));

  pc = (second_half << 8) + bytecode + CODE_START;
  /* pc = ((rom_addr)(bytecode_lo4 + (CODE_START >> 8)) << 8) + bytecode; */ // TODO not anymore

  DISPATCH();

  /***************************************************************************/
  CASE(GOTO_IF_FALSE);

  FETCH_NEXT_BYTECODE();
  second_half = bytecode;

  FETCH_NEXT_BYTECODE();

  IF_TRACE(printf("  (goto-if-false 0x%04x)\n", ((rom_addr)(bytecode_lo4 + (CODE_START >> 8)) << 8) + bytecode));

  if (POP() == OBJ_FALSE)
    pc = (second_half << 8) + bytecode + CODE_START;
    /* pc = ((rom_addr)(bytecode_lo4 + (CODE_START >> 8)) << 8) + bytecode; */

  DISPATCH();

  /***************************************************************************/
  CASE(CLOSURE);

  FETCH_NEXT_BYTECODE();
  second_half = bytecode;
  
  FETCH_NEXT_BYTECODE();

  IF_TRACE(printf("  (closure 0x%04x)\n", (second_half << 8) | bytecode));
  // TODO original had CODE_START, while the real code below didn't

  /* arg2 = POP(); // #f TODO should be, at least, and not used anymore, would it break anything not to use it in the compiler anymore ? maybe try, it's not urgent, but would be nice */ // TODO we got rid of this in the compiler
  arg3 = POP(); // env

  entry = (second_half << 8) | bytecode; // TODO original had no CODE_START, why ?

  arg1 = alloc_ram_cell_init (CLOSURE_FIELD0 | (second_half >> 3),
			      ((second_half & 0x07) << 5) | (bytecode >> 3),
			      ((bytecode & 0x07) << 5) |((arg3 & 0x1f00) >> 8),
                              arg3 & 0xff);

  PUSH_ARG1();

  arg2 = OBJ_FALSE;
  arg3 = OBJ_FALSE;

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

      PUSH_ARG1(); // TODO we don't call the continuation, no change was needed

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

      second_half = ram_get_cdr (cont);
      
      pc = ram_get_entry (second_half);

      env = ram_get_cdr (second_half);
      cont = ram_get_car (cont);

      PUSH_ARG1();

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
#if 0
    case 13:
      break;
    case 14:
      break;
    case 15:
      break;
#endif
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
      /* prim #%dac */
      arg1 = POP();  prim_dac ();  break;
    case 10:
      /* prim #%sernum */
      prim_sernum ();  PUSH_ARG1();  break;
#if 0
    case 11:
      break;
#endif
    case 12:
      /* push-constant [long] */
      FETCH_NEXT_BYTECODE();
      second_half = bytecode;
      FETCH_NEXT_BYTECODE();
      arg1 = (second_half << 8) | bytecode;
      PUSH_ARG1();
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
      second_half = ram_get_cdr (cont);
      pc = ram_get_entry (second_half);
      env = ram_get_cdr (second_half);
      cont = ram_get_car (cont);
      PUSH_ARG1();
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
  printf ("Start address = 0x%04x\n", rom_start_addr);
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
