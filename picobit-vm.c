/* file: "picobit-vm.c" */

/*
 * Copyright 2004 by Marc Feeley, All Rights Reserved.
 *
 * History:
 *
 *   15/08/2004  Release of version 1
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


#ifdef __18CXX
#define ROBOT
#endif

#ifdef HI_TECH_C
#define ROBOT
#endif

#ifndef ROBOT
#define WORKSTATION
#endif


#ifdef __18CXX

#include <p18f452.h>

extern volatile near uint8 IR_TX_BUF[2+(8+2)+2];
extern volatile near uint8 FW_EVENTS;
extern volatile near uint8 FW_OPS;
extern volatile near uint8 IR_TX_LENGTH;
extern volatile near uint8 IR_TX_LEDS;
extern volatile near uint8 IR_TX_CURRENT_LEDS;
extern volatile near uint8 IR_TX_POWER;
extern volatile near uint8 IR_TX_CURRENT_POWER;
extern volatile near uint8 IR_TX_SHIFT_REG;
extern volatile near uint8 IR_TX_PTR;
extern volatile near uint8 IR_TX_TIMEOUT;
extern volatile near uint8 IR_TX_WAIT_RANGE;
extern volatile near uint8 IR_TX_RETRY_COUNT;
extern volatile near uint8 IR_TX_CRC_HI;
extern volatile near uint8 IR_TX_CRC_LO;
extern volatile near uint8 IR_TX_HI4;
extern volatile near uint8 IR_TX_LO4;
extern volatile near uint8 INT_IR_STATE_HI;
extern volatile near uint8 INT_IR_STATE_LO;
extern volatile near uint8 INT_PCLATH;
extern volatile near uint8 INT_CODE;
extern volatile near uint8 IR_BIT_CLOCK;
extern volatile near uint8 CLOCK_UP;
extern volatile near uint8 CLOCK_HI;
extern volatile near uint8 CLOCK_LO;
extern volatile near uint8 RANDOM;
extern volatile near uint8 NODE_NUM;
extern volatile near uint8 IR_RX_SOURCE;
extern volatile near uint8 IR_RX_LENGTH;
extern volatile near uint8 IR_RX_BUF[2+(2+8)+2];
extern volatile near uint8 IR_RX_CRC_HI;
extern volatile near uint8 IR_RX_CRC_LO;
extern volatile near uint8 IR_RX_HI4;
extern volatile near uint8 IR_RX_LO4;
extern volatile near uint8 DRIVE_A_MODE;
extern volatile near uint8 DRIVE_A_PWM;
extern volatile near uint8 DRIVE_B_MODE;
extern volatile near uint8 DRIVE_B_PWM;
extern volatile near uint8 DRIVE_C_MODE;
extern volatile near uint8 DRIVE_C_PWM;
extern volatile near uint8 MOTOR_ID;
extern volatile near uint8 FW_VALUE_UP;
extern volatile near uint8 MOTOR_ROT;
extern volatile near uint8 FW_VALUE_HI;
extern volatile near uint8 MOTOR_POW;
extern volatile near uint8 FW_VALUE_LO;
extern volatile near uint8 FW_VALUE_TMP;
extern volatile near uint8 FW_LAST_TX_TIME_LO;
extern volatile near uint8 IR_RX_SAMPLE_TIMER;
extern volatile near uint8 IR_RX_SHIFT_REG;
extern volatile near uint8 IR_RX_PREVIOUS;
extern volatile near uint8 IR_RX_PTR;
extern volatile near uint8 IR_RX_BYTE;
extern volatile near uint8 STDIO_TX_SEQ_NUM;
extern volatile near uint8 STDIO_RX_SEQ_NUM;
extern volatile near uint8 FW_TEMP1;

extern void fw_clock_read (void);
extern void fw_motor (void);
extern void fw_light_read (void);
extern void fw_ir_tx (void);
extern void fw_ir_rx_stdio_char (void);
extern void fw_ir_tx_wait_ready (void);
extern void fw_ir_tx_stdio (void);
extern void program_mode (void);

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

#define CODE_START 0x2000

#define GLOVARS 16
// TODO raise ?

#ifdef DEBUG
#define IF_TRACE(x) x
#define IF_GC_TRACE(x)
#else
#define IF_TRACE(x)
#define IF_GC_TRACE(x)
#endif

/*---------------------------------------------------------------------------*/


#ifdef __18CXX

#define ERROR(msg) program_mode ()
#define TYPE_ERROR(type) program_mode ()

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
#define MAX_RAM_ENCODING 4095

#define RAM_BYTES ((MAX_RAM_ENCODING - MIN_RAM_ENCODING + 1)*4)


// TODO was 8 for ram, maybe change for rom too if we get bigger ?
#if WORD_BITS == 8
#define OBJ_TO_RAM_ADDR(o,f) (((ram_addr)((uint16)(o) - MIN_RAM_ENCODING) << 2) + (f))
#define OBJ_TO_ROM_ADDR(o,f) (((rom_addr)((uint8)(o) - MIN_ROM_ENCODING) << 2) + (CODE_START + 4 + (f)))
#endif


#ifdef __18CXX

#define ram_get(a) *(uint8*)(a+0x200)
#define ram_set(a,x) *(uint8*)(a+0x200) = (x)

#endif


#ifdef WORKSTATION

uint8 ram_mem[RAM_BYTES];

#define ram_get(a) ram_mem[a]
#define ram_set(a,x) ram_mem[a] = (x)

#endif


/*---------------------------------------------------------------------------*/

#ifdef __18CXX

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
  TODO do we want 0..127 as fixnums ? would reduce number of ram objects
  rom object   4 + (MAX_FIXNUM-MIN_FIXNUM) ... MIN_RAM_ENCODING-1
  ram object   MIN_RAM_ENCODING ... 4095 TODO was 255, now we have 12 bits
  TODO divide more evenly between RAM and ROM ?

  layout of memory allocated objects:

  bignum n     00000000 uuuuuuuu hhhhhhhh llllllll  (24 bit signed integer)

  TODO free space here

  pair         00000010 aaaaaaaa aaaadddd dddddddd
  a is car
  d is cdr
  gives an address space of 2^12 * 4 = 16k

  symbol       00000011 00000000 00000000 00000000

  string       00000100 *chars** ****0000 00000000

  vector       00000101 *elems** ****0000 00000000  TODO not used yet

  closure      00aaaaaa xxxxxxxx xxxxyyyy aaaaaaaa
  0x5ff<a<0x4000 is entry
  x is pointer to environment
  y is unused
  TODO we can use this y for a

  continuation 00aaaaaa xxxxxxxx xxxxyyyy aaaaaaaa  0x5ff<a<0x4000 is pc
  x is pointer to the second half
  y is unused TODO ok, ugly hack, closures are in only one object, but continuations (since they seem to only be created by the runtime) are stored in 2, the compiler doesn't need to be changed much, not for closures at least, we'll just have to see if the similar representation is used somewhere in the vm

  second half  00000010 xxxxxxxx xxxxyyyy yyyyyyyy
  of continuations, actually a simple pair
  x is environment
  y is parent continuation

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

#define BIGNUM_FIELD0 0
#define RAM_BIGNUM(o) (ram_get_field0 (o) == BIGNUM_FIELD0)
#define ROM_BIGNUM(o) (rom_get_field0 (o) == BIGNUM_FIELD0)

// TODO this is not in use
#define TRIPLET_FIELD0 1
#define RAM_TRIPLET(o) (ram_get_field0 (o) == TRIPLET_FIELD0)
#define ROM_TRIPLET(o) (rom_get_field0 (o) == TRIPLET_FIELD0)

#define PAIR_FIELD0 2
#define RAM_PAIR(o) (ram_get_field0 (o) == PAIR_FIELD0)
#define ROM_PAIR(o) (rom_get_field0 (o) == PAIR_FIELD0)

#define SYMBOL_FIELD0 3
#define RAM_SYMBOL(o) (ram_get_field0 (o) == SYMBOL_FIELD0)
#define ROM_SYMBOL(o) (rom_get_field0 (o) == SYMBOL_FIELD0)

#define STRING_FIELD0 4
#define RAM_STRING(o) (ram_get_field0 (o) == STRING_FIELD0)
#define ROM_STRING(o) (rom_get_field0 (o) == STRING_FIELD0)

#define VECTOR_FIELD0 5
#define RAM_VECTOR(o) (ram_get_field0 (o) == VECTOR_FIELD0)
#define ROM_VECTOR(o) (rom_get_field0 (o) == VECTOR_FIELD0)

#define PROCEDURE_FIELD0 6
#define RAM_PROCEDURE(o) (ram_get_field0 (o) >= PROCEDURE_FIELD0)
#define ROM_PROCEDURE(o) (rom_get_field0 (o) >= PROCEDURE_FIELD0)

/*---------------------------------------------------------------------------*/

#define RAM_GET_FIELD0_MACRO(o) ram_get (OBJ_TO_RAM_ADDR(o,0))
#define RAM_SET_FIELD0_MACRO(o,val) ram_set (OBJ_TO_RAM_ADDR(o,0), val)
#define ROM_GET_FIELD0_MACRO(o) rom_get (OBJ_TO_ROM_ADDR(o,0))

#define RAM_GET_GC_TAGS_MACRO(o) (RAM_GET_FIELD0_MACRO(o) & 0xc0)
#define RAM_SET_GC_TAGS_MACRO(o,tags)                                   \
  RAM_SET_FIELD0_MACRO(o,(RAM_GET_FIELD0_MACRO(o) & 0x3f) | (tags))

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

// TODO this might be of use
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
void ram_set_gc_tags (obj o, uint8 tags) { RAM_SET_GC_TAGS_MACRO(o,tags); }
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

// TODO change code to use them when necessary
obj ram_get_car (obj o)
{ return (ram_get_field1 (o) << 4) | ((ram_get_field2 (o) & 0xf0) >> 4); }
obj rom_get_car (obj o)
{ return (rom_get_field1 (o) << 4) | ((rom_get_field2 (o) & 0xf0) >> 4); }
obj ram_get_cdr (obj o)
{ return ((ram_get_field2 (o) & 0xf) << 8) | (ram_get_field3 (o)); }
obj rom_get_cdr (obj o)
{ return ((rom_get_field2 (o) & 0xf) << 8) | (rom_get_field3 (o)); }
void ram_set_car (obj o, obj val)
{
  ram_set_field1 (o, (val & 0xff0) >> 4);
  ram_set_field2 (o, ((val & 0xf) << 4) | (ram_get_field2 (o) & 0xf));
}
void ram_set_cdr (obj o, obj val)
{
  ram_set_field2 (o, (ram_get_field2 (o) & 0xf0) | ((val & 0xf00) >> 8));
  ram_set_field3 (o, val & 0xff);
}

obj get_global (uint8 i) // TODO 8 ? do we want more than 256 globals ?
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

#define GC_TAG_0_LEFT   (3<<6)
#define GC_TAG_1_LEFT   (2<<6)
#define GC_TAG_2_LEFT   (1<<6)
#define GC_TAG_UNMARKED (0<<6)  /* must be 0 */

/* Number of object fields of objects in ram */

#define HAS_3_OBJECT_FIELDS(field0) 0
#define HAS_2_OBJECT_FIELDS(field0) ((field0) ==  PAIR_FIELD0)
#define HAS_1_OBJECT_FIELD(field0)  ((field0) >= PROCEDURE_FIELD0)
// procedures and continuations have both 1 pointer
// TODO change when we add true vectors

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
obj second_half; /* the second half of continuations */

uint8 na; /* interpreter variables */ // TODO what's that ?
rom_addr pc;
rom_addr entry;
uint8 bytecode;
uint8 bytecode_hi4;
uint8 bytecode_lo4;
uint8 field0;
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
      ram_set_field1 (o, free_list);
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
  
  obj stack;
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
      
      if (ram_get_gc_tags (visit) != GC_TAG_UNMARKED)
        IF_GC_TRACE(printf ("case 1\n"));
      else
        {
          field0 = ram_get_field0 (visit);

          if (HAS_2_OBJECT_FIELDS(field0))
            {
              IF_GC_TRACE(printf ("case 5\n"));
	      // TODO we don't have cases 2-4 anymore

            visit_field2:

              temp = ram_get_cdr (visit); // TODO was field2

              if (IN_RAM(temp))
                {
                  IF_GC_TRACE(printf ("case 6\n"));
                  ram_set_gc_tags (visit, GC_TAG_1_LEFT);
                  ram_set_cdr (visit, stack); // TODO was field2
                  goto push;
                }

              IF_GC_TRACE(printf ("case 7\n"));

              goto visit_field1;
            }

          if (HAS_1_OBJECT_FIELD(field0))
            {
              IF_GC_TRACE(printf ("case 8\n"));

            visit_field1:

              temp = ram_get_car (visit); // TODO was field1

              if (IN_RAM(temp))
                {
                  IF_GC_TRACE(printf ("case 9\n"));
                  ram_set_gc_tags (visit, GC_TAG_0_LEFT);
                  ram_set_car (visit, stack); // TODO was field1
                  goto push;
                }

              IF_GC_TRACE(printf ("case 10\n"));
            }
          else
            IF_GC_TRACE(printf ("case 11\n"));

          ram_set_gc_tags (visit, GC_TAG_0_LEFT);
        }

    pop:

      IF_GC_TRACE(printf ("pop    stack=%d (tag=%d)  visit=%d (tag=%d)\n", stack, ram_get_gc_tags (stack)>>6, visit, ram_get_gc_tags (visit)>>6));

      if (stack != NIL)
        {
          if (ram_get_gc_tags (stack) == GC_TAG_1_LEFT)
            {
              IF_GC_TRACE(printf ("case 13\n"));

              temp = ram_get_cdr (stack);  /* pop through field 2 */ // TODO was field2
              ram_set_cdr (stack, visit); // TODO was field2
              visit = stack;
              stack = temp;

              goto visit_field1;
            }

          IF_GC_TRACE(printf ("case 14\n"));

          temp = ram_get_car (stack);  /* pop through field 1 */ // TODO was field1
          ram_set_car (stack, visit); // TODO was field1
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
      if (ram_get_gc_tags (visit) == GC_TAG_UNMARKED) /* unmarked? */
        {
          ram_set_field1 (visit, free_list);
          free_list = visit;
        }
      else
        {
          ram_set_gc_tags (visit, GC_TAG_UNMARKED);
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
      uint8 field0;

      if (IN_RAM(o))
        field0 = ram_get_field0 (o);
      else
        field0 = rom_get_field0 (o);

      if (field0 == BIGNUM_FIELD0)
        printf ("%d", decode_int (o));
      else if (field0 == TRIPLET_FIELD0) // TODO not in use
        printf ("#<triplet>");
      else if (field0 == PAIR_FIELD0)
        {
          obj car;
	  obj cdr;

	  if (IN_RAM(o))
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
          else if ((IN_RAM(cdr) ? ram_get_field0 (cdr) : rom_get_field0 (cdr))
                   == PAIR_FIELD0)
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
      else if (field0 == SYMBOL_FIELD0)
        printf ("#<symbol>");
      else if (field0 == STRING_FIELD0)
        printf ("#<string>");
      else if (field0 == VECTOR_FIELD0)
        printf ("#<vector>");
      else
        {
          /* obj env; */
          /* obj parent_cont; */
          /* rom_addr pc; */

          /* if (IN_RAM(o)) */
          /*   env = ram_get_field1 (o); */
          /* else */
          /*   env = rom_get_field1 (o); */

          /* if (IN_RAM(o)) */
          /*   parent_cont = ram_get_field2 (o); */
          /* else */
          /*   parent_cont = rom_get_field2 (o); */

          /* if (IN_RAM(o)) */
          /*   pc = ((rom_addr)(field0 + ((CODE_START>>8) - PROCEDURE_FIELD0)) << 8) + ram_get_field3 (o); */
          /* else */
          /*   pc = ((rom_addr)(field0 + ((CODE_START>>8) - PROCEDURE_FIELD0)) << 8) + rom_get_field3 (o); */

          /* printf ("{0x%04x ", pc); */
          /* show (env); */
          /* printf (" "); */
          /* show (parent_cont); */
          /* printf ("}"); */ // TODO the representation of procedures changed
	  printf ("#<procedure>");
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
  return alloc_ram_cell_init (PAIR_FIELD0,
			      (car & 0xff0) >> 4,
			      ((car & 0xf) << 4) | ((cdr & 0xf00) >> 8),
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
    TYPE_ERROR("pair");
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
    TYPE_ERROR("pair");
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
    TYPE_ERROR("pair");
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
    TYPE_ERROR("pair");
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

      arg1 = ram_get_car (arg1); // TODO was field1, but pointer size changed
    }
  else if (IN_ROM(arg1))
    {
      if (!ROM_STRING(arg1))
        TYPE_ERROR("string");

      arg1 = rom_get_car (arg1); // TODO was field1, but pointer size changed
    }
  else
    TYPE_ERROR("string");
}

 void prim_list2string (void)
{
  arg1 = alloc_ram_cell_init (STRING_FIELD0,
			      (arg1 & 0xff0) >> 4, // TODO pointer size changed
			      (arg1 & 0xf) << 4,
			      0);
}

void prim_ior (void) // ADDED
{
  a1 = decode_int (arg1);
  a2 = decode_int (arg2);
  arg1 = encode_int (a1 | a2);
  arg2 = OBJ_FALSE;
}

void prim_xor (void) // ADDED
{
  a1 = decode_int (arg1);
  a2 = decode_int (arg2);
  arg1 = encode_int (a1 ^ a2);
  arg2 = OBJ_FALSE;
}


/*---------------------------------------------------------------------------*/

/* Robot specific operations */


void prim_print (void)
{
#ifdef __18CXX
#endif

#ifdef WORKSTATION

  print (arg1);

#endif

  arg1 = OBJ_FALSE;
}


int32 read_clock (void)
{
  int32 now = 0;

#ifdef __18CXX

  fw_clock_read ();

  now = ((int32)(((int16)FW_VALUE_UP << 8) + FW_VALUE_HI) << 8) + FW_VALUE_LO;

#endif

#ifdef WORKSTATION

#ifdef _WIN32

  static int32 start = 0;
  struct timeb tb;

  ftime (&tb);

  now = tb.time * 100 + tb.millitm / 10;
  if (start == 0)
    start = now;
  now -= start;

#else

  static int32 start = 0;
  struct timeval tv;

  if (gettimeofday (&tv, NULL) == 0)
    {
      now = tv.tv_sec * 100 + tv.tv_usec / 10000;
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
  a3 = decode_int (arg3);

  if (a1 < 0 || a1 > 2 || a2 < -1 || a2 > 1 || a3 < -4 || a3 > 4)
    ERROR("argument out of range to procedure \"motor\"");

#ifdef __18CXX

  MOTOR_ID  = a1;
  MOTOR_ROT = a2;
  MOTOR_POW = a3;

  fw_motor ();

#endif

#ifdef WORKSTATION

  printf ("motor %d -> rotation=%d power=%d\n", a1, a2, a3);
  fflush (stdout);

#endif

  arg1 = OBJ_FALSE;
  arg2 = OBJ_FALSE;
  arg3 = OBJ_FALSE;
}


void prim_led (void)
{
  a1 = decode_int (arg1);

  if (a1 < 0 || a1 > 2){
    printf("%d", a1); // TODO debug
    ERROR("argument out of range to procedure \"led\"");
  }

#ifdef __18CXX

  LATBbits.LATB5 = (a1 == 1);
  LATBbits.LATB4 = (a1 == 2);

#endif

#ifdef HI_TECH_C

  ACTIVITY_LED1 = (a1 == 1);
  ACTIVITY_LED2 = (a1 == 2);

#endif

#ifdef WORKSTATION

  printf ("led -> %s\n", (a1==1)?"red":(a1==2)?"green":"off");
  fflush (stdout);

#endif

  arg1 = OBJ_FALSE;
}


void prim_getchar_wait (void)
{
  a1 = decode_int (arg1);
  a1 = read_clock () + a1;

#ifdef __18CXX

  arg1 = OBJ_FALSE;

  do
    {
      uint8 seq_num = STDIO_RX_SEQ_NUM;

      fw_ir_rx_stdio_char ();

      if (seq_num != STDIO_RX_SEQ_NUM)
        {
          arg1 = encode_int (FW_VALUE_LO);
          break;
        }
    } while (read_clock () < a1);

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
  a1 = decode_int (arg1);

  if (a1 < 0 || a1 > 255)
    ERROR("argument out of range to procedure \"putchar\"");

#ifdef __18CXX

  fw_ir_tx_wait_ready ();

  IR_TX_BUF[2] = a1;
  IR_TX_LENGTH = 1;

  fw_ir_tx_stdio ();

#endif

#ifdef WORKSTATION

  putchar (a1);
  fflush (stdout);

#endif

  arg1 = OBJ_FALSE;
}


void prim_light (void)
{
  uint8 light;

#ifdef __18CXX

  fw_light_read ();

  light = FW_VALUE_LO;

#endif

#ifdef WORKSTATION

  light = read_clock () & 31;

  if (light > 15) light = 32 - light;

  light += 40;

#endif

  arg1 = encode_int (light);
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
    "prim #%ior", // ADDED, was "prim #%<=",
    "prim #%>",
    "prim #%xor", // ADDED, was "prim #%>=",
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
    "prim #%set-fst!", // ADDED TODO obsolete, but kept to have the right size
    "prim #%set-snd!", // ADDED
    "prim #%set-trd!", // ADDED
    "prim #%print",
    "prim #%clock",
    "prim #%motor",
    "prim #%led",
    "prim #%getchar-wait",
    "prim #%putchar",
    "prim #%light",
    "prim #%triplet?", // ADDED
    "prim #%triplet", // ADDED
    "prim #%fst", // ADDED
    "prim #%snd", // ADDED
    "prim #%trd", // ADDED
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
  obj o = ram_get_car (env); // TODO changed type
  env = ram_get_cdr (env);
  return o;
}

void pop_procedure (void) // TODO where do we get the env of the procedure ?
{ // TODO can continuations end up ond the stack ? if so, they act differently than procedures
  arg1 = POP();
  if (IN_RAM(arg1))
    {
      field0 = ram_get_field0 (arg1);

      if (field0 < PROCEDURE_FIELD0)
	TYPE_ERROR("procedure");

      entry = ((rom_addr)(field0 + ((CODE_START>>8) - PROCEDURE_FIELD0)) << 8) + ram_get_field3 (arg1);
    }
  else if (IN_ROM(arg1))
    {
      field0 = rom_get_field0 (arg1);

      if (field0 < PROCEDURE_FIELD0)
        TYPE_ERROR("procedure");

      entry = ((rom_addr)(field0 + ((CODE_START>>8) - PROCEDURE_FIELD0)) << 8) + rom_get_field3 (arg1);
    }
  else
    TYPE_ERROR("procedure");
}

void handle_arity_and_rest_param (void)
{
  uint8 np;

  np = rom_get (entry++); // TODO does that mean we can't have procedures in ram ?

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
  second_half = cons (env, cont);
  cont = alloc_ram_cell_init ((uint8)(pc >> 8) - ((CODE_START>>8)-PROCEDURE_FIELD0),
                              (second_half & 0xff0) >> 4,
                              (second_half & 0xf)   << 4,
                              (uint8)pc);
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
      arg1 = ram_get_cdr (arg1); // TODO was field2
      bytecode_lo4--;
    }

  arg1 = ram_get_car (arg1); // TODO was field1

  PUSH_ARG1();

  DISPATCH();

  /***************************************************************************/
  CASE(PUSH_STACK2);

  IF_TRACE(printf("  (push-stack %d)\n", bytecode_lo4+16)); // TODO do we ever need to go this far in the stack ? since the stack is the env, maybe, if not, we have one free instruction

  bytecode_lo4 += 16;

  arg1 = env;

  while (bytecode_lo4 != 0)
    {
      arg1 = ram_get_cdr (arg1); // TODO was field2
      bytecode_lo4--;
    }

  arg1 = ram_get_car (arg1); // TODO was field1

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
  CASE(CALL_TOPLEVEL);

  FETCH_NEXT_BYTECODE();

  IF_TRACE(printf("  (call-toplevel 0x%04x)\n", ((rom_addr)(bytecode_lo4 + (CODE_START >> 8)) << 8) + bytecode));

  entry = ((rom_addr)(bytecode_lo4 + (CODE_START >> 8)) << 8) + bytecode;
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

  IF_TRACE(printf("  (jump-toplevel 0x%04x)\n", ((rom_addr)(bytecode_lo4 + (CODE_START >> 8)) << 8) + bytecode));

  entry = ((rom_addr)(bytecode_lo4 + (CODE_START >> 8)) << 8) + bytecode;
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

  IF_TRACE(printf("  (goto 0x%04x)\n", ((rom_addr)(bytecode_lo4 + (CODE_START >> 8)) << 8) + bytecode));

  pc = ((rom_addr)(bytecode_lo4 + (CODE_START >> 8)) << 8) + bytecode;

  DISPATCH();

  /***************************************************************************/
  CASE(GOTO_IF_FALSE);

  FETCH_NEXT_BYTECODE();

  IF_TRACE(printf("  (goto-if-false 0x%04x)\n", ((rom_addr)(bytecode_lo4 + (CODE_START >> 8)) << 8) + bytecode));

  if (POP() == OBJ_FALSE)
    pc = ((rom_addr)(bytecode_lo4 + (CODE_START >> 8)) << 8) + bytecode;

  DISPATCH();

  /***************************************************************************/
  CASE(CLOSURE);

  FETCH_NEXT_BYTECODE();

  IF_TRACE(printf("  (closure 0x%04x)\n", ((rom_addr)(bytecode_lo4 + (CODE_START >> 8)) << 8) + bytecode));

  arg2 = POP(); // #f TODO should be, at least, and not used anymore
  arg3 = POP(); // env

  entry = ((rom_addr)bytecode_lo4 << 8) + bytecode;

  /* second_half = cons (arg3, arg2); */ // TODO not anymore
  arg1 = alloc_ram_cell_init ((uint8)(entry >> 8) + PROCEDURE_FIELD0,
                              /* (second_half & 0xff0) >> 4, */ // TODO not anymore
                              /* (second_half & 0xf)   << 4, */
			      (arg3 & 0xff0) >> 4,
                              (arg3 & 0xf)   << 4,
                              (uint8)entry);

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

      pc = ((rom_addr)(ram_get_field0 (cont) + ((CODE_START>>8) - PROCEDURE_FIELD0)) << 8) + ram_get_field3 (cont);
      env = ram_get_car (ram_get_car (cont)); // TODO were field1 and 2 of cont
      cont = ram_get_cdr (ram_get_car (cont));

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
      arg3 = POP();  arg2 = POP();  arg1 = POP();  prim_motor ();  break;
    case 3:
      /* prim #%led */
      arg1 = POP();  prim_led ();  ;break;
    case 4:
      /* prim #%getchar-wait */
      arg1 = POP();  prim_getchar_wait ();  PUSH_ARG1();  break;
    case 5:
      /* prim #%putchar */
      arg1 = POP();  prim_putchar ();  break;
    case 6:
      /* prim #%light */
      prim_light ();  PUSH_ARG1();  break;
#if 0
    case 7: // TODO since not all of them will be used for vectors, maybe some could be used to have more globals ? or something else ?
      break;
    case 8:
      break;
    case 9:
      break;
    case 10:
      break;
    case 11:
      break;
#endif
    case 12:
      /* push-constant [long] */ // TODO must find a way to get 12bit objects, use 2 bytes ? FOOBAR
      FETCH_NEXT_BYTECODE();
      arg1 = bytecode;
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
      pc = ((rom_addr)(ram_get_field0 (cont) + ((CODE_START>>8) - PROCEDURE_FIELD0)) << 8) + ram_get_field3 (cont);
      env = ram_get_car (ram_get_car (cont)); // TODO were field1 and 2 of cont
      cont = ram_get_cdr (ram_get_car (cont));
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

#ifdef __18CXX

/* $Id: c018i.c,v 1.1.2.1 2004/03/09 16:47:01 sealep Exp $ */

/* Copyright (c)1999 Microchip Technology */

/* MPLAB-C18 startup code, including initialized data */

#if 0
/* external reference to the user's main routine */
extern void main (void);
/* prototype for the startup function */
void _entry (void);
#endif
void _startup (void);
/* prototype for the initialized data setup */
void _do_cinit (void);

extern volatile near unsigned long short TBLPTR;
extern near unsigned FSR0;
extern near char FPFLAGS;
#define RND 6

#if 0
#pragma code _entry_scn=0x000000
void
_entry (void)
{
  _asm goto _startup _endasm

    }
#pragma code _startup_scn
#endif

void
_startup (void)
{
  _asm
    // Initialize the stack pointer
    lfsr 1, _stack lfsr 2, _stack clrf TBLPTRU, 0 // 1st silicon doesn't do this on POR
    bcf FPFLAGS,RND,0 // Initialize rounding flag for floating point libs

    _endasm
    _do_cinit ();

  // Call the user's main routine
  interpreter ();

  ERROR("halted");
}                               /* end _startup() */

/* MPLAB-C18 initialized data memory support */
/* The linker will populate the _cinit table */
extern far rom struct
{
  unsigned short num_init;
  struct _init_entry
  {
    unsigned long from;
    unsigned long to;
    unsigned long size;
  }
    entries[];
}
  _cinit;

#pragma code _cinit_scn
void
_do_cinit (void)
{
  /* we'll make the assumption in the following code that these statics
   * will be allocated into the same bank.
   */
  static short long prom;
  static unsigned short curr_byte;
  static unsigned short curr_entry;
  static short long data_ptr;

  // Initialized data...
  TBLPTR = (short long)&_cinit;
  _asm
    movlb data_ptr
    tblrdpostinc
    movf TABLAT, 0, 0
    movwf curr_entry, 1
    tblrdpostinc
    movf TABLAT, 0, 0
    movwf curr_entry+1, 1
    _endasm
    //while (curr_entry)
    //{
    test:
  _asm
    bnz 3
    tstfsz curr_entry, 1
    bra 1
    _endasm
    goto done;
  /* Count down so we only have to look up the data in _cinit
   * once.
   *
   * At this point we know that TBLPTR points to the top of the current
   * entry in _cinit, so we can just start reading the from, to, and
   * size values.
   */
  _asm
    /* read the source address */
    tblrdpostinc
    movf TABLAT, 0, 0
    movwf prom, 1
    tblrdpostinc
    movf TABLAT, 0, 0
    movwf prom+1, 1
    tblrdpostinc
    movf TABLAT, 0, 0
    movwf prom+2, 1
    /* skip a byte since it's stored as a 32bit int */
    tblrdpostinc
    /* read the destination address directly into FSR0 */
    tblrdpostinc
    movf TABLAT, 0, 0
    movwf FSR0L, 0
    tblrdpostinc
    movf TABLAT, 0, 0
    movwf FSR0H, 0
    /* skip two bytes since it's stored as a 32bit int */
    tblrdpostinc
    tblrdpostinc
    /* read the destination address directly into FSR0 */
    tblrdpostinc
    movf TABLAT, 0, 0
    movwf curr_byte, 1
    tblrdpostinc
    movf TABLAT, 0, 0
    movwf curr_byte+1, 1
    /* skip two bytes since it's stored as a 32bit int */
    tblrdpostinc
    tblrdpostinc
    _endasm
    //prom = data_ptr->from;
    //FSR0 = data_ptr->to;
    //curr_byte = (unsigned short) data_ptr->size;
    /* the table pointer now points to the next entry. Save it
     * off since we'll be using the table pointer to do the copying
     * for the entry.
     */
    data_ptr = TBLPTR;

  /* now assign the source address to the table pointer */
  TBLPTR = prom;

  /* do the copy loop */
  _asm
    // determine if we have any more bytes to copy
    movlb curr_byte
    movf curr_byte, 1, 1
    copy_loop:
  bnz 2 // copy_one_byte
    movf curr_byte + 1, 1, 1
    bz 7 // done_copying

    copy_one_byte:
  tblrdpostinc
    movf TABLAT, 0, 0
    movwf POSTINC0, 0

    // decrement byte counter
    decf curr_byte, 1, 1
    bc -8 // copy_loop
    decf curr_byte + 1, 1, 1
    bra -7 // copy_one_byte

    done_copying:

  _endasm
    /* restore the table pointer for the next entry */
    TBLPTR = data_ptr;
  /* next entry... */
  curr_entry--;
  goto test;
 done:
  ;
}

#pragma code picobit_boot=0x001ffa
void _picobit_boot (void)
{
  _asm goto _startup _endasm
    }

#endif

/*---------------------------------------------------------------------------*/
