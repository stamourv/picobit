#ifndef PICOBIT_OBJECT_H
#define PICOBIT_OBJECT_H

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

  ifdef CONFIG_BIGNUM_LONG
  bignum n     00G***** **next** hhhhhhhh llllllll  (16 bit digit)
  TODO what to do with the gc tags for the bignums ? will this work ?
  TODO since bignums have only 1 field, only one gc tag is should be enough
  (only one is used anyway), so no conflict with closures

  ifndef CONFIG_BIGNUM_LONG
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

  closure      01Gxxxxx xxxxxxxx aaaaaaaa aaaaaaaa
  0x5ff<a<0x4000 is entry
  x is pointer to environment

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

#ifdef LESS_MACROS
uint16 ENCODE_FIXNUM(uint16 n)
{
	return ((n) + (MIN_FIXNUM_ENCODING - MIN_FIXNUM));
}
uint8  DECODE_FIXNUM(uint16 o)
{
	return ((o) - (MIN_FIXNUM_ENCODING - MIN_FIXNUM));
}
#else
#define ENCODE_FIXNUM(n) ((n) + (MIN_FIXNUM_ENCODING - MIN_FIXNUM))
#define DECODE_FIXNUM(o) ((o) - (MIN_FIXNUM_ENCODING - MIN_FIXNUM))
#endif

#ifdef LESS_MACROS
uint8 IN_RAM(uint16 o)
{
	return ((o) >= MIN_RAM_ENCODING);
}
uint8 IN_ROM(uint16 o)
{
	return (!IN_RAM(o) && ((o) >= MIN_ROM_ENCODING));
}
#else
#define IN_RAM(o) ((o) >= MIN_RAM_ENCODING)
#define IN_ROM(o) (!IN_RAM(o) && ((o) >= MIN_ROM_ENCODING))
#endif

// bignum first byte : 00Gxxxxx
#define BIGNUM_FIELD0 0
#ifdef LESS_MACROS
uint8 RAM_BIGNUM_P(uint16 o)
{
	return ((ram_get_field0 (o) & 0xc0) == BIGNUM_FIELD0);
}
uint8 ROM_BIGNUM_P(uint16 o)
{
	return ((rom_get_field0 (o) & 0xc0) == BIGNUM_FIELD0);
}
#else
#define RAM_BIGNUM_P(o) ((ram_get_field0 (o) & 0xc0) == BIGNUM_FIELD0)
#define ROM_BIGNUM_P(o) ((rom_get_field0 (o) & 0xc0) == BIGNUM_FIELD0)
#endif

// composite first byte : 1GGxxxxx
#define COMPOSITE_FIELD0 0x80
#ifdef LESS_MACROS
uint8 RAM_COMPOSITE_P(uint16 o)
{
	return ((ram_get_field0 (o) & 0x80) == COMPOSITE_FIELD0);
}
uint8 ROM_COMPOSITE_P(uint16 o)
{
	return ((rom_get_field0 (o) & 0x80) == COMPOSITE_FIELD0);
}
#else
#define RAM_COMPOSITE_P(o) ((ram_get_field0 (o) & 0x80) == COMPOSITE_FIELD0)
#define ROM_COMPOSITE_P(o) ((rom_get_field0 (o) & 0x80) == COMPOSITE_FIELD0)
#endif

// pair third byte : 000xxxxx
#define PAIR_FIELD2 0
#ifdef LESS_MACROS
uint8 RAM_PAIR_P(uint16 o)
{
	return (RAM_COMPOSITE_P (o) && ((ram_get_field2 (o) & 0xe0) == PAIR_FIELD2));
}
uint8 ROM_PAIR_P(uint16 o)
{
	return (ROM_COMPOSITE_P (o) && ((rom_get_field2 (o) & 0xe0) == PAIR_FIELD2));
}
#else
#define RAM_PAIR_P(o) (RAM_COMPOSITE_P (o) && ((ram_get_field2 (o) & 0xe0) == PAIR_FIELD2))
#define ROM_PAIR_P(o) (ROM_COMPOSITE_P (o) && ((rom_get_field2 (o) & 0xe0) == PAIR_FIELD2))
#endif

// symbol third byte : 001xxxxx
#define SYMBOL_FIELD2 0x20
#ifdef LESS_MACROS
uint8 RAM_SYMBOL_P(uint16 o)
{
	return (RAM_COMPOSITE_P (o) && ((ram_get_field2 (o) & 0xe0) == SYMBOL_FIELD2));
}
uint8 ROM_SYMBOL_P(uint16 o)
{
	return (ROM_COMPOSITE_P (o) && ((rom_get_field2 (o) & 0xe0) == SYMBOL_FIELD2));
}
#else
#define RAM_SYMBOL_P(o) (RAM_COMPOSITE_P (o) && ((ram_get_field2 (o) & 0xe0) == SYMBOL_FIELD2))
#define ROM_SYMBOL_P(o) (ROM_COMPOSITE_P (o) && ((rom_get_field2 (o) & 0xe0) == SYMBOL_FIELD2))
#endif

// string third byte : 010xxxxx
#define STRING_FIELD2 0x40
#ifdef LESS_MACROS
uint8 RAM_STRING_P(uint16 o)
{
	return (RAM_COMPOSITE_P (o) && ((ram_get_field2 (o) & 0xe0) == STRING_FIELD2));
}
uint8 ROM_STRING_P(uint16 o)
{
	return (ROM_COMPOSITE_P (o) && ((rom_get_field2 (o) & 0xe0) == STRING_FIELD2));
}
#else
#define RAM_STRING_P(o) (RAM_COMPOSITE_P (o) && ((ram_get_field2 (o) & 0xe0) == STRING_FIELD2))
#define ROM_STRING_P(o) (ROM_COMPOSITE_P (o) && ((rom_get_field2 (o) & 0xe0) == STRING_FIELD2))
#endif

// u8vector third byte : 011xxxxx
#define VECTOR_FIELD2 0x60
#ifdef LESS_MACROS
uint8 RAM_VECTOR_P(uint16 o)
{
	return (RAM_COMPOSITE_P (o) && ((ram_get_field2 (o) & 0xe0) == VECTOR_FIELD2));
}
uint8 ROM_VECTOR_P(uint16 o)
{
	return (ROM_COMPOSITE_P (o) && ((rom_get_field2 (o) & 0xe0) == VECTOR_FIELD2));
}
#else
#define RAM_VECTOR_P(o) (RAM_COMPOSITE_P (o) && ((ram_get_field2 (o) & 0xe0) == VECTOR_FIELD2))
#define ROM_VECTOR_P(o) (ROM_COMPOSITE_P (o) && ((rom_get_field2 (o) & 0xe0) == VECTOR_FIELD2))
#endif

// continuation third byte : 100xxxxx
#define CONTINUATION_FIELD2 0x80
#ifdef LESS_MACROS
uint8 RAM_CONTINUATION_P(uint16 o)
{
	return (RAM_COMPOSITE_P (o) && ((ram_get_field2 (o) & 0xe0) == CONTINUATION_FIELD2));
}
uint8 ROM_CONTINUATION_P(uint16 o)
{
	return (ROM_COMPOSITE_P (o) && ((rom_get_field2 (o) & 0xe0) == CONTINUATION_FIELD2));
}
#else
#define RAM_CONTINUATION_P(o) (RAM_COMPOSITE_P (o) && ((ram_get_field2 (o) & 0xe0) == CONTINUATION_FIELD2))
#define ROM_CONTINUATION_P(o) (ROM_COMPOSITE_P (o) && ((rom_get_field2 (o) & 0xe0) == CONTINUATION_FIELD2))
#endif

// closure first byte : 01Gxxxxx
// closures are only found in RAM
#define CLOSURE_FIELD0 0x40
#ifdef LESS_MACROS
uint8 RAM_CLOSURE_P(uint16 o)
{
	return ((ram_get_field0 (o) & 0xc0) == CLOSURE_FIELD0);
}
#else
#define RAM_CLOSURE_P(o) ((ram_get_field0 (o) & 0xc0) == CLOSURE_FIELD0)
#endif

#endif
