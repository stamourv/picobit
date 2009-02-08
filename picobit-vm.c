/* file: "picobit-vm.c" */

/*
 * Copyright 2004-2009 by Marc Feeley and Vincent St-Amour, All Rights Reserved.
 *
 * History:
 *
 *   15/08/2004  Release of version 1
 *   06/07/2008  Modified for PICOBOARD2_R3
 *   18/07/2008  Modified to use new object representation
 *   17/12/2008  Release of version 2
 */

#include "picobit-vm.h"

/*---------------------------------------------------------------------------*/

// error handling

void error (char *prim, char *msg) {
  printf ("ERROR: %s: %s\n", prim, msg);
  exit (1);
}

void type_error (char *prim, char *type) {
  printf ("ERROR: %s: An argument of type %s was expected\n", prim, type);
  exit (1);
}

/*---------------------------------------------------------------------------*/

// memory access

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
word ram_get_fieldn (obj o, word n) { // TODO have as a macro ?
  switch (n) {
  case 0: return ram_get_field0 (o);
  case 1: return ram_get_field1 (o);
  case 2: return ram_get_field2 (o);
  case 3: return ram_get_field3 (o);
  }
}
void ram_set_field0 (obj o, word val) { RAM_SET_FIELD0_MACRO(o,val); }
void ram_set_field1 (obj o, word val) { RAM_SET_FIELD1_MACRO(o,val); }
void ram_set_field2 (obj o, word val) { RAM_SET_FIELD2_MACRO(o,val); }
void ram_set_field3 (obj o, word val) { RAM_SET_FIELD3_MACRO(o,val); }
void ram_set_fieldn (obj o, uint8 n, word val) { // TODO have as a macro ?
  switch (n) {
  case 0: ram_set_field0 (o, val); break;
  case 1: ram_set_field1 (o, val); break;
  case 2: ram_set_field2 (o, val); break;
  case 3: ram_set_field3 (o, val); break;
  }  
}
word rom_get_field0 (obj o) { return ROM_GET_FIELD0_MACRO(o); }
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

void ram_set_car (obj o, obj val) {
  ram_set_field0 (o, (val >> 8) | (ram_get_field0 (o) & 0xe0));
  ram_set_field1 (o, val & 0xff);
}
void ram_set_cdr (obj o, obj val) {
  ram_set_field2 (o, (val >> 8) | (ram_get_field2 (o) & 0xe0));
  ram_set_field3 (o, val & 0xff);
}

// function entry point
obj ram_get_entry (obj o) {
  return (((ram_get_field0 (o) & 0x1f) << 11)
	  | (ram_get_field1 (o) << 3)
	  | (ram_get_field2 (o) >> 5));
}
obj rom_get_entry (obj o){
  return (((rom_get_field0 (o) & 0x1f) << 11)
	  | (rom_get_field1 (o) << 3)
	  | (rom_get_field2 (o) >> 5));
}

obj get_global (uint8 i) {
// globals occupy the beginning of ram, with 2 globals per word
  if (i & 1)
    return ram_get_cdr (MIN_RAM_ENCODING + (i / 2));
  else
    return ram_get_car (MIN_RAM_ENCODING + (i / 2));
}
void set_global (uint8 i, obj o) {
  if (i & 1)
    ram_set_cdr (MIN_RAM_ENCODING + (i / 2), o);
  else
    ram_set_car (MIN_RAM_ENCODING + (i / 2), o);
}


obj get_field0 (obj o) { // TODO these are not used yet, will they be useful at all ? FOOBAR find a use, or trash
  if (IN_RAM(o))
    return ram_get_field0 (o);
  else
    return rom_get_field0 (o);
}
obj get_field1 (obj o) {
  if (IN_RAM(o))
    return ram_get_field1 (o);
  else
    return rom_get_field1 (o);
}
obj get_field2 (obj o) {
  if (IN_RAM(o))
    return ram_get_field2 (o);
  else
    return rom_get_field2 (o);
}
obj get_field3 (obj o) {
  if (IN_RAM(o))
    return ram_get_field3 (o);
  else
    return rom_get_field3 (o);
}

obj get_car (obj o) { // TODO used ?
  if (IN_RAM(o))
    return ram_get_car (o);
  else
    return rom_get_car (o);
}
obj get_cdr (obj o) {
  if (IN_RAM(o))
    return ram_get_cdr (o);
  else
    return rom_get_cdr (o);
}

obj get_entry (obj o) { // TODO used ?
  if (IN_RAM(o))
    return ram_get_entry (o);
  else
    return rom_get_entry (o);
}

/*---------------------------------------------------------------------------*/

#ifdef INFINITE_PRECISION_BIGNUMS

int8 decode_int8 (obj o) // TODO never used except in decode_int, clean useless functions
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

  if (u >= 128) // negative
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

#ifdef INFINITE_PRECISION_BIGNUMS

#define obj_eq(x,y) ((x) == (y))

#define integer_hi_set(x,y) ram_set_car (x, y)
// TODO FOOBIGNUMS won't work, I think, will erase next pointer (or set it only in part) ACTUALLY, this is probably supposed to change the pointer. changed field1, npw changes the whole car

#define ZERO ENCODE_FIXNUM(0)
#define NEG1 (ZERO-1)
#define POS1 (ZERO+1)

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
    return ram_get_car (x);
  else if (IN_ROM(x))
    return rom_get_car (x);
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
              n = ENCODE_FIXNUM ((uint8)d);
              continue; // TODO with cast to unsigned, will it work for negative numbers ? or is it only handled in the next branch ?
            }
        }
      else if (obj_eq (n, NEG1))
        {
          if (d >= (1<<digit_width) + MIN_FIXNUM)
            {
              n = ENCODE_FIXNUM (d - (1<<digit_width)); // TODO had a cast, origianlly to int8, changed to uint8 which didn't work (obviously, we use -1 here), is a cast necessary at all ?
              continue;
            }
        }

      integer_hi_set (temp, n);
      n = temp;
    }

  return n;
}

uint8 negp (integer x)
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

  for (;;)
    {
      if (obj_eq (x, negc))
        {
          result = norm (result, x);
          break;
        }

      d = integer_lo (x);
      x = integer_hi (x);
      temp = negc;
      negc = negative_carry (d & (1<<(digit_width-1))); // TODO right side is constant, and sixpic has no constant folding
      result = make_integer ((d << 1) | obj_eq (temp, NEG1), result);
    }

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

// TODO have functions mul and div that handle negative arguments ? currently, the logic is in prim_mul and prim_div
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

#ifdef WORKSTATION
void p (integer n)
{
  long long x; // TODO long long is 32 bits here, what about on a 64 bit machine ?
  x = ((long long)integer_lo (integer_hi (integer_hi (integer_hi (n))))<<48)+
    ((long long)integer_lo (integer_hi (integer_hi (n)))<<32)+
    ((long long)integer_lo (integer_hi (n))<<16)+
    (long long)integer_lo (n);
  printf ("%lld ", x);
  // TODO test for hex output, to avoid signedness problems
/*   printf("%x %x %x %x\n", // TODO prob, if a lower part is 0, will show 0, not 0000 */
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
    // should show powers of 2 incerasing, then decreasing
    for (i=1; i<=34; i++)
      {
	printf("\nloop-1 : i=%d len=%d ", i, integer_length(n));
        p (n);
        n = shl(n);
      }
    for (i=1; i<=35; i++)
      {
	printf("\nloop-2 : i=%d len=%d ", i, integer_length(n));
        p (n);
        n = shr(n);
      }
  }

  {
    integer n = shift_left (four, 5);
    int i;

    for (i=0; i<=14; i++)
      {
	printf("\nloop-3 : i=%d len=%d ", i);
        p (shift_left (n, i*4));
      }
  }

  printf("\n");
  p (add (enc (32768), enc (32768))); printf("\n"); // 65536
  p (add (enc (32768+(65536*65535LL)), enc (32768))); printf("\n"); // 4294967296

  p (sub (enc (32768), enc (-32768))); printf("\n"); // 65536
  p (sub (enc (32768+(65536*65535LL)), enc (-32768))); printf("\n"); // 4294967296

  p (sub (enc (32768), enc (32769))); printf("\n"); // -1
  p (sub (enc (32768), enc (132768))); printf("\n"); // -100000
  p (add(sub (enc (32768), enc (32769)), enc(1000))); printf("\n"); // 999

  // TODO mul was scrapped, logic is now in prim_mul
/*   p (mul (enc (123456789), enc (1000000000))); printf("\n"); // 123456789000000000 */
/*   p (mul (enc (123456789), enc (-1000000000))); printf("\n"); // -123456789000000000 */
/*   p (mul (enc (-123456789), enc (1000000000))); printf("\n"); // -123456789000000000 */
/*   p (mul (enc (-123456789), enc (-1000000000))); printf("\n"); // 123456789000000000 */
/*   p (mul (enc (-123456789), neg (enc (1000000000)))); printf("\n"); // 123456789000000000 */

  p (divnonneg (enc (10000000-1), enc (500000))); printf("\n"); // 19

  printf ("done\n");

  exit (0);
}
#endif

#endif


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
  a1 = negp (arg1);
  a2 = negp (arg2); // -1 if negative
  arg1 = mulnonneg (a1 ? neg(arg1) : arg1,
		    a2 ? neg(arg2) : arg2);
  if (a1 + a2 == 1) // only one of the 2 was negative
    arg1 = neg(arg1);
#else
  decode_2_int_args ();
  arg1 = encode_int (a1 * a2);
#endif
  arg2 = OBJ_FALSE;
}

void prim_div (void)
{
#ifdef INFINITE_PRECISION_BIGNUMS
  if (obj_eq(arg2, ZERO))
    ERROR("quotient", "divide by 0");
  a1 = negp (arg1);
  a2 = negp (arg2); // -1 if negative
  arg1 = divnonneg (a1 ? neg(arg1) : arg1,
		    a2 ? neg(arg2) : arg2);
  if (a1 + a2 == 1) // only one of the 2 was negative
    arg1 = neg(arg1);
#else
  decode_2_int_args ();
  if (a2 == 0)
    ERROR("quotient", "divide by 0");
  arg1 = encode_int (a1 / a2);
#endif
  arg2 = OBJ_FALSE;
}

void prim_rem (void)
{
#ifdef INFINITE_PRECISION_BIGNUMS
  if (obj_eq(arg2, ZERO))
    ERROR("remainder", "divide by 0");
  if (negp(arg1) || negp(arg2))
    ERROR("remainder", "only positive numbers are supported");
  // TODO fix this to handle negatives
  // TODO logic quite similar to mul and div (likely, once we fix), abstract ?
  arg3 = divnonneg (arg1, arg2);
  arg4 = mulnonneg (arg2, arg3);
  arg1 = sub(arg1, arg4 );
  arg3 = OBJ_FALSE;
  arg4 = OBJ_FALSE;
#else
  decode_2_int_args ();
  if (a2 == 0)
    ERROR("remainder", "divide by 0");
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

#ifdef TEST_BIGNUM
  test();
#endif
  
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
