/* file: "bignums.c" */

/*
 * Copyright 2004-2009 by Marc Feeley and Vincent St-Amour, All Rights Reserved.
 */

#include "picobit-vm.h"

#ifdef INFINITE_PRECISION_BIGNUMS

// TODO clean up all this

integer make_integer (digit lo, integer hi) {
/*   if(!hi && lo <= MAX_FIXNUM && lo >= MIN_FIXNUM) */
/*     return ENCODE_FIXNUM(lo);o */
  // TODO won't work, and the bignum functions are unaware of fixnums
  // TODO if I uncomment, segfaults every time this is called. no ill effect is noticed without it, but kept just in case
  return alloc_ram_cell_init (BIGNUM_FIELD0 | (hi >> 8), hi, lo >> 8, lo);
}

integer integer_hi (integer x) {
  if (IN_RAM(x))
    return ram_get_car (x);
  else if (IN_ROM(x))
    return rom_get_car (x);
  else if (x < (MIN_FIXNUM_ENCODING - MIN_FIXNUM))
    return NEG1; /* negative fixnum */
  else
    return ZERO; /* nonnegative fixnum */
}

digit integer_lo (integer x) {
  if (IN_RAM(x))
    return (((digit)ram_get_field2 (x)) << 8) + ram_get_field3 (x);
  else if (IN_ROM(x))
    return (((digit)rom_get_field2 (x)) << 8) + rom_get_field3 (x);
  else
    return DECODE_FIXNUM(x);
}

integer norm (obj prefix, integer n) {
  /* norm(prefix,n) returns a normalized integer whose value is the
     integer n prefixed with the digits in prefix (a list of digits) */

  while (prefix != NIL) {
    digit d = integer_lo (prefix);
    obj temp = prefix;
    
    prefix = integer_hi (temp);
    
    if (obj_eq (n, ZERO)) {
      if (d <= MAX_FIXNUM) {
	n = ENCODE_FIXNUM ((uint8)d);
	continue; // TODO with cast to unsigned, will it work for negative numbers ? or is it only handled in the next branch ?
      }
    }
    else if (obj_eq (n, NEG1)) {
      if (d >= (1<<digit_width) + MIN_FIXNUM) {
	n = ENCODE_FIXNUM (d - (1<<digit_width)); // TODO had a cast, origianlly to int8, changed to uint8 which didn't work (obviously, we use -1 here), is a cast necessary at all ?
	continue;
      }
    }

    integer_hi_set (temp, n);
    n = temp;
  }

  return n;
}

uint8 negp (integer x) {
  /* negp(x) returns true iff x is negative */

  do {
    x = integer_hi (x);
    if (obj_eq (x, ZERO)) return false;
  } while (!obj_eq (x, NEG1));

  return true;
}

int8 cmp (integer x, integer y) {
  /* cmp(x,y) return -1 when x<y, 1 when x>y, and 0 when x=y */
  
  int8 result = 0;
  digit xlo;
  digit ylo;

  for (;;) {
    if (obj_eq (x, ZERO) || obj_eq (x, NEG1)) {
      if (!obj_eq (x, y))
	{ if (negp (y)) result = 1; else result = -1; }
      break;
    }
    
    if (obj_eq (y, ZERO) || obj_eq (y, NEG1)) {
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

uint16 integer_length (integer x) {
  /* integer_length(x) returns the number of bits in the binary
     representation of the nonnegative integer x */
  
  uint16 result = 0;
  integer next;
  digit d;

  while (!obj_eq ((next = integer_hi (x)), ZERO)) {
    result += digit_width;
    x = next;
  }

  d = integer_lo (x);

  while (d > 0) {
    result++;
    d >>= 1;
  }

  return result;
}

integer shr (integer x) { // TODO have shift_right
  /* shr(x) returns the integer x shifted one bit to the right */

  obj result = NIL;
  digit d;

  for (;;) {
    if (obj_eq (x, ZERO) || obj_eq (x, NEG1)) {
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

integer negative_carry (integer carry) {
  if (carry)
    return NEG1;
  else
    return ZERO;
}

integer shl (integer x) {
  /* shl(x) returns the integer x shifted one bit to the left */

  integer negc = ZERO; /* negative carry */
  integer temp;
  obj result = NIL;
  digit d;

  for (;;) {
    if (obj_eq (x, negc)) {
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

integer shift_left (integer x, uint16 n) { // TODO have the primitves been changed for this and right ?
  /* shift_left(x,n) returns the integer x shifted n bits to the left */
  
  if (obj_eq (x, ZERO))
    return x;

  while (n & (digit_width-1)) {
    x = shl (x);
    n--;
  }
  
  while (n > 0) {
    x = make_integer (0, x);
    n -= digit_width;
  }

  return x;
}

integer add (integer x, integer y) {
  /* add(x,y) returns the sum of the integers x and y */
  
  integer negc = ZERO; /* negative carry */
  obj result = NIL; /* nil terminated for the norm function */
  digit dx;
  digit dy;

  for (;;) {
    if (obj_eq (x, negc)) {
      result = norm (result, y);
      break;
    }

    if (obj_eq (y, negc)) {
      result = norm (result, x);
      break;
    }

    dx = integer_lo (x);
    dy = integer_lo (y);
    dx = dx + dy; /* may wrap around */
    
    if (obj_eq (negc, ZERO))
      negc = negative_carry (dx < dy);
    else {
      dx++; /* may wrap around */
      negc = negative_carry (dx <= dy);
    }

    x = integer_hi (x);
    y = integer_hi (y);
    
    result = make_integer (dx, result);
  }
  
  return result;
}

integer invert (integer x) {
  if (obj_eq (x, ZERO))
    return NEG1;
  else
    return ZERO;
}

integer sub (integer x, integer y) {
  /* sub(x,y) returns the difference of the integers x and y */
  integer negc = NEG1; /* negative carry */
  obj result = NIL;
  digit dx;
  digit dy;

  for (;;) {
    if (obj_eq (x, negc) && (obj_eq (y, ZERO) || obj_eq (y, NEG1))) {
      result = norm (result, invert (y));
      break;
    }

    if (obj_eq (y, invert (negc))) {
      result = norm (result, x);
      break;
    }

    dx = integer_lo (x);
    dy = ~integer_lo (y);
    dx = dx + dy; /* may wrap around */
    
    if (obj_eq (negc, ZERO))
      negc = negative_carry (dx < dy);
    else {
      dx++; /* may wrap around */
      negc = negative_carry (dx <= dy);
    }
    
    x = integer_hi (x);
    y = integer_hi (y);
    
    result = make_integer (dx, result);
  }

  return result;
}

integer neg (integer x) {
  /* neg(x) returns the integer -x */

  return sub (ZERO, x);
}

integer scale (digit n, integer x) {
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
  
  for (;;) {
    if (obj_eq (x, ZERO)){
      if (carry <= MAX_FIXNUM)
	result = norm (result, ENCODE_FIXNUM ((uint8)carry));
      else
	result = norm (result, make_integer (carry, ZERO));
      break;
    }
    
    if (obj_eq (x, NEG1)) {
      carry = carry - n;
      if (carry >= ((1<<digit_width) + MIN_FIXNUM))
	result = norm (result, ENCODE_FIXNUM ((uint8)carry));
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

integer mulnonneg (integer x, integer y) {
  /* mulnonneg(x,y) returns the product of the integers x and y
     where x is nonnegative */

  obj result = NIL;
  integer s = scale (integer_lo (x), y);
  
  for (;;) {
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
integer divnonneg (integer x, integer y) {
  /* divnonneg(x,y) returns the quotient and remainder of
     the integers x and y where x and y are nonnegative */

  integer result = ZERO;
  uint16 lx = integer_length (x);
  uint16 ly = integer_length (y);

  if (lx >= ly) {
    lx = lx - ly;
    
    y = shift_left (y, lx);
    
    do {
      result = shl (result);
      if (cmp (x, y) >= 0) {
	x = sub (x, y);
	result = add (POS1, result);
      }
      y = shr (y);
    } while (lx-- != 0);
  }

  return result;
}

// used only in primitives that use small numbers only
// for example, vector primitives
int32 decode_int (obj o) {
  int8 result;
  if (o < MIN_FIXNUM_ENCODING)
    TYPE_ERROR("decode_int.0", "integer");
  
  if (o <= (MIN_FIXNUM_ENCODING + (MAX_FIXNUM - MIN_FIXNUM)))
    return DECODE_FIXNUM(o);
  
  if (IN_RAM(o)) {
    if (!RAM_BIGNUM(o))
      TYPE_ERROR("decode_int.1", "integer");
    return ram_get_field3 (o);
  }
  else if (IN_ROM(o)) {
    if (!ROM_BIGNUM(o))
      TYPE_ERROR("decode_int.2", "integer");
    return rom_get_field3 (o);
  }
  else
    TYPE_ERROR("decode_int.3", "integer");
}

// same purpose as decode_int
obj encode_int (int32 n) {
  if (n >= MIN_FIXNUM && n <= MAX_FIXNUM) {
    return ENCODE_FIXNUM(n);
  }
  
  return alloc_ram_cell_init (BIGNUM_FIELD0, ENCODE_FIXNUM(0), n >> 8, n);
}

#endif
