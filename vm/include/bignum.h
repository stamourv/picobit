#ifndef PICOBIT_BIGNUM_H
#define PICOBIT_BIGNUM_H

#include <picobit.h>

uint16 decode_int (obj o);
obj encode_int (uint16 n);

#ifdef CONFIG_BIGNUM_LONG


typedef obj integer;

/*
 * A `digit' is a numeric representation of one entry
 * of a bignum linked list. A `two_digit` is a numeric
 * representation for the cases where a result of
 * an operation is wider than a `digit'.
 */
typedef uint16 digit;
typedef uint32 two_digit;

#define digit_width (sizeof(digit) * 8)

#define obj_eq(x,y) ((x) == (y))
#define integer_hi_set(x,y) ram_set_car (x, y)

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
integer bitwise_xor (integer x, integer y);
integer bitwise_ior (integer x, integer y);

void bignum_gc_init();
void bignum_gc_mark();

#endif

#endif
