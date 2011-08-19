#ifndef PICOBIT_BIGNUMS_H
#define PICOBIT_BIGNUMS_H

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

#endif
