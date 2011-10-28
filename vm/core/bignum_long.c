#include <picobit.h>
#include <bignum.h>
#include <debug.h>
#include <gc.h>

/* Temps in bignum algorithms must be registered as roots too, since
 * GC can occur during bignum operations (they allocate).
 * Bignum ops can share variables as long as they don't interfere.
 *
 * Usage map:
 * bignum_tmp1 => ior xor add sub scale shr shl
 * bignum_tmp2 => shift_left mul
 * bignum_tmp3 => div mul
 * bignum_tmp4 => div mul
 * bignum_tmp5 => div
 */

static obj bignum_tmp1, bignum_tmp2, bignum_tmp3,
	bignum_tmp4, bignum_tmp5;

void bignum_gc_init()
{
	bignum_tmp1 = OBJ_FALSE;
	bignum_tmp2 = OBJ_FALSE;
	bignum_tmp3 = OBJ_FALSE;
	bignum_tmp4 = OBJ_FALSE;
	bignum_tmp5 = OBJ_FALSE;
}

void bignum_gc_mark()
{
	IF_GC_TRACE(printf("bignum_tmp1\n"));
	mark (bignum_tmp1);
	IF_GC_TRACE(printf("bignum_tmp2\n"));
	mark (bignum_tmp2);
	IF_GC_TRACE(printf("bignum_tmp3\n"));
	mark (bignum_tmp3);
	IF_GC_TRACE(printf("bignum_tmp4\n"));
	mark (bignum_tmp4);
	IF_GC_TRACE(printf("bignum_tmp5\n"));
	mark (bignum_tmp5);
}

integer make_integer (digit lo, integer hi)
{
	return alloc_ram_cell_init (BIGNUM_FIELD0 | (hi >> 8), hi, lo >> 8, lo);
}

integer integer_hi (integer x)
{
	if (IN_RAM(x)) {
		return ram_get_car (x);
	} else if (IN_ROM(x)) {
		return rom_get_car (x);
	} else if (x < (MIN_FIXNUM_ENCODING - MIN_FIXNUM)) {
		return NEG1;        /* negative fixnum */
	} else {
		return ZERO;        /* nonnegative fixnum */
	}
}

digit integer_lo (integer x)
{
	uint16 f2;

	if (IN_RAM(x)) {
		f2 = ram_get_field2 (x);
		return (f2 << 8) + ram_get_field3 (x);
	} else if (IN_ROM(x)) {
		f2 = rom_get_field2 (x);
		return (f2 << 8) + rom_get_field3 (x);
	} else {
		return DECODE_FIXNUM(x);
	}
}

integer norm (obj prefix, integer n)
{
	/* norm(prefix,n) returns a normalized integer whose value is the
	   integer n prefixed with the digits in prefix (a list of digits) */

	while (prefix != OBJ_FALSE) {
		digit d = integer_lo (prefix);
		obj temp = prefix;

		prefix = integer_hi (temp);

		if (obj_eq (n, ZERO)) {
			if (d <= MAX_FIXNUM) {
				n = ENCODE_FIXNUM (d);
				continue;
			}
		} else if (obj_eq (n, NEG1)) {
			// -1 is an illegal literal in SIXPIC, thus the double negative
			if (d >= (1 << digit_width) - (- MIN_FIXNUM)) {
				n = ENCODE_FIXNUM (d - (1 << digit_width));
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

	do {
		x = integer_hi (x);

		if (obj_eq (x, ZERO)) {
			return 0;
		}
	} while (!obj_eq (x, NEG1));

	return 1;
}

uint8 cmp (integer x, integer y)
{
	/* cmp(x,y) return 0 when x<y, 2 when x>y, and 1 when x=y */

	uint8 result = 1;
	digit xlo;
	digit ylo;

	for (;;) {
		if (obj_eq (x, ZERO) || obj_eq (x, NEG1)) {
			if (!obj_eq (x, y)) {
				if (negp (y)) {
					result = 2;
				} else {
					result = 0;
				}
			}

			break;
		}

		if (obj_eq (y, ZERO) || obj_eq (y, NEG1)) {
			if (negp (x)) {
				result = 0;
			} else {
				result = 2;
			}

			break;
		}

		xlo = integer_lo (x);
		ylo = integer_lo (y);
		x = integer_hi (x);
		y = integer_hi (y);

		if (xlo != ylo) {
			if (xlo < ylo) {
				result = 0;
			} else {
				result = 2;
			}
		}
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

integer shr (integer x)   // TODO have shift_right
{
	/* shr(x) returns the integer x shifted one bit to the right */

	// Note: we don't need to register x with the GC, even though we
	//  assign it. Our caller registered the original value of x, and
	//  we only cdr it down. Thus, any local value of x is pointed to
	//  by the original x, so we're good. Same situation in most other
	//  bignum operations.

	bignum_tmp1 = OBJ_FALSE;
	digit d;

	for (;;) {
		if (obj_eq (x, ZERO) || obj_eq (x, NEG1)) {
			bignum_tmp1 = norm (bignum_tmp1, x);
			break;
		}

		d = integer_lo (x);
		x = integer_hi (x);
		bignum_tmp1 =
		        make_integer ((d >> 1) |
		                      ((integer_lo (x) & 1) ? (1 << (digit_width - 1)) : 0),
		                      bignum_tmp1);
	}

	// clear the root then return
	obj tmp = bignum_tmp1;
	bignum_tmp1 = OBJ_FALSE;
	return tmp;
}

integer negative_carry (integer carry)
{
	if (carry) {
		return NEG1;
	} else {
		return ZERO;
	}
}

integer shl (integer x)
{
	/* shl(x) returns the integer x shifted one bit to the left */

	// These two are always 0 or -1, never allocated values.
	// No need to register them as GC roots.
	// Same for other negc variables in other operations.
	integer negc = ZERO; /* negative carry */
	integer temp;

	bignum_tmp1 = OBJ_FALSE;
	digit d;

	for (;;) {
		if (obj_eq (x, negc)) {
			bignum_tmp1 = norm (bignum_tmp1, x);
			break;
		}

		d = integer_lo (x);
		x = integer_hi (x);
		temp = negc;
		negc = negative_carry (d & (1 << (digit_width - 1)));
		bignum_tmp1 =
		        make_integer ((d << 1) | obj_eq (temp, NEG1), bignum_tmp1);
	}

	// clear the root then return
	obj tmp = bignum_tmp1;
	bignum_tmp1 = OBJ_FALSE;
	return tmp;
}

integer shift_left (integer x, uint16 n)
{
	/* shift_left(x,n) returns the integer x shifted n bits to the left */

	if (obj_eq (x, ZERO)) {
		return x;
	}

	bignum_tmp2 = x;

	while (n & (digit_width-1)) {
		bignum_tmp2 = shl (bignum_tmp2);
		n--;
	}

	while (n > 0) {
		bignum_tmp2 = make_integer (0, bignum_tmp2);
		n -= digit_width;
	}

	// clear the root then return
	obj tmp = bignum_tmp2;
	bignum_tmp2 = OBJ_FALSE;
	return tmp;
}

integer add (integer x, integer y)
{
	/* add(x,y) returns the sum of the integers x and y */

	integer negc = ZERO; /* negative carry */
	bignum_tmp1 = OBJ_FALSE; /* #f terminated for the norm function */
	digit dx;
	digit dy;

	for (;;) {
		if (obj_eq (x, negc)) {
			bignum_tmp1 = norm (bignum_tmp1, y);
			break;
		}

		if (obj_eq (y, negc)) {
			bignum_tmp1 = norm (bignum_tmp1, x);
			break;
		}

		dx = integer_lo (x);
		dy = integer_lo (y);
		dx = dx + dy; /* may wrap around */

		if (obj_eq (negc, ZERO)) {
			negc = negative_carry (dx < dy);
		} else {
			dx++; /* may wrap around */
			negc = negative_carry (dx <= dy);
		}

		x = integer_hi (x);
		y = integer_hi (y);

		bignum_tmp1 = make_integer (dx, bignum_tmp1);
	}

	// clear the root then return
	obj tmp = bignum_tmp1;
	bignum_tmp1 = OBJ_FALSE;
	return tmp;
}

integer invert (integer x)
{
	if (obj_eq (x, ZERO)) {
		return NEG1;
	} else {
		return ZERO;
	}
}

integer sub (integer x, integer y)
{
	/* sub(x,y) returns the difference of the integers x and y */
	integer negc = NEG1; /* negative carry */
	bignum_tmp1 = OBJ_FALSE;
	digit dx;
	digit dy;

	for (;;) {
		if (obj_eq (x, negc) && (obj_eq (y, ZERO) || obj_eq (y, NEG1))) {
			bignum_tmp1 = norm (bignum_tmp1, invert (y));
			break;
		}

		if (obj_eq (y, invert (negc))) {
			bignum_tmp1 = norm (bignum_tmp1, x);
			break;
		}

		dx = integer_lo (x);
		dy = ~integer_lo (y);
		dx = dx + dy; /* may wrap around */

		if (obj_eq (negc, ZERO)) {
			negc = negative_carry (dx < dy);
		} else {
			dx++; /* may wrap around */
			negc = negative_carry (dx <= dy);
		}

		x = integer_hi (x);
		y = integer_hi (y);

		bignum_tmp1 = make_integer (dx, bignum_tmp1);
	}

	// clear the root then return
	obj tmp = bignum_tmp1;
	bignum_tmp1 = OBJ_FALSE;
	return tmp; // TODO have macro for that.
}

integer scale (digit n, integer x)
{
	/* scale(n,x) returns the integer n*x */

	digit carry;
	two_digit m;

	if ((n == 0) || obj_eq (x, ZERO)) {
		return ZERO;
	}

	if (n == 1) {
		return x;
	}

	bignum_tmp1 = OBJ_FALSE;
	carry = 0;

	for (;;) {
		if (obj_eq (x, ZERO)) {
			if (carry <= MAX_FIXNUM) {
				bignum_tmp1 = norm (bignum_tmp1, ENCODE_FIXNUM (carry));
			} else {
				bignum_tmp1 = norm (bignum_tmp1, make_integer (carry, ZERO));
			}

			break;
		}

		if (obj_eq (x, NEG1)) {
			carry = carry - n;

			// -1 as a literal is wrong with SIXPIC, thus the double negative
			if (carry >= ((1<<digit_width) - (- MIN_FIXNUM))) {
				bignum_tmp1 = norm (bignum_tmp1, ENCODE_FIXNUM (carry));
			} else {
				bignum_tmp1 = norm (bignum_tmp1, make_integer (carry, NEG1));
			}

			break;
		}

		m = integer_lo (x);
		m = m * n + carry;

		x = integer_hi (x);
		carry = m >> digit_width;
		bignum_tmp1 = make_integer (m, bignum_tmp1);
	}

	// clear the root then return
	obj tmp = bignum_tmp1;
	bignum_tmp1 = OBJ_FALSE;
	return tmp;
}

integer mulnonneg (integer x, integer y)
{
	/* mulnonneg(x,y) returns the product of the integers x and y
	   where x is nonnegative */

	bignum_tmp3 = OBJ_FALSE;
	bignum_tmp4 = scale (integer_lo (x), y);

	for (;;) {
		bignum_tmp3 = make_integer (integer_lo (bignum_tmp4), bignum_tmp3);
		bignum_tmp4 = integer_hi (bignum_tmp4);
		x = integer_hi (x);

		if (obj_eq (x, ZERO)) {
			break;
		}

		// We need to register the result of scale because add can cause GC.
		bignum_tmp2 = scale (integer_lo (x), y);
		bignum_tmp4 = add (bignum_tmp4, bignum_tmp2);
	}

	obj tmp1 = bignum_tmp3;
	obj tmp2 = bignum_tmp4;
	bignum_tmp2 = OBJ_FALSE;
	bignum_tmp3 = OBJ_FALSE;
	bignum_tmp4 = OBJ_FALSE;
	return norm (tmp1, tmp2);
}

integer divnonneg (integer x, integer y)
{
	/* divnonneg(x,y) returns the quotient and remainder of
	   the integers x and y where x and y are nonnegative */

	// x and y end up pointing to newly allocated bignums, so we need
	// to register them with the GC.
	bignum_tmp4 = x;
	bignum_tmp5 = y;

	bignum_tmp3 = ZERO;
	uint16 lx = integer_length (bignum_tmp4);
	uint16 ly = integer_length (bignum_tmp5);

	if (lx >= ly) {
		lx = lx - ly;

		bignum_tmp5 = shift_left (bignum_tmp5, lx);

		do {
			bignum_tmp3 = shl (bignum_tmp3);

			if (cmp (bignum_tmp4, bignum_tmp5) >= 1) {
				bignum_tmp4 = sub (bignum_tmp4, bignum_tmp5);
				bignum_tmp3 = add (POS1, bignum_tmp3);
			}

			bignum_tmp5 = shr (bignum_tmp5);
		} while (lx-- != 0);
	}

	obj tmp = bignum_tmp3;
	bignum_tmp3 = OBJ_FALSE;
	bignum_tmp4 = OBJ_FALSE;
	return tmp;
}

integer bitwise_ior (integer x, integer y)
{
	/* returns the bitwise inclusive or of x and y */

	bignum_tmp1 = OBJ_FALSE;

	for (;;) {
		if (obj_eq(x, ZERO)) {
			obj tmp = bignum_tmp1;
			bignum_tmp1 = OBJ_FALSE;
			return norm(tmp, y);
		}

		if (obj_eq(x, NEG1)) {
			obj tmp = bignum_tmp1;
			bignum_tmp1 = OBJ_FALSE;
			return norm(tmp, x);
		}

		bignum_tmp1 = make_integer(integer_lo(x) | integer_lo(y),
		                           bignum_tmp1);
		x = integer_hi(x);
		y = integer_hi(y);
	}
}

integer bitwise_xor (integer x, integer y)   // TODO similar to ior (only diff is the test), abstract ?
{
	/* returns the bitwise inclusive or of x and y */

	bignum_tmp1 = OBJ_FALSE;

	for (;;) {
		if (obj_eq(x, ZERO)) {
			obj tmp = bignum_tmp1;
			bignum_tmp1 = OBJ_FALSE;
			return norm(tmp, y);
		}

		if (obj_eq(x, NEG1)) {
			obj tmp = bignum_tmp1;
			bignum_tmp1 = OBJ_FALSE;
			return norm(tmp, x);
		}

		bignum_tmp1 = make_integer(integer_lo(x) ^ integer_lo(y),
		                           bignum_tmp1);
		x = integer_hi(x);
		y = integer_hi(y);
	}
}

// supports up to 16 bits
// used only in primitives that use small numbers only
// for example, vector primitives
uint16 decode_int (obj o)
{
	if (o < MIN_FIXNUM_ENCODING) {
		TYPE_ERROR("decode_int.0", "integer");
	}

	if (o <= (MIN_FIXNUM_ENCODING + (MAX_FIXNUM - MIN_FIXNUM))) {
		return DECODE_FIXNUM(o);
	}

	if (IN_RAM(o)) {
		if (!RAM_BIGNUM_P(o)) {
			TYPE_ERROR("decode_int.1", "integer");
		}

		return (ram_get_field2 (o) << 8) | ram_get_field3 (o);
	} else if (IN_ROM(o)) {
		if (!ROM_BIGNUM_P(o)) {
			TYPE_ERROR("decode_int.2", "integer");
		}

		return (rom_get_field2 (o) << 8) | rom_get_field3 (o);
	} else {
		TYPE_ERROR("decode_int.3", "integer");
	}
}

// same purpose as decode_int
obj encode_int (uint16 n)
{
	if (n <= MAX_FIXNUM) {
		return ENCODE_FIXNUM(n);
	}

	return alloc_ram_cell_init (BIGNUM_FIELD0, ENCODE_FIXNUM(0),
	                            n >> 8, n & 0xff);
}
