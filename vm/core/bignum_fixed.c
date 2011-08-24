#include <picobit.h>
#include <bignum.h>
#include <gc.h>

/*
 * Implementation of bignums as fixed-precision,
 * 24-bit integers.
 */

uint16 decode_int (obj o)
{
	uint16 u; // TODO should be 32, but is lost anyway since this returns a uint16
	uint16 h;
	uint8  l;

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

		u = ram_get_field1 (o);
		h = ram_get_field2 (o);
		l = ram_get_field3 (o);
	} else if (IN_ROM(o)) {
		if (!ROM_BIGNUM_P(o)) {
			TYPE_ERROR("decode_int.2", "integer");
		}

		u = rom_get_field1 (o);
		h = rom_get_field2 (o);
		l = rom_get_field3 (o);
	} else {
		TYPE_ERROR("decode_int.3", "integer");
	}

	if (u >= 128) { // negative
		return ((((u - 256) << 8) + h) << 8) + l;        // TODO ints are all 16 bits, 24 bits won't work
	}

	return (((u << 8) + h) << 8) + l;
}

obj encode_int (uint16 n)   // TODO does not use the full 24 bits
{
	if (n >= MIN_FIXNUM && n <= MAX_FIXNUM) {
		return ENCODE_FIXNUM(n);
	}

	return alloc_ram_cell_init (BIGNUM_FIELD0, n >> 16, n >> 8, n);
}
