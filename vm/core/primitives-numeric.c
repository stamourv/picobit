#include <picobit.h>
#include <bignum.h>
#include <primitives.h>

PRIMITIVE(number?, number_p, 1)
{
	if (arg1 >= MIN_FIXNUM_ENCODING
	    && arg1 <= (MIN_FIXNUM_ENCODING + (MAX_FIXNUM - MIN_FIXNUM))) {
		arg1 = OBJ_TRUE;
	} else {
		if (IN_RAM(arg1)) {
			arg1 = encode_bool (RAM_BIGNUM_P(arg1));
		} else if (IN_ROM(arg1)) {
			arg1 = encode_bool (ROM_BIGNUM_P(arg1));
		} else {
			arg1 = OBJ_FALSE;
		}
	}
}

void decode_2_int_args () {
	a1 = decode_int (arg1);
	a2 = decode_int (arg2);
}

PRIMITIVE(=, equal, 2)
{
#ifdef CONFIG_BIGNUM_LONG
	arg1 = encode_bool(cmp (arg1, arg2) == 1);
#else
	decode_2_int_args ();
	arg1 = encode_bool(a1 == a2);
#endif
	arg2 = OBJ_FALSE;
}

PRIMITIVE(#%+, add, 2)
{
#ifdef CONFIG_BIGNUM_LONG
	arg1 = add (arg1, arg2);
#else
	decode_2_int_args ();
	arg1 = encode_int (a1 + a2);
#endif
	arg2 = OBJ_FALSE;
}

PRIMITIVE(#%-, sub, 2)
{
#ifdef CONFIG_BIGNUM_LONG
	arg1 = sub (arg1, arg2);
#else
	decode_2_int_args ();
	arg1 = encode_int (a1 - a2);
#endif
	arg2 = OBJ_FALSE;
}

PRIMITIVE(#%mul-non-neg, mul_non_neg, 2)
{
#ifdef CONFIG_BIGNUM_LONG
	arg1 = mulnonneg (arg1, arg2);
#else
	decode_2_int_args ();
	arg1 = encode_int (a1 * a2);
#endif
	arg2 = OBJ_FALSE;
}

PRIMITIVE(#%div-non-neg, div_non_neg, 2)
{
#ifdef CONFIG_BIGNUM_LONG
	if (obj_eq(arg2, ZERO)) {
		ERROR("quotient", "divide by 0");
	}

	arg1 = divnonneg (arg1, arg2);
#else
	decode_2_int_args ();

	if (a2 == 0) {
		ERROR("quotient", "divide by 0");
	}

	arg1 = encode_int (a1 / a2);
#endif

	arg2 = OBJ_FALSE;
}

PRIMITIVE(#%rem-non-neg, rem_non_neg, 2)
{
#ifdef CONFIG_BIGNUM_LONG
	if (obj_eq(arg2, ZERO)) {
		ERROR("remainder", "divide by 0");
	}

	arg3 = divnonneg (arg1, arg2);
	arg4 = mulnonneg (arg2, arg3);
	arg1 = sub(arg1, arg4);
	arg3 = OBJ_FALSE;
	arg4 = OBJ_FALSE;
#else
	decode_2_int_args ();

	if (a2 == 0) {
		ERROR("remainder", "divide by 0");
	}

	arg1 = encode_int (a1 % a2);
#endif

	arg2 = OBJ_FALSE;
}

PRIMITIVE(<, lt, 2)
{
#ifdef CONFIG_BIGNUM_LONG
	arg1 = encode_bool(cmp (arg1, arg2) < 1);
#else
	decode_2_int_args ();
	arg1 = encode_bool(a1 < a2);
#endif
	arg2 = OBJ_FALSE;
}

PRIMITIVE(>, gt, 2)
{
#ifdef CONFIG_BIGNUM_LONG
	arg1 = encode_bool(cmp (arg1, arg2) > 1);
#else
	decode_2_int_args ();
	arg1 = encode_bool(a1 > a2);
#endif
	arg2 = OBJ_FALSE;
}

PRIMITIVE(bitwise-ior, bitwise_ior, 2)
{
#ifdef CONFIG_BIGNUM_LONG
	arg1 = bitwise_ior(arg1, arg2);
#else
	decode_2_int_args ();
	arg1 = encode_int (a1 | a2);
#endif
	arg2 = OBJ_FALSE;
}

PRIMITIVE(bitwise-xor, bitwise_xor, 2)
{
#ifdef CONFIG_BIGNUM_LONG
	arg1 = bitwise_xor(arg1, arg2);
#else
	decode_2_int_args ();
	arg1 = encode_int (a1 ^ a2);
#endif
	arg2 = OBJ_FALSE;
}

// TODO add bitwise-and and bitwise-not
