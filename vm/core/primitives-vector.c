#include <picobit.h>
#include <bignum.h>
#include <primitives.h>
#include <gc.h>

PRIMITIVE(u8vector?, u8vector_p, 1)
{
	if (IN_RAM(arg1)) {
		arg1 = encode_bool (RAM_VECTOR_P(arg1));
	} else if (IN_ROM(arg1)) {
		arg1 = encode_bool (ROM_VECTOR_P(arg1));
	} else {
		arg1 = OBJ_FALSE;
	}
}

PRIMITIVE(#%make-u8vector, make_u8vector, 1)
{
	a1 = decode_int (arg1); // arg1 is length
	// TODO adapt for the new bignums

	arg1 = alloc_ram_cell_init (COMPOSITE_FIELD0 | (a1 >> 8),
	                            a1 & 0xff,
	                            VECTOR_FIELD2,
	                            0); // will be filled in later
	arg2 = alloc_vec_cell (a1, arg1);
	ram_set_cdr(arg1, arg2);
	arg2 = OBJ_FALSE;
}

PRIMITIVE(u8vector-ref, u8vector_ref, 2)
{
	a2 = decode_int (arg2);

	// TODO adapt for the new bignums
	if (IN_RAM(arg1)) {
		if (!RAM_VECTOR_P(arg1)) {
			TYPE_ERROR("u8vector-ref.0", "vector");
		}

		if (ram_get_car (arg1) <= a2) {
			ERROR("u8vector-ref.0", "vector index invalid");
		}

		arg1 = VEC_TO_RAM_OBJ(ram_get_cdr (arg1));
	} else if (IN_ROM(arg1)) {
		if (!ROM_VECTOR_P(arg1)) {
			TYPE_ERROR("u8vector-ref.1", "vector");
		}

		if (rom_get_car (arg1) <= a2) {
			ERROR("u8vector-ref.1", "vector index invalid");
		}

		arg1 = rom_get_cdr (arg1);

		while (a2--) {
			arg1 = rom_get_cdr (arg1);
		}

		// the contents are already encoded as fixnums
		arg1 = rom_get_car (arg1);
		arg2 = OBJ_FALSE;
		arg3 = OBJ_FALSE;
		arg4 = OBJ_FALSE;
		return;
	} else {
		TYPE_ERROR("u8vector-ref.2", "vector");
	}

	arg1 += (a2 >> 2);
	a2 %= 4;

	arg1 = encode_int (ram_get (OBJ_TO_RAM_ADDR(arg1, a2)));

	arg2 = OBJ_FALSE;
	arg3 = OBJ_FALSE;
	arg4 = OBJ_FALSE;
}

PRIMITIVE_UNSPEC(u8vector-set!, u8vector_set, 3)
	// TODO a lot in common with ref, abstract that
{
	a2 = decode_int (arg2); // TODO adapt for bignums
	a3 = decode_int (arg3);

	if (a3 > 255) {
		ERROR("u8vector-set!", "byte vectors can only contain bytes");
	}

	if (IN_RAM(arg1)) {
		if (!RAM_VECTOR_P(arg1)) {
			TYPE_ERROR("u8vector-set!.0", "vector");
		}

		if (ram_get_car (arg1) <= a2) {
			ERROR("u8vector-set!", "vector index invalid");
		}

		arg1 = VEC_TO_RAM_OBJ(ram_get_cdr (arg1));
	} else {
		TYPE_ERROR("u8vector-set!.1", "vector");
	}

	arg1 += (a2 >> 2);
	a2 %= 4;

	ram_set (OBJ_TO_RAM_ADDR(arg1, a2), a3);

	arg1 = OBJ_FALSE;
	arg2 = OBJ_FALSE;
	arg3 = OBJ_FALSE;
}

PRIMITIVE(u8vector-length, u8vector_length, 1)
{
	if (IN_RAM(arg1)) {
		if (!RAM_VECTOR_P(arg1)) {
			TYPE_ERROR("u8vector-length.0", "vector");
		}

		arg1 = encode_int (ram_get_car (arg1));
	} else if (IN_ROM(arg1)) {
		if (!ROM_VECTOR_P(arg1)) {
			TYPE_ERROR("u8vector-length.1", "vector");
		}

		arg1 = encode_int (rom_get_car (arg1));
	} else {
		TYPE_ERROR("u8vector-length.2", "vector");
	}
}
