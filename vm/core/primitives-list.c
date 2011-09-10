#include <picobit.h>
#include <primitives.h>

PRIMITIVE(pair?, pair_p, 1)
{
	if (IN_RAM(arg1)) {
		arg1 = encode_bool (RAM_PAIR_P(arg1));
	} else if (IN_ROM(arg1)) {
		arg1 = encode_bool (ROM_PAIR_P(arg1));
	} else {
		arg1 = OBJ_FALSE;
	}
}

PRIMITIVE(cons, cons, 2)
{
	arg1 = cons (arg1, arg2);
	arg2 = OBJ_FALSE;
}

PRIMITIVE(car, car, 1)
{
	if (IN_RAM(arg1)) {
		if (!RAM_PAIR_P(arg1)) {
			TYPE_ERROR("car.0", "pair");
		}

		arg1 = ram_get_car (arg1);
	} else if (IN_ROM(arg1)) {
		if (!ROM_PAIR_P(arg1)) {
			TYPE_ERROR("car.1", "pair");
		}

		arg1 = rom_get_car (arg1);
	} else {
		TYPE_ERROR("car.2", "pair");
	}
}

PRIMITIVE(cdr, cdr, 1)
{
	if (IN_RAM(arg1)) {
		if (!RAM_PAIR_P(arg1)) {
			TYPE_ERROR("cdr.0", "pair");
		}

		arg1 = ram_get_cdr (arg1);
	} else if (IN_ROM(arg1)) {
		if (!ROM_PAIR_P(arg1)) {
			TYPE_ERROR("cdr.1", "pair");
		}

		arg1 = rom_get_cdr (arg1);
	} else {
		TYPE_ERROR("cdr.2", "pair");
	}
}

PRIMITIVE_UNSPEC(set-car!, set_car_bang, 2)
{
	if (IN_RAM(arg1)) {
		if (!RAM_PAIR_P(arg1)) {
			TYPE_ERROR("set-car!.0", "pair");
		}

		ram_set_car (arg1, arg2);
		arg1 = OBJ_FALSE;
		arg2 = OBJ_FALSE;
	} else {
		TYPE_ERROR("set-car!.1", "pair");
	}
}

PRIMITIVE_UNSPEC(set-cdr!, set_cdr_bang, 2)
{
	if (IN_RAM(arg1)) {
		if (!RAM_PAIR_P(arg1)) {
			TYPE_ERROR("set-cdr!.0", "pair");
		}

		ram_set_cdr (arg1, arg2);
		arg1 = OBJ_FALSE;
		arg2 = OBJ_FALSE;
	} else {
		TYPE_ERROR("set-cdr!.1", "pair");
	}
}

PRIMITIVE(null?, null_p, 1)
{
	arg1 = encode_bool (arg1 == OBJ_NULL);
}
