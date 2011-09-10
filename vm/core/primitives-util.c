#include <picobit.h>
#include <primitives.h>
#include <gc.h>

PRIMITIVE(eq?, eq_p, 2)
{
	arg1 = encode_bool (arg1 == arg2);
	arg2 = OBJ_FALSE;
}

PRIMITIVE(not, not, 1)
{
	arg1 = encode_bool (arg1 == OBJ_FALSE);
}

PRIMITIVE(symbol?, symbol_p, 1)
{
	if (IN_RAM(arg1)) {
		arg1 = encode_bool (RAM_SYMBOL_P(arg1));
	} else if (IN_ROM(arg1)) {
		arg1 = encode_bool (ROM_SYMBOL_P(arg1));
	} else {
		arg1 = OBJ_FALSE;
	}
}

PRIMITIVE(boolean?, boolean_p, 1)
{
	arg1 = encode_bool (arg1 < 2);
}

PRIMITIVE(string?, string_p, 1)
{
	if (IN_RAM(arg1)) {
		arg1 = encode_bool (RAM_STRING_P(arg1));
	} else if (IN_ROM(arg1)) {
		arg1 = encode_bool (ROM_STRING_P(arg1));
	} else {
		arg1 = OBJ_FALSE;
	}
}

PRIMITIVE(string->list, string2list, 1)
{
	if (IN_RAM(arg1)) {
		if (!RAM_STRING_P(arg1)) {
			TYPE_ERROR("string->list.0", "string");
		}

		arg1 = ram_get_car (arg1);
	} else if (IN_ROM(arg1)) {
		if (!ROM_STRING_P(arg1)) {
			TYPE_ERROR("string->list.1", "string");
		}

		arg1 = rom_get_car (arg1);
	} else {
		TYPE_ERROR("string->list.2", "string");
	}
}

PRIMITIVE(list->string, list2string, 1)
{
	arg1 = alloc_ram_cell_init (COMPOSITE_FIELD0 | ((arg1 & 0x1f00) >> 8),
	                            arg1 & 0xff,
	                            STRING_FIELD2,
	                            0);
}
