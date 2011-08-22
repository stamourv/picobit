#include <picobit.h>

// memory access

word ram_get_fieldn (obj o, word n)
{
	switch (n) {
	case 0:
		return ram_get_field0 (o);
	case 1:
		return ram_get_field1 (o);
	case 2:
		return ram_get_field2 (o);
	case 3:
		return ram_get_field3 (o);
	}
}
void ram_set_fieldn (obj o, uint8 n, word val)   // TODO have as a macro ?
{
	switch (n) {
	case 0:
		ram_set_field0 (o, val);
		break;
	case 1:
		ram_set_field1 (o, val);
		break;
	case 2:
		ram_set_field2 (o, val);
		break;
	case 3:
		ram_set_field3 (o, val);
		break;
	}
}

// these temporary variables are necessary with SIXPIC, or else the shift
// results will be 8 bits values, which is wrong
obj ram_get_car (obj o)
{
	uint16 tmp = ram_get_field0 (o) & 0x1f;
	return (tmp << 8) | ram_get_field1 (o);
}
obj rom_get_car (obj o)
{
	uint16 tmp = rom_get_field0 (o) & 0x1f;
	return (tmp << 8) | rom_get_field1 (o);
}
obj ram_get_cdr (obj o)
{
	uint16 tmp = ram_get_field2 (o) & 0x1f;
	return (tmp << 8) | ram_get_field3 (o);
}
obj rom_get_cdr (obj o)
{
	uint16 tmp = rom_get_field2 (o) & 0x1f;
	return (tmp << 8) | rom_get_field3 (o);
}

void ram_set_car (obj o, obj val)
{
	ram_set_field0 (o, (val >> 8) | (ram_get_field0 (o) & 0xe0));
	ram_set_field1 (o, val & 0xff);
}
void ram_set_cdr (obj o, obj val)
{
	ram_set_field2 (o, (val >> 8) | (ram_get_field2 (o) & 0xe0));
	ram_set_field3 (o, val & 0xff);
}

// function entry point
// the temporary variables are necessary with SIXPIC, see above
obj ram_get_entry (obj o)
{
	uint16 tmp  = ram_get_field2 (o);
	return ((tmp << 8) | ram_get_field3 (o));
}

obj get_global (uint8 i)
{
// globals occupy the beginning of ram, with 2 globals per word
	if (i & 1) {
		return ram_get_cdr (MIN_RAM_ENCODING + (i >> 1));
	} else {
		return ram_get_car (MIN_RAM_ENCODING + (i >> 1));
	}
}
void set_global (uint8 i, obj o)
{
	if (i & 1) {
		ram_set_cdr (MIN_RAM_ENCODING + (i >> 1), o);
	} else {
		ram_set_car (MIN_RAM_ENCODING + (i >> 1), o);
	}
}

// TODO generic functions (get_field0, get_car, etc) that work for both rom and ram were not used, are in garbage
