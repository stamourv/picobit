#include <picobit.h>
#include <debug.h>

void show_type (obj o)
{
	printf("%04x : ", o);

	if (o == OBJ_FALSE) {
		printf("#f");
	} else if (o == OBJ_TRUE) {
		printf("#t");
	} else if (o == OBJ_NULL) {
		printf("()");
	} else if (o < MIN_ROM_ENCODING) {
		printf("fixnum");
	} else if (IN_RAM (o)) {
		if (RAM_BIGNUM(o)) {
			printf("ram bignum");
		} else if (RAM_PAIR(o)) {
			printf("ram pair");
		} else if (RAM_SYMBOL(o)) {
			printf("ram symbol");
		} else if (RAM_STRING(o)) {
			printf("ram string");
		} else if (RAM_VECTOR(o)) {
			printf("ram vector");
		} else if (RAM_CONTINUATION(o)) {
			printf("ram continuation");
		} else if (RAM_CLOSURE(o)) {
			printf("ram closure");
		}
	} else { // ROM
		if (ROM_BIGNUM(o)) {
			printf("rom bignum");
		} else if (ROM_PAIR(o)) {
			printf("rom pair");
		} else if (ROM_SYMBOL(o)) {
			printf("rom symbol");
		} else if (ROM_STRING(o)) {
			printf("rom string");
		} else if (ROM_VECTOR(o)) {
			printf("rom vector");
		} else if (ROM_CONTINUATION(o)) {
			printf("rom continuation");
		}

		// ROM closures don't exist
	}

	printf("\n");
}

void show (obj o)
{
#if 0
	printf ("[%d]", o);
#endif

	if (o == OBJ_FALSE) {
		printf ("#f");
	} else if (o == OBJ_TRUE) {
		printf ("#t");
	} else if (o == OBJ_NULL) {
		printf ("()");
	} else if (o <= (MIN_FIXNUM_ENCODING + (MAX_FIXNUM - MIN_FIXNUM))) {
		printf ("%d", DECODE_FIXNUM(o));
	} else {
		uint8 in_ram;

		if (IN_RAM(o)) {
			in_ram = 1;
		} else {
			in_ram = 0;
		}

		if ((in_ram && RAM_BIGNUM(o)) || (!in_ram && ROM_BIGNUM(o))) { // TODO fix for new bignums, especially for the sign, a -5 is displayed as 251
			printf ("%d", decode_int (o));
		} else if ((in_ram && RAM_COMPOSITE(o)) || (!in_ram && ROM_COMPOSITE(o))) {
			obj car;
			obj cdr;

			if ((in_ram && RAM_PAIR(o)) || (!in_ram && ROM_PAIR(o))) {
				if (in_ram) {
					car = ram_get_car (o);
					cdr = ram_get_cdr (o);
				} else {
					car = rom_get_car (o);
					cdr = rom_get_cdr (o);
				}

				printf ("(");

loop:

				show (car);

				if (cdr == OBJ_NULL) {
					printf (")");
				} else if ((IN_RAM(cdr) && RAM_PAIR(cdr))
					   || (IN_ROM(cdr) && ROM_PAIR(cdr))) {
					if (IN_RAM(cdr)) {
						car = ram_get_car (cdr);
						cdr = ram_get_cdr (cdr);
					} else {
						car = rom_get_car (cdr);
						cdr = rom_get_cdr (cdr);
					}

					printf (" ");
					goto loop;
				} else {
					printf (" . ");
					show (cdr);
					printf (")");
				}
			} else if ((in_ram && RAM_SYMBOL(o)) || (!in_ram && ROM_SYMBOL(o))) {
				printf ("#<symbol>");
			} else if ((in_ram && RAM_STRING(o)) || (!in_ram && ROM_STRING(o))) {
				printf ("#<string>");
			} else if ((in_ram && RAM_VECTOR(o)) || (!in_ram && ROM_VECTOR(o))) {
				printf ("#<vector %d>", o);
			} else {
				printf ("(");
				cdr = ram_get_car (o);
				car = ram_get_cdr (o);
				// ugly hack, takes advantage of the fact that pairs and
				// continuations have the same layout
				goto loop;
			}
		} else { // closure
			obj env;
			rom_addr pc;

			env = ram_get_car (o);
			pc = ram_get_entry (o);

			printf ("{0x%04x ", pc);
			show (env);
			printf ("}");
		}
	}

	fflush (stdout);
}

void show_state (rom_addr pc) {
	printf ("\n");
	printf ("pc=0x%04x bytecode=0x%02x env=", pc, rom_get (pc));
	show (env);
	printf (" cont=");
	show (cont);
	printf ("\n");
	fflush (stdout);
}

/*void print (obj o)
{
	show (o);
	printf ("\n");
	fflush (stdout);
}
*/