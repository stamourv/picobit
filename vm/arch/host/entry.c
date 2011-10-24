#include <stdio.h>
#include <stdlib.h>

#include <picobit.h>
#include <dispatch.h>
#include <gc.h>

uint8 ram_mem[RAM_BYTES + VEC_BYTES] = {0}, rom_mem[ROM_BYTES] = {0};

void error (char *prim, char *msg)
{
	fprintf (stderr, "ERROR: %s: %s\n", prim, msg);
	exit (1);
}

void type_error (char *prim, char *type)
{
	fprintf (stderr, "ERROR: %s: An argument of type %s was expected\n",
		 prim, type);
	exit (1);
}

void write_hex_nibble (int n)
{
	putchar ("0123456789ABCDEF"[n]);
}

void write_hex (uint8 n)
{
	write_hex_nibble (n >> 4);
	write_hex_nibble (n & 0x0f);
}

int hex (int c)
{
	if (c >= '0' && c <= '9') {
		return (c - '0');
	}

	if (c >= 'A' && c <= 'F') {
		return (c - 'A' + 10);
	}

	if (c >= 'a' && c <= 'f') {
		return (c - 'a' + 10);
	}

	return -1;
}

int read_hex_byte (FILE *f)
{
	int h1 = hex (fgetc (f));
	int h2 = hex (fgetc (f));

	if (h1 >= 0 && h2 >= 0) {
		return (h1<<4) + h2;
	}

	return -1;
}

int read_hex_file (char *filename)
{
	int c;
	FILE *f = fopen (filename, "r");
	int result = 0;
	int len;
	int a, a1, a2;
	int t;
	int b;
	int i;
	uint8 sum;
	int hi16 = 0;

	for (i=0; i<ROM_BYTES; i++) {
		rom_mem[i] = 0xff;
	}

	if (f != NULL) {
		while ((c = fgetc (f)) != EOF) {
			if ((c == '\r') || (c == '\n')) {
				continue;
			}

			if (c != ':' ||
			    (len = read_hex_byte (f)) < 0 ||
			    (a1 = read_hex_byte (f)) < 0 ||
			    (a2 = read_hex_byte (f)) < 0 ||
			    (t = read_hex_byte (f)) < 0) {
				break;
			}

			a = (a1 << 8) + a2;

			i = 0;
			sum = len + a1 + a2 + t;

			if (t == 0) {
next0:

				if (i < len) {
					unsigned long adr = ((unsigned long)hi16 << 16) + a - CODE_START;

					if ((b = read_hex_byte (f)) < 0) {
						break;
					}

					if (adr >= 0 && adr < ROM_BYTES) {
						rom_mem[adr] = b;
					}

					a = (a + 1) & 0xffff;
					i++;
					sum += b;

					goto next0;
				}
			} else if (t == 1) {
				if (len != 0) {
					break;
				}
			} else if (t == 4) {
				if (len != 2) {
					break;
				}

				if ((a1 = read_hex_byte (f)) < 0 ||
				    (a2 = read_hex_byte (f)) < 0) {
					break;
				}

				sum += a1 + a2;

				hi16 = (a1<<8) + a2;
			} else {
				break;
			}

			if ((b = read_hex_byte (f)) < 0) {
				break;
			}

			sum = -sum;

			if (sum != b) {
				printf ("*** HEX file checksum error (expected 0x%02x)\n", sum);
				break;
			}

			c = fgetc (f);

			if ((c != '\r') && (c != '\n')) {
				break;
			}

			if (t == 1) {
				result = 1;
				break;
			}
		}

		if (result == 0) {
			printf ("*** HEX file syntax error\n");
		}

		fclose (f);
	}

	return result;
}

void usage ()
{
	printf ("usage: sim file.hex\n");
	exit (1);
}

int main (int argc, char *argv[])
{
	int errcode = 0;

	if (argc != 2) {
		usage ();
	}

	if (!read_hex_file (argv[1])) {
		printf ("*** Could not read hex file \"%s\"\n", argv[1]);
	} else {
		if (rom_get (CODE_START+0) != 0xfb ||
		    rom_get (CODE_START+1) != 0xd7) {
			printf ("*** The hex file was not compiled with PICOBIT\n");
		} else {
			interpreter ();

#ifdef CONFIG_GC_DEBUG
			printf ("**************** memory needed = %d\n", max_live + 1);
#endif
		}
	}

	return errcode;
}
