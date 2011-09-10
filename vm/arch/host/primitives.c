#include <picobit.h>
#include <primitives.h>
#include <bignum.h>

#include <stdio.h>
#include <sys/time.h>

// most of this is for the host architecture
// there's some PIC18 code in there too
// it should eventually be moved to its own architecture

#ifdef CONFIG_ARCH_HOST

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

		if ((in_ram && RAM_BIGNUM_P(o)) || (!in_ram && ROM_BIGNUM_P(o))) { // TODO fix for new bignums, especially for the sign, a -5 is displayed as 251
			printf ("%d", decode_int (o));
		} else if ((in_ram && RAM_COMPOSITE_P(o)) || (!in_ram && ROM_COMPOSITE_P(o))) {
			obj car;
			obj cdr;

			if ((in_ram && RAM_PAIR_P(o)) || (!in_ram && ROM_PAIR_P(o))) {
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
				} else if ((IN_RAM(cdr) && RAM_PAIR_P(cdr))
				           || (IN_ROM(cdr) && ROM_PAIR_P(cdr))) {
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
			} else if ((in_ram && RAM_SYMBOL_P(o)) || (!in_ram && ROM_SYMBOL_P(o))) {
				printf ("#<symbol>");
			} else if ((in_ram && RAM_STRING_P(o)) || (!in_ram && ROM_STRING_P(o))) {
				printf ("#<string>");
			} else if ((in_ram && RAM_VECTOR_P(o)) || (!in_ram && ROM_VECTOR_P(o))) {
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

void print (obj o)
{
	show (o);
	printf ("\n");
	fflush (stdout);
}

#endif

PRIMITIVE_UNSPEC(print, print, 1)
{
#ifdef CONFIG_ARCH_HOST
	print (arg1);
#endif

	arg1 = OBJ_FALSE;
}


uint32 read_clock ()
{
	uint32 now = 0;

#ifdef  PICOBOARD2
	now = from_now( 0 );
#endif

#ifdef CONFIG_ARCH_HOST
#ifdef _WIN32
	static int32 start = 0;
	struct timeb tb;
	ftime (&tb);
	now = tb.time * 1000 + tb.millitm;

	if (start == 0) {
		start = now;
	}

	now -= start;
#else
	static uint32_t start = 0;
	struct timeval tv;

	if (gettimeofday (&tv, NULL) == 0) {
		now = tv.tv_sec * 1000 + tv.tv_usec / 1000;

		if (start == 0) {
			start = now;
		}

		now -= start;
	}

#endif
#endif

	return now;
}

PRIMITIVE(clock, clock, 0)
{
	arg1 = encode_int (read_clock ());
}

PRIMITIVE_UNSPEC(motor, motor, 2)
{
	decode_2_int_args ();

	if (a1 < 1 || a1 > 2 || a2 < -100 || a2 > 100) { // TODO since we now use unsigned values, we can't go backwards anymore
		ERROR("motor", "argument out of range");
	}

#ifdef  PICOBOARD2
	MOTOR_set( a1, a2 );
#endif

#ifdef CONFIG_ARCH_HOST
	printf ("motor %d -> power=%d\n", a1, a2);
	fflush (stdout);
#endif

	arg1 = OBJ_FALSE;
	arg2 = OBJ_FALSE;
}

PRIMITIVE_UNSPEC(led, led, 3)
{
	decode_2_int_args ();
	a3 = decode_int (arg3);

	if (a1 < 1 || a1 > 3) {
		ERROR("led", "argument out of range");
	}

#ifdef  PICOBOARD2
	LED_set( a1, a2, a3 );
#endif

#ifdef CONFIG_ARCH_HOST
	printf ("led %d -> duty=%d period=%d\n", a1, a2, a3 );
	fflush (stdout);
#endif

	arg1 = OBJ_FALSE;
	arg2 = OBJ_FALSE;
	arg3 = OBJ_FALSE;
}

PRIMITIVE_UNSPEC(#%led2-color, led2_color, 1)
{
	a1 = decode_int (arg1);

	if (a1 > 1) {
		ERROR("led2-colors", "argument out of range");
	}

#ifdef PICOBOARD2
	LED2_color_set( a1 );
#endif

#ifdef CONFIG_ARCH_HOST
	printf ("led2-color -> %s\n", (a1==0)?"green":"red");
	fflush (stdout);
#endif

	arg1 = OBJ_FALSE;
}

PRIMITIVE(#%getchar-wait, getchar_wait, 2)
{
	decode_2_int_args();
	a1 = read_clock () + a1;

	if (a2 < 1 || a2 > 3) {
		ERROR("getchar-wait", "argument out of range");
	}

	arg1 = OBJ_FALSE;

#ifdef PICOBOARD2
	{
		serial_port_set ports;
		ports = serial_rx_wait_with_timeout( a2, a1 );

		if (ports != 0) {
			arg1 = encode_int (serial_rx_read( ports ));
		}
	}
#endif

#ifdef CONFIG_ARCH_HOST
#ifdef _WIN32
	arg1 = OBJ_FALSE;

	do {
		if (_kbhit ())  {
			arg1 = encode_int (_getch ());
			break;
		}
	} while (read_clock () < a1);

#else
	arg1 = encode_int (getchar ());
#endif
#endif
}

PRIMITIVE(#%putchar, putchar, 2)
{
	decode_2_int_args ();

	if (a1 > 255 || a2 < 1 || a2 > 3) {
		ERROR("putchar", "argument out of range");
	}

#ifdef  PICOBOARD2
	serial_tx_write( a2, a1 );
#endif
#ifdef SIXPIC
	uart_write(a1);
#endif

#ifdef CONFIG_ARCH_HOST
	putchar (a1);
	fflush (stdout);
#endif

	arg1 = OBJ_FALSE;
	arg2 = OBJ_FALSE;
}

PRIMITIVE(beep, beep, 2)
{
	decode_2_int_args ();

	if (a1 < 1 || a1 > 255) {
		ERROR("beep", "argument out of range");
	}

#ifdef  PICOBOARD2
	beep( a1, from_now( a2 ) );
#endif

#ifdef CONFIG_ARCH_HOST
	printf ("beep -> freq-div=%d duration=%d\n", a1, a2 );
	fflush (stdout);
#endif

	arg1 = OBJ_FALSE;
	arg2 = OBJ_FALSE;
}

PRIMITIVE(adc, adc, 1)
{
	uint16 x;

	a1 = decode_int (arg1);

	if (a1 < 1 || a1 > 3) {
		ERROR("adc", "argument out of range");
	}

#ifdef  PICOBOARD2
	x = adc( a1 );
#endif

#ifdef CONFIG_ARCH_HOST
	x = read_clock () & 255;

	if (x > 127) {
		x = 256 - x;
	}

	x += 200;
#endif

//	arg1 = encode_int (x);
	arg1 = encode_int (0);
}

PRIMITIVE(sernum, sernum, 0)
{
	uint16 x = 0;

	arg1 = encode_int (x);
}


/*---------------------------------------------------------------------------*/

// networking primitives
// to enable them, compilation must be done with the -lpcap option

PRIMITIVE_UNSPEC(network-init, network_init, 0)
{
	// TODO maybe put in the initialization of the vm
#ifdef NETWORKING
	handle = pcap_open_live(INTERFACE, MAX_PACKET_SIZE, PROMISC, TO_MSEC, errbuf);

	if (handle == NULL) {
		ERROR("network-init", "interface not responding");
	}

#endif
}

PRIMITIVE_UNSPEC(network-cleanup, network_cleanup, 0)
{
	// TODO maybe put in halt ?
#ifdef NETWORKING
	pcap_close(handle);
#endif
}

PRIMITIVE(receive-packet-to-u8vector, receive_packet_to_u8vector, 1)
{
	// arg1 is the vector in which to put the received packet
	if (!RAM_VECTOR_P(arg1)) {
		TYPE_ERROR("receive-packet-to-u8vector", "vector");
	}

#ifdef NETWORKING
	// receive the packet in the buffer
	struct pcap_pkthdr header;
	const u_char *packet;

	packet = pcap_next(handle, &header);

	if (packet == NULL) {
		header.len = 0;
	}

	if (ram_get_car (arg1) < header.len) {
		ERROR("receive-packet-to-u8vector", "packet longer than vector");
	}

	if (header.len > 0) { // we have received a packet, write it in the vector
		arg2 = VEC_TO_RAM_OBJ(ram_get_cdr (arg1));
		arg1 = header.len; // we return the length of the received packet
		a1 = 0;

		while (a1 < arg1) {
			ram_set_fieldn (arg2, a1 % 4, (char)packet[a1]);
			a1++;
			arg2 += (a1 % 4) ? 0 : 1;
		}

		arg2 = OBJ_FALSE;
	} else { // no packet to be read
		arg1 = OBJ_FALSE;
	}

#endif
}

PRIMITIVE(send-packet-from-u8vector, send_packet_from_u8vector, 2)
{
	// arg1 is the vector which contains the packet to be sent
	// arg2 is the length of the packet
	// TODO only works with ram vectors for now
	if (!RAM_VECTOR_P(arg1)) {
		TYPE_ERROR("send-packet-from-vector!", "vector");
	}

	a2 = decode_int (arg2); // TODO fix for bignums
	a1 = 0;

#ifdef NETWORKING

	// TODO test if the length of the packet is longer than the length of the vector
	if (ram_get_car (arg1) < a2) {
		ERROR("send-packet-from-u8vector", "packet cannot be longer than vector");
	}

	arg1 = VEC_TO_RAM_OBJ(ram_get_cdr (arg1));

	// copy the packet to the output buffer
	while (a1 < a2) {
		buf[a1] = ram_get_fieldn (arg1, a1 % 4);
		a1++;
		arg1 += (a1 % 4) ? 0 : 1;
	}

	// TODO maybe I could just give pcap the pointer to the memory

	if (pcap_sendpacket(handle, buf, a2) < 0) { // TODO an error has occurred, can we reuse the interface ?
		arg1 = OBJ_FALSE;
	} else {
		arg1 = OBJ_TRUE;
	}

#endif

	arg2 = OBJ_FALSE;
}
