#ifndef PICOBIT_VM_H
#define PICOBIT_VM_H

#define DEBUG_not
#define DEBUG_GC_not
#define INFINITE_PRECISION_BIGNUMS

/*---------------------------------------------------------------------------*/

// types

#ifndef SIXPIC
// these types are already defined in SIXPIC
typedef char int8;
typedef short int16;
typedef long int32;
typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned long uint32;
#endif

typedef uint8 word;

typedef uint16 ram_addr;
typedef uint16 rom_addr;

// pointers are 13 bits
typedef uint16 obj;

/*---------------------------------------------------------------------------*/

// environment

#ifdef PICOBOARD2
#define ROBOT
#endif

#ifdef HI_TECH_C
#define ROBOT
#endif

#ifdef MCC18
#define ROBOT
#endif

#ifdef SIXPIC
#define ROBOT
#endif

#ifndef ROBOT
#define WORKSTATION
#endif


#ifdef HI_TECH_C

#include <pic18.h>

static volatile near uint8 FW_VALUE_UP       @ 0x33;
static volatile near uint8 FW_VALUE_HI       @ 0x33;
static volatile near uint8 FW_VALUE_LO       @ 0x33;

#define ACTIVITY_LED1_LAT LATB
#define ACTIVITY_LED1_BIT 5
#define ACTIVITY_LED2_LAT LATB
#define ACTIVITY_LED2_BIT 4
static volatile near bit ACTIVITY_LED1 @ ((unsigned)&ACTIVITY_LED1_LAT*8)+ACTIVITY_LED1_BIT;
static volatile near bit ACTIVITY_LED2 @ ((unsigned)&ACTIVITY_LED2_LAT*8)+ACTIVITY_LED2_BIT;

#endif


#ifdef WORKSTATION

#include <stdio.h>
#include <stdlib.h>

#ifdef NETWORKING
#include <pcap.h>
#define MAX_PACKET_SIZE BUFSIZ
#define PROMISC 1
#define TO_MSEC 1
char errbuf[PCAP_ERRBUF_SIZE];
pcap_t *handle;
#define INTERFACE "eth0"
char buf [MAX_PACKET_SIZE]; // buffer for writing
#endif

#ifdef _WIN32

#include <sys/types.h>
#include <sys/timeb.h>
#include <conio.h>

#else

#include <sys/time.h>

#endif

#endif

/*---------------------------------------------------------------------------*/

// miscellaneous definitions
// TODO put at the end ?

// TODO these 2 are only used in negp, use them elsewhere ?
#define true  1
#define false 0

#define CODE_START 0x8000

/*---------------------------------------------------------------------------*/

// debugging

#ifdef DEBUG
#define IF_TRACE(x) x
#define IF_GC_TRACE(x) x
#else
#define IF_TRACE(x)
#define IF_GC_TRACE(x)
#endif

#ifdef DEBUG_GC
int max_live;
#endif

/*---------------------------------------------------------------------------*/

// error handling

#ifdef HI_TECH_C
void halt_with_error ()
{
        while(1) {
                ;
        }
}
#endif

#ifdef WORKSTATION
#define ERROR(prim, msg) error (prim, msg)
#define TYPE_ERROR(prim, type) type_error (prim, type)
void error (char *prim, char *msg);
void type_error (char *prim, char *type);
#else
#define ERROR(prim, msg) halt_with_error()
#define TYPE_ERROR(prim, type) halt_with_error()
#endif

/*---------------------------------------------------------------------------*/

// debugging functions

#ifdef WORKSTATION
void show_type (obj o);
void show_state (rom_addr pc);
#endif

/*---------------------------------------------------------------------------*/

#endif
