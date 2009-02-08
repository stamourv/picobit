/* file: "debug.c" */

/*
 * Copyright 2004-2009 by Marc Feeley and Vincent St-Amour, All Rights Reserved.
 */

// debugging functions

#include "picobit-vm.h"
// needs some memory access functions defined above
#include "objects.h"

#ifdef WORKSTATION
void show_type (obj o) // for debugging purposes
  {
    printf("%d : ", o);
    if (o == OBJ_FALSE) printf("#f");
    else if (o == OBJ_TRUE) printf("#t");
    else if (o == OBJ_NULL) printf("()");
    else if (o < MIN_ROM_ENCODING) printf("fixnum");
    else if (IN_RAM (o))
      {
	if (RAM_BIGNUM(o)) printf("ram bignum");
	else if (RAM_PAIR(o)) printf("ram pair");
	else if (RAM_SYMBOL(o)) printf("ram symbol");
	else if (RAM_STRING(o)) printf("ram string");
	else if (RAM_VECTOR(o)) printf("ram vector");
	else if (RAM_CONTINUATION(o)) printf("ram continuation");
	else if (RAM_CLOSURE(o)) printf("ram closure");
      }
    else // ROM
      {
	if (ROM_BIGNUM(o)) printf("rom bignum");
	else if (ROM_PAIR(o)) printf("rom pair");
	else if (ROM_SYMBOL(o)) printf("rom symbol");
	else if (ROM_STRING(o)) printf("rom string");
	else if (ROM_VECTOR(o)) printf("rom vector");
	else if (ROM_CONTINUATION(o)) printf("rom continuation");
	else if (RAM_CLOSURE(o)) printf("rom closure");
      }
    printf("\n");
  }
#endif
