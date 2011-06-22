/* file: "debug.c" */

/*
 * Copyright 2004-2009 by Marc Feeley and Vincent St-Amour, All Rights Reserved.
 */

#include "picobit-vm.h"

// debugging functions

#ifdef WORKSTATION
void show_type (obj o) {
  printf("%d : ", o);
  if (o == OBJ_FALSE) printf("#f");
  else if (o == OBJ_TRUE) printf("#t");
  else if (o == OBJ_NULL) printf("()");
  else if (o < MIN_ROM_ENCODING) printf("fixnum");
  else if (IN_RAM (o)) {
    if (RAM_BIGNUM(o)) printf("ram bignum");
    else if (RAM_PAIR(o)) printf("ram pair");
    else if (RAM_SYMBOL(o)) printf("ram symbol");
    else if (RAM_STRING(o)) printf("ram string");
    else if (RAM_VECTOR(o)) printf("ram vector");
    else if (RAM_CONTINUATION(o)) printf("ram continuation");
    else if (RAM_CLOSURE(o)) printf("ram closure");
  }
  else { // ROM
    if (ROM_BIGNUM(o)) printf("rom bignum");
    else if (ROM_PAIR(o)) printf("rom pair");
    else if (ROM_SYMBOL(o)) printf("rom symbol");
    else if (ROM_STRING(o)) printf("rom string");
    else if (ROM_VECTOR(o)) printf("rom vector");
    else if (ROM_CONTINUATION(o)) printf("rom continuation");
    // ROM closures don't exist
  }
  printf("\n");
}

void show_state (rom_addr pc) {
  printf("\n");
  printf ("pc=0x%04x bytecode=0x%02x env=", pc, rom_get (pc));
  show (env);
  printf (" cont=");
  show (cont);
  printf ("\n");
  fflush (stdout);
}

void p (integer n) {
  long long x; // TODO long long is 32 bits here, what about on a 64 bit machine ?
  x = ((long long)integer_lo (integer_hi (integer_hi (integer_hi (n))))<<48)+
    ((long long)integer_lo (integer_hi (integer_hi (n)))<<32)+
    ((long long)integer_lo (integer_hi (n))<<16)+
    (long long)integer_lo (n);
  printf ("%lld ", x);
  // TODO test for hex output, to avoid signedness problems
/*   printf("%x %x %x %x\n", // TODO prob, if a lower part is 0, will show 0, not 0000 */
/* 	 integer_lo (integer_hi (integer_hi (integer_hi (n)))), */
/* 	 integer_lo (integer_hi (integer_hi (n))), */
/* 	 integer_lo (integer_hi (n)), */
/* 	 integer_lo (n)); */
}

integer enc (long long n) {
  integer result = NIL;
  
  while (n != 0 && n != -1) {
    result = make_integer ((digit)n, result);
    n >>= digit_width;
  }
  
  if (n < 0)
    return norm (result, NEG1);
  else
    return norm (result, ZERO);
}

void test () {
  integer min2;
  integer min1;
  integer zero;
  integer one;
  integer two;
  integer three;
  integer four;

  zero = make_integer (0x0000, 0);
  min1 = make_integer (0xffff, 0);
  integer_hi_set (zero, ZERO);
  integer_hi_set (min1, NEG1);
  
  min2 = make_integer (0xfffe, NEG1);
  one  = make_integer (0x0001, ZERO);
  two  = make_integer (0x0002, ZERO);
  three= make_integer (0x0003, ZERO);
  four = make_integer (0x0004, ZERO);
  
  if (negp (ZERO)) printf ("zero is negp\n"); // should not show
  if (negp (NEG1)) printf ("min1 is negp\n");

  printf ("cmp(5,5) = %d\n",cmp (make_integer (5, ZERO), make_integer (5, ZERO)));
  printf ("cmp(2,5) = %d\n",cmp (make_integer (2, ZERO), make_integer (5, ZERO)));
  printf ("cmp(5,2) = %d\n",cmp (make_integer (5, ZERO), make_integer (2, ZERO)));

  printf ("cmp(-5,-5) = %d\n",cmp (make_integer (-5, NEG1), make_integer (-5, NEG1)));
  printf ("cmp(-2,-5) = %d\n",cmp (make_integer (-2, NEG1), make_integer (-5, NEG1)));
  printf ("cmp(-5,-2) = %d\n",cmp (make_integer (-5, NEG1), make_integer (-2, NEG1)));

  printf ("cmp(-5,65533) = %d\n",cmp (make_integer (-5, NEG1), make_integer (65533, ZERO)));
  printf ("cmp(-5,2)     = %d\n",cmp (make_integer (-5, NEG1), make_integer (2, ZERO)));
  printf ("cmp(5,-65533) = %d\n",cmp (make_integer (5, ZERO), make_integer (-65533, NEG1)));
  printf ("cmp(5,-2)     = %d\n",cmp (make_integer (5, ZERO), make_integer (-2, NEG1)));

  printf ("integer_length(0) = %d\n", integer_length (ZERO)); // these return the number of bits necessary to encode
  printf ("integer_length(1) = %d\n", integer_length (make_integer (1, ZERO)));
  printf ("integer_length(2) = %d\n", integer_length (make_integer (2, ZERO)));
  printf ("integer_length(3) = %d\n", integer_length (make_integer (3, ZERO)));
  printf ("integer_length(4) = %d\n", integer_length (make_integer (4, ZERO)));
  printf ("integer_length(65536 + 4) = %d\n", integer_length (make_integer (4, make_integer (1, ZERO))));


  printf ("1 = %d\n", one);
  printf ("2 = %d\n", two);
  printf ("4 = %d\n", four);
  printf ("norm(2) = %d\n", norm (make_integer (0, make_integer (2, NIL)), ZERO));
  printf ("norm(2) = %d\n", norm (make_integer (0, make_integer (2, NIL)), ZERO));
  printf ("norm(3) = %d\n", norm (make_integer (0, make_integer (3, NIL)), ZERO));
  printf ("norm(3) = %d\n", norm (make_integer (0, make_integer (3, NIL)), ZERO));

  printf ("shl(1) = %d\n", shl (one));
  printf ("shl(2) = %d\n", shl (two));

  integer n = one;
  int i;
  // should show powers of 2 incerasing, then decreasing
  for (i=1; i<=34; i++) {
    printf("\nloop-1 : i=%d len=%d ", i, integer_length(n));
    p (n);
    n = shl(n);
  }
  for (i=1; i<=35; i++) {
    printf("\nloop-2 : i=%d len=%d ", i, integer_length(n));
    p (n);
    n = shr(n);
  }

  n = shift_left (four, 5);
  
  for (i=0; i<=14; i++) {
    printf("\nloop-3 : i=%d len=%d ", i, integer_length(n));
    p (shift_left (n, i*4));
  }

  printf("\n");
  p (add (enc (32768), enc (32768))); printf("\n"); // 65536
  p (add (enc (32768+(65536*65535LL)), enc (32768))); printf("\n"); // 4294967296

  p (sub (enc (32768), enc (-32768))); printf("\n"); // 65536
  p (sub (enc (32768+(65536*65535LL)), enc (-32768))); printf("\n"); // 4294967296

  p (sub (enc (32768), enc (32769))); printf("\n"); // -1
  p (sub (enc (32768), enc (132768))); printf("\n"); // -100000
  p (add(sub (enc (32768), enc (32769)), enc(1000))); printf("\n"); // 999

  // Handling of sign is done at the Scheme level.
  p (mulnonneg (enc (123456789), enc (1000000000))); printf("\n"); // 123456789000000000

  p (divnonneg (enc (10000000-1), enc (500000))); printf("\n"); // 19

  printf ("done\n");

  exit (0);
}

#endif
