/* file: "gc.c" */

/*
 * Copyright 2004-2009 by Marc Feeley and Vincent St-Amour, All Rights Reserved.
 */

#include "picobit-vm.h"

void init_ram_heap () {
  uint8 i;
  obj o = MAX_RAM_ENCODING;
  uint16 bound = MIN_RAM_ENCODING + ((glovars + 1) >> 1);
  
  free_list = 0;

  while (o > bound) {
    // we don't want to add globals to the free list, and globals occupy the
    // beginning of memory at the rate of 2 globals per word (car and cdr)
    ram_set_gc_tags (o, GC_TAG_UNMARKED);
    ram_set_car (o, free_list);
    free_list = o;
    o--;
  }

  free_list_vec = MIN_VEC_ENCODING;
  ram_set_car (free_list_vec, 0);
  // each node of the free list must know the free length that follows it
  // this free length is stored in words, not in bytes
  // if we did count in bytes, the number might need more than 13 bits
  ram_set_cdr (free_list_vec, (VEC_BYTES >> 2));
  
  for (i=0; i<glovars; i++)
    set_global (i, OBJ_FALSE);
  
  arg1 = OBJ_FALSE;
  arg2 = OBJ_FALSE;
  arg3 = OBJ_FALSE;
  arg4 = OBJ_FALSE;
  cont = OBJ_FALSE;
  env  = OBJ_NULL;

#ifdef INFINITE_PRECISION_BIGNUMS
  bignum_shr_result = OBJ_FALSE;
  bignum_shl_result = OBJ_FALSE;
  bignum_shift_left_result = OBJ_FALSE;
  bignum_add_result = OBJ_FALSE;
  bignum_sub_result = OBJ_FALSE;
  bignum_scale_result = OBJ_FALSE;
  bignum_mul_result = OBJ_FALSE;
  bignum_mul_s = OBJ_FALSE;
  bignum_div_result = OBJ_FALSE;
  bignum_div_x = OBJ_FALSE;
  bignum_ior_result = OBJ_FALSE;
  bignum_xor_result = OBJ_FALSE;
#endif
}


void mark (obj temp) {  
  /* mark phase */
  
  obj stack;
  obj visit;
  
  if (IN_RAM(temp)) {
    visit = NIL;
    
  push:
    
    stack = visit;
    visit = temp;
    
    IF_GC_TRACE(printf ("push   stack=%d  visit=%d (tag=%d)\n", stack, visit, ram_get_gc_tags (visit)>>5));
    
    if ((HAS_1_OBJECT_FIELD (visit) && ram_get_gc_tag0 (visit))
	|| (HAS_2_OBJECT_FIELDS (visit)
	    && (ram_get_gc_tags (visit) != GC_TAG_UNMARKED)))
      IF_GC_TRACE(printf ("case 1\n"));
    else {
      if (HAS_2_OBJECT_FIELDS(visit)) { // pairs and continuations
	IF_GC_TRACE(printf ("case 2\n"));
	
      visit_field2:
	
	temp = ram_get_cdr (visit);
	
	if (IN_RAM(temp)) {
	  IF_GC_TRACE(printf ("case 3\n"));
	  ram_set_gc_tags (visit, GC_TAG_1_LEFT);
	  ram_set_cdr (visit, stack);
	  goto push;
	}

	IF_GC_TRACE(printf ("case 4\n"));
	
	goto visit_field1;
      }

      if (HAS_1_OBJECT_FIELD(visit)) {
	IF_GC_TRACE(printf ("case 5\n"));
	
      visit_field1:

	temp = ram_get_car (visit);
	
	if (IN_RAM(temp)) {
	  IF_GC_TRACE(printf ("case 6\n"));
	  ram_set_gc_tag0 (visit, GC_TAG_0_LEFT);
	  ram_set_car (visit, stack);		  
	  
	  goto push;
	}

	IF_GC_TRACE(printf ("case 7\n"));
      }
      else
	IF_GC_TRACE(printf ("case 8\n"));
      
      ram_set_gc_tag0 (visit, GC_TAG_0_LEFT);
    }

  pop:
    
    IF_GC_TRACE(printf ("pop    stack=%d  visit=%d (tag=%d)\n", stack, visit, ram_get_gc_tags (visit)>>6));
    
    if (stack != NIL) {
      if (HAS_2_OBJECT_FIELDS(stack) && ram_get_gc_tag1 (stack)) {
	IF_GC_TRACE(printf ("case 9\n"));
	
	temp = ram_get_cdr (stack);  /* pop through cdr */
	ram_set_cdr (stack, visit);
	visit = stack;
	stack = temp;

	ram_set_gc_tag1(visit, GC_TAG_UNMARKED);
	// we unset the "1-left" bit
	
	goto visit_field1;
      }

      IF_GC_TRACE(printf ("case 11\n"));
      
      temp = ram_get_car (stack);  /* pop through car */
      ram_set_car (stack, visit);
      visit = stack;
      stack = temp;
      
      goto pop;
    }
  }
}

#ifdef DEBUG_GC
int max_live = 0;
#endif

void sweep () {
  /* sweep phase */
  
#ifdef DEBUG_GC
  int n = 0;
#endif

  obj visit = MAX_RAM_ENCODING;

  free_list = 0;

  while (visit >= (MIN_RAM_ENCODING + ((glovars + 1) >> 1))) {
    // we don't want to sweep the global variables area
    if ((RAM_COMPOSITE(visit)
	 && (ram_get_gc_tags (visit) == GC_TAG_UNMARKED)) // 2 mark bit
	|| !(ram_get_gc_tags (visit) & GC_TAG_0_LEFT)) { // 1 mark bit
      /* unmarked? */
      if (RAM_VECTOR(visit)) {
	// when we sweep a vector, we also have to sweep its contents
	obj o = ram_get_cdr (visit);
	uint16 i = ram_get_car (visit); // number of elements
	ram_set_car (o, free_list_vec);
	ram_set_cdr (o, ((i + 3) >> 2)); // free length, in words
	free_list_vec = o;
	// TODO merge free spaces
      }
      ram_set_car (visit, free_list);
      free_list = visit;
    }
    else {
      if (RAM_COMPOSITE(visit))
	ram_set_gc_tags (visit, GC_TAG_UNMARKED);
      else // only 1 mark bit to unset
	ram_set_gc_tag0 (visit, GC_TAG_UNMARKED);
#ifdef DEBUG_GC
      n++;
#endif
    }
    visit--;
  }

#ifdef DEBUG_GC
  if (n > max_live) {
    max_live = n;
    printf ("**************** memory needed = %d\n", max_live+1);
    fflush (stdout);
  }
#endif
}

void gc () {
  uint8 i;
  
  IF_TRACE(printf("\nGC BEGINS\n"));
  
  IF_GC_TRACE(printf("arg1\n"));
  mark (arg1);
  IF_GC_TRACE(printf("arg2\n"));
  mark (arg2);
  IF_GC_TRACE(printf("arg3\n"));
  mark (arg3);
  IF_GC_TRACE(printf("arg4\n"));
  mark (arg4);
  IF_GC_TRACE(printf("cont\n"));
  mark (cont);
  IF_GC_TRACE(printf("env\n"));
  mark (env);

#ifdef INFINITE_PRECISION_BIGNUMS
  IF_GC_TRACE(printf("bignum_shr_result\n"));
  mark (bignum_shr_result);
  IF_GC_TRACE(printf("bignum_shl_result\n"));
  mark (bignum_shl_result);
  IF_GC_TRACE(printf("bignum_shift_left_result\n"));
  mark (bignum_shift_left_result);
  IF_GC_TRACE(printf("bignum_add_result\n"));
  mark (bignum_add_result);
  IF_GC_TRACE(printf("bignum_sub_result\n"));
  mark (bignum_sub_result);
  IF_GC_TRACE(printf("bignum_scale_result\n"));
  mark (bignum_scale_result);
  IF_GC_TRACE(printf("bignum_mul_result\n"));
  mark (bignum_mul_result);
  IF_GC_TRACE(printf("bignum_mul_s\n"));
  mark (bignum_mul_s);
  IF_GC_TRACE(printf("bignum_div_result\n"));
  mark (bignum_div_result);
  IF_GC_TRACE(printf("bignum_div_x\n"));
  mark (bignum_div_x);
  IF_GC_TRACE(printf("bignum_ior_result\n"));
  mark (bignum_ior_result);
  IF_GC_TRACE(printf("bignum_xor_result\n"));
  mark (bignum_xor_result);
#endif

  IF_GC_TRACE(printf("globals\n"));
  for (i=0; i<glovars; i++)
    mark (get_global (i));

  sweep ();
}

obj alloc_ram_cell () {
  obj o;
  
#ifdef DEBUG_GC
  gc ();
#endif

  if (free_list == 0) {
#ifndef DEBUG_GC
    gc ();
    if (free_list == 0)
#endif
      ERROR("alloc_ram_cell", "memory is full");
  }
  
  o = free_list;
  
  free_list = ram_get_car (o);

  return o;
}

obj alloc_ram_cell_init (uint8 f0, uint8 f1, uint8 f2, uint8 f3) {
  obj o = alloc_ram_cell ();
  
  ram_set_field0 (o, f0);
  ram_set_field1 (o, f1);
  ram_set_field2 (o, f2);
  ram_set_field3 (o, f3);

  return o;
}

obj alloc_vec_cell (uint16 n) {
  obj o = free_list_vec;
  obj prec = 0;
  uint8 gc_done = 0;
  
#ifdef DEBUG_GC
  gc ();
  gc_done = 1;
#endif

  while ((ram_get_cdr (o) * 4) < n) { // free space too small
    if (o == 0) { // no free space, or none big enough
      if (gc_done) // we gc'd, but no space is big enough for the vector
	ERROR("alloc_vec_cell", "no room for vector");
#ifndef DEBUG_GC
      gc ();
      gc_done = 1;
#endif
      o = free_list_vec;
      prec = 0;
      continue;
    } // TODO merge adjacent free spaces, maybe compact ?
    prec = o;
    o = ram_get_car (o);
  }

  // case 1 : the new vector fills every free word advertized, we remove the
  //  node from the free list
  if (((ram_get_cdr(o) * 4) - n) < 4) {
    if (prec)
      ram_set_car (prec, ram_get_car (o));
    else
      free_list_vec = ram_get_car (o);
  }
  // case 2 : there is still some space left in the free section, create a new
  //  node to represent this space
  else {
    obj new_free = o + ((n + 3) >> 2);
    if (prec)
      ram_set_car (prec, new_free);
    else
      free_list_vec = new_free;
    ram_set_car (new_free, ram_get_car (o));
    ram_set_cdr (new_free, ram_get_cdr (o) - ((n + 3) >> 2));
  }
  
  return o;
}
