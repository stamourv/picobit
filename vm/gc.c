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

  free_list_vec = VEC_TO_RAM_OBJ(MIN_VEC_ENCODING);
  ram_set_gc_tag0 (free_list_vec, 0); // block is free
  ram_set_car (free_list_vec, 0); // free list terminator
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
  bignum_tmp1 = OBJ_FALSE;
  bignum_tmp2 = OBJ_FALSE;
  bignum_tmp3 = OBJ_FALSE;
  bignum_tmp4 = OBJ_FALSE;
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
	// we subtract 1 to get to the header of the block, before the data
	obj o = VEC_TO_RAM_OBJ(ram_get_cdr (visit) - 1);
	ram_set_car (o, RAM_TO_VEC_OBJ(free_list_vec));
	// No need to set the block length, it's already there from when
	// the used block was initialized.
	ram_set_gc_tag0 (o, 0); // mark the block as free
	free_list_vec = o;
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
  IF_GC_TRACE(printf("bignum_tmp1\n"));
  mark (bignum_tmp1);
  IF_GC_TRACE(printf("bignum_tmp2\n"));
  mark (bignum_tmp2);
  IF_GC_TRACE(printf("bignum_tmp3\n"));
  mark (bignum_tmp3);
  IF_GC_TRACE(printf("bignum_tmp4\n"));
  mark (bignum_tmp4);
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


/*
  Vector space layout.

  Vector space is divided into blocks of 4-byte words.
  A block can be free, in which case it can be found by traversing the free
  list, or taken, in which case it holds useful data.

  All blocks start with a 4-byte header.
  The car of this header is a pointer.
  In the case of free blocks, it points to the next block on the free list.
  (Using vector space indexing, not RAM addressing, so starting from 0.)
  In the case of used blocks, it points to its vector header object in the
  regular heap.
  GC tag 0 is 0 for free blocks, 1 for used block.

  The vector space starts as a single free block of the size of the entire
  vector space, minus one 4-byte word.
  The word at address 0 is unused, since address 0 serves as the free list
  terminator.
 */

// move all used blocks to the left
// this is done by scanning the heap, and moving any taken block to the
// left if there's free space before it
// at the end, a single free block will remain, at the right of the space
void compact () {
  obj cur = VEC_TO_RAM_OBJ(MIN_VEC_ENCODING);
  obj prev = 0;

  obj cur_free;
  obj prev_free;
  obj cur_size;
  uint16 prev_size;

  while (cur) {
    cur_free = ram_get_gc_tag0 (cur);
    prev_free = ram_get_gc_tag0 (prev);
    cur_size  = ram_get_cdr(cur);
      if (prev && prev_free) {
	prev_size = ram_get_cdr(prev);
	if (cur_free) { // merge free spaces
	  // if prev stays free until the end of compaction, it will be
	  // the last (only) block on the free list, terminate the list,
	  // just in case
	  ram_set_car(prev, 0);
	  // new free size is the sum of the old ones
	  ram_set_cdr(prev, prev_size + cur_size);
	  // advance cur, but prev stays in place
	  cur += cur_size;
	}
	else { // prev is free, but not cur, move cur to start at prev
	  while(cur_size--) { // copy cur's data, which includes header
	    ram_set_field0(prev, ram_get_field0(cur));
	    ram_set_field1(prev, ram_get_field1(cur));
	    ram_set_field2(prev, ram_get_field2(cur));
	    ram_set_field3(prev, ram_get_field3(cur));
	    cur++; prev++;
	  }

	  // set up a free block where the end of cur's data was
	  // (prev is already there from the iteration above)
	  ram_set_gc_tag0(prev, 0);
	  ram_set_car(prev, 0); // could be the last free block, see above
	  ram_set_cdr(prev, prev_size); // size of new free block
	  // at this point, cur is after the new free space, where the
	  // next block is
	}
      }
      else {
	// Go to the next block, which is <size> away from cur.
	prev = cur;
	cur += cur_size;
      }
  }

  // free space is now all at the end
  free_list_vec = prev;
}

obj alloc_vec_cell (uint16 n, obj from) {
  obj o = free_list_vec;
  obj prev = 0;
  uint8 gc_done = 0;
  uint8 compact_done = 0;
  
#ifdef DEBUG_GC
  gc ();
  gc_done = 1;
#endif

  // get minimum number of 4-byte blocks (round to nearest 4)
  // this includes a 4-byte vector space header
  n = ((n+3) >> 2) + 1;

  while (ram_get_cdr (o) < n) { // free space too small
    if (o == 0) { // no free space, or none big enough
      if (compact_done) // really no space left
	ERROR("alloc_vec_cell", "no room for vector");
      if (gc_done) { // we gc'd, but no space is big enough for the vector
	compact();
	compact_done = 1;
      }
#ifndef DEBUG_GC
      gc ();
      gc_done = 1;
#endif
      // start again, maybe we can allocate now
      o = free_list_vec;
      prev = 0;
      continue;
    }
    prev = o;
    o = VEC_TO_RAM_OBJ(ram_get_car (o));
  }

  obj car_o = ram_get_cdr(o); // next on free list
  obj cdr_o = ram_get_cdr(o); // block size

  // case 1 : the new vector fills every free word advertized, we remove the
  //  node from the free list
  if (!(cdr_o - n)) {
    if (prev)
      ram_set_car (prev, car_o);
    else
      free_list_vec = VEC_TO_RAM_OBJ(car_o);
  }
  // case 2 : there is still some space left in the free section, create a new
  //  node to represent this space
  else {
    obj new_free = o + n;
    if (prev)
      ram_set_car (prev, RAM_TO_VEC_OBJ(new_free));
    else
      free_list_vec = new_free;
    ram_set_car (new_free, car_o);
    ram_set_cdr (new_free, cdr_o - n);
    // mark new block as free
    ram_set_gc_tag0 (new_free, 0);

    // store size of the taken block. this includes header size
    ram_set_cdr (o, n);
  }

  // set up pointer back to the regular heap header
  // stored in the car, instead of the free list pointer
  ram_set_car (o, from);
  ram_set_gc_tag0 (o, 1); // mark block as used

  // return pointer to start of data, skipping the header
  return RAM_TO_VEC_OBJ(o+1);
}
