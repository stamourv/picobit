#include <picobit.h>
#include <bignum.h>
#include <debug.h>
#include <gc.h>
#include <primitives.h>

static obj free_list, free_vec_pointer;

#ifdef CONFIG_GC_STATISTICS
int max_live = 0;
#endif

void init_ram_heap ()
{
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

	free_vec_pointer = VEC_TO_RAM_OBJ(MIN_VEC_ENCODING);

	for (i = 0; i < glovars; i++) {
		set_global (i, OBJ_FALSE);
	}

	arg1 = OBJ_FALSE;
	arg2 = OBJ_FALSE;
	arg3 = OBJ_FALSE;
	arg4 = OBJ_FALSE;
	cont = OBJ_FALSE;
	env  = OBJ_NULL;

#ifdef CONFIG_BIGNUM_LONG
	bignum_gc_init();
#endif
}

void mark (obj temp)
{
	/* mark phase */

	obj stack;
	obj visit;

	if (IN_RAM(temp)) {
		visit = OBJ_FALSE;

push:
		stack = visit;
		visit = temp;

		IF_GC_TRACE(printf ("push   stack=%d  visit=%d (tag=%d)\n", stack, visit, ram_get_gc_tags (visit)>>5));

		if ((HAS_1_OBJECT_FIELD (visit) && ram_get_gc_tag0 (visit))
		    || (HAS_2_OBJECT_FIELDS (visit)
		        && (ram_get_gc_tags (visit) != GC_TAG_UNMARKED))) {
			IF_GC_TRACE(printf ("case 1\n"));
		} else {
			if (HAS_2_OBJECT_FIELDS(visit)) { // pairs and continuations
				IF_GC_TRACE(printf ("case 2\n"));

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
			} else {
				IF_GC_TRACE(printf ("case 8\n"));
			}

			ram_set_gc_tag0 (visit, GC_TAG_0_LEFT);
		}

pop:
		IF_GC_TRACE(printf ("pop    stack=%d  visit=%d (tag=%d)\n", stack, visit, ram_get_gc_tags (visit)>>6));

		if (stack != OBJ_FALSE) {
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

void sweep ()
{
	/* sweep phase */

#ifdef CONFIG_GC_STATISTICS
	int n = 0;
#endif

	obj visit = MAX_RAM_ENCODING;

	free_list = 0;

	while (visit >= (MIN_RAM_ENCODING + ((glovars + 1) >> 1))) {
		// we don't want to sweep the global variables area
		if ((RAM_COMPOSITE_P(visit)
		     && (ram_get_gc_tags (visit) == GC_TAG_UNMARKED)) // 2 mark bit
		    || (!RAM_COMPOSITE_P(visit)
		        && !(ram_get_gc_tags (visit) & GC_TAG_0_LEFT))) { // 1 mark bit
			/* unmarked? */
			if (RAM_VECTOR_P(visit)) {
				// when we sweep a vector, we also have to mark its contents as free
				// we subtract 1 to get to the header of the block, before the data
				obj o = VEC_TO_RAM_OBJ(ram_get_cdr (visit) - 1);
				ram_set_gc_tag0 (o, 0); // mark the block as free
			}

			ram_set_car (visit, free_list);
			free_list = visit;
		} else {
			if (RAM_COMPOSITE_P(visit)) {
				ram_set_gc_tags (visit, GC_TAG_UNMARKED);
			} else { // only 1 mark bit to unset
				ram_set_gc_tag0 (visit, GC_TAG_UNMARKED);
			}

#ifdef CONFIG_GC_STATISTICS
			n++;
#endif
		}

		visit--;
	}

#ifdef CONFIG_GC_STATISTICS
	if (n > max_live) {
		max_live = n;
#ifdef CONFIG_GC_DEBUG
		printf ("**************** memory needed = %d\n", max_live+1);
		fflush (stdout);
#endif
	}
#endif
}

void gc ()
{
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

#ifdef CONFIG_BIGNUM_LONG
	bignum_gc_mark();
#endif

	IF_GC_TRACE(printf("globals\n"));

	for (i=0; i<glovars; i++) {
		mark (get_global (i));
	}

	sweep ();
}

obj alloc_ram_cell ()
{
	obj o;

#ifdef CONFIG_GC_AGGRESSIVE
	gc ();
#endif

	if (free_list == 0) {
#ifndef CONFIG_GC_DEBUG
		gc ();

		if (free_list == 0)
#endif
			ERROR("alloc_ram_cell", "memory is full");
	}

	o = free_list;

	free_list = ram_get_car (o);

	return o;
}

obj alloc_ram_cell_init (uint8 f0, uint8 f1, uint8 f2, uint8 f3)
{
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
  A block can be free or used, in which case it holds useful data.

  All blocks start with a 4-byte header.
  The GC tag 0 is 0 for free blocks, 1 for used block.
  The car of this header is the size (in 4-byte words, including header) of
  the block.
  Used blocks have a pointer to their header in the object heap in the cdr.

  free_vec_pointer points (using RAM addressing) to the first free word.
  Allocation involves bumping that pointer and setting the header of the new
  used block.
  Freeing is done as part of the object heap GC. Any time a vector header in
  the object heap is freed, the vector space block corresponding to its
  contents is marked as free.
  When the vector space is full (free pointer would go out of bounds after
  an allocation), object heap GC is triggered and the vector space is
  compacted.
 */

void compact ()
{
	/*
	  Move all used blocks to the left.
	  This is done by scanning the heap, and moving any taken block to the
	  left if there's free space before it.
	*/

	obj cur = VEC_TO_RAM_OBJ(MIN_VEC_ENCODING);
	obj prev = 0;

	uint16 cur_size;

	while (cur < free_vec_pointer) {
		cur_size  = ram_get_car(cur);

		if (prev && !ram_get_gc_tag0 (prev)) { // previous block is free
			if (!ram_get_gc_tag0 (cur)) { // current is free too, merge free spaces
				// advance cur, but prev stays in place
				cur += cur_size;
			} else { // prev is free, but not cur, move cur to start at prev
				// fix header in the object heap to point to the data's new
				// location
				ram_set_cdr(ram_get_cdr(cur), RAM_TO_VEC_OBJ(prev+1));

				while(cur_size--) { // copy cur's data, which includes header
					ram_set_field0(prev, ram_get_field0(cur));
					ram_set_field1(prev, ram_get_field1(cur));
					ram_set_field2(prev, ram_get_field2(cur));
					ram_set_field3(prev, ram_get_field3(cur));
					cur++;
					prev++;
				}

				// set up a free block where the end of cur's data was
				// (prev is already there from the iteration above)
				ram_set_gc_tag0(prev, 0);
				// at this point, cur is after the new free space, where the
				// next block is
			}
		} else {
			// Go to the next block, which is <size> away from cur.
			prev = cur;
			cur += cur_size;
		}
	}

	// free space is now all at the end
	free_vec_pointer = prev;
}

obj alloc_vec_cell (uint16 n, obj from)
{
	uint8 gc_done = 0;

#ifdef CONFIG_GC_DEBUG
	gc ();
	compact();
	gc_done = 1;
#endif

	// get minimum number of 4-byte blocks (round to nearest 4)
	// this includes a 4-byte vector space header
	n = ((n+3) >> 2) + 1;

	while ((VEC_TO_RAM_OBJ(MAX_RAM_ENCODING) - free_vec_pointer) < n) {
		// free space too small, trigger gc
		if (gc_done) { // we gc'd, but no space is big enough for the vector
			ERROR("alloc_vec_cell", "no room for vector");
		}

#ifndef CONFIG_GC_DEBUG
		gc ();
		compact();
		gc_done = 1;
#endif
	}

	obj o = free_vec_pointer;

	// advance the free pointer
	free_vec_pointer += n;

	// store block size
	ram_set_car (o, n);
	// set up pointer back to the regular heap header
	// stored in the car, instead of the free list pointer
	ram_set_cdr (o, from);
	ram_set_gc_tag0 (o, GC_TAG_0_LEFT); // mark block as used

	// return pointer to start of data, skipping the header
	return RAM_TO_VEC_OBJ(o + 1);
}

#ifdef CONFIG_GC_STATISTICS_PRIMITIVE
PRIMITIVE(#%gc-max-live, gc_max_live, 0)
{
	arg1 = encode_int (max_live);
}
#endif
