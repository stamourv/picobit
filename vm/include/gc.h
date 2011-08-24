#ifndef PICOBIT_GC_H
#define PICOBIT_GC_H

/* TODO explain what each tag means, with 1-2 mark bits.
   Currently, they're described in IFL paper. */
#define GC_TAG_0_LEFT   (1<<5)
#define GC_TAG_1_LEFT   (2<<5)
#define GC_TAG_UNMARKED (0<<5)

/* Number of object fields of objects in ram */
#ifdef LESS_MACROS
uint8 HAS_2_OBJECT_FIELDS(uint16 visit)
{
	return (RAM_PAIR_P(visit) || RAM_CONTINUATION_P(visit));
}

#ifdef CONFIG_BIGNUM_LONG
uint8 HAS_1_OBJECT_FIELD(uint16 visit)
{
	return (RAM_COMPOSITE_P(visit) || RAM_CLOSURE_P(visit) || RAM_BIGNUM_P(visit));
}
#else
uint8 HAS_1_OBJECT_FIELD(uint16 visit)
{
	return (RAM_COMPOSITE_P(visit) || RAM_CLOSURE_P(visit));
}
#endif

#else
#define HAS_2_OBJECT_FIELDS(visit) (RAM_PAIR_P(visit) || RAM_CONTINUATION_P(visit))
#ifdef CONFIG_BIGNUM_LONG
#define HAS_1_OBJECT_FIELD(visit)  (RAM_COMPOSITE_P(visit) \
				    || RAM_CLOSURE_P(visit) || RAM_BIGNUM_P(visit))
#else
#define HAS_1_OBJECT_FIELD(visit)  (RAM_COMPOSITE_P(visit) || RAM_CLOSURE_P(visit))
#endif
#endif
// all composites except pairs and continuations have 1 object field

#ifdef CONFIG_GC_DEBUG
extern int max_live;
#endif

void init_ram_heap ();

void mark (obj temp);
void sweep ();
void gc ();

obj alloc_ram_cell ();
obj alloc_ram_cell_init (uint8 f0, uint8 f1, uint8 f2, uint8 f3);
obj alloc_vec_cell (uint16 n, obj from);

#endif
