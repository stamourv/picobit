#ifndef PICOBIT_GC_H
#define PICOBIT_GC_H

// TODO explain what each tag means, with 1-2 mark bits
#define GC_TAG_0_LEFT   (1<<5)
#define GC_TAG_1_LEFT   (2<<5)
#define GC_TAG_UNMARKED (0<<5)

/* Number of object fields of objects in ram */
#ifdef LESS_MACROS
uint8 HAS_2_OBJECT_FIELDS(uint16 visit) {return (RAM_PAIR(visit) || RAM_CONTINUATION(visit));}
#ifdef INFINITE_PRECISION_BIGNUMS
uint8 HAS_1_OBJECT_FIELD(uint16 visit)  {return (RAM_COMPOSITE(visit) || RAM_CLOSURE(visit) || RAM_BIGNUM(visit));}
#else
uint8 HAS_1_OBJECT_FIELD(uint16 visit)  {return (RAM_COMPOSITE(visit) || RAM_CLOSURE(visit));}
#endif

#else
#define HAS_2_OBJECT_FIELDS(visit) (RAM_PAIR(visit) || RAM_CONTINUATION(visit))
#ifdef INFINITE_PRECISION_BIGNUMS
#define HAS_1_OBJECT_FIELD(visit)  (RAM_COMPOSITE(visit) \
				    || RAM_CLOSURE(visit) || RAM_BIGNUM(visit))
#else
#define HAS_1_OBJECT_FIELD(visit)  (RAM_COMPOSITE(visit) || RAM_CLOSURE(visit))
#endif
#endif
// all composites except pairs and continuations have 1 object field

#define NIL OBJ_FALSE

obj free_list; /* list of unused cells */
/* first unused cell in vector space. */
/* points into vector space using whole-RAM addressing  */
/* its value should be over MAX_RAM_ENCODING */
obj free_vec_pointer;

obj arg1; /* root set */
obj arg2;
obj arg3;
obj arg4;
obj cont;
obj env;

#ifdef INFINITE_PRECISION_BIGNUMS
// Temps in bignum algorithms must be registered as roots too, since
// GC can occur during bignum operations (they allocate).
// Bignum ops can share variables as long as they don't interfere.
obj bignum_tmp1; // ior xor add sub scale shr shl
obj bignum_tmp2; // shift_left mul
obj bignum_tmp3; // div mul
obj bignum_tmp4; // div mul
obj bignum_tmp5; // div
#endif


rom_addr pc; /* interpreter variables */
uint8 glovars;
rom_addr entry;
uint8 bytecode;
uint8 bytecode_hi4;
uint8 bytecode_lo4;
uint16 a1;
uint16 a2;
uint16 a3;

#endif
