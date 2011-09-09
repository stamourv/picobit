#ifndef PICOBIT_PRIMITIVES_H
#define PICOBIT_PRIMITIVES_H

#ifdef CONFIG_DEBUG_STRINGS
extern const char* const primitive_names[];
#endif /* CONFIG_DEBUG_STRINGS */

/* For the primitive scanning pass. */
#ifndef NO_PRIMITIVE_EXPAND

#define PRIMITIVE(scheme_name, c_name, args) \
	void prim_ ## c_name ()

#define PRIMITIVE_UNSPEC PRIMITIVE

#endif /* NO_PRIMITIVE_EXPAND */

/* Temporary storage for primitives */
extern obj a1, a2, a3;

void decode_2_int_args ();
void push_arg1 ();

#endif
