#include <picobit.h>
#include <primitives.h>
#include <dispatch.h>

PRIMITIVE(return, return, 1)
{
	arg2 = ram_get_cdr (cont);
	pc = ram_get_entry (arg2);
	env = ram_get_car (arg2);
	cont = ram_get_car (cont);
	arg2 = OBJ_FALSE;
}

PRIMITIVE_UNSPEC(pop, pop, 0)
{
	pop();
}

PRIMITIVE(get-cont, get_cont, 0)
{
	arg1 = cont;
}

PRIMITIVE_UNSPEC(graft-to-cont, graft_to_cont, 2)
{
	/* arg2 is thunk to call, arg1 is continuation */
	cont = arg1;

	arg1 = arg2;
	push_arg1();

	pop_procedure ();
	build_env (handle_arity_and_rest_param (0));

	env = arg1;
	pc = entry;

	arg1 = OBJ_FALSE;
	arg2 = OBJ_FALSE;
}

PRIMITIVE(return-to-cont, return_to_cont, 2)
{
	/* arg2 is value to return, arg1 is continuation */
	cont = arg1;
	arg1 = arg2;

	arg2 = ram_get_cdr(cont);

	pc = ram_get_entry(arg2);

	env = ram_get_car (arg2);
	cont = ram_get_car (cont);

	arg2 = OBJ_FALSE;
}
