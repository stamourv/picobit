/* file: "dispatch.c" */

/*
 * Copyright 2004-2009 by Marc Feeley and Vincent St-Amour, All Rights Reserved.
 */

#include "picobit-vm.h"

void push_arg1 () {
  env = cons (arg1, env);
  arg1 = OBJ_FALSE;
}

obj pop () {
  obj o = ram_get_car (env);
  env = ram_get_cdr (env);
  return o;
}

void pop_procedure () {
  arg1 = pop();

  if (IN_RAM(arg1)) {
    if (!RAM_CLOSURE(arg1))
      TYPE_ERROR("pop_procedure.0", "procedure");

    entry = ram_get_entry (arg1) + CODE_START;
  }
  else
    TYPE_ERROR("pop_procedure.1", "procedure");
}

uint8 handle_arity_and_rest_param (uint8 na) {
  uint8 np;

  np = rom_get (entry++);

  if (arg1 != OBJ_NULL)
    arg1 = ram_get_car(arg1); // closed environment

  if ((np & 0x80) == 0) {
    if (na != np)
      ERROR("handle_arity_and_rest_param.0", "wrong number of arguments");
  }
  else {
    np = ~np;

    if (na < np)
      ERROR("handle_arity_and_rest_param.1", "wrong number of arguments");

    arg3 = OBJ_NULL;

    while (na > np) {
      arg4 = pop();

      arg3 = cons (arg4, arg3);
      arg4 = OBJ_FALSE;

      na--;
    }

    arg1 = cons (arg3, arg1);
    arg3 = OBJ_FALSE;
  }

  return na;
}

uint8 build_env (uint8 na) {
  while (na != 0) {
    arg3 = pop();

    arg1 = cons (arg3, arg1);

    na--;
  }

  arg3 = OBJ_FALSE;
}

void save_cont () {
  // the second half is a closure
  arg3 = alloc_ram_cell_init (CLOSURE_FIELD0 | (env >> 8),
			      env & 0xff,
                              (pc >> 8),
                              (pc & 0xff));
  cont = alloc_ram_cell_init (COMPOSITE_FIELD0 | (cont >> 8),
                              cont & 0xff,
                              CONTINUATION_FIELD2 | (arg3 >> 8),
                              arg3 & 0xff);
  arg3 = OBJ_FALSE;
}

void interpreter () {
  pc = rom_get (CODE_START+2);
  pc = (CODE_START + 4) + (pc << 2);

  glovars = rom_get (CODE_START+3); // number of global variables

  init_ram_heap ();

 dispatch:
  IF_TRACE(show_state (pc));
  FETCH_NEXT_BYTECODE();
  bytecode_hi4 = bytecode & 0xf0;
  bytecode_lo4 = bytecode & 0x0f;

  switch (bytecode_hi4 >> 4) {

    /*************************************************************************/
  case PUSH_CONSTANT1 :

    IF_TRACE(printf("  (push-constant "); show (bytecode_lo4); printf (")\n"));

    arg1 = bytecode_lo4;

    push_arg1();

    goto dispatch;

    /*************************************************************************/
  case PUSH_CONSTANT2 :

    IF_TRACE(printf("  (push-constant "); show (bytecode_lo4+16); printf (")\n"));
    arg1 = bytecode_lo4+16;

    push_arg1();

    goto dispatch;

    /*************************************************************************/
  case PUSH_STACK1 :

    IF_TRACE(printf("  (push-stack %d)\n", bytecode_lo4));

    arg1 = env;

    while (bytecode_lo4 != 0) {
      arg1 = ram_get_cdr (arg1);
      bytecode_lo4--;
    }

    arg1 = ram_get_car (arg1);

    push_arg1();

    goto dispatch;

    /*************************************************************************/
  case PUSH_STACK2 :

    IF_TRACE(printf("  (push-stack %d)\n", bytecode_lo4+16));

    bytecode_lo4 += 16;

    arg1 = env;

    while (bytecode_lo4 != 0) {
      arg1 = ram_get_cdr (arg1);
      bytecode_lo4--;
    }

    arg1 = ram_get_car (arg1);

    push_arg1();

    goto dispatch;

    /*************************************************************************/
  case PUSH_GLOBAL :

    IF_TRACE(printf("  (push-global %d)\n", bytecode_lo4));

    arg1 = get_global (bytecode_lo4);

    push_arg1();

    goto dispatch;

    /*************************************************************************/
  case SET_GLOBAL :

    IF_TRACE(printf("  (set-global %d)\n", bytecode_lo4));

    set_global (bytecode_lo4, pop());

    goto dispatch;

    /*************************************************************************/
  case CALL :

    IF_TRACE(printf("  (call %d)\n", bytecode_lo4));

    pop_procedure ();
    build_env (handle_arity_and_rest_param (bytecode_lo4));
    save_cont ();

    env = arg1;
    pc = entry;

    arg1 = OBJ_FALSE;

    goto dispatch;

    /*************************************************************************/
  case JUMP :

    IF_TRACE(printf("  (jump %d)\n", bytecode_lo4));

    pop_procedure ();
    build_env (handle_arity_and_rest_param (bytecode_lo4));

    env = arg1;
    pc = entry;

    arg1 = OBJ_FALSE;

    goto dispatch;

    /*************************************************************************/
  case LABEL_INSTR :

    switch (bytecode_lo4) {
    case 0: // call-toplevel
      FETCH_NEXT_BYTECODE();
      arg2 = bytecode;

      FETCH_NEXT_BYTECODE();

      IF_TRACE(printf("  (call-toplevel 0x%04x)\n",
                      ((arg2 << 8) | bytecode) + CODE_START));

      entry = (arg2 << 8) + bytecode + CODE_START;
      arg1 = OBJ_NULL;

      build_env (rom_get (entry++));
      save_cont ();

      env = arg1;
      pc = entry;

      arg1 = OBJ_FALSE;
      arg2 = OBJ_FALSE;

      break;

    case 1: // jump-toplevel
      FETCH_NEXT_BYTECODE();
      arg2 = bytecode;

      FETCH_NEXT_BYTECODE();

      IF_TRACE(printf("  (jump-toplevel 0x%04x)\n",
                      ((arg2 << 8) | bytecode) + CODE_START));

      entry = (arg2 << 8) + bytecode + CODE_START;
      arg1 = OBJ_NULL;

      build_env (rom_get (entry++));

      env = arg1;
      pc = entry;

      arg1 = OBJ_FALSE;
      arg2 = OBJ_FALSE;

      break;

    case 2: // goto
      FETCH_NEXT_BYTECODE();
      arg2 = bytecode;

      FETCH_NEXT_BYTECODE();

      IF_TRACE(printf("  (goto 0x%04x)\n",
                      (arg2 << 8) + bytecode + CODE_START));

      pc = (arg2 << 8) + bytecode + CODE_START;

      break;

    case 3: // goto-if-false
      FETCH_NEXT_BYTECODE();
      arg2 = bytecode;

      FETCH_NEXT_BYTECODE();

      IF_TRACE(printf("  (goto-if-false 0x%04x)\n",
                      (arg2 << 8) + bytecode + CODE_START));

      if (pop() == OBJ_FALSE)
        pc = (arg2 << 8) + bytecode + CODE_START;

      break;

    case 4: // closure
      FETCH_NEXT_BYTECODE();
      arg2 = bytecode;

      FETCH_NEXT_BYTECODE();

      entry = (arg2 << 8) | bytecode;

      IF_TRACE(printf("  (closure 0x%04x)\n", entry));

      arg3 = pop(); // env

      arg1 = alloc_ram_cell_init (CLOSURE_FIELD0 | (arg3 >> 8),
				  arg3 & 0xff,
                                  entry >> 8,
                                  (entry & 0xff));

      push_arg1();

      arg2 = OBJ_FALSE;
      arg3 = OBJ_FALSE;

      break;

#if 1
    case 5: // call-toplevel-rel8
      FETCH_NEXT_BYTECODE(); // TODO the short version have a lot in common with the long ones, abstract ?

      IF_TRACE(printf("  (call-toplevel-rel8 0x%04x)\n", pc + bytecode - 128));

      entry = pc + bytecode - 128;
      arg1 = OBJ_NULL;

      build_env (rom_get (entry++));
      save_cont ();

      env = arg1;
      pc = entry;

      arg1 = OBJ_FALSE;

      break;

    case 6: // jump-toplevel-rel8
      FETCH_NEXT_BYTECODE();

      IF_TRACE(printf("  (jump-toplevel-rel8 0x%04x)\n", pc + bytecode - 128));

      entry = pc + bytecode - 128;
      arg1 = OBJ_NULL;

      build_env (rom_get (entry++));

      env = arg1;
      pc = entry;

      arg1 = OBJ_FALSE;

      break;

    case 7: // goto-rel8
      FETCH_NEXT_BYTECODE();

      IF_TRACE(printf("  (goto-rel8 0x%04x)\n", pc + bytecode - 128));

      pc = pc + bytecode - 128;

      break;

    case 8: // goto-if-false-rel8
      FETCH_NEXT_BYTECODE();

      IF_TRACE(printf("  (goto-if-false-rel8 0x%04x)\n", pc + bytecode - 128));

      if (pop() == OBJ_FALSE)
        pc = pc + bytecode - 128;

      break;

    // TODO why does this not work?  don't worry about it now, as it is disabled in the compiler

    case 9: // closure-rel8
      FETCH_NEXT_BYTECODE();

      entry = pc + bytecode - 128;

      IF_TRACE(printf("  (closure-rel8 0x%04x)\n", entry));

      arg3 = pop(); // env

      arg1 = alloc_ram_cell_init (CLOSURE_FIELD0 | (entry >> 11),
                                  entry >> 3,
                                  ((entry & 0x07) << 5) | ((arg3 >> 8) & 0x1f),
                                  arg3 & 0xff);

      push_arg1();

      arg3 = OBJ_FALSE;

      break;
#endif

#if 0
    case 10: // FREE
      break;
    case 11:
      break;
    case 12:
      break;
    case 13:
      break;
#endif
    case 14: // push_global [long]
      FETCH_NEXT_BYTECODE();

      IF_TRACE(printf("  (push-global [long] %d)\n", bytecode));

      arg1 = get_global (bytecode);

      push_arg1();

      break;

    case 15: // set_global [long]
      FETCH_NEXT_BYTECODE();

      IF_TRACE(printf("  (set-global [long] %d)\n", bytecode));

      set_global (bytecode, pop());

      break;
    }

    goto dispatch;

    /*************************************************************************/
  case PUSH_CONSTANT_LONG :

    /* push-constant [long] */

    FETCH_NEXT_BYTECODE();

    IF_TRACE(printf("  (push [long] 0x%04x)\n", (bytecode_lo4 << 8) + bytecode));

    // necessary since SIXPIC would have kept the result of the shift at 8 bits
    arg1 = bytecode_lo4;
    arg1 = (arg1 << 8) | bytecode;
    push_arg1();

    goto dispatch;

    /*************************************************************************/

  case JUMP_TOPLEVEL_REL4 :

    IF_TRACE(printf("  (jump-toplevel-rel4 0x%04x)\n", pc + (bytecode & 0x0f)));

    entry = pc + (bytecode & 0x0f);
    arg1 = OBJ_NULL;

    build_env (rom_get (entry++));

    env = arg1;
    pc = entry;

    arg1 = OBJ_FALSE;

    goto dispatch;

    /*************************************************************************/

  case GOTO_IF_FALSE_REL4 :

    IF_TRACE(printf("  (goto-if-false-rel4 0x%04x)\n", pc + (bytecode & 0x0f)));

    if (pop() == OBJ_FALSE)
      pc = pc + (bytecode & 0x0f);

    goto dispatch;

    /*************************************************************************/
  case PRIM1 :

    IF_TRACE(printf("  (%s)\n", prim_name[bytecode_lo4]));

    switch (bytecode_lo4) {
    case 0:
      arg1 = pop();  prim_numberp ();  push_arg1();  break;
    case 1:
      arg2 = pop();  arg1 = pop();  prim_add ();         push_arg1();  break;
    case 2:
      arg2 = pop();  arg1 = pop();  prim_sub ();         push_arg1();  break;
    case 3:
      arg2 = pop();  arg1 = pop();  prim_mul_non_neg (); push_arg1();  break;
    case 4:
      arg2 = pop();  arg1 = pop();  prim_div_non_neg (); push_arg1();  break;
    case 5:
      arg2 = pop();  arg1 = pop();  prim_rem ();         push_arg1();  break;
#if 0
    case 6: // FREE
      break;
#endif
    case 7:
      arg2 = pop();  arg1 = pop();  prim_eq ();       push_arg1();  break;
    case 8:
      arg2 = pop();  arg1 = pop();  prim_lt ();       push_arg1();  break;
#if 0
    case 9:
      break; // FREE
#endif
    case 10:
      arg2 = pop();  arg1 = pop();  prim_gt ();       push_arg1();  break;
#if 0
    case 11:
      break; // FREE
#endif
    case 12:
      arg1 = pop();  prim_pairp ();    push_arg1();  break;
    case 13:
      arg2 = pop();  arg1 = pop();  prim_cons ();     push_arg1();  break;
    case 14:
      arg1 = pop();  prim_car ();      push_arg1();  break;
    case 15:
      arg1 = pop();  prim_cdr ();      push_arg1();  break;
    }

    goto dispatch;

    /*************************************************************************/
  case PRIM2 :

    IF_TRACE(printf("  (%s)\n", prim_name[bytecode_lo4+16]));

    switch (bytecode_lo4) {
    case 0:
      arg2 = pop();  arg1 = pop();  prim_set_car ();  break;
    case 1:
      arg2 = pop();  arg1 = pop();  prim_set_cdr ();  break;
    case 2:
      arg1 = pop();  prim_nullp ();    push_arg1();  break;
    case 3:
      arg2 = pop();  arg1 = pop();  prim_eqp ();      push_arg1();  break;
    case 4:
      arg1 = pop();  prim_not ();      push_arg1();  break;
    case 5:
      /* prim #%get-cont */
      arg1 = cont;
      push_arg1();
      break;
    case 6:
      /* prim #%graft-to-cont */

      arg1 = pop(); /* thunk to call */
      cont = pop(); /* continuation */

      push_arg1();

      pop_procedure ();
      build_env (handle_arity_and_rest_param (0));

      env = arg1;
      pc = entry;

      arg1 = OBJ_FALSE;

      break;
    case 7:
      /* prim #%return-to-cont */

      arg1 = pop(); /* value to return */
      cont = pop(); /* continuation */

      arg2 = ram_get_cdr (cont);

      pc = ram_get_entry (arg2);

      env = ram_get_car (arg2);
      cont = ram_get_car (cont);

      push_arg1();
      arg2 = OBJ_FALSE;

      break;
    case 8:
      /* prim #%halt */
      return;
    case 9:
      /* prim #%symbol? */
      arg1 = pop();  prim_symbolp ();  push_arg1();  break;
    case 10:
      /* prim #%string? */
      arg1 = pop();  prim_stringp ();  push_arg1();  break;
    case 11:
      /* prim #%string->list */
      arg1 = pop();  prim_string2list ();  push_arg1();  break;
    case 12:
      /* prim #%list->string */
      arg1 = pop();  prim_list2string ();  push_arg1();  break;
    case 13:
      /* prim #%make-u8vector */
      // not exactly like the standard Scheme function.
      // only takes one argument, and does not fill the vector
      arg1 = pop(); prim_make_u8vector (); push_arg1(); break;
    case 14:
      /* prim #%u8vector-ref */
      arg2 = pop(); arg1 = pop(); prim_u8vector_ref (); push_arg1(); break;
    case 15:
      /* prim #%u8vector-set! */
      arg3 = pop(); arg2 = pop(); arg1 = pop(); prim_u8vector_set (); break;
    }

    goto dispatch;

    /*************************************************************************/
  case PRIM3 :

    IF_TRACE(printf("  (%s)\n", prim_name[bytecode_lo4+32]));

    switch (bytecode_lo4) {
    case 0:
      /* prim #%print */
      arg1 = pop();
      prim_print ();
      break;
    case 1:
      /* prim #%clock */
      prim_clock ();  push_arg1();  break;
    case 2:
      /* prim #%motor */
      arg2 = pop();  arg1 = pop();  prim_motor ();  break;
    case 3:
      /* prim #%led */
      arg3 = pop();  arg2 = pop();  arg1 = pop();  prim_led ();  ;break;
    case 4:
      /* prim #%led2-color */
      arg1 = pop();  prim_led2_color ();  break;
    case 5:
      /* prim #%getchar-wait */
      arg2 = pop();  arg1 = pop();  prim_getchar_wait ();  push_arg1();  break;
    case 6:
      /* prim #%putchar */
      arg2 = pop();  arg1 = pop();  prim_putchar ();  break;
    case 7:
      /* prim #%beep */
      arg2 = pop();  arg1 = pop();  prim_beep ();  break;
    case 8:
      /* prim #%adc */
      arg1 = pop();  prim_adc ();  push_arg1();  break;
    case 9:
      /* prim #%u8vector? */
      arg1 = pop(); prim_u8vectorp (); push_arg1(); break;
    case 10:
      /* prim #%sernum */
      prim_sernum ();  push_arg1();  break;
    case 11:
      /* prim #%u8vector-length */
      arg1 = pop(); prim_u8vector_length (); push_arg1(); break;
    case 12:
      // FREE
      break;
    case 13:
      /* shift */
      arg1 = pop();
      pop();
      push_arg1();
      break;
    case 14:
      /* pop */
      pop();
      break;
    case 15:
      /* return */
      arg1 = pop();
      arg2 = ram_get_cdr (cont);
      pc = ram_get_entry (arg2);
      env = ram_get_car (arg2);
      cont = ram_get_car (cont);
      push_arg1();
      arg2 = OBJ_FALSE;
      break;
    }

    goto dispatch;

    /*************************************************************************/

  case PRIM4 :

    IF_TRACE(printf("  (%s)\n", prim_name[bytecode_lo4]));

    switch (bytecode_lo4) {
    case 0:
      /* prim #%boolean? */
      arg1 = pop(); prim_booleanp (); push_arg1(); break;
#ifdef NETWORKING
    case 1:
      /* prim #%network-init */
      prim_network_init (); break;
    case 2:
      /* prim #%network-cleanup */
      prim_network_cleanup (); break;
    case 3:
      /* prim #%receive-packet-to-u8vector */
      arg1 = pop(); prim_receive_packet_to_u8vector (); push_arg1(); break;
    case 4:
      /* prim #%send-packet-from-u8vector */
      arg2 = pop(); arg1 = pop(); prim_send_packet_from_u8vector ();
      push_arg1(); break;
#endif
    case 5:
      arg2 = pop(); arg1 = pop(); prim_ior (); push_arg1(); break;
      break;
    case 6:
      arg2 = pop(); arg1 = pop(); prim_xor (); push_arg1(); break;
      break;
#if 0
    case 7: // FREE
      break;
    case 8:
      break;
    case 9:
      break;
    case 10:
      break;
    case 11:
      break;
    case 12:
      break;
    case 13:
      break;
    case 14:
      break;
    case 15:
      break;
#endif
    }

    goto dispatch;

    /*************************************************************************/

  }
}
