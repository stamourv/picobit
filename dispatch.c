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
  arg1 = POP();
  
  if (IN_RAM(arg1)) {      
    if (!RAM_CLOSURE(arg1))
      TYPE_ERROR("pop_procedure.0", "procedure");
    
    entry = ram_get_entry (arg1) + CODE_START;
  }
  else if (IN_ROM(arg1)) {      
    if (!ROM_CLOSURE(arg1))
      TYPE_ERROR("pop_procedure.1", "procedure");
    
    entry = rom_get_entry (arg1) + CODE_START;
  }
  else
    TYPE_ERROR("pop_procedure.2", "procedure");
}

void handle_arity_and_rest_param () {
  uint8 np;
  
  np = rom_get (entry++);
  
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
      arg4 = POP();
      
      arg3 = cons (arg4, arg3);
      arg4 = OBJ_FALSE;
      
      na--;
    }
    
    arg1 = cons (arg3, arg1);
    arg3 = OBJ_FALSE;
  }
}

void build_env () {
  while (na != 0) {
    arg3 = POP();
    
    arg1 = cons (arg3, arg1);
    
    na--;
  }
  
  arg3 = OBJ_FALSE;
}

void save_cont () {
  // the second half is a closure
  arg3 = alloc_ram_cell_init (CLOSURE_FIELD0 | (pc >> 11),
			      (pc >> 3) & 0xff,
			      ((pc & 0x0007) << 5) | (env >> 8),
			      env & 0xff);
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
  
  BEGIN_DISPATCH();

  /***************************************************************************/
  CASE(PUSH_CONSTANT1);

  IF_TRACE(printf("  (push-constant "); show (bytecode_lo4); printf (")\n"));

  arg1 = bytecode_lo4;

  PUSH_ARG1();

  DISPATCH();

  /***************************************************************************/
  CASE(PUSH_CONSTANT2);

  IF_TRACE(printf("  (push-constant "); show (bytecode_lo4+16); printf (")\n"));
  arg1 = bytecode_lo4+16;

  PUSH_ARG1();

  DISPATCH();

  /***************************************************************************/
  CASE(PUSH_STACK1);

  IF_TRACE(printf("  (push-stack %d)\n", bytecode_lo4));

  arg1 = env;

  while (bytecode_lo4 != 0) {
    arg1 = ram_get_cdr (arg1);
    bytecode_lo4--;
  }

  arg1 = ram_get_car (arg1);

  PUSH_ARG1();

  DISPATCH();

  /***************************************************************************/
  CASE(PUSH_STACK2);

  IF_TRACE(printf("  (push-stack %d)\n", bytecode_lo4+16));

  bytecode_lo4 += 16;

  arg1 = env;

  while (bytecode_lo4 != 0) {
    arg1 = ram_get_cdr (arg1);
    bytecode_lo4--;
  }

  arg1 = ram_get_car (arg1);

  PUSH_ARG1();
  
  DISPATCH();

  /***************************************************************************/
  CASE(PUSH_GLOBAL);

  IF_TRACE(printf("  (push-global %d)\n", bytecode_lo4));

  arg1 = get_global (bytecode_lo4);

  PUSH_ARG1();

  DISPATCH();

  /***************************************************************************/
  CASE(SET_GLOBAL);

  IF_TRACE(printf("  (set-global %d)\n", bytecode_lo4));

  set_global (bytecode_lo4, POP());

  DISPATCH();

  /***************************************************************************/
  CASE(CALL);

  IF_TRACE(printf("  (call %d)\n", bytecode_lo4));

  na = bytecode_lo4;

  pop_procedure ();
  handle_arity_and_rest_param ();
  build_env ();
  save_cont ();

  env = arg1;
  pc = entry;

  arg1 = OBJ_FALSE;

  DISPATCH();

  /***************************************************************************/
  CASE(JUMP);

  IF_TRACE(printf("  (jump %d)\n", bytecode_lo4));

  na = bytecode_lo4;

  pop_procedure ();
  handle_arity_and_rest_param ();
  build_env ();

  env = arg1;
  pc = entry;

  arg1 = OBJ_FALSE;

  DISPATCH();

  /***************************************************************************/
  CASE(LABEL_INSTR);

  switch (bytecode_lo4) {
  case 0: // call-toplevel
    FETCH_NEXT_BYTECODE();  
    arg2 = bytecode;
    
    FETCH_NEXT_BYTECODE();
    
    IF_TRACE(printf("  (call-toplevel 0x%04x)\n",
		    ((arg2 << 8) | bytecode) + CODE_START));
    
    entry = (arg2 << 8) + bytecode + CODE_START;
    arg1 = OBJ_NULL;
      
    na = rom_get (entry++);
    
    build_env ();
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
    
    na = rom_get (entry++);
    
    build_env ();
    
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
    
    if (POP() == OBJ_FALSE)
      pc = (arg2 << 8) + bytecode + CODE_START;
    
    break;
    
  case 4: // closure
    FETCH_NEXT_BYTECODE();
    arg2 = bytecode;
    
    FETCH_NEXT_BYTECODE();
    
    IF_TRACE(printf("  (closure 0x%04x)\n", (arg2 << 8) | bytecode));
    
    arg3 = POP(); // env
    
    entry = (arg2 << 8) | bytecode;
    
    arg1 =
      alloc_ram_cell_init (CLOSURE_FIELD0 | (arg2 >> 3),
			   ((arg2 & 0x07) << 5) | (bytecode >> 3),
			   ((bytecode & 0x07) << 5) | ((arg3 & 0x1f00) >> 8),
			   arg3 & 0xff);
    
    PUSH_ARG1();
    
    arg2 = OBJ_FALSE;
    arg3 = OBJ_FALSE;
    
    break;

#if 0
  case 5: // call-toplevel-short
    FETCH_NEXT_BYTECODE(); // TODO the short version have a lot in common with the long ones, abstract ?
    // TODO short instructions don't work at the moment
    IF_TRACE(printf("  (call-toplevel-short 0x%04x)\n",
		    pc + bytecode + CODE_START));
    
    entry = pc + bytecode + CODE_START;
    arg1 = OBJ_NULL;
    
    na = rom_get (entry++);
    
    build_env ();
    save_cont ();
    
    env = arg1;
    pc = entry;
    
    arg1 = OBJ_FALSE;
    
    break;
    
  case 6: // jump-toplevel-short
    FETCH_NEXT_BYTECODE();
    
    IF_TRACE(printf("  (jump-toplevel-short 0x%04x)\n",
		    pc + bytecode + CODE_START));
    
    entry = pc + bytecode + CODE_START;
    arg1 = OBJ_NULL;
    
    na = rom_get (entry++);
    
    build_env ();
    
    env = arg1;
    pc = entry;
    
    arg1 = OBJ_FALSE;
    
    break;
    
  case 7: // goto-short
    FETCH_NEXT_BYTECODE();
    
    IF_TRACE(printf("  (goto-short 0x%04x)\n", pc + bytecode + CODE_START));
    
    pc = pc + bytecode + CODE_START;
    
    break;
    
  case 8: // goto-if-false-short
    FETCH_NEXT_BYTECODE();
    
    IF_TRACE(printf("  (goto-if-false-short 0x%04x)\n",
		    pc + bytecode + CODE_START));
    
    if (POP() == OBJ_FALSE)
      pc = pc + bytecode + CODE_START;
    
    break;
    
  case 9: // closure-short
    FETCH_NEXT_BYTECODE();
    
    IF_TRACE(printf("  (closure-short 0x%04x)\n", pc + bytecode));
    
    arg3 = POP(); // env
    
    entry = pc + bytecode;
    
    arg1 = alloc_ram_cell_init (CLOSURE_FIELD0 | (arg2 >> 3),
				((arg2 & 0x07) << 5) | (bytecode >> 3),
				((bytecode &0x07) <<5) |((arg3 &0x1f00) >>8),
				arg3 & 0xff);
    
    PUSH_ARG1();
    
    arg3 = OBJ_FALSE;
    
    break;
#endif
    
#if 0
  case 10:
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
    
    PUSH_ARG1();
    
    break;
    
  case 15: // set_global [long]
    FETCH_NEXT_BYTECODE();
    
    IF_TRACE(printf("  (set-global [long] %d)\n", bytecode));
    
    set_global (bytecode, POP());
    
    break;
  }
  
  DISPATCH();

  /***************************************************************************/
  CASE(PUSH_CONSTANT_LONG);
  
  /* push-constant [long] */

  FETCH_NEXT_BYTECODE();
  
  IF_TRACE(printf("  (push [long] 0x%04x)\n", (bytecode_lo4 << 8) + bytecode));

  // necessary since SIXPIC would have kept the result of the shift at 8 bits
  arg1 = bytecode_lo4;
  arg1 = (arg1 << 8) | bytecode;
  PUSH_ARG1();
  
  DISPATCH();
  
  /***************************************************************************/
  CASE(FREE1); // FREE

  DISPATCH();

  /***************************************************************************/
  CASE(FREE2); // FREE

  DISPATCH();

  /***************************************************************************/
  CASE(PRIM1);

  IF_TRACE(printf("  (%s)\n", prim_name[bytecode_lo4]));

  switch (bytecode_lo4) {
  case 0:
    arg1 = POP();  prim_numberp ();  PUSH_ARG1();  break;
  case 1:
    arg2 = POP();  arg1 = POP();  prim_add ();      PUSH_ARG1();  break;
  case 2:
    arg2 = POP();  arg1 = POP();  prim_sub ();      PUSH_ARG1();  break;
  case 3:
    arg2 = POP();  arg1 = POP();  prim_mul ();      PUSH_ARG1();  break;
  case 4:
    arg2 = POP();  arg1 = POP();  prim_div ();      PUSH_ARG1();  break;
  case 5:
    arg2 = POP();  arg1 = POP();  prim_rem ();      PUSH_ARG1();  break;
  case 6:
    arg1 = POP();  prim_neg ();      PUSH_ARG1();  break;
  case 7:
    arg2 = POP();  arg1 = POP();  prim_eq ();       PUSH_ARG1();  break;
  case 8:
    arg2 = POP();  arg1 = POP();  prim_lt ();       PUSH_ARG1();  break;
  case 9:
    arg2 = POP();  arg1 = POP();  prim_leq ();      PUSH_ARG1();  break;
  case 10:
    arg2 = POP();  arg1 = POP();  prim_gt ();       PUSH_ARG1();  break;
  case 11:
    arg2 = POP();  arg1 = POP();  prim_geq ();      PUSH_ARG1();  break;
  case 12:
    arg1 = POP();  prim_pairp ();    PUSH_ARG1();  break;
  case 13:
    arg2 = POP();  arg1 = POP();  prim_cons ();     PUSH_ARG1();  break;
  case 14:
    arg1 = POP();  prim_car ();      PUSH_ARG1();  break;
  case 15:
    arg1 = POP();  prim_cdr ();      PUSH_ARG1();  break;
  }
  
  DISPATCH();
  
  /***************************************************************************/
  CASE(PRIM2);
  
  IF_TRACE(printf("  (%s)\n", prim_name[bytecode_lo4+16]));

  switch (bytecode_lo4) {
  case 0:
    arg2 = POP();  arg1 = POP();  prim_set_car ();  break;
  case 1:
    arg2 = POP();  arg1 = POP();  prim_set_cdr ();  break;
  case 2:
    arg1 = POP();  prim_nullp ();    PUSH_ARG1();  break;
  case 3:
    arg2 = POP();  arg1 = POP();  prim_eqp ();      PUSH_ARG1();  break;
  case 4:
    arg1 = POP();  prim_not ();      PUSH_ARG1();  break;
  case 5:
    /* prim #%get-cont */
    arg1 = cont;
    PUSH_ARG1();
    break;
  case 6:
    /* prim #%graft-to-cont */
    
    arg1 = POP(); /* thunk to call */
    cont = POP(); /* continuation */
    
    PUSH_ARG1();
    
    na = 0;
    
    pop_procedure ();
    handle_arity_and_rest_param ();
    build_env ();
    
    env = arg1;
    pc = entry;
    
    arg1 = OBJ_FALSE;
    
    break;
  case 7:
    /* prim #%return-to-cont */
    
    arg1 = POP(); /* value to return */
    cont = POP(); /* continuation */
    
    arg2 = ram_get_cdr (cont);
    
    pc = ram_get_entry (arg2);
    
    env = ram_get_cdr (arg2);
    cont = ram_get_car (cont);
    
    PUSH_ARG1();
    arg2 = OBJ_FALSE;
    
    break;
  case 8:
    /* prim #%halt */
    return;
  case 9:
    /* prim #%symbol? */
    arg1 = POP();  prim_symbolp ();  PUSH_ARG1();  break;
  case 10:
    /* prim #%string? */
    arg1 = POP();  prim_stringp ();  PUSH_ARG1();  break;
  case 11:
    /* prim #%string->list */
    arg1 = POP();  prim_string2list ();  PUSH_ARG1();  break;
  case 12:
    /* prim #%list->string */
    arg1 = POP();  prim_list2string ();  PUSH_ARG1();  break;
  case 13:
    /* prim #%make-u8vector */
    arg2 = POP(); arg1 = POP(); prim_make_u8vector (); PUSH_ARG1(); break;
  case 14:
    /* prim #%u8vector-ref */
    arg2 = POP(); arg1 = POP(); prim_u8vector_ref (); PUSH_ARG1(); break;
  case 15:
    /* prim #%u8vector-set! */
    arg3 = POP(); arg2 = POP(); arg1 = POP(); prim_u8vector_set (); break;
  }
  
  DISPATCH();

  /***************************************************************************/
  CASE(PRIM3);

  IF_TRACE(printf("  (%s)\n", prim_name[bytecode_lo4+32]));

  switch (bytecode_lo4) {
  case 0:
    /* prim #%print */
    arg1 = POP();
    prim_print ();
    break;
  case 1:
    /* prim #%clock */
    prim_clock ();  PUSH_ARG1();  break;
  case 2:
    /* prim #%motor */
    arg2 = POP();  arg1 = POP();  prim_motor ();  break;
  case 3:
    /* prim #%led */
    arg3 = POP();  arg2 = POP();  arg1 = POP();  prim_led ();  ;break;
  case 4:
    /* prim #%led2-color */
    arg1 = POP();  prim_led2_color ();  break;
  case 5:
    /* prim #%getchar-wait */
    arg2 = POP();  arg1 = POP();  prim_getchar_wait ();  PUSH_ARG1();  break;
  case 6:
    /* prim #%putchar */
    arg2 = POP();  arg1 = POP();  prim_putchar ();  break;
  case 7:
    /* prim #%beep */
    arg2 = POP();  arg1 = POP();  prim_beep ();  break;
  case 8:
    /* prim #%adc */
    arg1 = POP();  prim_adc ();  PUSH_ARG1();  break;
  case 9:
    /* prim #%u8vector? */
    arg1 = POP(); prim_u8vectorp (); PUSH_ARG1(); break;
  case 10:
    /* prim #%sernum */
    prim_sernum ();  PUSH_ARG1();  break;
  case 11:
    /* prim #%u8vector-length */
    arg1 = POP(); prim_u8vector_length (); PUSH_ARG1(); break;
  case 12:
    /* prim #%u8vector-copy! */
    arg5 = POP(); arg4 = POP(); arg3 = POP(); arg2 = POP(); arg1 = POP();
    prim_u8vector_copy (); break;
    break;
  case 13:
    /* shift */
    arg1 = POP();
    POP();
    PUSH_ARG1();
    break;
  case 14:
    /* pop */
    POP();
    break;
  case 15:
    /* return */
    arg1 = POP();
    arg2 = ram_get_cdr (cont);
    pc = ram_get_entry (arg2);
    env = ram_get_cdr (arg2);
    cont = ram_get_car (cont);
    PUSH_ARG1();
    arg2 = OBJ_FALSE;
    break;
  }
  
  DISPATCH();

  /***************************************************************************/

  CASE(PRIM4);

  IF_TRACE(printf("  (%s)\n", prim_name[bytecode_lo4]));
  
  switch (bytecode_lo4) {
  case 0:
    /* prim #%boolean? */
    arg1 = POP(); prim_booleanp (); PUSH_ARG1(); break;
  case 1:
    /* prim #%network-init */
    prim_network_init (); break;
  case 2:
    /* prim #%network-cleanup */
    prim_network_cleanup (); break;
  case 3:
    /* prim #%receive-packet-to-u8vector */
    arg1 = POP(); prim_receive_packet_to_u8vector (); PUSH_ARG1(); break;
  case 4:
    /* prim #%send-packet-from-u8vector */
    arg2 = POP(); arg1 = POP(); prim_send_packet_from_u8vector ();
    PUSH_ARG1(); break;
  case 5:
    arg2 = POP(); arg1 = POP(); prim_ior (); PUSH_ARG1(); break;
    break;
  case 6:
    arg2 = POP(); arg1 = POP(); prim_xor (); PUSH_ARG1(); break;
    break;
#if 0
  case 7:
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
  
  DISPATCH();
  
  /***************************************************************************/
  
  END_DISPATCH();
}
