/* file: "picobit-vm.c" */

/*
 * Copyright 2004-2009 by Marc Feeley and Vincent St-Amour, All Rights Reserved.
 *
 * History:
 *
 *   15/08/2004  Release of version 1
 *   06/07/2008  Modified for PICOBOARD2_R3
 *   18/07/2008  Modified to use new object representation
 *   17/12/2008  Release of version 2
 */

#include "picobit-vm.h"

/*---------------------------------------------------------------------------*/

// error handling

#ifdef WORKSTATION
void error (char *prim, char *msg) {
  printf ("ERROR: %s: %s\n", prim, msg);
  exit (1);
}

void type_error (char *prim, char *type) {
  printf ("ERROR: %s: An argument of type %s was expected\n", prim, type);
  exit (1);
}
#endif

/*---------------------------------------------------------------------------*/

// memory access

#ifdef WORKSTATION
uint8 rom_mem[ROM_BYTES] =
  {
#define RED_GREEN
#define PUTCHAR_LIGHT_not
#ifdef RED_GREEN
    0xFB, 0xD7, 0x03, 0x00, 0x00, 0x00, 0x00, 0x32
    , 0x03, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00
    , 0x08, 0x50, 0x80, 0x16, 0xFE, 0xE8, 0x00, 0xFC
    , 0x32, 0x80, 0x2D, 0xFE, 0xFC, 0x31, 0x80, 0x43
    , 0xFE, 0xFC, 0x33, 0x80, 0x2D, 0xFE, 0xFC, 0x31
    , 0x80, 0x43, 0xFE, 0x90, 0x16, 0x01, 0x20, 0xFC
    , 0x32, 0xE3, 0xB0, 0x37, 0x09, 0xF3, 0xFF, 0x20
    , 0xFC, 0x33, 0xE3, 0xB0, 0x40, 0x0A, 0xF3, 0xFF
    , 0x08, 0xF3, 0xFF, 0x01, 0x40, 0x21, 0xD1, 0x00
    , 0x02, 0xC0, 0x4C, 0x71, 0x01, 0x20, 0x50, 0x90
    , 0x51, 0x00, 0xF1, 0x40, 0xD8, 0xB0, 0x59, 0x90
    , 0x51, 0x00, 0xFF
#endif
#ifdef PUTCHAR_LIGHT
    0xFB, 0xD7, 0x00, 0x00, 0x80, 0x08, 0xFE, 0xE8
    , 0x00, 0xF6, 0xF5, 0x90, 0x08
#endif
  };
uint8 rom_get (rom_addr a) {
  return rom_mem[a-CODE_START];
}
#endif

word ram_get_gc_tags (obj o) { return RAM_GET_GC_TAGS_MACRO(o); }
word ram_get_gc_tag0 (obj o) { return RAM_GET_GC_TAG0_MACRO(o); }
word ram_get_gc_tag1 (obj o) { return RAM_GET_GC_TAG1_MACRO(o); }
void ram_set_gc_tags (obj o, word tags) { RAM_SET_GC_TAGS_MACRO(o, tags); }
void ram_set_gc_tag0 (obj o, word tag) { RAM_SET_GC_TAG0_MACRO(o,tag); }
void ram_set_gc_tag1 (obj o, word tag) { RAM_SET_GC_TAG1_MACRO(o,tag); }
word ram_get_field0 (obj o) { return RAM_GET_FIELD0_MACRO(o); }
word ram_get_field1 (obj o) { return RAM_GET_FIELD1_MACRO(o); }
word ram_get_field2 (obj o) { return RAM_GET_FIELD2_MACRO(o); }
word ram_get_field3 (obj o) { return RAM_GET_FIELD3_MACRO(o); }
word ram_get_fieldn (obj o, word n) { // TODO have as a macro ?
  switch (n) {
  case 0: return ram_get_field0 (o);
  case 1: return ram_get_field1 (o);
  case 2: return ram_get_field2 (o);
  case 3: return ram_get_field3 (o);
  }
}
void ram_set_field0 (obj o, word val) { RAM_SET_FIELD0_MACRO(o,val); }
void ram_set_field1 (obj o, word val) { RAM_SET_FIELD1_MACRO(o,val); }
void ram_set_field2 (obj o, word val) { RAM_SET_FIELD2_MACRO(o,val); }
void ram_set_field3 (obj o, word val) { RAM_SET_FIELD3_MACRO(o,val); }
void ram_set_fieldn (obj o, uint8 n, word val) { // TODO have as a macro ?
  switch (n) {
  case 0: ram_set_field0 (o, val); break;
  case 1: ram_set_field1 (o, val); break;
  case 2: ram_set_field2 (o, val); break;
  case 3: ram_set_field3 (o, val); break;
  }  
}
word rom_get_field0 (obj o) { return ROM_GET_FIELD0_MACRO(o); }
word rom_get_field1 (obj o) { return ROM_GET_FIELD1_MACRO(o); }
word rom_get_field2 (obj o) { return ROM_GET_FIELD2_MACRO(o); }
word rom_get_field3 (obj o) { return ROM_GET_FIELD3_MACRO(o); }

obj ram_get_car (obj o)
{ return ((ram_get_field0 (o) & 0x1f) << 8) | ram_get_field1 (o); }
obj rom_get_car (obj o)
{ return ((rom_get_field0 (o) & 0x1f) << 8) | rom_get_field1 (o); }
obj ram_get_cdr (obj o)
{ return ((ram_get_field2 (o) & 0x1f) << 8) | ram_get_field3 (o); }
obj rom_get_cdr (obj o)
{ return ((rom_get_field2 (o) & 0x1f) << 8) | rom_get_field3 (o); }

void ram_set_car (obj o, obj val) {
  ram_set_field0 (o, (val >> 8) | (ram_get_field0 (o) & 0xe0));
  ram_set_field1 (o, val & 0xff);
}
void ram_set_cdr (obj o, obj val) {
  ram_set_field2 (o, (val >> 8) | (ram_get_field2 (o) & 0xe0));
  ram_set_field3 (o, val & 0xff);
}

// function entry point
obj ram_get_entry (obj o) {
  return (((ram_get_field0 (o) & 0x1f) << 11)
	  | (ram_get_field1 (o) << 3)
	  | (ram_get_field2 (o) >> 5));
}
obj rom_get_entry (obj o){
  return (((rom_get_field0 (o) & 0x1f) << 11)
	  | (rom_get_field1 (o) << 3)
	  | (rom_get_field2 (o) >> 5));
}

obj get_global (uint8 i) {
// globals occupy the beginning of ram, with 2 globals per word
  if (i & 1)
    return ram_get_cdr (MIN_RAM_ENCODING + (i / 2));
  else
    return ram_get_car (MIN_RAM_ENCODING + (i / 2));
}
void set_global (uint8 i, obj o) {
  if (i & 1)
    ram_set_cdr (MIN_RAM_ENCODING + (i / 2), o);
  else
    ram_set_car (MIN_RAM_ENCODING + (i / 2), o);
}

// TODO generic functions (get_field0, get_car, etc) that work for both rom and ram were not used, are in garbage

/*---------------------------------------------------------------------------*/

// primitives

#ifndef INFINITE_PRECISION_BIGNUMS

// regular (finite, 24 bits) bignums

int32 decode_int (obj o) {
  uint8 u;
  uint8 h;
  uint8 l;

  if (o < MIN_FIXNUM_ENCODING)
    TYPE_ERROR("decode_int.0", "integer");

  if (o <= (MIN_FIXNUM_ENCODING + (MAX_FIXNUM - MIN_FIXNUM)))
    return DECODE_FIXNUM(o);

  if (IN_RAM(o)) {
    if (!RAM_BIGNUM(o))
      TYPE_ERROR("decode_int.1", "integer");
    
    u = ram_get_field1 (o);
    h = ram_get_field2 (o);
    l = ram_get_field3 (o);
  }
  else if (IN_ROM(o)) {
    if (!ROM_BIGNUM(o))
      TYPE_ERROR("decode_int.2", "integer");
    
    u = rom_get_field1 (o);
    h = rom_get_field2 (o);
    l = rom_get_field3 (o);
  }
  else
    TYPE_ERROR("decode_int.3", "integer");
  
  if (u >= 128) // negative
    return ((int32)((((int16)u - 256) << 8) + h) << 8) + l;
  
  return ((int32)(((int16)u << 8) + h) << 8) + l;
}

obj encode_int (int32 n) {
  if (n >= MIN_FIXNUM && n <= MAX_FIXNUM)
    return ENCODE_FIXNUM(n);

  return alloc_ram_cell_init (BIGNUM_FIELD0, n >> 16, n >> 8, n);
}

#endif

void decode_2_int_args (void) {
  a1 = decode_int (arg1);
  a2 = decode_int (arg2);
}

/*---------------------------------------------------------------------------*/

#ifdef WORKSTATION

int hidden_fgetc (FILE *f)
{
  int c = fgetc (f);
#if 0
  printf ("{%d}",c);
  fflush (stdout);
#endif
  return c;
}

#define fgetc(f) hidden_fgetc(f)

void write_hex_nibble (int n)
{
  putchar ("0123456789ABCDEF"[n]);
}

void write_hex (uint8 n)
{
  write_hex_nibble (n >> 4);
  write_hex_nibble (n & 0x0f);
}

int hex (int c)
{
  if (c >= '0' && c <= '9')
    return (c - '0');

  if (c >= 'A' && c <= 'F')
    return (c - 'A' + 10);

  if (c >= 'a' && c <= 'f')
    return (c - 'a' + 10);

  return -1;
}

int read_hex_byte (FILE *f)
{
  int h1 = hex (fgetc (f));
  int h2 = hex (fgetc (f));

  if (h1 >= 0 && h2 >= 0)
    return (h1<<4) + h2;

  return -1;
}

int read_hex_file (char *filename)
{
  int c;
  FILE *f = fopen (filename, "r");
  int result = 0;
  int len;
  int a, a1, a2;
  int t;
  int b;
  int i;
  uint8 sum;
  int hi16 = 0;

  for (i=0; i<ROM_BYTES; i++)
    rom_mem[i] = 0xff;

  if (f != NULL)
    {
      while ((c = fgetc (f)) != EOF)
        {
          if ((c == '\r') || (c == '\n'))
            continue;

          if (c != ':' ||
              (len = read_hex_byte (f)) < 0 ||
              (a1 = read_hex_byte (f)) < 0 ||
              (a2 = read_hex_byte (f)) < 0 ||
              (t = read_hex_byte (f)) < 0)
            break;

          a = (a1 << 8) + a2;

          i = 0;
          sum = len + a1 + a2 + t;

          if (t == 0)
            {
            next0:

              if (i < len)
                {
                  unsigned long adr = ((unsigned long)hi16 << 16) + a - CODE_START;

                  if ((b = read_hex_byte (f)) < 0)
                    break;

                  if (adr >= 0 && adr < ROM_BYTES)
                    rom_mem[adr] = b;

                  a = (a + 1) & 0xffff;
                  i++;
                  sum += b;

                  goto next0;
                }
            }
          else if (t == 1)
            {
              if (len != 0)
                break;
            }
          else if (t == 4)
            {
              if (len != 2)
                break;

              if ((a1 = read_hex_byte (f)) < 0 ||
                  (a2 = read_hex_byte (f)) < 0)
                break;

              sum += a1 + a2;

              hi16 = (a1<<8) + a2;
            }
          else
            break;

          if ((b = read_hex_byte (f)) < 0)
            break;

          sum = -sum;

          if (sum != b)
            {
              printf ("*** HEX file checksum error (expected 0x%02x)\n", sum);
              break;
            }

          c = fgetc (f);

          if ((c != '\r') && (c != '\n'))
            break;

          if (t == 1)
            {
              result = 1;
              break;
            }
        }

      if (result == 0)
        printf ("*** HEX file syntax error\n");

      fclose (f);
    }

  return result;
}

#endif

/*---------------------------------------------------------------------------*/

#define FETCH_NEXT_BYTECODE() bytecode = rom_get (pc++)

#define BEGIN_DISPATCH()                        \
  dispatch:                                     \
  IF_TRACE(show_state (pc));                    \
  FETCH_NEXT_BYTECODE();                        \
  bytecode_hi4 = bytecode & 0xf0;               \
  bytecode_lo4 = bytecode & 0x0f;               \
  switch (bytecode_hi4 >> 4) {

#define END_DISPATCH() }

#define CASE(opcode) case (opcode>>4):;

#define DISPATCH(); goto dispatch;

#if 0
#define pc FSR1
#define sp FSR2
#define bytecode TABLAT
#define bytecode_hi4 WREG
#endif

#define PUSH_CONSTANT1     0x00
#define PUSH_CONSTANT2     0x10
#define PUSH_STACK1        0x20
#define PUSH_STACK2        0x30
#define PUSH_GLOBAL        0x40
#define SET_GLOBAL         0x50
#define CALL               0x60
#define JUMP               0x70
#define LABEL_INSTR        0x80
#define PUSH_CONSTANT_LONG 0x90

#define FREE1              0xa0
#define FREE2              0xb0

#define PRIM1              0xc0
#define PRIM2              0xd0
#define PRIM3              0xe0
#define PRIM4              0xf0

#ifdef WORKSTATION

char *prim_name[64] =
  {
    "prim #%number?",
    "prim #%+",
    "prim #%-",
    "prim #%*",
    "prim #%quotient",
    "prim #%remainder",
    "prim #%neg",
    "prim #%=",
    "prim #%<",
    "prim #%ior",
    "prim #%>",
    "prim #%xor",
    "prim #%pair?",
    "prim #%cons",
    "prim #%car",
    "prim #%cdr",
    "prim #%set-car!",
    "prim #%set-cdr!",
    "prim #%null?",
    "prim #%eq?",
    "prim #%not",
    "prim #%get-cont",
    "prim #%graft-to-cont",
    "prim #%return-to-cont",
    "prim #%halt",
    "prim #%symbol?",
    "prim #%string?",
    "prim #%string->list",
    "prim #%list->string",
    "prim #%make-u8vector",
    "prim #%u8vector-ref",
    "prim #%u8vector-set!",
    "prim #%print",
    "prim #%clock",
    "prim #%motor",
    "prim #%led",
    "prim #%led2-color",
    "prim #%getchar-wait",
    "prim #%putchar",
    "prim #%beep",
    "prim #%adc",
    "prim #%u8vector?",
    "prim #%sernum",
    "prim #%u8vector-length",
    "prim #%u8vector-copy!",
    "shift",
    "pop",
    "return",
    "prim #%boolean?",
    "prim #%network-init",
    "prim #%network-cleanup",
    "prim #%receive-packet-to-u8vector",
    "prim #%send-packet-from-u8vector",
    "prim 53",
    "prim 54",
    "prim 55",
    "prim 56",
    "prim 57",
    "prim 58",
    "prim 59",
    "prim 60",
    "prim 61",
    "prim 62",
    "prim 63"
  };

#endif

#define PUSH_ARG1() push_arg1 ()
#define POP() pop()

void push_arg1 (void)
{
  env = cons (arg1, env);
  arg1 = OBJ_FALSE;
}

obj pop (void)
{
  obj o = ram_get_car (env);
  env = ram_get_cdr (env);
  return o;
}

void pop_procedure (void)
{
  arg1 = POP();
  
  if (IN_RAM(arg1))
    {      
      if (!RAM_CLOSURE(arg1))
	TYPE_ERROR("pop_procedure.0", "procedure");
      
      entry = ram_get_entry (arg1) + CODE_START;
    }
  else if (IN_ROM(arg1))
    {      
      if (!ROM_CLOSURE(arg1))
        TYPE_ERROR("pop_procedure.1", "procedure");

      entry = rom_get_entry (arg1) + CODE_START;
    }
  else
    TYPE_ERROR("pop_procedure.2", "procedure");
}

void handle_arity_and_rest_param (void)
{
  uint8 np;

  np = rom_get (entry++);

  if ((np & 0x80) == 0)
    {
      if (na != np)
        ERROR("handle_arity_and_rest_param.0", "wrong number of arguments");
    }
  else
    {
      np = ~np;

      if (na < np)
        ERROR("handle_arity_and_rest_param.1", "wrong number of arguments");

      arg3 = OBJ_NULL;

      while (na > np)
        {
          arg4 = POP();

          arg3 = cons (arg4, arg3);
          arg4 = OBJ_FALSE;

          na--;
        }

      arg1 = cons (arg3, arg1);
      arg3 = OBJ_FALSE;
    }
}

void build_env (void)
{
  while (na != 0)
    {
      arg3 = POP();

      arg1 = cons (arg3, arg1);

      na--;
    }

  arg3 = OBJ_FALSE;
}

void save_cont (void)
{
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

void interpreter (void)
{
  pc = (CODE_START + 4) + ((rom_addr)rom_get (CODE_START+2) << 2);

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

  while (bytecode_lo4 != 0)
    {
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

  while (bytecode_lo4 != 0)
    {
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

  switch (bytecode_lo4)
    {
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
      
      arg1 = alloc_ram_cell_init (CLOSURE_FIELD0 | (arg2 >> 3),
				  ((arg2 & 0x07) << 5) | (bytecode >> 3),
				  ((bytecode &0x07) <<5) |((arg3 &0x1f00) >>8),
				  arg3 & 0xff);
      
      PUSH_ARG1();
      
      arg2 = OBJ_FALSE;
      arg3 = OBJ_FALSE;
      
      break;

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

  arg1 = (bytecode_lo4 << 8) | bytecode;
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

  switch (bytecode_lo4)
    {
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
      arg2 = POP(); arg1 = POP(); prim_ior (); PUSH_ARG1(); break;
    case 10:
      arg2 = POP();  arg1 = POP();  prim_gt ();       PUSH_ARG1();  break;
    case 11:
      arg2 = POP(); arg1 = POP(); prim_xor (); PUSH_ARG1(); break;
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

  switch (bytecode_lo4)
    {
    case 0:
      arg2 = POP();  arg1 = POP();  prim_set_car ();                break;
    case 1:
      arg2 = POP();  arg1 = POP();  prim_set_cdr ();                break;
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

  switch (bytecode_lo4)
    {
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
  
  switch (bytecode_lo4)
    {
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
      break;
    case 6:
      break;
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
    }
  
  DISPATCH();

  /***************************************************************************/
  
  END_DISPATCH();
}

/*---------------------------------------------------------------------------*/

#ifdef WORKSTATION

void usage (void)
{
  printf ("usage: sim file.hex\n");
  exit (1);
}

int main (int argc, char *argv[])
{
  int errcode = 1;
  rom_addr rom_start_addr = 0;

#ifdef TEST_BIGNUM
  test();
#endif
  
  if (argc > 1 && argv[1][0] == '-' && argv[1][1] == 's')
    {
      int h1;
      int h2;
      int h3;
      int h4;

      if ((h1 = hex (argv[1][2])) < 0 ||
          (h2 = hex (argv[1][3])) < 0 ||
          (h3 = hex (argv[1][4])) != 0 ||
          (h4 = hex (argv[1][5])) != 0 ||
          argv[1][6] != '\0')
        usage ();

      rom_start_addr = (h1 << 12) | (h2 << 8) | (h3 << 4) | h4;

      argv++;
      argc--;
    }

#ifdef DEBUG
  printf ("Start address = 0x%04x\n", rom_start_addr + CODE_START);
#endif

  if (argc != 2)
    usage ();

  if (!read_hex_file (argv[1]))
    printf ("*** Could not read hex file \"%s\"\n", argv[1]);
  else
    {
      int i;

      if (rom_get (CODE_START+0) != 0xfb ||
          rom_get (CODE_START+1) != 0xd7)
        printf ("*** The hex file was not compiled with PICOBIT\n");
      else
        {
#if 0
          for (i=0; i<8192; i++)
            if (rom_get (i) != 0xff)
              printf ("rom_mem[0x%04x] = 0x%02x\n", i, rom_get (i));
#endif

          interpreter ();

#ifdef DEBUG_GC
          printf ("**************** memory needed = %d\n", max_live+1);
#endif
        }
    }

  return errcode;
}

#endif
