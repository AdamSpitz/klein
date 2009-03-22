# include "abstract_interpreter.hh"
# include "abstract_interpreter_inline.hh"

# include "objVectorLayout.hh"
# include "byteVectorLayout.hh"
# include "immediateLayout.hh"
# include "maps.hh"


abstract_interpreter::abstract_interpreter(Oop methodOop)
 : mi(MemoryObjectLayout().mapOf(methodOop)) {
  pc= mi.firstBCI();
  error_msg= NULL;
}


void abstract_interpreter_method_info::init(ByteVectorOop c, ObjVectorOop l) {
     codes_object = c;
  literals_object = l;
  codes           = (unsigned char*) ByteVectorLayout().for_AddressOfIndexableAt(codes_object, 0);
  literals        =  ObjVectorLayout().for_AddressOfIndexableAt(literals_object, 0);
  length_codes    = ByteVectorLayout().indexableSizeOf(   codes_object);
  length_literals =  ObjVectorLayout().indexableSizeOf(literals_object);
  
  if (length_codes == 0) {
    instruction_set = TWENTIETH_CENTURY_INSTRUCTION_SET;
  }
  else {
    char first_code = ByteVectorLayout().for_IndexableAt(codes_object, 0);
    instruction_set =     getOp((u_char)first_code) == INSTRUCTION_SET_SELECTION_CODE
                        ? (InstructionSetKind) getIndex(first_code)
                        : TWENTIETH_CENTURY_INSTRUCTION_SET;
    always_assert( instruction_set == TWENTIETH_CENTURY_INSTRUCTION_SET
                  ||  0 <= instruction_set  &&  instruction_set <= LAST_INSTRUCTION_SET,
                 "bad instruction set");
  }    
}


abstract_interpreter_method_info::abstract_interpreter_method_info(
                                                              MethodMapOop m)  {
  // TODO: I don't know how to check whether it hasCode or not. always_assert(MethodMapLayout().hasCode(m), "cannot interpret data");
  _map_oop       = MethodMapLayout().enclosingMapOopOf(m);
  init(MethodMapLayout().codesOf(m), MethodMapLayout().literalsOf(m));
}    


/*
TODO AAA
void abstract_interpreter_method_info::print_short() {
  lprintf("method_map %#lx, "
         "codes %#lx, length %d,  literals %#lx, length %d\n",
         map(), 
         codes,         length_codes,
         literals,      length_literals);
}


void abstract_interpreter_bytecode_info::print_short() {
  lprintf( "code %d, op %d, index %d\n", code, op, x);
}


void abstract_interpreter_interbytecode_state::print_short() {
  lprintf( "lexical_level %d, index %d,"
          " delegatee %#lx, is_undirected_resend %s,"
          " argument_count %d\n", 
          lexical_level, index, 
          (unsigned long)delegatee, 
          is_undirected_resend ? "true" : "false",
          argument_count);
}




void abstract_interpreter::print_short() {
  lprintf( "pc %d\n", pc);
  mi.print_short();
  bc.print_short();
  is.print_short();
}
*/


void abstract_interpreter::interpret_method() {
  for ( ;  pc < mi.length_codes;  ++pc ) {
    interpret_bytecode();
    if ( get_error_msg() )
      return;
  }
}

void abstract_interpreter::fetch_and_decode_bytecode() {
  bc.decode(mi.codes[pc]);

  // when asserts are turned on, it is illegal to carry
  // a non-zero index during NO_OPERAND_CODE's.
  // So the following predicate is technically not needed.
  // But I don't want illegal programs doing strange things
  // with asserts off, so put it in anyway -- dmu.
  if ( bc.op != NO_OPERAND_CODE ) 
    is.index = (is.index << INDEXWIDTH) | bc.x;
}

#   define interpret(opExpr) \
      pre_ ## opExpr (); \
       do_ ## opExpr (); \
     post_ ## opExpr ();

# define case_op(opName) \
  case opName: interpret(opName)
  
void abstract_interpreter::dispatch_bytecode() {
  switch (bc.op) {
   default: interpret(illegal_code);               break;
   case_op(LEXICAL_LEVEL_CODE);                    break;
   case_op(READ_LOCAL_CODE);                       break;
   case_op(WRITE_LOCAL_CODE);                      break;
   case_op(INDEX_CODE);                            break;
   case_op(LITERAL_CODE);                          break;
   case_op(DELEGATEE_CODE);                        break;
   case_op(ARGUMENT_COUNT_CODE);                   break;
   case_op(SEND_CODE);                             break;
   case_op(IMPLICIT_SEND_CODE);                    break;
   
   case_op(BRANCH_CODE);                           break;
   case_op(BRANCH_TRUE_CODE);                      break;
   case_op(BRANCH_FALSE_CODE);                     break;
   case_op(BRANCH_INDEXED_CODE);                   break;
   
   case     NO_OPERAND_CODE:
    switch (bc.x) {
     default: interpret(illegal_no_operand_code);  break;
     case_op(SELF_CODE);                           break;
     case_op(POP_CODE);                            break;
     case_op(UNDIRECTED_RESEND_CODE);              break;
     case_op(NONLOCAL_RETURN_CODE);                break;
    }
    break;
  }
}


fint abstract_interpreter::get_argument_count() {
  # ifdef GENERATE_ASSERTIONS
  if ( CheckAssertions   &&   mi.instruction_set == TWENTIETH_CENTURY_PLUS_ARGUMENT_COUNT_INSTRUCTION_SET
  &&   is.argument_count != get_selector()->arg_count()) {
    error("argument_count does not match selector's argument count", is.argument_count, get_selector()->arg_count());
    // TODO AAA : fatal2("argument_count %d does not match selector's argument count %d", is.argument_count, get_selector()->arg_count());
  }
  # endif
  return mi.instruction_set == TWENTIETH_CENTURY_PLUS_ARGUMENT_COUNT_INSTRUCTION_SET ? is.argument_count : ByteVectorLayout().argCountOf(get_selector());
}


StringOop abstract_interpreter::get_selector() { 
  Oop s = get_literal();
  return check(check_selector_string, s)  
    ?  s  
    :  ByteVectorLayout().newString("Error: cannot find selector");
}



void abstract_interpreter::do_LITERAL_CODE() { 
 do_literal_code( get_literal()); 
}



bool abstract_interpreter_method_info::verify() {
/* TODO AAA
  if (_map_oop->verify_oop()) {
  } else {
    error1("bad oop in abstract_interpreter_method_info 0x%x", this);
    return false;
  }
  if ( codes           == (unsigned char*) map()->codes()->bytes()
  &&   length_codes    == map()->codes()->length()
  &&   literals        == map()->literals()->objs()
  &&   length_literals == map()->literals()->length() ) {
  } else {
    error1("inconsistency in abstract_interpreter_method_info 0x%x", this);
    return false;
  }
  if (! ObjVectorLayout().isObjVector(literals_object)) {
	error("literals not an objVector");
    // TODO AAA error2("literals_object 0x%x in "
    //       "abstract_interpreter_method_info 0x%x not objVector",
    //       literals_object,
    //       this);
    return false;
  }
  if (! ByteVectorLayout().isByteVector(codes_object)) {
	error("codes not a byteVector");
    // TODO AAA error2("codes_object 0x%x in "
    //       "abstract_interpreter_method_info 0x%x not byteVector",
    //       codes_object,
    //       this);
    return false;
  }
*/
  return true;
}
  

bool abstract_interpreter::verify() {
  return mi.verify();
}


void abstract_interpreter::check_branch_target(Oop p) {
  if (! SmiLayout().hasMyTag(p)) {
    set_error_msg( "branch target must be smallInt");
  }
  else if (   0 <= SmiLayout().valueOf(p)
           &&      SmiLayout().valueOf(p) <= mi.length_codes ) { // == length_codes means return
  }
  else {
    set_error_msg( "bad branch target");
  }
}


intNN abstract_interpreter::get_branch_pc() {
  Oop p = get_literal();
  return check(::check_branch_target, p)  ?  SmiLayout().valueOf(p)  :  0;
}


ObjVectorOop abstract_interpreter::get_branch_vector() {
  Oop p = get_literal();
  return check(check_branch_vector, p)
    ?  p  :  VECTOR_OOP;
}


void check_index_range(abstract_interpreter *ai, Oop) {
  if ( ai->is.index < ai->mi.length_literals ) return;
  ai->set_error_msg( "index out of bounds");
}

void check_selector_string(abstract_interpreter *ai, Oop s) {
  if ( ByteVectorLayout().isString(s) ) return;
  ai->set_error_msg( "selector must be a string");
}

void check_branch_target(abstract_interpreter *ai, Oop p) {
  ai->check_branch_target(p);
}

void check_no_send_modifiers(abstract_interpreter *ai, Oop) {
  ai->set_error_msg( ai->is.check_no_send_modifiers());
}

void check_no_lexical_level(abstract_interpreter *ai, Oop) {
  ai->set_error_msg( ai->is.check_no_lexical_level());
}

void check_no_two_send_modifiers(abstract_interpreter *ai, Oop) {
  ai->set_error_msg( ai->is.check_no_two_send_modifiers());
}

void check_no_operand(abstract_interpreter *ai, Oop) {
  ai->set_error_msg( ai->is.check_no_operand());
}

void check_delegatee(abstract_interpreter *ai, Oop) {
  Oop p = ai->get_literal();
  if ( !ai->error_msg  &&  ! ByteVectorLayout().isString(p))
    ai->set_error_msg( "delegatee must be string"); 
}

void check_no_argument_count(abstract_interpreter *ai, Oop) {
  if (ai->is.argument_count != 0)
    ai->set_error_msg( "should not have argument count before argument count setter");
}

void check_branch_vector(abstract_interpreter *ai, Oop) {
  Oop p= ai->get_literal();
  if (ai->error_msg)  return;
  if (! ObjVectorLayout().isObjVector(p)) {
    ai->set_error_msg( "branch vector must be object vector");
    return;
  }
  for (int i = 0, n = ObjVectorLayout().indexableSizeOf(p); i < n;  ++i) {
    Oop x = ObjVectorLayout().for_IndexableAt(p, i);
    check_branch_target(ai, x);
    if (ai->error_msg) return;
  }
}

void check_for_pop(abstract_interpreter *ai, Oop n) {
  ai->check_for_pop(n);
}

