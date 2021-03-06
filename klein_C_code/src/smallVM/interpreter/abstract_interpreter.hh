# ifndef KLEIN_ABSTRACT_INTERPRETER_H
# define KLEIN_ABSTRACT_INTERPRETER_H

# include "base.hh"
# include "byteCodes.hh"

class abstract_interpreter;
typedef void (*aiCheckFn)(abstract_interpreter*, Oop);

// I am an abstract super class for interpreting bytecodes

class abstract_interpreter_method_info {
  friend class frame; // for the ITERATORs (_map_oop) (ugh)
  friend class InterpreterIterator; // for the interators
  friend class FrameIterator;

 protected:
  MapOop     _map_oop;      // =badOop when checking codes and literals

 public:
  u_char*    codes;
  fint       length_codes;
  Oop*       literals;
  fint       length_literals;
  
  InstructionSetKind instruction_set;
  
  // next two are for real interpreter scavenging & gc
  
  ObjVectorOop  literals_object;
  ByteVectorOop    codes_object;
  
  abstract_interpreter_method_info(MethodMapOop mm);
    
  // hide whether oop or map is stored
  
  // TODO: AAA MapOop       map_oop() { return _map_oop; }
  // TODO: AAA MethodMapOop map()     { return (methodMapOop) _map_oop->map_addr(); }
  
  // skip first bc if it is Instruction set selection
  fint firstBCI() { return instruction_set == TWENTIETH_CENTURY_INSTRUCTION_SET  ? 0 : 1; }
      
  void print_short();

  bool verify();

 protected:
  void init(ByteVectorOop codes, ObjVectorOop literals);

};

# define ABSTRACT_INTERPRETER_METHOD_INFO_ITERATOR(mi, template, doReinit ) \
  { \
    Oop* p; \
    p = (Oop*)&(mi)->_map_oop;          template; \
    p = (Oop*)&(mi)->literals_object;   template; \
    p = (Oop*)&(mi)->codes_object;      template; \
    \
    if (doReinit) { \
      assert_objVector((mi)->literals_object, \
           "literals must be object vector"); \
      (mi)->literals= (mi)->literals_object->objs(); \
      assert_byteVector((mi)->codes_object, "codes must be byte vector"); \
      (mi)->codes= (unsigned char*) (mi)->codes_object->bytes(); \
    } \
  } 


class abstract_interpreter_bytecode_info {
 public:
  fint code; // the whole bytecode
  fint op;   // opcode field
  fint x;    // index field
  
  inline void decode(fint c);
  void print_short();
};


class abstract_interpreter_interbytecode_state {
 public:
  fint       lexical_level;  // set by lexical level code, used by local accessors
  fint       index;          // set by index code for extended indices
  Oop        last_literal;   // set by literal code, used for setting delegatee
  
  StringOop  delegatee;      // delegatee for next send, or NULL
  bool       is_undirected_resend;
  
  fint       argument_count; // count of upcoming send

 public:  
  inline abstract_interpreter_interbytecode_state();
  
  void reset_index()          { index= 0; }
  void reset_lexical_level()  { lexical_level= 0; }
  void reset_send_modifiers() {
    delegatee= NULL;  is_undirected_resend= false; argument_count= 0; }


  
  const char* check_no_index()         { 
    return  index == 0  ?  NULL  :  "this code does not use or preserve index";
  }
  const char* check_no_lexical_level() { 
    return  lexical_level == 0  
               ?  NULL  
               :  "this code does not use or preserve lexical_level";
  }
  const char* check_no_operand() {
    const char* c= check_no_index();
    return c ? c : check_no_lexical_level();
  }
  const char* check_no_send_modifiers() {
    return !is_undirected_resend  &&  delegatee == NULL
      ?  NULL
      :  "send modifiers should be set right before the send" ;
  }
  const char* check_no_argument_count() {
    return argument_count == 0
      ?  NULL
      :  "argument count should not be set twice in a row";
  }
  const char* check_no_two_send_modifiers() {
    return !is_undirected_resend  ||  delegatee == NULL
      ?  NULL
      :  "send cannot be both undirected and directed" ;
  }
  
  void print_short();
};


# define ABSTRACT_INTERPRETER_INTERBYTECODE_STATE_ITERATOR(is, \
                 template, \
                 reinit ) \
  { \
    Oop* p; \
    p =       &(is)->last_literal;  template; \
    p = (Oop*)&(is)->delegatee;     template; \
  } 


// I factor out bytecode decoding for all the places in the VM
// that do it.
// I only know about "syntax".

class abstract_interpreter {
  /* AAA
  friend class frame; // for the ITERATORs (ugh)
  friend class FrameIterator; // for the ITERATORs (ugh)
  friend class InterpreterIterator;
  friend Location location_of_interpreter(void*); // needs prot fns
  */

 public:
 
  fint  pc;
 
 protected:
 
  const char* error_msg;
  
  class abstract_interpreter_method_info          mi;
  class abstract_interpreter_bytecode_info        bc;
  class abstract_interpreter_interbytecode_state  is;
  
 public: 
  abstract_interpreter(Oop methodOop);
  
  
 protected:  // Checkers
 
  friend void check_index_range(           abstract_interpreter*, Oop);
  friend void check_selector_string(       abstract_interpreter*, Oop);
  friend void check_branch_target(         abstract_interpreter*, Oop);
  friend void check_no_send_modifiers(     abstract_interpreter*, Oop);
  friend void check_no_lexical_level(      abstract_interpreter*, Oop);
  friend void check_no_two_send_modifiers( abstract_interpreter*, Oop);
  friend void check_no_argument_count(     abstract_interpreter*, Oop);
  friend void check_no_operand(            abstract_interpreter*, Oop);
  friend void check_delegatee(             abstract_interpreter*, Oop);
  friend void check_branch_vector(         abstract_interpreter*, Oop);
  friend void check_for_pop(               abstract_interpreter*, Oop);
  
  void set_error_msg(const char* s) {
    if (!error_msg) error_msg= s; } // want first one
    
 public:
  const char* get_error_msg() { return error_msg; }
  
 protected:
  // inflection points for bytecode checker
  
  virtual void check_branch_target(Oop p);
    
  virtual bool check(aiCheckFn fn, Oop p = NULL) {
    always_assert(
      ( (*fn)(this, p), !error_msg), 
      error_msg);
    return true;
  }

  virtual bool check_for_pop(Oop) {
    error("subclass responsibility");
    return false;
  }
  
 protected:


  Oop get_literal() {
    return check(check_index_range)  ?  mi.literals[is.index]  :  NULL;
  }

  StringOop get_selector();
  fint      get_argument_count(); 
  
  intNN get_branch_pc();
  ObjVectorOop get_branch_vector();

 public:
  virtual void interpret_method();
  virtual void interpret_bytecode() {
    fetch_and_decode_bytecode();
    dispatch_bytecode();
  }
  virtual void fetch_and_decode_bytecode();
  void dispatch_bytecode();
  
 protected:
 
  // NOTE: The pre_*_CODE, do_*_CODE, and post_*_CODE
  //  routines are called via macro from dispatch_bytecode()
  
  // The following pre_ routines check "syntax" here, 
  // i.e. checks that do not use method.
  
  void pre_INDEX_CODE()              { }
  
  void pre_SEND_CODE()               { check(check_no_send_modifiers);
                                       check(check_no_lexical_level);  }
  void pre_IMPLICIT_SEND_CODE()      { check(check_no_lexical_level);
                                       check(check_no_two_send_modifiers);  }
   
  void pre_LEXICAL_LEVEL_CODE()      { check(check_no_send_modifiers); }
  void pre_READ_LOCAL_CODE()         { check(check_no_send_modifiers); }
  void pre_WRITE_LOCAL_CODE()        { check(check_no_send_modifiers); }
  void pre_LITERAL_CODE()            { check(check_no_send_modifiers); }
  void pre_DELEGATEE_CODE()          { check(check_no_send_modifiers); 
                                       check(check_delegatee); }
  void pre_ARGUMENT_COUNT_CODE()     { check(check_no_argument_count); }
  
  void pre_SELF_CODE()              { check(check_no_operand); 
                                      check(check_no_send_modifiers); }
  void pre_POP_CODE()               { check(check_no_operand); 
                                      check(check_no_send_modifiers); }
  void pre_NONLOCAL_RETURN_CODE()   { check(check_no_operand); 
                                      check(check_no_send_modifiers); }
  void pre_UNDIRECTED_RESEND_CODE() { check(check_no_operand); 
                                      check(check_no_send_modifiers); }
  
  void pre_branch_code()            { check(check_no_send_modifiers);
                                      check(check_no_lexical_level); }

  
  void pre_BRANCH_CODE()            { pre_branch_code(); }
  void pre_BRANCH_TRUE_CODE()       { pre_branch_code(); }
  void pre_BRANCH_FALSE_CODE()      { pre_branch_code(); }
  void pre_BRANCH_INDEXED_CODE()    { pre_branch_code(); }
  
  
  void pre_illegal_no_operand_code() { }
  void pre_illegal_code()            { }
 
 protected: 
  // NOTE: these routines are called via macro from dispatch_bytecode()
  // see comment above the pre_'s

  // override the following virtuals to do something real
 
  virtual void do_SELF_CODE()              { }
  virtual void do_POP_CODE()               { }
  virtual void do_NONLOCAL_RETURN_CODE()   { }
  virtual void do_INDEX_CODE()             { }
  
  virtual void do_literal_code(Oop lit) {  Unused(lit); }

  virtual void do_LITERAL_CODE(); 
  
  // override either first one or 2 and 3
  
  virtual void do_read_write_local_code(bool isWrite) { Unused(isWrite); }
  virtual void do_READ_LOCAL_CODE()        { do_read_write_local_code(false); }
  virtual void do_WRITE_LOCAL_CODE()       { do_read_write_local_code(true);  }
  
  // override either first one or 2 and 3
  
  virtual void do_send_code(bool /*isSelfImplicit*/, StringOop /*selector*/, fint /*arg_count*/) {
  }

  virtual void do_SEND_CODE()              {
    do_send_code(false, get_selector(), get_argument_count()); }
  virtual void do_IMPLICIT_SEND_CODE()     {
    do_send_code(true,  get_selector(), get_argument_count()); }
  
  virtual void do_UNDIRECTED_RESEND_CODE() { }
  virtual void do_DELEGATEE_CODE()         { }
  virtual void do_ARGUMENT_COUNT_CODE()    { }
  virtual void do_LEXICAL_LEVEL_CODE()     { }
  
  virtual void do_branch_code( intNN targetPC, Oop targetOop ) { Unused(targetPC);  Unused(targetOop); }

  virtual void do_BRANCH_CODE()          { do_branch_code( get_branch_pc(), INVALID_OOP ); }
  virtual void do_BRANCH_TRUE_CODE()     { do_branch_code( get_branch_pc(), TRUE_OOP    ); }
  virtual void do_BRANCH_FALSE_CODE()    { do_branch_code( get_branch_pc(), FALSE_OOP   ); }
  virtual void do_BRANCH_INDEXED_CODE()  { get_branch_vector(); }

  virtual void do_illegal_no_operand_code() { do_illegal_code(); }
  virtual void do_illegal_code()            {
	error("bad op code");
    //TODO: This is how it looks in the Self VM: fatal3("bad op code: %ld (opcode: $ld, index: %ld)",   bc.code, bc.op, bc.x); 
  }
  
  
 protected: 
  // NOTE: these routines are called via macro from dispatch_bytecode()
  // see comment above the pre_'s

  // fixup after bytcode

  void post_SELF_CODE()              { }
  void post_POP_CODE()               { }
  void post_NONLOCAL_RETURN_CODE()   { }
  void post_INDEX_CODE()             { }
  
  void post_LITERAL_CODE()           {  is.reset_index(); }
  
  void post_READ_LOCAL_CODE()        {
    is.reset_index();   is.reset_lexical_level();  }
  void post_WRITE_LOCAL_CODE()       {
    is.reset_index();   is.reset_lexical_level();  }
  
  void post_SEND_CODE()              {
    is.reset_index();  is.reset_send_modifiers(); }
  void post_IMPLICIT_SEND_CODE()     {
    is.reset_index();  is.reset_send_modifiers(); }
  
  void post_UNDIRECTED_RESEND_CODE() {  is.is_undirected_resend= true; }
  void post_DELEGATEE_CODE()         {
    is.delegatee= get_literal(); 
    is.reset_index();
  }
  void post_ARGUMENT_COUNT_CODE()    { is.argument_count= is.index;   is.reset_index(); }
  void post_LEXICAL_LEVEL_CODE()     { is.lexical_level=  is.index;   is.reset_index(); }
  
  void post_BRANCH_CODE()            { is.reset_index(); }
  void post_BRANCH_TRUE_CODE()       { is.reset_index(); }
  void post_BRANCH_FALSE_CODE()      { is.reset_index(); }
  void post_BRANCH_INDEXED_CODE()    { is.reset_index(); }
  
  
  void post_illegal_no_operand_code() { }
  void post_illegal_code()            { }
  
 public:
 
  void print_short();
  virtual bool verify();
};


// for scavenging, etc, see interpreter.hh

# define ABSTRACT_INTERPRETER_ITERATOR(intrp, template, zap, reinit) \
  { \
    ABSTRACT_INTERPRETER_METHOD_INFO_ITERATOR(        &(intrp)->mi, template, reinit) \
    ABSTRACT_INTERPRETER_INTERBYTECODE_STATE_ITERATOR(&(intrp)->is, template, reinit) \
  } 


# endif // KLEIN_ABSTRACT_INTERPRETER_H
