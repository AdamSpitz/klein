# include "prims.hh"
# include "errorCodes.hh"
# include "lookup.hh"
# include "stringObj.hh"
# include "activation.hh"
# include "utils.hh"
# include "memory.hh"
# include <alloca.h>
# include <unistd.h>

const char* Primitives::          failure_suffix = "IfFail:";
const int   Primitives::length_of_failure_suffix = 7;

# define smi_check_unary() {      \
  if (!is_smi(rcvr))             \
    return primitive_fail_string(BADTYPEERROR);}
    
# define smi_check_binary() {      \
  smi_check_unary();               \
  if (!is_smi(argsp[0])) {         \
    return primitive_fail_string(BADTYPEERROR);}}
    
oop_t primitive_fail_string (error_t errorID) {
  oop_t error_string = ErrorCodes::stringForError(errorID);
  return change_tag(error_string, mark_tag);
}

oop_t primitive_fail ( oop_t selfStringForPrimitive, oop_t rcvr_for_fail, oop_t error_result, oop_t failBlock, oop_t current_activation, oop_t* activation){
  assert(is_mark(error_result));
  oop_t error_string = change_tag(error_result, mem_tag);
  oop_t argsp[2] = {error_string, selfStringForPrimitive};  
  
  oop_t selfStringForMessage;
  
  if (failBlock == badOop) {
    selfStringForMessage = StringObj::intern("primitiveFailedError:Name:");
    *activation = ActivationObj::clone_for_failure(rcvr_for_fail, selfStringForMessage, (oop_t*) argsp, 2, current_activation);
  }
  else {
    rcvr_for_fail = failBlock;
    selfStringForMessage = StringObj::intern("value:With:");
    *activation = ActivationObj::clone_for_failure(rcvr_for_fail, selfStringForMessage, (oop_t*) argsp, 2, current_activation);
  }  
  
  return error_result;
}

oop_t string_print_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {  
  if (!is_byteVector(rcvr))
    return primitive_fail_string(BADTYPEERROR);
  
  ByteVectorObj::from(rcvr)->string_print();
  return rcvr;
}

oop_t debug_print_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {  
  MemObj::debug_print(rcvr);
  return rcvr;
}

oop_t print_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {  
  MemObj::print(rcvr);
  return rcvr;
}

oop_t string_canonicalize_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  if (!is_byteVector(rcvr))
    return primitive_fail_string(BADTYPEERROR);
  
  ByteVectorObj* bv = ByteVectorObj::from(rcvr);
  return StringObj::intern(bv->bytes(), bv->indexableSize());
}


oop_t smi_complement_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  smi_check_unary();
  return smiOop_for_value ( -1 - value_of_smiOop(rcvr)); 
}

oop_t smi_and_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  smi_check_binary(); 
  return smiOop_for_value( value_of_smiOop(rcvr) & value_of_smiOop(argsp[0]) );   
}

oop_t smi_or_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  smi_check_binary();
  return smiOop_for_value( value_of_smiOop(rcvr) | value_of_smiOop(argsp[0]) );   
}

oop_t smi_xor_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  smi_check_binary();
  return smiOop_for_value( value_of_smiOop(rcvr) ^ value_of_smiOop(argsp[0]) );   
}

oop_t smi_arleftshift_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  smi_check_binary();
  smi r = value_of_smiOop(rcvr);
  smi a = value_of_smiOop(argsp[0]);
  smi result = r << a;
  if (result > MAXIMUM_SMI_VALUE || result < MINIMUM_SMI_VALUE)
    return primitive_fail_string(OVERFLOWERROR); //overflow
  return smiOop_for_value( result );   
}

oop_t smi_loleftshift_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  return smi_arleftshift_prim( rcvr, argsp, current_activation, new_actp); //todo primitives test Is this correct? ~ Ausch   
}

smi abs_value (smi arg) {
  return arg < 0 ? -arg : arg;
}

smi arrightshift(smi r, smi a) {
  smi result = r >> a; // aaa According to the C spec, the implementation of >> is platform specific !! 
                       // (for -'ve numbers, at least) ~ Ausch

  // keep the sign
  if (result > 0 && r < 0)
    result = -1 * result;

  return result;
}

oop_t smi_arrightshift_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  smi_check_binary();
  smi r = value_of_smiOop(rcvr), a = value_of_smiOop(argsp[0]);
  return smiOop_for_value( arrightshift(r, a) );
}

oop_t smi_lorightshift_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  //logical right shift does not keep the sign
  smi_check_binary();
  smi a = value_of_smiOop(argsp[0]);
  oop_t shifted = ((u_int32)rcvr) >> a;
  return change_tag( shifted, smi_tag );
  //todo primitives add assertion to verify platform-dependant shifting ~ Ausch
}

oop_t smi_add_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  smi_check_binary();
  smi r = value_of_smiOop(rcvr), a = value_of_smiOop(argsp[0]);
  smi sum = r + a;
  // todo optimize time: There's gotta be a better way to check for overflow
  //                     (in this and all the other arithmetic prims). -- Adam, 5/06
  if (sum > MAXIMUM_SMI_VALUE || sum < MINIMUM_SMI_VALUE)
    return primitive_fail_string(OVERFLOWERROR);
  return smiOop_for_value(sum);
}

oop_t smi_sub_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  smi_check_binary();
  smi r = value_of_smiOop(rcvr), a = value_of_smiOop(argsp[0]);
  smi difference = r - a;
  if (difference > MAXIMUM_SMI_VALUE || difference < MINIMUM_SMI_VALUE)
    return primitive_fail_string(OVERFLOWERROR);
  return smiOop_for_value(difference);    
}

oop_t smi_mul_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  smi_check_binary();
  smi a = value_of_smiOop(argsp[0]); if (a == 0) return smiOop_for_value(0);
  smi r = value_of_smiOop(rcvr    );
  if (a > 0) {
    if (r > MAXIMUM_SMI_VALUE / a || r < MINIMUM_SMI_VALUE / a)
      return primitive_fail_string(OVERFLOWERROR);
  } else {
    if (r < MAXIMUM_SMI_VALUE / a || r > MINIMUM_SMI_VALUE / a)
      return primitive_fail_string(OVERFLOWERROR);
  }
  return smiOop_for_value(r * a);
}

oop_t smi_div_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  smi_check_binary();
  smi a = value_of_smiOop(argsp[0]);
  if (a == 0)
    return primitive_fail_string(DIVISIONBYZEROERROR);
  smi r = value_of_smiOop(rcvr);
  if (r == MINIMUM_SMI_VALUE && a == -1)
    return primitive_fail_string(OVERFLOWERROR);
  int32 quo = (r / a);
  //assert( abs(quo) == abs(r) / abs(a), "smi_div_prim is wrong on this platform");
  return smiOop_for_value(quo);
}

oop_t smi_mod_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  smi_check_binary();
  smi a = value_of_smiOop(argsp[0]);
  if (a == 0)
    return primitive_fail_string(DIVISIONBYZEROERROR);
  smi r = value_of_smiOop(rcvr);
  if (r == MINIMUM_SMI_VALUE && a == -1)
    return primitive_fail_string(OVERFLOWERROR);
  smi mod = (r % a);
  return smiOop_for_value(mod);
}



# define condition_test_init()        \
  smi_check_binary();                 \
  smi r = value_of_smiOop (rcvr    ); \
  smi a = value_of_smiOop (argsp[0]);

oop_t smi_eq_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  condition_test_init();
  return The::oop_of ( (r == a) ? The::true_object : The::false_object);
}


oop_t smi_ne_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  condition_test_init();
  return The::oop_of ( (r != a ) ? The::true_object : The::false_object);
}

oop_t smi_lt_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  condition_test_init();
  return The::oop_of ( ( r < a ) ? The::true_object : The::false_object);
}

oop_t smi_le_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  condition_test_init();
  return The::oop_of ( ( r <= a ) ? The::true_object : The::false_object);
}

oop_t smi_gt_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  condition_test_init();
  return The::oop_of ( ( r > a ) ? The::true_object : The::false_object);
}

oop_t smi_ge_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  condition_test_init();
  return The::oop_of ( ( r >= a ) ? The::true_object : The::false_object);
}

# undef condition_test_init()


# define int32_check(x, var)                            \
  if (is_smi(x)) {                                      \
    var = value_of_smiOop(x);                           \
  } else {                                              \
    if (! is_mem(x))                                    \
      return primitive_fail_string(BADTYPEERROR);       \
    MemObj* m = MemObj::from(x);                        \
    if (! m->is_byteVector())                           \
      return primitive_fail_string(BADTYPEERROR);       \
    var = *((int32*) ((ByteVectorObj*)m)->bytes());     \
  }

# define int32_binary_check()                           \
  int32 r, a;                                           \
  int32_check(rcvr, r);                                 \
  oop_t a_oop = argsp[0];                               \
  int32_check(a_oop, a);                                

# define return_int32(v)                                \
  if (MINIMUM_SMI_VALUE <= v && v <= MAXIMUM_SMI_VALUE) \
    return smiOop_for_value(v);                         \
  else                                                  \
    return                                              \
      ((ByteVectorObj*) The::addr_of(The::int32_proto)) \
          ->clone_for_int32(v);

# define int32_binary_prim(func, valueExpr)   \
  oop_t func (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {   \
    int32_binary_check();                                                              \
    int32 result = (valueExpr);                                                        \
    return_int32( result );                                                            \
  }

int32_binary_prim( int32_add_prim,  r + a );
int32_binary_prim( int32_sub_prim,  r - a );
int32_binary_prim( int32_mul_prim,  r * a );
int32_binary_prim( int32_cmp_prim,  r == a ? 0 : (r < a ? -1 : 1) );
int32_binary_prim( int32_and_prim,  r & a );
int32_binary_prim( int32_or_prim,   r | a );
int32_binary_prim( int32_xor_prim,  r ^ a );
int32_binary_prim( int32_shl_prim,  r << a );
int32_binary_prim( int32_shr_prim,  arrightshift(r, a) );
int32_binary_prim( int32_ushr_prim, r >> a );

oop_t int32_div_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  int32_binary_check();
  if (a == 0)
    return primitive_fail_string(DIVISIONBYZEROERROR);
  int32 result = r / a;
  return_int32( result );
}

oop_t int32_rem_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  int32_binary_check();
  if (a == 0)
    return primitive_fail_string(DIVISIONBYZEROERROR);
  int32 result = r % a;
  return_int32( result );
}


oop_t eq_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  return The::oop_of(rcvr == argsp[0] ? The::true_object : The::false_object);
}

oop_t at_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  if ( ! (     is_objVector(     rcvr )  
           &&  is_smi      ( argsp[0] ) ))
    return primitive_fail_string(BADTYPEERROR);
  
  ObjVectorObj* r = ObjVectorObj::from(rcvr);
  smi a = value_of_smiOop (argsp[0]);
  
  if ( a < 0 || a >= r->indexableSize())
    return primitive_fail_string(BADINDEXERROR);
    
  return r->indexable_at(a);
}

oop_t at_put_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  if ( ! (     is_objVector(     rcvr )  
           &&  is_smi      ( argsp[0] ) ))
    return primitive_fail_string(BADTYPEERROR);
  
  ObjVectorObj* r = ObjVectorObj::from(rcvr);
  smi a = value_of_smiOop (argsp[0]);
  
  if ( a < 0 || a >= r->indexableSize())
    return primitive_fail_string(BADINDEXERROR);

  r->write_indexable_at(a, argsp[1]);
  return rcvr;
}

oop_t size_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  if (!is_objVector(rcvr))
    return primitive_fail_string(BADTYPEERROR);
  return smiOop_for_value(ObjVectorObj::from(rcvr)->indexableSize());
}

oop_t byteat_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  if ( ! (     is_byteVector(     rcvr )  
           &&  is_smi       ( argsp[0] ) ))
    return primitive_fail_string(BADTYPEERROR);

  ByteVectorObj* r = ByteVectorObj::from(rcvr);
  smi a = value_of_smiOop (argsp[0]);
  
  if ( a < 0 || a >= r->indexableSize())
    return primitive_fail_string(BADINDEXERROR);
  return r -> byteAt(a);
}

oop_t byteat_put_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  if ( ! (     is_byteVector(     rcvr )  
           &&  is_smi       ( argsp[0] ) 
           &&  is_smi       ( argsp[1] ) ))
    return primitive_fail_string(BADTYPEERROR);
  

  ByteVectorObj* r = ByteVectorObj::from(rcvr);
  smi a = value_of_smiOop (argsp[0]);
  
  if ( a < 0 || a >= r->indexableSize() )
    return primitive_fail_string(BADINDEXERROR);
  
  r -> byteAt_Put( a, argsp[1] );
  return rcvr;
}

oop_t bytesize_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  if ( !is_byteVector( rcvr ))
    return primitive_fail_string(BADTYPEERROR);
    
  return smiOop_for_value(ByteVectorObj::from(rcvr)->indexableSize());
}

oop_t byteVectorConcatenate_Prototype_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  if ( ! (     is_byteVector(     rcvr )  
           &&  is_byteVector( argsp[0] ) 
           &&  is_byteVector( argsp[1] ) ))
    return primitive_fail_string(BADTYPEERROR);
 // if ( is_canonical_string(argsp[1])) //todo: implement is_canonical_string(oop_t oop)
 //   return primitive_fail_string(BADTYPEERROR); //can't use canonical strings as prototypes for concatenation
  
  ByteVectorObj *result_bv, *rcvr_bv, *arg_bv;
  rcvr_bv  = ByteVectorObj::from(     rcvr);
  arg_bv   = ByteVectorObj::from( argsp[0]);
  smi rcvr_size = rcvr_bv->indexableSize();
  smi  arg_size =  arg_bv->indexableSize();

  oop_t result_oop = ByteVectorObj::from( argsp[1]) -> clone_and_resize( rcvr_size + arg_size, 0, &result_bv); 
  
  if (result_oop == badOop)
    unimplemented ("failure: out of memory?"); //todo gc OOM checks are GC dependant?
  
  result_bv->range_set(  0       , rcvr_size, rcvr_bv->bytes());
  result_bv->range_set( rcvr_size,  arg_size,  arg_bv->bytes());
  
  return result_oop;
}


// aaa Does _CloneBytes:Filler: only fail when out of memory or when filler is out of bounds?
oop_t bv_clone_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  if ( ! (     is_byteVector(rcvr)  
           &&  is_smi       (argsp[0]) 
           &&  is_smi       (argsp[1]) ))
    return primitive_fail_string(BADTYPEERROR);
  
  smi size = value_of_smiOop(argsp[0]);
  smi fill = value_of_smiOop(argsp[1]);
  if (! (0 <= fill  &&  fill <= 255))
    return primitive_fail_string(BADTYPEERROR);
  
  oop_t r = ByteVectorObj::from(rcvr)->clone_and_resize( size, fill );
  if (r == badOop)
    unimplemented ("failure: out of memory?"); //todo gc OOM checks are GC dependant?
  
  return r;
}

oop_t ov_clone_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  if ( ! (     is_objVector(rcvr)  
           &&  is_smi      (argsp[0]) ))
    return primitive_fail_string(BADTYPEERROR);
  
  smi   size = value_of_smiOop(argsp[0]);
  oop_t fill = argsp[1];
  
  oop_t r = ObjVectorObj::from(rcvr)->clone_and_resize( size, fill );
  if (r == badOop)
    unimplemented ("failure: out of memory?"); //todo gc OOM checks are GC dependant?
  
  return r;
}

// aaa Does _Clone only fail when out of memory?
oop_t clone_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  oop_t r;
  if (is_mark(rcvr))
    return primitive_fail_string(BADTYPEERROR);
    
  else if (is_smi(rcvr) || is_float(rcvr)) // todo primitives cleanup Also check if it is a canonical string
    return rcvr;
  else if (is_byteVector(rcvr))
    r = bv_clone_prim (rcvr, argsp, current_activation, new_actp);
  else if  (is_objVector(rcvr))
    r = ObjVectorObj::from(rcvr) -> clone();
//  else if  (is_block(rcvr))
//    r = clone_block(rcvr, activation);
  else
    r = MemObj::from(rcvr) -> clone();
  
  if (r == badOop) 
    unimplemented ("failure: out of memory?"); //todo gc OOM checks are GC dependant?
  
  return r;
}

oop_t identity_hash_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  if (is_smi(rcvr))
    return rcvr;
  if (is_float(rcvr))
    return primitive_fail_string(PRIMITIVENOTDEFINEDERROR); // unimplemented floats
  assert(is_mem(rcvr));
  return smiOop_for_value(MemObj::from(rcvr)->oid());
}

oop_t asObject_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
 if (!is_smi(rcvr))
   return primitive_fail_string(BADTYPEERROR);
 //todo primitives cleanup failure Handle the case where an oid is not in the table ~Ausch
 return Object_Table::oop_for_int(value_of_smiOop(rcvr));
}

oop_t objectId_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
 if (!is_mem(rcvr))
   return primitive_fail_string(BADTYPEERROR);
 //todo primitives cleanup failure Handle the case where an object is not in the table ~Ausch
 return smiOop_for_value(Object_Table::index_for_oop(rcvr));
}


oop_t tagPartOfObjectReference_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
 return smiOop_for_value(tag(rcvr));
}


oop_t map_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
 return mapOop(rcvr);
}


oop_t wizard_mode_prim(oop_t rcvr, oop_t* , oop_t , oop_t* ) {
  return The::addr_of(The::vm)->contents_of_slot(StringObj::intern("wizardMode"));
}


oop_t set_wizard_mode_prim(oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  oop_t new_value = argsp[0];
  if (! is_boolean(new_value))
    return primitive_fail_string(BADTYPEERROR);
  oop_t wizard_mode_str = StringObj::intern("wizardMode");
  oop_t old_value = The::addr_of(The::vm)->contents_of_slot(wizard_mode_str);
  The::addr_of(The::vm)->set_contents_of_slot(wizard_mode_str, new_value);
  return old_value;
}


oop_t scavenge_prim(oop_t rcvr, oop_t* , oop_t , oop_t* ) {
  Memory::scavenge();
  return rcvr;
}


oop_t breakpoint_prim(oop_t rcvr, oop_t* , oop_t , oop_t* ) {
  printf_and_flush("breakpoint: ");
  if (is_byteVector(rcvr)) ByteVectorObj::from(rcvr)->string_print();
  else                     printf_and_flush("rcvr not a string");
  printf_and_flush("\n");
  
  if (false) { // for debugging
    printf_and_flush("ac oop %d, ac addr 0x%x, map oop %d, map addr 0x%x, ts %d\n", 
                      The::oop_of(The::active_context),
                      The::addr_of(The::active_context),
                                   The::addr_of(The::active_context)->map_oop(),
                      MemObj::from(The::addr_of(The::active_context)->map_oop()),
                      *(int*)Object_Table::get_timestamp_address()
                      );
  }
  kill(getpid(), SIGTRAP);
  return rcvr;
}


oop_t perform_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  oop_t sel = argsp[0];
  oop_t* start_of_args = argsp + 1;
  fint arg_count = 0;
  bool isImplicitSelf = false; // todo adam broken: how can I find out whether it isImplicitSelf or not?
  bool isUndirected = false;
  oop_t delegatee = NULL;
  
  ActivationObj::from(current_activation)
     -> non_primitive_send(rcvr,
                           start_of_args,
                           arg_count,
                           isImplicitSelf,
                           sel,
                           isUndirected,
                           delegatee,
                           current_activation,
                           new_actp);

  return badOop;
}


// todo garbageCollection verify primitive
oop_t verify_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  printf_and_flush("Warning!! call to unimplemented primitive stub: _Verify.");
  return rcvr;
}


oop_t theVM_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  return The::oop_of(The::vm);
}

oop_t set_theVM_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  // Here for compatibility with Klein. Hopefully we'll never try to set it to a
  // different VM than the one that's already running. (I suppose we could, but we'd
  // have to fix up a lot of stuff in The and so on, and it'd be a big pain.) -- Adam, 5/06
  assert(argsp[0] == The::oop_of(The::vm));
  return rcvr;
}

oop_t thisProcess_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  // todo processes: Once we have multiple processes, this shouldn't return the prototypical process. :) -- Adam, 5/06
  return The::oop_of(The::process_proto);
}

oop_t noMapTest_prim (oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp) {
  // Here for compatibility with Klein; doesn't need to do anything. -- Adam, 5/06
  return rcvr;
}


oop_t unimp_prim (oop_t rcvr, oop_t* argsp, smi arg_count, oop_t current_activation, oop_t* new_actp) {
  return primitive_fail_string(PRIMITIVENOTDEFINEDERROR);
}
# undef smi_check_unary
# undef smi_check_binary


PrimitiveTableEntry Primitives::entries[] = {
//Format: 
//{ "_PrimName",      function_name, nargsCache, selfNameCache} -- last two are caches.

  { "_Verify", verify_prim, 0, 0},

  { "_TheVM", theVM_prim, 0, 0},
  { "_TheVM:", set_theVM_prim, 0, 0},
  { "_ThisProcess", thisProcess_prim, 0, 0},

  { "_NoMapTest", noMapTest_prim, 0, 0},

  { "_StringPrint", string_print_prim, 0, 0 },
  { "_DebugPrint",   debug_print_prim, 0, 0 },
  { "_Print",              print_prim, 0, 0 },
  { "_StringCanonicalize", string_canonicalize_prim, -1, 0 },
//{ "_Quit", unimp_prim, -1, 0 },
  
// Cloning

  { "_Clone",               clone_prim, 0, 0 },
  { "_Clone:Filler:",       ov_clone_prim, 0, 0 },
  { "_CloneBytes:Filler:",  bv_clone_prim, 0, 0 },
//{ "_CloneVector",         unimp_prim, 0, 0 },
//{ "_Define:",             unimp_prim, 0, 0 },

//  { "_ErrorMessage",        unimp_prim, 0, 0 },  //returns the description string for an error message coming out of a primitive
  
//  { "_Restart",             restart_prim, 0, 0 },  // no need - implemented in the interpreter loop

  { "_IdentityHash",        identity_hash_prim, 0, 0 }, 

  { "_AsObject",            asObject_prim, 0, 0 }, //returns an object for an OID
  { "_ObjectId",            objectId_prim, 0, 0 }, //returns an OID for an object

//  { "_MethodPointer",       unimp_prim, 0, 0 }, 
  
  { "_TagPartOfObjectReference",  tagPartOfObjectReference_prim, 0, 0 },
  { "_Map",  map_prim, 0, 0 },
  
  { "_Eq:",                   eq_prim, 0, 0 },
  { "_At:",                   at_prim, 0, 0 },
  { "_At:Put:",           at_put_prim, 0, 0 },
  { "_Size",                size_prim, 0, 0 },
  { "_ByteAt:",           byteat_prim, 0, 0 },
  { "_ByteAt:Put:",   byteat_put_prim, 0, 0 },
  { "_ByteSize",        bytesize_prim, 0, 0 },
  
//  { "_ByteVectorCompare:",          unimp_prim, 0, 0 },
  { "_ByteVectorConcatenate:Prototype:",          byteVectorConcatenate_Prototype_prim, 0, 0 },

  
  
//Integer Arithmetic
//  { "_FloatAsInt",        unimp_prim, 0, 0 }, 
//  { "_IntAsFloat",        unimp_prim, 0, 0 }, 

  { "_IntAdd:",          smi_add_prim, 0, 0 },
  { "_IntSub:",          smi_sub_prim, 0, 0 },
  { "_IntMul:",          smi_mul_prim, 0, 0 },
  { "_IntDiv:",          smi_div_prim, 0, 0 },
  { "_IntMod:",          smi_mod_prim, 0, 0 },

  { "_IntComplement",              smi_complement_prim, 0, 0 },  
  { "_IntAnd:",                           smi_and_prim, 0, 0 },
  { "_IntOr:",                             smi_or_prim, 0, 0 },
  { "_IntXor:",                           smi_xor_prim, 0, 0 },
  { "_IntArithmeticShiftLeft:",  smi_arleftshift_prim , 0, 0 },
  { "_IntLogicalShiftLeft:",     smi_loleftshift_prim , 0, 0 },
  { "_IntArithmeticShiftRight:", smi_arrightshift_prim , 0, 0 },
  { "_IntLogicalShiftRight:",    smi_lorightshift_prim , 0, 0 },
  
  { "_IntEQ:",          smi_eq_prim, 0, 0 },
  { "_IntNE:",          smi_ne_prim, 0, 0 },
  { "_IntLT:",          smi_lt_prim, 0, 0 },
  { "_IntLE:",          smi_le_prim, 0, 0 },
  { "_IntGE:",          smi_ge_prim, 0, 0 },
  { "_IntGT:",          smi_gt_prim, 0, 0 },

  { "_Int32:Add:",          int32_add_prim,  0, 0 },
  { "_Int32:Sub:",          int32_sub_prim,  0, 0 },
  { "_Int32:Mul:",          int32_mul_prim,  0, 0 },
  { "_Int32:Div:",          int32_div_prim,  0, 0 },
  { "_Int32:Rem:",          int32_rem_prim,  0, 0 },
  { "_Int32:Cmp:",          int32_cmp_prim,  0, 0 },
  { "_Int32:And:",          int32_and_prim,  0, 0 },
  { "_Int32:Or:" ,          int32_or_prim,   0, 0 },
  { "_Int32:Xor:",          int32_xor_prim,  0, 0 },
  { "_Int32:Shl:",          int32_shl_prim,  0, 0 },
  { "_Int32:Shr:",          int32_shr_prim,  0, 0 },
  { "_Int32:Ushr:",         int32_ushr_prim, 0, 0 },



// Mirrors
/*  { "_Mirror",                                             mirror_prim, 0, 0 }, 
  { "_MirrorAnnotation",                                   unimp_prim, 0, 0 }, 
  { "_MirrorAnnotationAt:",                                unimp_prim, 0, 0 }, 
  { "_MirrorByteCodePosition",                             unimp_prim, 0, 0 },
  { "_MirrorCodes",                                        unimp_prim, 0, 0 },
  { "_MirrorContentsAt:",                                  unimp_prim, 0, 0 },
  { "_MirrorCopyAt:Put:IsParent:IsArgument:Annotation:",   unimp_prim, 0, 0 },
  { "_MirrorCopyAnnotation:",            unimp_prim, 0, 0 },
  { "_MirrorCopyRemoveSlot:",            unimp_prim, 0, 0 },
  { "_MirrorCreateBlock",            unimp_prim, 0, 0 },
  { "_MirrorDefine:",            unimp_prim, 0, 0 },
  { "_MirrorEvaluate:",            unimp_prim, 0, 0 },
  { "_MirrorIsArgumentAt:",            unimp_prim, 0, 0 },
  { "_MirrorIsAssignableAt:",            unimp_prim, 0, 0 },
  { "_MirrorIsParentAt:",            unimp_prim, 0, 0 },
  { "_MirrorLiterals",            unimp_prim, 0, 0 },
  { "_MirrorMethodHolder",            unimp_prim, 0, 0 },
  { "_MirrorNames",            unimp_prim, 0, 0 },
  { "_MirrorNameAt:",            unimp_prim, 0, 0 },
  { "_MirrorReceiver",            unimp_prim, 0, 0 },
  { "_MirrorReflectee",            unimp_prim, 0, 0 },
  { "_MirrorReflecteeEq:",            unimp_prim, 0, 0 },
  { "_MirrorReflecteeIdentityHash",            unimp_prim, 0, 0 },
  { "_MirrorSelector",            unimp_prim, 0, 0 },
  { "_MirrorSender",            unimp_prim, 0, 0 },
  { "_MirrorSize",            unimp_prim, 0, 0 },
  { "_MirrorSource",            unimp_prim, 0, 0 },
  { "_MirrorSourceOffset",            unimp_prim, 0, 0 },
  { "_MirrorSourceLength",            unimp_prim, 0, 0 },*/
  
  
// Others

/* 
  { "_GarbageCollect",    unimp_prim, 0, 0 }, 
  { "_CurrentTimeString",   unimp_prim, 0, 0 },
  { "_DateTime:",           unimp_prim, 0, 0 },

  { "_AbortProcess",          unimp_prim, 0, 0 },
  { "_ActivationAt:",          unimp_prim, 0, 0 },
  { "_ActivationStack",          unimp_prim, 0, 0 },
  { "_AddSlots:",          unimp_prim, 0, 0 },
  { "_AddSlotsIfAbsent:",          unimp_prim, 0, 0 },
  { "_AddressAsObject",          unimp_prim, 0, 0 },
  { "_AnnotateSpyLog",          unimp_prim, 0, 0 },
  { "_BitSize",          unimp_prim, 0, 0 },
  { "_BlockSignals",          unimp_prim, 0, 0 },
  */
  
  { "_Breakpoint",         breakpoint_prim, 0, 0 },
  
  /*
  { "_CopyByteRangeDstPos:Src:SrcPos:Length:",          unimp_prim, 0, 0 },
  { "_CopyRangeDstPos:Src:SrcPos:Length:",          unimp_prim, 0, 0 },
  { "_CFloatDouble:At:",          unimp_prim, 0, 0 },
  { "_CFloatDouble:At:Put:",          unimp_prim, 0, 0 },
  { "_CSignedIntSize:At:",          unimp_prim, 0, 0 },
  { "_CSignedIntSize:At:Put:",          unimp_prim, 0, 0 },
  { "_CUnsignedIntSize:At:",          unimp_prim, 0, 0 },
  { "_CUnsignedIntSize:At:Put:",          unimp_prim, 0, 0 },
  { "_OnNonLocalReturn:",          unimp_prim, 0, 0 },
*/
  { "_Perform:",          perform_prim, 0, 0 }, // todo adam perform: do the rest of these - probably need a more general mechanism, though
/*  { "_Perform:With:",          unimp_prim, 0, 0 },
  { "_Perform:With:With:",          unimp_prim, 0, 0 },
  { "_PerformResend:",          unimp_prim, 0, 0 },
  { "_PerformResend:With:",          unimp_prim, 0, 0 },
  { "_PerformResend:With:With:",          unimp_prim, 0, 0 },
  { "_Perform:DelegatingTo:",          unimp_prim, 0, 0 },
  { "_PrimitiveDocumentation",  unimp_prim, 0, 0 },
  { "_PrimitiveList",          unimp_prim, 0, 0 },
*/
  { "_Scavenge",                  scavenge_prim, 0, 0 },
  { "_WizardMode",             wizard_mode_prim, 0, 0 },
  { "_WizardMode:",        set_wizard_mode_prim, 0, 0 },
  
   
  { "", 0, 0, 0 }
};


oop_t PrimitiveTableEntry::invoke(oop_t rcvr, oop_t* argsp, smi nargs, oop_t current_actp, oop_t* new_actp) {
  if ( arg_count_sans_IfFail != nargs )  {
    printf_and_flush("%d, %d, \n", arg_count_sans_IfFail, nargs);
    unimplemented("arg count mismatch");
  }
  return fn(rcvr, argsp, current_actp, new_actp);
}



PrimitiveTableEntry* Primitives::lookup(oop_t sel, bool* has_IfFail) {
  // todo optimize time, use a hash table, dmu 1/06
  
  for (PrimitiveTableEntry *e = entries;  !e->is_null();  ++e) {
    if (e->matches_sans_IfFail(sel)) {
      *has_IfFail = false;
      return e;
    }
    if (e->matches_with_IfFail(sel)) {
      *has_IfFail = true;
      return e;
    }
  }

  // If the primitive isn't found, we figure out whether it has
  // a fail block  the slow way. This is needed for compatibility
  // with the original Self vm ~ Ausch Mar/06
  
  // todo optimize primitive failure
  *has_IfFail = ByteVectorObj::from(sel)->ends_with_C_string((char*)failure_suffix, length_of_failure_suffix);
  return NULL;
}


oop_t Primitives::invoke( oop_t sel, oop_t rcvr, oop_t* argsp, smi arg_count, oop_t current_activation, oop_t* new_actp ) {
  assert(length_of_C_string((char*)failure_suffix) == length_of_failure_suffix);
  
  bool has_IfFail = false;
  
  PrimitiveTableEntry* pte = lookup(sel, &has_IfFail);
  
  oop_t result =  (pte != NULL)      
                ? pte->invoke( rcvr, argsp, pte->arg_count_sans_IfFail, current_activation, new_actp )
                : unimp_prim ( rcvr, argsp, arg_count,                  current_activation, new_actp );
                
  if (!is_mark(result)) // normal primitive returned successfully
    return result;
  
  if (result == badOop) // the primitive wants to call a method; for now, only the _Perform: primitives do this
    return result;
  
  oop_t rcvr_for_fail = (rcvr == NULL || rcvr == badOop) ? The::oop_of(The::lobby) : rcvr;

  return primitive_fail ( sel,
                          rcvr_for_fail,
                          result, 
                          has_IfFail ? argsp[arg_count-1] : badOop, 
                          current_activation,
                          new_actp);
}


void Primitives::initialize() {
  for ( PrimitiveTableEntry *e = entries;  !e->is_null();  ++e) {

    int cName_sans_IfFail_length  = length_of_C_string(e->cName_sans_IfFail);
    int cName_with_IfFail_length  = cName_sans_IfFail_length + length_of_failure_suffix;
    
    char* cName_with_IfFail = (char*) alloca(cName_with_IfFail_length + 1 /* for the null char */);
    sprintf(cName_with_IfFail, "%s%s", e->cName_sans_IfFail, failure_suffix);
    
    e->selfName_sans_IfFail = StringObj::intern( e->cName_sans_IfFail,  cName_sans_IfFail_length );
    e->selfName_with_IfFail = StringObj::intern(    cName_with_IfFail,  cName_with_IfFail_length );

    e->arg_count_sans_IfFail = arg_count_of_string(e->cName_sans_IfFail, cName_sans_IfFail_length);
  }   
}

