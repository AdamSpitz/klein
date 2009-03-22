# ifndef PRIMS_HH
# define PRIMS_HH

# include "small_self_types.hh"


typedef   oop_t (*prim_fn_t)(oop_t rcvr, oop_t* argsp, oop_t current_activation, oop_t* new_actp);


class PrimitiveTableEntry {

 public:
  char* cName_sans_IfFail;
  prim_fn_t fn;

  // computed at initialize time  
  smi arg_count_sans_IfFail; 
  oop_t selfName_sans_IfFail;
  oop_t selfName_with_IfFail;
  
 public:
  bool  is_null() { return !fn; }

  // sel should always be a canonical string
  bool  matches_sans_IfFail(oop_t sel) {   return sel == selfName_sans_IfFail; }
  bool  matches_with_IfFail(oop_t sel) {   return sel == selfName_with_IfFail; }
                            
  oop_t invoke(oop_t rcvr, oop_t* argsp, smi nargs, oop_t current_actp, oop_t* new_actp);

};  
  

class Primitives {
 private:
  static const char*            failure_suffix;
  static const int    length_of_failure_suffix;
  static PrimitiveTableEntry entries[];
  static PrimitiveTableEntry* lookup(oop_t sel, bool* has_IfFail);
 public:
  static oop_t invoke( oop_t sel, oop_t rcvr, oop_t* argsp, smi arg_count, oop_t current_activation, oop_t* new_actp );
  static void initialize();
};

# endif // PRIMS_HH