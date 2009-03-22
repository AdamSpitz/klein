# ifndef MEMORY_HH
# define MEMORY_HH

# include "small_self_types.hh"

# define NO_BYTES_PART 0

class MemObj;

class Memory {
 public:
  static oop_t allocate_oops          (fint nOops,              MemObj** addrp = 0                   );
  static oop_t allocate_oops_and_bytes(fint nOops, fint nBytes, MemObj** addrp = 0, char** bytesp = 0);
  
  static bool is_address_in_space( oop_t* addr, MemObj* space_addr );
  
  static void  remember_to_revisit(MemObj* addr);
  static oop_t adjust_markOop_for_clone(oop_t mark_oop, oop_t new_obj);
  
  static void  add_to_remembered_set(oop_t o);
  
  static void  scavenge();
  
  static void  not_safe_to_do_gc_now();
  static void      safe_to_do_gc_now();
};


# endif // MEMORY_HH
