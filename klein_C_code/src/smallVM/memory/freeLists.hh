# ifndef FREE_LISTS_HH
# define FREE_LISTS_HH


# include "tag.hh"

class MemObj;
class ObjVectorObj;

class FreeOops {
 public:
  oop_t next_freeOops_encodedAsMark; // encoded as a mark so as to serve as the end-of-object sentinel
  oop_t sizeOop;
  
  FreeOops*     next_freeOops(           ) { return (FreeOops*) smiOop_for_value(value_of_markOop(next_freeOops_encodedAsMark)); }
  void      set_next_freeOops(FreeOops* f) { next_freeOops_encodedAsMark = markOop_for_value(value_of_smiOop((oop_t) f)); }
  
  smi      size(     ) { return value_of_smiOop(sizeOop); }
  void set_size(smi s) { sizeOop = smiOop_for_value(s); }

  static fint oop_size_of_an_entry() { return sizeof(FreeOops) / sizeof(oop_t); }

  static bool is_acceptable_size(smi nOopsDesired, smi sizeAvailable) {
    return     sizeAvailable == nOopsDesired
           ||  sizeAvailable -  nOopsDesired  >=  oop_size_of_an_entry();  // don't leave holes too small for a whole FreeOops entry
  }
};


class FreeOopsLists {
 public:
  static const fint last_index = 19;
  
  static FreeOops* get_first_entry(ObjVectorObj* freeOopsLists, fint i             );
  static void      set_first_entry(ObjVectorObj* freeOopsLists, fint i, FreeOops* f);
  
  static void add(ObjVectorObj* freeOopsLists, fint nOops, oop_t* p);
  static FreeOops* find_freeOops(ObjVectorObj* freeOopsLists, fint nOops);
  
  static bool has_a_fair_amount_of_free_space_left(ObjVectorObj* freeOopsLists);

  static void FreeOopsLists::print_freeOops_left(ObjVectorObj* freeOopsLists);
};


# endif // FREE_LISTS_HH
