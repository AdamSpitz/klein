# ifndef OBJECT_TABLE_HH
# define OBJECT_TABLE_HH

# include "small_self_types.hh"
# include "asserts.hh"
# include "tag.hh"

class MemObj;
class ObjVectorObj;

class Object_Table {
  friend class LayoutConstantsGetter;
 private:
  static const fint lastInvalidEntry_offset = 4;
  
  static ObjVectorObj* ot_addr;
  static MemObj** base;
  static fint     size;

  static int32 timestamp;

  static oop_t        lastInvalidEntry(       );
  static void  record_lastInvalidEntry(oop_t o);
  
 public:
  static void initialize( ObjVectorObj* a );
  
  static MemObj** base_addr() { return base; }
  static MemObj**  end_addr() { return base + size; }
  static MemObj** address_of_entry_for_oop(oop_t x) { return base + index_for_oop(x); }

  static MemObj*  at_int(    fint x            )  {          return base[assert_bounds(x, size)];       }
  static void     at_int_put(fint x, MemObj* mo)  {  ++timestamp;   base[assert_bounds(x, size)] = mo;  }
  
  static fint index_for_oop(oop_t x) { return value_of_smiOop(assert_mem(x)); }
  
  static MemObj* at_oop(    oop_t x            )  { return at_int(    index_for_oop(x) ) ; }
  static void    at_oop_put(oop_t x, MemObj* mo)  { return at_int_put(index_for_oop(x), mo      ); }
  
  static oop_t   oop_for_int(fint x)  {return memOop_for_value(index_for_oop(x));}          
  
  static bool is_valid(  MemObj*  addr ) { return ! is_float( (oop_t)addr ); }
  static void invalidate(MemObj** addrp) { *((oop_t*)addrp)  = lastInvalidEntry();
                                           record_lastInvalidEntry(invalid_entry_for_int(addrp - base));
                                         }
  static void recycle_oop(oop_t o) { invalidate(address_of_entry_for_oop(o)); }
  static oop_t invalid_entry_for_int( fint x  ) { return (oop_t(x) << tag_shift) | float_tag; }
  static oop_t int_for_invalid_entry( oop_t o ) { return o >> tag_shift; }
  
  static oop_t  alloc_oop(MemObj* addr) {
                  fint i = int_for_invalid_entry(lastInvalidEntry());
                  if (i == -1)
                    unimplemented("call GC or expand OT");
                  record_lastInvalidEntry((oop_t) at_int(i));
                  base[i] = addr; /* todo unimplemented write barrier? dmu 1/06 */ 
                  return smiOop_for_value(i) - smi_tag + mem_tag;
                }
                
  static int32 get_timestamp_address() { return (int)&timestamp; }
};


# endif OBJECT_TABLE_HH
