# ifndef OBJECT_HH
# define OBJECT_HH

// $Revision: 1.29 $

# include "tag.hh"
# include "mark.hh"
# include "objectTable.hh"
# include "wellKnownObjects.hh"
# include "memory.hh"
# include <stdio.h>

//todo cleanup: Get these numbers from inside the image? ~Ausch
const int MAXIMUM_SMI_VALUE =  536870911; //      2^29 - 1
const int MINIMUM_SMI_VALUE = -536870912; // -1 * 2^29

extern bool is_boolean(    oop_t),
            is_method(     oop_t),
            is_objVector(  oop_t),
            is_byteVector( oop_t),
            is_blockMethod(oop_t),
            is_block(      oop_t);

inline oop_t assert_objVector(oop_t x)  { assert(is_objVector(x));  return x; }

class MapObj;

class MemObj {
 friend class LayoutConstantsGetter;
 protected:
  static const fint      mark_offset =  0;
  static const fint       map_offset =  1;
  static const fint  contents_offset =  2;
  
  
 public:
  static MemObj* from(oop_t x) { return Object_Table::at_oop(x)->assert_valid(); } 
  
  bool         is_valid() { return (int(this) & 3) == 0; }
  MemObj*  assert_valid() { assert(is_valid());  return this; }

  oop_t     mark_oop()       { return read_oop(     mark_offset    ); }
  oop_t      map_oop()       { return read_oop(      map_offset    ); }
  oop_t contents_oop(fint i) { return read_oop( contents_offset + i); }
  
  fint  oid() { return mark_oop() >> markOop_oid_shift; }
  oop_t oop() { return memOop_for_value( oid() ); }
  void set_oop( oop_t o ) { set_mark( adjust_mark_oop_to_encode_oid( mark_oop(), value_of_smiOop(o) ) ); }
  
  static oop_t  markOop_for_clone( oop_t orig_mark, oop_t new_obj );
  
  MapObj* map_addr();
  

  void  set_mark(             oop_t x)  { write_oop(     mark_offset,      x); }
  void  set_map(              oop_t x)  { write_oop(      map_offset,      x); }
  void  set_contents(fint i,  oop_t x)  { write_oop( contents_offset + i,  x); }
  
  oop_t* oop_addr( fint offset )  { return &((oop_t*)this)[offset]; }

  oop_t read_oop( fint offset          )  {  read_barrier(offset);  return *oop_addr(offset); }
  void write_oop( fint offset,  oop_t x)  { write_barrier(offset, x);      *oop_addr(offset) = x;  }
    
  // bypass the barriers for smiOops
  oop_t read_smiOop( fint offset )           { return assert_smi(*oop_addr(offset)); }
  void write_smiOop( fint offset,  oop_t x)  { *oop_addr(offset) = assert_smi(x);    }
  
  smi    read_smi(fint offset)         { return value_of_smiOop(read_smiOop(offset)); }
  void  write_smi(fint offset, smi x)  { write_smiOop(offset, smiOop_for_value(x)); }
  
  smi     read_mark_value()            { return value_of_markOop((u_smi)*oop_addr(mark_offset)); }
  void   write_mark_value(smi x)       { *oop_addr(mark_offset) = markOop_for_value(x); }

  // for GC
  void  read_barrier( fint /* offset */ ) {}
  void write_barrier( fint /* offset */,  oop_t x ) {
    if (is_mem(x)) {
      MemObj* xaddr = NULL;
      if (gc_is_marked_black()) {
        xaddr = MemObj::from(x);
        if (xaddr->gc_is_marked_white())
          Memory::remember_to_revisit( this ); // todo adam gc compaction: revisit this object, or the target object?
      }
      
      if (is_old()) {
        if (! gc_is_already_in_remembered_set() ) {
          if (!xaddr) xaddr = MemObj::from(x);
          if ( xaddr->is_young() ) {
            gc_mark_as_being_in_remembered_set();
            Memory::add_to_remembered_set( oop() );
          }
        }
      }
    }
  }
  
  oop_t alloc_oop_for_me()  { return Object_Table::alloc_oop(this); }
  
  fint total_size_in_oops();
  
  
  oop_t    contents_of_slot(oop_t name);
  void set_contents_of_slot(oop_t name, oop_t x);

  oop_t    contents_of_slot_for_bootstrapping(char* name);
  
  bool is_byteVector()    { return read_mark_value() & mark_byteVector_bit;    }
  bool is_activationMap() { return read_mark_value() & mark_activationMap_bit; }
  bool is_activation()    { return read_mark_value() & mark_activation_bit;    }
  
  //todo optimize too slow? ~Ausch
  bool is_objVector (); 
  bool is_block     ();
  bool is_map       ();
  
  bool is_in_space(MemObj* space_addr);
  bool is_old()   { return is_in_space(The::addr_of(The::tenuredSpace)); }
  bool is_young() { return is_in_space(The::addr_of(The::   edenSpace)); }
  
  // optimized to avoid the shifting by using mark_oop() and set_mark() instead of read_mark_value() and write_mark_value()
  void    set_bit_in_mark( oop_t bit_mask ) { set_mark( mark_oop() |  bit_mask ); }
  void  clear_bit_in_mark( oop_t bit_mask ) { set_mark( mark_oop() & ~bit_mask ); }
  bool is_bit_set_in_mark( oop_t bit_mask ) { return    mark_oop() &  bit_mask  ; }
  
  void set_mark_bit_for_activation() { set_bit_in_mark( markOop_activation_bit ); }

  void lookup_mark()      {           set_bit_in_mark( markOop_hasBeenVisitedForLookup_bit ); }
  void lookup_unmark()    {         clear_bit_in_mark( markOop_hasBeenVisitedForLookup_bit ); }
  bool lookup_is_marked() { return is_bit_set_in_mark( markOop_hasBeenVisitedForLookup_bit ); }

  // todo optimize time: I'm probably reading and writing the mark word more often than necessary.
  bool gc_is_marked_as_live() { return is_bit_set_in_mark( markOop_hasBeenVisitedForGC_bit ); }

  void gc_mark_as_off_mark_stack() { clear_bit_in_mark( markOop_isOnMarkStackForGC_bit ); }
  
  void gc_mark_black() { set_mark( gc_make_markOop_black( mark_oop()) ); }
  void gc_mark_grey()  { set_mark( gc_make_markOop_grey(  mark_oop()) ); }
  void gc_mark_white() { set_mark( gc_make_markOop_white( mark_oop()) ); }
  bool gc_is_marked_black() { return gc_is_markOop_black( mark_oop()  ); }
  bool gc_is_marked_white() { return gc_is_markOop_white( mark_oop()  ); }

  void gc_mark_as_being_in_remembered_set()     {           set_bit_in_mark( markOop_isInRememberedSetForGC_bit ); }
  void gc_mark_as_not_being_in_remembered_set() {         clear_bit_in_mark( markOop_isInRememberedSetForGC_bit ); }
  bool gc_is_already_in_remembered_set()        { return is_bit_set_in_mark( markOop_isInRememberedSetForGC_bit ); }

  
  // cloning
  oop_t clone(MemObj** addrp = NULL);
 protected:
  oop_t clone_oops_and_allocate_bytes( smi nOops, smi nBytes, MemObj** addrp = NULL, char** bytesp = NULL);
  
  // printing  
 public:
  static void debug_print( oop_t x );
  static void       print( oop_t x );
  static void   print_oop( oop_t x );
};


inline oop_t mapOop(oop_t x) {
  switch(tag(x)) {
    case  float_tag:  return The::oop_of(The::float_map);
    case    smi_tag:  return The::oop_of(The::  smi_map) ;
    case    mem_tag:  return MemObj::from(x)->map_oop();
      
    case   mark_tag:  fatal("Marks have no tag_t");
            default:  fatal("???");
                      return 0;
  }
}

extern void print(oop_t x);
extern void get_print_string(oop_t x, char*, int);


# endif // OBJECT_HH
