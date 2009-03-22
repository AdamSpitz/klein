# include "memory.hh"
# include "stringObj.hh"
# include "activation.hh"
# include "interpreter.hh"
# include "freeLists.hh"


// todo optimize time: This is pretty ridiculous. :) So many lookups. Hard-code the offset?
// todo optimize time: Maybe have objsTop, etc. be the actual address, rather than a tagged smi with the address?
inline oop_t*     top_of_space( MemObj* space_addr ) { return (oop_t*) value_of_smiOop( space_addr->contents_of_slot(StringObj::intern("top"       )) ); }
inline oop_t* objsTop_of_space( MemObj* space_addr ) { return (oop_t*) value_of_smiOop( space_addr->contents_of_slot(StringObj::intern("objsTop"   )) ); }
inline oop_t*  bottom_of_space( MemObj* space_addr ) { return (oop_t*) value_of_smiOop( space_addr->contents_of_slot(StringObj::intern("objsBottom")) ); }

inline void set_objsTop_of_space( MemObj* space_addr, oop_t* p ) { space_addr->set_contents_of_slot( StringObj::intern("objsTop"), smiOop_for_value((smi) p) ); }


bool Memory::is_address_in_space( oop_t* addr, MemObj* space_addr ) {
  return bottom_of_space(space_addr) <= addr  &&  addr < top_of_space(space_addr);
}


# include "smallVMDeclarationsOrStubs.hh"
// todo adam gc: What's the right way to allocate this mark stack? And how big should it be?
const int mark_stack_limit = fixed_address_buffer_length / (sizeof(int) * 32);
static oop_t mark_stack[mark_stack_limit];
static int top_of_mark_stack;
static int number_of_objects_marked;

static MemObj** current_sweep_OT_entry_addr;

// todo adam gc: how big should the remembered set be?
const int remembered_set_size = 200;
static oop_t remembered_set[remembered_set_size];
static int number_of_remembered_objects;

void Memory::add_to_remembered_set(oop_t o) {
  remembered_set[number_of_remembered_objects++] = o;
  if (number_of_remembered_objects == remembered_set_size) {
    // todo adam gc: do a scavenge? That would mean that any pointer write could end up moving objects around.
    unimplemented("just filled up the remembered set; now what?");
  }
}


enum gc_phase {
    gc_mark_phase,
    gc_sweep_phase,
    gc_idling_phase
} current_gc_phase;


// todo adam gc: this feels like a hack.
static bool is_safe_to_do_gc = false;
void Memory::not_safe_to_do_gc_now() { is_safe_to_do_gc = false; }
void Memory::    safe_to_do_gc_now() {
 is_safe_to_do_gc = true;
 current_gc_phase = gc_idling_phase;
 top_of_mark_stack = 0;
 current_sweep_OT_entry_addr = Object_Table::base_addr();
 number_of_remembered_objects = 0;
}


void push_on_mark_stack(oop_t o, MemObj* addr) {
  if (top_of_mark_stack >= mark_stack_limit)
    unimplemented("cannot yet recover from mark stack overflow");

  addr->gc_mark_grey();
  mark_stack[top_of_mark_stack++] = o;
}


void push_on_mark_stack_if_necessary(oop_t x) {
  if (! is_mem(x))
    return;
  
  MemObj* addr = MemObj::from(x);
  if (addr->gc_is_marked_as_live())
    return;

  push_on_mark_stack(x, addr);
}


void mark_one_object() {
    oop_t obj = mark_stack[--top_of_mark_stack];
    MemObj* addr = MemObj::from(obj);
    addr->gc_mark_as_off_mark_stack();
    oop_t* p = (oop_t*)addr;
    if (addr->is_activation()) {
      oop_t* end = ((ActivationObj*)addr)->end_of_live_oops();
      while ( ++p != end ) {
        push_on_mark_stack_if_necessary(*p);
      }
    } else if (addr->is_byteVector()) {
      oop_t* end = (oop_t*) ((ByteVectorObj*)addr)->bytes();
      while ( ++p != end ) {
        push_on_mark_stack_if_necessary(*p);
      }
    } else {
      oop_t o;
      while ( !is_mark(o = *++p) ) {
        push_on_mark_stack_if_necessary(o);
      }
    }
}


void mark_n_objects(fint n) {
  fint i = 0;
  while (i < n && top_of_mark_stack > 0) {
    ++i;
    mark_one_object();
  }
  number_of_objects_marked += i;
}


void emergency_full_marking_phase() {
  while (top_of_mark_stack > 0) {
    mark_one_object();
  }
}


inline ObjVectorObj* freeOopsLists_in_space(MemObj* space_addr) {
  // todo optimize time: shouldn't need to do a lookup - hard-code the offset?
  return ObjVectorObj::from( space_addr->contents_of_slot(StringObj::intern("freeOopsLists")) );
}


inline void sweep_one_object(ObjVectorObj* freeOopsLists) {
  MemObj* addr = *current_sweep_OT_entry_addr;
  if (Object_Table::is_valid(addr)) {
    if (addr->gc_is_marked_as_live()) {
      addr->gc_mark_white();
    } else {
      fint nOops = addr->total_size_in_oops();
      FreeOopsLists::add(freeOopsLists, nOops, (oop_t*)addr);
      Object_Table::invalidate(current_sweep_OT_entry_addr);
    }
  }
  ++current_sweep_OT_entry_addr;
}


void sweep_n_objects(fint n) {
  ObjVectorObj* freeOopsLists = freeOopsLists_in_space( The::addr_of(The::tenuredSpace) );
  MemObj** end_addr = Object_Table::end_addr();
  fint i = 0;
  while (i++ < n && current_sweep_OT_entry_addr < end_addr) {
    sweep_one_object(freeOopsLists);
  }
}


void emergency_full_sweeping_phase() {
  ObjVectorObj* freeOopsLists = freeOopsLists_in_space( The::addr_of(The::tenuredSpace) );
  MemObj** end_addr = Object_Table::end_addr();
  while (current_sweep_OT_entry_addr < end_addr) {
    sweep_one_object(freeOopsLists);
  }
}


void reset_gc() {
  current_gc_phase = gc_mark_phase;
  top_of_mark_stack = 0;
  number_of_objects_marked = 0;
  current_sweep_OT_entry_addr = Object_Table::base_addr();
  // Must stay in sync with startingPoints in the exportPolicy in the Self world.
  push_on_mark_stack_if_necessary( Interpreter::current_activation() ); // todo adam gc: I'd like to have a way to start from the root of the stack, since it's less volatile
  push_on_mark_stack_if_necessary( The::oop_of(The::vm   )           );
  push_on_mark_stack_if_necessary( The::oop_of(The::lobby)           );
}


bool should_start_collecting_again() {
  ObjVectorObj* freeOopsLists = freeOopsLists_in_space( The::addr_of(The::tenuredSpace) );
  return ! FreeOopsLists::has_a_fair_amount_of_free_space_left(freeOopsLists);
}


void do_a_little_bit_of_gc() {
  switch (current_gc_phase) {
    case gc_idling_phase:
      if (should_start_collecting_again())
        reset_gc();
      break;
    case gc_mark_phase:
      if (top_of_mark_stack > 0)
        mark_n_objects(300); // todo adam gc magic number
      else
        current_gc_phase = gc_sweep_phase;
      break;
    case gc_sweep_phase:
      sweep_n_objects(300); // todo adam gc magic number
      if (current_sweep_OT_entry_addr >= Object_Table::end_addr())
        current_gc_phase = gc_idling_phase;
      break;
    default:
      fatal("what phase are we in???");
  }
}


// I don't know if we want to leave this functionality in, but it was
// easy to write, and might be useful as a last-resort recovery mechanism
// (though hopefully we can write the incremental GC in a way that this
// will never be necessary). -- Adam, 4/06
void emergency_full_gc() {
  switch (current_gc_phase) {
    case gc_idling_phase:
      reset_gc();
      // fall through
    case gc_mark_phase:
      emergency_full_marking_phase();
      current_gc_phase = gc_sweep_phase;
      // fall through
    case gc_sweep_phase:
      emergency_full_sweeping_phase();
      current_gc_phase = gc_idling_phase;
      break;
    default:
      fatal("what phase are we in???");
  }
}


oop_t Memory::allocate_oops(fint nOops, MemObj** addrp) {
  return Memory::allocate_oops_and_bytes(nOops, NO_BYTES_PART, addrp);
}


inline fint oops_needed_to_hold_bytes(fint bytes_needed) {
  return divide_and_round_up(bytes_needed, sizeof(oop_t));
}


FreeOops* allocate_oops_from_freeOopsLists( MemObj* space_addr, fint nOops ) {
  ObjVectorObj* freeOopsLists = freeOopsLists_in_space( space_addr );
  FreeOops* f = FreeOopsLists::find_freeOops(freeOopsLists, nOops);
  if (!f) {
    printf_and_flush("ran out of memory allocating %i oops; invoking emergency stop-the-world GC...\n", nOops);
    FreeOopsLists::print_freeOops_left(freeOopsLists);
    emergency_full_gc();
    f = FreeOopsLists::find_freeOops(freeOopsLists, nOops);
    if (!f) {
      unimplemented("OK, we're REALLY out of memory. Now what? Compaction?");
    }
  }
  return f;
}


void promote( oop_t o, MemObj* addr ) {
  // todo optimize time: pass the tenured_space_addr in so we don't have to keep finding it again (but watch out for it moving)
  MemObj* tenured_space_addr = The::addr_of(The::tenuredSpace);
  fint nOops = addr->total_size_in_oops();
  FreeOops* f = allocate_oops_from_freeOopsLists( tenured_space_addr, nOops );
  oop_t* new_addr = (oop_t*)f;
  oop_t *src = (oop_t*)addr, *dst = new_addr, *end = new_addr + nOops;
  while (dst != end)
    *dst++ = *src++;
  if (! is_mark(*end))
    *end = trailingMarkOop;
  Object_Table::at_oop_put( o, (MemObj*)new_addr );
}


// todo adam gc: do a non-recursive version of this?
// todo adam gc: duplication with the marking algorithm, but I'm reluctant to make a macro or use a function pointer.
void recursive_scavenge(oop_t o);
void recursive_scavenge_young_objects_in(MemObj* addr) {
  oop_t* p = (oop_t*)addr;
  if (addr->is_activation()) {
    oop_t* end = ((ActivationObj*)addr)->end_of_live_oops();
    while ( ++p != end ) {
      recursive_scavenge(*p);
    }
  } else if (addr->is_byteVector()) {
    oop_t* end = (oop_t*) ((ByteVectorObj*)addr)->bytes();
    while ( ++p != end ) {
      recursive_scavenge(*p);
    }
  } else {
    oop_t o;
    while ( !is_mark(o = *++p) ) {
      recursive_scavenge(o);
    }
  }
}


// todo adam gc: do a non-recursive version of this?
void recursive_scavenge(oop_t o) {
  if (! is_mem(o) )
    return;
  
  MemObj* addr = MemObj::from(o);

  if (! addr->is_young())
    return;
  
  promote( o, addr );
  recursive_scavenge_young_objects_in( addr );
}


void recycle_oops_of_new_objects( MemObj* eden_space_addr ) {
  oop_t* objsBottom =  bottom_of_space(eden_space_addr);
  oop_t* objsTop    = objsTop_of_space(eden_space_addr);
  oop_t* p          = objsBottom;
  while (p < objsTop) {
    MemObj* addr = (MemObj*)p;
    oop_t o = addr->oop();
    MemObj* recorded_addr = MemObj::from(o);
    if (recorded_addr == addr) { // has not been scavenged
      Object_Table::recycle_oop(o);
    }
    p += addr->total_size_in_oops();
  }
  set_objsTop_of_space( eden_space_addr, objsBottom );
}


void do_full_scavenge( MemObj* eden_space_addr ) {
  printf("invoking scavenger...\n");
  for (fint i = 0; i < number_of_remembered_objects; ++i) {
    oop_t   remembered_obj      = remembered_set[i];
    MemObj* remembered_obj_addr = MemObj::from(remembered_obj);
    remembered_obj_addr->gc_mark_as_not_being_in_remembered_set();
    recursive_scavenge_young_objects_in( remembered_obj_addr );
  }
  number_of_remembered_objects = 0;
  recursive_scavenge( Interpreter::current_activation() );
  
  The::moved_some_objects();
  
  recycle_oops_of_new_objects(eden_space_addr);
  
  printf("done scavenging.\n");
}


void Memory::scavenge() {
  do_full_scavenge(The::addr_of(The::edenSpace));
}


oop_t* allocate_oops_in_eden( MemObj* eden_space_addr, fint nOops ) {
  oop_t* old_objsTop = objsTop_of_space(eden_space_addr);
  oop_t*         top =     top_of_space(eden_space_addr);

  oop_t* new_addr    = old_objsTop;
  oop_t* new_objsTop = new_addr + nOops;

  if (new_objsTop >= top) { // >= so as to leave room for the trailing mark
    do_full_scavenge(eden_space_addr);
    return allocate_oops_in_eden(The::addr_of(The::edenSpace), nOops);
  }
  
  *new_objsTop = trailingMarkOop;
  set_objsTop_of_space( eden_space_addr, new_objsTop );
  return new_addr;
}


oop_t Memory::allocate_oops_and_bytes(fint nOops, fint nBytes, MemObj** addrp, char** bytesp) {
  if (is_safe_to_do_gc)
    do_a_little_bit_of_gc();
  
  MemObj* eden_addr = The::addr_of(The::edenSpace);
  oop_t* new_addr = allocate_oops_in_eden( eden_addr, nOops + oops_needed_to_hold_bytes(nBytes) );
  MemObj* a = (MemObj*) new_addr;
  
  if (addrp )  *addrp = a;
  if (bytesp)  *bytesp = (char*) (new_addr + nOops);
  oop_t x = a->alloc_oop_for_me();
  
  return x;
}


void  Memory::remember_to_revisit(MemObj* addr) {
  push_on_mark_stack(addr->oop(), addr);
}

inline bool has_not_swept_yet(oop_t obj) {
  return current_sweep_OT_entry_addr <= Object_Table::address_of_entry_for_oop(obj);
}

oop_t  Memory::adjust_markOop_for_clone(oop_t mark_oop, oop_t new_obj) {
  // todo adam gc: do something less conservative? is this even correct?
  if (    current_gc_phase == gc_mark_phase
      || (current_gc_phase == gc_sweep_phase && has_not_swept_yet(new_obj)))
    return gc_make_markOop_black(mark_oop);
  else
    return mark_oop;
}
