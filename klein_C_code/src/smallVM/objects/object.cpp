# include "object.hh"
# include "map.hh"
# include "stringObj.hh"
# include "activation.hh"


MapObj* MemObj::map_addr() {
  return MapObj::from(map_oop());
}


oop_t MemObj::contents_of_slot(oop_t n) {
  SlotDesc* sd = map_addr()->find_slot(n);
  assert(sd);
  return sd->contents(this);
}


oop_t MemObj::contents_of_slot_for_bootstrapping(char* n) {
  SlotDesc* sd = map_addr()->find_slot_with_C_name(n);
  assert(sd);
  return sd->contents(this);
}  


void MemObj::set_contents_of_slot(oop_t n, oop_t x) {
  SlotDesc* sd = map_addr()->find_slot(n);
  assert(sd);
  sd->set_contents(this, x);
}


fint MemObj::total_size_in_oops() {
  if (is_activation()) {
    return ((ActivationObj*)this)->total_size_in_oops();
  } else if (is_byteVector()) {
    return ((ByteVectorObj*)this)->total_size_in_oops();
  } else {
    oop_t* start = (oop_t*)this;
    oop_t*   end = start;
    while ( !is_mark(*++end) ) {}
    return end - start;
  }
}


bool MemObj::is_objVector()  { oop_t mt = map_addr() -> mapType();
                               return mt == The::oop_of(The::objectVectorMap_mapType)
                                   || mt == The::oop_of(The::mapMap_mapType)
                                   || mt == The::oop_of(The::blockActivationMap_mapType)
                                   || mt == The::oop_of(The::outerActivationMap_mapType); }
bool MemObj::is_block()      { return (map_addr() -> mapType()) == The::oop_of(The::blockMap_mapType);        }

bool MemObj::is_map()        { return  map_addr() -> is_mapMap(); }


bool MemObj::is_in_space(MemObj* space_addr) { return Memory::is_address_in_space( (oop_t*)this, space_addr ); }


oop_t MemObj::clone(MemObj** addrp) {
  if (is_byteVector()) {
    untested("calling regular clone() on a byte vector");
    ByteVectorObj* this_bv = (ByteVectorObj*) this;
    return this_bv->clone_and_resize(this_bv->indexableSize(), 0, (ByteVectorObj**)addrp);
  }
  
  return clone_oops_and_allocate_bytes(total_size_in_oops(), NO_BYTES_PART, addrp);
}


oop_t  MemObj::markOop_for_clone( oop_t orig_mark, oop_t new_obj ) {
  return adjust_mark_oop_to_encode_oid( Memory::adjust_markOop_for_clone(orig_mark, new_obj), value_of_markOop(new_obj) );
}


// todo cleanup refactor this and the ObjVectorObj clone
oop_t MemObj::clone_oops_and_allocate_bytes(smi nOops, smi nBytes, MemObj** addrp, char** bytesp) {
  oop_t* start_of_copy = (oop_t*)this;
  oop_t*   end_of_copy = start_of_copy + nOops;
  
  MemObj* a;
  oop_t new_obj = Memory::allocate_oops_and_bytes(nOops, nBytes, &a, bytesp);
  
  if (new_obj == badOop)
    unimplemented("out of memory");
  
  oop_t* new_obj_addr = (oop_t*)a;
  
  assert(mark_offset == 0);
  a->set_mark(markOop_for_clone(mark_oop(), new_obj));
  
  for (oop_t *dstp =  new_obj_addr + mark_offset + 1,
             *srcp = start_of_copy + mark_offset + 1;
       srcp < end_of_copy;
       *dstp++ = *srcp++)  {}
  
  assert(a->oop() == new_obj);
  if (addrp)  *addrp = a;
  return new_obj;
}


bool is_method(oop_t x) {
  return  is_mem(x)  &&  MemObj::from(x)->is_activationMap();
}

// todo optimize time should be fast -- dmu 1/06
bool is_blockMethod(oop_t x) {
  unimplemented("is_blockMethod");
  return false;
}  

bool is_boolean(oop_t x) {
  return x == The::oop_of(The:: true_object)
      || x == The::oop_of(The::false_object);
}

bool is_byteVector(oop_t x) {
  return is_mem(x)  &&  MemObj::from(x)->is_byteVector();
}

bool is_objVector(oop_t x) {
  return is_mem(x)  &&  MemObj::from(x)->is_objVector();
}

bool is_block(oop_t x) {
  return is_mem(x)  &&  MemObj::from(x)->is_block();
}


// todo cleanup _Print get printing to work right. Dispose of debug_print

void print(oop_t x) {
  char buf[1000];
  get_print_string(x, buf, sizeof(buf));
  printf_and_flush("%s\n", buf);
} 


void MemObj::debug_print(oop_t debugee) {
  const int MaxLen = 30;
  fint t = tag(debugee);
  char s[MaxLen]; // = NEW_RESOURCE_ARRAY(char, MaxLen);
  if (t == smi_tag) {
    sprintf(s, "a smi%ld", (value_of_smiOop(debugee)));
  } else if (t == mem_tag) {
    if      (debugee ==  The::oop_of (The::true_object))          sprintf(s, "true");
    else if (debugee ==  The::oop_of (The::false_object))         sprintf(s, "false");
    else if (debugee ==  The::oop_of (The::nil_object))           sprintf(s, "nil");
    else if (debugee ==  The::oop_of (The::vector_proto))         sprintf(s, "vector prototype");
    else if (debugee ==  The::oop_of (The::string_proto))         sprintf(s, "string prototype");
    else if (debugee ==  The::oop_of (The::set_emptyMarker))      sprintf(s, "set empty marker");
    else if (debugee ==  The::oop_of (The::set_removedMarker))    sprintf(s, "set removed marker"); 
    
    else if (debugee ==  The::oop_of (The::blockMap_mapType))
      sprintf(s, "block map");
    else if (debugee ==  The::oop_of (The::objectVectorMap_mapType))
      sprintf(s, "object vector map");
    else if (debugee ==  The::oop_of (The::outerActivationMap_mapType))
      sprintf(s, "outer activation map");
    else if (debugee ==  The::oop_of (The::blockActivationMap_mapType))
      sprintf(s, "block activation map");
      
    else if (debugee ==  The::oop_of (The::smi_map))   sprintf(s, "smi map");
    else if (debugee ==  The::oop_of (The::float_map))  sprintf(s, "float map");
    
    else if (MemObj::from(debugee) -> is_byteVector()) 
      ByteVectorObj::from(debugee)->string_print();
    else sprintf(s, "??? %#lx", long(debugee));
  } else if (t == float_tag) {
    sprintf(s, "yoda can't handle floats yet");
  } else {
    assert(t == mark_tag);
    sprintf(s, "Mark#%#lx", long(debugee));
  }
  printf_and_flush("%s", s);
}

void MemObj::print(oop_t x) {
  MapObj::print(x);
}


void MemObj::print_oop( oop_t x) {
  if (x == badOop)  
    printf("badOop");
  else 
    MapObj::print_oop(x);
}

void get_print_string(oop_t x, char* s, int size) {
  // toto unimplemented incomplete -- dmu 1/06
  switch (tag(x)) {
    default: fatal("???");
    case float_tag:  sprintf(s, "a float 0x%x", x);                return;
    case   smi_tag:  sprintf(s, "a smi %d", value_of_smiOop(x));   return;
    case  mark_tag:  sprintf(s, "a markOop 0x%x", x);              return;
    case   mem_tag:  break;
  }
  if (size < length_of_C_string(s))  fatal("string overflow");
  size -= length_of_C_string(s);
  if (is_byteVector(x)) {
    ByteVectorObj::from(x)->copy_to_C_string(s, size);
  }
  else if (is_method(x)) {
   printf_and_flush("object vector printing not implemented\n");
//  }else if (is_blockMethod(x)) {
//   printf_and_flush("block method printing not implemented\n");
  } else if (is_block(x)) {
   printf_and_flush("block printing not implemented\n");
  } else if (is_objVector(x)) {
    smi id =  Object_Table::index_for_oop(x);
    ObjVectorObj* x_ov = ObjVectorObj::from(x);
    printf_and_flush("object vector [ID: %d, size: %d], contents:\n", id , x_ov->indexableSize());
    //x_ov->print();
    printf_and_flush("done contents [ID: %d].\n", id);
  } else {
    printf_and_flush("printing not implemented for ??? object type\n");
  }
   
  
  if (size < length_of_C_string(s))  fatal("string overflow");
}

