# include "objVector.hh"
# include "utils.hh"

oop_t ObjVectorObj::clone() {
  return clone_and_resize(indexableSize(), 0, NULL);
}


void ObjVectorObj::print_objVector(oop_t x) {
  printf("object vector: {");
  if (MemObj::from(x)->is_map()) {
    printf("...");
  } else {
    ObjVectorObj* obj = ObjVectorObj::from(x);
    bool first = true;
    oop_t* p    =     (oop_t*)(obj -> indexableOrigin());
    oop_t* end  = p + obj -> indexableSize();
    oop_t* end2 = p + VectorPrintLimit < end ? p + VectorPrintLimit : end;
    for (; p < end2; p ++) {
      if (first) first = false;
      else printf(", ");
      MemObj::print_oop(*p);
    }
    if (end != end2) {
      printf(", ... (%d more elements) ", end - end2);
    }
  }
  printf("} ");
}

oop_t ObjVectorObj::clone_and_resize(smi new_obj_indexable_size, oop_t fill_if_not_badOop, ObjVectorObj** addrp, smi new_obj_indexable_origin_or_zero) {
  smi  io = indexableOrigin();
  smi  new_obj_indexable_origin  =   new_obj_indexable_origin_or_zero == 0  ?  io  :  new_obj_indexable_origin_or_zero;

  fint nOops = new_obj_indexable_origin + new_obj_indexable_size;
  
  ObjVectorObj* a;
  oop_t new_obj = Memory::allocate_oops(nOops, (MemObj**) &a);
  
  if (new_obj == badOop)
    unimplemented("out of memory");
  
  oop_t* new_obj_addr = (oop_t*)a;
  
  smi  sz = indexableSize();
  
  oop_t*  start_of_new_obj_indexables = new_obj_addr                +         new_obj_indexable_origin ;
  oop_t*  end_of_indexables_copy      = start_of_new_obj_indexables + min(sz, new_obj_indexable_size  );
  oop_t*  past_new_obj                = start_of_new_obj_indexables +         new_obj_indexable_size   ;
  
  assert(mark_offset == 0);
  a->set_mark(markOop_for_clone(mark_oop(), new_obj));
  
  {
    bool shouldFill = fill_if_not_badOop != badOop;
    assert(shouldFill = true); // skip badOop fill if no asserts

    oop_t *srcp = ((oop_t*)this) + mark_offset + 1,
          *dstp = new_obj_addr   + mark_offset + 1;

    if (io != new_obj_indexable_origin) {
      oop_t*  end_of_named_slots_copy = new_obj_addr + min(io, new_obj_indexable_origin);
      
                       while ( dstp < end_of_named_slots_copy     )  *dstp++ = *srcp++;
      if (shouldFill)  while ( dstp < start_of_new_obj_indexables )  *dstp++ = fill_if_not_badOop;
      
      a->set_indexableOrigin(new_obj_indexable_origin);
      srcp = ((oop_t*)this) + io;
    }

                     while ( dstp < end_of_indexables_copy      )  *dstp++ = *srcp++;
    if (shouldFill)  while ( dstp < past_new_obj                )  *dstp++ = fill_if_not_badOop;
  }
  
  a->set_indexableSize(new_obj_indexable_size);
  if (addrp)  *addrp = a;
  return new_obj;
}
