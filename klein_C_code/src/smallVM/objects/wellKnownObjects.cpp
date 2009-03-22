# include "wellKnownObjects.hh"
# include "stringObj.hh"
# include "lookup.hh"


The::Well_Known_Object The::wks[The::last];

void The::Well_Known_Object::reset_addr() {           addr = MemObj::from(oop); }
void The::Well_Known_Object::set(oop_t x) { oop = x;  addr = MemObj::from( x ); }

void setBootstrapInfo(int oop, int addr) {
  Memory::not_safe_to_do_gc_now();
  Object_Table::initialize( (ObjVectorObj*)addr );
  The::initialize_for_vm(oop_t(oop));
  Memory::safe_to_do_gc_now();
}

void The::set_from(ID dst, ID src, char* slotName) {
  // printf_and_flush("setting %s\n", slotName);
  set_oop_and_addr_of( dst, addr_of(src)->contents_of_slot_for_bootstrapping(slotName) );
} 


void The::initialize_for_vm(oop_t vm_oop) {
  set_oop_and_addr_of( vm, oop_t(vm_oop));

  set_from( universe,             vm,             "universe");
  set_from( objectLocator,        vm,             "objectLocator");
  set_from( start_selector,       vm,             "startSelector");
  set_from( newGeneration,        universe,       "newGeneration");
  set_from( oldGeneration,        universe,       "oldGeneration");
  set_from( edenSpace,            newGeneration,  "edenSpace");
  set_from( tenuredSpace,         oldGeneration,  "tenuredSpace");
  set_from( canonicalizedStrings, universe,       "canonicalizedStrings");
  set_from( float_map,            universe,       "floatMap");
  set_from( smi_map,              universe,       "smiMap");

  set_from( canonicalizedStrings,      universe,             "canonicalizedStrings");
  set_from( canonicalizedStringVector, canonicalizedStrings, "myKeys");
  
  set_oop_and_addr_of(       true_object, Lookup::findObject( vm_oop, StringObj::slow_intern("true"   ) ) );
  set_oop_and_addr_of(      false_object, Lookup::findObject( vm_oop, StringObj::slow_intern("false"  ) ) );
  set_oop_and_addr_of(        nil_object, Lookup::findObject( vm_oop, StringObj::slow_intern("nil"    ) ) );
  set_oop_and_addr_of(      vector_proto, Lookup::findObject( vm_oop, StringObj::slow_intern("vector" ) ) );
  set_oop_and_addr_of(      string_proto, Lookup::findObject( vm_oop, StringObj::slow_intern("string" ) ) );
  set_oop_and_addr_of(             lobby, Lookup::findObject( vm_oop, StringObj::slow_intern("lobby"  ) ) );

  
  set_oop_and_addr_of( set_emptyMarker,   Lookup::findObject( oop_of(canonicalizedStrings), StringObj::slow_intern(  "emptyMarker") ) );
  set_oop_and_addr_of( set_removedMarker, Lookup::findObject( oop_of(canonicalizedStrings), StringObj::slow_intern("removedMarker") ) );
  
  set_oop_and_addr_of( size_string,  StringObj::slow_intern("size") );
  
  // It is safe to use StringObj::intern after this point, because the well-known objects
  // needed for the correct functioning of the string table have been found. -- Adam 2/06
  // todo: Have a little flag indicating this, so that if someone calls intern before
  // this point it'll just call slow_intern?
  set_oop_and_addr_of(                int32_proto, Lookup::findObject( vm_oop, StringObj::intern("int32"  ) ) );
  set_oop_and_addr_of(              process_proto, Lookup::findObject( vm_oop, StringObj::intern("process") ) );
  
  set_oop_and_addr_of(    objectVectorMap_mapType, StringObj::intern(      "objVectorMap") );
  set_oop_and_addr_of(           blockMap_mapType, StringObj::intern(          "blockMap") ); 
  set_oop_and_addr_of( outerActivationMap_mapType, StringObj::intern("outerActivationMap") ); 
  set_oop_and_addr_of( blockActivationMap_mapType, StringObj::intern("blockActivationMap") );
  set_oop_and_addr_of(             mapMap_mapType, StringObj::intern(            "mapMap") );



  set_oop_and_addr_of(           restart_selector, StringObj::intern("_Restart") );

  set_oop_and_addr_of( mirrors_namespace, Lookup::findObject( Lookup::findObject( vm_oop, StringObj::intern("yoda") ), StringObj::intern("mirrors") ) );
  
  // XXXX unimplemented("set assignment primitive");

  assert_all_valid();
}


void The::assert_all_valid() {
  for (ID i = vm;  i < last;  i = ID(int(i) + 1))
    wks[i].addr->assert_valid();
}


void The::moved_some_objects() {
  for (ID i = vm;  i < last;  i = ID(int(i) + 1))
    reset_addr_of(i);
}
