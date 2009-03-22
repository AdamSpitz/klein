# include "map.hh"
# include "stringObj.hh"


SlotDesc*   MapObj::find_slot_with_C_name(char* n) {
  FOR_EACH_SLOT_DESC(this, sd)
    if (sd->name_addr()->is_equal_to_C_string(n))
      return sd;
  return NULL;
}

// todo optimize time should do either binary search or move to front -- dmu 1/06

SlotDesc*   MapObj::find_slot(oop_t selector) {
  FOR_EACH_SLOT_DESC(this, sd) {
    // error_printf_and_flush("find_slot "); StringObj::from(sd->name())->error_print();
    if (sd->name() == selector)
      return sd;
  }
  return NULL;
}


oop_t  SlotDesc::write_map_slot(MemObj*, oop_t) {
  unimplemented("write_map_slot error?");
  return 0; 
}  


void SlotDesc::print() {
  printf_and_flush("Slot Desc 0x%x: %*s, type 0x%x, value 0x%x, is_obj %d, is_map %d, is_parent %d\n", 
                   this,
                   name_addr()->indexableSize(),
                   name_addr()->indexableOrigin(),
                   type(),
                   value(),
                   is_object(),
                   is_map(),
                   is_parent());
}

void SlotDesc::printAugmentedName() {
  if (is_argument()) printf(":");
  ByteVectorObj::from(name())->string_print();
  if (is_parent())   printf("*");
}

void printMark(oop_t oop) {
  assert(tag(oop) == mark_tag);
  printf("a Mark#%#lx", long(oop)); 
}

void printObjectId( oop_t oop ) {
  assert(is_mem(oop));
  //printf("<id: %ld oop: (0x%lx)>", Object_Table::index_for_oop(oop), oop);
  printf("%ld",Object_Table::index_for_oop(oop));
}

void MapObj::print( oop_t oopForObj ) {
  MapObj* map;
  bool printingAMap = false;
  switch (tag(oopForObj)) {
    case float_tag: unimplemented("floats"); return;
    case  mark_tag: printMark(oopForObj);    return;
    case   smi_tag: 
      printf("%d: ", (value_of_smiOop(oopForObj)));
      map = MapObj::from(The::oop_of(The::smi_map)); 
      break;
    case   mem_tag: 
      printf("<"); printObjectId (oopForObj); printf(">: ");
      MemObj* obj = MemObj::from(oopForObj);
      map = obj->map_addr();
      if (map->is_mapMap()) { printf("map "); printingAMap = true;}
      break;
  }
  
  printf("( ");
  fint length = map->slotDesc_count();
  if (length || map->is_block()) {
    printf("| ");
    FOR_EACH_SLOT_DESC(map, slot) {
      if (!slot->is_assignment()) { //don't print assignment slot names
        slot->printAugmentedName();
        if (slot->is_argument()) 
          printf(" = <arg %ld>", slot->contents(oopForObj));
        else if (slot->is_map()) {
          printf(" = ");
          print_oop(slot->contents(oopForObj));
        } else if (slot->is_assignment()) {
          //do nothing
        } else {
          assert(slot->is_object());
          if (printingAMap) {
            // just printing a map; there isn't an object to print
            printf(" = <offset %ld>", slot->value());
          } else {
            // printing a real object; fetch its slot
            printf(" <- ");
            print_oop(slot->contents(oopForObj));
          }
        }
        printf(". ");
      }
    }
    printf("| ");
  }
  if ( ::is_byteVector( oopForObj ))  ByteVectorObj::print_byteVector(oopForObj);
  if ( ::is_objVector ( oopForObj ))   ObjVectorObj::print_objVector( oopForObj);
//  if ( obj -> is_method()     ) printCode(obj);
  
  printf(")\n");
}

 
void MapObj::print_oop ( oop_t objOop ) {
  switch(tag (objOop)) {
   case   smi_tag:  printf("<a smi, value: %d>", (value_of_smiOop(objOop))); return;
   case float_tag:  printf("<a float  ???>");                                return;// yoda can't handle floats yet
   case  mark_tag:  printMark( objOop );                                     return;
   case   mem_tag:  break;
  }  
  //printf ("[object table index: %d] ", Object_Table::index_for_oop(debugee));
  printf("<");
  if      (objOop ==  The::oop_of (The::true_object))          printf("true");
  else if (objOop ==  The::oop_of (The::false_object))         printf("false");
  else if (objOop ==  The::oop_of (The::nil_object))           printf("nil");
  else if (objOop ==  The::oop_of (The::vector_proto))         printf("vector prototype");
  else if (objOop ==  The::oop_of (The::string_proto))         printf("string prototype");
  else if (objOop ==  The::oop_of (The::set_emptyMarker))      printf("set empty marker");
  else if (objOop ==  The::oop_of (The::set_removedMarker))    printf("set removed marker"); 
  
  else if (objOop ==  The::oop_of (The::blockMap_mapType))                printf("block map");
  else if (objOop ==  The::oop_of (The::objectVectorMap_mapType))         printf("object vector map");
  else if (objOop ==  The::oop_of (The::outerActivationMap_mapType))      printf("outer activation map");
  else if (objOop ==  The::oop_of (The::blockActivationMap_mapType))      printf("block activation map");
    
  else if (objOop ==  The::oop_of (The::smi_map))         printf("smi map");
  else if (objOop ==  The::oop_of (The::float_map))       printf("float map");

  else if (::is_method ( objOop ))        printf("a method");
  else if (::is_block  ( objOop ))        printf("a block");
  
  
 // else if (::is_string ( objOop ))       ByteVectorObj::from(debugee)->string_print(); //todo implement case for immutable strings
 // else if (::is_map    ( objOop ))        printf(" a map "); //

  else if (objOop ==  The::oop_of (The::vm))       printf("the vm");

  else printObjectId(objOop);
  printf(">");
}