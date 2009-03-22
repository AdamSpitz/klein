# include <stdio.h>
# include "stringObj.hh"
# include "utils.hh"
# include "objVector.hh"

// must be kept in sync with traits smallInt hash
inline smi hash_of_smi(smi i) { return i; }

// must be kept in sync with klein virtualMachines abstractVM stringComparisonMixin hashElement:
smi hash_of_string(char* str, smi size) {
  smi h = size;
  char *x = str, *end = str + size, *end_minus_three = end - 3;
  while (x <= end_minus_three) {
    h = h ^ hash_of_smi( *x++       )
          ^ hash_of_smi( *x++ <<  8 )
          ^ hash_of_smi( *x++ << 16 );
  }
  if (! (x < end)) return h;  h = h ^ hash_of_smi( *x++      );
  if (! (x < end)) return h;  h = h ^ hash_of_smi( *x++ <<  8);
  
  return h;
}


smi StringObj::hash_for_comparison() {
  return hash_of_string( bytes(), indexableSize() );
}


// must be kept in sync with traits hashTableSetOrDictionary indexOf:IfPresent:IfAbsent:FirstRM:
smi find_string_in_table_past_first_RM(char* str, smi str_size, smi firstRM, smi* indexp, ObjVectorObj* selfStrings, smi io, smi sz, oop_t emptyMarker, oop_t removedMarker) {
  bool firstWrap = true;
  smi i = firstRM + 1;
  
  while (true) {
    oop_t k;
    if (i < sz) {
      k = selfStrings->read_oop(io + i);
    } else {
      if (firstWrap) {
        firstWrap = false;
        i = 0;
        k = selfStrings->read_oop(io + 0);
      } else {
        *indexp = firstRM;
        return false;
      }
    }
    
    if (  emptyMarker == k)  { *indexp = firstRM;  return false; }
    
    if (removedMarker != k  &&  ::is_byteVector(k)  &&  ByteVectorObj::from(k)->is_equal_to_bytes_at(str, str_size)) {
      selfStrings->write_oop(firstRM, k                           );
      selfStrings->write_oop(i,       The::oop_of(The::nil_object));
      *indexp = firstRM;
      return true;
    }
    
    ++i;
  }
}


// must be kept in sync with traits hashTableSetOrDictionary unsafe_indexOf:IfPresent:IfAbsent:
bool find_string_in_table(char* str, smi str_size, smi* indexp) {
  ObjVectorObj* selfStrings = (ObjVectorObj*) The::addr_of(The::canonicalizedStringVector);
  smi io   =  selfStrings->indexableOrigin();
  smi sz   =  selfStrings->indexableSize();
  
  smi i = (sz - 1) & hash_of_string(str, str_size);

  oop_t   emptyMarker = The::oop_of(The::set_emptyMarker  );
  oop_t removedMarker = The::oop_of(The::set_removedMarker);
  bool firstWrap = true;
  while (true) {
    oop_t k;
    if (i < sz) {
      k = selfStrings->read_oop(io + i);
    } else {
      if (firstWrap) {
        firstWrap = false;
        i = 0;
        k = selfStrings->read_oop(io + 0);
      } else {
        fatal("table should never get this full!");
      }
    }
    
    if (  emptyMarker == k)  { *indexp = i;  return false; }
    if (removedMarker == k)  return find_string_in_table_past_first_RM(str, str_size, i, indexp, selfStrings, io, sz, emptyMarker, removedMarker);
    
    if (::is_byteVector(k) && ByteVectorObj::from(k)->is_equal_to_bytes_at(str, str_size)) { *indexp = i;  return true; }
    
    ++i;
  }
}



oop_t StringObj::slow_intern(char* cString) {
  ObjVectorObj* selfStrings = (ObjVectorObj*) The::addr_of(The::canonicalizedStringVector);
  smi i =      selfStrings->indexableOrigin();
  smi n =  i + selfStrings->indexableSize();
  for ( ; i < n;  ++i ) {
    oop_t x = selfStrings->read_oop(i);
    if ( ::is_byteVector(x) && ByteVectorObj::from(x)->is_equal_to_C_string(cString) ) {

      return x;
    }
  }
  error_printf_and_flush("could not find \'%s\' in the string table; slow_intern cannot add it\n", cString);
  return 0; // shut up the compiler
}


oop_t StringObj::intern(char* cString, fint length) {
  ObjVectorObj* selfStrings = (ObjVectorObj*) The::addr_of(The::canonicalizedStringVector);
  smi io =      selfStrings->indexableOrigin();
  
  smi i;
  fint n = (length == -1) ? length_of_C_string(cString)
                          : length;
  
  bool found = find_string_in_table(cString, n, &i);
  
  if (found)
    return selfStrings->read_oop(io + i);
  
  // printf("adding a new string \'%*s\' at index %i\n", n, cString, i);
    
  ByteVectorObj* new_string_addr;
  oop_t new_string = ((StringObj*) The::addr_of(The::string_proto))->clone_for_C_string(cString, &new_string_addr, n); // could use any canonical string as the prototype
  
  selfStrings->write_oop(io + i, new_string);
  
  smi old_size = value_of_smiOop(assert_smi(The::addr_of(The::canonicalizedStrings)->contents_of_slot(The::oop_of(The::size_string))));
  smi new_size = old_size + 1;
  The::addr_of(The::canonicalizedStrings)->set_contents_of_slot(The::oop_of(The::size_string), smiOop_for_value(new_size));
  
  return new_string;
}
