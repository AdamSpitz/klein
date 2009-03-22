# include "string.h"
# include "utils.hh"
# include "byteVector.hh"
# include "stringObj.hh"



void ByteVectorObj::range_set(smi from, smi length, char* new_values) {
    if ( length == 0 )
      return;

    assert( length > 0 );
    assert( range_check( from              ));
    assert( range_check( from + length - 1 ));
//  todo alex somehow ensure that the length of new values is at least length in size        
    for ( char *oc = &(bytes()[from]),  *nc = new_values; 
              oc < &(bytes()[from + length]); 
             *oc++ = *nc++) {}
}



bool ByteVectorObj::is_equal_to_C_string(char* cString) {
  // There's no '\0' at the end of the Self string, so be careful.
  smi   n = indexableSize();
  char* p = bytes();
  
  char* cp = cString;
  
  for (  ;  n--;  ++p, ++cp ) {
    char c = *cp;
    if ( c == '\0'  ||  c != *p )
      return false;
  }
  return *cp == '\0';
}


bool ByteVectorObj::is_equal_to_bytes_at(char* other_bytes, fint other_bytes_size) {
  smi   n = indexableSize();
  if (n != other_bytes_size)
    return false;
  
  char* p = bytes();
  
  char* cp = other_bytes;
  
  while (n--)
    if ( *p++ != *cp++ )
      return false;

  return true;
}


//aaa todo check if can factor this and is_equal_to_C_string
bool ByteVectorObj::ends_with_C_string(char* cString, int size) {
  // passing in a size is just an optimization
  // size should not include null character
  if (size == -1)            size  = length_of_C_string(cString);
  else               assert( size == length_of_C_string(cString));
    
  if (size > indexableSize())  return false; //optimization
  
  for (char *sp = bytes() + indexableSize(),
            *cp = cString +          size;
         cString <= cp; )  {
    if ( *cp-- != *sp-- )
      return false;
  }
  return true;
}


void ByteVectorObj::copy_to_C_string(char* s, unsigned int size) {
  unsigned int n = min(indexableSize(), size - 1);
  strncpy(s, bytes(), n);
  s[n] = '\0';
}


oop_t ByteVectorObj::clone_and_resize(smi new_indexable_size, char fill, ByteVectorObj** addrp, bool shouldFill) {
  ByteVectorObj* new_addr;
  char* new_bytes;
  oop_t new_bv = clone_oops_and_allocate_bytes(indexableOrigin(), new_indexable_size, (MemObj**) &new_addr, &new_bytes);
  
  new_addr->set_indexableSize(new_indexable_size);
  copy_bytes_to(new_bytes, new_indexable_size, fill, shouldFill);
  if (addrp) *addrp = new_addr;
  
  assert(new_addr->is_byteVector());
  return new_bv;
}

void ByteVectorObj::copy_bytes_to(char* new_bytes, smi new_indexable_size, char fill, bool shouldFill) {
  char  *srcp = bytes();
  char  *dstp = new_bytes;

  char* end_before_fill = dstp + min(indexableSize(), new_indexable_size);
  char* end_for_fill    = dstp + new_indexable_size ;
  
                    while ( dstp <  end_before_fill )  *dstp++ = *srcp++;
  if (shouldFill)   while ( dstp <  end_for_fill    )  *dstp++ = fill;
}


oop_t ByteVectorObj::clone_for_C_string(char* cString, ByteVectorObj** addrp, fint length) {
  fint n = (length == -1) ? length_of_C_string(cString)
                          : length;
  
  ByteVectorObj* addr;
  oop_t new_bv = clone_and_resize(n, 0, &addr, false);
  if (addrp) *addrp = addr;
  
  char *srcp = cString, *dstp = addr->bytes(), *endp = cString + n;
  while (srcp != endp)
    *dstp++ = *srcp++;
  
  return new_bv;
}

oop_t ByteVectorObj::clone_for_int32(int32 i, ByteVectorObj** addrp) {
  ByteVectorObj* addr;
  oop_t new_bv = clone_and_resize(sizeof(int32), 0, &addr, false);
  if (addrp) *addrp = addr;

  *((int32*) bytes()) = i;

  return new_bv;
}

void ByteVectorObj::print_byteVector(oop_t oop) {
  printf("byte array: {");
  if (MemObj::from(oop)->is_map()) {
    printf("...");
  } else {
    ByteVectorObj* obj = ByteVectorObj::from(oop);
    bool first = true;
    char* p =         obj->bytes();
    char* end = p +   obj->indexableSize();
    char* end2 = p + VectorPrintLimit < end ? p + VectorPrintLimit : end;
    for (; p < end2; p ++) {
      if (first) first = false;
      else printf(", ");
      printf("%ld", long(*p));
    }
    if (end != end2) {
      printf(", ... (%d more elements) ", end - end2);
    }
  }
  printf("} ");
}
