# ifndef BYTE_VECTOR_HH
# define BYTE_VECTOR_HH

# include "abstractVector.hh"
//todo cleanup if we fix utils to use stdio, won't need 2 includes here ~ausch
# include "utils.hh" 
# include <stdio.h>


class ByteVectorObj : public AbstractVectorObj {
 public:
  static ByteVectorObj* from(oop_t x) { return (ByteVectorObj*) AbstractVectorObj::from(x); }
  
 private:
  bool range_check (smi i) { return i >= 0  &&  i < indexableSize(); }

 public:
  // add in byte accessing code here
  unsigned char   raw_byteAt(     smi i                  )  { assert(range_check(i)); return bytes()[i];     }
  void            raw_byteAt_Put( smi i, unsigned char c )  { assert(range_check(i));        bytes()[i] = c; }
  
  oop_t  byteAt(     smi i        )  { return smiOop_for_value(raw_byteAt(i)); }
  void   byteAt_Put( smi i, smi c )  { raw_byteAt_Put(i, (unsigned char) value_of_smiOop(c)); }
  
  char* bytes()  { return (char*) oop_addr(indexableOrigin()); }

  bool is_equal_to_bytes_at(char* other_bytes, int other_bytes_size);

  bool is_equal_to_C_string(char*);
  bool   ends_with_C_string(char* cString, int size = -1);

  void   copy_bytes_to(     char* new_bytes, smi new_indexable_size, char fill = 0,                               bool shouldFill = true);
  oop_t  clone_and_resize(                   smi new_indexable_size, char fill = 0, ByteVectorObj** addrp = NULL, bool shouldFill = true);
  oop_t  clone_for_C_string(                 char* cString,                         ByteVectorObj** addrp = NULL,                          fint length = -1);
  oop_t  clone_for_int32(   int32 i,                                                ByteVectorObj** addrp = NULL                                           );

  void range_set (smi from, smi length, char* new_values);

  fint total_size_in_oops() { return indexableOrigin() + divide_and_round_up(indexableSize(), sizeof(oop_t)); }

  void  error_print()   { error_printf_and_flush( "%*s", indexableSize(), bytes()); }
  void  string_print()  {    
    // Should just be: 
    //  printf( "%*s", indexableSize(), bytes());
    //  But this breaks on the MacBookPro, I don't know why. dmu 6/06
    int n = indexableSize();
    char* p = bytes();
    char c = p[n]; p[n] = '\0';
    printf("%s", p);
    p[n] = c;
  }
  
  void  copy_to_C_string(char* s, unsigned int size);
  
  static void  print_byteVector(oop_t obj);
  
  fint arg_count() { return arg_count_of_string( bytes(), indexableSize() ); }
  
};



# endif // BYTE_VECTOR_HH