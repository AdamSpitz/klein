# ifndef STRINGOBJ_HH
# define STRINGOBJ_HH

# include "byteVector.hh"

class StringObj : public ByteVectorObj {
 public:
  static StringObj* from(oop_t s) { return (StringObj*) ByteVectorObj::from(s); }
 
  static oop_t slow_intern(char* cString); // does a simple linear search through the entire vector
  static oop_t      intern(char* cString, int length = -1); // does a hash lookup; cannot be used until The is initialized
  
  smi hash_for_comparison();
};


# endif // STRINGOBJ_HH