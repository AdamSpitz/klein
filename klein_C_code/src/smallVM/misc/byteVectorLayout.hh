# ifndef KLEIN_BYTE_VECTOR_LAYOUT_H
# define KLEIN_BYTE_VECTOR_LAYOUT_H

# include "abstractVectorLayout.hh"


class ByteVectorLayout : public AbstractVectorLayout {
 public:

          OopValueHeaderField*  bytesPartRefField();
  virtual AbstractHeaderField*          lastField();
 	
  BPRef     bytesPartRefOf( Oop o              );
  void  set_bytesPartRefOf( Oop o, BPRef bpRef );
 
  int      indexableSizeOf(Oop o);
  void set_indexableSizeOf(Oop o, int n);
 	
  Byte* for_AddressOfIndexableAt(Oop o, int i        );
  Byte  for_IndexableAt         (Oop o, int i        );
  void  for_IndexableAt_Put     (Oop o, int i, Byte b);
  
  bool isByteVector(Oop o);
  bool isString    (Oop o);
    
  virtual void verifyObject_UpTo_With(Oop mapOop, Oop o, int nextIndex, Verifier* aVerifier);
          void verifyBytesPartRefOf  (            Oop o,                Verifier* aVerifier);
  
  int argCountOf(Oop o);
  
  Oop newString(char* s);
};

# endif // KLEIN_BYTE_VECTOR_LAYOUT_H
