# ifndef KLEIN_OBJ_VECTOR_LAYOUT_H
# define KLEIN_OBJ_VECTOR_LAYOUT_H

# include "abstractVectorLayout.hh"

class MapLayout;

class ObjVectorLayout : public AbstractVectorLayout {
 public:
  SmiValueHeaderField* indexableOriginField();
  SmiValueHeaderField* indexableSizeField();
  virtual AbstractHeaderField* lastField();
  
  int      indexableOriginOf( Oop o        );
  void set_indexableOriginOf( Oop o, int n );
  
  int      indexableSizeOf  ( Oop o        );
  void set_indexableSizeOf  ( Oop o, int n );
  
  Address     for_AddressOfIndexableAt( Oop o, int i        );
  Oop         for_IndexableAt         ( Oop o, int i        );
  FailureCode for_IndexableAt_Put     ( Oop o, int i, Oop x );
  
  bool isObjVector( Oop o );
  
  int  verifyIndexableSizeOf  (             Oop o,                Verifier* aVerifier );
  int  verifyIndexableOriginOf(             Oop o,                Verifier* aVerifier );
  void verifyObject_UpTo_With ( Oop mapOop, Oop o, int nextIndex, Verifier* aVerifier );
  
  MapLayout* mapLayoutFor( Oop mapOop );
};

# endif // KLEIN_OBJ_VECTOR_LAYOUT_H
