# ifndef KLEIN_MEMORY_OBJECT_LAYOUT_H
# define KLEIN_MEMORY_OBJECT_LAYOUT_H

# include "objectLayout.hh"
# include "tag.hh"

class AbstractHeaderField;
class OopValueHeaderField;
class SmiValueHeaderField;

class MemoryObjectLayout : public ObjectLayout {
 public:
  virtual TagValue myTag() { return Tag::mem; }

  Oop     encode(Address addr) { return (Oop    ) (((intNN) addr) + myTag()); }
  Address decode(Oop     oop ) { return (Address) (((intNN) oop ) - myTag()); }

  Address addressOfMem ( Oop     oop  ) { return decode(oop ); }
  Oop     memForAddress( Address addr ) { return encode(addr); }

  Address   for_AddressAt      ( Oop o, int i        );
  Oop       for_At             ( Oop o, int i        );
  void      for_At_Put         ( Oop o, int i, Oop x );

  int       for_SmiValueAt     ( Oop o, int i        );
  void      for_At_PutSmiValue ( Oop o, int i, int x );

  MarkValue for_MarkValueAt    ( Oop o, int i               );
  void      for_At_PutMarkValue( Oop o, int i, MarkValue mv );
  
  virtual AbstractHeaderField* lastField();
          AbstractHeaderField* lastHeaderField();

  int emptyObjectSizeFor(Oop o);
  
  MarkValue    markValueOf( Oop o               );
  void      setMarkValueOf( Oop o, MarkValue mv );
  
  int                oidOf( Oop o               );
  void            setOIDOf( Oop o, int oid      );
  
  Oop                mapOf( Oop o               );
  void            setMapOf( Oop o, Oop mapOop   );
  
  Oop            contentsOfSlotInObject( Oop oop,                  char* n );
  FailureCode setContentsOfSlotInObject( Oop oop, Oop newValueOop, char* n );
  
  virtual void verifyObject_UpTo_With( Oop mapOop, Oop o, int nextIndex,        Verifier* aVerifier );
          int  verifyOopsOf          (             Oop o,                       Verifier* aVerifier );
          Oop  verifyContentsOfOop   (             Oop o,                       Verifier* aVerifier );
          int  for_VerifySmiAt_With  (             Oop o, int i,                Verifier* aVerifier );
          void verifyObjectSlotBounds( Oop mapOop, int lowest, int pastHighest, Verifier* aVerifier );
};

# endif // KLEIN_MEMORY_OBJECT_LAYOUT_H
