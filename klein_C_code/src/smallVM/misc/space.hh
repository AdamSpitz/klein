# ifndef KLEIN_SPACE_H
# define KLEIN_SPACE_H

# include "kleinObject.hh"

class Verifier;

class Space : public KleinObject {
 protected:
  char* _name;
 public:
  Space(char* n, Oop oop) : KleinObject(oop), _name(n) {}
    
  Address    objsBottom ();
  Address    objsTop    ();
  Byte*      bytesBottom();
  Byte*      bytesTop   ();
  void    setObjsBottom ( Address v );
  void    setObjsTop    ( Address v );
  void    setBytesBottom( Byte*   v );
  void    setBytesTop   ( Byte*   v );
  
  BPRef   allocateBytes( int nBytes );
  Address allocateOops ( int nOops  );
  
  void verifySpace( char* n, Verifier* aVerifier );

  bool isBytesPartAddressInBounds( Address bpAddr );
  bool isObjectAddressInBounds   ( Address   addr );
 private:
  void verifyName      ( char* n, Verifier* aVerifier );
  void verifyBoundaries(          Verifier* aVerifier );
  void verifyBytes     (          Verifier* aVerifier );
  void verifyObjs      (          Verifier* aVerifier );
  void verifySentinel  (          Verifier* aVerifier );
};

class EdenSpace : public Space {
 public:
  EdenSpace(Oop oop) : Space("eden", oop) {}
};

# endif // KLEIN_SPACE_H
