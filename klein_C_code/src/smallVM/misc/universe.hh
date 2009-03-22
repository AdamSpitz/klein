# ifndef KLEIN_UNIVERSE_H
# define KLEIN_UNIVERSE_H

# include "kleinObject.hh"

class NewGeneration;
class Verifier;

class Universe : public KleinObject {
 public:
  Universe(Oop oop) : KleinObject(oop) {}
  
  NewGeneration* newGeneration();
  
  BPRef   allocateBytes(int nBytes);
  Address allocateOops (int nOops );
  
  bool isBytesPartAddressInBounds(Address bpAddr);
  bool isObjectAddressInBounds   (Address   addr);
  
  void verify(Verifier* aVerifier);
 private:
  void verifyCanonicalizedStrings(Verifier* aVerifier);
  void verifyGenerations         (Verifier* aVerifier);
};

# endif // KLEIN_UNIVERSE_H
