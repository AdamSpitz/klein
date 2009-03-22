# ifndef KLEIN_GENERATION_H
# define KLEIN_GENERATION_H

# include "kleinObject.hh"

class EdenSpace;
class Verifier;

class Generation : public KleinObject {
 public:
  Generation(Oop oop) : KleinObject(oop) {}
};

class NewGeneration : public Generation {
 public:
  NewGeneration(Oop oop) : Generation(oop) {}
    
  EdenSpace* edenSpace();
  
  BPRef   allocateBytes(int nBytes);
  Address allocateOops (int nOops );
  
  void verify(Verifier* aVerifier);
};

# endif // KLEIN_GENERATION_H
