# ifndef KLEIN_OBJECT_H
# define KLEIN_OBJECT_H

# include "base.hh"

class KleinObject {
 protected:
  Oop _oop;
 public:
  KleinObject(Oop oop) : _oop(oop) {}
  
  Oop               contentsOfSlotNamed( char* n        );
  int            smiContentsOfSlotNamed( char* n        );

  FailureCode    setContentsOfSlotNamed( char* n, Oop o );
  FailureCode setSmiContentsOfSlotNamed( char* n, int i );
};

# endif // KLEIN_OBJECT_H
