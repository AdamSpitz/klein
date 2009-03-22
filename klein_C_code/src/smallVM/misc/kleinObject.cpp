# include "kleinObject.hh"
# include "maps.hh"
# include "immediateLayout.hh"


Oop KleinObject::contentsOfSlotNamed( char* n ) {
  return MemoryObjectLayout().contentsOfSlotInObject( _oop, n );
}

int KleinObject::smiContentsOfSlotNamed( char* n ) {
  // TODO: This method does not properly handle the case
  // of an improper oop. Need to fix to handle illegal oops.
  return SmiLayout().valueOf( contentsOfSlotNamed(n) );
}

FailureCode KleinObject::setContentsOfSlotNamed( char* n, Oop o ) {
  return MemoryObjectLayout().setContentsOfSlotInObject( _oop, o, n );
}

FailureCode KleinObject::setSmiContentsOfSlotNamed( char* n, int i ) {
  return setContentsOfSlotNamed( n, SmiLayout().oopForValue(i) );
}
