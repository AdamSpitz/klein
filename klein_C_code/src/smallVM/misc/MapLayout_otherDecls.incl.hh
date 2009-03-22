virtual ObjectLayout* myLayout() {
  error( "this is just an abstract map" ); 
  return NULL;
}


int slotDescSize();
int scalarValueCount();

int       sizeOf( Oop mapOop );

int  basicSizeOf( Oop mapOop );
Oop  basicAt(     Oop mapOop, int i );
void basicAt_Put( Oop mapOop, int i, Oop x );

Oop mapTypeOopOf( Oop mapOop);

int nameIndexAt( int i );
int typeIndexAt( int i );
int dataIndexAt( int i );

Oop      nameOopAt( Oop mapOop, int i );
Oop      typeOopAt( Oop mapOop, int i );
int      typeAt   ( Oop mapOop, int i ); // decode the smi
Oop      dataOopAt( Oop mapOop, int i );

void at_PutNameOop( Oop mapOop, int i, Oop v );
void at_PutTypeOop( Oop mapOop, int i, Oop v );
void at_PutDataOop( Oop mapOop, int i, Oop v );

int indexOfSlotWithSelector( Oop mapOop, StringOop desiredSlotNameOop );
int indexOfSlotNamed       ( Oop mapOop, char* n );

Oop            contentsOfSlotAt   ( Oop mapOop,  Oop obj,                    int   i );
Oop            contentsOfSlotNamed( Oop mapOop,  Oop obj,                    char* n );
FailureCode setContentsOfSlotAt   ( Oop mapOop,  Oop obj,  Oop newValueOop,  int   i );
FailureCode setContentsOfSlotNamed( Oop mapOop,  Oop obj,  Oop newValueOop,  char* n );

virtual void verifyObject( Oop mapOop, Oop o, int nextIndex, Verifier* aVerifier );
