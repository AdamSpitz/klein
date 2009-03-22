# include "space.hh"
# include "verifier.hh"
# include "bytesPartLayout.hh"
# include "memoryObjectLayout.hh"

void Space::verifySpace( char* n, Verifier* aVerifier ) {
  verifyName      (n, aVerifier);
  verifyBoundaries(   aVerifier);
  verifyBytes     (   aVerifier);
  verifyObjs      (   aVerifier);
  verifySentinel  (   aVerifier);
}

Address Space::   objsBottom () {return (Address) smiContentsOfSlotNamed( "objsBottom"  ); }
Address Space::   objsTop    () {return (Address) smiContentsOfSlotNamed( "objsTop"     ); }
Byte*   Space::   bytesBottom() {return (Byte*)   smiContentsOfSlotNamed( "bytesBottom" ); }
Byte*   Space::   bytesTop   () {return (Byte*)   smiContentsOfSlotNamed( "bytesTop"    ); }

void    Space::setObjsBottom ( Address v ) {   setSmiContentsOfSlotNamed( "objsBottom"  , (int) v ); }
void    Space::setObjsTop    ( Address v ) {   setSmiContentsOfSlotNamed( "objsTop"     , (int) v ); }
void    Space::setBytesBottom( Byte*   v ) {   setSmiContentsOfSlotNamed( "bytesBottom" , (int) v ); }
void    Space::setBytesTop   ( Byte*   v ) {   setSmiContentsOfSlotNamed( "bytesTop"    , (int) v ); }

void Space::verifyName(char* n, Verifier* aVerifier) {
  aVerifier->always_assert(areCStringsEqual(_name, n), "space has wrong name");
}

void Space::verifyBoundaries(Verifier* aVerifier) {
  aVerifier->always_assert(objsBottom() <= objsTop() && objsTop() <= ((Address) bytesBottom()) && bytesBottom() <= bytesTop(), "space bounds");
}

void Space::verifyBytes(Verifier* aVerifier) {
  BPRef bp = (BPRef) bytesBottom();
  BPRef bt = (BPRef) bytesTop   ();
  while (bp < bt) {
    bp = BytesPartLayout().nextBytesPartAfter(bp);
  }
  aVerifier->always_assert(bp == bt, "bytes part of space");
}

void Space::verifyObjs(Verifier* aVerifier) {
  Oop ob = MemoryObjectLayout().memForAddress((Address) objsBottom());
  Oop ot = MemoryObjectLayout().memForAddress((Address) objsTop   ());
  Oop o = ob;
  Oop last = 0, last1 = 0, last2 = 0;
  while (o < ot) {
    last2 = last1;  last1 = last; last = o;
    o = MemoryObjectLayout().verifyContentsOfOop(o, aVerifier);
  }
  aVerifier->always_assert(o == ot, "last object");
}

void Space::verifySentinel(Verifier* aVerifier) {
  Oop s = * ((Oop*) objsTop());
  aVerifier->always_assert( Tag::tagOfOop(s) == Tag::mark,  "not a sentinel" );
}

bool Space::isBytesPartAddressInBounds(Address bpAddr) {
  // TODO: It bothers me that we're doing all these stupid casts between Oop* and Byte* and so on.
  return bytesBottom() <= ((Byte*) bpAddr) && ((Byte*) bpAddr) < bytesTop();
}

bool Space::isObjectAddressInBounds(Address addr) {
  return objsBottom() <= addr && addr < objsTop();
}

BPRef Space::allocateBytes(int nBytes) {
  Byte* newBytesBottom = ((Byte*) bytesBottom()) - roundUpTo(nBytes, oopSize);
  
  // Check for out of memory: using '<=' to reserve space for the mark following
  // the last word in the space.
  if (newBytesBottom <= ((Byte*) objsTop())) {
    error("space full");
  }
  
  setBytesBottom(newBytesBottom);
  return (BPRef) newBytesBottom;
}

Address Space::allocateOops(int nOops) {
  Address oldObjsTop = (Address) objsTop();
  Address newObjsTop = oldObjsTop + nOops;
  
  // Check for out of memory: using '<=' to reserve space for the mark following
  // the last word in the space.
  if (bytesBottom() <= ((Byte*) newObjsTop)) {
    error("space full");
  }
  
  setObjsTop(newObjsTop);
  return newObjsTop;
}

oop Space::clone( oop original ) {
  oop mapOop = MemoryObjectLayout().mapOf(original);
  int oldBytesBottom = bytesBottom;
  int oldOopsTop     = oopsTop;
  allocateOops (oriMap -> oopSize);
  allocateBytes(oriMap -> byteSize);
  copyOopPart  (oriMap -> oopPart()  , oldOopsTop    , oopsTop    );
  copyBytesPart(oriMap -> bytesPart(), oldBytesBottom, bytesBottom);
  Map* newMap = mapForOop(oldOopsTop);
  newMap -> adjustForNewOop       (oldOopsTop);
  newMap -> adjustForNewBytesPart (oldBytesBottom);
  return oldOopsTop;
}