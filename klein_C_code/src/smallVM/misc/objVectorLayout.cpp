# include "objVectorLayout.hh"
# include "headerFields.hh"
# include "byteVectorLayout.hh"
# include "bytesPartLayout.hh"
# include "maps.hh"
# include "verifier.hh"


AbstractHeaderField* ObjVectorLayout::lastField() {return indexableSizeField();}

int  ObjVectorLayout ::     indexableOriginOf( Oop o        ) { return indexableOriginField()->   valueFor( o,    this ); }
void ObjVectorLayout :: set_indexableOriginOf( Oop o, int n ) {        indexableOriginField()->setValueFor( o, n, this ); }

int  ObjVectorLayout ::     indexableSizeOf  ( Oop o        ) { return indexableSizeField()  ->   valueFor( o,    this ); }
void ObjVectorLayout :: set_indexableSizeOf  ( Oop o, int n ) {        indexableSizeField()  ->setValueFor( o, n, this ); }

Address ObjVectorLayout :: for_AddressOfIndexableAt( Oop o, int i )  {
  FailureCode c = boundsCheck(o, i);
  if ( c != SUCCEEDED )  return INVALID_ADDRESS;
  
  return for_AddressAt( o,  indexableOriginOf(o) + i );
}

Oop ObjVectorLayout::for_IndexableAt(Oop o, int i) {
  FailureCode c = boundsCheck(o, i);
  if ( c != SUCCEEDED )  return INVALID_OOP;

  return for_At( o, indexableOriginOf(o) + i );
}

FailureCode ObjVectorLayout :: for_IndexableAt_Put(Oop o, int i, Oop x) {
  FailureCode c = boundsCheck(o, i);
  if ( c != SUCCEEDED )  return c;

  for_At_Put( o, indexableOriginOf(o) + i, x );
  return SUCCEEDED;
}

bool ObjVectorLayout::isObjVector(Oop o) {
  return mapLayoutFor( mapOf(o) ) -> isVector();
}

int ObjVectorLayout :: verifyIndexableSizeOf  ( Oop o, Verifier* aVerifier )  { return indexableSizeField  ()->verifyObject( o, this, aVerifier ); }
int ObjVectorLayout :: verifyIndexableOriginOf( Oop o, Verifier* aVerifier )  { return indexableOriginField()->verifyObject( o, this, aVerifier ); }

void ObjVectorLayout :: verifyObject_UpTo_With( Oop mapOop, Oop o, int nextIndex, Verifier* aVerifier ) {
  // TODO: If o happens to be a map, maybe do extra verification?
  int s  = verifyIndexableSizeOf  (o, aVerifier);
  int io = verifyIndexableOriginOf(o, aVerifier);
  aVerifier->always_assert(io + s <= nextIndex, "indexables go past this object");
  verifyObjectSlotBounds(mapOop, emptyObjectSizeFor(o), io, aVerifier);
}


MapLayout* ObjVectorLayout :: mapLayoutFor(Oop mapOop) {
  // TODO: Put a pointer to a vtable in the map object, figure out at the right offset for a
  // pointer so that the vtable is in the place where the C++ compiler expects it, and then
  // do a simple dispatch.
  Oop   mapTypeOop  =        MapLayout().mapTypeOopOf(mapOop);
  BPRef bpRef       = ByteVectorLayout().bytesPartRefOf(mapTypeOop);
  Byte* mapType     =  BytesPartLayout().addressOfFirstByteInBytesPart(bpRef);
  int   mapTypeSize =  BytesPartLayout().     indexableSizeOfBytesPart(bpRef);

  # include "mapTypeTesters.incl.impl.hh"

  error("unknown map type");
  return NULL;
}
