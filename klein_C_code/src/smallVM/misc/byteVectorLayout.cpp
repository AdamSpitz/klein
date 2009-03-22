# include "byteVectorLayout.hh"
# include "headerFields.hh"
# include "bytesPartLayout.hh"
# include "verifier.hh"
# include "universe.hh"
# include "theVM.hh"
# include "immediateLayout.hh"
# include "markLayout.hh"
# include "smallVMDeclarationsOrStubs.hh"

AbstractHeaderField* ByteVectorLayout::lastField() { return bytesPartRefField(); }

BPRef ByteVectorLayout ::     bytesPartRefOf( Oop o              ) { return bytesPartRefField()->   valueFor(o,        this); }
void  ByteVectorLayout :: set_bytesPartRefOf( Oop o, BPRef bpRef ) {        bytesPartRefField()->setValueFor(o, bpRef, this); }

int  ByteVectorLayout ::     indexableSizeOf(Oop o       ) { return BytesPartLayout().    indexableSizeOfBytesPart(bytesPartRefOf(o)   ); }
void ByteVectorLayout :: set_indexableSizeOf(Oop o, int n) {        BytesPartLayout().set_indexableSizeOfBytesPart(bytesPartRefOf(o), n); }

Byte* ByteVectorLayout :: for_AddressOfIndexableAt(Oop o, int i        ) { return BytesPartLayout().forBytesPart_AddressAt( bytesPartRefOf(o), i    ); }
Byte  ByteVectorLayout :: for_IndexableAt         (Oop o, int i        ) { return BytesPartLayout().forBytesPart_At       ( bytesPartRefOf(o), i    ); }
void  ByteVectorLayout :: for_IndexableAt_Put     (Oop o, int i, Byte b) {        BytesPartLayout().forBytesPart_At_Put   ( bytesPartRefOf(o), i, b ); }

bool ByteVectorLayout :: isByteVector(Oop o) {
  return MarkLayout().isMarkValueForByteVector( markValueOf(o) );
}

bool ByteVectorLayout :: isString    (Oop o)  {
  error("TODO: isString(o): should this be different? Check the map, maybe?");
  return isByteVector(o);
}

void ByteVectorLayout :: verifyBytesPartRefOf(Oop o, Verifier* aVerifier) {
  Oop bpRefAsSmi = SmiLayout().oopForValue( for_VerifySmiAt_With(o, bytesPartRefField()->indexFor(o, this), aVerifier) );
  aVerifier->always_assert(theVM()->universe()->isBytesPartAddressInBounds( (Address) bpRefAsSmi ),  "bad bytes part ref");
}

void ByteVectorLayout :: verifyObject_UpTo_With(Oop mapOop, Oop o, int nextIndex, Verifier* aVerifier) {
  verifyBytesPartRefOf( o, aVerifier );
  AbstractVectorLayout :: verifyObject_UpTo_With(mapOop, o, emptyObjectSizeFor(o), aVerifier);
  //TODO: if it's a canonical string, check it against the canonicalized strings table?
}

int ByteVectorLayout :: argCountOf(Oop o) {
  int len = indexableSizeOf(o);
  always_assert( len > 0,  "should have a positive length" );
  Byte* s = BytesPartLayout().addressOfFirstByteInBytesPart( bytesPartRefOf(o) );
  return argCountOfString(s, len);
}

Oop ByteVectorLayout :: newString(char* s) {
  error("newString not implemented yet");
  return INVALID_OOP;
}
