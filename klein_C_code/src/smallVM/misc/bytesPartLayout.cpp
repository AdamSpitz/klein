# include "bytesPartLayout.hh"
# include "immediateLayout.hh"
# include "universe.hh"
# include "theVM.hh"
# include "smallVMDeclarationsOrStubs.hh"


const int bytesPartIndexableSizeFieldOffsetInBytes = 0;
const int lastBytesHeaderWordOffset                = bytesPartIndexableSizeFieldOffsetInBytes;
const int bytesPartHeaderSizeInBytes               = (lastBytesHeaderWordOffset + 1) * oopSize;
const int     firstByteOffsetInBytes               = bytesPartHeaderSizeInBytes;

Address addressAtByteOffsetFromBytesPart(BPRef bpRef, int offsetInBytes) {
  return (Address) (((Byte*) bpRef) + offsetInBytes);
}

Address addressOfIndexableSizeFieldInBytesPart(BPRef bpRef) {
  return addressAtByteOffsetFromBytesPart(bpRef, bytesPartIndexableSizeFieldOffsetInBytes);
}

Byte* BytesPartLayout::addressOfFirstByteInBytesPart(BPRef bpRef) {
  return (Byte*) addressAtByteOffsetFromBytesPart(bpRef, firstByteOffsetInBytes);
}

int BytesPartLayout::indexableSizeOfBytesPart(BPRef bpRef) {
  Address indexableSizeAddress = addressOfIndexableSizeFieldInBytesPart(bpRef);
  return SmiLayout().valueOf(*indexableSizeAddress);
}

void BytesPartLayout::set_indexableSizeOfBytesPart(BPRef bpRef, int s) {
  Address indexableSizeAddress = addressOfIndexableSizeFieldInBytesPart(bpRef);
  *indexableSizeAddress = SmiLayout().oopForValue(s);
}

BPRef BytesPartLayout::allocateBytesPartWithIndexableSize(int nBytes) {
  BPRef bpRef = theVM()->universe()->allocateBytes(nBytes + bytesPartHeaderSizeInBytes);
  set_indexableSizeOfBytesPart(bpRef, nBytes);
  return bpRef;
}

bool isByteIndexOutOfBounds(BPRef bpRef, int i) {
  return (i < 0)  ||  (i >= BytesPartLayout().indexableSizeOfBytesPart(bpRef));
}

void bytesPartBoundsCheck(BPRef bpRef, int i) {
  if (isByteIndexOutOfBounds(bpRef, i)) {throwException("bytes part index out of bounds: %i");}
}	

Byte* forBytesPart_UncheckedAddressAt(BPRef bpRef, int i) {
  return (Byte*) addressAtByteOffsetFromBytesPart(bpRef, firstByteOffsetInBytes + i);
}

Byte* BytesPartLayout::forBytesPart_AddressAt(BPRef bpRef, int i) {
  bytesPartBoundsCheck(bpRef, i);
  return forBytesPart_UncheckedAddressAt(bpRef, i);
}

Byte BytesPartLayout::forBytesPart_At(BPRef bpRef, int i) {
  return *(forBytesPart_AddressAt(bpRef, i));
}

void BytesPartLayout::forBytesPart_At_Put(BPRef bpRef, int i, Byte b) {
  *(forBytesPart_AddressAt(bpRef, i)) = b;
}

BPRef BytesPartLayout::nextBytesPartAfter(BPRef bpRef) {
  int s = indexableSizeOfBytesPart(bpRef);
  s = roundUpTo(s, oopSize);
  return (BPRef) forBytesPart_UncheckedAddressAt(bpRef, s);
}
