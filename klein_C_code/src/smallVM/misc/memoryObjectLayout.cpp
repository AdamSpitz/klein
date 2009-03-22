# include "memoryObjectLayout.hh"
# include "immediateLayout.hh"
# include "headerFields.hh"
# include "verifier.hh"
# include "maps.hh"
# include "universe.hh"
# include "space.hh"
# include "theVM.hh"
# include "markLayout.hh"
# include "slotType.hh"


Address   MemoryObjectLayout :: for_AddressAt      ( Oop o, int i               ) { return addressOfMem(o) + i; }
Oop       MemoryObjectLayout :: for_At             ( Oop o, int i               ) { return *((Oop*) for_AddressAt(o, i))    ; }
void      MemoryObjectLayout :: for_At_Put         ( Oop o, int i, Oop       x  ) {        *((Oop*) for_AddressAt(o, i)) = x; }

int       MemoryObjectLayout :: for_SmiValueAt     ( Oop o, int i               ) { return SmiLayout().valueOf(for_At(o, i));     }
void      MemoryObjectLayout :: for_At_PutSmiValue ( Oop o, int i, int       x  ) { for_At_Put(o, i, SmiLayout().oopForValue(x)); }

MarkValue MemoryObjectLayout :: for_MarkValueAt    ( Oop o, int i               ) { return MarkLayout().valueOf(for_At(o, i));      }
void      MemoryObjectLayout :: for_At_PutMarkValue( Oop o, int i, MarkValue mv ) { for_At_Put(o, i, MarkLayout().oopForValue(mv)); }


# include "headerFields.incl.hh"

AbstractHeaderField* MemoryObjectLayout :: lastHeaderField() { return memoryObject_mapField; }
AbstractHeaderField* MemoryObjectLayout :: lastField      () { return lastHeaderField(); }

int MemoryObjectLayout :: emptyObjectSizeFor( Oop o ) { return lastField()->indexAfterMeFor(o, this); }

// TODO: Get this stuff into blockLayout.cpp or wherever it belongs.
# include "blockLayout.hh"
OopValueHeaderField*      BlockLayout::homeFramePointerField() { return block_homeFramePointerField   ; }
# include "objVectorLayout.hh"
SmiValueHeaderField*  ObjVectorLayout:: indexableOriginField() { return objVector_indexableOriginField; }
SmiValueHeaderField*  ObjVectorLayout::   indexableSizeField() { return objVector_indexableSizeField  ; }
# include "byteVectorLayout.hh"
OopValueHeaderField* ByteVectorLayout::    bytesPartRefField() { return byteVector_bytesPartRefField  ; }


MarkValue MemoryObjectLayout ::    markValueOf( Oop o                   ) { return memoryObject_markField->   valueFor(o,         this); }
void      MemoryObjectLayout :: setMarkValueOf( Oop o, MarkValue mv     ) {        memoryObject_markField->setValueFor(o, mv,     this); }

int       MemoryObjectLayout ::          oidOf( Oop o                   ) { return memoryObject_oidField ->   valueFor(o,         this); }
void      MemoryObjectLayout ::       setOIDOf( Oop o, int       oid    ) {        memoryObject_oidField ->setValueFor(o, oid,    this); }

Oop       MemoryObjectLayout ::          mapOf( Oop o                   ) { Oop moop = memoryObject_mapField ->   valueFor(o, this); 
                                                                            ASSERT( moop != 0 );
                                                                            return moop;                                                 }
void      MemoryObjectLayout ::       setMapOf( Oop o, Oop       mapOop ) {        memoryObject_mapField ->setValueFor(o, mapOop, this); }


Oop         MemoryObjectLayout ::    contentsOfSlotInObject( Oop oop,                  char* n ) { return MapLayout().   contentsOfSlotNamed( mapOf(oop), oop,              n ); }
FailureCode MemoryObjectLayout :: setContentsOfSlotInObject( Oop oop, Oop newValueOop, char* n ) { return MapLayout().setContentsOfSlotNamed( mapOf(oop), oop, newValueOop, n ); }


int MemoryObjectLayout :: verifyOopsOf( Oop o, Verifier* aVerifier ) {
  ASSERT( memoryObject_markField->isFirst() );
  Oop* originalAddr = decode(o);
  Oop* addr = originalAddr;
  while (true) {
    ++addr;
    Oop oop = *addr;
    TagValue t = Tag::tagOfOop(oop);
    if (t == Tag::mark) break;
    if (t == Tag::mem ) {
      Address addr = MemoryObjectLayout().addressOfMem(oop);
      aVerifier->always_assert(theVM()->universe()->isObjectAddressInBounds(addr), "mem oop does not point inside object space");
      Oop* targetAddr = decode(oop);
      TagValue targetTag = Tag::tagOfOop(*targetAddr);
      aVerifier->always_assert(targetTag == Tag::mark, "mem oop does not point to a mark");
    }
  }
  return addr - originalAddr; //TODO: did I get the pointer arithmetic right here?
}

Oop MemoryObjectLayout :: verifyContentsOfOop( Oop o, Verifier* aVerifier ) {
  int nextIndex = verifyOopsOf(o, aVerifier);
  Oop mapOop = MemoryObjectLayout().mapOf(o);
  MapLayout* mapLayout = ObjVectorLayout().mapLayoutFor(mapOop);
  MarkValue mv;
  //TODO: fix this verifier failblock thingy.
  try {mv = ((MemoryObjectLayout*) (mapLayout->myLayout()))->markValueOf(o);} catch (...) {mv = 0;}
  //TODO: put this back in in a way that works.
  //bool markIsBV = MarkLayout().isMarkValueForByteVector(mv);
  //bool  mapIsBV = mapLayout->isByteVector();
  //aVerifier->always_assert(markIsBV == mapIsBV, "byte vector bit in mark is wrong");
  mapLayout->verifyObject(mapOop, o, nextIndex, aVerifier);
  return memForAddress(for_AddressAt(o, nextIndex));
}

int MemoryObjectLayout :: for_VerifySmiAt_With( Oop o, int i, Verifier* aVerifier ) {
  Oop oop = for_At(o, i);
   
  aVerifier->always_assert( Tag::tagOfOop(oop) == Tag::smi, "not a smi" );
  return SmiLayout().valueOf(oop);
}

void MemoryObjectLayout :: verifyObject_UpTo_With( Oop mapOop, Oop o, int nextIndex, Verifier* aVerifier ) {
  verifyObjectSlotBounds( mapOop, emptyObjectSizeFor(o), nextIndex, aVerifier );
}

void MemoryObjectLayout :: verifyObjectSlotBounds( Oop mapOop, int lowest, int pastHighest, Verifier* aVerifier ) {
  for (int i = 0, n = MapLayout().sizeOf(mapOop); i < n; i++) {
    if (isObjectSlot( MapLayout().typeAt(mapOop, i))) {
      int dataOffset = SmiLayout().valueOf( MapLayout().dataOopAt(mapOop, i));
      aVerifier->always_assert(lowest     <= dataOffset , "slot offset is too small");
      aVerifier->always_assert(dataOffset <  pastHighest, "slot offset is too big"  );
    }
  }
}
