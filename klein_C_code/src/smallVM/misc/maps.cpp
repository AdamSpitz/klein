# include "maps.hh"

# include "allNonMapLayouts.hh"
# include "verifier.hh"
# include "slotType.hh"

# include "maps.incl.impl.hh"

int  MapLayout::slotDescSize    () {return 3;} //TODO: generate this from Self
int  MapLayout::scalarValueCount() {return 2;} //TODO: generate this from Self
Oop  MapLayout::basicAt    (Oop mapOop, int i       ) {return ObjVectorLayout().for_IndexableAt(mapOop, i);}
void MapLayout::basicAt_Put(Oop mapOop, int i, Oop x) {ObjVectorLayout().for_IndexableAt_Put(mapOop, i, x);}
int  MapLayout::basicSizeOf(Oop mapOop) {return ObjVectorLayout().indexableSizeOf(mapOop);}
int  MapLayout::     sizeOf(Oop mapOop) {return (basicSizeOf(mapOop) - scalarValueCount()) / slotDescSize();}

const static int mapTypeIndex = 0; //TODO: generate this from Self
Oop MapLayout::mapTypeOopOf(Oop mapOop) {return for_IndexableAt(mapOop, mapTypeIndex);}

//TODO: generate this from Self?
int MapLayout::nameIndexAt(int i) {return scalarValueCount() + (i * slotDescSize())    ;}
int MapLayout::typeIndexAt(int i) {return scalarValueCount() + (i * slotDescSize()) + 1;}
int MapLayout::dataIndexAt(int i) {return scalarValueCount() + (i * slotDescSize()) + 2;}

Oop MapLayout::nameOopAt(Oop mapOop, int i) {return basicAt(mapOop, nameIndexAt(i));}
Oop MapLayout::typeOopAt(Oop mapOop, int i) {return basicAt(mapOop, typeIndexAt(i));}
Oop MapLayout::dataOopAt(Oop mapOop, int i) {return basicAt(mapOop, dataIndexAt(i));}

int MapLayout::typeAt(Oop mapOop, int i) { return SmiLayout().valueOf(typeOopAt(mapOop, i)); }

void MapLayout::at_PutNameOop(Oop mapOop, int i, Oop v) {basicAt_Put(mapOop, nameIndexAt(i), v);}
void MapLayout::at_PutTypeOop(Oop mapOop, int i, Oop v) {basicAt_Put(mapOop, typeIndexAt(i), v);}
void MapLayout::at_PutDataOop(Oop mapOop, int i, Oop v) {basicAt_Put(mapOop, dataIndexAt(i), v);}

int MapLayout::indexOfSlotWithSelector(Oop mapOop, StringOop desiredSlotNameOop) {
  if (mapOop == 0) return INVALID_OOP;
  int s = sizeOf(mapOop);
  for (int i = 0; i < s; i++) {
    Oop slotNameOop = nameOopAt(mapOop, i);
    if (desiredSlotNameOop == slotNameOop) {return i;}
  }
  return INVALID_MAP_INDEX;
}

int MapLayout::indexOfSlotNamed(Oop mapOop, char* n) {
  if (mapOop == 0) return INVALID_OOP;
  int s = sizeOf(mapOop);
  int nSize = lengthOfCString(n);
  for (int i = 0; i < s; i++) {
    Oop slotNameOop = nameOopAt(mapOop, i);
    if (isCStringEqualToKleinString(nSize, n, slotNameOop)) {return i;}
  }
  return INVALID_MAP_INDEX;
}

Oop MapLayout::contentsOfSlotAt(Oop mapOop, Oop oop, int i) {
  if (i == INVALID_MAP_INDEX) return INVALID_OOP;
  Oop mapData = dataOopAt(mapOop, i);
  if (! isObjectSlot(typeAt(mapOop, i))) {
    return mapData;
  } else {
    return MemoryObjectLayout().for_At(oop, SmiLayout().valueOf(mapData));
  }
}

Oop MapLayout::contentsOfSlotNamed(Oop mapOop, Oop oop, char* n) {
  int i = indexOfSlotNamed(mapOop, n);
  return contentsOfSlotAt(mapOop, oop, i);
}

FailureCode MapLayout::setContentsOfSlotAt(Oop mapOop, Oop oop, Oop newValueOop, int i) {
  if (i == INVALID_MAP_INDEX) return FAILED;
  if (! isObjectSlot(typeAt(mapOop, i))) {
    at_PutDataOop(mapOop, i, newValueOop);
  } else {
    MemoryObjectLayout().for_At_Put(oop, SmiLayout().valueOf(dataOopAt(mapOop, i)), newValueOop);
  }
  return SUCCEEDED;
}

FailureCode MapLayout::setContentsOfSlotNamed(Oop mapOop, Oop oop, Oop newValueOop, char* n) {
  int i = indexOfSlotNamed(mapOop, n);
  return setContentsOfSlotAt(mapOop, oop, newValueOop, i);
}

void MapLayout::verifyObject(Oop mapOop, Oop o, int nextIndex, Verifier* aVerifier) {
  myLayout()->verifyObject_UpTo_With(mapOop, o, nextIndex, aVerifier);
}
