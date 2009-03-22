# include "headerFields.hh"
# include "memoryObjectLayout.hh"
# include "wordLayout.hh"
# include "immediateLayout.hh"

# define VALUE_ACCESSORS( className, valueType, basicGetter, basicSetter )                             \
valueType className :: valueFor( Oop o, MemoryObjectLayout* aLayout )  {                              \
  if ( _markField != NULL ) {                                                                         \
    MarkValue  mv = aLayout->markValueOf(o);                                                          \
    FieldValue fv = _markField->valueOfWord(mv);                                                      \
    if ( fv != _markField->valueMeaningUnencodable() ) {                                              \
      int n = _markField->numberForValue(fv);                                                         \
      return valueForEncodedNumberInMark(n);                                                          \
    }                                                                                                 \
  }                                                                                                   \
  return aLayout-> basicGetter (o, indexFor(o, aLayout));                                             \
}                                                                                                     \
                                                                                                      \
void className :: setValueFor( Oop o, valueType v, MemoryObjectLayout* aLayout )  {                   \
  if ( _markField != NULL ) {                                                                         \
    MarkValue  mv = aLayout->markValueOf(o);                                                          \
    FieldValue fv = _markField->valueOfWord(mv);                                                      \
    if ( fv != _markField->valueMeaningUnencodable() ) {                                              \
      error( "this is difficult - we would have to create a whole new object with the extra field" ); \
    }                                                                                                 \
  }                                                                                                   \
  return aLayout-> basicSetter (o, indexFor(o, aLayout), v);                                          \
}

VALUE_ACCESSORS(  OopValueHeaderField, Oop      , for_At         , for_At_Put          )
VALUE_ACCESSORS(  SmiValueHeaderField, int      , for_SmiValueAt , for_At_PutSmiValue  )
VALUE_ACCESSORS( MarkValueHeaderField, MarkValue, for_MarkValueAt, for_At_PutMarkValue )

Oop OopValueHeaderField::valueForEncodedNumberInMark(int n) { return SmiLayout().encode(n); }

Oop MapHeaderField::valueForEncodedNumberInMark(int n) {
  error("not implemented yet, but should look up the map at that index in the compact map table");
  return 0;
}

int AbstractHeaderField::indexFor(Oop o, MemoryObjectLayout* aLayout) {
  if (_precedingField == NULL) {return 0;}
  int i = _precedingField->indexAfterMeFor(o, aLayout);
  return i;
}

int AbstractHeaderField::indexAfterMeFor(Oop o, MemoryObjectLayout* aLayout) {
  return indexFor(o, aLayout) + (isIncludedIn(o, aLayout) ? 1 : 0);
}

bool AbstractHeaderField::isIncludedIn(Oop o, MemoryObjectLayout* aLayout) {
  if (_markField != NULL) {
    MarkValue mv = aLayout->markValueOf(o);
    FieldValue fv = _markField->valueOfWord(mv);
    if (fv != _markField->valueMeaningUnencodable()) {
      return false;
    }
  }
  return true;
}

int SmiValueHeaderField::verifyObject(Oop o, MemoryObjectLayout* aLayout, Verifier* aVerifier) {
  if (_markField != NULL) {
    MarkValue mv = aLayout->markValueOf(o);
    FieldValue fv = _markField->valueOfWord(mv);
    if (fv != _markField->valueMeaningUnencodable()) {
      int n = _markField->numberForValue(fv);
      return valueForEncodedNumberInMark(n);
    }
  }
  return aLayout->for_VerifySmiAt_With( o, indexFor(o, aLayout), aVerifier );
}
