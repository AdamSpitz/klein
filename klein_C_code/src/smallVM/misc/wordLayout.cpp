# include "wordLayout.hh"

FieldValue BitField :: allOnesValue() { return (1 << width()) - 1; }
BitCount   BitField :: shiftPast   () { return shift() + width(); }
intNN      BitField :: mask        () { return wordForValue( allOnesValue() ); }

intNN      BitField :: wordForValue(FieldValue v) { return v << shift(); }

intNN      BitField :: inPlaceValueOfWord(intNN w              ) { return w & mask(); }
FieldValue BitField ::        valueOfWord(intNN w              ) { return inPlaceValueOfWord(w) >> shift(); }
intNN      BitField ::     setValueOfWord(intNN w, FieldValue v) { return (w & (-1 - mask())) | wordForValue(v); }

bool       BitField ::  doesWordHaveValue(intNN w, FieldValue v) { return valueOfWord(w) == v; }

int        BitField :: encodableNumberCount()    { return (1 << width()) - 1; }
FieldValue BitField :: valueMeaningUnencodable() { return encodableNumberCount(); }
