# include "markLayout.hh"
# include "wordLayout.hh"

# include "markFields.incl.hh"

static       BitField* mark_isByteVectorField             = new       BitField(mark_isByteVectorField_width,             mark_isByteVectorField_shift  );
static NumberBitField* mark_compactMapIndexField          = new NumberBitField(mark_compactMapIndexField_width,          mark_compactMapIndexField_shift,          mark_compactMapIndexField_lowestEncodableNumber         );
static NumberBitField* mark_objVectorIndexableOriginField = new NumberBitField(mark_objVectorIndexableOriginField_width, mark_objVectorIndexableOriginField_shift, mark_objVectorIndexableOriginField_lowestEncodableNumber);
static NumberBitField* mark_objVectorIndexableSizeField   = new NumberBitField(mark_objVectorIndexableSizeField_width,   mark_objVectorIndexableSizeField_shift,   mark_objVectorIndexableSizeField_lowestEncodableNumber  );
static       BitField* mark_hasBeenVisitedField           = new       BitField(mark_hasBeenVisitedField_width,           mark_hasBeenVisitedField_shift);
static       BitField* mark_hashField                     = new       BitField(mark_hashField_width,                     mark_hashField_shift          );

BitField*       MarkLayout ::             isByteVectorField() { return mark_isByteVectorField; }
NumberBitField* MarkLayout ::          compactMapIndexField() { return mark_compactMapIndexField; }
NumberBitField* MarkLayout :: objVectorIndexableOriginField() { return mark_objVectorIndexableOriginField; }
NumberBitField* MarkLayout ::   objVectorIndexableSizeField() { return mark_objVectorIndexableSizeField; }
BitField*       MarkLayout ::           hasBeenVisitedField() { return mark_hasBeenVisitedField; }
BitField*       MarkLayout ::                     hashField() { return mark_hashField; }

Oop MarkLayout::trailingMark() {return oopForValue(0);}

const HashValue    noHashValue = 0;
const HashValue firstHashValue = 1;

HashValue MarkLayout::hashOfMarkValue(MarkValue mv) {
  return mark_hashField->valueOfWord(mv);
}

MarkValue MarkLayout::set_hashOfMarkValue(MarkValue mv, HashValue h) {
  HashValue newHashValue = (h == noHashValue) ? firstHashValue : h;
  MarkValue newMarkValue = mark_hashField->setValueOfWord(mv, newHashValue);
  ASSERT(hashOfMarkValue(newMarkValue) == newHashValue);
  return newMarkValue;
}

bool MarkLayout::isMarkValueForByteVector(MarkValue mv) {
  return mark_isByteVectorField->doesWordHaveValue(mv, 1);
}
