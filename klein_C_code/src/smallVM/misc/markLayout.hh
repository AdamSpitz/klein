# ifndef KLEIN_MARK_LAYOUT_H
# define KLEIN_MARK_LAYOUT_H

# include "base.hh"
# include "immediateLayout.hh"

typedef int HashValue;

class BitField;
class NumberBitField;

class MarkLayout : public ImmediateLayout {
 public:
  virtual TagValue myTag() { return Tag::mark; }

  MarkValue decode ( Oop oop )  { return oop >> Tag::width; }

  MarkValue valueOf( Oop oop )  { return decode(oop); }

  Oop trailingMark();

  HashValue     hashOfMarkValue( MarkValue mv              );
  MarkValue set_hashOfMarkValue( MarkValue mv, HashValue h );

  bool isMarkForByteVector     ( Oop       oop )  {  return isMarkValueForByteVector( valueOf(oop) );  }
  bool isMarkValueForByteVector( MarkValue mv  );
  
        BitField*             isByteVectorField();
  NumberBitField*          compactMapIndexField();
  NumberBitField* objVectorIndexableOriginField();
  NumberBitField*   objVectorIndexableSizeField();
        BitField*           hasBeenVisitedField();
        BitField*                     hashField();
};

# endif
