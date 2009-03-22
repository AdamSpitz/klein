# ifndef KLEIN_WORD_LAYOUT_H
# define KLEIN_WORD_LAYOUT_H

# include "base.hh"

typedef int BitCount;
typedef intNN FieldValue;

class BitField {
 protected:
  BitCount _width, _shift;
 public:
  BitField( BitCount w, BitCount s ) : _width(w), _shift(s) {}

  BitCount width    () { return _width; }
  BitCount shift    () { return _shift; }
  BitCount shiftPast();
  intNN    mask     ();

  FieldValue allOnesValue();

  intNN wordForValue( FieldValue v );

  intNN       inPlaceValueOfWord( intNN w               );
  FieldValue         valueOfWord( intNN w               );
  intNN           setValueOfWord( intNN w, FieldValue v );

  bool         doesWordHaveValue( intNN w, FieldValue v );

  int        encodableNumberCount   ();
  FieldValue valueMeaningUnencodable();
  intNN       wordMeaningUnencodable() { return wordForValue( valueMeaningUnencodable() ); }
};

class NumberBitField : public BitField {
 protected:
  int _lowestEncodableNumber;
 public:
  NumberBitField( BitCount w, BitCount s, int n ) : BitField(w, s), _lowestEncodableNumber(n) {}
  
  int numberForValue( FieldValue v )  { return _lowestEncodableNumber + v; }
};

# endif // KLEIN_WORD_LAYOUT_H
