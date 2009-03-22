# ifndef KLEIN_IMMEDIATE_LAYOUT_H
# define KLEIN_IMMEDIATE_LAYOUT_H

# include "objectLayout.hh"
# include "tag.hh"

class ImmediateLayout : public ObjectLayout {
 public:
  virtual TagValue myTag() {error("childShouldImplement"); return -1;}

  Oop encode     ( int v )  { return (v << Tag::width) | myTag(); }

  Oop oopForValue( int v )  { return encode(v); }

  bool hasMyTag( Oop o );

  void verifyObject_UpTo_With( Oop mapOop, Oop o, int nextIndex, Verifier* aVerifier );
};

class SmiLayout : public ImmediateLayout {
 public:
  virtual TagValue myTag()  { return Tag::smi; }

  int decode( Oop oop )  { return oop >> Tag::width; }

  int valueOf( Oop oop )  { return decode(oop); }
};

class FloatLayout : public ImmediateLayout {
 public:
  virtual TagValue myTag()  { return Tag::flt; }
};

# endif // KLEIN_IMMEDIATE_LAYOUT_H
