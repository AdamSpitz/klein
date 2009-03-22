# ifndef KLEIN_OBJECT_LAYOUT_H
# define KLEIN_OBJECT_LAYOUT_H

# include "layout.hh"

class Verifier;

class ObjectLayout : public AbstractLayout {
 public:
  Oop mapOf( Oop o );

  virtual void verifyObject_UpTo_With( Oop mapOop, Oop o, int nextIndex, Verifier* aVerifier )  =  0;
};

# endif // KLEIN_OBJECT_LAYOUT_H
