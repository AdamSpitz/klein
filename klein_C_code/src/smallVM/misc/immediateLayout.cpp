# include "immediateLayout.hh"

void ImmediateLayout :: verifyObject_UpTo_With( Oop mapOop, Oop o, int nextIndex, Verifier* aVerifier )  {
  error("should never get here");
}

bool ImmediateLayout :: hasMyTag( Oop o )  {
  return Tag::tagOfOop(o) == myTag();
}
