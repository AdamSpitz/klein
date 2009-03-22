Oop    codesOf( Oop o )  { return contentsOfSlotInObject( o, "_codes"    ); }
Oop literalsOf( Oop o )  { return contentsOfSlotInObject( o, "_literals" ); }

Oop enclosingMapOopOf( Oop o ) {
  //TODO: I bet this is wrong. I'm confused.
  error("TODO: enclosingMapOopOf(o)");
  return mapOf(o);
}