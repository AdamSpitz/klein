# include "blockLayout.hh"
# include "immediateLayout.hh"
# include "headerFields.hh"

intNN BlockLayout ::     homeFramePointerOf( Oop o           )  { return homeFramePointerField()->   valueFor( o,     this ); }
void  BlockLayout :: set_homeFramePointerOf( Oop o, intNN fp )  {        homeFramePointerField()->setValueFor( o, fp, this ); }

void  BlockLayout :: zap_homeFramePointerOf( Oop o           )  { set_homeFramePointerOf( o, SmiLayout().encode(0) ); }

AbstractHeaderField* BlockLayout :: lastField()  { return homeFramePointerField(); }
