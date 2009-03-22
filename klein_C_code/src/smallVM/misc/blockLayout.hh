# ifndef KLEIN_BLOCK_LAYOUT_H
# define KLEIN_BLOCK_LAYOUT_H

# include "memoryObjectLayout.hh"

class BlockLayout :  public MemoryObjectLayout {
 public:

  intNN     homeFramePointerOf( Oop o           );
  void  set_homeFramePointerOf( Oop o, intNN fp );
  void  zap_homeFramePointerOf( Oop o           );
	
          OopValueHeaderField*  homeFramePointerField();
  virtual AbstractHeaderField*              lastField();
};


# endif // KLEIN_BLOCK_LAYOUT_H
