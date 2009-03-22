# ifndef BLOCK_OBJ_HH
# define BLOCK_OBJ_HH

# include "object.hh"


class BlockObj : public MemObj {
  friend class LayoutConstantsGetter;

 protected:
  static const fint  homeFramePointer_offset =  MemObj::contents_offset;  
  static const fint          contents_offset =  homeFramePointer_offset + 1;

 public:
  static BlockObj* from(oop_t x) { return (BlockObj*) MemObj::from(x); }

  oop_t  homeFramePointer()    { return read_oop( homeFramePointer_offset ); }
  
  void  set_homeFramePointer( oop_t x )     { write_oop( homeFramePointer_offset, x); }

  oop_t clone(BlockObj** addrp = NULL) {
    assert(contents_offset == 3); 
    // todo optimization: return clone_3();
    return MemObj::clone( (MemObj**)addrp );
  }
  
  static oop_t clone_block(oop_t blk, oop_t home_activation) {
    BlockObj* r_addr;
    oop_t r = from(blk)->clone(&r_addr);
    r_addr->set_homeFramePointer(home_activation);
    assert(r_addr->oop() == r);
    return r;
  }
};


# endif

