# ifndef ABSTRACT_VECTOR_HH
# define ABSTRACT_VECTOR_HH

# include "object.hh"

class AbstractVectorObj: public MemObj {
  friend class LayoutConstantsGetter;

 protected:
  static const fint        indexableOrigin_offset =  MemObj::contents_offset;  
  static const fint          indexableSize_offset =  indexableOrigin_offset + 1;
  static const fint               contents_offset =  indexableSize_offset + 1;
  
 public:
  smi  indexableSize()    { return read_smi(   indexableSize_offset ); }
  smi  indexableOrigin()  { return read_smi( indexableOrigin_offset ); }
  
  oop_t  contents_oop(fint i)    { return read_oop( contents_offset + i ); }
  
  void  set_indexableSize(   smi x)     { write_smi(   indexableSize_offset,     x); }
  void  set_indexableOrigin( smi x)     { write_smi( indexableOrigin_offset,     x); }
  void  set_contents(fint i,  oop_t x)  { write_oop(        contents_offset + i, x); }
};

static const smi VectorPrintLimit = 20;


# endif // ABSTRACT_VECTOR_HH
