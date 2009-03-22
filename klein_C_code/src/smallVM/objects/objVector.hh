# ifndef OBJ_VECTOR_HH
# define OBJ_VECTOR_HH

# include "abstractVector.hh"


class ObjVectorObj: public AbstractVectorObj {
 public:
  static ObjVectorObj* from(oop_t x) { return (ObjVectorObj*) AbstractVectorObj::from(x); }
  
  fint   indexable_offset(fint i)  { return indexableOrigin() + i; } 
  oop_t* indexable_addr(fint i)    { return oop_addr( indexable_offset(i) ); } 
  oop_t* indexable_end()           { return indexable_addr(indexableSize()); }
  
  oop_t       indexable_at( fint i )           { return read_oop( indexable_offset(i)    ); }
  void  write_indexable_at( fint i, oop_t x )  {       write_oop( indexable_offset(i), x ); }
  
  oop_t clone();
  oop_t clone_and_resize( smi new_size, oop_t fill_if_not_badOop, ObjVectorObj** addrp = NULL, smi new_indexable_origin = 0 );

  fint total_size_in_oops() { return indexableOrigin() + indexableSize(); }
  
  static void print_objVector(oop_t x);
};


# endif // OBJ_VECTOR_HH
