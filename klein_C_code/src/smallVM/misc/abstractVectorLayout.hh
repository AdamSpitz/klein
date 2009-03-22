# ifndef KLEIN_ABSTRACT_VECTOR_LAYOUT_H
# define KLEIN_ABSTRACT_VECTOR_LAYOUT_H

# include "memoryObjectLayout.hh"

class AbstractVectorLayout : public MemoryObjectLayout {
 public:
  virtual int  indexableSizeOf( Oop o )  =  0;

          bool isIndexOutOfBounds( Oop o, int i );

          FailureCode boundsCheck( Oop o, int i )  {
            return isIndexOutOfBounds(o, i) ? FAILED : SUCCEEDED; 
          }
};

# endif // KLEIN_ABSTRACT_VECTOR_LAYOUT_H
