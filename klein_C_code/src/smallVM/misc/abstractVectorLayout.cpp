# include "abstractVectorLayout.hh"

bool AbstractVectorLayout :: isIndexOutOfBounds( Oop o, int i )  {
       return  !(  0 <= i  &&  i <  indexableSizeOf(o)  );
}
