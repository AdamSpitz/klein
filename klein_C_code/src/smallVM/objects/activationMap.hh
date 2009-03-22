# ifndef ACTIVATION_MAP_HH
# define ACTIVATION_MAP_HH

# include "map.hh"


class ActivationMapObj : public MapObj {
 friend class LayoutConstantsGetter;

 public:
  static ActivationMapObj* from(oop_t x)  { return (ActivationMapObj*) MapObj::from(x); }
  static fint starting_pc()  { return 1; } // todo cleanup magic number dmu 1/06

  oop_t literals()                 { return                 read_oop( expectedMapObjOffsetForLiterals            ) ; }
  oop_t codes()                    { return                 read_oop( expectedMapObjOffsetForCodes               ) ; }
  fint  get_activationPartSizes()  { return value_of_smiOop(read_oop( expectedMapObjOffsetForActivationPartSizes )); }

  static fint get_argCount(             fint partSizes ) { return  partSizes        & 255; }
  static fint get_assignableLocalCount( fint partSizes ) { return (partSizes >>  8) & 255; }
  static fint get_maxStackSize(         fint partSizes ) { assert( OOP_SIZE == 32 );
                                                           return (partSizes >> 16) & 255; }
  
  bool is_outer_activation_map() {
    oop_t mt = mapType();
    assert( mt == The::oop_of(The::outerActivationMap_mapType)
       ||   mt == The::oop_of(The::blockActivationMap_mapType) );
    
    return mt == The::oop_of(The::outerActivationMap_mapType);
  }
  
 protected:
   // These are regular named slots on the methodMap object. But we happen to
   // know that they are always at the same offset, so we hard-code that offset
   // for efficiency.
   static const fint expectedMapObjOffsetForActivationPartSizes = contents_offset + 0;
   static const fint expectedMapObjOffsetForCodes               = contents_offset + 1;
   static const fint expectedMapObjOffsetForLiterals            = contents_offset + 2;

};


# endif ACTIVATION_MAP_HH
