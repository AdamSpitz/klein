# ifndef MAP_HH
# define MAP_HH


# include "byteVector.hh"
# include "objVector.hh"

class SlotType {
 friend class LayoutConstantsGetter;

 enum basic_type {  object = 0,  map = 1,  argument = 2, assignment = 3 };

 static const int basic_slot_type_shift = 0;
 static const int basic_slot_type_width = 2;
 static const int       is_parent_shift = basic_slot_type_shift + basic_slot_type_width;
 static const int   is_assignable_shift = is_parent_shift + 1;

 static const int        is_parent_mask =  1 << is_parent_shift;
 static const int    is_assignable_mask =  1 << is_assignable_shift;
 
 unsigned int basic_slot_type() { return ((unsigned int)this >> basic_slot_type_shift) & ((1 << basic_slot_type_width) - 1); }
 bool         is_parent()       { return int(this) & is_parent_mask; }
 bool         is_assignable()   { return int(this) & is_assignable_mask; } // todo cleanup why do we need this? -- dmu 1/06
 
public:
 bool isArgumentSlot()   { return basic_slot_type() == (unsigned int)argument; }
 bool isMapSlot()        { return basic_slot_type() == (unsigned int)map; }
 bool isObject()         { return basic_slot_type() == (unsigned int)object; }
 bool isAssignmentSlot() { return basic_slot_type() == (unsigned int)assignment; }
 bool isParent()         { return is_parent(); }
 
 static SlotType* parentMapSlotType()    { 
   return (SlotType*) ((map << basic_slot_type_shift) | is_parent_mask);
 }
 
 static SlotType* parentObjectSlotType() { 
   return (SlotType*) ((object << basic_slot_type_shift) | is_parent_mask);
 }
 
 static SlotType* from_value(smi i) { return (SlotType*)i; };
        smi         to_value()      { return (smi)this; }
}; 


class SlotDesc {
 protected:
  oop_t _name;
  oop_t type_oop;
  oop_t data_or_offset;
  oop_t annotation;

 public:
  oop_t                name()   { return _name; }
  ByteVectorObj*  name_addr()   { return (ByteVectorObj*) MemObj::from(name()); } // StringObj??
  oop_t               value()   { return data_or_offset; }
  smi                offset()   { return value_of_smiOop(data_or_offset); }
  SlotType*            type()   { return SlotType::from_value(value_of_smiOop(type_oop)); }
  bool            is_object()   { return type()->isObject(); }
  bool               is_map()   { return type()->isMapSlot(); }
  bool            is_parent()   { return type()->isParent(); }
  bool          is_argument()   { return type()->isArgumentSlot(); }
  bool        is_assignment()   { return type()->isAssignmentSlot(); }
  
  oop_t            contents(MemObj* addr)  { return  is_map()  ?    data_or_offset :              addr->read_oop(offset()); }
  oop_t            contents(oop_t obj)     { return  is_map()  ?    data_or_offset : MemObj::from(obj)->read_oop(offset()); }
  
  void         set_contents(MemObj* addr, oop_t x)     { if (is_object())               addr->write_oop(offset(), x);  else write_map_slot(             addr, x); }
  void         set_contents(   oop_t obj, oop_t x)     { if (is_object())  MemObj::from(obj)->write_oop(offset(), x);  else write_map_slot(MemObj::from(obj), x); }

  void         set_assignable_contents(oop_t obj, oop_t x)     { assert(is_assignment());  MemObj::from(obj)->write_oop(offset(), x); }
  
  oop_t        write_map_slot(MemObj*, oop_t);
  
  void print();
  void printAugmentedName();
  
 private:  
 friend class LayoutConstantsGetter;
  static int           name_offset() { return (int)&((SlotDesc*)0)->_name / sizeof(oop_t); }
  static int       type_oop_offset() { return (int)&((SlotDesc*)0)->type_oop / sizeof(oop_t); }
  static int data_or_offset_offset() { return (int)&((SlotDesc*)0)->data_or_offset / sizeof(oop_t); }
  static int     annotation_offset() { return (int)&((SlotDesc*)0)->annotation / sizeof(oop_t); }
};



class MapObj : public ObjVectorObj {
 friend class LayoutConstantsGetter;

 protected:
  static const fint mapTypeIndex = 0;
  static const fint nmethodCacheIndex = 1; // could eliminate this word
  static const fint scalarValueCount = 2;
  
 public:
  oop_t       mapType()                        { return indexable_at(mapTypeIndex); }
  bool        is_mapMap()                      { return mapType() == The::oop_of(The::mapMap_mapType); }
  SlotDesc*   slotDesc_origin()                { return (SlotDesc*)indexable_addr(scalarValueCount); }
  fint        slotDesc_count()                 { return (indexableSize() - scalarValueCount) * sizeof(oop_t) / sizeof(SlotDesc); }
  SlotDesc*   slotDesc_unchecked_at(fint i)    { return slotDesc_origin() + i; }
  SlotDesc*   slotDesc_at(fint i)              { return slotDesc_unchecked_at( assert_bounds(i, slotDesc_count()) ); }
  
  SlotDesc*   find_slot_with_C_name(char*);
  SlotDesc*   find_slot(oop_t str);
  
  static MapObj* from(oop_t x) { return (MapObj*) ObjVectorObj::from(x); }
  static void print     (oop_t obj);
  static void print_oop (oop_t obj);
};

# define FOR_EACH_SLOT_DESC( mo, sd ) \
    for ( SlotDesc *sd = (mo)->slotDesc_origin(),  *FOR_EACH_SLOT_DESC_end = sd + (mo)->slotDesc_count();   \
          sd < FOR_EACH_SLOT_DESC_end;  \
        ++sd )
        
# define FOR_EACH_PARENT_SLOT_DESC( mo, sd ) \
  FOR_EACH_SLOT_DESC( (mo), sd) \
    if (sd->is_parent())

# define FOR_EACH_ARG_SLOT_DESC( mo, sd ) \
  FOR_EACH_SLOT_DESC( (mo), sd) \
    if (sd->is_argument())

# endif // MAP_HH
