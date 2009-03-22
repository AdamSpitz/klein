# ifndef WELL_KNOWN_OBJECTS_HH
# define WELL_KNOWN_OBJECTS_HH


# include "small_self_types.hh"

class MemObj;

class The {
 public:
  
  enum ID { 
   vm = 0, 
   universe, 
   newGeneration, 
   oldGeneration, 
   edenSpace, 
   tenuredSpace, 
   objectLocator, 
   canonicalizedStrings, 
   canonicalizedStringVector,
   smi_map, 
   float_map, 
   start_selector,
   restart_selector,
   true_object,
   false_object,
   size_string,
   mapMap_mapType,
   blockMap_mapType,
   objectVectorMap_mapType,
   outerActivationMap_mapType,
   blockActivationMap_mapType,
   nil_object,
   vector_proto,
   string_proto,
   int32_proto,
   process_proto,
   set_emptyMarker,
   set_removedMarker,
   active_context,
   lobby,
   mirrors_namespace,
   last };
   

 private:

  struct Well_Known_Object {
    oop_t  oop;
    MemObj* addr;
    void set(oop_t x);
    void reset_addr();
  };
  

  // todo unimplemented GC has to keep these up to date -- dmu 1/06
  static Well_Known_Object wks[];
  
  static void assert_all_valid();  

 public:
 
  static oop_t    oop_of(ID id) { return wks[id].oop; }
  static MemObj* addr_of(ID id) { return wks[id].addr; }
  static void    set_oop_and_addr_of(ID id, oop_t o)  {  wks[id].set(o); }
  static void    set_addr_of(ID id, MemObj* addr)  { wks[id].addr = addr; }
  static void    reset_addr_of(ID id)  { wks[id].reset_addr(); }
  
  static void    moved_some_objects();
  
  static void    initialize_for_vm(oop_t);
  static void    set_from( ID, ID, char* );
  
  static void    set_active_context( oop_t o, MemObj* a) { wks[active_context].oop = o;  wks[active_context].addr = a; }
  
  friend class WKS_AddressGetter;
  friend class LayoutConstantsGetter;
};


# endif // WELL_KNOWN_OBJECTS_HH
