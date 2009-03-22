// $Revision: 1.1 $
 

# include <inttypes.h>
# include <stdio.h>
# include <netinet/in.h>

# include "map.hh"
# include "byteVector.hh"
# include "activationMap.hh"
# include "blockObj.hh"
# include "activation.hh"
# include "freeLists.hh"



enum smallSelf_request_type_t {
  smallSelf_request_setBootstrapInfo        =  1,
  smallSelf_request_getStartFunctionAddress =  2,
  smallSelf_request_getLayoutConstants      =  3,
  smallSelf_request_getWKSAddress           =  4,
  smallSelf_request_getOTTimestampAddress   =  5
};

const char* smallSelfRequestTypes[] = {
  "zero",
  "setVMOop",
  "getStartFunctionAddress",
  "getActiveContextAddress"
};

const char* string_for_smallSelf_request_type(smallSelf_request_type_t t) {
  return (unsigned int)t < sizeof(smallSelfRequestTypes)/sizeof(smallSelfRequestTypes[0])
    ?  smallSelfRequestTypes[t]
    :  NULL;
}


class BootstrapInfoSetter: public SocketUser {
 public:
  BootstrapInfoSetter(BufferedSocket& ss): SocketUser(ss) {}
  
  void do_it() {
	int vmOop = s.read_int ("getting vmOop");
        int objectTableAddress = s.read_int("getting object table address");
	setBootstrapInfo(vmOop, objectTableAddress);
	s.write_string("", "future result status");
  }
};


class StartFunctionGetter: public SocketUser {
 public:
  StartFunctionGetter(BufferedSocket& ss) : SocketUser(ss) {}
  
  void do_it() {
	s.write_int   ((int) (& startSmallSelf), "returning start function pointer");
	s.write_string("", "empty result status");
  }

};


class LayoutConstantsGetter: public SocketUser {
 public:
  LayoutConstantsGetter(BufferedSocket& ss) : SocketUser(ss) {}
  
  void do_it() {
    # ifdef SMALL_VM
       # define WRITE_CONSTANT(const)  s.write_string(#const, #const);  s.write_int((int) (const), #const)
	WRITE_CONSTANT(smi_tag);
	WRITE_CONSTANT(mem_tag);
	WRITE_CONSTANT(float_tag);
	WRITE_CONSTANT(mark_tag);
        
        WRITE_CONSTANT(tag_mask);
        WRITE_CONSTANT(tag_shift);
        
        WRITE_CONSTANT(sizeof(oop_t));
        
        WRITE_CONSTANT(mark_byteVector_shift);
        WRITE_CONSTANT(mark_activationMap_shift);
        WRITE_CONSTANT(mark_activation_shift);
        WRITE_CONSTANT(mark_hasBeenVisitedForLookup_shift);
        WRITE_CONSTANT(mark_hasBeenVisitedForGC_shift);
        WRITE_CONSTANT(mark_isOnMarkStackForGC_shift);
        WRITE_CONSTANT(mark_isInRememberedSetForGC_shift);
        WRITE_CONSTANT(mark_oid_shift);
        
        WRITE_CONSTANT(MemObj::mark_offset);
        WRITE_CONSTANT(MemObj::map_offset);
        WRITE_CONSTANT(MemObj::contents_offset);
        
        WRITE_CONSTANT(AbstractVectorObj::indexableSize_offset);
        WRITE_CONSTANT(AbstractVectorObj::indexableOrigin_offset);
        WRITE_CONSTANT(AbstractVectorObj::contents_offset);

        WRITE_CONSTANT(BlockObj::homeFramePointer_offset);
        WRITE_CONSTANT(BlockObj::contents_offset);


        WRITE_CONSTANT(SlotType::object);
        WRITE_CONSTANT(SlotType::map);
        WRITE_CONSTANT(SlotType::argument);
        
        WRITE_CONSTANT(SlotDesc::name_offset());
        WRITE_CONSTANT(SlotDesc::type_oop_offset());
        WRITE_CONSTANT(SlotDesc::data_or_offset_offset());
        WRITE_CONSTANT(SlotDesc::annotation_offset());
        WRITE_CONSTANT(sizeof(SlotDesc) / sizeof(oop_t));
        
        WRITE_CONSTANT(MapObj::mapTypeIndex);
        WRITE_CONSTANT(MapObj::nmethodCacheIndex);
        WRITE_CONSTANT(MapObj::scalarValueCount);

        WRITE_CONSTANT(ActivationMapObj::expectedMapObjOffsetForActivationPartSizes);
        WRITE_CONSTANT(ActivationMapObj::expectedMapObjOffsetForCodes);
        WRITE_CONSTANT(ActivationMapObj::expectedMapObjOffsetForLiterals);

        WRITE_CONSTANT(Object_Table::lastInvalidEntry_offset);
        
        WRITE_CONSTANT(FreeOopsLists::last_index);
        
        WRITE_CONSTANT(The::vm); 
        WRITE_CONSTANT(The::universe); 
        WRITE_CONSTANT(The::newGeneration); 
        WRITE_CONSTANT(The::oldGeneration); 
        WRITE_CONSTANT(The::edenSpace); 
        WRITE_CONSTANT(The::tenuredSpace); 
        WRITE_CONSTANT(The::objectLocator); 
        WRITE_CONSTANT(The::canonicalizedStrings); 
        WRITE_CONSTANT(The::canonicalizedStringVector);
        WRITE_CONSTANT(The::smi_map); 
        WRITE_CONSTANT(The::float_map); 
        WRITE_CONSTANT(The::start_selector);
        WRITE_CONSTANT(The::restart_selector);
        WRITE_CONSTANT(The::true_object);
        WRITE_CONSTANT(The::false_object);
        WRITE_CONSTANT(The::size_string);
        WRITE_CONSTANT(The::mapMap_mapType);
        WRITE_CONSTANT(The::blockMap_mapType);
        WRITE_CONSTANT(The::objectVectorMap_mapType);
        WRITE_CONSTANT(The::outerActivationMap_mapType);
        WRITE_CONSTANT(The::blockActivationMap_mapType);
        WRITE_CONSTANT(The::nil_object);
        WRITE_CONSTANT(The::vector_proto);
        WRITE_CONSTANT(The::string_proto);
        WRITE_CONSTANT(The::int32_proto);
        WRITE_CONSTANT(The::process_proto);
        WRITE_CONSTANT(The::set_emptyMarker);
        WRITE_CONSTANT(The::set_removedMarker);
        WRITE_CONSTANT(The::active_context);
        WRITE_CONSTANT(The::lobby);
        WRITE_CONSTANT(The::mirrors_namespace);
        WRITE_CONSTANT(The::last);

       
        const int wksOopOffset  = (char*)&The::wks[0].oop  - (char*)&The::wks[0];
        const int wksAddrOffset = (char*)&The::wks[0].addr - (char*)&The::wks[0];
        const int wksLength     = sizeof(The::wks[0]);
        
        WRITE_CONSTANT(wksOopOffset);
        WRITE_CONSTANT(wksAddrOffset);
        WRITE_CONSTANT(wksLength);
        
        WRITE_CONSTANT(ActivationObj::sp_offset);
        WRITE_CONSTANT(ActivationObj::pc_offset);
        WRITE_CONSTANT(ActivationObj::self_offset);
        WRITE_CONSTANT(ActivationObj::rcvr_offset);
        WRITE_CONSTANT(ActivationObj::sender_offset);
        WRITE_CONSTANT(ActivationObj::methodHolder_offset);
        WRITE_CONSTANT(ActivationObj::pc_after_endInit_offset);
        WRITE_CONSTANT(ActivationObj::first_stack_offset);
           
        s.write_string("", "the end");

      # undef WRITE_CONSTANT
     # endif
  }

};



class WKS_AddressGetter: public SocketUser {
 public:
  WKS_AddressGetter(BufferedSocket& ss) : SocketUser(ss) {}
  
  void do_it() {
    # ifdef SMALL_VM
      s.write_int((int)The::wks, "writing wks address");
    # endif
  }
};



class OTTimestampAddressGetter: public SocketUser {
 public:
  OTTimestampAddressGetter(BufferedSocket& ss) : SocketUser(ss) {}
  
  void do_it() {
    # ifdef SMALL_VM
      s.write_int(Object_Table::get_timestamp_address(), "writing ObjectTable timestamp address");
    # endif
  }
};



class SmallSelfRequestServer: public SocketUser {
  public:
    SmallSelfRequestServer(BufferedSocket& ss) : SocketUser(ss) {}

    bool do_it() {
      smallSelf_request_type_t request_type = (smallSelf_request_type_t)s.read_byte("reading smallSelf request type");
      if (verbose) printf_and_flush("servicing smallSelf request type: %d\n", request_type);
      if (print_request_types  &&  string_for_smallSelf_request_type(request_type))
        printf_and_flush("request type: %s (smallSelf), pid: %d...\n",
                         string_for_smallSelf_request_type(request_type), getpid());

      bool r = true;
      switch (request_type) {
       case smallSelf_request_setBootstrapInfo:            {          BootstrapInfoSetter  x(s);  x.do_it(); }  break;
       case smallSelf_request_getStartFunctionAddress:     {          StartFunctionGetter  x(s);  x.do_it(); }  break;
       case smallSelf_request_getLayoutConstants:          {        LayoutConstantsGetter  x(s);  x.do_it(); }  break;
       case smallSelf_request_getWKSAddress:               {            WKS_AddressGetter  x(s);  x.do_it(); }  break;
       case smallSelf_request_getOTTimestampAddress:       {   OTTimestampAddressGetter  x(s);  x.do_it(); }  break;

       default:  error_printf_and_flush( "SmallSelfRequestServer: " "bad smallSelf request type: %d",
                                        request_type);  
                 exit(1);  
                 return false; /* for compiler */
      }
      if (print_request_types  &&  string_for_smallSelf_request_type(request_type))
         printf_and_flush("request type: %s (mach), pid %d done\n",
           string_for_smallSelf_request_type(request_type), getpid());
     return r;
    }
};
