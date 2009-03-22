# ifndef ACTIVATION_HH
# define ACTIVATION_HH

# include "activationMap.hh"
# include "blockObj.hh"


class ActivationObj : public ObjVectorObj {
 public:
  ActivationMapObj* map_addr() { return (ActivationMapObj*)ObjVectorObj::map_addr(); }
  
 protected:
  static const fint first_arg_offset = contents_offset;
  
 private:  
  friend class LayoutConstantsGetter;
  enum { 
                       sp_offset,
                       pc_offset,
                     self_offset,
                     rcvr_offset,
                   sender_offset,
             methodHolder_offset,
         pc_after_endInit_offset,
              first_stack_offset };
  
  smi   get_sp_quickly(               smi io )  { return read_smi(io +                 sp_offset); }
  smi   get_pc_quickly(               smi io )  { return read_smi(io +                 pc_offset); }
  oop_t get_self_quickly(             smi io )  { return read_oop(io +               self_offset); }
  oop_t get_rcvr_quickly(             smi io )  { return read_oop(io +               rcvr_offset); }
  oop_t get_sender_quickly(           smi io )  { return read_oop(io +             sender_offset); }
  oop_t get_methodHolder_quickly(     smi io )  { return read_oop(io +       methodHolder_offset); }
  oop_t get_pc_after_endInit_quickly( smi io )  { return read_smi(io +   pc_after_endInit_offset); }
  
  void put_sp_quickly(               smi io,  smi   x)  { write_smi(io +                 sp_offset, x); }
  void put_pc_quickly(               smi io,  smi   x)  { write_smi(io +                 pc_offset, x); }
  void put_self_quickly(             smi io,  oop_t x)  { write_oop(io +               self_offset, x); }
  void put_rcvr_quickly(             smi io,  oop_t x)  { write_oop(io +               rcvr_offset, x); }
  void put_sender_quickly(           smi io,  oop_t x)  { write_oop(io +             sender_offset, x); }
  void put_methodHolder_quickly(     smi io,  oop_t x)  { write_oop(io +       methodHolder_offset, x); }
  void put_pc_after_endInit_quickly( smi io,  smi   x)  { write_smi(io +   pc_after_endInit_offset, x); }

 public:
  smi   get_sp(              )  { return get_sp_quickly(               indexableOrigin() ); }
  smi   get_pc(              )  { return get_pc_quickly(               indexableOrigin() ); }
  oop_t get_self(            )  { return get_self_quickly(             indexableOrigin() ); }
  oop_t get_rcvr(            )  { return get_rcvr_quickly(             indexableOrigin() ); }
  oop_t get_sender(          )  { return get_sender_quickly(           indexableOrigin() ); }
  oop_t get_methodHolder(    )  { return get_methodHolder_quickly(     indexableOrigin() ); }
  oop_t get_pc_after_endInit()  { return get_pc_after_endInit_quickly( indexableOrigin() ); }
  
  oop_t  read_arg_or_local( fint i          ) { return  read_oop( i    ); }
  void  write_arg_or_local( fint i, oop_t x ) {        write_oop( i, x ); }

  oop_t* end_of_live_oops() {
    smi io = indexableOrigin();
    smi sp = get_sp_quickly(io);
    return oop_addr(io + sp);
  }
  
  oop_t vector_of_outgoing_arguments(fint argc, bool isImplicitSelf);
  
 private:
  void initialize( ActivationMapObj* m_addr,
                   oop_t             activationMap,
                   oop_t             method_holder,
                   oop_t             self,
                   oop_t             rcvr,
                   oop_t*            args,
                   fint              arg_count,
                   fint              assignable_local_count,
                   fint              max_stack_size,
                   oop_t             sender_act);

  
 public:
  static ActivationObj* from(oop_t x) { return (ActivationObj*) ObjVectorObj::from(x); }
  oop_t run();
  oop_t loop(oop_t myAct);
  //clone for primitive failure
  //todo cleanup alex think of a better name for this message
  static oop_t clone_for_failure(oop_t rcvr_for_fail, oop_t self_string_for_message, oop_t* argsp, fint arg_count, oop_t sender_act);
 
  static oop_t clone_for(oop_t method, oop_t method_holder, oop_t rcvr, oop_t* args, fint arg_count, oop_t sender_act);
  
  ActivationObj* local_obj_addr(fint lex_level) {
    return lex_level == 0  ?  this  :  lexical_parent_addr()->local_obj_addr(lex_level - 1); }
  ActivationObj* lexical_parent_addr() {
    oop_t rcvr = get_rcvr();
    assert(::is_block(rcvr));
    return ActivationObj::from(BlockObj::from(rcvr)->homeFramePointer());
  }
  
  SlotDesc* local_slot_desc_for(fint index, fint lex_level) {
    if (lex_level > 0)  return lexical_parent_addr()->local_slot_desc_for(index, lex_level - 1);
    return &map_addr()->slotDesc_origin()[index];
  }
  
  oop_t               send( bool isImplicitSelf, oop_t sel, bool isUndirected, oop_t delegatee, fint arg_count, oop_t this_act);
  void non_primitive_send( oop_t rcvr, oop_t* argsp, fint arg_count, bool isImplicitSelf, oop_t sel, bool isUndirected, oop_t delegatee, oop_t this_act, oop_t* new_actp);
  oop_t     primitive_send( oop_t sel, oop_t rcvr, oop_t* argsp, fint arg_count, oop_t this_act, oop_t* new_actp);

  void  remote_push(oop_t);
  oop_t remote_pop();

  ActivationObj* outermost_lexical_frame();
  
  oop_t nonlocal_return(oop_t result, oop_t rcvr);
};

static oop_t home_frame(oop_t rcvr);

# endif ACTIVATION_HH
