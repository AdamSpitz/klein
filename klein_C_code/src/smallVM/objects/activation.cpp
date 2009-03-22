# include "activation.hh"
# include "interpreter.hh"
# include "lookup.hh"
# include "byteCodes.hh"
# include "stringObj.hh"
# include "prims.hh"
# include "asserts.hh"
# include "blockObj.hh"





// todo optimize time by including io in sp -- dmu 1/06

# define DECLARE_STACK \
  fint io          = indexableOrigin(); \
  fint sp          = get_sp_quickly(io);  \
  fint sp_limit    = indexableSize(); 

# define push(x)  if (sp >= sp_limit) { \
                    printf("sp is %i, sp_limit is %i, activationMap oop is %i\n", sp, sp_limit, map_oop()); \
                    unimplemented("stack overflow, suggest cloning and ``becoming'' activation"); \
                  } \
                  else write_oop( io +   sp++,  x )
                  
# define pop()    read_oop( io + --sp ) 


oop_t ActivationObj::loop(oop_t this_activation) {

  The::set_active_context( this_activation, this);
  
  DECLARE_STACK;
  smi         bci = get_pc_quickly(io);
  
  ActivationMapObj* m_addr = map_addr();
  
  oop_t           codes_oop    = m_addr->codes();
  ByteVectorObj*  codes_addr   = ByteVectorObj::from(codes_oop);
  char*           codes        = codes_addr->bytes();
  fint            codes_length = codes_addr->indexableSize();



  oop_t         literals       = m_addr->literals();
  ObjVectorObj* literals_addr  = ObjVectorObj::from(literals);
  fint          literals_io    = literals_addr->indexableOrigin();
  
 
  fint index = 0, temp_index;
  # define UC_index ((temp_index = index << INDEXWIDTH), (index = 0), temp_index | bc_index)
  bool undirected_resend = false;
  # define UC_undirected_resend (undirected_resend ? (undirected_resend = false, true) : false)
  
  fint lexical_level = 0;
  
  # define use_lit (literals_addr->read_oop(literals_io + UC_index))
  
  oop_t delegatee = 0, temp_del;
  # define UC_del  ((temp_del = delegatee), (delegatee = 0), temp_del)
  
  fint arg_count = 0, temp_arg_count;
  # define UC_arg_count ((temp_arg_count = arg_count), (arg_count = 0), temp_arg_count)
  
  fint temp_bci;
  // for process pre-emption, stop on backward branches
  // todo optimize should probably just stop every 10 or 100 backward branches, or even just every N bytecodes
  # define set_bci(bci_oop) (temp_bci = value_of_smiOop(assert_smi(bci_oop)), stop = temp_bci < bci, bci = temp_bci)
  
  oop_t self = get_self_quickly(io);
  oop_t rcvr = get_rcvr_quickly(io);
  for ( bool stop = false; !stop; ) {
    if (bci >= codes_length) {
      oop_t r = pop();
      oop_t s = get_sender_quickly(io);
      if (s != NULL) // it'll be NULL if we're returning from the start method
        ActivationObj::from(s)->remote_push(r);
      // todo optimize time slow; quits this routine just for a return -- dmu 1/06
      return s;
    }
    unsigned char bc = codes[bci++];
    ByteCodeKind kind  = getOp(bc);
    fint         bc_index = getIndex(bc);
    // printf("interpreting a bytecode in activationMap %i, bc is %i, kind is %i, bc_index is %i\n", map_oop(), bc, kind, bc_index);
    switch (kind) {
     default:   fatal("unknown kind of bytecode"); break;
     
     case                   INDEX_CODE:          index = UC_index;     break;
     case           LEXICAL_LEVEL_CODE:  lexical_level = UC_index;     break;
     case          ARGUMENT_COUNT_CODE:      arg_count = UC_index;     break;
  
     case           READ_LOCAL_CODE:   push(local_obj_addr(lexical_level)-> read_arg_or_local(UC_index)      );  lexical_level = 0;               break;
     case          WRITE_LOCAL_CODE:        local_obj_addr(lexical_level)->write_arg_or_local(UC_index, pop());  lexical_level = 0;  push(self);  break;
     
     case          BRANCH_CODE:                                                          set_bci(use_lit);                   break;
     case          BRANCH_TRUE_CODE:     if ( pop() == The::oop_of(The:: true_object))   set_bci(use_lit);  else index = 0;  break;
     case          BRANCH_FALSE_CODE:    if ( pop() == The::oop_of(The::false_object))   set_bci(use_lit);  else index = 0;  break;
     case          BRANCH_INDEXED_CODE:
                                        {
                                         ObjVectorObj* branch_vector_addr = ObjVectorObj::from(assert_objVector(use_lit));
                                         oop_t branch_index_oop = pop();
                                         if ( is_smi(branch_index_oop) ) {
                                            smi branch_index = value_of_smiOop(branch_index_oop);
                                            if (  0 <= branch_index  &&  branch_index < branch_vector_addr->indexableSize()  )   {
                                              oop_t dest_oop = branch_vector_addr->indexable_at(branch_index);
                                              set_bci(dest_oop);
                                            }
                                         }
                                        }
                                        break;
       
     
     case      DELEGATEE_CODE:               delegatee = use_lit;                                     break;


     case LITERAL_CODE:
      {
       oop_t lit = use_lit;
       if (::is_block(lit)) {
         put_sp_quickly(io, sp); // make sure that the sp is stored correctly, because an allocation could trigger a GC
         oop_t cloned_block = BlockObj::clone_block(lit, this_activation);
         ActivationObj* possibly_moved_act_addr = ActivationObj::from(this_activation); // mightHaveScavengedTheActivation
         if (possibly_moved_act_addr != this) {
           possibly_moved_act_addr->remote_push(cloned_block);
           possibly_moved_act_addr->put_pc_quickly( io, bci );
           return this_activation;
         } else {
           push(cloned_block);
         }
       } else {
         push(lit);
       }
      }
      break;
     
     case IMPLICIT_SEND_CODE:
      // fall through
     case SEND_CODE:
     {
      oop_t selector = use_lit;
      if (selector == The::oop_of(The::restart_selector)) {
        put_sp_quickly( io,  sp  = first_stack_offset               );
        put_pc_quickly( io,  bci = get_pc_after_endInit_quickly(io) );
        break;
      }
      put_sp_quickly( io, sp );
      // todo optimize dmu 3/6. This is here for the _Breakpoint primitve to help debugging by storing the PC.
      // But it slows every primitive, sigh.
      put_pc_quickly( io, bci);

      oop_t a = send(kind == IMPLICIT_SEND_CODE, selector, UC_undirected_resend, UC_del, UC_arg_count, this_activation); 
      if (a != this_activation || ActivationObj::from(a) != this) { // mightHaveScavengedTheActivation
        // put_pc_quickly( io, bci); // commented out after I added the put_pc_quickly above, dmu 3/6
        return a;
      }
      sp = get_sp_quickly(io);
     }
     break;
      
     case NO_OPERAND_CODE:
      switch(bc_index) {
       default: fatal("???"); break;
        case               POP_CODE:      pop();                                  break;
        case              SELF_CODE:      push(self);                             break;
        case          END_INIT_CODE:      put_pc_after_endInit_quickly(io, bci);  break;

        case   NONLOCAL_RETURN_CODE:      return nonlocal_return(pop(), rcvr);    break;
        case UNDIRECTED_RESEND_CODE:      undirected_resend = true;               break;
       }
       break;

    }
  }
  put_sp_quickly( io, sp  );
  put_pc_quickly( io, bci );
  return this_activation;
}


void ActivationObj::remote_push(oop_t x) {
  DECLARE_STACK;
  push(x);
  put_sp_quickly( io, sp );
}


oop_t ActivationObj::remote_pop() {
  DECLARE_STACK;
  sp_limit; // unused
  oop_t r = pop();
  put_sp_quickly( io, sp );
  return r;
}


inline static oop_t assert_block(oop_t rcvr) { assert(is_block(rcvr)); return rcvr; }


static oop_t home_frame(oop_t rcvr) {
  return BlockObj::from(assert_block(rcvr))->homeFramePointer();
}


oop_t ActivationObj::send(bool isImplicitSelf, oop_t sel, bool isUndirected, oop_t delegatee, fint arg_count, oop_t this_act) {
  DECLARE_STACK;
  sp_limit; // unused
  sp -= arg_count;
  smi args_offset = sp;
  oop_t* argsp = oop_addr(io + args_offset);
  oop_t  rcvr = isImplicitSelf  ?  get_self_quickly(io)  :  pop();
  put_sp_quickly(io, sp);

  // todo optimize time slow? -- dmu 1/06
  for (smi i = 0;  i < arg_count;  ++i )
    read_barrier(io + args_offset + i);
  
  oop_t new_act = NULL;

  StringObj* sel_addr = StringObj::from(sel);
  
  // I use this printf so often that I'd like to leave it here (commented out). -- Adam, 5/06
  // printf("sending "); sel_addr->string_print(); printf("\n");
  
  if (sel_addr->bytes()[0] == '_')  {
 
    oop_t result = primitive_send(sel, rcvr, argsp, arg_count, this_act, &new_act);
    // todo optimize time mightHaveScavengedTheActivation:
    // Use a boolean flag to tell if the activation has moved?
    // Look for other places tagged mightHaveScavengedTheActivation. -- Adam, 5/06
    if (!is_mark(result)) {
      ActivationObj* possibly_moved_act_addr = ActivationObj::from(this_act);
      possibly_moved_act_addr->remote_push(result);
    }
  }
  else {
    non_primitive_send(rcvr, argsp, arg_count, isImplicitSelf, sel, isUndirected, delegatee, this_act, &new_act);
  }
  return new_act ? new_act : this_act;
}


void ActivationObj::non_primitive_send(oop_t rcvr, oop_t* argsp, fint arg_count, bool isImplicitSelf, oop_t sel, bool isUndirected, oop_t delegatee, oop_t this_act, oop_t* new_actp) {
    // todo optimize time  For block method activations, could cache the result of outermost_lexical_frame()->get_methodHolder()
    //                     (or just outermost_lexical_frame(), so that it could be useful for NLRs too) in the methodHolder indexable. -- Adam, 3/06
    oop_t holder_of_sender_method = (isUndirected || delegatee) ? outermost_lexical_frame()->get_methodHolder() : The::oop_of(The::nil_object);

    oop_t holder;
    SlotDesc* sd;

    oop_t contents = Lookup::findObject( rcvr, sel, &holder, &sd, new_actp, this_act, isUndirected, delegatee, holder_of_sender_method, isImplicitSelf );
    if (contents == badOop) { // lookup failure
    }
    else {
      if (sd->is_assignment()) {
        assert(arg_count == 1);
        sd->set_assignable_contents(holder, argsp[0]);
        remote_push(rcvr);
      }
      else if (is_mem(contents)  &&  MemObj::from(contents)->is_activationMap()) {
        // todo optimize time: pass in the contents address, too, since we've already got it. -- Adam, 5/06
        *new_actp = ActivationObj::clone_for(contents, holder, rcvr, argsp, arg_count, this_act);
      }
      else {
        assert(arg_count == 0);
        remote_push(contents);
      }
    }
}


// todo optimize time finding primtives someday, also optimized fail block creation -- dmu 1/06

oop_t ActivationObj::primitive_send( oop_t sel, oop_t rcvr, oop_t* argsp, fint arg_count, oop_t this_act, oop_t* new_actp) {
  oop_t result = Primitives::invoke(sel, rcvr, argsp, arg_count, this_act, new_actp);
  return result;
}


// shortcut for use with primitive failure
oop_t ActivationObj::clone_for_failure(oop_t rcvr_for_fail, oop_t self_string_for_message, oop_t* argsp, fint arg_count, oop_t sender_act ) {
  oop_t holder, lookup_failed_act;
  oop_t contents = Lookup::findObject(rcvr_for_fail, self_string_for_message, &holder, NULL, &lookup_failed_act, sender_act, false, NULL, The::oop_of(The::nil_object));
  if (contents == badOop)
    return lookup_failed_act;
  return clone_for(contents, holder, rcvr_for_fail, argsp, 2, sender_act);
}

static oop_t self_for_new_activation(ActivationMapObj* contents_addr, oop_t rcvr) {
  return contents_addr->is_outer_activation_map()
             ? rcvr
             : ActivationObj::from(home_frame(rcvr))->get_self();
}

oop_t ActivationObj::clone_for(oop_t method, oop_t method_holder, oop_t rcvr, oop_t* args, fint arg_count, oop_t sender_act) {
  assert(is_mem(method));
  ActivationMapObj* m_addr = ActivationMapObj::from(method);
  assert(m_addr->is_activationMap());
  oop_t self = self_for_new_activation(m_addr, rcvr);
  
  fint partSizes = m_addr->get_activationPartSizes();
  assert( ActivationMapObj::get_argCount(partSizes) == arg_count );
  fint assignable_local_count = ActivationMapObj::get_assignableLocalCount( partSizes );
  fint max_stack_size         = ActivationMapObj::get_maxStackSize(         partSizes );
  smi new_indexable_origin = first_arg_offset + arg_count + assignable_local_count;
  smi new_indexable_size   = first_stack_offset + max_stack_size;
  ActivationObj* addr;
  oop_t a =  ((ObjVectorObj*) The::addr_of( The::vector_proto ))
                  ->clone_and_resize( new_indexable_size, badOop, (ObjVectorObj**)&addr, new_indexable_origin );
  addr->initialize(m_addr, method, method_holder, self, rcvr, args, arg_count, assignable_local_count, max_stack_size, sender_act);
  return a;
}


void ActivationObj::initialize(ActivationMapObj* m_addr,
                               oop_t             activationMap,
                               oop_t             method_holder,
                               oop_t             self,
                               oop_t             rcvr,
                               oop_t*            args,
                               fint              arg_count,
                               fint              assignable_local_count,
                               fint              max_stack_size,
                               oop_t             sender_act) {

  set_mark_bit_for_activation();
  set_map( activationMap );
  fint io = indexableOrigin();
  
  put_sp_quickly(             io,  first_stack_offset     );
  put_pc_quickly(             io,  m_addr->starting_pc()  ); 
  put_self_quickly(           io,  self                   );
  put_rcvr_quickly(           io,  rcvr                   );
  put_sender_quickly(         io,  sender_act             );
  put_methodHolder_quickly(   io,  method_holder          );

  // set args
  for (fint i = 0;  i < arg_count;  ++i)
    write_arg_or_local( first_arg_offset + i,  args[i] );
    
  // set locals
  oop_t nilOop = The::oop_of(The::nil_object);
  for (fint i = 0;  i < assignable_local_count;  ++i)
    write_arg_or_local( first_arg_offset + arg_count + i,  nilOop );
}


ActivationObj* ActivationObj::outermost_lexical_frame() {
  return map_addr()->is_outer_activation_map()
             ? this
             : ActivationObj::from(home_frame(get_rcvr()))->outermost_lexical_frame();
}


oop_t ActivationObj::nonlocal_return(oop_t result, oop_t rcvr) {
  ActivationObj* outermost_frame;
  if (! ::is_block(rcvr)) {
    // todo optimize transmogrify away NLR bytecodes in outer methods
    assert(map_addr()->is_outer_activation_map());
    outermost_frame = this;
  } else {
    // todo unimplemented _OnNonLocalReturn:
    outermost_frame = ActivationObj::from(home_frame(rcvr))->outermost_lexical_frame();
  }
  oop_t sender_frame = outermost_frame->get_sender();
  ActivationObj::from(sender_frame)->remote_push(result);
  return sender_frame;
}


oop_t ActivationObj::vector_of_outgoing_arguments(fint argc, bool isImplicitSelf) {
  ObjVectorObj* vaddr;
  oop_t v = ((ObjVectorObj*) The::addr_of(The::vector_proto))->clone_and_resize(argc, The::oop_of(The::nil_object), &vaddr);
  fint beginning_of_args_offset = indexableOrigin() + get_sp() + (isImplicitSelf ? 0 : 1); // sp has already been cut back
  fint       end_of_args_offset = beginning_of_args_offset + argc;
  for (fint i = beginning_of_args_offset; i < end_of_args_offset; ++i) {
    vaddr->write_indexable_at( i, read_oop(i) );
  }
  return v;
}
