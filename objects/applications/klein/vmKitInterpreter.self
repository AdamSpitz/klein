 '$Revision: 30.2 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: vmKitInterpreter InitialContents: FollowSlot\x7fVisibility: public'
        
         loop: act = ( |
             actMap.
             arg_count <- 0.
             bci.
             codes.
             codes_size.
             delegatee <- 0.
             index <- 0.
             lexical_level <- 0.
             literals.
             sp.
             sp_limit.
             stop <- bootstrap stub -> 'globals' -> 'false' -> ().
             temp_arg_count.
             temp_bci.
             temp_del.
             undirected_resend <- bootstrap stub -> 'globals' -> 'false' -> ().
            | 


            [todo interpreter].
            "This method is a not-yet-finished translation of the main interpreter loop in the Yoda C code.
             The code definitely did not translate nicely to Self; it might be better to throw this away
             and start over in a more idiomatic Self way. -- Adam, 7/06"


            activeContext: act.

            sp_limit:       sp_limit_of: act.
            sp:                   sp_of: act.
            self_obj:           self_of: act.
            rcvr_obj:           rcvr_of: act.
            bci:                  pc_of: act.
            actMap:               mapOf: act.

            codes:      actMap codes.
            codes_size: codes size.
            literals:   actMap literals.

            [stop] whileFalse: [| bc. bc_kind. bc_index. popped. new_bci |
              bci >= codes_size ifTrue: [| r. s |
                sp: sp pred.
                popped:  for: act IndexableAt: sp.
                s:       sender_of: act.
                s ifNotNil: [ "It'll be nil if we're returning from the start method."
                  for: act RemotePush: popped.
                ].
                ^ s
              ].

              bc: codes byteAt: bci.
              bci: bci succ.
              bc_kind:  bytecodeFormat opcodeOf: bc.
              bc_index: bytecodeFormat  indexOf: bc.

              __BranchIndexedBy: bc_kind To: 'index'        To: 'literal'      To: 'send'          To: 'implicitSelfSend'
                                         To: 'extended'     To: 'readLocal'    To: 'writeLocal'    To: 'lexicalLevel'
                                         To: 'branchAlways' To: 'branchIfTrue' To: 'branchIfFalse' To: 'branchIndexed'
                                         To: 'delegatee'    To: 'argumentCount'.

                error: 'unknown kind of bytecode'.

                __DefineLabel: 'index'.
                index: bytecodeFormat shiftIndex: index AndAdd: bc_index.

                __DefineLabel: 'literal'.
                __BranchTo: 'end'.

                __DefineLabel: 'send'.
                __BranchTo: 'end'.

                __DefineLabel: 'implicitSelfSend'.
                __BranchTo: 'end'.

                __DefineLabel: 'extended'.
                __BranchTo: 'end'.

                __DefineLabel: 'readLocal'.
                sp >= sp_limit ifTrue: [error: 'stack overflow'].
                index: bytecodeFormat shiftIndex: index AndAdd: bc_index.
                for: act IndexableAt: sp Put:
                   for: (for: act LexicalParentAtLevel: lexical_level) ArgOrLocalAt: index.

                index:         0.
                lexical_level: 0.
                __BranchTo: 'end'.

                __DefineLabel: 'writeLocal'.
                sp: sp pred.
                popped: for: act IndexableAt: sp.
                index: bytecodeFormat shiftIndex: index AndAdd: bc_index.
                for: (for: act LexicalParentAtLevel: lexical_level) ArgOrLocalAt: index Put: popped.
                index:         0.
                lexical_level: 0.
                sp >= sp_limit ifTrue: [error: 'stack overflow'].
                for: act IndexableAt: sp Put: self_obj.
                sp: sp succ.
                __BranchTo: 'end'.

                __DefineLabel: 'lexicalLevel'.
                lexicalLevel: bytecodeFormat shiftIndex: index AndAdd: bc_index.
                index: 0.
                __BranchTo: 'end'.

                __DefineLabel: 'branchAlways'.
                index: bytecodeFormat shiftIndex: index AndAdd: bc_index.
                new_bci: layouts smi valueOf: layouts objVector for: literals IndexableAt: index.
                stop: new_bci < bci.
                bci: new_bci.
                index: 0.
                __BranchTo: 'end'.

                __DefineLabel: 'branchIfTrue'.
                sp: sp pred.
                popped:  for: act IndexableAt: sp.
                __BranchIfTrue: popped To: 'branchAlways'.
                index: 0.
                __BranchTo: 'end'.

                __DefineLabel: 'branchIfFalse'.
                sp: sp pred.
                popped:  for: act IndexableAt: sp.
                __BranchIfFalse: popped To: 'branchAlways'.
                index: 0.
                __BranchTo: 'end'.

                __DefineLabel: 'branchIndexed'.
                _Breakpoint: 'indexed branch not implemented yet'.
                __BranchTo: 'end'.

                __DefineLabel: 'delegatee'.
                __BranchTo: 'end'.

                __DefineLabel: 'argumentCount'.
                arg_count: bytecodeFormat shiftIndex: index AndAdd: bc_index.
                index: 0.
              __DefineLabel: 'end'.
            ].

            halt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitInterpreter InitialContents: FollowSlot'
        
         vmKitInterpreter = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitInterpreter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitInterpreter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitInterpreter.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitInterpreter' -> () From: ( | {
         'ModuleInfo: Module: vmKitInterpreter InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitInterpreter' -> () From: ( | {
         'ModuleInfo: Module: vmKitInterpreter InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitInterpreter' -> () From: ( | {
         'ModuleInfo: Module: vmKitInterpreter InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitInterpreter' -> () From: ( | {
         'ModuleInfo: Module: vmKitInterpreter InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitInterpreter' -> () From: ( | {
         'ModuleInfo: Module: vmKitInterpreter InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.2 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitInterpreter' -> () From: ( | {
         'ModuleInfo: Module: vmKitInterpreter InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitInterpreter postFileIn
