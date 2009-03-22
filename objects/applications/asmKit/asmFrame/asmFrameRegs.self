 '$Revision: 30.14 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: InitializeToExpression: (\'\')'
        
         name <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: InitializeToExpression: (0)'
        
         number <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators constants parent proto parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot\x7fVisibility: public'
        
         < x = ( |
            | 
            value < x value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | 
            == x).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            identityHash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinAssemblerExternalStoreString = ( |
            | name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot\x7fVisibility: public'
        
         nameSpace = ( |
            | 
            "This used to be implemented by looking
             at the creatorSlotHint, but now we don't
             make these objects well-known anymore.
             -- Adam, 11/04"
            childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'orderedClonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot\x7fVisibility: public'
        
         printString = ( |
            | 
            name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot\x7fVisibility: public'
        
         value = ( |
            | number).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot'
        
         asmFrameRegs = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameRegs' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameRegs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmFrameRegs.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameRegs' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmFrame'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameRegs' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameRegs' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameRegs' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameRegs' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.14 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameRegs' -> () From: ( | {
         'ModuleInfo: Module: asmFrameRegs InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules asmFrameRegs postFileIn
