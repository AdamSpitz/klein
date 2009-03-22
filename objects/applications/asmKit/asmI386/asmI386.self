 '$Revision: 1.2 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> () From: ( | {
         'ModuleInfo: Module: asmI386 InitialContents: FollowSlot'
        
         i386 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'i386' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems i386.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: asmI386 InitialContents: FollowSlot\x7fVisibility: private'
        
         littleEndianMixin* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: asmI386 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmI386 InitialContents: FollowSlot'
        
         asmI386 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmI386' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmI386' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmI386.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmI386' -> () From: ( | {
         'ModuleInfo: Module: asmI386 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmI386'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmI386' -> () From: ( | {
         'ModuleInfo: Module: asmI386 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmI386' -> () From: ( | {
         'ModuleInfo: Module: asmI386 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmI386' -> () From: ( | {
         'ModuleInfo: Module: asmI386 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmI386' -> () From: ( | {
         'ModuleInfo: Module: asmI386 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 1.2 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmI386' -> () From: ( | {
         'ModuleInfo: Module: asmI386 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules asmI386 postFileIn
