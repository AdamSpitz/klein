 '$Revision: 30.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg3 InitialContents: FollowSlot'
        
         asmPPCReg3 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg3' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg3' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmPPCReg3.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg3' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg3 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmPPC'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg3' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg3 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg3' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg3 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg3' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg3 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg3' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg3 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg3' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg3 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules asmPPCReg3 postFileIn
