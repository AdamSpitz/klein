 '$Revision: 30.9 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmKit InitialContents: FollowSlot'
        
         asmKit = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmKit' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmKit' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmKit.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmKit' -> () From: ( | {
         'ModuleInfo: Module: asmKit InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmKit' -> () From: ( | {
         'ModuleInfo: Module: asmKit InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmKit' -> () From: ( | {
         'ModuleInfo: Module: asmKit InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmKit' -> () From: ( | {
         'ModuleInfo: Module: asmKit InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmKit' -> () From: ( | {
         'ModuleInfo: Module: asmKit InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.9 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmKit' -> () From: ( | {
         'ModuleInfo: Module: asmKit InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'asmFrame
asmPPC
asmSPARC
asmI386
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'asmFrame' From: 'applications/asmKit/asmFrame'
 bootstrap read: 'asmPPC' From: 'applications/asmKit/asmPPC'
 bootstrap read: 'asmSPARC' From: 'applications/asmKit/asmSPARC'
 bootstrap read: 'asmI386' From: 'applications/asmKit/asmI386'



 '-- Side effects'

 globals modules asmKit postFileIn
