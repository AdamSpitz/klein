 '$Revision: 30.12 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot'
        
         kleinBlockInlining = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinBlockInlining.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.12 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinBlockInlining postFileIn
