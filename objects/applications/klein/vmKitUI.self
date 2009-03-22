 '$Revision: 30.4 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitUI InitialContents: FollowSlot'
        
         vmKitUI = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUI' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUI' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitUI.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUI' -> () From: ( | {
         'ModuleInfo: Module: vmKitUI InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUI' -> () From: ( | {
         'ModuleInfo: Module: vmKitUI InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUI' -> () From: ( | {
         'ModuleInfo: Module: vmKitUI InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUI' -> () From: ( | {
         'ModuleInfo: Module: vmKitUI InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUI' -> () From: ( | {
         'ModuleInfo: Module: vmKitUI InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.4 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUI' -> () From: ( | {
         'ModuleInfo: Module: vmKitUI InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitMorphs
vmKitModels
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitMorphs' From: 'applications/klein'
 bootstrap read: 'vmKitModels' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitUI postFileIn
