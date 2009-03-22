 '$Revision: 30.5 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjects InitialContents: FollowSlot'
        
         vmKitObjects = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjects' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjects' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitObjects.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjects' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjects InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjects' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjects InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjects' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjects InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjects' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjects InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjects' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjects InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.5 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjects' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjects InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitLenses
vmKitLayouts
vmKitMaps
vmKitSlotType
vmKitOops
vmKitWordLayout
vmKitObjectLocator
vmKitCloning
vmKitLookup
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitLenses' From: 'applications/klein'
 bootstrap read: 'vmKitLayouts' From: 'applications/klein'
 bootstrap read: 'vmKitMaps' From: 'applications/klein'
 bootstrap read: 'vmKitSlotType' From: 'applications/klein'
 bootstrap read: 'vmKitOops' From: 'applications/klein'
 bootstrap read: 'vmKitWordLayout' From: 'applications/klein'
 bootstrap read: 'vmKitObjectLocator' From: 'applications/klein'
 bootstrap read: 'vmKitCloning' From: 'applications/klein'
 bootstrap read: 'vmKitLookup' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitObjects postFileIn
