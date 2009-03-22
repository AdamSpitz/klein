 '$Revision: 30.4 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitDB InitialContents: FollowSlot'
        
         vmKitDB = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitDB' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitDB' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitDB.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitDB' -> () From: ( | {
         'ModuleInfo: Module: vmKitDB InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitDB' -> () From: ( | {
         'ModuleInfo: Module: vmKitDB InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitDB' -> () From: ( | {
         'ModuleInfo: Module: vmKitDB InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitDB' -> () From: ( | {
         'ModuleInfo: Module: vmKitDB InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitDB' -> () From: ( | {
         'ModuleInfo: Module: vmKitDB InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.4 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitDB' -> () From: ( | {
         'ModuleInfo: Module: vmKitDB InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitProxyClients
vmKitProxySockets
vmKitUI
vmKitReflection
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitProxyClients' From: 'applications/klein'
 bootstrap read: 'vmKitProxySockets' From: 'applications/klein'
 bootstrap read: 'vmKitUI' From: 'applications/klein'
 bootstrap read: 'vmKitReflection' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitDB postFileIn
