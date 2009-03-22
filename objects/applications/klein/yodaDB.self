 '$Revision: 30.4 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaDB InitialContents: FollowSlot'
        
         yodaDB = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaDB' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaDB' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaDB.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaDB' -> () From: ( | {
         'ModuleInfo: Module: yodaDB InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaDB' -> () From: ( | {
         'ModuleInfo: Module: yodaDB InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaDB' -> () From: ( | {
         'ModuleInfo: Module: yodaDB InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaDB' -> () From: ( | {
         'ModuleInfo: Module: yodaDB InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaDB' -> () From: ( | {
         'ModuleInfo: Module: yodaDB InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.4 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaDB' -> () From: ( | {
         'ModuleInfo: Module: yodaDB InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'yodaUI
yodaProxyClients
yodaReflection
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'yodaUI' From: 'applications/klein'
 bootstrap read: 'yodaProxyClients' From: 'applications/klein'
 bootstrap read: 'yodaReflection' From: 'applications/klein'



 '-- Side effects'

 globals modules yodaDB postFileIn
