 '$Revision: 30.4 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaUI InitialContents: FollowSlot'
        
         yodaUI = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaUI' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaUI' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaUI.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaUI' -> () From: ( | {
         'ModuleInfo: Module: yodaUI InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaUI' -> () From: ( | {
         'ModuleInfo: Module: yodaUI InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaUI' -> () From: ( | {
         'ModuleInfo: Module: yodaUI InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaUI' -> () From: ( | {
         'ModuleInfo: Module: yodaUI InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaUI' -> () From: ( | {
         'ModuleInfo: Module: yodaUI InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.4 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaUI' -> () From: ( | {
         'ModuleInfo: Module: yodaUI InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'yodaMorphs
yodaModels
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'yodaMorphs' From: 'applications/klein'
 bootstrap read: 'yodaModels' From: 'applications/klein'



 '-- Side effects'

 globals modules yodaUI postFileIn
