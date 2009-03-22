 '$Revision: 30.4 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinUI InitialContents: FollowSlot'
        
         kleinUI = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinUI' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinUI' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinUI.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinUI' -> () From: ( | {
         'ModuleInfo: Module: kleinUI InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinUI' -> () From: ( | {
         'ModuleInfo: Module: kleinUI InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinUI' -> () From: ( | {
         'ModuleInfo: Module: kleinUI InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinUI' -> () From: ( | {
         'ModuleInfo: Module: kleinUI InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinUI' -> () From: ( | {
         'ModuleInfo: Module: kleinUI InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.4 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinUI' -> () From: ( | {
         'ModuleInfo: Module: kleinUI InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'kleinModels
kleinMorphs
kleinCompilerTester
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'kleinModels' From: 'applications/klein'
 bootstrap read: 'kleinMorphs' From: 'applications/klein'
 bootstrap read: 'kleinCompilerTester' From: 'applications/klein'



 '-- Side effects'

 globals modules kleinUI postFileIn
