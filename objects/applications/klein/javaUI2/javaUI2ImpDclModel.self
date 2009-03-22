 '$Revision: 30.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'Category: models\x7fModuleInfo: Module: javaUI2ImpDclModel InitialContents: FollowSlot'
        
         importDclModel = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importDclModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaUI2 importOrPackageDclModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importDclModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 importDclModel.

CopyDowns:
globals javaUI2 importOrPackageDclModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importDclModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpDclModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importDclModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 importDclModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importDclModel' -> 'parent' -> () From: ( | {
         'Category: comment\x7fModuleInfo: Module: javaUI2ImpDclModel InitialContents: FollowSlot'
        
         comment = ( |
            | importDcl comment).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importDclModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpDclModel InitialContents: FollowSlot'
        
         importDcl = ( |
            | referrent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importDclModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpDclModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaUI2' -> 'importOrPackageDclModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpDclModel InitialContents: FollowSlot'
        
         javaUI2ImpDclModel = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpDclModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpDclModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaUI2ImpDclModel.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpDclModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpDclModel InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaUI2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpDclModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpDclModel InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpDclModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpDclModel InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpDclModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpDclModel InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpDclModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpDclModel InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpDclModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpDclModel InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules javaUI2ImpDclModel postFileIn
