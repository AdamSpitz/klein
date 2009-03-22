 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'Category: models\x7fCategory: slots\x7fModuleInfo: Module: javaUI2ASModel InitialContents: FollowSlot'
        
         abstractSlotModel = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractSlotModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalSlotModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractSlotModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 abstractSlotModel.

CopyDowns:
globals generalSlotModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractSlotModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ASModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractSlotModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 abstractSlotModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractSlotModel' -> 'parent' -> () From: ( | {
         'Category: comment\x7fModuleInfo: Module: javaUI2ASModel InitialContents: FollowSlot'
        
         commentButtonText = ( |
            | 
            [todo cleanup "should be factored out for all java models"].
            '/* ... */').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractSlotModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ASModel InitialContents: FollowSlot\x7fVisibility: private'
        
         methodText = ( |
            | 
            javaUI2 javaMethodText).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractSlotModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ASModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalSlotModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractSlotModel' -> 'parent' -> () From: ( | {
         'Category: comment\x7fModuleInfo: Module: javaUI2ASModel InitialContents: FollowSlot'
        
         sliceGroupModel = ( |
            | javaUI2 sliceGroupModel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ASModel InitialContents: FollowSlot'
        
         javaUI2ASModel = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ASModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ASModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaUI2ASModel.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ASModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ASModel InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaUI2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ASModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ASModel InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ASModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ASModel InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ASModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ASModel InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ASModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ASModel InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ASModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ASModel InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'javaUI2SlotModel
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'javaUI2SlotModel' From: 'applications/klein/javaUI2'



 '-- Side effects'

 globals modules javaUI2ASModel postFileIn
