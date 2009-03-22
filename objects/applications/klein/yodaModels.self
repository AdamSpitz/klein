 '$Revision: 30.5 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: FollowSlot'
        
         yodaModels = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaModels' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaModels' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaModels.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaModels' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaModels' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaModels' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaModels' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaModels' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.5 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaModels' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: models for debugger\x7fModuleInfo: Module: yodaModels InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcessModel = bootstrap define: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcessModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda foreignProcessModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcessModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda foreignProcessModel.

CopyDowns:
globals kleinAndYoda foreignProcessModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcessModel' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda foreignProcessModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: FollowSlot\x7fVisibility: private'
        
         myVMKit = ( |
            | yoda).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: FollowSlot\x7fVisibility: public'
        
         titleString = ( |
            | 
            'Yoda ', resend.titleString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: models for debugger\x7fModuleInfo: Module: yodaModels InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcessStackModel = bootstrap define: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcessStackModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda foreignProcessStackModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcessStackModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda foreignProcessStackModel.

CopyDowns:
globals kleinAndYoda foreignProcessStackModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcessStackModel' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda foreignProcessStackModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: FollowSlot\x7fVisibility: private'
        
         myVMKit = ( |
            | yoda).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> ().
        } | ) 



 '-- Side effects'

 globals modules yodaModels postFileIn
