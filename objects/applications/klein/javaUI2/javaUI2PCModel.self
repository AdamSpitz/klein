 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'Category: models\x7fCategory: collections of slots\x7fModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot'
        
         pseudoCategoryModel = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'pseudoCategoryModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaUI2 abstractCategoryModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'pseudoCategoryModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 pseudoCategoryModel.

CopyDowns:
globals javaUI2 abstractCategoryModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'pseudoCategoryModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'pseudoCategoryModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 pseudoCategoryModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'pseudoCategoryModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot'
        
         comment = ( |
            | 
            [todo unimplemented]. '').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'pseudoCategoryModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot'
        
         isCategoryModel = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'pseudoCategoryModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot\x7fVisibility: public'
        
         isPseudoCategoryModel = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'pseudoCategoryModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot'
        
         mirror = ( |
            | referrent mirror).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'pseudoCategoryModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'pseudoCategoryModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot'
        
         pseudoCategories = ( |
            | vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'pseudoCategoryModel' -> 'parent' -> () From: ( | {
         'Comment: no need to filter assignable slots\x7fModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot'
        
         shownSlotsInMe = ( |
            | slotsInMe).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'pseudoCategoryModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot'
        
         slotsInMe = ( |
            | slotsInMeAndSubcategories).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'pseudoCategoryModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot'
        
         slotsInMeAndSubcategories = ( |
            | 
            case
              if:   [categoriesString = javaParser importDcl category]
              Then: [referrent mirror importDcls]
              Else: [resend.slotsInMe]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot'
        
         javaUI2PCModel = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2PCModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2PCModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaUI2PCModel.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2PCModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaUI2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2PCModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2PCModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2PCModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2PCModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2PCModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2PCModel InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules javaUI2PCModel postFileIn
