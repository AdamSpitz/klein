 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'Category: models\x7fCategory: collections of slots\x7fModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot'
        
         abstractCategoryModel = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalCategoryModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 abstractCategoryModel.

CopyDowns:
globals generalCategoryModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 abstractCategoryModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> () From: ( | {
         'Category: updating\x7fCategory: updater objects\x7fModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot\x7fVisibility: private'
        
         categoriesUpdater = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'categoriesUpdater' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalCategoryModel parent categoriesUpdater copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'categoriesUpdater' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 abstractCategoryModel parent categoriesUpdater.

CopyDowns:
globals generalCategoryModel parent categoriesUpdater. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'categoriesUpdater' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'categoriesUpdater' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 abstractCategoryModel parent categoriesUpdater parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'categoriesUpdater' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot'
        
         modelPrototype = ( |
            | 
            javaUI2 categoryModel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'categoriesUpdater' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'generalCategoryModel' -> 'parent' -> 'categoriesUpdater' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> () From: ( | {
         'Category: adding things\x7fModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot\x7fVisibility: public'
        
         categoryReferrentProto = ( |
            | 
            javaUI2 javaCategoryReferrent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> () From: ( | {
         'Category: comment\x7fModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot'
        
         commentButtonText = ( |
            | 
            [todo cleanup "should be factored out for all java models"].
            '/* ... */').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalCategoryModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> () From: ( | {
         'Category: updating\x7fCategory: updater objects\x7fModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot'
        
         pseudoCategoriesUpdater = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'pseudoCategoriesUpdater' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalCategoryModel parent pseudoCategoriesUpdater copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'pseudoCategoriesUpdater' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 abstractCategoryModel parent pseudoCategoriesUpdater.

CopyDowns:
globals generalCategoryModel parent pseudoCategoriesUpdater. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'pseudoCategoriesUpdater' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'pseudoCategoriesUpdater' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 abstractCategoryModel parent pseudoCategoriesUpdater parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'pseudoCategoriesUpdater' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot'
        
         modelPrototypeForThing: t = ( |
            | 
            javaUI2 pseudoCategoryModel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'pseudoCategoriesUpdater' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalCategoryModel' -> 'parent' -> 'pseudoCategoriesUpdater' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> () From: ( | {
         'Category: yanking\x7fModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot'
        
         sliceGroupModel = ( |
            | javaUI2 sliceGroupModel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> () From: ( | {
         'Category: updating\x7fCategory: updater objects\x7fModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot\x7fVisibility: private'
        
         slotsUpdater = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'slotsUpdater' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalCategoryModel parent slotsUpdater copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'slotsUpdater' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 abstractCategoryModel parent slotsUpdater.

CopyDowns:
globals generalCategoryModel parent slotsUpdater. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'slotsUpdater' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'slotsUpdater' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 abstractCategoryModel parent slotsUpdater parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'slotsUpdater' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot'
        
         modelPrototypeForThing: t = ( |
            | 
            "t is a slot-like think, a node summary"
            case
              if: [t isImportDcl ] Then: [javaUI2 importDclModel]
              Else: [ javaUI2 slotModel]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'slotsUpdater' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'generalCategoryModel' -> 'parent' -> 'slotsUpdater' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot'
        
         javaUI2ACModel = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ACModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ACModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaUI2ACModel.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ACModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaUI2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ACModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ACModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ACModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ACModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ACModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ACModel InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'javaUI2TDModel
javaUI2CatModel
javaUI2PCModel'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'javaUI2TDModel' From: 'applications/klein/javaUI2'
 bootstrap read: 'javaUI2CatModel' From: 'applications/klein/javaUI2'
 bootstrap read: 'javaUI2PCModel' From: 'applications/klein/javaUI2'



 '-- Side effects'

 globals modules javaUI2ACModel postFileIn
