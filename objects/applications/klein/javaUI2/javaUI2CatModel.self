 '$Revision: 30.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'Category: models\x7fCategory: collections of slots\x7fModuleInfo: Module: javaUI2CatModel InitialContents: FollowSlot'
        
         categoryModel = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'categoryModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaUI2 abstractCategoryModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'categoryModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 categoryModel.

CopyDowns:
globals javaUI2 abstractCategoryModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'categoryModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CatModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'categoryModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 categoryModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'categoryModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CatModel InitialContents: FollowSlot'
        
         isJavaCategoryModel = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'categoryModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CatModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CatModel InitialContents: FollowSlot'
        
         javaUI2CatModel = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CatModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CatModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaUI2CatModel.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CatModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CatModel InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaUI2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CatModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CatModel InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CatModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CatModel InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CatModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CatModel InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CatModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CatModel InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CatModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CatModel InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules javaUI2CatModel postFileIn
