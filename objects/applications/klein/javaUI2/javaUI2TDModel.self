 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'Category: models\x7fCategory: collections of slots\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         typeDclModel = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaUI2 abstractCategoryModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 typeDclModel.

CopyDowns:
globals javaUI2 abstractCategoryModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 typeDclModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'Category: accessing the class or interface\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         classOrInterface = ( |
            | 
            myClassOrInterface classOrInterface).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'Category: accessing the class or interface\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         classOrInterfaceName = ( |
            | myClassOrInterface name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'Category: comment\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         comment = ( |
            | 
            myClassOrInterface comment).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         isCategoryModel = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         isJavaTypeDclModel = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         mirror = ( |
            | 
            referrent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'Category: accessing the class or interface\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         myClassOrInterface = ( |
            | referrent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'Category: copying and creating\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         newOutlinerFor: aClassOrInterface = ( |
            | 
            == javaUI2 typeDclModel
             ifFalse: [^ resend.newOutlinerFor: aClassOrInterface].

            (aClassOrInterface classOrInterface = 'class'
             ifTrue: [javaUI2 classModel]
              False: [javaUI2 interfaceModel]
            ) newOutlinerFor: aClassOrInterface).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'Category: accessing the class or interface\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         receiver = ( |
            | referrent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'Category: dropping\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldWrapMeWhenGrowingSkin = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'Category: accessing the class or interface\x7fComment: Slots in exactly this category.
-- Ungar, 2/4/95
Override because slots only know category within Java class, etc.
-- Ungar 4/01
\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot\x7fVisibility: public'
        
         slotsInMe = ( |
            | 
            slotsInMirror asList copyFilteredBy: [|:s|
              '' = s category]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'Category: accessing the class or interface\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         slotsInMirror = ( |
            | 
            myClassOrInterface slots).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'Category: title\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot\x7fVisibility: private'
        
         titleFontSpec = ( |
            | 
            isRootOutliner ifTrue: [fontSpec copyName: 'times' Size: 14 Style: 'bold']
                            False: [resend.titleFontSpec]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> () From: ( | {
         'Category: title\x7fModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         titleString = ( |
            | referrent longKey).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         javaUI2TDModel = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2TDModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2TDModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaUI2TDModel.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2TDModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaUI2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2TDModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2TDModel InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2TDModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2TDModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2TDModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2TDModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2TDModel InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'javaUI2ClassModel
javaUI2InterfModel
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'javaUI2ClassModel' From: 'applications/klein/javaUI2'
 bootstrap read: 'javaUI2InterfModel' From: 'applications/klein/javaUI2'



 '-- Side effects'

 globals modules javaUI2TDModel postFileIn
