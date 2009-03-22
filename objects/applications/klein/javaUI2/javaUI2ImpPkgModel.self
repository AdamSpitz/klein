 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'Category: models\x7fModuleInfo: Module: javaUI2ImpPkgModel InitialContents: FollowSlot'
        
         importOrPackageDclModel = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importOrPackageDclModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalSlotModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importOrPackageDclModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 importOrPackageDclModel.

CopyDowns:
globals generalSlotModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importOrPackageDclModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpPkgModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importOrPackageDclModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 importOrPackageDclModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importOrPackageDclModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpPkgModel InitialContents: FollowSlot'
        
         commentButtonText = ( |
            | 
            [todo cleanup "should be factored out for all java models"].
            '/* ... */').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importOrPackageDclModel' -> 'parent' -> () From: ( | {
         'Category: sprouting\x7fModuleInfo: Module: javaUI2ImpPkgModel InitialContents: FollowSlot\x7fVisibility: public'
        
         outlinerForSprouting: evt IfNone: nb = ( |
            | 
            slot exists ifFalse: [|m|
              m: 'Sorry, but that slot does not exist (any more).'.
              (message copy receiver: userQuery 
                            Selector: 'report:'
                                With: m
              ) forkForBirthEvent: evt.
            ^ nb value: m.
            ].
            javaUI2 compilationUnitModel
               outlinerFor: (javaParser compilationUnit fromPackageNamed: slot value)
                   InWorld:  evt sourceHand world).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importOrPackageDclModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpPkgModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalSlotModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'importOrPackageDclModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpPkgModel InitialContents: FollowSlot'
        
         showSlotContents = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpPkgModel InitialContents: FollowSlot'
        
         javaUI2ImpPkgModel = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpPkgModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpPkgModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaUI2ImpPkgModel.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpPkgModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpPkgModel InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaUI2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpPkgModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpPkgModel InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpPkgModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpPkgModel InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpPkgModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpPkgModel InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpPkgModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpPkgModel InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2ImpPkgModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2ImpPkgModel InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'javaUI2PkgModel
javaUI2ImpDclModel
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'javaUI2PkgModel' From: 'applications/klein/javaUI2'
 bootstrap read: 'javaUI2ImpDclModel' From: 'applications/klein/javaUI2'



 '-- Side effects'

 globals modules javaUI2ImpPkgModel postFileIn
