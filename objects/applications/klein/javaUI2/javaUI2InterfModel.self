 '$Revision: 30.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'Category: models\x7fCategory: collections of slots\x7fModuleInfo: Module: javaUI2InterfModel InitialContents: FollowSlot'
        
         interfaceModel = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'interfaceModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaUI2 typeDclModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'interfaceModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 interfaceModel.

CopyDowns:
globals javaUI2 typeDclModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'interfaceModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2InterfModel InitialContents: FollowSlot'
        
         myInterface = ( |
            | myClassOrInterface).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'interfaceModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2InterfModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'interfaceModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 interfaceModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'interfaceModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2InterfModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaUI2' -> 'typeDclModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaUI2InterfModel InitialContents: FollowSlot'
        
         javaUI2InterfModel = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2InterfModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2InterfModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaUI2InterfModel.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2InterfModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2InterfModel InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaUI2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2InterfModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2InterfModel InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2InterfModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2InterfModel InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2InterfModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2InterfModel InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2InterfModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2InterfModel InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2InterfModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2InterfModel InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules javaUI2InterfModel postFileIn
