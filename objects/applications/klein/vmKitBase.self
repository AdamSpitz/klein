 '$Revision: 30.5 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: base \"class\"\x7fModuleInfo: Module: vmKitBase InitialContents: FollowSlot\x7fVisibility: public'
        
         base = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> () From: ( |
             {} = 'Comment: Provides some basic services that klein objects
can choose to inherit to reduce duplication.\x7fModuleInfo: Creator: globals kleinAndYoda base.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> () From: ( | {
         'ModuleInfo: Module: vmKitBase InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda base parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> () From: ( | {
         'Category: common stuff\x7fModuleInfo: Module: vmKitBase InitialContents: FollowSlot\x7fVisibility: public'
        
         ^^^ anOop = ( |
            | 
            mapOf: anOop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitBase InitialContents: FollowSlot\x7fVisibility: private'
        
         layouts = ( |
            | 
            vmKit layouts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> () From: ( | {
         'Category: common stuff\x7fModuleInfo: Module: vmKitBase InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOf: anOop = ( |
            | 
            mapOf: anOop IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> () From: ( | {
         'Category: common stuff\x7fModuleInfo: Module: vmKitBase InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOf: anOop IfFail: fb = ( |
            | 
            layouts object mapOf: anOop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitBase InitialContents: FollowSlot\x7fVisibility: private'
        
         maps = ( |
            | 
            vmKit maps).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> () From: ( | {
         'Category: common stuff\x7fModuleInfo: Module: vmKitBase InitialContents: FollowSlot\x7fVisibility: public'
        
         oopSize = ( |
            | 
            layouts abstract oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitBase InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitBase InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitBase InitialContents: FollowSlot'
        
         vmKitBase = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitBase' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitBase' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitBase.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitBase' -> () From: ( | {
         'ModuleInfo: Module: vmKitBase InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitBase' -> () From: ( | {
         'ModuleInfo: Module: vmKitBase InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitBase' -> () From: ( | {
         'ModuleInfo: Module: vmKitBase InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitBase' -> () From: ( | {
         'ModuleInfo: Module: vmKitBase InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitBase' -> () From: ( | {
         'ModuleInfo: Module: vmKitBase InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.5 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitBase' -> () From: ( | {
         'ModuleInfo: Module: vmKitBase InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitBase postFileIn
