 '$Revision:$'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: garbage collection\x7fModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: public'
        
         cardTable = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals byteVector copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda cardTable.

CopyDowns:
globals byteVector. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> () From: ( | {
         'ModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda cardTable parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: public'
        
         cardSize = ( |
            | 
            1 << shift).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForMaxAddress: maxAddr = ( |
            | 
            copySize:  maxAddr /+ cardSize  FillingWith:  valueForUnchangedCard).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfCardContainingAddress: addr = ( |
            | 
            addr >> shift).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: private'
        
         oopSize = ( |
            | 
            vmKit layouts abstract oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'byteVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: public'
        
         shift = 7.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForChangedCard = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForUnchangedCard = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            kleinAndYoda).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> () From: ( | {
         'ModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: public'
        
         storeStringNeeds = ( |
            | 
            kleinAndYoda cardTable).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot'
        
         vmKitCardTable = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCardTable' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCardTable' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitCardTable.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCardTable' -> () From: ( | {
         'ModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCardTable' -> () From: ( | {
         'ModuleInfo: Module: vmKitCardTable InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCardTable' -> () From: ( | {
         'ModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCardTable' -> () From: ( | {
         'ModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCardTable' -> () From: ( | {
         'ModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision:$'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCardTable' -> () From: ( | {
         'ModuleInfo: Module: vmKitCardTable InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitCardTable postFileIn
