 '$Revision:$'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: memory interfaces\x7fModuleInfo: Module: vmKitLclMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         localMemoryInterface = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localMemoryInterface' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda abstractMemoryInterface copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localMemoryInterface' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda localMemoryInterface.

CopyDowns:
globals kleinAndYoda abstractMemoryInterface. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localMemoryInterface' -> () From: ( | {
         'ModuleInfo: Module: vmKitLclMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localMemoryInterface' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda localMemoryInterface parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single oops\x7fModuleInfo: Module: vmKitLclMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         at: addr PutOop: oop IfFail: fb = ( |
            | 
            oop _UnsafePutOopAtAddress: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: marks\x7fModuleInfo: Module: vmKitLclMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         markValueAt: addr IfFail: fb = ( |
            | 
            addr _UnsafeMarkValueAtAddressIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: single oops\x7fModuleInfo: Module: vmKitLclMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         oopAt: addr IfFail: fb = ( |
            | 
            _UnsafeObjectForOopAtAddress: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localMemoryInterface' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitLclMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMemoryInterface' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localMemoryInterface' -> 'parent' -> () From: ( | {
         'Category: tags\x7fModuleInfo: Module: vmKitLclMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         tagAt: addr IfFail: fb = ( |
            | 
            _UnsafeTagOfOopAtAddress: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitLclMemIntrface InitialContents: FollowSlot'
        
         vmKitLclMemIntrface = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLclMemIntrface' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLclMemIntrface' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitLclMemIntrface.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLclMemIntrface' -> () From: ( | {
         'ModuleInfo: Module: vmKitLclMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLclMemIntrface' -> () From: ( | {
         'ModuleInfo: Module: vmKitLclMemIntrface InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLclMemIntrface' -> () From: ( | {
         'ModuleInfo: Module: vmKitLclMemIntrface InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLclMemIntrface' -> () From: ( | {
         'ModuleInfo: Module: vmKitLclMemIntrface InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLclMemIntrface' -> () From: ( | {
         'ModuleInfo: Module: vmKitLclMemIntrface InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision:$'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLclMemIntrface' -> () From: ( | {
         'ModuleInfo: Module: vmKitLclMemIntrface InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitLclMemIntrface postFileIn
