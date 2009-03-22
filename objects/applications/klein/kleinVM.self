 '$Revision: 30.84 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: virtual machines\x7fModuleInfo: Module: kleinVM InitialContents: FollowSlot\x7fVisibility: public'
        
         virtualMachines = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> () From: ( | {
         'ModuleInfo: Module: kleinVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinVM InitialContents: FollowSlot'
        
         kleinVM = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVM' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVM' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinVM.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVM' -> () From: ( | {
         'ModuleInfo: Module: kleinVM InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVM' -> () From: ( | {
         'ModuleInfo: Module: kleinVM InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVM' -> () From: ( | {
         'ModuleInfo: Module: kleinVM InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVM' -> () From: ( | {
         'ModuleInfo: Module: kleinVM InitialContents: FollowSlot'
        
         postFileIn = ( |
             newSlotsMirror.
            | 
            newSlotsMirror: reflect: ().
            (reflect: process) do: [ |:slot|
              (slot module = 'kleinVM') ifTrue: [
                newSlotsMirror addSlot: slot
              ]
            ].
            (browse childrenOf: traits process) do: [ |:cMirror|
              cMirror slotAt: 'myVM' IfAbsent:[
                cMirror addSlots: newSlotsMirror.
              ].
            ].
            resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVM' -> () From: ( | {
         'ModuleInfo: Module: kleinVM InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.84 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVM' -> () From: ( | {
         'ModuleInfo: Module: kleinVM InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'kleinSelfVM
kleinTestVM
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'kleinSelfVM' From: 'applications/klein'
 bootstrap read: 'kleinTestVM' From: 'applications/klein'



 '-- Side effects'

 globals modules kleinVM postFileIn
