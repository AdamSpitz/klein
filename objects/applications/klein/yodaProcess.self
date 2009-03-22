 '$Revision: 30.9 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaProcess InitialContents: FollowSlot'
        
         yodaProcess = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProcess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProcess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaProcess.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProcess' -> () From: ( | {
         'ModuleInfo: Module: yodaProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProcess' -> () From: ( | {
         'ModuleInfo: Module: yodaProcess InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProcess' -> () From: ( | {
         'ModuleInfo: Module: yodaProcess InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProcess' -> () From: ( | {
         'ModuleInfo: Module: yodaProcess InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProcess' -> () From: ( | {
         'ModuleInfo: Module: yodaProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.9 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProcess' -> () From: ( | {
         'ModuleInfo: Module: yodaProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fModuleInfo: Module: yodaProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcess = bootstrap define: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda foreignProcess copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda foreignProcess.

CopyDowns:
globals kleinAndYoda foreignProcess. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> () From: ( | {
         'ModuleInfo: Module: yodaProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda foreignProcess parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: yodaProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         currentActivation = ( |
             oop.
            | 
            oop: getYodaWellKnownObjectsWordAt: myVMKit layouts wellKnownObjects offsetOfOopForActiveContext
                                        IfFail: [^ mirrors methodActivation ].
            0 = oop  ifTrue: [^ mirrors methodActivation ].
            myVM mirrorFor: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: yodaProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         getYodaWellKnownObjectsAddress = ( |
            | 
            getYodaWellKnownObjectsAddressIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: yodaProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         getYodaWellKnownObjectsAddressIfFail: fb = ( |
            | 
            myProxy ifNil: [fb value: 'no proxy'].
            safelyDo: [safeProxy getYodaWellKnownObjectsAddressIfFail: [|:e| ^ fb value: e]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: yodaProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         getYodaWellKnownObjectsWordAt: offset = ( |
            | 
            getYodaWellKnownObjectsWordAt: offset IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: yodaProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         getYodaWellKnownObjectsWordAt: offset IfFail: fb = ( |
            | 
            readMemoryWordAt:
                offset
              + (getYodaWellKnownObjectsAddressIfFail: [|:e| ^ fb value: e])
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaProcess InitialContents: FollowSlot'
        
         myVMKit = ( |
            | 
            yoda).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: yodaProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         objectLocatorTimestampIfFail: fb = ( |
            | 
            myProxy ifNil: [^fb value: 'no proxy'].
            safelyDo: [safeProxy objectLocatorTimestampIfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fCategory: preemption\x7fModuleInfo: Module: yodaProcess InitialContents: FollowSlot'
        
         resetPreemption = ( |
            | 
            [todo yodaDebugger stepping dmu].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fComment: Returns the address of the first indexable in the
object table. -- Adam, 7/06\x7fModuleInfo: Module: yodaProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         startOfObjectAddressesIfFail: fb = ( |
             ola.
            | 
            ola: getYodaWellKnownObjectsWordAt: myVMKit layouts wellKnownObjects offsetOfAddressForObjectLocator
                                        IfFail: [|:e| ^ fb value: e].
            ola + (myVMKit layouts objVector oopSize * myVM image objectsOracle objectLocatorIndexableOrigin)).
        } | ) 



 '-- Side effects'

 globals modules yodaProcess postFileIn
