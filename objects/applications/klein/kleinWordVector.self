 '$Revision: 30.4 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fCategory: mirrors\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         machineLevelWordVector = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein machineLevelWordVector.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> () From: ( | {
         'ModuleInfo: Module: kleinWordVector InitialContents: InitializeToExpression: (0)'
        
         baseAddress <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> () From: ( | {
         'ModuleInfo: Module: kleinWordVector InitialContents: InitializeToExpression: (nil)'
        
         machineMemory.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> () From: ( | {
         'ModuleInfo: Module: kleinWordVector InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein machineLevelWordVector parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         == x = ( |
            | 
            _Eq: x
            "(machineMemory = x machineMemory)
            && [(baseAddress = x baseAddress)
            && [size = x size]]").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'Category: reflecteePrimitives\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         addressAt: idx = ( |
            | baseAddress + (klein layouts abstract oopSize  * idx)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         copyMachineMemory: mm Address: a Size: s = ( |
            | 
            ((copy machineMemory: mm)
            baseAddress: a)
            size: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'Category: reflecteePrimitives\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | machineMemory hash ^^ baseAddress hash ^^ size hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'Category: annotations\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         isOKToTransformAnnotationAfterParsing = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'Category: reflecteePrimitives\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         namesIfFail: fb = ( |
            | vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinWordVector InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'mirrors' -> 'vector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'Category: reflecteePrimitives\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeAt: idx IfFail: errBlk = ( |
            | 
            machineMemory wordAt: (addressAt: idx) IfFail: errBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'Category: reflecteePrimitives\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeAt: idx Put: val IfFail: errBlk = ( |
            | 
            machineMemory at: (addressAt: idx)
                     PutWord: val IfFail: errBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'Category: reflecteePrimitives\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeMethodPointerIfFail: fb = ( |
            | 
            fb value: 'you must be kidding').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'Category: reflecteePrimitives\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeMirrorAt: idx IfFail: errBlk = ( |
            | 
            reflect: reflecteeAt: idx IfFail: [|:e| ^ errBlk value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'Category: reflecteePrimitives\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeSizeIfFail: errBlk = ( |
            | 
            size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         safeName = ( |
            | 
            size printString, ' words at: 0x', baseAddress hexPrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> 'parent' -> () From: ( | {
         'Category: reflecteePrimitives\x7fModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         unparsedAnnotationIfFail: fb = ( |
            | '').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelWordVector' -> () From: ( | {
         'ModuleInfo: Module: kleinWordVector InitialContents: InitializeToExpression: (0)'
        
         size <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinWordVector InitialContents: FollowSlot'
        
         kleinWordVector = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinWordVector' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinWordVector' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinWordVector.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinWordVector' -> () From: ( | {
         'ModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinWordVector' -> () From: ( | {
         'ModuleInfo: Module: kleinWordVector InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinWordVector' -> () From: ( | {
         'ModuleInfo: Module: kleinWordVector InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinWordVector' -> () From: ( | {
         'ModuleInfo: Module: kleinWordVector InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinWordVector' -> () From: ( | {
         'ModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.4 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinWordVector' -> () From: ( | {
         'ModuleInfo: Module: kleinWordVector InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinWordVector postFileIn
