 '$Revision: 30.3 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> 'spaceSizes' -> () From: ( | {
         'Comment: This is really much too big, but we leave it this way
for now to facilitate testing of Klein.
At last estimate (Dec 03) Klein took up ~ 2680650 bytes.\x7fModuleInfo: Module: vmKitMemory InitialContents: InitializeToExpression: (10000000)'
        
         edenSize <- 10000000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> 'spaceSizes' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> 'spaceSizes' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda universe parent spaceSizes parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> 'spaceSizes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot'
        
         base* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> 'spaceSizes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot\x7fVisibility: public'
        
         cleanup = ( |
            | 
            roundToIdealizedPageSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> 'spaceSizes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | resend.copy initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> 'spaceSizes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            roundToPageSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> 'spaceSizes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot\x7fVisibility: private'
        
         mapSizesBy: blk = ( |
            | 
            sizesDo: [|:s. :setter|
              setter value: blk value: s].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> 'spaceSizes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot'
        
         pageSize = ( |
            | 
            [todo gc]. "move to base, ask vm which asks machineMemory, which
            asks proxy, etc."
            16 * 1024).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> 'spaceSizes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot\x7fVisibility: private'
        
         roundToPageSize = ( |
            | 
            mapSizesBy: [|:s| s roundUpTo: pageSize].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> 'spaceSizes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot\x7fVisibility: private'
        
         sizesDo: blk = ( |
            | 
            blk value: edenSize With: [|:x| edenSize: x]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot'
        
         vmKitMemory = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemory' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemory' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitMemory.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemory' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemory' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemory' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemory' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemory' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.3 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMemory' -> () From: ( | {
         'ModuleInfo: Module: vmKitMemory InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitSpace
vmKitGeneration
vmKitUniverse
vmKitVerifier
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitSpace' From: 'applications/klein'
 bootstrap read: 'vmKitGeneration' From: 'applications/klein'
 bootstrap read: 'vmKitUniverse' From: 'applications/klein'
 bootstrap read: 'vmKitVerifier' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitMemory postFileIn
