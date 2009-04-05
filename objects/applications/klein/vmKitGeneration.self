 '$Revision: 30.6 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: generations\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: private'
        
         generation = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda base copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda generation.

CopyDowns:
globals kleinAndYoda base. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda generation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot'
        
         base* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesIncludesAddress: addr = ( |
            | 
            spacesDo: [|:g| (g bytesIncludesAddress: addr) ifTrue: [^ true]].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
             c.
            | 
            c: resend.copy.
            spacesDo: [|:s. :sName| (sName, 'Space:') sendTo: c With: s copy].
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForLaunch = ( |
             c.
            | 
            c: clone.
            spacesDo: [|:s. :sName| (sName, 'Space:') sendTo: c With: s copyForLaunch].
            theVM image reassociateKleinObjectFrom: self 
                                                To: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: updating\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlotsToFixUpIn: generationMir Do: blk = ( |
            | 
            spacesDo: [|:s. :sName|
              s dataSlotsToFixUpIn: (generationMir primitiveContentsAt: sName, 'Space') Do: blk.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeForVM: aVM = ( |
             prefix.
            | 
            prefix: aVM byteVectorLayout isSegregated ifTrue: 'segregated' False: 'unsegregated'.
            spacesDo: [|:s. :sName. proto|
              proto: prefix, sName capitalize, 'Space' sendTo: aVM vmKit.
               sName, 'Space:' sendTo: self With: proto copy.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         oopsIncludesAddress: addr = ( |
            | 
            spacesDo: [|:g| (g oopsIncludesAddress: addr) ifTrue: [^ true]].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         spacesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: generations\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         newGeneration = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'newGeneration' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda generation copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'newGeneration' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda newGeneration.

CopyDowns:
globals kleinAndYoda generation. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'newGeneration' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: InitializeToExpression: (nil)'
        
         edenSpace.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'newGeneration' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: InitializeToExpression: (\'newGeneration\')\x7fVisibility: public'
        
         name <- 'newGeneration'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'newGeneration' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'newGeneration' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda newGeneration parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'newGeneration' -> 'parent' -> () From: ( | {
         'Category: heap\x7fComment: Allocates a new heap from theVM machineMemory of the
specified size.  Must be called exactly once before
objects are allocated within the space.\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateHeapAt: base Size: s = ( |
            | 
            edenSpace allocateHeapAt: base Size: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'newGeneration' -> 'parent' -> () From: ( | {
         'Category: memory interface\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         createBufferMemoryInterfaceUsingPrototype: miProto = ( |
            | 
            edenSpace createBufferMemoryInterfaceUsingPrototype: miProto).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'newGeneration' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'newGeneration' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         spacesDo: blk = ( |
            | 
            [  edenSpace.   edenSpace: nil]. "browsing"
            [  pastSpace.   pastSpace: nil]. "browsing"
            [futureSpace. futureSpace: nil]. "browsing"

            blk value: edenSpace With: 'eden'.
            [
              [todo gc].
              blk value:   pastSpace With: 'past'.
              blk value: futureSpace With: 'future'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: generations\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         oldGeneration = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'oldGeneration' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda generation copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'oldGeneration' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda oldGeneration.

CopyDowns:
globals kleinAndYoda generation. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'oldGeneration' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: InitializeToExpression: (\'oldGeneration\')\x7fVisibility: public'
        
         name <- 'oldGeneration'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'oldGeneration' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'oldGeneration' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda oldGeneration parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'oldGeneration' -> 'parent' -> () From: ( | {
         'Category: heap\x7fComment: Allocates a new heap from theVM machineMemory of the
specified size.  Must be called exactly once before
objects are allocated within the space.\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateHeapAt: base Size: s = ( |
            | 
            tenuredSpace allocateHeapAt: base Size: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'oldGeneration' -> 'parent' -> () From: ( | {
         'Category: memory interface\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         createBufferMemoryInterfaceUsingPrototype: miProto = ( |
            | 
            tenuredSpace createBufferMemoryInterfaceUsingPrototype: miProto).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'oldGeneration' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'oldGeneration' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         spacesDo: blk = ( |
            | 
            [tenuredSpace. tenuredSpace: nil]. "browsing"
            blk value: tenuredSpace With: 'tenured'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'oldGeneration' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: InitializeToExpression: (nil)'
        
         tenuredSpace.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot'
        
         vmKitGeneration = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGeneration' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGeneration' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitGeneration.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGeneration' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGeneration' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGeneration' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGeneration' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGeneration' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.6 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGeneration' -> () From: ( | {
         'ModuleInfo: Module: vmKitGeneration InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitGeneration postFileIn
