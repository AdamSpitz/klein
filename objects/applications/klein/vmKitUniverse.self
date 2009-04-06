 '$Revision: 30.6 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         universe = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda base copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda universe.

CopyDowns:
globals kleinAndYoda base. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'Category: universe state\x7fModuleInfo: Module: vmKitUniverse InitialContents: InitializeToExpression: (identitySet copyRemoveAll)'
        
         allNMethods <- identitySet copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'Category: universe state\x7fModuleInfo: Module: vmKitUniverse InitialContents: InitializeToExpression: (nil)'
        
         allocationSpace.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'Category: universe state\x7fModuleInfo: Module: vmKitUniverse InitialContents: InitializeToExpression: (customizableSet copyRemoveAll)'
        
         canonicalizedStrings <- customizableSet copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'Category: universe state\x7fModuleInfo: Module: vmKitUniverse InitialContents: InitializeToExpression: (kleinAndYoda cardTable)'
        
         cardTable <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cardTable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'Category: universe state\x7fModuleInfo: Module: vmKitUniverse InitialContents: InitializeToExpression: (nil)'
        
         floatMap.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'Category: universe state\x7fModuleInfo: Module: vmKitUniverse InitialContents: InitializeToExpression: (kleinAndYoda newGeneration)'
        
         newGeneration <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'newGeneration' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'Category: universe state\x7fModuleInfo: Module: vmKitUniverse InitialContents: InitializeToExpression: (vector)'
        
         objectsByOID <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'Category: universe state\x7fModuleInfo: Module: vmKitUniverse InitialContents: InitializeToExpression: (kleinAndYoda oldGeneration)'
        
         oldGeneration <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'oldGeneration' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'ModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda universe parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: heap\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         allAllocatedRegions = ( |
             v.
            | 
            v: list copyRemoveAll.
            spacesDo: [|:s| v addAll: s allAllocatedRegions].
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: heap\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateHeapAt: base NewGenSize: ngs ScavengeSpaceSize: sss TotalSize: s = ( |
             newGenSize.
             oldGenSize.
             scavSpaceSize.
            | 

               newGenSize: ngs roundUpTo: oopSize.
            scavSpaceSize: sss roundUpTo: oopSize.
               oldGenSize: s - newGenSize - scavSpaceSize.

            newGeneration        allocateHeapAt: base                              Size: newGenSize.
            scavengeGarbageSpace allocateHeapAt: base + newGenSize                 Size: scavSpaceSize.
            oldGeneration        allocateHeapAt: base + newGenSize + scavSpaceSize Size: oldGenSize.

            allocationSpace: edenSpace.

            cardTable: cardTable copyForMaxAddress: base + s.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOops: nOops = ( |
            | 
            allocationSpace allocateOops: nOops).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOops: nOops AndBytes: nBytes = ( |
            | 
            allocationSpace allocateOops: nOops AndBytes: nBytes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: heap\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         allocatedRegionFor: pc = ( |
            | 
            spacesDo: [|:s|
              (s includesAddress: pc) ifTrue: [^ s bottom @ s top].
            ].
            pc @ pc succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: private'
        
         base* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: heap\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesIncludesAddress: addr = ( |
            | 
            generationsDo: [|:g| (g bytesIncludesAddress: addr) ifTrue: [^ true]].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
             c.
            | 
            c: resend.copy.
            generationsDo: [|:g| (g name, ':') sendTo: c With: g copy].
            c scavengeGarbageSpace: scavengeGarbageSpace copy.
            c canonicalizedStrings: canonicalizedStrings copy.
            c allNMethods:          allNMethods          copy.
            c compactMapOops:       compactMapOops       copy.
            c compactMaps:          compactMaps          copy.
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: heap\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         copy: n BytesFromAddress: srcAddr ToAddress: dstAddr = ( |
             bv.
            | 
            bv: theVM machineMemory bytesAt: srcAddr Size: n.
            theVM machineMemory at: dstAddr PutBytes: bv.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForLaunch = ( |
             c.
            | 
            c: clone.
            generationsDo: [|:g| (g name, ':') sendTo: c With: g copyForLaunch].
            c scavengeGarbageSpace: scavengeGarbageSpace copyForLaunch.
            theVM image reassociateKleinObjectFrom: self 
                                                To: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForVM: aVM = ( |
            | 
            copy initializeForVM: aVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: memory interface\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         createBufferMemoryInterface = ( |
            | 
            createBufferMemoryInterfaceUsingPrototype:
              theVM vmKit growingBufferMemoryInterface).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: memory interface\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         createBufferMemoryInterfaceUsingPrototype: miProto = ( |
             new.
             old.
             scav.
            | 
            scav: scavengeGarbageSpace createBufferMemoryInterfaceUsingPrototype: miProto.
            new:  newGeneration        createBufferMemoryInterfaceUsingPrototype: miProto.
            old:  oldGeneration        createBufferMemoryInterfaceUsingPrototype: miProto.

            theVM vmKit compositeMemoryInterface
              copyForLowMemory: new
                    HighMemory:
                       theVM vmKit compositeMemoryInterface
                         copyForLowMemory: scav
                               HighMemory: old).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: updating\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlotsToFixUpIn: universeMir Do: blk = ( |
            | 
            generationsDo: [|:g|
              g dataSlotsToFixUpIn: (universeMir primitiveContentsAt: g name) Do: blk.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         edenSpace = ( |
            | 
            newGeneration edenSpace).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: heap\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         generationsDo: blk = ( |
            | 
            [newGeneration. newGeneration: nil]. "browsing - we sometimes use the name of the generation as a slot name"
            [oldGeneration. oldGeneration: nil]. "browsing - we sometimes use the name of the generation as a slot name"
            blk value: newGeneration.
            blk value: oldGeneration.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: heap\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         getReadyToStartAllocating = ( |
            | 
            spacesDo: [|:s| s getReadyToStartAllocating].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: reflective mutation\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: private'
        
         incrementProgrammingTimestamp = ( |
            | programmingTimestamp: programmingTimestamp succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForVM: aVM = ( |
            | 
            generationsDo: [|:g| g initializeForVM: aVM].
            scavengeGarbageSpace: edenSpace copy name: 'scavengeGarbage'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         isScavenging = ( |
            | 
            allocationSpace == scavengeGarbageSpace).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: heap\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         objsTopFor: start = ( |
            | 
            spacesDo: [|:sp|
              (sp includesAddress: start) ifTrue: [^ sp objsTop].
            ].
            error: 'what?').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: heap\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         oopsIncludesAddress: addr = ( |
            | 
            generationsDo: [|:g| (g oopsIncludesAddress: addr) ifTrue: [^ true]].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: heap\x7fComment: invoke blk with contents and addr of each oop matching the desiredOop\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         oopsMatchingMemOop: desiredOop Do: blk = ( |
            | 
            spacesDo: [|:s| s oopsMatchingMemOop: desiredOop Do: blk].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: genesis\x7fCategory: helpers\x7fComment: unused?\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         spaceSizes = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> 'spaceSizes' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda base copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> 'spaceSizes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda universe parent spaceSizes.

CopyDowns:
globals kleinAndYoda base. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: heap\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         spacesDo: blk = ( |
            | 
            generationsDo: [|:g| g spacesDo: blk].

            [scavengeGarbageSpace. scavengeGarbageSpace: nil]. "browsing"
            blk value: scavengeGarbageSpace With: 'scavengeGarbage'.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fCategory: switching spaces\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         switchAllocationSpaceTo: aSpace = ( |
             previousSpace.
            | 
            previousSpace: allocationSpace.

            __BranchIfFalse: (previousSpace _Eq: aSpace) To: 'notAlreadyUsingThatSpace'.
            _Breakpoint: 'Already using that space. Maybe we ran out of memory in the middle of a GC?'.

            __DefineLabel: 'notAlreadyUsingThatSpace'.
            allocationSpace: aSpace.
            previousSpace).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         tenuredSpace = ( |
            | 
            oldGeneration tenuredSpace).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'Category: universe state\x7fModuleInfo: Module: vmKitUniverse InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         programmingTimestamp <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'Category: universe state\x7fModuleInfo: Module: vmKitUniverse InitialContents: InitializeToExpression: (nil)'
        
         scavengeGarbageSpace.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'Category: universe state\x7fModuleInfo: Module: vmKitUniverse InitialContents: InitializeToExpression: (nil)'
        
         smiMap.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: initializing\x7fComment: Allocates a new heap from machineMemory of the
specified size.  Must be called exactly once before
objects are allocated within the space.\x7fModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateHeapAt: base Size: s = ( |
            | 
            [todo optimize gc multipleSpaces]. "What's the right way to divide the space? -- Adam, 5/06"

            universe allocateHeapAt: base
                         NewGenSize: s / 8
                  ScavengeSpaceSize: s / 4  "big because the scavenger creates a lot of garbage for now -- Adam, Mar, 2009"
                          TotalSize: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot'
        
         vmKitUniverse = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUniverse' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUniverse' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitUniverse.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUniverse' -> () From: ( | {
         'ModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUniverse' -> () From: ( | {
         'ModuleInfo: Module: vmKitUniverse InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUniverse' -> () From: ( | {
         'ModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUniverse' -> () From: ( | {
         'ModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUniverse' -> () From: ( | {
         'ModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.6 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUniverse' -> () From: ( | {
         'ModuleInfo: Module: vmKitUniverse InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitVarHdrsUniv
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitVarHdrsUniv' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitUniverse postFileIn
