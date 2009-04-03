 '$Revision: 30.2 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: garbage collection\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: public'
        
         garbageCollector = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda garbageCollector.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: scavenging\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         copyObject: o Size: nOops ToAddress: newAddr = ( |
             endAddr.
             oldAddr.
             sizeInBytes.
            | 
            oldAddr: layouts memoryObject addressOfMem: o.
            ('about to copy object from ', oldAddr printString, ' to ', newAddr printString, '\n') _StringPrint.
            sizeInBytes: nOops _IntMul: oopSize.
            endAddr: newAddr _IntAdd: sizeInBytes.

            [todo optimize gc untaggedAddresses]. "OK, this is hideously inefficient. Maybe convert to using
                                                   untagged addresses and then just make a primitive to copy one word?"
            0 upTo: sizeInBytes By: oopSize WithoutCloningDo: [|:i. old_a. new_a. w|
              old_a: oldAddr _IntAdd: i.
              new_a: newAddr _IntAdd: i.
              w: intNN copy _UnsafeWordAtAddress: old_a.
              w _UnsafePutWordAtAddress: new_a.
            ].

            (vmKit tag tagOfOop:  intNN copy _UnsafeWordAtAddress: endAddr IfFail: vmKit tag mark) = vmKit tag mark ifFalse: [
              endAddr _UnsafeWriteTrailingMark.
            ].

            [todo gc]. "If we just moved the object table, we'd better keep track of it and change the objectAddressesBaseRegister."

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = ( |
            | 
            layouts abstract intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         layouts = ( |
            | 
            vmKit layouts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: mark stack\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         markStack = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> 'markStack' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda garbageCollector markStack.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> 'markStack' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         objects <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> 'markStack' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> 'markStack' -> () From: ( | {
         'Category: resizing\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: public'
        
         resizeTo: n = ( |
            | 
            objects: objects copySize: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> 'markStack' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         top <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: scavenging\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         movedSomeObjects = ( |
            | 
            [todo gc]. "Are there well-known objects we should be keeping track of?
                        And should we be doing something to make sure the development-side
                        objectLocator updates itself?"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: mark stack\x7fModuleInfo: Module: vmKitGC InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         numberOfObjectsMarked <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: remembered set\x7fModuleInfo: Module: vmKitGC InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         numberOfRememberedObjects <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         oopSize = ( |
            | 
            layouts abstract oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: iterating through oops of an object\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         oopsIn: o Do: blk = ( |
            | 
            o _Map myLayout for: o Do: blk IfFail: raiseError.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: scavenging\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         possiblyLiveOopsDo: blk = ( |
             currentActivation.
             senderActivation.
            | 
            _SaveAllNonVolatileRegisters.
            currentActivation: theVM vmKit mirrors methodActivation
                                   activationForLocalProcess: theVM vmKit foreignProcess copyLocal
                                                          SP: int32 copy _SetInt32FromStackPointer.
            senderActivation: currentActivation sender.
            senderActivation meAndSendersDo: [|:a|
              a possiblyLiveOopsDo: blk.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: scavenging\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         promote: o = ( |
             nOops.
             newAddr.
            | 
            nOops: o _Map myLayout wordSizeOf: o.
            newAddr: theVM universe tenuredSpace allocateOops: nOops.
            copyObject: o Size: nOops ToAddress: newAddr.
            theVM objectLocator switchPointersFromObjectWithOop: o ToHaveAddress: newAddr.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: scavenging\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         recursiveScavenge: o = ( |
            | 
            [todo optimize gc]. "Optimize to avoid block cloning?"
            (layouts object isYoung: o) ifFalse: [^ self].
            promote: o.
            recursiveScavengeYoungObjectsIn: o.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: scavenging\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         recursiveScavengeYoungObjectsIn: o = ( |
            | 
            [todo optimize gc]. "Do a non-recursive version of this?"
            oopsIn: o Do: [|:c| recursiveScavenge: c].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: scavenging\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         recycleOopsOfObjectsIn: aSpace = ( |
             a.
             elseBlk.
             freeBlk.
             markLayout.
             memoryObjectLayout.
             objBlk.
             objectLocator.
             oopSize.
             ot.
            | 

            "Code contorted so as to avoid cloning anything during the loop. -- Adam, Mar. 2009"

            objectLocator: theVM objectLocator.

            a:       aSpace objsBottom.
            ot:      aSpace objsTop.

            "Optimization: cache some things that are used frequently during the loop."
            oopSize: aSpace oopSize.
            memoryObjectLayout: layouts memoryObject.
                    markLayout: layouts mark.

            objBlk: [|:mv. o. byteSize. recordedAddr. hasBeenScavenged. oid. map|
              o:            objectLocator localMemForAddress: a.
              recordedAddr: objectLocator addressOfLocalMem:  o.

              [aaa]. "What if it's an int32? We need a polymorphic way to check for
                      equality without cloning anything. -- Adam, Mar. 2009"
              hasBeenScavenged: a _IntNE: recordedAddr.

              __BranchIfTrue: hasBeenScavenged To: 'doneRecyclingThisOne'.
                  oid: markLayout oidOfMarkAtLocalAddress: a.
                  objectLocator invalidateEntryForLocalOID: oid.
              __DefineLabel: 'doneRecyclingThisOne'.

              map: memoryObjectLayout mapOfObjectWithAddress: a.
              byteSize: oopSize _IntMul: map myLayout wordSizeOfObjectWithAddress: a.
              a:  a _IntAdd: byteSize.
            ].

            freeBlk: [|:s|
              "Skip over the free oops."
              a:  a _IntAdd:  s _IntMul: oopSize
            ].

            elseBlk: raiseError. "Should be skipping over all of those."

            __DefineLabel: 'startOfLoop'.
            __BranchIfFalse: (a _IntLT: ot) To: 'done'.
            aSpace ifOopAt: a MarksStartOfObject: objBlk MarksStartOfFreeOops: freeBlk Else: elseBlk.
            __BranchTo: 'startOfLoop'.
            __DefineLabel: 'done'.

            aSpace objsTop: aSpace objsBottom.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: remembered set\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         rememberedSet = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> 'rememberedSet' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda garbageCollector rememberedSet.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> 'rememberedSet' -> () From: ( | {
         'Category: adding\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: public'
        
         add: o = ( |
            | 
            objects at: top Put: o.
            top: top succ.
            top = objects size ifTrue: [
              [todo gc].
              _Breakpoint: 'Just filled up the remembered set. Now what?'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> 'rememberedSet' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         objects <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> 'rememberedSet' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> 'rememberedSet' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAllAndDo: blk = ( |
            | 
            0 upTo: top Do: [|:i. o|
              o: objects at: i.
              o _MarkAsNotBeingInRememberedSet.
              objects at: i Put: 0.
              blk value: o.
            ].
            top: 0.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> 'rememberedSet' -> () From: ( | {
         'Category: resizing\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: public'
        
         resizeTo: n = ( |
            | 
            objects: objects copySize: n FillingWith: 0.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> 'rememberedSet' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         top <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: scavenging\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: public'
        
         scavenge = ( |
             garbageSpace.
             previousSpace.
            | 

            garbageSpace: _TheVM universe scavengeGarbageSpace.
            previousSpace: _TheVM universe switchAllocationSpaceTo: garbageSpace.

            'Invoking scavenger...\n' _StringPrint.

            rememberedSet removeAllAndDo: [|:o| recursiveScavengeYoungObjectsIn: o].
            possiblyLiveOopsDo:           [|:o| recursiveScavenge: o].

            'Done copying the live objects to tenuredSpace, just gotta recycle oops now. But that can take a while.\n' _StringPrint.
            movedSomeObjects.

            recycleOopsOfObjectsIn: previousSpace.
            _TheVM universe switchAllocationSpaceTo: previousSpace.
            recycleOopsOfObjectsIn: garbageSpace.
            'Done scavenging\n' _StringPrint.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'garbageCollector' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: FollowSlot'
        
         vmKitGC = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGC' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGC' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitGC.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGC' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGC' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGC' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGC' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGC' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.2 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitGC' -> () From: ( | {
         'ModuleInfo: Module: vmKitGC InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitGC postFileIn
