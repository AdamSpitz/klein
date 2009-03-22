 '$Revision: 30.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         setTrailingMarkIn: aSpace = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateByBumpingAPointerMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateByBumpingAPointerMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda allocateByBumpingAPointerMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateByBumpingAPointerMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOops: nOops = ( |
            | 
            allocateOops: nOops IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateByBumpingAPointerMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOops: nOops IfFail: fb = ( |
             oldObjsTop.
            | 
            [_NoGCAllowed].

            "Using primitives to avoid cloning any blocks, so that
             this code can be used inside Klein. -- Adam, 7/06"

            oldObjsTop: objsTop.
            [todo untaggedAddresses optimization]. "Convert to using untagged addresses, for efficiency."
            [todo cleanup allocating oopSize]. "Fix the hardcoded 4. Problem is that the layout code isn't included in the miniVM."
            objsTop:  oldObjsTop _IntAdd:  nOops _IntMul: 4.

            "Check for out of memory: using _IntLE: instead of _IntLT:
             to reserve space for the mark following last object in
             the space -- jb 6/03"
            __BranchIfFalse: (objsLimit _IntLE: objsTop) To: 'ok'.
            objsTop: oldObjsTop.
            fb value: 'space full' With: 'allocateOops:'.

            __DefineLabel: 'ok'.

            setTrailingMark.

            oldObjsTop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateByBumpingAPointerMixin' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlotsToFixUpIn: spaceMir Do: blk = ( |
            | 
            spaceBoundarySlotsToFixUpIn: spaceMir Do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateByBumpingAPointerMixin' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         getReadyToStartAllocating = ( |
            | 
            initializeAllocationPointers).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateByBumpingAPointerMixin' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         ifOop: oop At: a MarksStartOfObject: objBlk MarksStartOfFreeOops: freeBlk Else: elseBlk = ( |
            | 
            (layouts object isMark: oop) ifTrue: objBlk False: elseBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateByBumpingAPointerMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         nextAddressToAllocateForObjectOfSize: nOops = ( |
            | 
            objsTop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateByBumpingAPointerMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         segregatedOopsDo: blk = ( |
            | 
            "Optimization - we can just iterate straight from
             objsBottom to objsTop. -- Adam, 5/06"
            theVM machineMemory
                         wordsAt: objsBottom
                            Size: (objsTop - objsBottom) / oopSize
                              Do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateUsingFreeListsMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda allocateUsingFreeListsMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOops: nOops = ( |
            | 
            [_NoGCAllowed].
            searchFreeOopsListsForSpaceForAnObjectOfSize: nOops AndDo: [|:addr. :previousAddr. :i. :size. nextEntry. extraOops|
              "Remove the entry from the list."
              nextEntry: vmKit layouts freeOopsListEntry nextEntryForRemoteEntryAtAddress: addr.
              previousAddr ifNil: [setFirstEntryInListAt: i To: nextEntry]
                        IfNotNil: [vmKit layouts freeOopsListEntry forRemoteEntryAtAddress: previousAddr SetNextEntry: nextEntry].

              "If we used a chunk that was too big, put the extra oops back on the appropriate list."
              extraOops: size - nOops.
              extraOops = 0 ifFalse: [| extraOopsAddr |
                extraOopsAddr: addr + (nOops * oopSize).
                record: extraOops FreeOopsAtAddress: extraOopsAddr.
                machineMemory at: extraOopsAddr
                          PutOop: theVM vmKit layouts mark trailingMark.
              ].
              addr
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlotsToFixUpIn: spaceMir Do: blk = ( |
            | 
                spaceBoundarySlotsToFixUpIn: spaceMir Do: blk.
            freeOopsListEntrySlotsToFixUpIn: spaceMir Do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         firstEntryInListAt: i = ( |
            | 
            "the entry is a smi containing the address / 4."
            [vmKit tag smi  = 0] assert.
            [vmKit tag size = 2] assert.
            freeOopsLists at: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         freeOopsListEntrySlotsToFixUpIn: spaceMir Do: blk = ( |
             freeOopsListsMir.
            | 
            freeOopsListsMir: spaceMir primitiveContentsAt: 'freeOopsLists'.
            freeOopsListsMir fakeSlotsDo: [|:s|
              s isVectorElement ifTrue: [
                blk value: s With: freeOopsLists.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         getReadyToStartAllocating = ( |
            | 
            initializeFreeLists).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         ifOop: oop At: a MarksStartOfObject: objBlk MarksStartOfFreeOops: freeBlk Else: elseBlk = ( |
             nextOop.
            | 
            (layouts object isMark: oop) ifFalse: [^ elseBlk value].

            nextOop: theVM machineMemory oopAt: a + oopSize.

            "nextOop should be either a mem (if there's an object here)
             or a smi (if this is free space). -- Adam, 5/06"
            [layouts memoryObject mapField fixedIndex = 1] assert.
            [layouts freeOopsListEntry sizeIndex      = 1] assert.

            (layouts object isSmi: nextOop) ifTrue: [^ freeBlk value: layouts smi valueOf: nextOop].
            [layouts object isMem: nextOop] assert.

            objBlk value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         indexForListContainingEntriesOfSize: nOops = ( |
            | 
            nOops min: lastFreeOopsListIndex).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         lastFreeOopsListIndex = ( |
            | 
            freeOopsLists lastKey).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         nextAddressToAllocateForObjectOfSize: nOops = ( |
            | 
            searchFreeOopsListsForSpaceForAnObjectOfSize: nOops AndDo: [|:addr| addr]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         record: nOops FreeOopsAtAddress: addr = ( |
             i.
            | 
            [nOops >= vmKit layouts freeOopsListEntry numberOfFields] assert.
            i: indexForListContainingEntriesOfSize: nOops.
            vmKit layouts freeOopsListEntry forRemoteEntryAtAddress: addr SetNextEntry: firstEntryInListAt: i.
            vmKit layouts freeOopsListEntry forRemoteEntryAtAddress: addr SetSize: nOops.
            setFirstEntryInListAt: i To: vmKit layouts smi decode: addr.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         searchFreeOopsListsForSpaceForAnObjectOfSize: nOops AndDo: blk = ( |
             addr.
             entry.
             i.
             nextAddr.
             previousAddr.
             s.
            | 
            i: nOops.

            [i: indexForListContainingEntriesOfSize: i.
             addr: vmKit layouts smi encode: firstEntryInListAt: i.
             addr = 0] whileTrue: [

              i = lastFreeOopsListIndex ifTrue: [^ 0].
              i: i + (i = nOops
                         ifFalse: 1
                            True: [vmKit layouts freeOopsListEntry numberOfFields]). "don't leave holes too small for an entry"
            ].
            s: vmKit layouts freeOopsListEntry sizeForRemoteEntryAtAddress: addr.
            i = lastFreeOopsListIndex ifTrue: [
              [vmKit layouts freeOopsListEntry isAnEntryOfSize: s AcceptableForHoldingAnObjectOfSize: nOops] whileFalse: [
                 previousAddr: addr.
                 addr: vmKit layouts smi encode: vmKit layouts freeOopsListEntry nextEntryForRemoteEntryAtAddress: addr.
                 addr = 0 ifTrue: [^ 0].
                 s: vmKit layouts freeOopsListEntry sizeForRemoteEntryAtAddress: addr.
              ].
            ].

            [addr < objsTop] assert.

            blk value: addr With: previousAddr With: i With: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         segregatedOopsDo: blk = ( |
             a.
             mm.
            | 
            mm: theVM machineMemory.
            a: objsBottom.
            [a < objsTop] whileTrue: [| oop |
              oop: mm oopAt: a.

              ifOop: oop
              At: a
              MarksStartOfFreeOops: [|:s|
                "Skip over the free oops."
                a: a + (s * oopSize).
              ]
              Else: [
                "Just do this oop."
                blk value: oop With: a.
                a: a + oopSize.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         setFirstEntryInListAt: i To: shiftedAddr = ( |
            | 
            "shiftedAddr is a smi containing the address >> 2."
            [vmKit tag smi  = 0] assert.
            [vmKit tag size = 2] assert.
            freeOopsLists at: i Put: shiftedAddr.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         edenSpace = ( |
            | 
            unsegregatedEdenSpace).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         setTrailingMarkIn: aSpace = ( |
            | 
            aSpace setLocalTrailingMark).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         setTrailingMarkIn: aSpace = ( |
            | 
            aSpace setRemoteTrailingMark).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         segregatedEdenSpace = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedEdenSpace' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda segregatedEdenSpace.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedEdenSpace' -> () From: ( | {
         'Category: bytes growing from top\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         bytesBottom <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedEdenSpace' -> () From: ( | {
         'Category: bytes growing from top\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         bytesTop <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedEdenSpace' -> () From: ( | {
         'Category: whole enchilada\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (\'eden\')'
        
         name <- 'eden'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedEdenSpace' -> () From: ( | {
         'Category: objects growing from bottom\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         objsBottom <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedEdenSpace' -> () From: ( | {
         'Category: objects growing from bottom\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         objsTop <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedEdenSpace' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedEdenSpace' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda segregatedEdenSpace parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedEdenSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         allocationMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateByBumpingAPointerMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedEdenSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         base* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         segregatedSpaceMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda segregatedSpaceMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedEdenSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         segregationMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         spaceMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda spaceMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedEdenSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         spaceMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         allAllocatedRegions = ( |
             v.
            | 
            v: list copyRemoveAll.
            v addLast:  objsBottom asInteger. v addLast:  objsTop asInteger.
            v addLast: bytesBottom asInteger. v addLast: bytesTop asInteger.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateBytes: nBytes = ( |
             newBytesBottom.
            | 
            [_NoGCAllowed].
            newBytesBottom: bytesBottom - (nBytes roundUpTo: oopSize).

            "Check for out of memory: using '<=' to reserve space for the mark
             following the last object in the space -- jb 6/03"

            newBytesBottom <= bytesLimit ifTrue: [
              [todo gc].
              error: 'space full'.
            ].

            bytesBottom: newBytesBottom.
            bytesBottom).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOops: nOops AndBytes: nBytes = ( |
             addr.
             bpRef.
            | 
            addr: allocateOops: nOops.
            bpRef: theVM vmKit layouts bytesPart allocateBytesPartWithIndexableSize: nBytes.
            theVM vmKit layouts byteVector forObjectWithAddress: addr SetBytesPartRef: bpRef IfFail: raiseError.
            addr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesIncludesAddress: addr = ( |
            | 
            (bytesBottom <= addr) && [addr < bytesTop]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesLimit = ( |
            | 
            objsTop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         ifOop: oop At: a MarksStartOfFreeOops: freeBlk Else: elseBlk = ( |
            | 
                           ifOop: oop
                              At: a
              MarksStartOfObject: elseBlk
            MarksStartOfFreeOops: freeBlk
                            Else: elseBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: initialization\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         initSize: s StartingAt: addr = ( |
            | 
            objsBottom:   addr.
            objsTop:      objsBottom.
            bytesBottom:  addr + s.
            bytesTop:     bytesBottom.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: initialization\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeAllocationPointers = ( |
            | 
            objsTop: objsBottom.
            bytesBottom: bytesTop.
            setTrailingMark. "not really necessary"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: initialization\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeFreeLists = ( |
            | 
            "We don't really want to divide up the segregated space, do we?
             I'm not sure what to do. -- Adam, 5/06"
            error: 'not implemented yet: mark/sweep for segregated spaces').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         isEmpty = ( |
            | 
            (objsBottom = objsTop) && [bytesBottom = bytesTop]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         objectAddressesDo: blk = ( |
             a.
             mm.
             ot.
            | 
            mm: theVM machineMemory.
            a: objsBottom.
            ot: objsTop.
            [a < ot] whileTrue: [| oop |
              oop: mm oopAt: a.

              ifOop: oop
              At: a
              MarksStartOfObject: [
                blk value: a.
                a: a + oopSize.
              ]
              MarksStartOfFreeOops: [|:s|
                "Skip over the free oops."
                a: a + (s * oopSize).
              ]
              Else: [
                a: a + oopSize.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         objsLimit = ( |
            | 
            bytesBottom).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         oopCount = ( |
            | 
            sizeOfAllocatedOopsRegion / oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         oopsDo: blk = ( |
            | 
            segregatedOopsDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: initialization\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateTo: addr = ( |
             delta.
            | 
            delta: addr - objsBottom.
            objsBottom:   objsBottom  + delta.
            objsTop:      objsTop     + delta.
            bytesBottom:  bytesBottom + delta.
            bytesTop:     bytesTop    + delta.
            delta).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         sizeOfAllocatedBytesRegion = ( |
            | 
            bytesTop - bytesBottom).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         spaceBoundarySlotsToFixUpIn: spaceMir Do: blk = ( |
            | 
            [objsBottom.    objsTop.    bytesBottom.    bytesTop   ]. "browsing"
            [objsBottom: 0. objsTop: 0. bytesBottom: 0. bytesTop: 0]. "browsing"

            blk value: (spaceMir at: 'objsBottom' ) With: self.
            blk value: (spaceMir at: 'objsTop'    ) With: self.
            blk value: (spaceMir at: 'bytesBottom') With: self.
            blk value: (spaceMir at: 'bytesTop'   ) With: self.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         top = ( |
            | 
            bytesTop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         segregatedTenuredSpace = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda segregatedTenuredSpace.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> () From: ( | {
         'Category: bytes growing from top\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         bytesBottom <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> () From: ( | {
         'Category: bytes growing from top\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         bytesTop <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> () From: ( | {
         'Category: freelists\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (vector copySize: 20 FillingWith: 0)'
        
         freeOopsLists <- vector copySize: 20 FillingWith: 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> () From: ( | {
         'Category: whole enchilada\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (\'tenured\')'
        
         name <- 'tenured'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> () From: ( | {
         'Category: objects growing from bottom\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         objsBottom <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> () From: ( | {
         'Category: objects growing from bottom\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         objsTop <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda segregatedTenuredSpace parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         allocationMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         base* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            resend.copy freeOopsLists: freeOopsLists copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForLaunch = ( |
             c.
            | 
            c: resend.copyForLaunch.
            theVM image reassociateKleinObjectFrom: freeOopsLists To: c freeOopsLists.
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         segregated* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedTenuredSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         spaceMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: genesis\x7fComment: Allocates a new heap from theVM machineMemory of the
specified size.  Must be called exactly once before
objects are allocated within the space.\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateHeapAt: base Size: s = ( |
            | 
            initSize: s StartingAt: base).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         bottom = ( |
            | 
            objsBottom).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForLaunch = ( |
             c.
            | 
            c: copy.
            theVM image reassociateKleinObjectFrom: self          To: c.
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         copyNamed: n = ( |
            | copy initSpaceNamed: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         includesAddress: a = ( |
            | 
            (bottom <= a) && [a < top]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: initialization\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         initSpaceNamed: n = ( |
            | 
            name: n.
            initSize: 0 StartingAt: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         machineMemory = ( |
            | 
            theVM machineMemory).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         oopsIncludesAddress: addr = ( |
            | 
            (objsBottom <= addr) && [addr < objsTop]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: allocating\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         setLocalTrailingMark = ( |
            | 
            objsTop _UnsafeWriteTrailingMark.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: allocating\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         setRemoteTrailingMark = ( |
            | 
            machineMemory at: objsTop
                      PutOop: layouts mark trailingMark.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         setTrailingMark = ( |
            | 
            theVM lens setTrailingMarkIn: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         sizeOfAllocatedOopsRegion = ( |
            | 
            (objsTop - objsBottom) + oopSize "for trailing mark").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         sizeOfAllocatedRegions = ( |
            | 
            sizeOfAllocatedOopsRegion + sizeOfAllocatedBytesRegion).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         sizeOfEntireRegion = ( |
            | 
            top - bottom).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         sizeOfUnallocatedRegion = ( |
            | 
            (objsLimit - objsTop) - oopSize "for trailing mark").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         tenuredSpace = ( |
            | 
            unsegregatedTenuredSpace).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         unsegregatedEdenSpace = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedEdenSpace' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda unsegregatedEdenSpace.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedEdenSpace' -> () From: ( | {
         'Category: whole enchilada\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (\'eden\')'
        
         name <- 'eden'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedEdenSpace' -> () From: ( | {
         'Category: objects growing from bottom\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         objsBottom <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedEdenSpace' -> () From: ( | {
         'Category: objects growing from bottom\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         objsTop <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedEdenSpace' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedEdenSpace' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda unsegregatedEdenSpace parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedEdenSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         allocationMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateByBumpingAPointerMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedEdenSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         base* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedEdenSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         spaceMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         unsegregatedSpaceMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda unsegregatedSpaceMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedEdenSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         unsegregated* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedEdenSpace' -> () From: ( | {
         'Category: objects growing from bottom\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         top <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         allAllocatedRegions = ( |
             v.
            | 
            v: list copyRemoveAll.
            v addLast: objsBottom asInteger.
            v addLast: objsTop    asInteger.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOops: nOops AndBytes: nBytes = ( |
             addr.
            | 
            [_NoGCAllowed].
            addr: allocateOops: nOops + (nBytes /+ oopSize).
            theVM vmKit layouts byteVector forObjectWithAddress: addr SetIndexableSize: nBytes IfFail: raiseError.
            addr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesBottom = ( |
            | 
            "So that this can work with the memory interfaces."
            top).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: memory interface\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         createBufferMemoryInterfaceUsingPrototype: miProto = ( |
            | 
            miProto copyForSpace: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         ifOop: oop At: a MarksStartOfBV: bvBlk MarksStartOfFreeOops: freeBlk Else: elseBlk = ( |
            | 
                           ifOop: oop
                              At: a
                  MarksStartOfBV: bvBlk
               MarksStartOfNonBV: elseBlk
            MarksStartOfFreeOops: freeBlk
                            Else: elseBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         ifOop: oop At: a MarksStartOfBV: bvBlk MarksStartOfNonBV: nonBVBlk MarksStartOfFreeOops: freeBlk Else: elseBlk = ( |
            | 
                           ifOop: oop
                              At: a
              MarksStartOfObject: [(layouts mark isMarkForByteVector: oop)
                                        ifTrue: [bvBlk value: (layouts byteVector indexableOriginOfObjectWithAddress: a IfFail: raiseError)
                                                        With:  layouts byteVector indexableSizeOfObjectWithAddress:   a IfFail: raiseError ]
                                         False: nonBVBlk]
            MarksStartOfFreeOops: freeBlk
                            Else: elseBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: initialization\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         initSize: s StartingAt: addr = ( |
            | 
            objsBottom:   addr.
            objsTop:      objsBottom.
            top:          addr + s.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: initialization\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeAllocationPointers = ( |
            | 
            objsTop: objsBottom.
            setTrailingMark. "not really necessary"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: initialization\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeFreeLists = ( |
             nOops.
            | 
            objsTop: top - oopSize. "leave room for the trailing mark"
            setTrailingMark.

            nOops: (objsTop - objsBottom) / oopSize.
            record: nOops FreeOopsAtAddress: bottom.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         isEmpty = ( |
            | 
            objsBottom = objsTop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         objectAddressesDo: blk = ( |
             a.
             mm.
             ot.
            | 
            mm: theVM machineMemory.
            a: objsBottom.
            ot: objsTop.
            [a < ot] whileTrue: [| oop |
              oop: mm oopAt: a.

              ifOop: oop
              At: a
              MarksStartOfBV: [|:io. :s|
                blk value: a.
                a: a + (io * oopSize) + (s roundUpTo: oopSize).
              ]
              MarksStartOfNonBV: [
                blk value: a.
                a: a + oopSize.
              ]
              MarksStartOfFreeOops: [|:s|
                "Skip over the free oops."
                a: a + (s * oopSize).
              ]
              Else: [
                a: a + oopSize.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         objsLimit = ( |
            | 
            top).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         oopCount = ( |
             nOops <- 0.
            | 
            oopsDo: [nOops: nOops succ].
            nOops).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         oopsDo: blk = ( |
            | 
            oopsDo: blk AndBytesPartsDo: []).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         oopsDo: blk AndBytesPartsDo: bytesBlk = ( |
             a.
             mm.
             ot.
            | 
            mm: theVM machineMemory.
            a: objsBottom.
            ot: objsTop.
            [a < ot] whileTrue: [| oop |
              oop: mm oopAt: a.

              ifOop: oop
              At: a
              MarksStartOfBV: [|:io. :s|
                "Do all the oops of this byteVector, then jump to the end of its bytes."
                mm wordsAt: a Size: io Do: blk.
                a: a + (io * oopSize).
                bytesBlk value: s With: a.
                a: a + (s roundUpTo: oopSize).
              ]
              MarksStartOfFreeOops: [|:s|
                "Skip over the free oops."
                a: a + (s * oopSize).
              ]
              Else: [
                "Just do this oop."
                blk value: oop With: a.
                a: a + oopSize.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: initialization\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateTo: addr = ( |
             delta.
            | 
            delta: addr - objsBottom.
            objsBottom:   objsBottom  + delta.
            objsTop:      objsTop     + delta.
            top:          top         + delta.
            delta).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         sizeOfAllocatedBytesRegion = ( |
            | 
            "So that this can work with the memory interfaces."
            0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         spaceBoundarySlotsToFixUpIn: spaceMir Do: blk = ( |
            | 
            [objsBottom.    objsTop.    top   ]. "browsing"
            [objsBottom: 0. objsTop: 0. top: 0]. "browsing"

            blk value: (spaceMir at: 'objsBottom') With: self.
            blk value: (spaceMir at: 'objsTop'   ) With: self.
            blk value: (spaceMir at: 'top'       ) With: self.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object heap\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         unsegregatedTenuredSpace = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedTenuredSpace' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda unsegregatedTenuredSpace.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedTenuredSpace' -> () From: ( | {
         'Category: freelists\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (vector copySize: 20 FillingWith: 0)'
        
         freeOopsLists <- vector copySize: 20 FillingWith: 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedTenuredSpace' -> () From: ( | {
         'Category: whole enchilada\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (\'tenured\')'
        
         name <- 'tenured'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedTenuredSpace' -> () From: ( | {
         'Category: objects growing from bottom\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         objsBottom <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedTenuredSpace' -> () From: ( | {
         'Category: objects growing from bottom\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         objsTop <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedTenuredSpace' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedTenuredSpace' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda unsegregatedTenuredSpace parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedTenuredSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         allocationMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedTenuredSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         base* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedTenuredSpace' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForLaunch = ( |
             c.
            | 
            c: resend.copyForLaunch.
            theVM image reassociateKleinObjectFrom: freeOopsLists To: c freeOopsLists.
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedTenuredSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         spaceMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedTenuredSpace' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         unsegregated* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedTenuredSpace' -> () From: ( | {
         'Category: objects growing from bottom\x7fModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (0)'
        
         top <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot'
        
         vmKitSpace = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSpace' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSpace' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitSpace.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSpace' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSpace' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSpace' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSpace' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSpace' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSpace' -> () From: ( | {
         'ModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitSpace postFileIn
