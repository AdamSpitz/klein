 '$Revision: 30.7 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         setTrailingMarkAt: addr In: aSpace = ( |
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
        
         ifOopAt: a MarksStartOfObject: objBlk MarksStartOfFreeOops: freeBlk Else: elseBlk = ( |
            | 
            theVM machineMemory ifOopAt: a IsObject: elseBlk IsMark: objBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateByBumpingAPointerMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         nextAddressToAllocateForObjectOfSize: nOops = ( |
            | 
            objsTop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateByBumpingAPointerMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         segregatedOopsMatching: desiredOop Do: blk = ( |
            | 
            "Optimization - we can just iterate straight from
             objsBottom to objsTop. -- Adam, 5/06"

            "Um, wait, that'll only work in the remote case. We can't
             run this code in the live local image; it'll need the
             'oops' to be the actual object references. -- Adam, 3/09"
            theVM lens = kleinAndYoda localObjectLens ifTrue: [
              halt. "Fix this later; we're not even using segregated
                     spaces right now. -- Adam, 3/09"
            ].

            theVM machineMemory
                         wordsAt: objsBottom
                            Size: (objsTop - objsBottom) / oopSize
                              Do: [|:w. :addr| (w _Eq: desiredOop) ifTrue: [blk value: w With: addr]].
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
              nextEntry: vmKit layouts freeOopsListEntry nextEntryForEntryAtAddress: addr.
              previousAddr ifNil: [setFirstEntryInListAt: i To: nextEntry]
                        IfNotNil: [vmKit layouts freeOopsListEntry forEntryAtAddress: previousAddr SetNextEntry: nextEntry].

              "If we used a chunk that was too big, put the extra oops back on the appropriate list."
              extraOops: size - nOops.
              extraOops = 0 ifFalse: [| extraOopsAddr |
                extraOopsAddr: addr + (nOops * oopSize).
                record: extraOops FreeOopsAtAddress: extraOopsAddr.
                setTrailingMarkAt: extraOopsAddr.
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
        
         ifOopAt: a MarksStartOfObject: objBlk MarksStartOfFreeOops: freeBlk Else: elseBlk = ( |
             mm.
            | 
            "Got to avoid cloning, since this is used while recycling an OID. -- Adam, Mar. 2009"
            mm: theVM machineMemory.
            mm ifOopAt: a IsObject: elseBlk IsMark: [|:mv. nextOop. r |
              nextOop: mm oopAtOffset: 1 From: a.

              "nextOop should be either a mem (if there's an object here)
               or a smi (if this is free space). -- Adam, 5/06"

              __BranchIfTrue: (layouts object isSmi: nextOop) To: 'freeOops'.
              r:  objBlk value: mv.
              __BranchTo: 'done'.
              __DefineLabel: 'freeOops'.
              r: freeBlk value: layouts smi valueOf: nextOop.
              __DefineLabel: 'done'.
              r
            ]).
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
            vmKit layouts freeOopsListEntry forEntryAtAddress: addr SetNextEntry: firstEntryInListAt: i.
            vmKit layouts freeOopsListEntry forEntryAtAddress: addr SetSize: nOops.
            setFirstEntryInListAt: i To: vmKit layouts smi decode: addr.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         searchFreeOopsListsForSpaceForAnObjectOfSize: nOops AndDo: blk = ( |
             addr.
             i.
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
            s: vmKit layouts freeOopsListEntry sizeForEntryAtAddress: addr.
            i = lastFreeOopsListIndex ifTrue: [
              [vmKit layouts freeOopsListEntry isAnEntryOfSize: s AcceptableForHoldingAnObjectOfSize: nOops] whileFalse: [
                 previousAddr: addr.
                 addr: vmKit layouts smi encode: vmKit layouts freeOopsListEntry nextEntryForEntryAtAddress: addr.
                 addr = 0 ifTrue: [^ 0].
                 s: vmKit layouts freeOopsListEntry sizeForEntryAtAddress: addr.
              ].
            ].

            [addr < objsTop] assert.

            blk value: addr With: previousAddr With: i With: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'allocateUsingFreeListsMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         segregatedOopsMatching: desiredOop Do: blk = ( |
             a.
            | 
            a: objsBottom.
            [a < objsTop] whileTrue: [
              ifOopAt: a
              MarksStartOfFreeOops: [|:s|
                "Skip over the free oops."
                a: a + (s * oopSize).
              ]
              Else: [|:oop|
                (oop _Eq: desiredOop) ifTrue: [blk value: oop With: a].
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
        
         setTrailingMarkAt: addr In: aSpace = ( |
            | 
            aSpace setLocalTrailingMarkAt: addr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: spaces\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         setTrailingMarkAt: addr In: aSpace = ( |
            | 
            aSpace setRemoteTrailingMarkAt: addr).
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
        
         ifOopAt: a MarksStartOfFreeOops: freeBlk Else: elseBlk = ( |
            | 
                         ifOopAt: a
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
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         objectAddressesDo: blk = ( |
             a.
             ot.
            | 
            a: objsBottom.
            ot: objsTop.
            [a < ot] whileTrue: [
              ifOopAt: a
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
        
         oopsMatching: desiredOop Do: blk = ( |
            | 
            segregatedOopsMatching: desiredOop Do: blk).
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
        
         setLocalTrailingMarkAt: addr = ( |
            | 
            addr _UnsafeWriteTrailingMark.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: allocating\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         setRemoteTrailingMarkAt: addr = ( |
            | 
            machineMemory at: addr
                      PutOop: layouts mark trailingMark.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         setTrailingMark = ( |
            | 
            setTrailingMarkAt: objsTop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         setTrailingMarkAt: addr = ( |
            | 
            theVM lens setTrailingMarkAt: addr In: self).
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
        
         ifOopAt: a MarksStartOfBV: bvBlk MarksStartOfNonBV: nonBVBlk MarksStartOfFreeOops: freeBlk Else: elseBlk = ( |
            | 
                         ifOopAt: a
              MarksStartOfObject: [|:mv| (layouts mark isMarkValueForByteVector: mv)
                                            ifTrue: [bvBlk value: (layouts byteVector indexableOriginOfObjectWithAddress: a)
                                                            With:  layouts byteVector indexableSizeOfObjectWithAddress:   a ]
                                             False: [nonBVBlk value: mv]]
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
             elseBlk.
             freeBlk.
             objBlk.
             oopSize.
             ot.
            | 

            "Code contorted so as to avoid cloning anything during the loop,
             so that this code can be used when recycling OIDs. -- Adam, Mar. 2009"

            a: objsBottom.
            ot: objsTop.
            oopSize: self oopSize.

            objBlk: [|:mv. io. s. newA|
                __BranchIfFalse: (layouts mark isMarkValueForByteVector: mv) To: 'notBV'.
                io:  layouts byteVector indexableOriginOfObjectWithAddress: a.
                 s:  layouts byteVector indexableSizeOfObjectWithAddress:   a.
                blk value: a.
                newA: a _IntAdd: (io _IntMul: oopSize) _IntAdd: (layouts byteVector bytesNeededToHoldBytes: s).
                __BranchTo: 'doneObject'.
                __DefineLabel: 'notBV'.
                blk value: a.
                newA: a _IntAdd: oopSize.
                __DefineLabel: 'doneObject'.
                newA
            ].

            freeBlk: [|:s|
                "Skip over the free oops."
                a _IntAdd:  s _IntMul: oopSize
            ].

            elseBlk: [
                a _IntAdd: oopSize
            ].

            [a _IntLT: ot] whileTrue: [
              a: ifOopAt: a
                 MarksStartOfObject: objBlk
                 MarksStartOfFreeOops: freeBlk
                 Else: elseBlk.
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
                  oopsDo: [nOops: nOops succ]
                 MarksDo: [nOops: nOops succ]
            BytesPartsDo: [].
            nOops).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: private'
        
         oopsDo: blk MarksDo: markBlk BytesPartsDo: bytesBlk = ( |
             a.
             mm.
             ot.
            | 
            mm: theVM machineMemory.
            a: objsBottom.
            ot: objsTop.
            [a < ot] whileTrue: [
              ifOopAt: a
              MarksStartOfBV: [|:io. :s|
                markBlk value.
                a: a + oopSize.
                "Do all the oops of this byteVector, then jump to the end of its bytes."
                mm wordsAt: a Size: io pred Do: blk.
                a: a + (io pred * oopSize).
                bytesBlk value: s With: a.
                a: a + (s roundUpTo: oopSize).
              ]
              MarksStartOfNonBV: [|:mv|
                markBlk value.
                a: a + oopSize.
              ]
              MarksStartOfFreeOops: [|:s|
                "Skip over the free oops."
                a: a + (s * oopSize).
              ]
              Else: [|:oop|
                "Just do this oop."
                blk value: oop With: a.
                a: a + oopSize.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitSpace InitialContents: FollowSlot\x7fVisibility: public'
        
         oopsMatching: desiredOop Do: blk = ( |
            | 
                  oopsDo: [|:oop. :addr| (desiredOop _Eq: oop) ifTrue: [blk value: oop With: addr]]
                 MarksDo: []
            BytesPartsDo: []).
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
        
         copy = ( |
            | 
            resend.copy freeOopsLists: freeOopsLists copy).
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
