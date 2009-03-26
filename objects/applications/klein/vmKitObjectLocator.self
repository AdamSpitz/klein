 '$Revision: 30.9 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addressForOID: oid In: anObjectLocator IfAbsent: ab = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidEntryForOID: oid In: anObjectLocator = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForInvalidEntry: f In: anObjectLocator = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oidOfMem: mem In: anObjectLocator = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         switchPointersFromObjectWithOID: oid ToHaveAddress: newAddr In: anObjectLocator = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fCategory: object locators\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractObjectLocator = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals vector copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda abstractObjectLocator.

CopyDowns:
globals vector. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: InitializeToExpression: (-0.5)\x7fVisibility: private'
        
         lastInvalidEntry <- -0.5.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda abstractObjectLocator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: relocating oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addOffset: heapOffset ToOop: oop = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: mapping oids to addresses\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         addressForElement: shiftedAddress IfNotReallyAnAddress: fb = ( |
            | 
            (reflect: shiftedAddress) isReflecteeFloat
               ifTrue: [fb value: 'unallocated']
                False: [vmKit layouts smi encode: shiftedAddress]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: mapping oids to addresses\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addressForOID: oid = ( |
            | 
            addressForOID: oid IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: mapping oids to addresses\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addressForOID: oid IfAbsent: ab = ( |
            | 
            theVM lens addressForOID: oid In: self IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfLocalMem: mem = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfRemoteMem: memOop = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addressesAndOIDsDo: blk = ( |
            | 
            do: [|:shiftedAddr. :oid|
              [|:exit|
               blk value: (addressForElement: shiftedAddr IfNotReallyAnAddress: exit)
                    With: oid.
              ] exit.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: store string\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         collectionName = 'klein abstractObjectLocator'.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         copySize: n = ( |
             c.
             oldSize.
            | 
            oldSize: size.
            c: resend.copySize: n.
            c initializeEntriesFrom: oldSize UpTo: n.
            c).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlotsToFixUpIn: objectLocatorMir Do: blk = ( |
            | 
            [lastInvalidEntry. lastInvalidEntry: 0.0]. "browsing"
            blk value: (objectLocatorMir at: 'lastInvalidEntry')
                 With: self.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         decodeAddressOrOIDFromMemOop: x = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: mapping oids to addresses\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         elementForAddress: addr = ( |
            | 
            "Duplication with" [withoutCloningAnythingRecordAddress: addr ForOID: oid].

            "shift the addr so that in C land it will be right"
            addr +> vmKit tag size).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         encodeMemOopFromAddressOrOID: x = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         ifDirect: db IfIndirect: ib = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: mapping oids to addresses\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         inRemoteImageRecordAddress: newAddr ForOID: oid = ( |
             objectLocatorOop.
            | 
            objectLocatorOop: theVM image oopForOriginalObject: self.
            vmKit layouts objVector
                       for: objectLocatorOop
               IndexableAt: oid
                       Put: vmKit layouts smi oopForValue: elementForAddress: newAddr.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeEntriesFrom: start UpTo: end = ( |
            | 
            "Not that it really matters, but I like going down instead of up
            so that when we start allocating, we'll start from the beginning
            of the table rather than from the end. -- Adam, 5/06"
            end pred downTo: start Do: [|:oid| invalidateEntryForOID: oid].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         intNN = ( |
            | 
            vmKit layouts abstract intNN).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: invalid entries\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         invalidEntryForLocalOID: oid = ( |
            | 
            "An invalid entry is the index of the previous
             invalid entry, but tagged as a float instead
             of a smi. -- Adam, 5/06"

            oid _ConvertOIDToInvalidObjectLocatorEntry).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: invalid entries\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         invalidEntryForOID: oid = ( |
            | 
            theVM lens invalidEntryForOID: oid In: self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: invalid entries\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         invalidEntryForRemoteOID: oid = ( |
            | 
            "An invalid entry is the index of the previous
             invalid entry, but tagged as a float instead
             of a smi. -- Adam, 5/06"

            vmKit layouts float decode:
               vmKit layouts smi encode: oid).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: invalid entries\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateEntryForOID: oid = ( |
             e.
            | 
            at: oid Put: lastInvalidEntry.
            e: invalidEntryForOID: oid.
            lastInvalidEntry: e.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: reloading\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateMyObsoleteCachedItems = ( |
             proc.
             ts.
            | 
            proc: theVM machineMemory foreignProcess.
            proc ifNil: [^ self].
            ts: proc objectLocatorTimestampIfFail: [^ self].
            ts = timestamp ifTrue: [^ self].
            reloadFromDebuggee.
            timestamp: ts).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: mapping oids to addresses\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         localAddressForOID: oid IfAbsent: ab = ( |
             r.
            | 
            r: at: oid IfAbsent: [^ ab value: 'absent'].
            addressForElement: r IfNotReallyAnAddress: ab).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         localMemForAddress: address = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         localMemForAddress: address OID: oid = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: getting free entries\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         nextFreeOID = ( |
            | 
            oidForInvalidEntry: lastInvalidEntry).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: invalid entries\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         oidForInvalidEntry: f = ( |
            | 
            theVM lens oidForInvalidEntry: f In: self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: invalid entries\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         oidForInvalidLocalEntry: f = ( |
            | 
            "An invalid entry is the index of the previous
             invalid entry, but tagged as a float instead
             of a smi. -- Adam, 5/06"

            f _ConvertInvalidObjectLocatorEntryToOID).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: invalid entries\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         oidForInvalidRemoteEntry: f = ( |
            | 
            "An invalid entry is the index of the previous
             invalid entry, but tagged as a float instead
             of a smi. -- Adam, 5/06"

            vmKit layouts smi decode:
              vmKit layouts float encode: f).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOop: oop IfNontrivial: ntb = ( |
            | 
            ntb value).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid Address: addr = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid IfAbsent: ab = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oopOfLocalMem: mem = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oopsAndOIDsDo: blk = ( |
            | 
            addressesAndOIDsDo: [|:addr. :oid|
              blk value: (oopForOID: oid Address: addr)
                   With: oid
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'vector' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: store string\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            vmKit abstractObjectLocator).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: mapping oids to addresses\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         recordAddress: addr ForOID: oid = ( |
            | 
            "Duplication with" [withoutCloningAnythingRecordAddress: addr ForOID: oid].

            at: oid Put: elementForAddress: addr).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: reloading\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         reloadFromDebuggee = ( |
             index <- 0.
             proc.
             startOfAddresses.
            | 
            proc: theVM machineMemory foreignProcess.
            startOfAddresses: proc startOfObjectAddressesIfFail: [^ self].
            proc
              readMemoryWordsAt:  startOfAddresses
                           Size:  size
                             Do:  [|:w. entry|
                                   entry: vmKit tag ifOop: w IsFloat: [vmKit layouts float decode: w]
                                                               IsSmi: [vmKit layouts smi   decode: w]
                                                              IsMark: raiseError
                                                               IsMem: raiseError.
                                   at: index Put: entry.
                                   index: index succ
                                  ]
                         IfFail:  raiseError).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: mapping oids to addresses\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteAddressForOID: oid IfAbsent: ab = ( |
             r.
            | 
            invalidateMyObsoleteCachedItems.
            r: at: oid IfAbsent: [^ ab value: 'absent'].
            addressForElement: r IfNotReallyAnAddress: [
              reloadFromDebuggee.
              addressForElement: (at: oid) IfNotReallyAnAddress: ab
            ]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteMemForAddress: address = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteMemForAddress: address OID: oid = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: switching pointers\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         switchPointersFromObjectWithOop: oldOop ToHaveAddress: newAddr = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: getting free entries\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         withoutCloningAnythingAllocateAnOID = ( |
             e.
             oid.
            | 
            oid: nextFreeOID.
            __BranchIfFalse: (oid _IntLT: 0) To: 'ok'.
            _Breakpoint: 'no more free OIDs'.
            __DefineLabel: 'ok'.
            e: _At: oid.
            lastInvalidEntry: e.
            oid).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: mapping oids to addresses\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         withoutCloningAnythingRecordAddress: addr ForOID: oid = ( |
            | 
            "Duplication with" [recordAddress: addr ForOID: oid].
            "Duplication with" [elementForAddress: addr].
            _At: oid Put: addr _IntArithmeticShiftRight: vmKit tag size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fCategory: object locators\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         directPointerObjectLocator = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda abstractObjectLocator copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda directPointerObjectLocator.

CopyDowns:
globals kleinAndYoda abstractObjectLocator. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda directPointerObjectLocator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addOffset: heapOffset ToOop: oop = ( |
            | oop + heapOffset).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfLocalMem: mem = ( |
            | 
            _NoGCAllowed.
            (mem _ValuePartOfObjectReference) asInt32 <+ vmKit tag size).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfRemoteMem: memOop = ( |
            | 
            decodeAddressOrOIDFromMemOop: memOop).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: store string\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         collectionName = 'klein directPointerObjectLocator'.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         decodeAddressFromMemOop: x = ( |
            | 
            x - vmKit tag mem).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         decodeAddressOrOIDFromMemOop: x = ( |
            | 
            decodeAddressFromMemOop: x).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         encodeMemOopFromAddress: addr = ( |
            | 
            addr + vmKit tag mem).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         encodeMemOopFromAddressOrOID: x = ( |
            | 
            encodeMemOopFromAddress: x).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         ifDirect: db IfIndirect: ib = ( |
            | 
            db value).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         localMemForAddress: address = ( |
            | 
            _NoGCAllowed.
            _CreateObjectReferenceWithTag: vmKit tag mem
                                 AndValue: address +> vmKit tag size).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         localMemForAddress: address OID: oid = ( |
            | 
            localMemForAddress: address).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid Address: addr = ( |
            | 
            encodeMemOopFromAddress: addr).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid IfAbsent: ab = ( |
            | 
            encodeMemOopFromAddress:  addressForOID: oid IfAbsent: [^ ab value]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oopOfLocalMem: mem = ( |
            | 
            vmKit layouts memoryObject encode: addressOfLocalMem: mem).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: store string\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            kleinAndYoda directPointerObjectLocator).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteMemForAddress: address = ( |
            | 
            encodeMemOopFromAddress: address).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteMemForAddress: address OID: oid = ( |
            | 
            remoteMemForAddress: address).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         switchPointersFromObjectWithOop: oldOop ToHaveAddress: newAddr = ( |
             mm.
             newOop.
             oid.
             oldAddr.
            | 
            oldAddr: decodeAddressFromMemOop: oldOop.
            oid: vmKit layouts memoryObject oidOfRemoteObjectWithAddress: oldAddr IfFail: raiseError.

            mm: theVM machineMemory.
            newOop: encodeMemOopFromAddress: newAddr.
            theVM universe oopsMatching: oldOop Do: [|:oop. :addr|
              mm at: addr PutOop: newOop.
            ].

                         recordAddress: newAddr ForOID: oid.
            inRemoteImageRecordAddress: newAddr ForOID: oid.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fCategory: object locators\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         indirectPointerObjectLocator = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda abstractObjectLocator copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda indirectPointerObjectLocator.

CopyDowns:
globals kleinAndYoda abstractObjectLocator. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda indirectPointerObjectLocator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addOffset: heapOffset ToOop: oop = ( |
            | oop).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfLocalMem: mem = ( |
            | 
            _NoGCAllowed.
            addressForOID: oidOfLocalMem: mem).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfRemoteMem: memOop = ( |
            | 
            addressForOID: decodeAddressOrOIDFromMemOop: memOop).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: store string\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         collectionName = 'klein indirectPointerObjectLocator'.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         decodeAddressOrOIDFromMemOop: x = ( |
            | 
            decodeOIDFromMemOop: x).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         decodeOIDFromMemOop: x = ( |
            | 
            intNN ushr: x With: vmKit tag size).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         encodeMemOopFromAddressOrOID: x = ( |
            | 
            encodeMemOopFromOID: x).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         encodeMemOopFromOID: oid = ( |
            | 
            (intNN shl: oid With: vmKit tag size) + vmKit tag mem).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         ifDirect: db IfIndirect: ib = ( |
            | 
            ib value).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         localMemForAddress: address = ( |
            | 
            localMemForOID: vmKit layouts mark oidOfMarkValue: address _UnsafeMarkValueAtAddress).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         localMemForAddress: address OID: oid = ( |
            | 
            localMemForOID: oid).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         localMemForOID: oid = ( |
            | 
            _CreateObjectReferenceWithTag: vmKit tag mem
                                 AndValue: oid).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOop: oop IfNontrivial: ntb = ( |
            | 
            decodeOIDFromMemOop: oop).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oidOfLocalMem: mem = ( |
            | 
            mem _ValuePartOfObjectReference).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oidOfMem: mem = ( |
            | 
            theVM lens oidOfMem: mem In: self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oidOfRemoteMem: mem = ( |
            | 
            decodeOIDFromMemOop: mem).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid Address: addr = ( |
            | 
            encodeMemOopFromOID: oid).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid IfAbsent: ab = ( |
            | 
            encodeMemOopFromOID: oid).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oopOfLocalMem: mem = ( |
            | 
            encodeMemOopFromOID: oidOfLocalMem: mem).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: store string\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            kleinAndYoda indirectPointerObjectLocator).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteMemForAddress: address = ( |
            | 
            encodeMemOopFromOID:
               vmKit layouts memoryObject oidOfRemoteObjectWithAddress: address IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteMemForAddress: address OID: oid = ( |
            | 
            remoteMemForOID: oid).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteMemForOID: oid = ( |
            | 
            encodeMemOopFromOID: oid).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: double dispatch\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         switchPointersFromLocalObjectWithOID: oid ToHaveAddress: newAddr = ( |
            | 
            recordAddress: newAddr ForOID: oid.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         switchPointersFromObjectWithOID: oid ToHaveAddress: newAddr = ( |
            | 
            theVM lens switchPointersFromObjectWithOID: oid
                                         ToHaveAddress: newAddr
                                                    In: self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         switchPointersFromObjectWithOop: oldOop ToHaveAddress: newAddr = ( |
            | 
            switchPointersFromObjectWithOID: (oidOfMem: oldOop)
                              ToHaveAddress: newAddr).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: double dispatch\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         switchPointersFromRemoteObjectWithOID: oid ToHaveAddress: newAddr = ( |
            | 
                         recordAddress: newAddr ForOID: oid.
            inRemoteImageRecordAddress: newAddr ForOID: oid.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         timestamp <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addressForOID: oid In: anObjectLocator IfAbsent: ab = ( |
            | 
            anObjectLocator localAddressForOID: oid IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidEntryForOID: oid In: anObjectLocator = ( |
            | 
            anObjectLocator invalidEntryForLocalOID: oid).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForInvalidEntry: f In: anObjectLocator = ( |
            | 
            anObjectLocator oidForInvalidLocalEntry: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oidOfMem: mem In: anObjectLocator = ( |
            | 
            anObjectLocator oidOfLocalMem: mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         switchPointersFromObjectWithOID: oid ToHaveAddress: newAddr In: anObjectLocator = ( |
            | 
            anObjectLocator switchPointersFromLocalObjectWithOID: oid ToHaveAddress: newAddr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         addressForOID: oid In: anObjectLocator IfAbsent: ab = ( |
            | 
            anObjectLocator remoteAddressForOID: oid IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidEntryForOID: oid In: anObjectLocator = ( |
            | 
            anObjectLocator invalidEntryForRemoteOID: oid).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForInvalidEntry: f In: anObjectLocator = ( |
            | 
            anObjectLocator oidForInvalidRemoteEntry: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         oidOfMem: mem In: anObjectLocator = ( |
            | 
            anObjectLocator oidOfRemoteMem: mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         switchPointersFromObjectWithOID: oid ToHaveAddress: newAddr In: anObjectLocator = ( |
            | 
            anObjectLocator switchPointersFromRemoteObjectWithOID: oid ToHaveAddress: newAddr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot'
        
         vmKitObjectLocator = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjectLocator' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjectLocator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitObjectLocator.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjectLocator' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjectLocator' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjectLocator' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjectLocator' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjectLocator' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.9 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjectLocator' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjectLocator InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitObjectLocator postFileIn
