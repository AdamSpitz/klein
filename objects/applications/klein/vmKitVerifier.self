 '$Revision: 30.9 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyVM: evt = ( |
            | 
            [todo cleanup "same code in other verifyVM: should factor"].
            process this birthEvent = evt ifFalse: raiseError.
            userQuery show: 'verifying'
                     While: [myProcess myVM verifyIfFail: raiseError].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyVM: evt = ( |
            | 
            [todo cleanup "same code in other verifyVM: should factor"].
            process this birthEvent = evt ifFalse: raiseError.
            userQuery show: 'verifying'
                     While: [vmImage myVM verifyIfFail: raiseError].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyWith: aVerifier = ( |
            | 
            spacesDo: [|:sp. :name| sp verifySpaceNamed: name With: aVerifier].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyIndexableOriginOf: o With: aVerifier = ( |
            | 
            indexableOriginField verifyObject: o Layout: self With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyIndexableSizeOf: o With: aVerifier = ( |
            | 
            indexableSizeField verifyObject: o Layout: self With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyVector: o WithMap: map UpTo: nextIndex With: aVerifier = ( |
             io.
             s.
            | 
            s:  verifyIndexableSizeOf:   o With: aVerifier.
            io: verifyIndexableOriginOf: o With: aVerifier.
            aVerifier unless: ((io * oopSize) + (s * elementByteSize)) <= (nextIndex * oopSize)  Error: 'indexables go past this object'.
            map verifyThatObjectSlotOffsetBoundsAreFrom: (emptyObjectSizeFor: o) To: io With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o Layout: aLayout With: aVerifier = ( |
            | 
            basicVerifyObject: o At: (offsetFor: o Layout: aLayout) Layout: aLayout With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o VerifyImmediate: anImmediateLayout At: index With: aVerifier = ( |
             oop.
            | 
            oop: for: o At: index IfFail: aVerifier failBlockReturning: 0.
            aVerifier unless: (does: oop HaveTag: anImmediateLayout myTag) FatalError: ['not a ', anImmediateLayout name].
            anImmediateLayout valueOf: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o VerifySmiAt: index With: aVerifier = ( |
            | for: o VerifyImmediate: layouts smi At: index With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: verifying\x7fCategory: finding next object\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         indexPast: o IfLastIndexVerifiedIs: lastIndex IfFail: fb = ( |
            | 
            lastIndex succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyContentsOfOop: o With: aVerifier = ( |
             map.
             mapBV.
             markAM.
             markBV.
             markValue.
             nextIndex.
            | 
            map: maps map importMapFor: o IfFail: aVerifier failBlock.
            markValue: map myLayout markValueOf: o IfFail: aVerifier failBlockReturning: 0.

            markBV: layouts mark isMarkValueForByteVector: markValue.
            mapBV: map isByteVector.
            aVerifier unless: markBV = mapBV
                       Error: 'byte vector bit in mark is wrong'.

            nextIndex: map myLayout verifyOopsOf: o With: aVerifier.

            markAM: layouts mark isMarkValueForActivationMap: markValue.
            markAM ifTrue: [| actMap |
              actMap: maps map importRemoteMap: o IfFail: aVerifier failBlock.
              aVerifier unless: actMap isMethodLike
                         Error: 'isActivationMap bit in mark is wrong'.
            ].

            map verifyObject: o UpTo: nextIndex With: aVerifier.
            for: o AddressAt: nextIndex IfFail: aVerifier failBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: verifying\x7fCategory: finding next object\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyOopsOf: o With: aVerifier = ( |
             fastResult.
            | 
            fastResult: verifyOopsQuicklyOf: o With: aVerifier.
            aVerifier ifDoubleCheckingOopVerificationCode: [
              | slowResult |
              slowResult: verifyOopsSlowlyOf:   o With: aVerifier.
              aVerifier unless: fastResult = slowResult
                         Error: 'bug in oop verification code'.
              ^ slowResult. "in case of error"
            ].
            fastResult).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: verifying\x7fCategory: finding next object\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyOopsQuicklyOf: o With: aVerifier = ( |
             lens.
             mm.
             origAddr.
             x.
            | 
            "optimized! -- dmu 8/05"
            x: addressOfMem: o.
            origAddr: x.
            mm: theVM machineMemory.
            lens: theVM lens.
            [|:exit. tag|
              x: x + oopSize.
              tag: mm tagAt: x.
              tag = vmKit tag mark  ifTrue: exit.
              tag = vmKit tag mem   ifTrue: [|cntsTag|
                cntsTag: mm tagAt: lens addressOfMem: mm oopAt: x IfFail: aVerifier failBlock.
                aVerifier unless: cntsTag = vmKit tag mark  Error: 'memOop does not point to mark'
              ]
            ] loopExit.
            (x - origAddr) / oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: verifying\x7fCategory: finding next object\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyOopsSlowlyOf: o With: aVerifier = ( |
             lastIndex.
            | 
            "clean but slow version -- dmu 8/05"
            [markField isFirstField] assert: 'This code assumes the mark is first'.
            for: o 
            Do: [ |:oop. :index| 
                   verifyImmOrMemOop: oop With: aVerifier.
                   lastIndex: index
            ]
            IfFail: aVerifier failBlock.
            indexPast: o IfLastIndexVerifiedIs: lastIndex IfFail: aVerifier failBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyImmOrMemOop: o With: aVerifier = ( |
            | 
            if:          o 
            IsImmediate: [self]
            IsMark:      [aVerifier error: 'found mark in object']
            IsMem:       [
              |addr|
              addr: addressOfMem: o.
             aVerifier unless: (theVM universe isAddressOfObjectInBounds: addr)
                        Error: 'oop out of bounds'.
             aVerifier unless: (layouts memoryObject for: o IsMarkAt: 0)
                        Error: 'oop does not point to mark'.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyBytesPartRefOf: o With: aVerifier = ( |
             bpAddr.
            | 
            bpAddr:
              layouts bytesPart addressOfBytesPart:
                layouts smi oopForValue:
                  bytesPartRefField verifyObject: o Layout: self With: aVerifier.
            aVerifier unless: (theVM universe isBytesPartAddressInBounds: bpAddr)
                       Error: 'bad bytes part ref'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyVector: o WithMap: map UpTo: nextIndex With: aVerifier = ( |
            | 
            verifyBytesPartRefOf: o With: aVerifier.
            map verifyThatObjectSlotOffsetBoundsAreFrom: (emptyObjectSizeFor: o) To: nextIndex With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         indexPast: o IfLastIndexVerifiedIs: lastIndex IfFail: fb = ( |
            | 
            lastIndex succ  +  wordsNeededToHoldIndexablesOf: o IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyOopsQuicklyOf: o With: aVerifier = ( |
             endAddr.
             endOfOopsAddr.
             io.
             lens.
             mm.
             origAddr.
             s.
             x.
            | 
            "Needs to be slightly different from the memoryObject version, to skip over the bytes part of the object. -- Adam, 4/06"
            x: addressOfMem: o.
            origAddr: x.
            io: indexableOriginOf: o IfFail: aVerifier failBlock.
             s: indexableSizeOf:   o IfFail: aVerifier failBlock.
            endOfOopsAddr: origAddr + (io * oopSize).
            endAddr: endOfOopsAddr + (bytesNeededToHoldBytes: s).
            mm: theVM machineMemory.
            lens: theVM lens.
            [|:exit. tag|
              x: x + oopSize.
              x = endOfOopsAddr     ifTrue: exit.
              tag: mm tagAt: x.
              tag = vmKit tag mark  ifTrue: [aVerifier failBlock value: 'found mark before indexableOrigin of unsegregated byteVector'].
              tag = vmKit tag mem   ifTrue: [|cntsTag|
                cntsTag: mm tagAt: lens addressOfMem: mm oopAt: x IfFail: aVerifier failBlock.
                aVerifier unless: cntsTag = vmKit tag mark  Error: 'memOop does not point to mark'.
              ]
            ] loopExit.
            aVerifier unless: (mm tagAt: endAddr) = vmKit tag mark  Error: 'byteVector does not have a mark after it'.
            (endAddr - origAddr) / oopSize).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o UpTo: nextIndex With: aVerifier = ( |
            | 
            [todo unimplemented verifying david "8/05"]. 
            "What else to check for vectors?"
            myLayout verifyVector: o WithMap: self UpTo: nextIndex With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o UpTo: nextIndex With: aVerifier = ( |
            | 
            [todo unimplemented verifying david "8/05"]. "Check block method map stuff."
            resend.verifyObject: o UpTo: nextIndex With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o UpTo: nextIndex With: aVerifier = ( |
            | 
            [todo unimplemented verifying david "8/05"]. 
            "Check block map stuff?"
            resend.verifyObject: o UpTo: nextIndex With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'foreignMap') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o UpTo: nextIndex With: aVerifier = ( |
            | 
            [todo unimplemented verifying david "8/05"].
            "Check foreignMap invariants"
            resend.verifyObject: o UpTo: nextIndex With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o UpTo: nextIndex With: aVerifier = ( |
            | 
            error: 'should never get here -- dmu 9/05').
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o UpTo: nextIndex With: aVerifier = ( |
            | 
            [unimplemented verifiying david "9/05"].
            error: 'Should never get here since we do not use this map today.\n',
                    'Maybe someday we should and move map testing from objeVectorMap to here -- dmu 9/05').
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mapMap') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o UpTo: nextIndex With: aVerifier = ( |
            | 
            [todo unimplemented verifying david "8/05"]. 
            "Check map map stuff?"
            resend.verifyObject: o UpTo: nextIndex With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mirrorMap') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o UpTo: nextIndex With: aVerifier = ( |
            | 
            [unimplemented verifying david "8/05"].
            "What else to check for mirrors?"
            resend.verifyObject: o UpTo: nextIndex With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'processMap') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o UpTo: nextIndex With: aVerifier = ( |
            | 
            [unimplemented verifying david "8/05"].
            "What else to check for processes?"
            resend.verifyObject: o UpTo: nextIndex With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'profilerMap') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o UpTo: nextIndex With: aVerifier = ( |
            | 
            [unimplemented verifying david "8/05"].
            "What else to check for profilers?"
            resend.verifyObject: o UpTo: nextIndex With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         isProgrammableSlots = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlots = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o UpTo: nextIndex With: aVerifier = ( |
            | 
            verifyThatObjectSlotOffsetBoundsAreFrom: (myLayout emptyObjectSizeFor: o) To: nextIndex With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyThatObjectSlotOffsetBoundsAreFrom: lowest To: pastHighest With: aVerifier = ( |
            | 
            objectSlotsDo: [|:name. :dataOffset|
              aVerifier unless: lowest     <= dataOffset   Error: 'slot offset is too small'.
              aVerifier unless: dataOffset <  pastHighest  Error: 'slot offset is too big'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o UpTo: nextIndex With: aVerifier = ( |
            | 
            resend.verifyObject: o UpTo: nextIndex With: aVerifier.
            [ todo verifying "fails after running image: -- dmu 9/05"
            aVerifier
             unless:   (theVM image objectsOracle oidForOop: o)
                    =  (theVM image objectsOracle oidForOriginalObject:
                          theVM universe canonicalizedStrings at:
                             maps byteVectorMap "an optimization"
                               importString: o IfFail: aVerifier failBlock)
              Error: 'canonical string problem'
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyObject: o UpTo: nextIndex With: aVerifier = ( |
            | 
            [unimplemented verifying david "8/05"].
            "What else to check for vframes?"
            resend.verifyObject: o UpTo: nextIndex With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'buffer' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyThatIEqual: aMemoryInterface = ( |
            | 
            userQuery show: 'verifying bytes at ', start printString  While: [
              buffer = (aMemoryInterface bytesAt: start Size: buffer size) ifFalse: raiseError.

              (aMemoryInterface bytesAt: start + buffer size Size: size - buffer size) do: [|:b|
                b = 0 ifFalse: raiseError.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'composite' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyThatIEqual: aMemoryInterface = ( |
            | 
            userQuery show:  'verifying memory interface copy'  While: [
              memoryInterfacesDo: [|:mi| mi verifyThatIEqual: aMemoryInterface].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyThatIEqual: aMemoryInterface = ( |
             nh.
             nl.
            | 
            nl:  lowEnd -  lowStart.
            nh: highEnd - highStart.

            userQuery show: 'verifying memory interface copy'  While: [
              userQuery show: 'verifying low words'  While: [
                lowStart upTo: lowEnd By: oopSize Do: [|:i|
                  (oopAt: i) = (aMemoryInterface oopAt: i) ifFalse: raiseError.
                ].
              ].
              userQuery show: 'verifying high bytes'  While: [
                                    (bytesAt: highStart Size: nh)
                =  (aMemoryInterface bytesAt: highStart Size: nh)
                  ifFalse: raiseError.
              ].    
              userQuery show: 'verifying low bytes'  While: [
                                    (bytesAt: lowStart Size: nl)
                =  (aMemoryInterface bytesAt: lowStart Size: nl)
                 ifFalse: raiseError.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         growingWordBuffer = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'lowBuffer' From:
             bootstrap remove: 'parent' From:
             globals kleinAndYoda memoryInterfaces growingBuffer copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda memoryInterfaces growingWordBuffer.

CopyDowns:
globals kleinAndYoda memoryInterfaces growingBuffer. copy 
SlotsToOmit: lowBuffer parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> () From: ( | {
         'Category: buffers that grow\x7fCategory: low\x7fModuleInfo: Module: vmKitVerifier InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         lowOopBuffer <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda memoryInterfaces growingWordBuffer parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single bytes\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutByte: b IfFail: fb = ( |
             m.
             o.
             x.
            | 
            i < lowEnd  ifFalse: [^ resend.at: i PutByte: b IfFail: fb].
            x: i % oopSize.
            m: 255 << (x * 8).
            at: i - x PutOop: (oopAt: i - x IfFail: [|:e| ^ fb value: e]) || (b << (x * 8))).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutBytes: bv IfFail: fb = ( |
             bi <- 0.
             n.
             wi <- 0.
            | 

            (i + bv size) <= lowEnd
              ifFalse: [ ^ resend.at: i PutBytes: bv IfFail: fb ].

            n: bv size.
            [bi < n] whileTrue: [
              lowOopBuffer at: wi Put: 
                 wordFrom: bv AtIndex: bi IfFail: raiseError.
              bi: bi + oopSize.
              wi: wi succ.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single words\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutWord: w IfFail: fb = ( |
            | 
            i < lowEnd ifFalse: [^ resend.at: i PutWord: w IfFail: fb ].
            lowOopBuffer at: i / oopSize Put: w.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single bytes\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         byteAt: i IfFail: fb = ( |
             o.
             x.
            | 
            i < lowEnd ifFalse: [^ resend.byteAt: i IfFail: fb].
            x: i % oopSize.
            o: oopAt: i - x IfFail: [|:e| ^ fb value: e].
            (o >> ((3 - x) * 8)) && 255).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesAt: i Size: n IfFail: fb = ( |
             r.
             startOfCopy.
            | 
            (i + n) <= lowEnd
              ifFalse: [ ^ resend.bytesAt: i Size: n IfFail: fb ].

            r: byteVector copySize: n.

            startOfCopy: i - lowStart.
            startOfCopy  upTo:  startOfCopy + n  By:  oopSize  Do: [
              |:byteIndex. oop|
              oop: lowOopBuffer at: byteIndex / oopSize.
              theVM myAssemblerSystem
                store: oop AsByteVectorInto: r 
                                         At: byteIndex - startOfCopy
                                     IfFail: ["if n is not an exact multiple of 4, will run off the end of r"].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fCategory: growing\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         changeSizeOfLowBufferTo: n = ( |
            | 
            lowOopBuffer: lowOopBuffer copySize: n / oopSize.
            lowEnd: lowStart + (lowOopBuffer size / oopSize).
            updateMidpoint).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: copying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSpace: space = ( |
             c.
            | 
            c:  copyAt: space bottom  Size: space sizeOfEntireRegion.
            c initializeFromVM: theVM Space: space.
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fCategory: growing\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         growLowTo: n = ( |
            | 
            changeSizeOfLowBufferTo: newSizeFor: lowOopBuffer ToGrowTo: n OtherBuffer: highBuffer).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: copying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeFromVM: aVM Space: s = ( |
             bytes.
             bytesSize.
             objs.
             oopsSize.
             verifyAfterCopy = bootstrap stub -> 'globals' -> 'false' -> ().
            | 

            "grow"
             oopsSize: s sizeOfAllocatedOopsRegion.
            bytesSize: s sizeOfAllocatedBytesRegion.

            lowOopBuffer: lowOopBuffer copySize:  oopsSize / oopSize.
              highBuffer:   highBuffer copySize: bytesSize.

            lowEnd:    lowStart +  oopsSize.
            highStart: highEnd  - bytesSize.

            objs:  aVM machineMemory bytesBypassingCacheAt: s objsBottom  Size:  oopsSize.
            bytes: aVM machineMemory bytesBypassingCacheAt: s bytesBottom Size: bytesSize.

            at: s objsBottom  PutBytes: objs.
            at: s bytesBottom PutBytes: bytes.

            verifyAfterCopy ifTrue: [
              verifyThatIEqual: aVM machineMemory.
              halt: 'if this works turn off the verification'.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot'
        
         lowBuffer = bootstrap stub -> 'globals' -> 'byteVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: private stuff\x7fCategory: growing\x7fCategory: helpers\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         newSizeFor: buf ToGrowTo: n OtherBuffer: other = ( |
             s1.
             s2.
            | 
            s1: buf size.
            s2: other size.
            lowOopBuffer = buf ifTrue: [s1: s1 * oopSize].
            lowOopBuffer = n   ifTrue: [s2: s2 * oopSize].

            [todo robustification].
            "can potentially run out of space if other has grown too much
             and we need the space for this one -- dmu 1/05"
            "If we kept track of the lowest address we've seen accessed in the
             high buffer, and vice-versa in the low buffer, we could trim them back
             if we need to."
            "The failure happens in " [updateMidpoint].
            s1 - s2 min: n + minGrowth max: s2 double).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single words\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         oopAtIndex: i IfFail: fb = ( |
            | 
            lowOopBuffer at: i / oopSize IfAbsent: [fb value: 'absent']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: testing\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         optimizesWordAccesses = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingBuffer' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: tags\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         tagAtIndex: i = ( |
            | 
            vmKit tag tagOfOop: oopAtIndex: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         totalByteSizeOfBuffers = ( |
            | 
            (lowOopBuffer size * oopSize) + highBuffer size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryInterfaces' -> 'growingWordBuffer' -> 'parent' -> () From: ( | {
         'Category: public interface\x7fCategory: single words\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         wordAt: i IfFail: fb = ( |
            | 
            i < lowEnd ifTrue: [ oopAt:         i IfFail: fb ]
                        False: [ resend.wordAt: i IfFail: fb ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         isBytesPartAddressInBounds: bpAddr = ( |
            | 
            (bytesBottom <= bpAddr) && [bpAddr < bytesTop]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyBoundariesWith: aVerifier = ( |
            | 
            aVerifier unless: 
                   (objsBottom <= objsTop)
                && [(objsTop <= bytesBottom)
                && [bytesBottom <= bytesTop]]
            Error: 'space bounds'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyBytesWith: aVerifier = ( |
             bp.
             bt.
             i <- 0.
             lastBP.
             nextBP.
            | 
            bp: layouts bytesPart bytesPartRefForAddress: bytesBottom.
            bt: layouts bytesPart bytesPartRefForAddress: bytesTop.
            [bp < bt] whileTrue: [
              nextBP: layouts bytesPart nextBytesPartAfter: bp.
              i: i succ.
              lastBP: bp.
              bp: nextBP.
            ].
            aVerifier unless: bp = bt  Error: 'bytes part of space'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         isAddressOfObjectInBounds: addr = ( |
            | 
            (objsBottom <= addr) && [addr < objsTop]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyNameIs: n With: aVerifier = ( |
            | 
            "Changed to use == instead of = so that this code can run
             inside Klein without needing to map the string-comparison
             code. -- Adam, 11/05"
            aVerifier unless: n == name  Error: 'space has wrong name'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyObjsWith: aVerifier = ( |
            | 
            objectAddressesDo: [|:addr. o|
              o: layouts memoryObject memForAddress: addr.
              layouts memoryObject verifyContentsOfOop: o With: aVerifier.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifySentinelWith: aVerifier = ( |
             s.
            | 
            s: theVM machineMemory wordAt: objsTop.
            aVerifier unless: (layouts object isMark: s)
                       Error: 'not a sentinel'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifySpaceNamed: n With: aVerifier = ( |
            | 
            verifyNameIs: n With: aVerifier.
            verifyBoundariesWith: aVerifier.
            verifyBytesWith:      aVerifier.
            verifyObjsWith:       aVerifier.
            verifySentinelWith:   aVerifier.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         isAddressOfObjectInBounds: addr = ( |
            | 
            spacesDo: [|:s| (s isAddressOfObjectInBounds: addr) ifTrue: [^ true]].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         isBytesPartAddressInBounds: bpAddr = ( |
            | 
            spacesDo: [|:s| (s isBytesPartAddressInBounds: bpAddr) ifTrue: [^ true]].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyAllNMethodsWith: aVerifier = ( |
            | 
            [unimplemented verifying david "8/05"].
            warning: 'verifyAllNMethods:With: unimplemented'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyCanonicalizedStringsWith: aVerifier = ( |
            | 
            [unimplemented verifying david "8/05"].
            "make sure each value in table is a canonical string"
            warning: 'verifyCanonicalizedStrings:With: unimplemented'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyGenerationsWith: aVerifier = ( |
            | 
            generationsDo: [|:g| g verifyWith: aVerifier].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyWith: aVerifier = ( |
            | 
            verifyGenerationsWith:          aVerifier.
            verifyCanonicalizedStringsWith: aVerifier.
            verifyAllNMethodsWith:          aVerifier.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyBoundariesWith: aVerifier = ( |
            | 
            aVerifier unless: 
                   (objsBottom <= objsTop)
                && [objsTop    <=     top]
            Error: 'space bounds'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyBytesWith: aVerifier = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifier = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda base copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> () From: ( |
             {} = 'Comment: Provides some basic services that klein objects
can choose to inherit to reduce duplication.\x7fModuleInfo: Creator: globals kleinAndYoda verifier.

CopyDowns:
globals kleinAndYoda base. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: InitializeToExpression: (raiseError)\x7fVisibility: private'
        
         failBlock <- bootstrap stub -> 'globals' -> 'raiseError' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myValueReturningFailBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda verifier parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( | {
         'Category: options\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         amIProfiled = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForVM: aVM FailBlock: fb = ( |
            | 
            ((copy vm: aVM copyToOptimizeWordAccesses ) 
             failBlock: fb)
             initializeValueReturningFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         error: msg = ( |
            | failBlock value: msg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( | {
         'Category: fake fail blocks\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         failBlockReturning: x = ( |
            | 
            myValueReturningFailBlock result: x).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         fatalError: msg = ( |
            | 
            failBlock value: msg.
            halt: msg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( | {
         'Category: options\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         ifDoubleCheckingOopVerificationCode: cb = ( |
             yes = bootstrap stub -> 'globals' -> 'false' -> ().
            | 
            yes ifTrue: cb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeValueReturningFailBlock = ( |
            | 
            myValueReturningFailBlock: protoFakeFailBlock copyFrom: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         justVerifyTheVM = ( |
            | 
            vm machineMemory invalidateCaches.
            vm machineMemory allowInfiniteSlopDuring: [
              vm setTheVMAndDo: [vm universe verifyWith: self]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( | {
         'Category: fake fail blocks\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         protoFakeFailBlock = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> 'protoFakeFailBlock' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda verifier parent protoFakeFailBlock.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> 'protoFakeFailBlock' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: InitializeToExpression: (nil)'
        
         myVerifier.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> 'protoFakeFailBlock' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> 'protoFakeFailBlock' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda verifier parent protoFakeFailBlock parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> 'protoFakeFailBlock' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         copyFrom: aVerifier = ( |
            | 
            copy myVerifier: aVerifier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> 'protoFakeFailBlock' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> 'protoFakeFailBlock' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         value: e = ( |
            | 
            myVerifier failBlock value: e.
            result).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> 'protoFakeFailBlock' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: InitializeToExpression: (nil)'
        
         result.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         unless: aBool Error: msgStringOrBlock = ( |
            | 
            aBool ifFalse: [error: msgStringOrBlock value].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         unless: aBool FatalError: msgStringOrBlock = ( |
            | 
            aBool ifFalse: [fatalError: msgStringOrBlock value]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyTheVM = ( |
            | 
            amIProfiled ifTrue: [[ justVerifyTheVM] profileSlice]
                         False: [  justVerifyTheVM]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         vm.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: initializing\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeToOptimizeWordAccessesFrom: oldVM = ( |
            | 
            initializeMirrorCache.
            lensSema: recursiveSemaphore copyBinary.
            image: oldVM image copy.  image myVM: self.
            image reassociateKleinObjectFrom: oldVM To: self.
            universe: universe copy.
            image reassociateKleinObjectFrom: oldVM universe To: universe.
            importHeapInformationFrom: oldVM image mirrorOnTheVM.
            setTheVMAndDo: [
              machineMemory: universe createBufferMemoryInterfaceUsingPrototype:
                                          vmKit memoryInterfaces growingWordBuffer.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: mapped\x7fCategory: verifying\x7fModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyIfFail: fb = ( |
            | 
            (vmKit verifier copyForVM: self FailBlock: fb) verifyTheVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot'
        
         vmKitVerifier = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVerifier' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVerifier' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitVerifier.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVerifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVerifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVerifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVerifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVerifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.9 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVerifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitVerifier InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitVarHdrsVerify
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitVarHdrsVerify' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitVerifier postFileIn
