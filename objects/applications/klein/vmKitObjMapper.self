 '$Revision: 30.13 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'fakeSlotsIterator' -> 'fakeVectorSlots' -> () From: ( | {
         'Category: Klein\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mirror: mirr PossibleVMSlotsDo: block = ( |
            | 
            "Optimization: Vector elements are never VM slots, and the mapping
             process is spending a lot of time creating them (especially their
             slot names). -- Adam, Apr. 2009"

            ifMirror: mirr HasMethodPointerDo: block).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'fakeSlotsIterator' -> () From: ( | {
         'Category: Klein\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mirror: mirr PossibleVMSlotsDo: block = ( |
            | 
            mirror: mirr Do: block).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o OID: oid UsingMap: map AndMapOop: mapOop AdjustMarkValue: initialMarkValue AndFillInHeaderFieldsFor: m Mapper: mapper = ( |
            | 
            indexableSizeField     ifShouldBeIncludedForReflecteeOf: m MapOop: mapOop Layout: self Mapper: mapper AdjustMarkValue: initialMarkValue AndDo: [|:mv1|
              indexableOriginField ifShouldBeIncludedForReflecteeOf: m MapOop: mapOop Layout: self Mapper: mapper AdjustMarkValue: mv1              AndDo: [|:mv2|
                resend.for: o OID: oid UsingMap: map AndMapOop: mapOop AdjustMarkValue: mv2 AndFillInHeaderFieldsFor: m Mapper: mapper
              ] IfNotEncodedSetValueFor: o
            ] IfNotEncodedSetValueFor: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableOriginField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         valueForReflecteeOf: m MapOop: mapOop Layout: aLayout Mapper: mapper = ( |
            | 
            aLayout indexableOriginOfReflecteeOf: m Mapper: mapper).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: layout constants\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableOriginOfReflecteeOf: m Mapper: mapper = ( |
            | 
              (lastField indexAfterMeForReflecteeOf: m Layout: self Mapper: mapper)
            + (   objectSlotWordCountForReflecteeOf: m              Mapper: mapper)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableSizeField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         valueForReflecteeOf: m MapOop: mapOop Layout: aLayout Mapper: mapper = ( |
            | 
            m reflecteeSizeIfFail: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         ifShouldBeIncludedForReflecteeOf: m MapOop: mapOop Layout: aLayout Mapper: mapper AdjustMarkValue: initialMarkValue AndDo: blk IfNotEncodedSetValueFor: o = ( |
             r.
            | 
            r: blk value: initialMarkValue.

            setValueFor: o
                     To: (valueForReflecteeOf: m MapOop: mapOop Layout: aLayout Mapper: mapper) 
                 Layout: aLayout
                 IfFail: raiseError.

            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         indexAfterMeForReflecteeOf: m Layout: aLayout Mapper: mapper = ( |
            | 
            fixedIndexAfterMe).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         emptyObjectSizeForReflecteeOf: m Mapper: mapper = ( |
            | 
            lastField fixedIndexAfterMe).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o OID: oid UsingMap: map AndMapOop: mapOop AdjustMarkValue: initialMarkValue AndFillInHeaderFieldsFor: m Mapper: mapper = ( |
             mv.
            | 
            mv: initialMarkValue || (layouts mark isByteVectorField    wordForObject: m)
                                 || (layouts mark isActivationMapField wordForObject: m)
                                 || (layouts mark oidField             wordForValue:  oid).

            mapField ifShouldBeIncludedForReflecteeOf: m MapOop: mapOop Layout: self Mapper: mapper AdjustMarkValue: mv AndDo: [|:mv1|
              for: o SetMarkValue: mv1.
            ] IfNotEncodedSetValueFor: o.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         valueForReflecteeOf: m MapOop: mapOop Layout: aLayout Mapper: mapper = ( |
            | 
            mapOop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         objectSlotWordCountForReflecteeOf: m Mapper: mapper = ( |
            | 
              (m countHowMany: [|:s| (vmKit slotType isObjectSlot: mapper policy slotTypeOf: s) && [mapper isSlotToBeMapped: s]])
            + (wordsNeededForAssignableFakeSlotsIn: m)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         wordsNeededForAssignableFakeSlotsIn: mir = ( |
            | 
            0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetBytes: bytes IfFail: fb = ( |
             bp.
            | 
            bp: bytesPartRefFor: o IfFail: [|:e| ^ fb value: e].
            layouts bytesPart forBytesPart: bp SetBytes: bytes IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetBytes: bytes IfFail: fb = ( |
            | 
            lens for: o WithLayout: self SetBytes: bytes IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalObject: o SetBytes: bv IfFail: fb = ( |
             origin.
             size.
            | 
            size: indexableSizeOf: o IfFail: [|:e| ^ fb value: e].
            bv size = size  ifFalse: [^ fb value: 'size mismatch'].

            origin: indexableOriginOf: o IfFail: [|:e| ^ fb value: e].

            bv do: [|:x. :i|
              for: bpRef UncheckedIndexableAt: i Put: x IndexableOrigin: origin IfFail: [|:e| ^ fb value: e].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteObject: o SetBytes: bv IfFail: fb = ( |
             s.
            | 
            s: indexableSizeOf: o IfFail: [|:e| ^ fb value: e].
            bv size = s  ifFalse: [theVM halt: theVM. ^ fb value: 'size mismatch'].

            machineMemory at: (addressOfFirstIndexableIn: o IfFail: [|:e| ^ fb value: e])
                    PutBytes: bv
                      IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         desiredObjectSlotIndicesOrNilForSlots: slots = ( |
             argumentSlots.
             assignableLocalSlots.
             indices.
             slotsInOrder.
            | 

            assignableLocalSlots: slots copyFilteredBy: [|:s| s isAssignable].
                   argumentSlots: slots copyFilteredBy: [|:s| s isArgument  ].

            slotsInOrder: argumentSlots, (assignableLocalSlots copySortBy: (| element: a Precedes: b = (a name < b name) |)).

            indices: dictionary copyRemoveAll.
            slotsInOrder asVector do: [|:s. :i| indices at: s kleinAndYodaSlotName Put: i + vmKit layouts objVector lastField fixedIndexAfterMe].

            indices).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         ifNecessaryInitializeArgAndAssignableLocalCount = ( |
             ac <- 0.
             lc <- 0.
            | 

            size do: [|:i. t|
              t: typeAt: i.
              case
                if: (vmKit slotType isObjectSlot:   t) Then: [lc: lc succ]
                If: [vmKit slotType isArgumentSlot: t] Then: [ac: ac succ].
            ].

            assignableLocalCount: lc.
                        argCount: ac.

            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeNonIndexableSlotsForReflecteeOf: m Mapper: mapper = ( |
            | 
            literals:      m literals.
            codes:         m codes.
            maxStackSize:  m maxStackSize.

            line:          m line.
            file:          m file.

            sourceString:  m sourceString.

            resend.initializeNonIndexableSlotsForReflecteeOf: m Mapper: mapper).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifyReflecteeOf: m Mapper: mapper = ( |
             map.
             slots.
             slotsByName.
             t.
            | 

            "A bit of duplication with" [fillInObjectAt: 0 OID: 0 AndCreateMapOfType: self ForReflecteeOf: m].

            "Ensure that the method's annotation and slot annotations are parsed."
            m annotation. m do: [|:s| s annotation].

            slots:       slotsToMapForReflecteeOf: m Mapper: mapper.
            slotsByName: dictionary copyRemoveAll.
            slots do: [|:s| slotsByName at: s kleinAndYodaSlotName Put: s].
            [slots size = slotsByName size] assert.

            map: copySize: slots size.
            mapper initializeMap: map ForReflecteeOf: m Slots: slots.

            t: mapper bytecodeTransmogrifier copyForMethod: m ActivationMap: map SlotsByName: slotsByName ExportPolicy: mapper policy.
            t interpretMethod.
            t setCodesAndLiteralsOfActivationMap.
            map cachedTransmogrifier: t.
            map).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeNonIndexableSlotsForReflecteeOf: m Mapper: mapper = ( |
            | 
            lexicalParent: mapper kleinifiedObjectForOriginalMirror: m lexicalParent.

            sourceOffset: m sourceOffset.
            sourceLength: m sourceLength.

            resend.initializeNonIndexableSlotsForReflecteeOf: m Mapper: mapper).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         addMappedSlot: s To: slots = ( |
            | 
            "Self won't let us keep the actual valueSlot in the compiledBlock
             (because it's not a real Self block object, I think), so we use
             this hack here to make sure the valueSlot gets added. -- Adam, May. 2009"

            [originalBlock_replaceThisSlotWithTheValueSlot.
             originalBlock_replaceThisSlotWithTheValueSlot: nil]. "browsing"

            s name = 'originalBlock_replaceThisSlotWithTheValueSlot' ifTrue: [
              resend.addMappedSlot: s contents valueSlot To: slots.
            ] False: [
              resend.addMappedSlot: s                    To: slots.
            ]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         slotsToMapForReflecteeOf: m Mapper: mapper = ( |
             rearrangedSlots.
             slots.
             valueSlot.
            | 
            "Rearrange the slots so that the value slot will be at index 1,
             so that we don't break" [valueSlotName].

            slots: resend.slotsToMapForReflecteeOf: m Mapper: mapper.
            valueSlot: (slots copyFilteredBy: [|:s|  'value' isPrefixOf: s name]) soleElement.
            slots remove: valueSlot.

            rearrangedSlots: list copyRemoveAll.
            slots doFirst: [|:s| rearrangedSlots add: s. rearrangedSlots add: valueSlot]
               MiddleLast: [|:s| rearrangedSlots add: s].
            rearrangedSlots).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         forReflecteeOf: m AddAllMappedVMSlotsTo: aList Mapper: mapper = ( |
            | 
            "It would be more robust to omit this method, but
             overriding it saves about 1.5 minutes out of 30 for
             building a selfVM snapshot. -- dmu 12/05"

            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         ifNeededFillInIndexableSlotsFor: m In: o Mapper: mapper = ( |
            | 
            myLayout for: o SetBytes: m reflectee IfFail: raiseError.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: method pointer fake slot\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         assignableFakeSlotForMethodPointer = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> 'assignableFakeSlotForMethodPointer' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'parent:' From:
             globals fakeSlot methodPointer copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> 'assignableFakeSlotForMethodPointer' -> () From: ( |
             {} = 'Comment: Like fakeSlot methodPointer, but assignable so that vectors
that have different method pointers can still have the same
map. -- Adam, 6/05\x7fModuleInfo: Creator: globals kleinAndYoda maps objVectorMap parent assignableFakeSlotForMethodPointer.

CopyDowns:
globals fakeSlot methodPointer. copy 
SlotsToOmit: parent parent:.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: method pointer fake slot\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         fakeSlotToAddFor: aFakeSlot = ( |
            | 
            aFakeSlot key = 'methodHolder' ifTrue: [^ assignableFakeSlotForMethodPointer copyMirror: aFakeSlot mirror].
            resend.fakeSlotToAddFor: aFakeSlot).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         fillInSimpleVector: v Oop: vOop IndexableOops: vIndexableOops Map: map MapOop: mapOop Mapper: mapper = ( |
             addr.
             orig.
            | 

            myLayout             for: vOop
                                 OID: (mapper oidForOop: vOop)
                            UsingMap: map
                           AndMapOop: mapOop
                     AdjustMarkValue: layouts mark defaultMarkValue
            AndFillInHeaderFieldsFor: (reflect: v)
                              Mapper: mapper.

            orig: myLayout indexableOriginOf: vOop.
            addr: myLayout for: vOop AddressAt: orig.
            vIndexableOops do: [|:o|
              theVM machineMemory at: addr PutOop: o.
              addr: addr + oopSize.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         ifNeededFillInIndexableSlotsFor: m In: o Mapper: mapper = ( |
            | 
            myLayout for: o PopulateIndexablesBy: [|:i| mapper allocateObject: m reflecteeAt: i].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         wordsNeededForIndexableSlotsIn: mir = ( |
            | 
            mir reflecteeSize).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         addMappedSlot: s To: slots = ( |
            | 
            slots add: s.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: adding slots\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         desiredObjectSlotIndicesOrNilForSlots: slots = ( |
            | 
            nil).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         fakeSlotToAddFor: aFakeSlot = ( |
            | 
            aFakeSlot).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         forReflecteeOf: m AddAllMappedVMSlotsTo: aList Mapper: mapper = ( |
            | 
            (fakeSlotsIterator dispatchOn: m) mirror: m PossibleVMSlotsDo: [|:s|
              (shouldMapper: mapper MapVMSlot: s) ifTrue: [| fs |
                fs: fakeSlotToAddFor: s.
                aList add: fs.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: adding slots\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         ifNecessaryInitializeArgAndAssignableLocalCount = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         ifNeededFillInIndexableSlotsFor: m In: o Mapper: mapper = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: adding slots\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeForReflecteeOf: m Slots: slots Mapper: mapper = ( |
            | 
            initializeNonIndexableSlotsForReflecteeOf: m Mapper: mapper.

            initializeSlotDescsForReflecteeOf: m Slots: slots Mapper: mapper.

            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: adding slots\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeNonIndexableSlotsForReflecteeOf: m Mapper: mapper = ( |
            | 
            annotation: objectAnnotationFor: m Mapper: mapper.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: adding slots\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeSlotDescsForReflecteeOf: m Slots: slots Mapper: mapper = ( |
             annoCache.
             argCount <- 0.
             indicesOrNil.
             initialWordOffset.
             localCount <- 0.
             mapIndex <- 0.
             maxObjectSlotIndex <- -1.
             slotOffsetsByName.
            | 
            annoCache: mapper vm slotAnnotationStringCache.
            initialWordOffset: myLayout emptyObjectSizeForReflecteeOf: m Mapper: mapper.

            indicesOrNil: desiredObjectSlotIndicesOrNilForSlots: slots.

            slotOffsetsByName: dictionary copyRemoveAll.
            slots do: [
              |:s. slotName. slotType. slotData|

              slotName: s kleinAndYodaSlotName.
              slotType: mapper policy slotTypeOf: s.
              case
                if: (vmKit slotType isMapSlot: slotType) Then: [
                  slotData:  mapper policy initialContentsOfSlot: s
                ]
                If: [vmKit slotType isAssignmentSlot: slotType] Then: [
                  slotData:  slotOffsetsByName at: slotName copyWithoutSuffix: ':'.
                ]
                Else: [
                  slotData:  indicesOrNil ifNil:    [maxObjectSlotIndex: maxObjectSlotIndex succ.
                                                     initialWordOffset + maxObjectSlotIndex]
                                          IfNotNil: [indicesOrNil at: s name].
                  mapper verifyThatItIsOKForSlot: s ToHaveOffset: slotData.
                  slotOffsetsByName at: slotName Put: slotData.
                ].

              at: mapIndex PutName: slotName
                              Type: slotType
                              Data: slotData
                        Annotation: slotAnnotationFor: s Cache: annoCache Mapper: mapper.

              mapIndex: mapIndex succ.
            ].

            ifNecessaryInitializeArgAndAssignableLocalCount.

            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: adding slots\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         objectAnnotationFor: m Mapper: mapper = ( |
            | 
            mapper policy shouldOmitObjectAnnotations
               ifTrue: ''
                False: [mapper vm objectAnnotationStringCache at: m annotation]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldMapper: mapper MapVMSlot: s = ( |
            | 
            "ignore reflectee slot because we cannot
             export reflectees that point to activations
             (eg. blockMethodActivation) and this seems
             to be a good criteria in the general case
             also -- jb 5/03"

            "ignore literals, codes, sourceString, etc., because they're
             stored as regular data slots on the methodMap. -- Adam, 1/06"

            (   s isVectorElement
            || [s isReflectee
            || [s isLiterals
            || [s isCodes
            || [s isLexicalParent
            || [s isSourceString
            || [s isSourceLine
            || [s isSourceFile
            || [s isSourceOffset
            || [s isSourceLength]]]]]]]]]) not).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: adding slots\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         slotAnnotationFor: s Cache: annoCache Mapper: mapper = ( |
            | 
            mapper policy shouldOmitSlotAnnotations
               ifTrue: ''
                False: [annoCache at: s annotation]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: adding slots\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         slotSorter = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> 'slotSorter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps slotsMap parent slotSorter.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> 'slotSorter' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         element: a Precedes: b = ( |
            | a name < b name).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> 'slotSorter' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         slotsToMapForReflecteeOf: m Mapper: mapper = ( |
             slots.
            | 
            slots: list copyRemoveAll.
            m do: [|:s| (mapper policy isSlotToBeMapped: s) ifTrue: [addMappedSlot: s To: slots]].
            forReflecteeOf: m AddAllMappedVMSlotsTo: slots Mapper: mapper.
            slots do: [|:s| mapper recordSlot: s].
            slots).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         totalWordCountForReflecteeOf: m Mapper: mapper = ( |
            | 
            [todo optimization]. "Would it be possible to not lose the information we
                                  gather here about which slots are object slots and
                                  so on? Keep it in a dictionary in the mapper, and
                                  then just access it when we go to fill the object in?
                                  -- Adam, 11/05"

              (myLayout     emptyObjectSizeForReflecteeOf: m Mapper: mapper)
            + (myLayout objectSlotWordCountForReflecteeOf: m Mapper: mapper)
            + (wordsNeededForIndexableSlotsIn: m)).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: adding slots\x7fCategory: counting words\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         wordsNeededForIndexableSlotsIn: mir = ( |
            | 0).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         ifNeededFillInIndexableSlotsFor: m In: o Mapper: mapper = ( |
            | 
            resend.ifNeededFillInIndexableSlotsFor: m In: o Mapper: mapper.
            mapper recordString: m reflectee.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: building VMs\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         objectMapper1 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         alreadyCompiledLookupTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: cache stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         blockNMethodHits <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: cache stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         blockNMethodMisses <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: cache stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         blockOuterMethodProbes <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: transient state\x7fCategory: caches\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         cachedResendsBySlot <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: transient state\x7fCategory: caches\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         cachedReusableNMethodOIDsBySlotAndKey <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         canonicalizedStrings.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         compactMapTableTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         compilationPhaseTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         compilationRequestersThatHaveJustRun <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         compilationRequestersToRun <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         compilationTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         compileCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: transient state\x7fCategory: caches\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         compiledBlocksByNMethodOID <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         eagerNMethodLoadingRelocationTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: cache stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         eagerNMethodOopAssignmentOptimismJustified <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: cache stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         eagerNMethodOopAssignmentOptimismNotJustified <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         exemplarMirrorsLeftToCompile <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         fill1Time <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         fill2Time <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         fill3Time <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         fill4Time <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         fill5Time <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         fillingDuringCompilationTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         fixingKleinMirrorsTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         isInCompilationPhase <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         isInNMethodCacheConstruction <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         kleinPrimitiveMirrorOops <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         myVM.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         nmethodAllocationTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         nmethodCacheConstructionTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: cache stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         nmethodsByMapHits <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: cache stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         nmethodsByMapMisses <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         numberOfLocalsThatDoNotNeedToBeInitialized <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         numberOfLocalsThatNeedToBeInitialized <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         objectTableTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         objectsOracle.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         oopsToFillIn <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         addNMethodOID: nmOID MapOID: mapOID LookupKey: key = ( |
            | 
            objectsOracle addNMethodOID: nmOID
                                 MapOID: mapOID
                              LookupKey: key.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateAndQuicklyFillInSimpleVector: v IndexableOops: vIndexableOops UsingMap: map MapOop: mapOop = ( |
             twc.
             vMir.
             vOop.
            | 
            fillInObjects. "ensure all filled in up to here"
            vMir: reflect: v.

            "There used to be an optimization here for calculating the total word count,
             but it didn't make much difference - only saved us about 0.2 seconds in the
             whole 5-minute mapping cycle. -- Adam, 7/05"
            twc: totalWordCountForReflecteeOf: vMir.

            vOop: allocateMemoryObjectFor: vMir OfTotalWordCount: twc OriginalMirror: vMir.
            quicklyFillInSimpleVector: v Oop: vOop UsingMap: map MapOop: mapOop IndexableOops: vIndexableOops.
            vOop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: immediates\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateImmediateMaps = ( |
            | 
            "Ensure that there will be maps for the immediates."
            immediateExemplarsDo: [|:imm| objectsOracle recordMapOID: (oidForOop: allocateMapForImmediate: imm)
                                                ForImmediateExemplar: imm]. 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: maps\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateMap: m ForOID: oid AndDo: blk = ( |
             cm.
             cmOop.
            | 
            objectsOracle
                        canonicalMapFor: m
                            IfPresentDo: [|:canonicalMap| cm:    canonicalMap.
                                                          cmOop: objectsOracle oopForOriginalObject: cm]
              IfAbsentRecordExemplarOID: oid
                                  AndDo: [cm: m.
                                          cmOop: allocateObject: cm.
                                          exemplarMirrorsLeftToCompile add: originalMirrorForOID: oid].
            blk value: cm With: cmOop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: immediates\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateMapForImmediate: imm = ( |
             map.
            | 
            map: imm asMirror vmKitMapForConversion.
            map recordYourselfInTheUniverse.
            allocateObject: map).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateMemoryObjectFor: kMir OfTotalWordCount: totalWordCount OriginalMirror: origMir = ( |
             oop.
            | 
            [kMir isReflecteeBlock not || [totalWordCount = layouts block numberOfWordsInABlock]] assert.
            oop: allocateMemoryObjectTotalWordCount: totalWordCount OriginalMirror: origMir KleinifiedMirror: kMir.
            rememberToFillInOop: oop.
            showAllocatedObjectFor: origMir.
            oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateMemoryObjectForReflecteeOf: aMir = ( |
             isAbsent <- bootstrap stub -> 'globals' -> 'false' -> ().
             oop.
            | 

            "Make sure the object is actually not allocated yet.
             We may have encountered it during a previous cycle via
             a different reference."

            [todo optimize]. "Possible optimization: do an IfAbsentPut: rather than
             having to do the dictionary lookup twice. (We used to have this
             optimization, but now we're changing the data structures and the
             optimization would be more awkward.) It doesn't seem to be very
             significant right now, so it's probably not worth worrying about;
             the code is cleaner without the optimization. -- Adam, 6/05"

            "Contorted slightly to make profiles come out prettier."

            oop: objectsOracle
                   oopForOriginalObject: aMir reflectee
                               IfAbsent: [isAbsent: true].

            isAbsent ifTrue: [oop: allocateNewMemoryObjectForReflecteeOf: aMir].

            oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateMemoryObjectTotalWordCount: totalWordCount OriginalMirror: originalMirror KleinifiedMirror: kleinifiedMirror = ( |
             addr.
             oid.
             oop.
             p.
            | 
            addr:
              kleinifiedMirror isReflecteeByteVector
                ifTrue: [allocateOops: totalWordCount AndBytes: kleinifiedMirror reflecteeSize]
                 False: [allocateOops: totalWordCount                                         ].
            oid: allocateOIDForOriginalMirror: originalMirror.
            oop: myVM objectLocator oopForOID: oid Address: addr.
            objectsOracle recordOop: oop AndAddress: addr ForOID: oid KleinifiedMirror: kleinifiedMirror OriginalMirror: originalMirror.
            oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateNewMemoryObjectForReflecteeOf: aMir = ( |
             kMir.
             oop.
             twc.
            | 
            kMir:  aMir kleinifyForObjectMapper: self.

            twc: totalWordCountForReflecteeOf: kMir.
            oop: allocateMemoryObjectFor: kMir
                        OfTotalWordCount: twc
                          OriginalMirror: aMir.

            kMir isReflecteeKleinOrYodaPrimitiveMirror ifTrue: [
              kMir reflectee !== kMir reflectee prototype ifTrue: [
                (kMir reflectee isForVM: myVM) ifTrue: [
                  recordKleinMirrorOop: oop.
                ].
              ].
            ].

            oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateOIDForOriginalMirror: originalMirror = ( |
            | 
            objectsOracle getANewOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateObject: objToMap = ( |
            | 
            allocateObjectForReflecteeOf: reflect: objToMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateObjectForReflecteeOf: objMirror = ( |
            | 
            objMirror isReflecteeKleinAndYodaImmediate
                ifTrue: [objMirror kleinAndYodaLayout oopForValue: objMirror reflectee]
                 False: [allocateMemoryObjectForReflecteeOf: objMirror]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: allcoation\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateOops: nOops = ( |
            | 
            objectsOracle allocateOops: nOops).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: allcoation\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateOops: nOops AndBytes: nBytes = ( |
            | 
            objectsOracle allocateOops: nOops AndBytes: nBytes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: caching nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocatePrototypeNMethodCache = ( |
            | 
            prototypeNMethodCacheOop: allocateObject: vmKit nmethodCache copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateStartingPoints: sp = ( |
            | 
            sp do: [|:o| allocateObject: o].
            allocatePrototypeNMethodCache.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         asSeconds: ms = ( |
            | 
            (ms asFloat / 1000) round asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: blk = ( |
            | 
            vm assert: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         base* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: transmogrifying bytecodes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         bytecodeTransmogrifier = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals abstractBytecodeInterpreter copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent bytecodeTransmogrifier.

CopyDowns:
globals abstractBytecodeInterpreter. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         bytecodesByOriginalPC <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         exportPolicy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         firstNewBCForThisOldBC.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (-1)\x7fVisibility: private'
        
         lastOldPC <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         newActivationMap.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         newBytecodes <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (-1)\x7fVisibility: private'
        
         nextPC <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         oldBCNowBeingTransmogrified.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         oldPCsByNewPC <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         originalBytecodes <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent bytecodeTransmogrifier parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fCategory: interpreting particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         accessLocal: bc = ( |
            | 
            popBytecodesFor: bc.
            push: bc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         addInitializationBytecodesTo: aList = ( |
            | 
            newActivationMap objectSlotsDo: [|:slotName. slot. contentsObj. i|
              slot:         slotsByName at: slotName.
              contentsObj:  exportPolicy initialContentsOfSlot: slot.
              i:            method indexOfLocalNamed: slot name IfFail: raiseError.

              aList addAll: bytecodesToInitializeSlotWithIndex: i ToLiteral: contentsObj.
            ].
            aList).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fCategory: particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         addNewBytecode: newBC = ( |
            | 
            firstNewBCForThisOldBC ifNil: [firstNewBCForThisOldBC: newBC].

            newBC pc: nextPC.
            oldPCsByNewPC at: nextPC Put: oldBCNowBeingTransmogrified pc.
            nextPC: nextPC succ.
            newBytecodes add: newBC.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         bytecodesToInitializeSlotWithIndex: slotIndex ToLiteral: lit = ( |
            | 
            (  ((bytecodes literal    copy interpreter: self) oopToPush: lit)
            & (((bytecodes writeLocal copy interpreter: self) indexOfLocal: slotIndex) lexicalLevel: 0)
            &   (bytecodes pop        copy interpreter: self)
            ) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForMethod: m ActivationMap: map SlotsByName: ss ExportPolicy: p = ( |
            | 
            (((copyForMethod: m) newActivationMap: map) slotsByName: ss) exportPolicy: p).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         endOfBytecodesPlaceholder = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> 'endOfBytecodesPlaceholder' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent bytecodeTransmogrifier parent endOfBytecodesPlaceholder.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> 'endOfBytecodesPlaceholder' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> 'endOfBytecodesPlaceholder' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent bytecodeTransmogrifier parent endOfBytecodesPlaceholder parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> 'endOfBytecodesPlaceholder' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> 'endOfBytecodesPlaceholder' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         pc <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         firstBCInvolvedInEvaluatingBC: bc = ( |
             poppedBCs.
             x.
            | 
            x: bc.
            [poppedBCs: poppedBytecodesFor: x.
             poppedBCs isEmpty]
               whileFalse: [x: poppedBCs first].
            x).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         fixBranchDestinationsAndRecreateFullListOfBytecodesFrom: bcs = ( |
            | 
            replaceBranchDestinationsWithPlaceholdersInBytecodes: bcs.
            newActivationMap literals: (literalsNeededForBytecodes: bcs) asVector.
            recreateFullListOfBytecodesFrom: bcs.
            replacePlaceholdersWithNewBranchDestinationsInBytecodes: newBytecodes BytecodesByOriginalPC: bytecodesByOriginalPC.
            newBytecodes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fCategory: particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         forOldBC: oldBC Generate: blk = ( |
            | 
            [firstNewBCForThisOldBC      isNil] assert.
            [oldBCNowBeingTransmogrified isNil] assert.
            oldBCNowBeingTransmogrified: oldBC.

            blk value.
            [firstNewBCForThisOldBC isNotNil] assert.

            [lastOldPC < oldBCNowBeingTransmogrified pc] whileTrue: [
              lastOldPC: lastOldPC succ.
              bytecodesByOriginalPC at: lastOldPC Put: firstNewBCForThisOldBC.
            ].

            firstNewBCForThisOldBC:      nil.
            oldBCNowBeingTransmogrified: nil.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fCategory: particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         generateBranchBytecode: bc = ( |
            | 
            forOldBC: bc Generate: [
              bc operandIfPresent: [|:d| generateCodesFor: bc Literal: d] IfAbsent: raiseError
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fCategory: particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCodesFor: bc = ( |
            | 
            addNewBytecode: (bc copy interpreter: self) bytecodeValue: instructionSet bytecodeNamed: bc name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fCategory: particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCodesFor: bc Index: i = ( |
            | 
            bytecodeFormat breakUpIndex: i AndDo: [|:iLow. :iHigh|
              iHigh = 0 ifFalse: [
                generateCodesFor: (bytecodes index copy interpreter: self) Index: iHigh.
              ].
              addNewBytecode: (bc copy interpreter: self) indexValue: iLow.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fCategory: particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCodesFor: bc Literal: lit = ( |
            | 
            generateCodesFor: bc Index:  newActivationMap indexOfLiteral: lit IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fCategory: particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         generateDelegateeBytecode: bc = ( |
            | 
            forOldBC: bc Generate: [
              generateCodesFor: bc Literal: bc delegatee
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fCategory: particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         generateLexicalLevel: ll = ( |
            | 
            ll = 0 ifTrue: [^ self].
            generateCodesFor: (bytecodes lexicalLevel copy interpreter: self) Index: ll.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fCategory: particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         generateLiteralBytecode: bc = ( |
            | 
            forOldBC: bc Generate: [
              generateCodesFor: bc Literal: bc oopToPush
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fCategory: particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         generateLocalBytecode: bc = ( |
            | 
            forOldBC: bc Generate: [
              generateLexicalLevel: bc lexicalLevel.
              generateCodesFor: bc Index: (lexicalParentActivationMapAtLevel: bc lexicalLevel) indexOfLocalNamed: bc slot name IfFail: raiseError.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fCategory: particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         generateParameterlessBytecode: bc = ( |
            | 
            forOldBC: bc Generate: [
              addNewBytecode:  bc copy bytecodeValue: instructionSet bytecodeNamed: bc name.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fCategory: particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         generateSendBytecode: bc = ( |
            | 
            forOldBC: bc Generate: [
              bc hasDelegatee                    ifTrue: [generateCodesFor:  bytecodes delegatee         Literal:  bc delegatee    ].
              bc isUndirectedResend              ifTrue: [generateCodesFor:  bytecodes undirectedResend                            ].
              (needArgumentCountBCForSendBC: bc) ifTrue: [generateCodesFor:  bytecodes argumentCount     Index:    bc argumentCount].

                                                          generateCodesFor:  bc                          Literal:  bc selector.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         if: bc IsForConstantLocalSlot: clBlk IsFakePrimitiveSend: fpBlk = ( |
            | 
            case
              if: bc isLocal && [bc isRead] Then: [| slot |
                slot: bc slot.
                slot isAssignable || [slot isArgument] ifFalse: [
                  clBlk value: slot.
                ]
              ]
              If: [bc isSend && [bc isPrimitive]] Then: [
                theVM vmKit primitives ifSelector: bc selector IsAFakePrimitiveThen: fpBlk Else: [].
              ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fCategory: interpreting particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         indexedBranch: bc = ( |
            | 
            popBytecodesFor: bc.
            push: bc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            resend.initialize.
            originalBytecodes: originalBytecodes copyRemoveAll.
            newBytecodes:      newBytecodes      copyRemoveAll.
            oldPCsByNewPC:     oldPCsByNewPC     copyRemoveAll.
            stackBCs:          stackBCs          copyRemoveAll.
            poppedBCsByPopper: poppedBCsByPopper copyRemoveAll.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         interpret: bc = ( |
            | 
            originalBytecodes addLast: bc.
            resend.interpret: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         interpretMethod = ( |
            | 
            resend.interpretMethod.
            [stackBCs size <= 1] assert.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         lexicalParentActivationMapAtLevel: lexicalLevel = ( |
             lp.
            | 
            lp: newActivationMap.
            lexicalLevel do: [
              lp: lp lexicalParent.

              [todo cleanup importingActivationMaps]. "Well, this is a hack. The problem
               is that if this is an imported blockActivationMap, its lexicalParent slot
               will contain the oop of the remote lexical parent map, not the imported
               lexical parent map itself. We could fix that by always importing the lexical
               parents of any blockActivationMaps that we import. -- Adam, 5/06"
              (reflect: lp) isReflecteeInteger ifTrue: [
                lp: theVM vmKit maps map importRemoteMap: lp IfFail: raiseError.
              ].
            ].
            lp).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         literalsNeededForBytecodes: bcs = ( |
             alreadySeen.
             r.
            | 
            alreadySeen: reflectiveIdentitySet copyRemoveAll.
            r: list copyRemoveAll.
            bcs do: [|:bc|
              bc isAPrefixCode ifFalse: [
                bc neededLiteralsDo: [|:lit|
                  alreadySeen if: lit IsAbsentAddAndDo: [r add: lit].
                ].
              ].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fCategory: particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         needArgumentCountBCForSendBC: sendBC = ( |
            | 
            instructionSet includesArgumentCount && [sendBC argumentCount > 0]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fCategory: interpreting particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         nonlocalReturn: bc = ( |
            | 
            popBytecodesFor: bc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fCategory: interpreting particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         pop: bc = ( |
            | 
            popBytecodesFor: bc.
            [stackBCs isEmpty] assert.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         popBytecodesFor: bc = ( |
             n.
             r.
            | 
            n: bc popCount.
            r: vector copySize: n.
            n do: [|:i| r at: n pred - i Put: stackBCs removeLast].
            poppedBCsByPopper at: bc Put: r.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         poppedBytecodesFor: bc = ( |
            | 
            poppedBCsByPopper at: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         positionTablePositionsByBCI = ( |
             currentBCI <- 0.
             i.
             noSlotsNonILBCs <- 0.
             origCodes.
             origIS.
             origNonILBCs <- 0.
             origNonILBCsByBCI.
             positionsByBCI.
             reparseCodes.
             reparseHasFmtBC.
             reparseIS.
             v.
            | 

            "Like" [forMethodMirror: m PositionForPositionTableAtBCI: bci].
            "But calculates the answers for all BCIs, as an optimization. -- Adam, 5/06"

            origCodes: method codes.
            origIS: bytecodeFormat instructionSetForCodes: origCodes.
            i: origIS firstBCI.

            origNonILBCsByBCI: vector copySize: origCodes size.

            [|:exit. bc. n|
              i < origCodes size  ifFalse: exit.
              bc: origCodes byteAt: i.
              n: origIS opcodeNameOf: bc.
                  (n = 'lexicalLevel') 
              || [(n = 'index')
              || [ n = 'argumentCount']]
                ifFalse: [origNonILBCs: origNonILBCs succ].
              origNonILBCsByBCI at: i Put: origNonILBCs.
              i: i succ.
            ] loopExit.

            reparseCodes: method source parseObjectBody codes.
            reparseIS: bytecodeFormat instructionSetForCodes: reparseCodes.
            positionsByBCI: vector copySize: origCodes size FillingWith: reparseCodes size pred.
            currentBCI: origIS firstBCI.
            [((origNonILBCsByBCI at: currentBCI) = 0) && [currentBCI < origNonILBCsByBCI size]] whileTrue: [currentBCI: currentBCI succ].
            i: reparseIS firstBCI.

            [|:exit. bc. n|
              i < reparseCodes size  ifFalse: exit.
              bc: reparseCodes byteAt: i.
              n: reparseIS opcodeNameOf: bc.
                  (n = 'lexicalLevel') 
              || [(n = 'index')
              || [ n = 'argumentCount']]
               ifFalse: [ 
                 noSlotsNonILBCs: noSlotsNonILBCs succ.
                 [(origNonILBCsByBCI at: currentBCI) = noSlotsNonILBCs] whileTrue: [
                   positionsByBCI at: currentBCI Put: i.
                   currentBCI: currentBCI succ.
                   currentBCI = positionsByBCI size ifTrue: [^ positionsByBCI].
                 ].
               ].
              i: i succ.
            ] loopExit.
            positionsByBCI).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         push: bc = ( |
            | 
            stackBCs addLast: bc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fCategory: interpreting particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         pushLiteral: bc = ( |
            | 
            popBytecodesFor: bc.
            push: bc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fCategory: interpreting particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         pushSelf: bc = ( |
            | 
            popBytecodesFor: bc.
            push: bc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         receiverAndArgumentBCsOfSendBC: bc = ( |
             r.
            | 
            r: poppedBytecodesFor: bc.
            bc isSelfImplicit ifTrue: [r: r copy addFirst: nil].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         recreateCodesStringFromBytecodes: bcs = ( |
             c.
            | 
            c: instructionSet recreateInstructionSetBytecodeIfNone: ''.
            bcs do: [|:bc| c: c & bc bytecodeValue asCharacter].
            c flatString canonicalize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         recreateFullListOfBytecodesFrom: bcs = ( |
            | 
            nextPC: instructionSet firstBCI.
            lastOldPC: -1.
            bytecodesByOriginalPC: dictionary copyRemoveAll.
            bcs do: [|:bc|
              bc isAPrefixCode ifFalse: [
                bc transmogrifyYourselfUsing: self.
              ].
            ].
            bytecodesByOriginalPC  at:  bcs last pc succ  Put:  endOfBytecodesPlaceholder copy pc: nextPC.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         replaceBranchDestinationsWithPlaceholdersInBytecodes: bcs = ( |
            | 
            bcs do: [|:bc|
              bc isBranch ifTrue: [
                "We haven't changed its destination(s) yet, so the bc object still holds the old destination(s)."
                bc mapDestinationsBy: [|:oldDestinationPC| unresolvedDestination copyForOldDestinationPC: oldDestinationPC].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         replacePlaceholdersWithNewBranchDestinationsInBytecodes: newBCs BytecodesByOriginalPC: bytecodesByOriginalPC = ( |
            | 
            newBCs do: [|:bc|
              bc isBranch ifTrue: [| i |
                bc operandIfPresent:  [|:unresolvedDest| i: newActivationMap indexOfLiteral: unresolvedDest IfFail: raiseError] IfAbsent: raiseError.
                bc mapDestinationsBy: [|:unresolvedDest| (bytecodesByOriginalPC at: unresolvedDest oldDestinationPC) pc].
                bc operandIfPresent:  [|:newLit. newLitMir|
                                       newLitMir: reflect: newLit.
                                       [newLitMir isReflecteeVector || [newLitMir isReflecteeInteger]] assert.
                                       newActivationMap literals at: i Put: newLit]
                           IfAbsent: raiseError.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fCategory: interpreting particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         scalarBranch: bc = ( |
            | 
            popBytecodesFor: bc.
            push: bc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fCategory: interpreting particular codes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         send: bc = ( |
            | 
            popBytecodesFor: bc.
            push: bc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         setCodesAndLiteralsOfActivationMap = ( |
             positions.
            | 
            instructionSet: bytecodeFormat instructionSets kleinAndYoda.
            newActivationMap codes: recreateCodesStringFromBytecodes: fixBranchDestinationsAndRecreateFullListOfBytecodesFrom: transmogrifiedBytecodes.

            positions: positionTablePositionsByBCI.
            positionTableIndicesByKleinBCI: vector copySize: newActivationMap codes size FillingWith: 0.
            oldPCsByNewPC do: [|:oldPC. :newPC| positionTableIndicesByKleinBCI at: newPC Put: positions at: oldPC].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         transmogrifiedBytecodes = ( |
             endInitBC.
             r.
            | 
            r: list copyRemoveAll.

            addInitializationBytecodesTo: r.
            endInitBC: bytecodes endInit copy interpreter: self.
            r addLast: endInitBC.

            originalBytecodes do: [|:oldBC|
              oldBC isAPrefixCode ifFalse: [| newBC |
                newBC: oldBC.

                if: oldBC
                IsForConstantLocalSlot: [|:slot. contentsObj|
                  contentsObj:  exportPolicy initialContentsOfSlot: slot.
                  newBC:  ((bytecodes literal copy interpreter: self) oopToPush: contentsObj) pc: oldBC pc.
                ]
                IsFakePrimitiveSend: [|:fpMethodName. firstBC. firstBCPC. bcsToInsert|
                  firstBC: firstBCInvolvedInEvaluatingBC: oldBC.
                  firstBCPC: firstBC pc.
                  bcsToInsert: list copyRemoveAll.
                                                bcsToInsert add: ((bytecodes literal  copy interpreter: self) pc: firstBCPC) oopToPush: theVM vmKit primitives.
                  oldBC isSelfImplicit ifTrue: [bcsToInsert add:  (bytecodes pushSelf copy interpreter: self) pc: firstBCPC].
                  r               insertAll: bcsToInsert
                    BeforeElementSatisfying: [|:newBC| newBC pc = firstBCPC]
                                   IfAbsent: [r addAll: bcsToInsert].
                  newActivationMap maxStackSize: newActivationMap maxStackSize + bcsToInsert size.

                  ('IfFail:' isSuffixOf: oldBC selector) ifFalse: [
                    r add: ((bytecodes literal  copy interpreter: self) pc: firstBCPC) oopToPush: theVM vmKit primitives objectToUseIfNoExplicitFailBlock.
                    newActivationMap maxStackSize: newActivationMap maxStackSize + 1.
                  ].
                  newBC:  bytecodes send copy. "can't just copy the oldBC because the old one might have been an implicitSelfSend, and the new one shouldn't be"
                  newBC interpreter:    oldBC interpreter.
                  newBC bytecodeValue:  oldBC bytecodeValue.
                  newBC pc:             oldBC pc.
                  newBC selector:       fpMethodName.
                  newBC argumentCount:  (selector copyStr: fpMethodName) numberOfArguments.
                ].

                r add: newBC.
              ].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: transmogrifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         unresolvedDestination = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> 'unresolvedDestination' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent bytecodeTransmogrifier parent unresolvedDestination.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> 'unresolvedDestination' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (-1)\x7fVisibility: private'
        
         oldDestinationPC <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> 'unresolvedDestination' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> 'unresolvedDestination' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent bytecodeTransmogrifier parent unresolvedDestination parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> 'unresolvedDestination' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForOldDestinationPC: old = ( |
            | 
            copy oldDestinationPC: old).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> 'unresolvedDestination' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (identityDictionary copyRemoveAll)\x7fVisibility: private'
        
         poppedBCsByPopper <- identityDictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         positionTableIndicesByKleinBCI <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         slotsByName.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         stackBCs <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationRequester = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent compilationRequester.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         context.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         couldReuseTheNMethod.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (reflectiveIdentitySet copyRemoveAll)'
        
         methodSlotsCompiled <- reflectiveIdentitySet copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         nmethodOID.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         objectMapper.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent compilationRequester parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: this method\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         buildNMethod = ( |
             c.
             nm.
             nmOop.
             predictedOop.
            | 
            objectMapper incrementCompileCount.
            objectMapper compilationTime:
              objectMapper compilationTime + [
                c: createCompiler.
                c: c compileForcingNonLeafIfNecessary.
                nm: c buildNMethod.
                klein relocators isEagerRelocationEnabled ifTrue: [|twc|
                  twc: objectMapper totalWordCountForReflecteeOf: reflect: nm.
                  predictedOop: c oracleForEagerRelocation nextOopToAllocateForObjectOfSize: twc.
                ].
                objectMapper incorporateCodeGenerationStatisticsFrom: c codeGenerator.
            ] cpuTime.

            objectMapper nmethodAllocationTime:
              objectMapper nmethodAllocationTime + [
                nmethodOID: objectMapper vm nextFreeOID.
                nmOop: objectMapper allocateMemoryObjectForReflecteeOf: nm asMirror.
                [objectMapper vm nextFreeOID = nmethodOID succ] assert.
            ] cpuTime.

            klein relocators isEagerRelocationEnabled ifTrue: [
              objectMapper eagerNMethodLoadingRelocationTime:
                objectMapper eagerNMethodLoadingRelocationTime + [
                  reassembleNMethod: nm WithOop: nmOop PredictedOop: predictedOop CodeGenerator: c codeGenerator.
              ] cpuTime.
            ].

            objectMapper objectsOracle resendBytecodesByNMethodOID at: nmethodOID Put: c resendsByContextAndSelfMap.

            c irNodeGenerator topBytecodeInterpreter allBytecodeInterpretersDo: [|:i. sla. bs|
              sla: i sourceLevelAllocator.
              bs: sla memoizedBlockValues keys copyMappedBy: [|:bm| bm reflectee].
              compiledBlocksWithContextAndSelfMap add: (bs & sla context & selfMap) asVector.

              sla context slot isMethod ifTrue: [methodSlotsCompiled add: sla context].
            ].

            nmethodOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         canReuseNMethod: nm = ( |
             rcs.
            | 
            rcs: objectMapper objectsOracle reusabilityConditionsForNMethod: nm.
            rcs allSatisfy: [|:c| c isSatisfiedBy: self]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         checkWhetherNMethodWasAlreadyCompiled = ( |
            | 
            objectMapper alreadyCompiledLookupTime: objectMapper alreadyCompiledLookupTime + [
              nmethodOIDIfPresent: [|:nmOID| nmethodOID: nmOID] IfAbsent: [].
              wasAlreadyCompiled: nmethodOID isNotNil.
            ] cpuTime.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: other methods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationRequesterForResend: bc Context: cntxt = ( |
             key.
             resendContext.
             resentSlot.
            | 
            "Let missing resent methods be an error at runtime rather than compile time, now
             that we're exporting way more stuff - we've got lots of resends that are never
             getting called, and it's annoying to have to pull in an entire module just
             because we resend to one method in it. -- Adam & Alex, 5/04"
            resentSlot: ((klein lookupKey copyForSendBC: bc) lookupSlotsUsing: protoSlotFinder Self: cntxt selfMap Holder: cntxt outermostMethodHolder)
                                             ifNone: [ ^ nil]
                                             IfOne:  [|:s| s]
                                             IfMany: [error: 'ambiguous resend'].

            key: klein lookupKey copyForSelector: resentSlot name
                                      LookupType: bc kleinBaseLookupType
                            ObjectDoingTheResend: cntxt outermostMethodHolder reflectee
                                      SlotHolder: [resentSlot holder reflectee].

            resendContext:
              theVM compilerPrototype prototypes compilationContext
                           copyForSlot: (objectMapper objectsOracle kleinifySlot: resentSlot)
                                   Key: key
                                  Self: cntxt selfMap
                              Receiver: cntxt selfMap
                    LexicalParentScope: nil.

            objectMapper compilationRequester copyForObjectMapper: objectMapper Context: resendContext).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: other methods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationRequestersForBlockLiterals = ( |
             crs.
            | 
            crs: list copyRemoveAll.
            compiledBlocksWithContextAndSelfMap do: [|:blocksAndContextAndSelfMap. c. m. bs|
              "Need to replace the original selfMap with my selfMap, because if I'm reusing this
               nmethod, the selfMap won't be right."
              bs:  blocksAndContextAndSelfMap at: 0.
              m:   blocksAndContextAndSelfMap at: 2.
              c:  (blocksAndContextAndSelfMap at: 1) replaceMap: m With: selfMap.

              bs do: [|:compiledBlock|
                "The compiledBlock might not have been mapped if it's a failblock for a primitive
                 that can't fail."
                (objectMapper objectsOracle includesOriginalObject: compiledBlock) ifTrue: [
                  crs add: objectMapper compilationRequester
                                           copyForBlockMirror: (reflect: compiledBlock)
                                                 ObjectMapper: objectMapper
                                         LexicalParentContext: c.
                ].
              ].
            ].
            crs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: other methods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationRequestersForResends = ( |
             crs.
            | 
            slot isMethod ifFalse: [^ vector].
            crs: list copyRemoveAll.
            [nmethodOID isNotNil] assert.
            (objectMapper objectsOracle resendsInNMethodWithOID: nmethodOID) do: [|:resends. :cntxtAndSelfMap. c. m|
              "Need to replace the original selfMap with my selfMap, because if I'm reusing this
               nmethod, the selfMap won't be right."
              m:   cntxtAndSelfMap at: 1.
              c:  (cntxtAndSelfMap at: 0) replaceMap: m With: selfMap.
              resends do: [|:bc|
                (compilationRequesterForResend: bc Context: c) ifNotNil: [|:cr|
                  crs add: cr.
                ].
              ].
            ].
            crs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: profiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compile: selector On: rcvr Times: n = ( |
             slot.
            | 
            "This is just a little helper method that I
             use for profiling. -- Adam, 11/04"
            slot: (reflect: rcvr) at: selector.
            n do: [| compiler. vm |
              compiler: compilerForSlot: slot.
              vm: klein virtualMachines selfVM copy.
              vm lens: vm vmKit memoryLens.
              vm setTheVMAndDo: [
                compiler compileForcingNonLeafIfNecessary buildNMethod.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         compileIfNecessary = ( |
            | 
            slot isMethod ifTrue: [methodSlotsCompiled add: context].
            checkWhetherNMethodWasAlreadyCompiled.
            wasAlreadyCompiled ifFalse: [compileThisMethod].
            objectMapper compilationRequestersThatHaveJustRun add: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: this method\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compileThisMethod = ( |
            | 
            nmethodOID:
              objectMapper nmethodOIDForSlot: slot
                                   LookupKey: lookupKey
                                  Satisfying: [|:nm| canReuseNMethod: nm]
                                 IfAbsentAdd: [couldReuseTheNMethod: false. buildNMethod].

            couldReuseTheNMethod ifNil: [couldReuseTheNMethod: true].

            objectMapper recordingNMethodsTime: objectMapper recordingNMethodsTime + [
              recordNMethodOID: nmethodOID.
            ] cpuTime.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compiledBlocksWithContextAndSelfMap = ( |
            | 
            [nmethodOID isNotNil] assert.
            objectMapper compiledBlocksByNMethodOID at: nmethodOID IfAbsentPut: [list copyRemoveAll]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: profiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilerForSlot: slot = ( |
             context.
             oracle.
            | 
            oracle: klein compiler1 oracleThatCannotDoEagerRelocation.
            context: klein compiler1 prototypes compilationContext copyForSlot: slot.

            klein compiler1
                     copyForContext: context
                       Architecture: 'ppc'
                             Oracle: oracle
                              Debug: true
                           Optimize: klein compiler1 prototypes optimizationPolicies compileFastCode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            resend.copy methodSlotsCompiled: methodSlotsCompiled copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForBlockMirror: bMir ObjectMapper: mapper LexicalParentContext: lpc = ( |
             bMap.
             bMapOID.
             key.
             oracle.
             s.
            | 

            oracle: mapper objectsOracle.
            s: oracle kleinifySlot: (reflect: bMir reflectee originalBlock_replaceThisSlotWithTheValueSlot) valueSlot.
            s holder: bMir. "Holder should be the compiled block instead of the original block."
            key: klein lookupKey copyForNormalSend: s name.
            bMapOID: oracle mapOIDForExemplarMirror: bMir.
            bMap: oracle kleinifiedObjectForOID: bMapOID.

            copyForObjectMapper: mapper Context:
                 theVM compilerPrototype prototypes compilationContext
                           copyForSlot: s
                                   Key: key
                                  Self: lpc selfMap
                              Receiver: bMap
                    LexicalParentScope: bMir reflectee lexicalParentScopeDesc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForObjectMapper: om Context: c = ( |
            | 
            (copy objectMapper: om) context: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForReceiverMap: rMap TargetSlot: s OnBehalfOf: anObjectMapper = ( |
            | 
            copyForObjectMapper: anObjectMapper Context:
               theVM compilerPrototype prototypes compilationContext
                           copyForSlot: (anObjectMapper objectsOracle kleinifySlot: s)
                                   Key: (klein lookupKey copyForNormalSend: s name)
                                  Self: rMap
                              Receiver: rMap
                    LexicalParentScope: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: this method\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         createCompiler = ( |
            | 
            showCompilingSlot.
            theVM compilerPrototype
                        copyForContext: context
                          Architecture: theVM architecture
                                Oracle: objectMapper oracleForEagerRelocation
                                 Debug: theVM exportPolicy shouldCompileInDebugMode
                              Optimize: theVM compilerPrototype prototypes optimizationPolicies compileQuicklyButOptimizeKleinKernel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         isForABlockMethod = ( |
            | 
            context isForABlockMethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         isOuterMethodForABlock = ( |
            | 
            context isOuterMethodForABlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         lookupKey = ( |
            | 
            context lookupKey).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         mapOIDOfReceiver = ( |
            | 
            objectMapper objectsOracle oidForOriginalObject: rcvrMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         mapOIDOfSelf = ( |
            | 
            objectMapper objectsOracle oidForOriginalObject: selfMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mySlot = ( |
            | 
            context slot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethod = ( |
            | 
            objectMapper objectsOracle originalObjectForOID: nmethodOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         nmethodOIDIfPresent: pBlk IfAbsent: aBlk = ( |
            | 
            "Do not recompile methods for receivers that share the same map.
             - Significant optimization allows us to avoid performing
               needless (surprisingly expensive!) lookupKey: operations.
             -- jb 8/03"

            objectMapper
              findNMethodOIDForKey: lookupKey
                         ForMapOID: mapOIDOfReceiver
                         IfPresent: pBlk
                          IfAbsent: aBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         outermostMethodHolder = ( |
            | 
            context outermostMethodHolder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         outermostMethodSlotName = ( |
            | 
            context outermostMethodSlotName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: profiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         profileBuildingIRNodes = ( |
             bcis.
             cs.
             slot.
            | 
            slot: klein maps slotsMap parent asMirror at: 'cloneLocalObject:Size:FillingWith:IfFail:'.
            cs: (vector copySize: 100) copyMappedBy: [compilerForSlot: slot].
            [cs do: [|:c| c makeSureIRNodesAreBuilt]] profileSlice).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: profiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         profileCompilingOneMethod = ( |
            | 
            [
              (vector copyAddLast: ('primReceiver:TheVM:IfFail:' @ klein primitives))
            "  (vector copyAddLast: ('primReceiver:ByteVectorConcatenate:Prototype:IfFail:' @ klein primitives))"
            "  (vector copyAddLast: ('removeAllBuckets:' @ traits hashTableSetOrDictionary))"
            "  (vector copyAddLast: ('canonicalRepresentative' @ assemblerSystems ppc generators instructionTemplates proto))"
            "  ('testSuccess' @ klein virtualMachines selfVM tests smallIntegers parent)"
            "  (vector copyAddLast: ('cloneLocalObject:Size:FillingWith:IfFail:' @ klein maps slotsMap parent))"
                  do: [|:p|
                compile: p x On: p y Times: 100.
              ].
            ] profileSlice).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         protoSlotFinder = ( |
            | 
            objectMapper policy slotFinder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrMap = ( |
            | 
            context rcvrMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: eager relocation\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         reassembleNMethod: nm WithOop: nmOop PredictedOop: predictedOop CodeGenerator: cg = ( |
            | 
            "Since we already have the nmOop, this saves us the cost of having
             to look it up later in the relocation phase. -- Adam, 3/05"

            nmOop = predictedOop ifTrue: [
              objectMapper eagerNMethodOopAssignmentOptimismJustified:
                objectMapper eagerNMethodOopAssignmentOptimismJustified succ.
            ] False: [| rels |
              "This code is never actually called because the optimism
               is always justified (the way the algorithm works now),
               but I've tested it and it does work. -- Adam, 3/05"
              [todo optimize]. "We're iterating over all the relocators when maybe there
                                could just be a separate thingy pointing to the nmethod
                                ones. And, in fact, there sort-of is: there's
                                nmethodRelocators in the compiler. But then we'd have
                                to unflatten the relocationInfo and then find the right
                                relocators in it anyway, so that we could change the
                                master copy, and then flatten it again... or we could
                                figure out a way to just update the flattened
                                relocationInfo directly... Anyway, this seems like a lot
                                of trouble to go to in order to speed up some code that's
                                never reached. -- Adam, 5/05"
              thisCodeIsNeverReached.
              objectMapper eagerNMethodOopAssignmentOptimismNotJustified:
                objectMapper eagerNMethodOopAssignmentOptimismNotJustified succ.
              rels: nm reconstructRelocators.
              rels do: [|:r|
                r isLoadingTheNMethod ifTrue: [
                  r compiledOop: nmOop.
                  cg a atLC: r offset Do: [r reassembleWith: cg NewOop: nmOop].
                  cg copyGeneratedCodeInto: nm.
                ].
              ].
              nm relocators: rels.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: this method\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         recordNMethodOID: nmOID = ( |
            | 
            objectMapper addNMethodOID: nmOID MapOID: mapOIDOfReceiver LookupKey: lookupKey.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         requestAdditionalCompilations = ( |
            | 
            wasAlreadyCompiled ifFalse: [
              compilationRequestersForBlockLiterals do: [|:cr| objectMapper compilationRequestersToRun add: cr].
            ].
            compilationRequestersForResends do: [|:cr| objectMapper compilationRequestersToRun add: cr].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         selfMap = ( |
            | 
            context selfMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: displaying status\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         showCompilingSlot = ( |
            | 
            isForABlockMethod ifTrue: [
              objectMapper showCompilingSlot: slot name
                                     InBlock: rcvrMap
                             OuterMethodSlot: outermostMethodSlotName
                                        Self: selfMap
            ] False: [
              objectMapper showCompilingSlot: slot name
                                   Receiving: rcvrMap
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         slot = ( |
            | 
            mySlot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: this method\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         slotToCompile = ( |
            | 
            objectMapper objectsOracle kleinifySlot: slot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequester' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         wasAlreadyCompiled.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationRequesterForOuterMethodOn: rMap TargetSlot: slot = ( |
            | 
            compilationRequester copyForReceiverMap: rMap
                                         TargetSlot: slot
                                         OnBehalfOf: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationSubtimes = ( |
            | 
            ( (subtime copyNamed: 'Compilation'                         Ms: compilationTime)
            & (subtime copyNamed: 'Filling during compilation'          Ms: fillingDuringCompilationTime)
            & (subtime copyNamed: 'slotsToCompileForReceiver:'          Ms: slotsToCompileForReceiverTime)
            & (subtime copyNamed: 'selectorsToCompile lookup'           Ms: selectorsToCompileLookupTime)
            & (subtime copyNamed: 'reusable nmethod hit'                Ms: reusableNMethodHitTime)
            & (subtime copyNamed: 'hasAlreadyCompiledNMethodForThisMap' Ms: alreadyCompiledLookupTime)
            & (subtime copyNamed: 'nmethod allocation'                  Ms: nmethodAllocationTime)
            & (subtime copyNamed: 'recording nmethods'                  Ms: recordingNMethodsTime)
            & (subtime copyNamed: 'eager nmethod loading relocation'    Ms: eagerNMethodLoadingRelocationTime)) asList).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationSubtimesString = ( |
             s <- ''.
            | 
            compilationSubtimes do: [|:cst| s: s & '     - ' & cst name & ' time: ' & (asSeconds: cst ms) printString & ' s\n'].
            s flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationTimeAccountedFor = ( |
            | 
            (compilationSubtimes copyMappedBy: [|:cst| cst ms]) sum).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         compileAll = ( |
            | 
            "I haven't decided yet whether I like this better than" [old_compileAll].
            "It sure makes the profiles come out nicer, though. -- Adam, June 2009"
            [
             case
              if: [oopsToFillIn                         isEmpty not] Then: [fillInTheNextObject]
              If: [compilationRequestersToRun           isEmpty not] Then: [compilationRequestersToRun           removeFirst compileIfNecessary]
              If: [compilationRequestersThatHaveJustRun isEmpty not] Then: [compilationRequestersThatHaveJustRun removeFirst requestAdditionalCompilations]
              If: [exemplarMirrorsLeftToCompile         isEmpty not] Then: [exemplarMirrorsLeftToCompile         removeFirst requestCompilationWithObjectMapper: self]
              Else: [^ self].

            ] loop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            (((((((((resend.copy
              objectsOracle:                                objectsOracleProto                           copy)
              oopsToFillIn:                                 oopsToFillIn                                 copy)
              exemplarMirrorsLeftToCompile:                 exemplarMirrorsLeftToCompile                 copy)
              compilationRequestersToRun:                   compilationRequestersToRun                   copy)
              compilationRequestersThatHaveJustRun:         compilationRequestersThatHaveJustRun         copy)
              cachedResendsBySlot:                          cachedResendsBySlot                          copy)
              cachedReusableNMethodOIDsBySlotAndKey:        cachedReusableNMethodOIDsBySlotAndKey        copy)
              compiledBlocksByNMethodOID:                   compiledBlocksByNMethodOID                   copy)
              canonicalizedStrings:                         canonicalizedStrings                         copy)
              kleinPrimitiveMirrorOops:                     kleinPrimitiveMirrorOops                     copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForVM: aVM ReportTo: reporter = ( |
            | 
            ((copy
              myVM:                    aVM)
              statusReporter:          reporter)
              initializeCanonicalizedStrings).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         createAndRecordMappedNMethodCache: nmOIDs UsingMap: map MapOop: mapOop = ( |
             nmOIDVector.
             nmcOop.
            | 
            nmOIDs isEmpty ifTrue: [^ prototypeNMethodCacheOop].

            nmOIDVector: nmOIDs asVector.

            shouldOptimizeNMethodCacheCreation ifFalse: [ | nmc |
              nmc:  nmOIDVector mapBy: [|:oid| originalObjectForOID: oid] Into: vmKit nmethodCache copySize: nmOIDVector size.
              assert: [nmc allSatisfy: [|:nm| nm isKleinNMethod]].
              shouldSortNMethodCaches ifTrue: [
                nmc: sortBy: (| element: a Precedes: b = (a lookupKey selector < b lookupKey selector) |).
              ].
              nmcOop: allocateObject: nmc.
              fillInObjects. "pay here to fill in newly-allocated nmc"
            ]
            True: [|nmethodsAndOops. sortedNMethodsAndOops. nmc. nmOops|
              nmethodsAndOops: nmOIDVector copyMappedBy: [|:nmOID| (originalObjectForOID: nmOID) @ (oopForOID: nmOID)].
              shouldSortNMethodCaches ifTrue: [
                nmethodsAndOops sortBy: (| element: a Precedes: b = (a x lookupKey selector < b x lookupKey selector) |).
              ].
              nmc:    nmethodsAndOops        mapBy: [|:nmethodAndOop| nmethodAndOop x] Into: vmKit nmethodCache copySize: nmethodsAndOops size.
              nmOops: nmethodsAndOops copyMappedBy: [|:nmethodAndOop| nmethodAndOop y].
              assert: [nmc allSatisfy: [|:nm| nm isKleinNMethod]].
              fillInObjects. "ensure everything is there"
              nmcOop: allocateAndQuicklyFillInSimpleVector: nmc IndexableOops: nmOops UsingMap: map MapOop: mapOop.
            ].

            nmcOop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         createObjectTable = ( |
             n.
             objectTable.
             oid.
             oldObjectTable.
             oopForObjectTable.
             oopForOldObjectTable.
            | 

            "Could maybe do maybe possible optimization for allocation. -- dave & Adam, 11/05"

            oldObjectTable:     objectsOracle addressesByOID.

            n: desiredStartingSizeForObjectTable: oldObjectTable.
            oopForOldObjectTable: oopForOriginalObject: myVM objectLocator IfAbsent: nil.
                oopForOldObjectTable isNotNil
            &&  [n = (layouts objVector indexableSizeOf: oopForOldObjectTable)]
             ifTrue: [
              oopForObjectTable: oopForOldObjectTable.
              layouts objVector
                                 for:  oopForObjectTable 
                PopulateIndexablesBy:  [|:i. x| x: oldObjectTable at: i.
                                                (reflect: x) kleinAndYodaLayout oopForValue: x].
            ]
            False: [
              myVM objectLocator ifDirect: [] IfIndirect: [
                [todo cleanup oopFormat]. halt.
                "Even though we change the identity of the object locator,
                 we don't do a switchPointers, so compiled code might be wrong.
                 But this is for an interpreted system for now, so we are deferring. -- Adam & David"
              ].

              warning: 'The completed object table is not the same size as the object
                        table back when the export cycle began. Exporting a whole new
                        object table. This means that the old object table is still
                        taking up space in the image.'.
              objectsOracle ensureOIDVectorsHaveSizeAtLeast: n.
              objectTable:  objectsOracle addressesByOID.

              oopForObjectTable: allocateObject: objectTable.
              fillInObjects.

              setContentsOfSlotNamed: 'objectLocator'
                               InOop: (oopForOriginalObject: myVM)
                               ToOop: oopForObjectTable
                              IfFail: raiseError.
            ].
            objectsOracle objectLocatorIndexableOrigin:
              layouts objVector indexableOriginOf: oopForObjectTable.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: canonical strings\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         createStringTable = ( |
            | 
            [myVM universe canonicalizedStrings: canonicalizedStrings]. "browsing"
            setLocalAndRemoteContentsOfAssignableSlotNamed: 'canonicalizedStrings'
                                                        In: myVM universe
                                                        To: canonicalizedStrings
                                                    IfFail: raiseError.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: selectors to compile\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         createTableOfSelectorsToCompile = ( |
             r.
            | 

            "The selectorsToCompile table exists as an optimization, so that we
             can know what selectors might be called inside Klein. We gather
             up all the string literals inside all the methods that we map,
             and then we don't bother compiling any slot whose name isn't in
             the table. -- Adam, 02/05"

            r: set copyRemoveAll.
            r add: myVM entryMethodName.

            [primitiveFailedError: '' Name: '']. "browsing"
            r add: 'primitiveFailedError:Name:'.

            vmKit primitives stubAndFakePrimitiveSelectorsDo: [|:s|
              r add: s.
            ].

            objectsOracle methodMirrorsAllowingDuplicates do: [|:m|
              m literalsDo: [|:lit|
                (reflect: lit) isReflecteeString ifTrue: [
                  r add: lit.
                ].
              ].
            ].

            "HACK: Don't want to compile all the lexicalParentScopeDesc
             slots; there's one for every compiledBlock, and they're only
             ever called when inlining a block method, so we can always
             just compile them just-in-time. -- Adam, June 2009"
            r remove: 'lexicalParentScopeDesc' IfAbsent: ["fine"].

            objectsOracle selectorsToCompile: r.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: object table\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         desiredStartingSizeForObjectTable: t = ( |
            | 
            policy desiredStartingSizeForObjectTable: t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         expectedNumberOfObjects: n = ( |
             conservativeEstimateOfNumberOfObjectsInEdenSpace.
             conservativeEstimateOfNumberOfObjectsInScavSpace.
            | 

            objectsOracle expectedNumberOfObjects: n.

            "Leave some extra room so that we don't have
             to grow it inside the image, for now."

            conservativeEstimateOfNumberOfObjectsInEdenSpace: ((myVM universe            edenSpace sizeOfEntireRegion / oopSize) / layouts block numberOfWordsInABlock succ).
            conservativeEstimateOfNumberOfObjectsInScavSpace: ((myVM universe scavengeGarbageSpace sizeOfEntireRegion / oopSize) / layouts block numberOfWordsInABlock     ).
            objectsOracle growOIDVectorsToAtLeast: n double + conservativeEstimateOfNumberOfObjectsInEdenSpace + conservativeEstimateOfNumberOfObjectsInScavSpace.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: filling in\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         fillInObjectAt: oop OID: oid AndCreateMapOfType: mapProto ForReflecteeOf: m = ( |
             map.
             slots.
             slotsByName.
            | 

            "A bit of duplication with" [kleinifyReflecteeOf: m Mapper: self].

            slots:  mapProto slotsToMapForReflecteeOf: m Mapper: self.
            slotsByName: dictionary copyRemoveAll.
            slots do: [|:s| slotsByName at: s kleinAndYodaSlotName Put: s].
            [slots size = slotsByName size] assert.

            map:  mapProto copySize: slots size.
            initializeMap: map ForReflecteeOf: m Slots: slots.

            allocateMap: map ForOID: oid AndDo: [|:canonicalMap. :canonicalMapOop|
              map myLayout         for: oop
                                   OID: oid
                              UsingMap: canonicalMap
                             AndMapOop: canonicalMapOop
                       AdjustMarkValue: layouts mark defaultMarkValue
              AndFillInHeaderFieldsFor: m
                                Mapper: self.
            ].

            map objectSlotsDo: [|:slotName. :slotOffset. slot. contentsObj. contentsOop|
              slot:         slotsByName at: slotName.
              contentsObj:  policy initialContentsOfSlot: slot.
              contentsOop:  allocateObject: contentsObj.

              map myLayout for: oop At: slotOffset Put: contentsOop.
            ].

            map ifNeededFillInIndexableSlotsFor: m In: oop Mapper: self.

            oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: filling in\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         fillInObjects = ( |
            | 
            [oopsToFillIn isEmpty] whileFalse: [fillInTheNextObject].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: filling in\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         fillInOop: oop = ( |
             mir.
             oid.
            | 
            oid: oidForOop: oop.
            showFillingInObjectWithOID: oid.
            mir: kleinifiedMirrorForOID: oid.

                fillInObjectAt: oop
                           OID: oid
            AndCreateMapOfType: mir vmKitMapForConversion
                ForReflecteeOf: mir.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: filling in\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         fillInTheNextObject = ( |
            | 
            fillInOop: oopsToFillIn removeFirst.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         findNMethodOIDForKey: key ForMapOID: mapOID IfPresent: pb IfAbsent: ab = ( |
            | 
            objectsOracle
              findNMethodOIDForKey: key
                         ForMapOID: mapOID
                         IfPresent: [|:nmOID| nmethodsByMapHits:   nmethodsByMapHits   succ.
                                              pb value: nmOID]
                          IfAbsent: [|:e|     nmethodsByMapMisses: nmethodsByMapMisses succ.
                                              ab value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: klein mirrors\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         fixReflecteeOopsAndMapsInKleinMirrors = ( |
            | 
            kleinPrimitiveMirrorOops do: [|:kleinPrimMirOop. reflecteeOop. reflecteeMapOop. kleinPrimMirMap. kleinPrimMir|
              kleinPrimMir:  kleinifiedObjectForOop: kleinPrimMirOop.
              reflecteeOop:  oopForOID: kleinPrimMir reflecteeOID.

              kleinPrimMir cachedReflecteeOop: reflecteeOop.
              kleinPrimMir isCachedReflecteeOopValid: true.

              reflecteeMapOop: mapOf: reflecteeOop IfFail: raiseError.
              kleinPrimMirMap: maps map importMapFor:  kleinPrimMirOop IfFail: raiseError.

              "The reflecteeOop in a mirror on a remote object is an integer
               (the oop of that object). But the reflecteeOop in a mirror in
               a running Klein process should be a reference to the reflectee
               object. So here we change the integer into a reference."
              [kleinAndYoda reflectionPrimitives cachedReflecteeOop: reflecteeOop]. "browsing"
              kleinPrimMirMap setContentsOfSlotNamed: 'cachedReflecteeOop'
                                                  In: kleinPrimMirOop
                                                  To: reflecteeOop
                                              IfFail: raiseError.
              [kleinAndYoda reflectionPrimitives isCachedReflecteeOopValid: true]. "browsing"
              kleinPrimMirMap setContentsOfSlotNamed: 'isCachedReflecteeOopValid'
                                                  In: kleinPrimMirOop
                                                  To: (oopForOriginalObject: true)
                                              IfFail: raiseError.

              "We may not have been able to correctly initialize the 'map'
               slot in the mirror when we first created the mirror, because
               the map of the reflectee may not have been filled in at that
               time. So we fix it here."
              kleinPrimMir setMapIfFail: raiseError.
              [kleinPrimMir map: reflecteeMap]. "browsing"
              kleinPrimMirMap setContentsOfSlotNamed: 'map'
                                                  In: kleinPrimMirOop
                                                  To: reflecteeMapOop
                                              IfFail: raiseError.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         gatherAndPrintStatisticsDuring: blk = ( |
            | 
            printStatistics: blk allTimes.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: tracking oops and objects by OID\x7fComment: This slot is here so that we can share trackingObjectsInMyOracle
with vmImage. -- Adam, 7/05\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         heapOffset = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: immediates\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         immediateExemplarsDo: blk = ( |
            | 
            blk value: 0.
            blk value: 0.0.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         incorporateCodeGenerationStatisticsFrom: cg = ( |
            | 
            objectsOracle incorporateCodeGenerationStatisticsFrom: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         incrementCompileCount = ( |
            | compileCount: compileCount succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeCanonicalizedStrings = ( |
            | 
            canonicalizedStrings:  customizableSet copyRemoveAll
                                      comparisonTraits: vm stringComparisonMixin.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeExemplarMirrorsLeftToCompile = ( |
            | 
            "Optimization: compile stub nmethods early on so that eager relocation
             will work for as many stub relocators as possible. -- Adam, 3/05"
            vm exportPolicy shouldIncludeKleinPrimitives ifTrue: [| stubHolderMirs |
              stubHolderMirs: set copyRemoveAll.
              vm vmKit primitives stubAndFakePrimitiveMethodHoldersDo: [|:h| stubHolderMirs add: reflect: h].
              exemplarMirrorsLeftToCompile filterBy: [|:mir| (stubHolderMirs includes: mir) not].
              stubHolderMirs do: [|:mir| exemplarMirrorsLeftToCompile addFirst: mir].
            ].

            immediateExemplarsDo: [|:imm| exemplarMirrorsLeftToCompile add: reflect: imm].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: transmogrifying bytecodes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeMap: map ForReflecteeOf: m Slots: slots = ( |
            | 
            map initializeForReflecteeOf: m Slots: slots Mapper: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: incremental update\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         isIncremental = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlotToBeMapped: s = ( |
            | 
            policy isSlotToBeMapped: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         mapNMethodCaches = ( |
             nmcMap.
             nmcMapOop.
            | 
            statusReporter show: 'Mapping NMethod caches'.
            isInNMethodCacheConstruction: true.
            nmcMapOop: mapOf: prototypeNMethodCacheOop.
            nmcMap: originalObjectForOop: nmcMapOop.

            objectsOracle nmethodOIDDictionariesByMapOopDo: [|:nmOIDs. :mapOop. nmcOop|
              nmcOop: createAndRecordMappedNMethodCache: nmOIDs UsingMap: nmcMap MapOop: nmcMapOop.
              setNMethodCacheOfMappedMap: mapOop To: nmcOop.
            ].

            isInNMethodCacheConstruction: false.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         mapObjects = ( |
             sp.
            | 
            "Get starting points first before withLensForMappingDo:
             changes theVM -- dmu 10/04"

            sp: startingPoints.

            _NakedMethods: true.

            objectsOracle ensureAddressesByOIDIsUpdated.

            allocateImmediateMaps.
            allocateStartingPoints: sp.

            fillInObjects. "do this now to ensure that all blocks have maps"

            createTableOfSelectorsToCompile.

            initializeExemplarMirrorsLeftToCompile.

            compileAll.
            fillInObjects.

            mapNMethodCaches.
            fillInObjects.

            createStringTable.
            fillInObjects. "just to be sure"

            fixReflecteeOopsAndMapsInKleinMirrors.
            [createNMethodTable]. [todo recompilation]. "Just for measuring -- breaks incremental update"

            [todo variableSizeHeaders. fixupCompactMapTable].

            createObjectTable.
            fillInObjects. "just to be sure"

            assert: [
              verifyOracles.
              true
            ].

            objectsOracle).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mapObjectsKeepingStatistics = ( |
            | 
            gatherAndPrintStatisticsDuring: [
              mapObjects
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: showing status\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         nameOfMap: rMap = ( |
            | 
            nameOfMirror: reflect: objectsOracle exemplarForMap: rMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: showing status\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         nameOfMirror: rMir = ( |
            | 
            "Usually the method is actually defined on the prototypical
             object, so pretend that we are actually compiling it for
             that one to make the output cleaner.  -- jb 8/03"
            rMir prototypeIfPresent: [|:myProto|
              myProto creatorNameIfPresent: [|:n| ^ n] IfAbsent: [].
            ] IfAbsent: [].
            rMir safeName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         nextOopToAllocateForObjectOfSize: nOops = ( |
            | 
            objectsOracle nextOopToAllocateForObjectOfSize: nOops).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         nmethodOIDForSlot: aSlot LookupKey: key Satisfying: conditionBlk IfAbsentAdd: absentBlk = ( |
             isAbsent <- bootstrap stub -> 'globals' -> 'false' -> ().
             r.
             t.
            | 
            t: [| nmOIDs |
              nmOIDs: cachedReusableNMethodOIDsBySlotAndKey at: aSlot @ key IfAbsentPut: [list copyRemoveAll].
              nmOIDs findFirst: [|:nmOID| conditionBlk value: originalObjectForOID: nmOID]
                     IfPresent: [|:nmOID| r: nmOID]
                      IfAbsent: [isAbsent: true. r: absentBlk value. nmOIDs add: r].
            ] cpuTime.
            isAbsent ifTrue: [
              reusableNMethodMisses:  reusableNMethodMisses succ.
            ] False: [
              reusableNMethodHits:    reusableNMethodHits   succ.
              reusableNMethodHitTime: reusableNMethodHitTime + t.
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         objectsOracleProto = ( |
            | 
            theVM vmKit objectsOracle).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         old_compileAll = ( |
            | 
            [exemplarMirrorsLeftToCompile isEmpty] whileFalse: [
              exemplarMirrorsLeftToCompile removeFirst requestCompilationWithObjectMapper: self.
              runCompilationRequesters.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: eager relocation\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oracleForEagerRelocation = ( |
            | 
            objectsOracle).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         policy = ( |
            | 
            myVM exportPolicy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         printStatistics: mappingTimes = ( |
             longs.
             shorts.
            | 
            longs: (
              '\n',
              'Mapping Statistics\n',
              ' - Real   time   : ', (asSeconds: mappingTimes realTime) printString, ' s\n',
              ' - CPU    time   : ', (asSeconds: mappingTimes  cpuTime) printString, ' s\n',
              '   - Fill 1 time: ', (asSeconds: fill1Time) printString, ' s\n',
              '   - SelectorsToCompile creation time: ', (asSeconds: selectorsToCompileCreationTime) printString, ' s\n',
              '   - Compilation Phase  time: '         , (asSeconds:           compilationPhaseTime) printString, ' s\n',
                      compilationSubtimesString,
              '     - unaccounted-for compilation time: ', (asSeconds: compilationPhaseTime - compilationTimeAccountedFor) printString, ' s\n',
              '   - Fill 2 time: '               , (asSeconds:                    fill2Time) printString, ' s\n',
              '   - NMethod cache time: '        , (asSeconds: nmethodCacheConstructionTime) printString, ' s\n',
              '   - Fill 3 time: '               , (asSeconds:                    fill3Time) printString, ' s\n',
              '   - String table time: '         , (asSeconds:              stringTableTime) printString, ' s\n',
              '   - Fill 4 time: '               , (asSeconds:                    fill4Time) printString, ' s\n',
              '   - Fixing Klein mirrors time: ' , (asSeconds:       fixingKleinMirrorsTime) printString, ' s\n',
              '   - Compact map table time: '    , (asSeconds:          compactMapTableTime) printString, ' s\n',
              '   - Object table time: '         , (asSeconds:              objectTableTime) printString, ' s\n',
              '   - Fill 5 time: '               , (asSeconds:                    fill5Time) printString, ' s\n',
              ' - User   time   : ', (asSeconds: mappingTimes   userTime) printString, ' s\n',
              ' - System time   : ', (asSeconds: mappingTimes systemTime) printString, ' s\n',
              ' - Objects mapped : ', objectsOracle numberOfObjectsMapped printString,   '\n', 
              '   - Maps         : ', objectsOracle    numberOfMapsMapped printString,   '\n',
              '   - NMethods     : ', objectsOracle          nmethodCount printString,   '\n',
              '   - Compilations : ',                        compileCount printString,   '\n',
              '   - Relocators eager/non-eager: ', objectsOracle relocatorDoesNotNeedToDoPlaceholderInstructions printString, '/',
                                                   objectsOracle relocatorNeedsToDoPlaceholderInstructions       printString, '\n',
              '   - Eager nmethod optimism justified/unjustified: ', eagerNMethodOopAssignmentOptimismJustified    printString, '/',
                                                                     eagerNMethodOopAssignmentOptimismNotJustified printString, '\n',
              '   - SelectorsToCompile hits/misses: ', selectorsToCompileHits printString, '/', 
                                                      (selectorsToCompileProbes - selectorsToCompileHits) printString, '\n',
              '   - BlockNMethod$ hits/misses: ', blockNMethodHits printString, '/', blockNMethodMisses printString, '\n',
              '   - ReusableNMethod hits/misses: ', reusableNMethodHits printString, '/', reusableNMethodMisses printString, '\n',
              '   - NMethodsByMap hits/misses: ', nmethodsByMapHits printString, '/', nmethodsByMapMisses printString
            ).
            longs printLine.
            shorts: (
              'Mapped ', objectsOracle numberOfObjectsMapped printString, ' objects, ',
              'compiled ', objectsOracle nmethodCount printString, ', ',
              ' in ',  mappingTimes cpuTime printString, ' ms.'
            ).
            statusReporter show: shorts.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: filling in\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         quicklyFillInSimpleVector: v Oop: vOop UsingMap: map MapOop: mapOop IndexableOops: vIndexableOops = ( |
             nextOopToFillIn.
            | 
            "optimized special-case version of" [fillInTheNextObject]. "-- dmu 2/05"
            nextOopToFillIn: oopsToFillIn removeFirst.
            [vOop = nextOopToFillIn] assert.

            showFillingInObjectWithOID: oidForOop: vOop.

            maps objVectorMap fillInSimpleVector: v Oop: vOop IndexableOops: vIndexableOops Map: map MapOop: mapOop Mapper: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: klein mirrors\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         recordKleinMirrorOop: oop = ( |
            | 
            kleinPrimitiveMirrorOops add: oop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         recordSlot: s = ( |
            | 
            objectsOracle recordSlot: s.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: canonical strings\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         recordString: s = ( |
            | 
            canonicalizedStrings add: s.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: filling in\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         rememberToFillInOop: oop = ( |
            | 
            oopsToFillIn addLast: oop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         requestAdditionalCompilations = ( |
            | 
            [compilationRequestersThatHaveJustRun isEmpty] whileFalse: [
              fillInObjects. "To make sure that any compiledBlock objects have maps."
              compilationRequestersThatHaveJustRun removeFirst requestAdditionalCompilations.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         requestCompilationForReceiverMirror: rMir Map: rMap MapOID: rMapOID = ( |
            | 
            (slotsToCompileForReceiver: kleinifiedMirrorForOriginalMirror: rMir) do: [|:slot|
              (shouldCompileSlot: slot) ifTrue: [
                compilationRequestersToRun add: compilationRequesterForOuterMethodOn: rMap TargetSlot: slot.
              ] False: [
                objectsOracle recordMapOID: rMapOID WithUncompiledSlotNamed: slot name.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: resends\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         resendDetector = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'resendDetector' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals abstractBytecodeInterpreter copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'resendDetector' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent resendDetector.

CopyDowns:
globals abstractBytecodeInterpreter. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: resends\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         resendsInSlot: aSlot = ( |
            | 
            resendDetector resendsInSlot: aSlot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         runCompilationRequesters = ( |
            | 
            [compilationRequestersToRun isEmpty] whileFalse: [
              compilationRequestersToRun removeFirst compileIfNecessary.
              requestAdditionalCompilations.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: adding extra objects after the main mapping phase\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         setContentsOfSlotNamed: slotName InOop: targetOop ToOop: newValueOop IfFail: fb = ( |
             targetMap.
            | 
            targetMap:  maps map importMapFor: targetOop IfFail: [|:e| ^ fb value: e].
            targetMap setContentsOfSlotNamed: slotName
                                          In: targetOop
                                          To: newValueOop
                                      IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: adding extra objects after the main mapping phase\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         setLocalAndRemoteContentsOfAssignableSlotNamed: slotName In: target To: newValue IfFail: fb = ( |
             newValueOop.
             targetOop.
            | 

            fillInObjects. "ensure target is filled in"

            targetOop:  objectsOracle oopForOriginalObject: target IfAbsent: [^ fb value: 'cannot find oop for target'].

            newValueOop:  allocateObject: newValue.
            fillInObjects. "fill in newValue in case it has not been mapped yet"

            (slotName, ':') sendTo: target With: newValue.
            setContentsOfSlotNamed: slotName
                             InOop: targetOop
                             ToOop: newValueOop
                            IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         setNMethodCacheOfMappedMap: mapOop To: nmcOop = ( |
            | 
            fillInObjects. "be sure map is filled in"
            maps mapMap for: mapOop SetNMethodCache: nmcOop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: nmethod invocation counts\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         setNMethodTableForUniverse: u To: nmethods = ( |
            | 
            [u allNMethods: nmethods]. "browsing"
            setLocalAndRemoteContentsOfAssignableSlotNamed: 'allNMethods'
                                                        In: u
                                                        To: nmethods
                                                    IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: incremental update\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldAlwaysReassembleRelocatorsForObject: o = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldCompileSlot: s = ( |
             b.
             n.
            | 
            "Optimization: Don't bother compiling any slot whose selector isn't
             called from any method inside Klein. -- Adam, 2/05"
            policy shouldOnlyCompileSelectorsCalledByMappedMethods ifFalse: [^ true].
            n: s name.

            [aaaaaaa].
            "HACK: Don't want to compile most slots named 'parent'.
             The only problem is the 'slots parent' slot. So for now
             I'm gonna hard-code that one. -- Adam, June 2009"
            (n = 'parent') && [s holder != (reflect: slots)] ifTrue: [^ false].

            selectorsToCompileProbes: selectorsToCompileProbes succ.
            b:  objectsOracle selectorsToCompile includes: n.
            b ifTrue: [selectorsToCompileHits: selectorsToCompileHits succ].
            b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldOptimizeNMethodCacheCreation = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: nmethods\x7fComment: The sorting isn\'t necessary, but it\'s sure more convenient for debugging if the
nmethod cache is in some sort of predictable order. So far the performance hit on
the mapping/compiling process has been negligible. -- Adam & Alex, 2/24/04

It seems like sorting the nmethod caches is taking up about 10% of incremental
update time, at least for some updates. Let\'s try turning it off. -- Adam, 10/05\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldSortNMethodCaches = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: showing status\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         show: aString = ( |
            | 
            aString printLine.
            statusReporter ifNotNil: [statusReporter show: aString].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: showing status\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         show: aStringOrBlock Nth: n = ( |
            | 
            "Optimization: allow the parameter to be a block that
             returns a string, because sometimes the string is
             expensive to construct, and we don't use it unless
             we're at a multiple of the showingFrequency.
             -- Adam & Alex, 6/04"
            (n % showingFrequency) = 0  ifFalse: [^ self].

            show: '(', n printString, ') ', aStringOrBlock value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: showing status\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         showAllocatedObjectFor: aMir = ( |
            | 
            show: [
             'Allocating: ', (
               aMir
                  creatorNameIfPresent: [|:n| n]
                              IfAbsent: '...')
            ] Nth: showObjectCount.
            showObjectCount: showObjectCount succ.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: showing status\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         showCompilingSlot: bSlotName InBlock: bMap OuterMethodSlot: outerMethodSlotName Self: selfMap = ( |
            | 
            show: ['Compiling: ', (nameOfMap: selfMap), ' ', outerMethodSlotName, ', [] ', bSlotName]
             Nth: compileCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: showing status\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         showCompilingSlot: rSlotName Receiving: rMap = ( |
            | 
            show: ['Compiling: ', (nameOfMap: rMap), ' ', rSlotName] Nth: compileCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: showing status\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         showFillingInObjectWithOID: oid = ( |
            | 
            show: [
             'Filling in: ', (
               (originalMirrorForOID: oid)
                  creatorNameIfPresent: [|:n| n]
                              IfAbsent: '...')
            ] Nth: oid.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: showing status\x7fComment: set to unity to see all well-known compiles, mappings, etc.\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (500)\x7fVisibility: private'
        
         showingFrequency = 500.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         slotsToCompileForReceiver: rMir = ( |
             slots.
            | 

            "Self does not have method objects, should not need any methods on methods -- dmu 2/05"
            rMir isReflecteeMethod ifTrue: [^ vector].

            "Call slotsToCompileForReceiver: instead of slotsToCompileForReceiver:Do: just so that
             the profile will turn out cleaner. -- Adam, 2/05"
            slotsToCompileForReceiverTime: slotsToCompileForReceiverTime + [
              slots:  policy slotsToCompileForReceiver: rMir.
            ] cpuTime.

            slots).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         startingPoints = ( |
            | policy startingPoints).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         subtime = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'subtime' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent subtime.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'subtime' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyNamed: n Ms: t = ( |
            | 
            (copy name: n) ms: t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'subtime' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         ms <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'subtime' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (\'\')\x7fVisibility: public'
        
         name <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'subtime' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         targetSpace = ( |
            | 
            objectsOracle targetSpace).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         totalWordCountForReflecteeOf: aMir = ( |
            | 
            aMir vmKitMapForConversion 
               totalWordCountForReflecteeOf: aMir
                                     Mapper: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         trackingObjectsInMyOracle* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent trackingObjectsInMyOracle.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: oop offset\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         addOffsetToOop: oop = ( |
            | 
            theVM objectLocator addOffset: heapOffset ToOop: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         blockCloneCounts = ( |
            | 
            myVM setTheVMAndDo: [| r |
              r: list copyRemoveAll.
              exemplarOIDsDo: [|:exemplarOID. :origMap|
                origMap isBlock ifTrue: [| blk. mapOop |
                  blk: originalObjectForOID: exemplarOID.
                  "Just want the compiledBlocks, not the ordinary Self blocks."
                  (reflect: blk) isReflecteeBlock ifFalse: [
                    mapOop: oopForOriginalObject: origMap.
                    [origMap cloneCount]. "browsing"
                    r add: blk @ ((myVM mirrorFor: mapOop) primitiveContentsAt: 'cloneCount') reflectee.
                  ].
                ].
              ].
              (r copySortBy: (| element: a Precedes: b = (a y > b y) |)) asVector
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         blockCloneCountsByOuterMethodSlot = ( |
            | 
            blockCloneCounts copyMappedBy: [|:p. s|
              s: p x lexicalParentScopeDesc outermostLexicalParentScope slot.
              s name @ s holder safeName @ p y
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         breakDownObjectsBy: blk = ( |
             r.
            | 
            r: dictionary copyRemoveAll.
            myVM setTheVMAndDo: [
              objectsOracle ensureAddressesByOIDIsUpdated.
              oopsAndAddressesAndOriginalMirrorsAndKleinifiedMirrorsAndOIDsDo: [|:oop. :addr. :mir. :kMir. :oid. size. t. ts. os|
                t: blk value: oop With: mir With: kMir With: oid.
                size: objectSize copyWords: mir vmKitMapForConversion myLayout wordSizeOf: oop.
                ts: r at: t IfAbsentPut: [typeStat copyForType: t Image: self].
                ts objectStats add: objectStat copyForOop: oop OriginalMirror: mir KleinifiedMirror: kMir Size: size.
              ].
            ].
            [typeStat size]. "browsing"
            (r asVector copySortBySelector: 'size') reverse).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         breakDownObjectsByMap = ( |
            | 
            breakDownObjectsBy: [|:oop| vmKit layouts object mapOf: oop]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         breakDownObjectsBySelfPrototype = ( |
            | 
            breakDownObjectsBy: [|:oop. :mir. :kMir| selfPrototypeOf: kMir]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         codeSizeStatistics = ( |
            | 
            objectsOracle codeSizeStatistics).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         exemplarOIDsDo: blk = ( |
            | 
            objectsOracle exemplarOIDsDo: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         invocationCounts = ( |
            | 
            myVM setTheVMAndDo: [| r |
              r: list copyRemoveAll.
              nmethodsDo: [|:nm. nmOop. map|
                nmOop: oopForOriginalObject: nm.
                map:  vmKit maps nmethodMap importMapFor: nmOop IfFail: raiseError.
                [nm invocationCount]. "browsing"
                r add: nm @ (vmKit layouts smi decode: map contentsOfSlotNamed: 'invocationCount' In: nmOop IfFail: raiseError).
              ].
              (r copySortBy: (| element: a Precedes: b = (a y > b y) |)) asVector
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: tracking objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedMirrorForOID: oid IfAbsent: ab = ( |
            | 
            objectsOracle kleinifiedMirrorForOID: oid IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: tracking objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOIDForExemplarOID: exemplarOID = ( |
            | 
            objectsOracle mapOIDForExemplarOID: exemplarOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: tracking objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         methodMirrorsAllowingDuplicates = ( |
            | 
            objectsOracle methodMirrorsAllowingDuplicates).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         moduleSizes = ( |
             d.
             r.
            | 
            d: dictionary copyRemoveAll.
            wellKnownSlotsDo: [|:s| (d at: s module IfAbsentPut: [list copyRemoveAll]) add: s].
            d: d copyMappedBy: [|:ss| (ss copyMappedBy: [|:s| myVM exportPolicy estimatedSizeOfSlot: s AssumingOopSizeIs: 2]) sum].
            r: list copyRemoveAll.
            d do: [|:size. :mName|
              r add: size @ mName.
            ].
            (r asVector copySortBySelector: 'x') reverse).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodsDo: blk = ( |
            | 
            objectsOracle nmethodsDo: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: tracking objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOop: oop IfAbsent: ab = ( |
            | 
            objectsOracle oidForOop: (subtractOffsetFromOop: oop) IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: tracking objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOriginalObject: obj IfAbsent: ab = ( |
            | 
            objectsOracle oidForOriginalObject: obj IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: tracking objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid IfAbsent: ab = ( |
            | 
            addOffsetToOop: (objectsOracle oopForOID: oid IfAbsent: [^ ab value])).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oopsAndAddressesAndOriginalMirrorsAndKleinifiedMirrorsAndOIDsDo: blk = ( |
            | 
            objectsOracle oopsAndAddressesAndOriginalMirrorsAndKleinifiedMirrorsAndOIDsDo: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: tracking objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         originalMirrorForOID: oid IfAbsent: ab = ( |
            | 
            objectsOracle originalMirrorForOID: oid IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         trackingObjects* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectsOracle parent trackingObjects.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         printModuleSizes = ( |
             r.
             totalSize <- 0.
            | 
            r: list copyRemoveAll.
            moduleSizes do: [|:size. :mName|
              totalSize: totalSize + size.
              r add: size @ mName.
            ].
            (totalSize printString, ' in total') printLine.
            (r copySortBySelector: 'x') reverseDo: [|:p| ((p x printString padOnLeft: totalSize printString size), ': ', p y) printLine].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         statisticsMethodNames = ( |
            | 
            ( [breakDownObjectsByMap]
            & [breakDownObjectsBySelfPrototype]
            & [moduleSizes]
            & [breakDownNMethodsByHolder]
            & [breakDownNMethodsBySlotType]
            & [breakDownRelocatorsByType]
            & [sortedCodeSizeStats]
            & [unsentSlotsByHolder]
            ) asVector copyMappedBy: [|:b| b asMirror methodSource]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: oop offset\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         subtractOffsetFromOop: oop = ( |
            | 
            theVM objectLocator addOffset: heapOffset negate ToOop: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         uninvokedNMethods = ( |
            | 
            (invocationCounts copyFilteredBy: [|:p| p y = 0]) copyMappedBy: [|:p| p x]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         uninvokedNMethodsByHolder = ( |
             d.
            | 
            d: dictionary copyRemoveAll.
            uninvokedNMethods do: [|:nm| (d at: (reflect: nm methodHolder) IfAbsentPut: [list copyRemoveAll]) add: nm].
            d).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         unsentSlots = ( |
             r.
             sels.
            | 
            r: list copyRemoveAll.
            sels: selectorsSent asSet.
            wellKnownSlotsDo: [|:s| (sels includes: s name) ifFalse: [r add: s]].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         unsentSlotsByHolder = ( |
             d.
             r.
            | 
            d: dictionary copyRemoveAll.
            unsentSlots do: [|:s|
              (d at: s holder IfAbsentPut: [list copyRemoveAll]) add: s.
            ].

            r: list copyRemoveAll.
            d do: [|:ss. :m| r add: (ss copyMappedBy: [|:s| s name]) asVector copySort @ m].
            r asVector copySortBy: (| element: a Precedes: b = (a x size > b x size) |)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         wellKnownSlotsDo: aBlock = ( |
            | 
            myVM setTheVMAndDo: [
              theVM exportPolicy invalidateCachedModuleNameLists.
              theVM exportPolicy modulesToMap allNamesOfIncludedModules do: [|:mName. m|
                mName isEmpty ifFalse: [
                  m: mName sendTo: modules.
                  m slots do: [|:s|
                    [|:exit. kleinMir. oop|
                      oop:  oopForOriginalMirror: s holder IfAbsent: exit.
                      kleinMir: theVM mirrorFor: oop.
                      (kleinMir includesKey: s name) ifTrue: [
                        aBlock value: s.
                      ].
                    ] exit.
                  ].
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyContentsOfSimpleVector: v = ( |
             vOop.
            | 
            vOop: oopForOriginalObject: v.
            myVM assert: [(layouts objVector indexableSizeOf: vOop) = v size].
            v do: [|:x. :i|
              myVM assert: [(oopForOriginalObject: x) = (layouts objVector for: vOop IndexableAt: i)].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyOracles = ( |
            | 
            show: 'Verifying oracles'.
            objectsOracle verify.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyThatItIsOKForSlot: s ToHaveOffset: offset = ( |
            | 
            policy ifSlot: s HasExpectedOffsetThen: [|:o|
              o = offset ifFalse: [error: 'expected slot ', s name, ' to have offset ', o printString, ', but it was ', offset printString].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         vm = ( |
            | 
            myVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            myVM vmKit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: transient state\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         prototypeNMethodCacheOop.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         recordingNMethodsTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         reusableNMethodHitTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: cache stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         reusableNMethodHits <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: cache stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         reusableNMethodMisses <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         selectorsToCompileCreationTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: selectorsToCompile\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         selectorsToCompileHits <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         selectorsToCompileLookupTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: selectorsToCompile\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         selectorsToCompileProbes <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         showObjectCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         slotsToCompileForReceiverTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         statusReporter.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         stringTableTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: building VMs\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         objectsOracle = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectsOracle.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Category: optimizations\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedNMethodOIDs.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Category: statistics\x7fCategory: code size\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         codeSizeStatistics <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Comment: keys: all (unmapped) maps
values: OID of an object whose map is the key\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         exemplarOIDsByCanonicalMap <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Comment: keys: the exemplars of each kind of immediate (i.e. just 0 and 0.0, unless
      we decide to change what kinds of things are immediates in Klein)
values: the OID of the map for that kind of immediate\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (reflectiveIdentityDictionary copyRemoveAll)\x7fVisibility: private'
        
         immediateMapOIDsByExemplar <- reflectiveIdentityDictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Comment: Some low-level objects (like mirrors) need to be transformed into
other kinds of objects when they\'re mapped over to Klein. For example,
a Self mirror will not function properly inside Klein, so when we map
a Self mirror, we turn it into a Klein mirror on the appropriate
object.

keys: OIDs of all Self objects that are mapped to Klein
values: the kleinified version of that Self object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         kleinifiedMirrorsByOID <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Category: optimizations\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         mapOIDsContainingUncompiledSlots <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Category: statistics\x7fCategory: slots\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         mappedSlots <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Comment: keys: OIDs of all maps
values: a dictionary mapping lookupKeys to the OIDs
        of the nmethods compiled for that map\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         nmethodOIDsByMapOID <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (-1)\x7fVisibility: public'
        
         objectLocatorIndexableOrigin <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         oidsByOop <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Comment: keys: all Self objects that are mapped to Klein
values: the OID assigned to that Self object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (reflectiveIdentityDictionary copyRemoveAll asUniversal)\x7fVisibility: private'
        
         oidsByOriginalObject <- reflectiveIdentityDictionary copyRemoveAll asUniversal.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Comment: keys: OIDs of all self objects that are mapped to Klein
values: the object with that OID\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         originalMirrorsByOID <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectsOracle parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         activationMaps = ( |
             r.
            | 
            r: list copyRemoveAll.
            kleinifiedMirrorsByOID do: [|:kMir|
              kMir ifNotNil: [
                kMir isReflecteeVMKitActivationMap ifTrue: [
                  r add: kMir reflectee.
                ].
              ].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: merging with other oracles\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         addAllNMethodsFrom: otherOracle = ( |
            | 
            otherOracle newNMethodOIDDictionariesByMapOIDDo: [|:nmOIDs. :mapOID|
              nmethodOIDDictionaryForMapOID: mapOID
                                  IfPresent: [|:d| d addAll: nmOIDs]
                                IfAbsentPut: [nmOIDs copy].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         addNMethodOID: nmOID MapOID: mapOID LookupKey: key = ( |
             nmOIDs.
            | 
            [(originalObjectForOID: nmOID) isKleinNMethod] assert.
            nmOIDs: nmethodOIDDictionaryForMapOID: mapOID
                                        IfPresent: [|:d| d]
                                      IfAbsentPut: [dictionary copyRemoveAll].
            nmOIDs at: key Put: nmOID.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         addressesByOID = ( |
            | theVM objectLocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         addressesByOID: v = ( |
            | 
            theVM objectLocator: v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOops: nOops = ( |
            | 
            targetSpace allocateOops: nOops).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOops: nOops AndBytes: nBytes = ( |
            | 
            targetSpace allocateOops: nOops AndBytes: nBytes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: blk = ( |
            | 
            theVM assert: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: a Is: b = ( |
            | 
            a = b ifFalse: [error: 'inconsistent'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         blockMaps = ( |
            | 
            mapsOfMemoryObjects copyFilteredBy: [|:m| m isBlock]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         blockMirrors = ( |
            | 
            originalMirrors copyFilteredBy: [|:m| m isReflecteeBlock]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         byteVectorMirrors = ( |
            | 
            originalMirrors copyFilteredBy: [|:m| m isReflecteeByteVector]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: maps and exemplars\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         canonicalMapFor: aMap IfPresentDo: pb IfAbsentDo: ab = ( |
            | 
            aMap isImmediate ifTrue: [error: 'should not be called for immediate maps'].

            exemplarOIDsByCanonicalMap
                       if: aMap
              IsPresentDo: [|:exemplarOID. :canonicalMap| pb value: canonicalMap ]
               IfAbsentDo: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: maps and exemplars\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         canonicalMapFor: aMap IfPresentDo: pb IfAbsentRecordExemplarOID: oid AndDo: ab = ( |
            | 
            aMap isImmediate ifTrue: [error: 'should not be called for immediate maps'].

            exemplarOIDsByCanonicalMap
              if: aMap
              IsPresentDo: [|:exemplarOID. :canonicalMap| pb value: canonicalMap ]
              IfAbsentPut: [ oid ]
              AndDo:       ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: maps and exemplars\x7fCategory: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         canonicalMapsDo: blk = ( |
            | 
            immediateMapOIDsByExemplar do: [|:immMapOID| blk value: originalObjectForOID: immMapOID].
            exemplarOIDsDo: [|:eOID. :m| blk value: m].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: incremental update\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         computeAncestorsOfMap: m Into: aDict = ( |
            | 
            aDict
              if: m
              IsPresentDo: [|:ancestors| ancestors]
              IfAbsentPut: [identitySet copyRemoveAll add: m]
              AndDo: [|:ancestors|
                m constantParentsDo: [|:pName. :pObj. pMir. pMapOop. pMap|
                  pMir: reflect: pObj.
                  pMap: pMir isReflecteeKleinAndYodaImmediate
                               ifTrue: [pMir vmKitMapForConversion]
                                False: [originalObjectForOop: kleinAndYoda layouts object mapOf: oopForOriginalObject: pObj].
                  ancestors addAll: computeAncestorsOfMap: pMap Into: aDict.
                ].
                ancestors
              ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: incremental update\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         computeAncestorsOfMaps = ( |
             r.
            | 
            r: identityDictionary copyRemoveAll.
            r desiredMinCapacity: numberOfMapsMapped.
            canonicalMapsDo: [|:m| computeAncestorsOfMap: m Into: r].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            ((((((((((((resend.copy
             oidsByOriginalObject:             oidsByOriginalObject             copy)
             oidsByOop:                        oidsByOop                        copy)
             exemplarOIDsByCanonicalMap:       exemplarOIDsByCanonicalMap       copy)
             immediateMapOIDsByExemplar:       immediateMapOIDsByExemplar       copy)
             originalMirrorsByOID:             originalMirrorsByOID             copy)
             kleinifiedMirrorsByOID:           kleinifiedMirrorsByOID           copy)
             selectorsToCompile:               selectorsToCompile               copy)
             mappedSlots:                      mappedSlots                      copy)
             codeSizeStatistics:               codeSizeStatistics               copy)
             resendBytecodesByNMethodOID:      resendBytecodesByNMethodOID      copy)
             reusabilityConditionsByNMethod:   reusabilityConditionsByNMethod   copy)
             nmethodOIDsByMapOID:              nmethodOIDsByMapOID              copyMappedBy: [|:nmOIDs|   nmOIDs copy])
             mapOIDsContainingUncompiledSlots: mapOIDsContainingUncompiledSlots copyMappedBy: [|:mapOIDs| mapOIDs copy]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: merging with other oracles\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyInfoFrom: otherOracle = ( |
            | 
            otherOracle mapOIDsContainingUncompiledSlots do: [|:mapOIDs. :s|
              (mapOIDsContainingUncompiledSlots at: s IfAbsentPut: [set copyRemoveAll]) addAll: mapOIDs.
            ].

            otherOracle newSelectorsToCompile do: [|:s|
              mapOIDsContainingUncompiledSlots removeKey: s IfAbsent: [].
              selectorsToCompile add: s.
            ].

            exemplarOIDsByCanonicalMap addAll: otherOracle exemplarOIDsByCanonicalMap.

            immediateMapOIDsByExemplar addAll: otherOracle immediateMapOIDsByExemplar.

            ensureOIDVectorsHaveSizeAtLeast: otherOracle addressesByOID size.

            otherOracle oopsAndAddressesAndOriginalMirrorsAndKleinifiedMirrorsAndOIDsDo: [|:oop. :addr. :mir. :kMir. :oid|
              mir = (reflect: otherOracle objectThatWasDefined) ifTrue: [
                oidsByOop removeKey: otherOracle oldOopForObjectThatWasDefined IfAbsent: [].
              ].
              recordOop: oop AndAddress: addr ForOID: oid KleinifiedMirror: kMir OriginalMirror: mir.
            ].

            addAllNMethodsFrom: otherOracle.

            copyStatisticsFrom: otherOracle.

            assert: [verify. true].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyRemoveAll = ( |
            | 
            (((((((((((((resend.copy invalidateCachedItems
             oidsByOriginalObject:             oidsByOriginalObject             copyRemoveAll)
             oidsByOop:                        oidsByOop                        copyRemoveAll)
             exemplarOIDsByCanonicalMap:       exemplarOIDsByCanonicalMap       copyRemoveAll)
             immediateMapOIDsByExemplar:       immediateMapOIDsByExemplar       copyRemoveAll)
             originalMirrorsByOID:             originalMirrorsByOID             copyRemoveAll)
             kleinifiedMirrorsByOID:           kleinifiedMirrorsByOID           copyRemoveAll)
             selectorsToCompile:               selectorsToCompile               copyRemoveAll)
             mappedSlots:                      mappedSlots                      copyRemoveAll)
             codeSizeStatistics:               codeSizeStatistics               copyRemoveAll)
             resendBytecodesByNMethodOID:      resendBytecodesByNMethodOID      copyRemoveAll)
             reusabilityConditionsByNMethod:   reusabilityConditionsByNMethod   copyRemoveAll)
             nmethodOIDsByMapOID:              nmethodOIDsByMapOID              copyRemoveAll)
             mapOIDsContainingUncompiledSlots: mapOIDsContainingUncompiledSlots copyRemoveAll)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: merging with other oracles\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         copyStatisticsFrom: otherOracle = ( |
            | 
            relocatorNeedsToDoPlaceholderInstructions: relocatorNeedsToDoPlaceholderInstructions
                                         + otherOracle relocatorNeedsToDoPlaceholderInstructions.

            relocatorDoesNotNeedToDoPlaceholderInstructions: relocatorDoesNotNeedToDoPlaceholderInstructions
                                               + otherOracle relocatorDoesNotNeedToDoPlaceholderInstructions.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureAddressesByOIDIsUpdated = ( |
            | 
            updatedAddressesByOIDIfFail: [addressesByOID: addressesByOID copySize: sizeOfOIDVectors].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: growing OID vectors\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         ensureOIDVectorsHaveSizeAtLeast: minSizeNeeded = ( |
            | 
              originalMirrorsByOID size < minSizeNeeded ifTrue: [  originalMirrorsByOID:   originalMirrorsByOID copySize: minSizeNeeded].
            kleinifiedMirrorsByOID size < minSizeNeeded ifTrue: [kleinifiedMirrorsByOID: kleinifiedMirrorsByOID copySize: minSizeNeeded].
               nmethodOIDsByMapOID size < minSizeNeeded ifTrue: [   nmethodOIDsByMapOID:    nmethodOIDsByMapOID copySize: minSizeNeeded].
                    addressesByOID size < minSizeNeeded ifTrue: [        addressesByOID:         addressesByOID copySize: minSizeNeeded].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: maps and exemplars\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         exemplarForMap: map = ( |
            | 
            map isImmediate ifTrue: [
              map myLayout exemplar
            ] False: [
              originalObjectForOID: exemplarOIDsByCanonicalMap at: map
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: maps and exemplars\x7fCategory: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         exemplarOIDsDo: blk = ( |
            | 
            exemplarOIDsByCanonicalMap do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         expectedNumberOfObjects: n = ( |
            | 
            "Optimization. We're spending a few percent of the total mapping
             time growing the mappedObjects dictionary. -- Adam, 2/05"
            oidsByOriginalObject desiredMinCapacity: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         findNMethodOIDForKey: key ForMapOID: mapOID IfPresent: pb IfAbsent: ab = ( |
            | 
            nmethodOIDDictionaryForMapOID: mapOID
                                IfPresent: [|:nmOIDs| nmOIDs if: key IsPresentDo: [|:nmOID| ^ pb value: nmOID]]
                                 IfAbsent: [].

            "Optimization: this error string used to include 'key printString',
            but that was slow. -- Adam & Alex, 5/04"
            ab value: 'No nmethod with that key was found in the oracle.\n',
                      'Check the export policy.').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         findOopForStubNMethodNamed: name IfPresent: pb IfAbsent: ab = ( |
            | 
            "Could cache these if it's slow to keep looking them up and converting
             the OIDs to oops."
            findNMethodOIDForKey: (klein lookupKey copyForNormalSend: name)
                       ForMapOID: (mapOIDForExemplar: klein primitives)
                       IfPresent: [|:nmOID| pb value: oopForOID: nmOID]
                        IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         getANewOID = ( |
            | 
            vm getANewOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: growing OID vectors\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         growOIDVectorsToAtLeast: minSizeNeeded = ( |
            | 
            ensureOIDVectorsHaveSizeAtLeast:  (10000 max: sizeOfOIDVectors double) max: minSizeNeeded.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         image = ( |
            | 
            vm image).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         immediateMaps = ( |
            | 
            immediateMapOIDsByExemplar asVector copyMappedBy: [|:mapOID| originalObjectForOID: mapOID]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         importReflecteeOf: m AsVectorOfImmediatesIfFail: fb = ( |
            | 
            m reflectionPrimitives forVectorMirror: m ImportReflecteeAsVectorOfImmediatesIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         incorporateCodeGenerationStatisticsFrom: cg = ( |
            | 
            cg codeSizeStatistics do: [|:codeSize. :kindOfGeneration|
              codeSizeStatistics if: kindOfGeneration
                       IsPresentPut: [|:n| n + codeSize] AndDo: []
                        IfAbsentPut: [         codeSize] AndDo: [].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: caching\x7fComment: Discards all cached information.\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateCachedItems = ( |
            | 
            [todo oopFormat incrementalUpdate]. "OK, this isn't gonna work for indirect pointers.
                                                 We need to be able to find the OT again so that
                                                 we can recreate it. Maybe we can use code like
                                                 startOfObjectAddressesIfFail:? -- Adam, 8/06"
            vm objectLocator ifDirect: [] IfIndirect: [halt].

            addressesByOID: addressesByOID copySize: 0.
            timestampOfOldestCachedItem: time current.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         invalidateMyObsoleteCachedItems = ( |
            | 
            vm machineMemory invalidateObsoleteCachedItemsIn: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedMirrorForOID: oid IfAbsent: ab = ( |
            | 
            (kleinifiedMirrorsByOID at: oid IfAbsent: [^ ab value])
                                            ifNil:    [  ab value]
                                            IfNotNil: [|:m| m]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: kleinifying methods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifySlot: s = ( |
            | 
            s isMethod ifFalse: [^ s].

            "Compile the kleinified activationMap, not the original Self method."
            remoteKleinMethodSlot copyName: s name
                                    Holder: s holder
                                  Contents: kleinifiedObjectForOriginalMirror: s contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: maps and exemplars\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOIDForExemplarMirror: mir = ( |
            | 
            mir isReflecteeKleinAndYodaImmediate
                    ifTrue: [oidForOriginalObject: mir vmKitMapForConversion]
                     False: [mapOIDForExemplarOID: oidForOriginalMirror: mir]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOIDForExemplarOID: exemplarOID = ( |
            | 
            oidForOop: vmKit layouts object mapOf: oopForOID: exemplarOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mapsOfMemoryObjects = ( |
            | 
            exemplarOIDsByCanonicalMap keys).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: incremental update\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mapsWhoseAncestorsIncludeTheMapOf: definee = ( |
             affectedMaps.
             mapOfObjectBeforeItWasDefined.
             mapOopOfObjectBeforeItWasDefined.
            | 

            mapOopOfObjectBeforeItWasDefined:  vmKit layouts object mapOf: oopForOriginalObject: definee.
            mapOfObjectBeforeItWasDefined: originalObjectForOop: mapOopOfObjectBeforeItWasDefined.

            affectedMaps: list copyRemoveAll.
            computeAncestorsOfMaps do: [|:ancestorMaps. :myMap|
              (ancestorMaps includes: mapOfObjectBeforeItWasDefined) ifTrue: [
                affectedMaps addLast: myMap
              ].
            ].
            affectedMaps).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         methodMirrors = ( |
            | 
            originalMirrors copyFilteredBy: [|:m| m isReflecteeMethod]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         methodMirrorsAllowingDuplicates = ( |
            | 
            originalMirrorsAllowingDuplicates copyFilteredBy: [|:m| m isReflecteeMethod]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         nextOopToAllocateForObjectOfSize: nOops = ( |
            | 
            vmKit layouts memoryObject encode:
              theVM objectLocator
               ifDirect:   [targetSpace nextAddressToAllocateForObjectOfSize: nOops]
               IfIndirect: [theVM nextFreeOID]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodCount = ( |
            | 
            nmethodOIDs size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fCategory: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         nmethodOIDDictionariesByMapOIDDo: blk = ( |
            | 
            nmethodOIDsByMapOID do: [|:nmOIDs. :mapOID|
              nmOIDs ifNotNil: [
                blk value: nmOIDs With: mapOID.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fCategory: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodOIDDictionariesByMapOopDo: blk = ( |
            | 
            nmethodOIDDictionariesByMapOIDDo: [|:nmOIDs. :mapOID|
              blk value: nmOIDs With: oopForOID: mapOID
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodOIDDictionaryForMapOID: mapOID IfPresent: pb IfAbsent: ab = ( |
            | 
            (nmethodOIDsByMapOID at: mapOID) ifNil: ab IfNotNil: pb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodOIDDictionaryForMapOID: mapOID IfPresent: pb IfAbsentPut: ab = ( |
            | 
            nmethodOIDDictionaryForMapOID: mapOID
                                IfPresent: pb
                                 IfAbsent: [| d| d: ab value.
                                                 recordNMethodOIDDictionary: d ForMapOID: mapOID.
                                                 d]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fCategory: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         nmethodOIDs = ( |
             r.
            | 
            [adam]. "Expensive, and we sometimes want to ask for this more than once. Could cache it?"
            r: set copyRemoveAll.
            nmethodOIDDictionariesByMapOIDDo: [|:nmOIDs|
              r addAll: nmOIDs.
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fCategory: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodOIDsDo: blk = ( |
            | 
            nmethodOIDs do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fCategory: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodsDo: blk = ( |
            | 
            nmethodOIDsDo: [|:nmOID| blk value: originalObjectForOID: nmOID].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfMapsMapped = ( |
            | 
            exemplarOIDsByCanonicalMap size + numberOfTypesOfImmediates).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfObjectsMapped = ( |
            | 
            oidsByOriginalObject size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfSlotDescs = ( |
            | 
            (originalMaps copyMappedBy: [|:m| m size]) sum).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfTypesOfImmediates = ( |
            | 
            immediateMapOIDsByExemplar size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         objVectorMirrors = ( |
            | 
            originalMirrors copyFilteredBy: [|:m| m isReflecteeVector]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         objectTable = ( |
            | 
            (kleinifiedMirrorsByOID copyAtMost: numberOfObjectsMapped) mapBy: [|:m| m reflectee]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         occurrencesOfModules = ( |
             r.
            | 
            "Just a little hack I use for gathering statistics. -- Adam, 10/05"
            shouldKeepTrackOfMappedSlots ifFalse: [error: 'This facility will not work unless shouldKeepTrackOfMappedSlots is turned on.'].
            r: (mappedSlots copyMappedBy: [|:s| s module]) occurrencesOfEachElement.
            r: r mapBy: [|:n. :modName| n @ modName] Into: list copyRemoveAll.
            r asVector copySortBy: (| element: a Precedes: b = (a x > b x) |)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOop: oop IfAbsent: ab = ( |
            | 
            oidsByOop at: oop IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOriginalObject: aSelfObj IfAbsent: ab = ( |
            | 
            oidsByOriginalObject at: aSelfObj IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oidOfNMethodNamed: name ForOriginalObject: aSelfObj IfPresent: pb IfAbsent: ab = ( |
             mapOID.
             mapOop.
             origObjOop.
            | 
            origObjOop: oopForOriginalObject: aSelfObj.
            mapOop: vmKit layouts memoryObject mapOf: origObjOop.
            mapOID: oidForOop: mapOop.

            findNMethodOIDForKey: (klein lookupKey copyForNormalSend: name)
                       ForMapOID: mapOID
                       IfPresent: pb
                        IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oidsDo: aBlock = ( |
            | 
            oidsByOop do: aBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid IfAbsent: ab = ( |
            | 
            addressesByOID oopForOID: oid IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oopsAndAddressesAndOIDsDo: aBlock = ( |
            | 
            addressesByOID addressesAndOIDsDo: [|:addr. :oid|
              aBlock value: (addressesByOID oopForOID: oid IfAbsent: [error: 'huh?'])
                      With: addr
                      With: oid.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oopsAndAddressesAndOriginalMirrorsAndKleinifiedMirrorsAndOIDsDo: aBlock = ( |
            | 
            oopsAndAddressesAndOIDsDo: [|:oop. :addr. :oid|
              aBlock value: oop
                      With: addr
                      With: (  originalMirrorForOID: oid)
                      With: (kleinifiedMirrorForOID: oid)
                      With: oid.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oopsDo: aBlock = ( |
            | 
            oopsAndAddressesAndOIDsDo: aBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         originalMaps = ( |
            | 
            mapsOfMemoryObjects, immediateMaps).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         originalMirrorForOID: oid IfAbsent: fb = ( |
            | 
            (originalMirrorsByOID at: oid IfAbsent: [^ fb value])
                                          ifNil:    [  fb value]
                                          IfNotNil: [|:m| m]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         originalMirrors = ( |
            | 
            oidsByOriginalObject keys copyMappedBy: [|:o| reflect: o]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         originalMirrorsAllowingDuplicates = ( |
            | 
            oidsByOriginalObject keys copyMappedBy: [|:o| reflect: o]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: recording\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         recordKleinifiedMirror: km ForOID: oid = ( |
            | 
            kleinifiedMirrorsByOID at: oid Put: km.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: maps and exemplars\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         recordMapOID: mapOID ForImmediateExemplar: immediateExemplar = ( |
            | 
            immediateMapOIDsByExemplar at: immediateExemplar Put: mapOID.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: maps with uncompiled slots\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         recordMapOID: mapOID WithUncompiledSlotNamed: slotName = ( |
            | 
            (mapOIDsContainingUncompiledSlots at: slotName IfAbsentPut: [set copyRemoveAll]) add: mapOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         recordNMethodOIDDictionary: d ForMapOID: mapOID = ( |
            | 
            sizeOfOIDVectors <= mapOID ifTrue: [growOIDVectorsToAtLeast: mapOID succ].
            nmethodOIDsByMapOID at: mapOID Put: d.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: recording\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         recordNewOop: oop AndAddress: addr ForOID: oid = ( |
             x.
            | 
            [oopFormatSmallSelf].
            addressesByOID recordAddress: addr ForOID: oid.
            oidsByOop at: oop Put: oid.
            verifySizesOfDictionaries.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: recording\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         recordOop: oop AndAddress: addr ForOID: oid KleinifiedMirror: km OriginalMirror: om = ( |
            | 
            sizeOfOIDVectors <= oid ifTrue: [growOIDVectorsToAtLeast: oid succ].

            recordOriginalMirror:   om  ForOID: oid.
            recordKleinifiedMirror: km  ForOID: oid.
            recordNewOop:           oop AndAddress: addr ForOID: oid.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: recording\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         recordOriginalMirror: om ForOID: oid = ( |
            | 
            originalMirrorsByOID at: oid          Put:  om.
            oidsByOriginalObject at: om reflectee Put: oid.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: recording\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         recordReusabilityConditions: rcs ForNMethod: nm = ( |
            | 
            reusabilityConditionsByNMethod at: nm Put: rcs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         recordSlot: s = ( |
            | 
            shouldKeepTrackOfMappedSlots ifTrue: [
              mappedSlots add: s.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         relocatorAssembledPlaceholderInstructions = ( |
            | 
            relocatorNeedsToDoPlaceholderInstructions:
              relocatorNeedsToDoPlaceholderInstructions succ.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         relocatorAssembledRealInstructions = ( |
            | 
            relocatorDoesNotNeedToDoPlaceholderInstructions:
              relocatorDoesNotNeedToDoPlaceholderInstructions succ.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: kleinifying methods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         remoteKleinMethodSlot = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectsOracle parent remoteKleinMethodSlot.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         contents.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         holder.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (\'\')\x7fVisibility: public'
        
         name <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectsOracle parent remoteKleinMethodSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         = s = ( |
            | 
            (holder = s holder) && [key = s key]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyName: n Holder: h Contents: c = ( |
            | 
            ((copy name: n) holder: h) contents: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            holder hash ^^ key hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignable = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignment = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         isMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         key = ( |
            | 
            name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinCompilerPrototypeForMe = ( |
            | 
            klein compiler1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAnyNMethodsForMapOID: mapOID = ( |
            | 
            recordNMethodOIDDictionary: nil ForMapOID: mapOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: replacing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceObject: oldObj With: newObj = ( |
             oid.
             oop.
            | 
            oid: oidsByOriginalObject at: oldObj IfAbsent: [^ self].
            oidsByOriginalObject removeKey: oldObj.
            oidsByOriginalObject at: newObj Put: oid.
            originalMirrorsByOID at: oid    Put: reflect: newObj.

            verifySizesOfDictionaries.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: resends\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         resendsInNMethodWithOID: nmOID = ( |
            | 
            resendBytecodesByNMethodOID at: nmOID IfAbsent: [dictionary copyRemoveAll]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         reusabilityConditionsForNMethod: nm = ( |
            | 
            reusabilityConditionsByNMethod at: nm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fComment: Keeping track of mappedSlots isn\'t necessary; I\'ve just found it
useful when trying to gather statistics about what\'s in the
Klein image. There sure are a lot of them, though, so when I
don\'t care about the statistics it\'s probably better not to
waste the space. -- Adam, 11/05\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldKeepTrackOfMappedSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: growing OID vectors\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         sizeOfOIDVectors = ( |
             a.
            | 
            a: addressesByOID.
            [a isEmpty ||
             [nmethodOIDsByMapOID size =                      a size]] assert.
            [ nmethodOIDsByMapOID size = kleinifiedMirrorsByOID size ] assert.
            [ nmethodOIDsByMapOID size =   originalMirrorsByOID size ] assert.
             nmethodOIDsByMapOID size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         stringMirrors = ( |
            | 
            "Useful when trying to find out what's taking up so much space in the bytes
             section. --Adam, 11/05"
            originalMirrors copyFilteredBy: [|:m| m isReflecteeString]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         targetSpace = ( |
            | 
            vm universe tenuredSpace).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: addresses\x7fCategory: by original object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         addressForOriginalObject: o = ( |
            | 
            addressForOriginalObject: o IfAbsent: [error: 'absent']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: addresses\x7fCategory: by original object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         addressForOriginalObject: o IfAbsent: ab = ( |
            | 
            myVM objectLocator
              addressForOID: (oidForOriginalObject: o IfAbsent: [^ ab value])
                   IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         breakDownNMethodsByHolder = ( |
             holders.
            | 
            holders: list copyRemoveAll.
            nmethodsDo: [|:nm. holder|
              holder: reflect: nm topScope methodHolder.
              holder isReflecteeBlock              ifTrue: [holder:         'block!!!' asMirror].
              holder isReflecteeKleinCompiledBlock ifTrue: [holder: 'compiledBlock!!!' asMirror].
              holders add: holder.
            ].
            sortedOccurrenceCountsFor: holders).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         breakDownNMethodsByHolderAndSlotName = ( |
             holdersAndSelectors.
            | 
            holdersAndSelectors: list copyRemoveAll.
            nmethodsDo: [|:nm. holder|
              holder: reflect: nm topScope methodHolder.
              holder isReflecteeBlock              ifTrue: [holder:         'block!!!' asMirror].
              holder isReflecteeKleinCompiledBlock ifTrue: [holder: 'compiledBlock!!!' asMirror].
              holdersAndSelectors add: holder @ nm lookupKey selector.
            ].
            sortedOccurrenceCountsFor: holdersAndSelectors).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         breakDownNMethodsBySlotType = ( |
             accs.
             meths.
             muts.
             rest.
            | 
            rest: list copyRemoveAll.
            meths: list copyRemoveAll.
            accs: list copyRemoveAll.
            muts: list copyRemoveAll.
            nmethodsDo: [|:nm. s|
              s:  (reflect: nm topScope methodHolder) at: nm lookupKey selector.
              case
                if: [s isAssignable] Then: [accs add: nm]
                If: [s isAssignment] Then: [muts add: nm]
                If: [s isMethod    ] Then: [meths add: nm]
                                     Else: [rest add: nm].
            ].
            (accs & muts & meths & rest) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         breakDownRelocatorsByType = ( |
             protos.
            | 
            protos: reflectiveIdentityDictionary copyRemoveAll.
            nmethodsDo: [|:nm|
              nm reconstructRelocators do: [|:r|
                protos if: r prototype IsPresentPut: [|:n| n succ] AndDo: [] IfAbsentPut: 1 AndDo: [].
              ].
            ].
            protos mapBy: [|:n. :r| r @ n] Into: vector copySize: protos size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: OIDs\x7fCategory: by object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         includesOriginalObject: aSelfObj = ( |
            | 
            oidForOriginalObject: aSelfObj IfAbsent: [^ false].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: kleinified objects\x7fCategory: by OID\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedMirrorForOID: oid = ( |
            | 
            kleinifiedMirrorForOID: oid IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: kleinified objects\x7fCategory: by oop\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedMirrorForOop: oop = ( |
            | 
            kleinifiedMirrorForOop: oop IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: kleinified objects\x7fCategory: by oop\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedMirrorForOop: oop IfAbsent: ab = ( |
            | 
            kleinifiedMirrorForOID: (oidForOop: oop IfAbsent: [^ ab value]) IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: kleinified objects\x7fCategory: by original object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedMirrorForOriginalMirror: origMir = ( |
            | 
            reflect: kleinifiedObjectForOriginalMirror: origMir).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: kleinified objects\x7fCategory: by OID\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedObjectForOID: oid = ( |
            | 
            kleinifiedObjectForOID: oid IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: kleinified objects\x7fCategory: by OID\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedObjectForOID: oid IfAbsent: ab = ( |
            | 
            (kleinifiedMirrorForOID: oid IfAbsent: [^ ab value]) reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: kleinified objects\x7fCategory: by oop\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedObjectForOop: oop = ( |
            | 
            kleinifiedObjectForOop: oop IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: kleinified objects\x7fCategory: by oop\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedObjectForOop: oop IfAbsent: ab = ( |
            | 
            (kleinifiedMirrorForOop: oop IfAbsent: [^ ab value]) reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: kleinified objects\x7fCategory: by original object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedObjectForOriginalMirror: origMir = ( |
            | 
            kleinifiedObjectForOID: oidForOriginalMirror: origMir IfAbsent: [
              [origMir isReflecteeKleinAndYodaImmediate] assert.
              ^ origMir reflectee
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: map OIDs by receiver\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mapForOriginalMirror: origMir = ( |
            | 
            mapForOriginalObject: origMir reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: map OIDs by receiver\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mapForOriginalObject: o = ( |
            | 
            kleinifiedObjectForOID: mapOIDForExemplar: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: map OIDs by receiver\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOIDForExemplar: e = ( |
            | 
            mapOIDForExemplarMirror: reflect: e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: statistics\x7fCategory: prototypes\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         objectSize = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> 'objectSize' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectsOracle parent trackingObjects objectSize.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: statistics\x7fCategory: prototypes\x7fComment: This is a terrible name for this object. -- Adam, 10/05\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         objectStat = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> 'objectStat' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectsOracle parent trackingObjects objectStat.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: OIDs\x7fCategory: by oop\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOop: oop = ( |
            | 
            oidForOop: oop IfAbsent: [error: 'absent']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: OIDs\x7fCategory: by object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOriginalMirror: origMir = ( |
            | 
            oidForOriginalMirror: origMir IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: OIDs\x7fCategory: by object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOriginalMirror: origMir IfAbsent: ab = ( |
            | 
            oidForOriginalObject: origMir reflectee IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: OIDs\x7fCategory: by object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOriginalObject: aSelfObj = ( |
            | 
            oidForOriginalObject: aSelfObj IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: oops\x7fCategory: by OID\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid = ( |
            | 
            oopForOID: oid IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: oops\x7fCategory: by object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOriginalMirror: origMir = ( |
            | 
            oopForOriginalMirror: origMir IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: oops\x7fCategory: by object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOriginalMirror: origMir IfAbsent: ab = ( |
            | 
            oopForOriginalObject: origMir reflectee IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: oops\x7fCategory: by object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOriginalObject: obj = ( |
            | 
            oopForOriginalObject: obj IfAbsent: 0 raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: oops\x7fCategory: by object\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOriginalObject: obj IfAbsent: ab = ( |
            | 
            oopForOID: (oidForOriginalObject: obj IfAbsent: [^ ab value]) IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: objects\x7fCategory: by OID\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         originalMirrorForOID: oid = ( |
            | 
            originalMirrorForOID: oid IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: objects\x7fCategory: by OID\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         originalObjectForOID: oid = ( |
            | 
            originalObjectForOID: oid IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: objects\x7fCategory: by OID\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         originalObjectForOID: oid IfAbsent: fb = ( |
            | 
            (originalMirrorForOID: oid IfAbsent: [^ fb value]) reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: objects\x7fCategory: by oop\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         originalObjectForOop: oop = ( |
            | 
            originalObjectForOop: oop IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: objects\x7fCategory: by oop\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         originalObjectForOop: oop IfAbsent: fb = ( |
            | 
            originalObjectForOID: (oidForOop: oop IfAbsent: [^ fb value]) IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         selectorsSent = ( |
             r.
            | 
            r: set copyRemoveAll.
            methodMirrorsAllowingDuplicates do: [|:m|
              m literalsDo: [|:lit|
                (reflect: lit) isReflecteeString ifTrue: [
                  r add: lit.
                ].
              ].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         selfPrototypeOf: m = ( |
            | 
            m prototypeIfPresent: [|:p| p]
                        IfAbsent: [case
                                          if: [m isReflecteeBlockMethod] Then: [mirrors blockMethod ]
                                          If: [m isReflecteeMethod     ] Then: [mirrors method      ]
                                          If: [m isReflecteeBlock      ] Then: [mirrors block       ]
                                          If: [m isReflecteeString     ] Then: [reflect: ''         ]
                                          If: [m isReflecteeVector     ] Then: [reflect: vector     ]
                                        Else:                                  [reflect: ()         ]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         sortedCodeSizeStats = ( |
            | 
            (codeSizeStatistics copyMappedBy: [|:codeSize. :kindOfGeneration| codeSize @ kindOfGeneration])
               asVector copySortBy: (| element: a Precedes: b = (a x > b x) |)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         sortedOccurrenceCountsFor: aCollection = ( |
             occurrences.
            | 
            occurrences: list copyRemoveAll.
            aCollection occurrencesOfEachElement do: [|:n. :p| occurrences add: n @ p].
            occurrences asVector copySortBy: (| element: a Precedes: b = (a x > b x) |)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: statistics\x7fCategory: prototypes\x7fComment: This is a terrible name for this object. -- Adam, 10/05\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         typeStat = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> 'typeStat' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectsOracle parent trackingObjects typeStat.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         updatedAddressesByOIDIfFail: fb = ( |
            | 
            invalidateMyObsoleteCachedItems.
            addressesByOID isEmpty ifTrue: [| m. v. newSize | 
                m: vm image mirrorOnTheObjectLocatorIfFail: [|:e| ^ fb value: e].
                v: importReflecteeOf: m AsVectorOfImmediatesIfFail: [|:e| ^ fb value: e].
                newSize: v size max: sizeOfOIDVectors.

                            addressesByOID:             addressesByOID copyContaining: v.
                      originalMirrorsByOID:       originalMirrorsByOID  copySize: newSize.
                    kleinifiedMirrorsByOID:     kleinifiedMirrorsByOID  copySize: newSize.
                       nmethodOIDsByMapOID:        nmethodOIDsByMapOID  copySize: newSize.

              addressesByOID dataSlotsToFixUpIn: m Do: [|:s. :localHolder|
                s fromRemoteContentsSetLocalContentsIn: localHolder.
              ].
            ].
            addressesByOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         verify = ( |
            | 
            verifyMaps.
            verifyNMethods.
            verifyObjectsAndOopsAndOIDs.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyMaps = ( |
            | 
            numberOfObjectsMapped do: [|:i. oop. origMap. mapOop. importedMap|
              oop: addressesByOID oopForOID: i IfAbsent: [error: 'huh?'].
              mapOop: vmKit layouts memoryObject mapOf: oop.
              origMap: originalObjectForOop: mapOop.
              importedMap: vmKit maps map importMapFor: oop IfFail: raiseError.
              origMap verifyEqualToImportedMap: importedMap.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyNMethods = ( |
            | 
            nmethodsDo: [|:nm|
              [nm isKleinNMethod] assert.
              oopForOriginalObject: nm IfAbsent: [error: nm printString, ' is absent'].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyObjectsAndOopsAndOIDs = ( |
            | 
            verifySizesOfDictionaries.

            oidsByOriginalObject do: [|:oid. :obj|
              assert: (originalMirrorsByOID at: oid) Is:  reflect: obj.
            ].
            (updatedAddressesByOIDIfFail: [vmKit abstractObjectLocator copyRemoveAll]) oopsAndOIDsDo: [|:oop. :oid|
              [todo cleanup oopFormat vacuous if object table].
              assert: (oidForOop: oop) Is: oid.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         verifySizesOfDictionaries = ( |
            | 
            [oidsByOriginalObject size = oidsByOop size] assert.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         vm = ( |
            | 
            theVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            vm vmKit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Category: statistics\x7fCategory: eager relocation\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         relocatorDoesNotNeedToDoPlaceholderInstructions <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Category: statistics\x7fCategory: eager relocation\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         relocatorNeedsToDoPlaceholderInstructions <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         resendBytecodesByNMethodOID <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (reflectiveIdentityDictionary copyRemoveAll)\x7fVisibility: private'
        
         reusabilityConditionsByNMethod <- reflectiveIdentityDictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Category: optimizations\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         selectorsToCompile <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (time origin)\x7fVisibility: private'
        
         timestampOfOldestCachedItem <- time origin.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         vmKitObjMapper = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjMapper' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjMapper' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitObjMapper.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjMapper' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjMapper' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjMapper' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjMapper' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjMapper' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.13 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitObjMapper' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitVarHdrsObjMapr
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: exporting\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         findReflecteeUsingOracle: oracle = ( |
            | 
            reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: exporting\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifyForObjectMapper: mapper = ( |
            | 
            "See the comment in" [kleinAndYoda objectsOracle kleinifiedMirrorsByOID].

            mapper policy shouldEviscerateModuleObjects && [isReflecteeModule] ifTrue: [
              [todo modulesInKlein]. "Maybe keep the postFileIn method or something? We're gonna need it."
              ^ reflect: ()
            ].

            "Most objects don't need to be changed when we map them
             to Klein."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: exporting\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         requestCompilationWithObjectMapper: mapper = ( |
             map.
             mapOID.
            | 
            mapOID:      mapper objectsOracle mapOIDForExemplarMirror: self.
            map:         mapper objectsOracle kleinifiedObjectForOID:  mapOID.
            mapper requestCompilationForReceiverMirror: self Map: map MapOID: mapOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: exporting\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'method' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifyForObjectMapper: mapper = ( |
            | reflect: vmKitMapForConversion kleinifyReflecteeOf: self Mapper: mapper).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'mirror' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifyForObjectMapper: mapper = ( |
            | 
            "Self mirrors will not work properly in Klein; we need to
             transform the Self mirror into a Klein mirror."

            mapper policy shouldKleinifyMirrors && [reflectee isReflecteeSlots] ifTrue: [
              | reflecteeOop. mirrorProto. kleinMir |
              reflecteeOop:  mapper allocateObjectForReflecteeOf: reflectee.
              mirrorProto: reflectee mirrorPrototypeFromNamespace: theVM vmKit mirrors.

              kleinMir:
                theVM vmKit layouts object
                  if:          reflecteeOop
                  IsImmediate: [mirrorProto copyForVM: theVM Oop: reflecteeOop IfFail: raiseError]
                  IsMark:      raiseError
                  IsMem:       [mirrorProto copyWithoutInitializingForVM: theVM
                                                                     OID: (mapper objectsOracle oidForOop: reflecteeOop)
                                                                  IfFail: raiseError].

              (reflect: kleinMir) kleinifyForObjectMapper: mapper
            ] False: [
              resend.kleinifyForObjectMapper: mapper
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'vector' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifyForObjectMapper: mapper = ( |
            | 
            mapper policy shouldCanonicalizeEmptyVectors
              && [reflecteeSize = 0]
              && [names size = 1]
              && [(at: 'parent' IfAbsent: nil) contents = (reflect: traits vector)] ifTrue: [

                reflect: vector
            ] False: [
                resend.kleinifyForObjectMapper: mapper
            ]).
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitVarHdrsObjMapr' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitObjMapper postFileIn
