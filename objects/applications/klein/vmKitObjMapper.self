 '$Revision: 30.13 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

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

            "A bit of duplication with" [fillInObjectAt: 0 OID: 0 AndCreateMapForReflecteeOf: m Mapper: mapper].

            "Ensure that the method's annotation and slot annotations are parsed."
            m annotation. m do: [|:s| s annotation].

            slots:       slotsToMapForReflecteeOf: m Mapper: mapper.
            slotsByName: dictionary copyRemoveAll.
            slots do: [|:s| slotsByName at: s kleinAndYodaSlotName Put: s].

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
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         fillInObjectAt: oop OID: oid AndCreateMapForReflecteeOf: m Mapper: mapper = ( |
             map.
             slots.
             slotsByName.
            | 

            "A bit of duplication with" [kleinifyReflecteeOf: m Mapper: mapper].

            slots:  slotsToMapForReflecteeOf: m Mapper: mapper.

            slotsByName: dictionary copyRemoveAll.
            slots do: [|:s| slotsByName at: s kleinAndYodaSlotName Put: s].

            map:  copySize: slots size.

            map initializeForReflecteeOf: m Slots: slots Mapper: mapper.

            mapper allocateMap: map ForOID: oid AndDo: [|:canonicalMap. :canonicalMapOop|
              myLayout             for: oop
                                   OID: oid
                              UsingMap: canonicalMap
                             AndMapOop: canonicalMapOop
                       AdjustMarkValue: layouts mark defaultMarkValue
              AndFillInHeaderFieldsFor: m
                                Mapper: mapper.
            ].

            map objectSlotsDo: [|:slotName. :slotOffset. slot. contentsObj. contentsOop|
              slot:         slotsByName at: slotName.
              contentsObj:  mapper policy initialContentsOfSlot: slot.
              contentsOop:  mapper allocateObject: contentsObj.

              myLayout for: oop At: slotOffset Put: contentsOop.
            ].

            map ifNeededFillInIndexableSlotsFor: m In: oop Mapper: mapper.

            oop).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         forReflecteeOf: m AddAllMappedVMSlotsTo: aList Mapper: mapper = ( |
            | 
            m fakeSlotsDo: [|:s|
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
            m do: [|:s| (mapper policy isSlotToBeMapped: s) ifTrue: [slots add: s]].
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
         'Category: object mapper state\x7fCategory: transient state\x7fCategory: caches\x7fCategory: block nmethods\x7fComment: nmethods compiled for block objects, keyed by lookupKey.
Every block\'s nmethod cache includes all of these nmethods
except one (the one that\'s overridden by that block\'s
valueSlot). -- Adam, 3/05\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         cachedBlockNMethodOIDs <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: cache stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         cachedResendMisses <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: cache stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         cachedResendProbes <- 0.
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
         'Category: object mapper state\x7fCategory: transient state\x7fCategory: caches\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         cachedSlotsToCompileByReceiverMirror <- dictionary copyRemoveAll.
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
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         compilationTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         compileCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fComment: It\'s expensive to go through the whole process of figuring
out which slots to compile, so when we see a map we\'ve
already compiled, don\'t bother going through it again.
-- Adam, 12/04 (this cache has been here since long before
                12/04, but I\'m going through and making
                sure all the caches make sense)

I don\'t see why we still need this, now that the one-pass Cheney
mapper compiles based on the exemplars dictionary.
But, I\'ll use it as an assertion check.
(Nice comment, Adam.)

Was: mappedMapsThatHaveBeenCompiled
 -- dmu 2/05\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         compiledReceiverMirrorsByMapOID <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
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
         'Category: object mapper state\x7fCategory: transient state\x7fCategory: caches\x7fCategory: block nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         foundFirstBlock <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: transient state\x7fCategory: caches\x7fCategory: block nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         foundSecondBlock <- bootstrap stub -> 'globals' -> 'false' -> ().
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
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         isReusableTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         kleinPrimitiveMirrorOops <- set copyRemoveAll.
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
         'Category: mapping objects\x7fCategory: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateContentsOfSlotsToCompile = ( |
             alreadyFoundOneBlock <- bootstrap stub -> 'globals' -> 'false' -> ().
             alreadyFoundTwoBlocks <- bootstrap stub -> 'globals' -> 'false' -> ().
             valueSlotNameOfFirstBlock.
            | 

            [todo cleanup someone]. "This code is needed for methods (like theVM in the miniVM) that are
             compiled but not reached by the mapping algorithm. I think the reason why some methods
             aren't reached by the mapping algorithm is because they're inherited through parent slots
             that aren't mapped. So, for example, in the miniVM, the theVM slot is inherited
             (in anything that descends from the lobby) through the defaultBehavior slot, but
             the defaultBehavior slot isn't mapped. We still compile it, though, because we're
             getting our list of slots to compile (see slotsToCompileForReceiver:) by getting a
             list of all the slots on the *Self object* (not the Klein object) and then compiling
             the ones specified by the exportPolicy. So first, we should probably be getting the
             list of slots from the Klein object, not the Self object. And second, we should
             probably make sure that all the methods we compile are inherited through parent
             slots that are mapped. -- Adam, 6/05"

            [todo cleanup adam]. "This is really messy and duplicates the block-optimization thingy elsewhere."

            exemplarMirrorsToCompileByMapOIDDo: [|:exemplarMir|
              [|:exit|
                exemplarMir isReflecteeBlock ifTrue: [
                  "Optimization: Don't bother going over every single block - there are a lot of
                   them, and it's unnecessary, because all blocks inherit the same slots, except
                   for the value slots (and those will definitely be allocated through the
                   regular mapping phase). -- Adam, 6/05"
                  alreadyFoundOneBlock ifFalse: [
                    valueSlotNameOfFirstBlock: exemplarMir valueSlotName.
                    alreadyFoundOneBlock: true.
                  ] True: [
                    alreadyFoundTwoBlocks || [exemplarMir valueSlotName = valueSlotNameOfFirstBlock] ifFalse: [
                      alreadyFoundTwoBlocks: true.
                    ] True: exit.
                  ].
                ].
                (slotsToCompileForReceiver: exemplarMir) do: [|:s|
                  (shouldCompileSlotNamed: s name) ifTrue: [
                    allocateObjectForReflecteeOf: s contents.
                  ].
                ].
              ] exit.
            ].

            self).
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
                                          cmOop: allocateObject: cm].
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
            oop: theVM objectLocator oopForOID: oid Address: addr.
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
                (kMir reflectee isForVM: theVM) ifTrue: [
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
        
         allocatePrototypeSimpleVector = ( |
            | 
            prototypeSimpleVectorOop: allocateObject: vector copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: allocating\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateStartingPoints: sp = ( |
            | 
            sp do: [|:o| allocateObject: o].
            allocatePrototypeSimpleVector.
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
             nilMir.
            | 
            nilMir: reflect: nil.
            newActivationMap objectSlotsDo: [|:slotName. slot. contentsObj|
              slot:         slotsByName at: slotName.
              contentsObj:  exportPolicy initialContentsOfSlot: slot.

              (reflect: contentsObj) = nilMir ifFalse: [| i |
                i: method indexOfLocalNamed: slot name IfFail: raiseError.
                aList addAll: bytecodesToInitializeSlotWithIndex: i ToLiteral: contentsObj.
              ].
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
        
         compilationRequesterFor = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent compilationRequesterFor.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent compilationRequesterFor abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         cachedLookupKey.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (-1)'
        
         mapOIDOfReceiver <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         myCompiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         mySelector.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         mySlot.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         objectMapper.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent compilationRequesterFor abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: this method\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         buildNMethod = ( |
             leafOrNonLeafCompiler.
             nm.
             nmOID.
             nmOop.
             predictedOop.
            | 
            objectMapper incrementCompileCount.
            objectMapper compilationTime:
              objectMapper compilationTime + [
                leafOrNonLeafCompiler: compiler compileForcingNonLeafIfNecessary.
                nm: leafOrNonLeafCompiler buildNMethod.
                klein relocators isEagerRelocationEnabled ifTrue: [|twc|
                  twc: objectMapper totalWordCountForReflecteeOf: reflect: nm.
                  predictedOop: compiler oracleForEagerRelocation nextOopToAllocateForObjectOfSize: twc.
                ].
                objectMapper incorporateCodeGenerationStatisticsFrom: leafOrNonLeafCompiler codeGenerator.
            ] cpuTime.

            objectMapper nmethodAllocationTime:
              objectMapper nmethodAllocationTime + [
                nmOID: objectMapper vm nextFreeOID.
                nmOop: objectMapper allocateMemoryObjectForReflecteeOf: nm asMirror.
                [objectMapper vm nextFreeOID = nmOID succ] assert.
            ] cpuTime.

            klein relocators isEagerRelocationEnabled ifTrue: [
              objectMapper eagerNMethodLoadingRelocationTime:
                objectMapper eagerNMethodLoadingRelocationTime + [
                  reassembleNMethod: nm WithOop: nmOop PredictedOop: predictedOop CodeGenerator: leafOrNonLeafCompiler codeGenerator.
              ] cpuTime.
            ].

            nmOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: other methods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationRequesterForBlockValueSlotOf: bMir = ( |
            | 
            objectMapper compilationRequesterFor block copyForBlockMirror: bMir
                                        LexicalParentCompilationRequester: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: other methods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationRequesterForResend: bc = ( |
             resentSlot.
            | 
            "Let missing resent methods be an error at runtime rather than compile time, now
             that we're exporting way more stuff - we've got lots of resends that are never
             getting called, and it's annoying to have to pull in an entire module just
             because we resend to one method in it. -- Adam & Alex, 5/04"
            resentSlot: (bc asMessage lookupSlotsUsing:  objectMapper policy slotFinder copyForMirror: outerMethodSlot holder)
                                                ifNone: [ ^ nil]
                                                IfOne:  [|:s| s]
                                                IfMany: [error: 'ambiguous resend'].

            (((((objectMapper compilationRequesterFor outerMethod copy
                             rcvrMir: selfMir)
                              mySlot: resentSlot)
                          lookupType: bc kleinBaseLookupType)
                        objectMapper: objectMapper)
                    mapOIDOfReceiver: mapOIDOfSelf)
                objectDoingTheResend: outerMethodSlot holder reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: other methods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationRequestersForBlockLiterals = ( |
            | 
            slot contents blockLiterals copyMappedBy: [|:blk| compilationRequesterForBlockValueSlotOf: blk asMirror]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: other methods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationRequestersForResends = ( |
             crs.
             resends.
            | 
            slot isMethod ifFalse: [^ vector].
            objectMapper resendDetectionTime: objectMapper resendDetectionTime + [resends:  objectMapper resendsInSlot: slot] cpuTime.
            crs: list copyRemoveAll.
            resends do: [|:bc| (compilationRequesterForResend: bc) ifNotNil: [|:cr| crs add: cr]].
            crs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         compileIfNecessary = ( |
            | 
            hasAlreadyCompiledNMethodForThisMap ifFalse: [
              compileThisMethod.
              compileValueSlotsOfBlockLiterals.
            ].
            compileOuterMethodsNeededForResends.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: other methods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compileOuterMethodsNeededForResends = ( |
            | 
            compilationRequestersForResends do: [|:cr| cr compileIfNecessary].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: this method\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compileThisMethod = ( |
             isR.
             nmOID.
            | 
            objectMapper isReusableTime: 
              objectMapper isReusableTime + [isR: willNMethodBeReusable] cpuTime.

            nmOID: isR
                     ifTrue: [objectMapper reusableNMethodOIDForSlot: slot
                                                           LookupKey: lookupKey
                                                         IfAbsentPut: [buildNMethod]]
                      False: [                                         buildNMethod ].

            objectMapper recordingNMethodsTime: objectMapper recordingNMethodsTime + [
              recordNMethodOID: nmOID.
              isOuterMethodForABlock ifTrue: [objectMapper recordBlockNMethodOID: nmOID LookupKey: lookupKey].
            ] cpuTime.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: other methods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compileValueSlotsOfBlockLiterals = ( |
            | 
            compilationRequestersForBlockLiterals do: [|:cr| cr compileIfNecessary].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: this method\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compiler = ( |
            | 
            myCompiler ifNil: [
              showCompilingSlot.
              myCompiler:
                theVM compilerPrototype
                          copyForSlot: slotToCompile
                                 Self: selfMir
                             Receiver: rcvrMir
                           LookupType: lookupType
                 ObjectDoingTheResend: objectDoingTheResend
                        OuterNMethods: findLexicalParentNMethods
                         Architecture: theVM architecture
                               Oracle: objectMapper oracleForEagerRelocation
                                Debug: theVM exportPolicy shouldCompileInDebugMode.

              myCompiler
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: profiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilerForSlot: slot = ( |
             arch = 'ppc'.
             lookupType = 4.
             objectDoingTheResend = bootstrap stub -> 'globals' -> 'nil' -> ().
             onms = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
             rcvrMir.
            | 
            rcvrMir: slot holder.
            klein compiler1s abstract
                        copyForSlot: slot
                               Self: rcvrMir
                           Receiver: rcvrMir
                         LookupType: lookupType
               ObjectDoingTheResend: objectDoingTheResend
                      OuterNMethods: onms
                       Architecture: arch
                             Oracle: klein compiler1s abstract oracleThatCannotDoEagerRelocation
                              Debug: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         findLexicalParentNMethods = ( |
             cr.
             nms.
            | 
            nms: list copyRemoveAll.
            cr: self.
            [cr isSlotAnOuterMethod] whileFalse: [
              cr: cr lexicalParentCompilationRequester.
              cr nmethodIfPresent: [|:nm| nms addLast: nm]
                         IfAbsent: [error: 'need parent nmethod for compiler'].
            ].
            nms).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         hasAlreadyCompiledNMethodForThisMap = ( |
             b.
            | 
            objectMapper alreadyCompiledLookupTime: objectMapper alreadyCompiledLookupTime + [
              b:  nmethodOIDIfPresent: true IfAbsent: false.
            ] cpuTime.
            b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         isOuterMethodForABlock = ( |
            | 
            rcvrMir isReflecteeBlock && [rcvrMir = selfMir]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         lookupKey = ( |
            | 
            cachedLookupKey ifNil: [
              cachedLookupKey:  klein lookupKey copyForSelector: selector
                                                     LookupType: lookupType
                                           ObjectDoingTheResend: objectDoingTheResend
                                                     SlotHolder: [slot holder reflectee].
            ].
            cachedLookupKey).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         mapOIDOfSelf = ( |
            | mapOIDOfReceiver "overridden for blocks").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodIfPresent: pBlk IfAbsent: aBlk = ( |
            | 
            nmethodOIDIfPresent: [|:nmOID| pBlk value:  objectMapper originalObjectForOID: nmOID]
                       IfAbsent: aBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: profiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         profileBuildingIRNodes = ( |
             bcis.
             cs.
             slot.
            | 
            slot: klein maps slotsMap parent asMirror at: 'cloneLocalObject:Size:FillingWith:IfFail:'.
            cs: (vector copySize: 100) copyMappedBy: [compilerForSlot: slot].
            [cs do: [|:c| c irNodes]] profileSlice).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: profiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         profileCompilingOneMethod = ( |
            | 
            [
            "  (vector copyAddLast: ('primReceiver:ByteVectorConcatenate:Prototype:IfFail:' @ klein primitives))"
            "  (vector copyAddLast: ('removeAllBuckets:' @ traits hashTableSetOrDictionary))"
            "  (vector copyAddLast: ('canonicalRepresentative' @ assemblerSystems ppc generators instructionTemplates proto))"
            "  ('testSuccess' @ klein virtualMachines selfVM tests smallIntegers parent)"
              (vector copyAddLast: ('cloneLocalObject:Size:FillingWith:IfFail:' @ klein maps slotsMap parent))
                  do: [|:p|
                compile: p x On: p y Times: 100.
              ].
            ] profileSlice).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: this method\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         recordNMethodOID: nmOID = ( |
            | 
            objectMapper addNMethodOID: nmOID MapOID: mapOIDOfReceiver LookupKey: lookupKey.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: this method\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         remoteKleinMethodSlot = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> 'remoteKleinMethodSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent compilationRequesterFor abstract parent remoteKleinMethodSlot.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> 'remoteKleinMethodSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         contents.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> 'remoteKleinMethodSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         holder.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> 'remoteKleinMethodSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (\'\')\x7fVisibility: public'
        
         name <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> 'remoteKleinMethodSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent compilationRequesterFor abstract parent remoteKleinMethodSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyName: n Holder: h Contents: c = ( |
            | 
            ((copy name: n) holder: h) contents: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinCompilerPrototypeForMe = ( |
            | 
            klein compiler1s methodSlot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> 'remoteKleinMethodSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         selector = ( |
            | 
            mySelector ifNil: [mySelector: mySlot name].
            mySelector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: this method\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         slotToCompile = ( |
            | 
            slot isMethod ifFalse: [^ slot].

            "Compile the kleinified activationMap, not the original Self method."
            remoteKleinMethodSlot copyName: slot name
                                    Holder: slot holder
                                  Contents: objectMapper kleinifiedObjectForOriginalMirror: slot contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         willNMethodBeReusable = ( |
            | 
            slot isAssignment || [slot isAssignable]  ifTrue: [^ false].

            slot isMethod ifFalse: [^ true]. "constant slot"

            compiler irNodes noneSatisfy: [|:n| n isDependentOnReceiver]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         rcvrMir.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         block = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda objectMapper1 parent compilationRequesterFor abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent compilationRequesterFor block.

CopyDowns:
globals kleinAndYoda objectMapper1 parent compilationRequesterFor abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         lexicalParentCompilationRequester.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         mapOIDOfSelf.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent compilationRequesterFor block parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForBlockMirror: bMir LexicalParentCompilationRequester: lpcr = ( |
            | 
            (((((copy
                                  objectMapper: lpcr objectMapper)
                                       rcvrMir: bMir)
                                        mySlot: bMir valueSlot)
                              mapOIDOfReceiver: lpcr objectMapper objectsOracle mapOIDForExemplar: bMir reflectee)
                                  mapOIDOfSelf: lpcr mapOIDOfSelf)
             lexicalParentCompilationRequester: lpcr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         isSlotAnOuterMethod = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         lookupType = ( |
            | 
            kleinAndYoda lookupType normal).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         objectDoingTheResend = bootstrap stub -> 'globals' -> 'nil' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         outerMethodSlot = ( |
            | 
            lexicalParentCompilationRequester outerMethodSlot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         selfMir = ( |
            | 
            lexicalParentCompilationRequester selfMir).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> 'parent' -> () From: ( | {
         'Category: displaying status\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         showCompilingSlot = ( |
            | 
            objectMapper showCompilingSlot: slot
                                   InBlock: rcvrMir
                           OuterMethodSlot: outerMethodSlot
                                      Self: selfMir).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'block' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         slot = ( |
            | 
            mySlot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         outerMethod = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda objectMapper1 parent compilationRequesterFor abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent compilationRequesterFor outerMethod.

CopyDowns:
globals kleinAndYoda objectMapper1 parent compilationRequesterFor abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         lookupType.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)'
        
         objectDoingTheResend.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent compilationRequesterFor outerMethod parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         compileIfNecessary = ( |
            | 
            "We always want to reach the block method through its outer method. -- Adam & Alex, 5/04"
            "We've changed the mapping algorithm to skip over the block's value slot; see" [compileSlotsForBlock: b MapOID: o].
            "So this check is no longer needed, but it seems worth leaving in as a safeguard anyway. -- Adam, 5/05"
            slot contents isReflecteeBlockMethod ifTrue: [thisCodeIsNeverReached. ^ self].

            resend.compileIfNecessary).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForReceiver: rMir TargetSlot: s OnBehalfOf: anObjectMapper = ( |
            | 
            (((copy
                       mySlot: s)
                   lookupType: kleinAndYoda lookupType normal)
                      rcvrMir: rMir)
                 objectMapper: anObjectMapper).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         isSlotAnOuterMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         outerMethodSlot = ( |
            | slot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         selfMir = ( |
            | 
            rcvrMir).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> 'parent' -> () From: ( | {
         'Category: displaying status\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         showCompilingSlot = ( |
            | 
            objectMapper showCompilingSlot: slot
                                 Receiving: rcvrMir).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'compilationRequesterFor' -> 'outerMethod' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         slot = ( |
            | 
            "optimization - slot lookup is expensive -- Adam & Alex, Feb. 2004"
            mySlot ifNil: [
              mySlot: (objectMapper policy slotFinder copyForMirror: rcvrMir Selector: mySelector) findSoleSlot.
            ].
            mySlot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationRequesterForOuterMethodOn: rMir TargetSlot: slot MapOID: rMapOID = ( |
            | 
            (compilationRequesterFor outerMethod
                                       copyForReceiver: rMir
                                            TargetSlot: slot
                                            OnBehalfOf: self)
                                            mapOIDOfReceiver: rMapOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compilationSubtimes = ( |
            | 
            ( (subtime copyNamed: 'Compilation'                         Ms: compilationTime)
            & (subtime copyNamed: 'Filling during compilation'          Ms: fillingDuringCompilationTime)
            & (subtime copyNamed: 'slotsToCompileForReceiver:'          Ms: slotsToCompileForReceiverTime)
            & (subtime copyNamed: 'resend detection'                    Ms: resendDetectionTime)
            & (subtime copyNamed: 'selectorsToCompile lookup'           Ms: selectorsToCompileLookupTime)
            & (subtime copyNamed: 'isReusable:'                         Ms: isReusableTime)
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
            exemplarMirrorsToCompileByMapOIDDo: [|:exemplarMir. :mapOID|
              compileSlotsForReceiverMirror: exemplarMir MapOID: mapOID.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compileSlot: slot ForReceiver: rMir MapOID: rMapOID = ( |
            | 
            (shouldCompileSlotNamed: slot name) ifTrue: [| compilationRequester |
              compilationRequester:
                compilationRequesterForOuterMethodOn: rMir
                                          TargetSlot: slot
                                              MapOID: rMapOID.
              compilationRequester compileIfNecessary.
              fillInObjects.
            ] False: [
              objectsOracle recordMapOID: rMapOID WithUncompiledSlotNamed: slot name.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compileSlotsForBlock: rMir MapOID: rMapOID = ( |
             d.
            | 

            "Optimization: We could just loop over cachedBlockNMethodOIDs and
             call addNMethod:MapOID: for all the entries except the one for
             the valueSlot, but all of these dictionaries are going
             to end up almost identical to cachedBlockNMethodOIDs. So it's
             faster (saves about 2-3% overall mapping time) to just copy
             cachedBlockNMethodOIDs and then fix the valueSlot entry.
             -- Adam, 2/05"

            foundFirstBlock ifFalse: [|slots|
              foundFirstBlock: true.
              valueSlotNameOfFirstBlock: rMir valueSlotName.
              slots:  slotsToCompileForReceiver: rMir.
              slots do: [|:slot|
                "The value slot method is compiled from compilationRequesterFor block compile
                 so that we have the lexical parent info. -- dmu 2/05"
                slot name = valueSlotNameOfFirstBlock ifFalse: [
                  compileSlot: slot ForReceiver: rMir MapOID: rMapOID.
                ].
              ].
            ] True: [
              foundSecondBlock ifFalse: [
                valueSlotNameOfFirstBlock = rMir valueSlotName ifFalse: [|slots. slot|
                  foundSecondBlock: true.
                  slots:  slotsToCompileForReceiver: rMir.
                  "Since we skipped over the first block's valueSlot, we still need to compile the
                   slot with that name. So grab a block that takes a different number of arguments
                   than the first block, and compile the slot with that name."
                  slot: traits block asMirror lookupSoleSlotNamed: valueSlotNameOfFirstBlock.
                  (slots includes: slot) ifTrue: [
                    compileSlot: slot ForReceiver: rMir MapOID: rMapOID.
                  ].
                ].
              ].
            ].

            d: cachedBlockNMethodOIDs copy.
            d removeKey:  (klein lookupKey copyForNormalSend: rMir valueSlotName) IfAbsent: [].

            objectsOracle
              nmethodOIDDictionaryForMapOID: rMapOID
                                  IfPresent: [|:nmOIDsAlreadyCompiledForThisBlock|
                                               "nmOIDsAlreadyCompiledForThisBlock should have size 1, except in the
                                                cases of the two blocks that we pre-compiled some nmethods for. So
                                                this addAll: operation is almost always cheap. -- Adam, 2/05"
                                               d addAll: nmOIDsAlreadyCompiledForThisBlock]
                                   IfAbsent: [].

            objectsOracle recordNMethodOIDDictionary: d ForMapOID: rMapOID.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         compileSlotsForReceiverMirror: rMir MapOID: rMapOID = ( |
             slots.
            | 

            "Blocks go through here so that we can deal with their (non-value) methods
             just like any other object. -- dmu 2/05"

            "Self does not have method objects, should not need any methods on methods -- dmu 2/05"
            rMir isReflecteeMethod ifTrue: [^ self].

            "Optimization: Blocks do not all share the same map, but they do share all the
             same nmethods except the value method. So we just keep a collection of block
             nmethods and then add all those nmethods to every block. -- Adam, 2/05"
            rMir isReflecteeBlock ifTrue: [
              compileSlotsForBlock: rMir MapOID: rMapOID.
              ^ self
            ].

            "Call slotsToCompileForReceiver: instead of slotsToCompileForReceiver:Do: just so that
             the profile will turn out cleaner. -- Adam, 2/05"
            slotsToCompileForReceiverTime: slotsToCompileForReceiverTime + [
              slots:  slotsToCompileForReceiver: rMir.
            ] cpuTime.

            slots do: [|:slot|
              compileSlot: slot ForReceiver: rMir MapOID: rMapOID.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            (((((((resend.copy
              objectsOracle:                             objectsOracleProto                       copy)
              oopsToFillIn:                              oopsToFillIn                             copy)
              cachedSlotsToCompileByReceiverMirror:      cachedSlotsToCompileByReceiverMirror     copy)
              cachedResendsBySlot:                       cachedResendsBySlot                      copy)
              cachedBlockNMethodOIDs:                    cachedBlockNMethodOIDs                   copy)
              cachedReusableNMethodOIDsBySlotAndKey:     cachedReusableNMethodOIDsBySlotAndKey    copy)
              canonicalizedStrings:                      canonicalizedStrings                     copy)
              kleinPrimitiveMirrorOops:                  kleinPrimitiveMirrorOops                 copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyPolicy: aVMExportPolicy ReportTo: reporter = ( |
            | 
            ((copy
              policy:                  aVMExportPolicy)
              statusReporter:          reporter)
              initializeCanonicalizedStrings).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         createAndRecordMappedNMethodCache: nmOIDs UsingMap: map MapOop: mapOop = ( |
             nmOIDVector.
             nmcOop.
            | 
            nmOIDs isEmpty ifTrue: [^ prototypeSimpleVectorOop].

            nmOIDVector: nmOIDs asVector.

            shouldOptimizeNMethodCacheCreation ifFalse: [ | nmc |
              nmc:  nmOIDVector copyMappedBy: [|:oid| originalObjectForOID: oid].
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
              nmc:    nmethodsAndOops copyMappedBy: [|:nmethodAndOop| nmethodAndOop x].
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
            oopForOldObjectTable: oopForOriginalObject: theVM objectLocator IfAbsent: nil.
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
              theVM objectLocator ifDirect: [] IfIndirect: [
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
                               InOop: (oopForOriginalObject: theVM)
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
            [theVM universe canonicalizedStrings: canonicalizedStrings]. "browsing"
            setLocalAndRemoteContentsOfAssignableSlotNamed: 'canonicalizedStrings'
                                                        In: theVM universe
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
            r add: theVM entryMethodName.

            [primitiveFailedError: '' Name: '']. "browsing"
            r add: 'primitiveFailedError:Name:'.

            theVM vmKit primitives stubAndFakePrimitiveSelectorsDo: [|:s|
              r add: s.
            ].

            objectsOracle methodMirrorsAllowingDuplicates do: [|:m|
              m literalsDo: [|:lit|
                (reflect: lit) isReflecteeString ifTrue: [
                  r add: lit.
                ].
              ].
            ].

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
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         exemplarMirrorsToCompileByMapOIDDo: blk = ( |
            | 
            immediateExemplarsDo: [|:immediateExemplar|
              (shouldCompileSlotsForImmediateExemplar: immediateExemplar) ifTrue: [| immMir |
                immMir:  reflect: immediateExemplar.
                blk value: immMir With: objectsOracle oidForOriginalObject: immMir vmKitMapForConversion.
              ].
            ].

            exemplarOIDsToCompile do: [|:exemplarOID. exemplarMir|
              exemplarMir:  objectsOracle mirrorForExemplarOID: exemplarOID.
              mapOIDForExemplarOID: exemplarOID
                         IfPresent: [|:mapOID| blk value: exemplarMir With: mapOID]
                          IfAbsent: raiseError.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         exemplarOIDsToCompile = ( |
             r.
             stubHolderOIDs.
            | 
            r: list copyRemoveAll.
            stubHolderOIDs: set copyRemoveAll.

            "Optimization: compile stub nmethods early on so that eager relocation
             will work for as many stub relocators as possible. -- Adam, 3/05"
            vm exportPolicy shouldIncludeKleinPrimitives ifTrue: [
              theVM vmKit primitives stubAndFakePrimitiveMethodHoldersDo: [|:h. oid|
                oid: objectsOracle oidForOriginalObject: h.
                stubHolderOIDs add: oid.
                r add: oid.
              ].
            ].

            objectsOracle exemplarOIDsDo: [|:exemplarOID|
              (stubHolderOIDs includes: exemplarOID) ifFalse: [
                r add: exemplarOID.
              ].
            ].

            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         expectedNumberOfObjects: n = ( |
            | 
            objectsOracle expectedNumberOfObjects: n.

            "Leave some extra room so that we don't have
             to grow it inside the image, for now."
            objectsOracle growOIDVectorsToAtLeast: n double + (theVM universe edenSpace sizeOfEntireRegion / (4 * oopSize)).

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: filling in\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         fillInObjects = ( |
             t.
            | 
            t: [
              [oopsToFillIn isEmpty] whileFalse: [fillInTheNextObject].
            ] cpuTime.
            isInCompilationPhase ifTrue: [fillingDuringCompilationTime: fillingDuringCompilationTime + t].
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

            mir vmKitMapForConversion
                            fillInObjectAt: oop
                                       OID: oid
                AndCreateMapForReflecteeOf: mir
                                    Mapper: self.
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

            numberOfLocalsThatNeedToBeInitialized:      numberOfLocalsThatNeedToBeInitialized      + cg numberOfLocalsThatNeedToBeInitialized.
            numberOfLocalsThatDoNotNeedToBeInitialized: numberOfLocalsThatDoNotNeedToBeInitialized + cg numberOfLocalsThatDoNotNeedToBeInitialized.

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
            nmcMapOop: mapOf: prototypeSimpleVectorOop.
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

            fill1Time: [fillInObjects. "do this now to ensure that all blocks have maps"] cpuTime.

            selectorsToCompileCreationTime: [createTableOfSelectorsToCompile] cpuTime.

            "This phase is probably just temporary. Once we fix the bug described in the
             comment in the allocateContentsOfSlotsToCompile method, we won't need this
             extra phase anymore. -- Adam, 6/05"
            unmappedSlotsTime: unmappedSlotsTime + [
              allocateContentsOfSlotsToCompile.
              fillInObjects.
              createTableOfSelectorsToCompile. "recreate it because we just mapped more stuff"
            ] cpuTime.

            compilationPhaseTime: [
              isInCompilationPhase: true.
              compileAll.
              isInCompilationPhase: false.
            ] cpuTime.
            fill2Time: [fillInObjects. "do not charge next routine for mapping the nmethods, etc."] cpuTime.
            nmethodCacheConstructionTime: [mapNMethodCaches] cpuTime.
            fill3Time: [fillInObjects.] cpuTime.

            stringTableTime: [createStringTable.] cpuTime.
            fill4Time: [fillInObjects. "just to be sure"] cpuTime.

            fixingKleinMirrorsTime: [fixReflecteeOopsAndMapsInKleinMirrors] cpuTime.
            [createNMethodTable]. [todo recompilation]. "Just for measuring -- breaks incremental update"

            [todo variableSizeHeaders. compactMapTableTime: [fixupCompactMapTable] cpuTime].

            objectTableTime: [createObjectTable.] cpuTime.
            fill5Time: [fillInObjects. "just to be sure"] cpuTime.

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
        
         nameOf: rMir = ( |
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
         'Category: copying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         objectsOracleProto = ( |
            | 
            vmKit objectsOracle).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: eager relocation\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oracleForEagerRelocation = ( |
            | 
            objectsOracle).
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
              '   - Unmapped slots hack time: '        , (asSeconds:              unmappedSlotsTime) printString, ' s\n',
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
              '   - Local initialization needed/unneeded ', numberOfLocalsThatNeedToBeInitialized      printString, '/',
                                                            numberOfLocalsThatDoNotNeedToBeInitialized printString, '\n',
              '   - SelectorsToCompile hits/misses: ', selectorsToCompileHits printString, '/', 
                                                      (selectorsToCompileProbes - selectorsToCompileHits) printString, '\n',
              '   - BlockNMethod$ hits/misses: ', blockNMethodHits printString, '/', blockNMethodMisses printString, '\n',
              '   - CachedResend  hits/misses: ', (cachedResendProbes - cachedResendMisses) printString, '/', cachedResendMisses printString, '\n',
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
         'Category: compiling\x7fCategory: caching nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         recordBlockNMethodOID: nmOID LookupKey: key = ( |
            | 
            "The lookup key is passed in just as a small optimization; we can find it
             this other way too, as the assertion shows. -- Adam, 5/05"
            assert: [(originalObjectForOID: nmOID) lookupKey = key].
            cachedBlockNMethodOIDs at: key Put: nmOID.
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
            cachedResendProbes: cachedResendProbes succ.
            cachedResendsBySlot at: aSlot IfAbsentPut: [
              cachedResendMisses: cachedResendMisses succ.
              resendDetector resendsInSlot: aSlot
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         reusableNMethodOIDForSlot: aSlot LookupKey: key IfAbsentPut: blk = ( |
             isAbsent <- bootstrap stub -> 'globals' -> 'false' -> ().
             r.
             t.
            | 
            t: [r: cachedReusableNMethodOIDsBySlotAndKey at: aSlot @ key 
                                                IfAbsentPut: [|:k| isAbsent: true.
                                                                   blk value: k   ]] cpuTime.
            isAbsent ifTrue: [
              reusableNMethodMisses:  reusableNMethodMisses succ.
            ] False: [
              reusableNMethodHits:    reusableNMethodHits   succ.
              reusableNMethodHitTime: reusableNMethodHitTime + t.
            ].
            r).
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
        
         shouldCompileSlotNamed: n = ( |
             b.
            | 
            "Optimization: Don't bother compiling any slot whose selector isn't
             called from any method inside Klein. -- Adam, 2/05"
            policy shouldOnlyCompileSelectorsCalledByMappedMethods ifFalse: [^ true].
            selectorsToCompileProbes: selectorsToCompileProbes succ.
            b:  objectsOracle selectorsToCompile includes: n.
            b ifTrue: [selectorsToCompileHits: selectorsToCompileHits succ].
            b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fCategory: immedates\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldCompileSlotsForImmediateExemplar: immediateExemplar = ( |
            | 
            true).
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
        
         showCompilingSlot: bSlot InBlock: bMir OuterMethodSlot: outerMethodSlot Self: selfMir = ( |
            | 
            show: ['Compiling: ', (nameOf: selfMir), ' ', outerMethodSlot key, ', [] ', bSlot key]
             Nth: compileCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         showCompilingSlot: rSlot Receiving: rMir = ( |
            | 
            show: ['Compiling: ', (nameOf: rMir), ' ', rSlot key] Nth: compileCount).
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
         'Category: compiling\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         slotsToCompileForReceiver: rMir = ( |
            | 
            cachedSlotsToCompileByReceiverMirror
                      at: rMir
             IfAbsentPut: [policy slotsToCompileForReceiver: rMir]).
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
         'Category: tracking objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedMirrorForOID: oid IfAbsent: ab = ( |
            | 
            objectsOracle kleinifiedMirrorForOID: oid IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> () From: ( | {
         'Category: tracking objects\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOIDForExemplarOID: oid IfPresent: pb IfAbsent: ab = ( |
            | 
            objectsOracle mapOIDForExemplarOID: oid IfPresent: pb IfAbsent: ab).
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
         'Category: oop offset\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         subtractOffsetFromOop: oop = ( |
            | 
            theVM objectLocator addOffset: heapOffset negate ToOop: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyContentsOfSimpleVector: v = ( |
             vOop.
            | 
            vOop: oopForOriginalObject: v.
            theVM assert: [(layouts objVector indexableSizeOf: vOop) = v size].
            v do: [|:x. :i|
              theVM assert: [(oopForOriginalObject: x) = (layouts objVector for: vOop IndexableAt: i)].
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
            theVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         policy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: transient state\x7fComment: Oop for a regular vector: size is 0, parent is
traits vector, no other named slots.\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         prototypeSimpleVectorOop.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         recordingNMethodsTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         resendDetectionTime <- 0.
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
         'Category: object mapper state\x7fComment: set to unity to see all well-known compiles, mappings, etc.\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (500)\x7fVisibility: private'
        
         showingFrequency <- 500.
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: compilation stats\x7fCategory: times\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         unmappedSlotsTime <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fCategory: transient state\x7fCategory: caches\x7fCategory: block nmethods\x7fModuleInfo: Module: vmKitObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         valueSlotNameOfFirstBlock.
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
            ((((((((((resend.copy
             oidsByOriginalObject:             oidsByOriginalObject             copy)
             oidsByOop:                        oidsByOop                        copy)
             exemplarOIDsByCanonicalMap:       exemplarOIDsByCanonicalMap       copy)
             immediateMapOIDsByExemplar:       immediateMapOIDsByExemplar       copy)
             originalMirrorsByOID:             originalMirrorsByOID             copy)
             kleinifiedMirrorsByOID:           kleinifiedMirrorsByOID           copy)
             selectorsToCompile:               selectorsToCompile               copy)
             mappedSlots:                      mappedSlots                      copy)
             codeSizeStatistics:               codeSizeStatistics               copy)
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
            (((((((((((resend.copy invalidateCachedItems
             oidsByOriginalObject:             oidsByOriginalObject             copyRemoveAll)
             oidsByOop:                        oidsByOop                        copyRemoveAll)
             exemplarOIDsByCanonicalMap:       exemplarOIDsByCanonicalMap       copyRemoveAll)
             immediateMapOIDsByExemplar:       immediateMapOIDsByExemplar       copyRemoveAll)
             originalMirrorsByOID:             originalMirrorsByOID             copyRemoveAll)
             kleinifiedMirrorsByOID:           kleinifiedMirrorsByOID           copyRemoveAll)
             selectorsToCompile:               selectorsToCompile               copyRemoveAll)
             mappedSlots:                      mappedSlots                      copyRemoveAll)
             codeSizeStatistics:               codeSizeStatistics               copyRemoveAll)
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
        
         exemplarOIDForMap: m = ( |
            | 
            exemplarOIDsByCanonicalMap
                    at: m 
              IfAbsent: [m isImmediate ifTrue: [error: 'should not be called for immediate maps']
                                        False: raiseError]).
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
         'Category: tracking\x7fCategory: maps and exemplars\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOIDForExemplar: e = ( |
             mir.
            | 
            mir:  reflect: e.
            mir isReflecteeKleinAndYodaImmediate
                    ifTrue: [oidForOriginalObject: mir vmKitMapForConversion]
                     False: [mapOIDForExemplarOID: oidForOriginalObject: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOIDForExemplarOID: exemplarOID IfPresent: pb IfAbsent: ab = ( |
             exemplarOop.
             mapOop.
            | 
            exemplarOop: oopForOID: exemplarOID IfAbsent: [^ ab value].
            mapOop: vmKit layouts object mapOf: exemplarOop IfFail: [^ ab value].
            pb value: oidForOop: mapOop IfAbsent: [^ ab value]).
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
         'Category: tracking\x7fCategory: maps and exemplars\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorForExemplarOID: exemplarOID = ( |
            | 
            kleinifiedMirrorForOID: exemplarOID).
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
            kleinifiedObjectForOID: oidForOriginalMirror: origMir).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> 'trackingObjects' -> () From: ( | {
         'Category: map OIDs by receiver\x7fModuleInfo: Module: vmKitObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOIDForExemplarOID: oid = ( |
            | 
            mapOIDForExemplarOID: oid IfPresent: [|:mapOID| mapOID] IfAbsent: raiseError).
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
