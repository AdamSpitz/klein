 '$Revision: 30.6 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBlockMapsBeCompact = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBlocksBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldMarkEncodeObjVectorIndexableOrigin = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldMarkEncodeObjVectorIndexableSize = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldMethodMapsBeCompact = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldStringsBeCompact = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldVectorsBeCompact = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableOriginField' -> () From: ( | {
         'Category: accessing value\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         bitFieldInMark = ( |
            | 
            vmKit layouts mark objVectorIndexableOriginField).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableOriginField' -> () From: ( | {
         'Category: accessing value\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         markMightEncodeMyValueForLayout: aLayout Policy: policy = ( |
            | 
            policy shouldMarkEncodeObjVectorIndexableOrigin).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableSizeField' -> () From: ( | {
         'Category: accessing value\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         bitFieldInMark = ( |
            | 
            vmKit layouts mark objVectorIndexableSizeField).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableSizeField' -> () From: ( | {
         'Category: accessing value\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         markMightEncodeMyValueForLayout: aLayout Policy: policy = ( |
            | 
            policy shouldMarkEncodeObjVectorIndexableSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( | {
         'Category: accessing\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         headerSizeWithNoMap = ( |
            | 
            mapIndex).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( | {
         'Category: testing\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         markMightEncodeMapForPolicy: policy = ( |
            | 
            policy shouldBlocksBeCompact).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( | {
         'Category: accessing\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         numberOfSpecialFieldsAfterHeader = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( | {
         'Category: accessing\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         variableHeader_numberOfWordsInABlock = ( |
            | 
            "We're maintaining the invariant that all blocks are the same size."
              headerSizeWithNoMap
            + theVM exportPolicy shouldBlocksBeCompact not asInteger
            + numberOfSpecialFieldsAfterHeader
            + theVM exportPolicy shouldBlockValueSlotsBeObjectSlots asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: indices\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         ifIndexAfterMeForLayout: aLayout IsFixed: fixedBlock IfVariesByOnlyOne: variesByOneBlock IfVariesByMoreThanOne: variesByMoreBlock = ( |
             b.
            | 
            b: markMightEncodeMyValueForLayout: aLayout.

                 ifIndexForLayout: aLayout
                          IsFixed: [|     :i| b ifTrue: [ variesByOneBlock value:      self With: i     ]
                                                 False: [       fixedBlock value:                 i succ]]
                IfVariesByOnlyOne: [|:f . :i| b ifTrue: [variesByMoreBlock value: f  & self With: i     ]
                                                 False: [ variesByOneBlock value: f         With: i succ]]
            IfVariesByMoreThanOne: [|:fs. :i| b ifTrue: [variesByMoreBlock value: fs & self With: i     ]
                                                 False: [variesByMoreBlock value: fs        With: i succ]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: indices\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         ifIndexForLayout: aLayout IsFixed: fixedBlock IfVariesByOnlyOne: variesByOneBlock IfVariesByMoreThanOne: variesByMoreBlock = ( |
            | 
            precedingField ifIndexAfterMeForLayout: aLayout
                                           IsFixed: fixedBlock
                                 IfVariesByOnlyOne: variesByOneBlock
                             IfVariesByMoreThanOne: variesByMoreBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         ifMarkOf: o EncodesMyValueThen: aBlock Else: elseBlock Layout: aLayout IfFail: fb = ( |
            | 
            (markMightEncodeMyValueForLayout: aLayout) ifFalse: [^ elseBlock value].

            bitFieldInMark
                           ifWord: (aLayout markValueOf: o IfFail: [|:e| ^ fb value: e])
               HasValidNumberThen: [|:n| aBlock value: valueForEncodedNumberInMark: n]
                             Else: elseBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: indices\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         isIncludedIn: o Layout: aLayout = ( |
            | 
            ifMarkOf: o EncodesMyValueThen: false Else: true Layout: aLayout IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: testing\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         markMightEncodeMyValueForLayout: aLayout = ( |
            | 
            markMightEncodeMyValueForLayout: aLayout Policy: theVM exportPolicy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: testing\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         markMightEncodeMyValueForLayout: aLayout Policy: policy = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         markNumberForValue: myValueIfEncodable Mapper: mapper = ( |
            | 
            myValueIfEncodable).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> 'noPrecedingField' -> () From: ( | {
         'Category: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         ifIndexAfterMeForLayout: aLayout IsFixed: fixedBlock IfVariesByOnlyOne: variesByOneBlock IfVariesByMoreThanOne: variesByMoreBlock = ( |
            | 
            fixedBlock value: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         valueForEncodedNumberInMark: n = ( |
            | 
            [todo cleanup variableSizeHeaders].
            "We've got so many terms - value, number, and there's also
             value in the mark... make this clearer and maybe simpler."

            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: indices\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         variableHeader_indexAfterMeFor: o Layout: aLayout = ( |
            | 
              (indexFor: o Layout: aLayout)
            + (isIncludedIn: o Layout: aLayout) asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         variableHeader_setValueFor: o To: v Layout: aLayout IfFail: fb = ( |
            | 
                      ifMarkOf: o
            EncodesMyValueThen: [|:v| error: 'this is difficult - we would have to create a whole new object with the extra field']
                          Else: [setBasicValueFor: o At: (offsetFor: o Layout: aLayout) To: v Layout: aLayout IfFail: fb]
                        Layout: aLayout
                        IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         variableHeader_valueFor: o Layout: aLayout IfFail: fb = ( |
            | 
                      ifMarkOf: o
            EncodesMyValueThen: [|:v| v]
                          Else: [basicValueFor: o At: (offsetFor: o Layout: aLayout) Layout: aLayout IfFail: fb]
                        Layout: aLayout
                        IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( | {
         'Category: accessing value\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         bitFieldInMark = ( |
            | 
            vmKit layouts mark compactMapIndexField).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( | {
         'Category: accessing value\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         markMightEncodeMyValueForLayout: aLayout Policy: policy = ( |
            | 
            aLayout markMightEncodeMapForPolicy: policy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( | {
         'Category: accessing value\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         valueForEncodedNumberInMark: n = ( |
            | 
            theVM universe compactMapForIndex: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: testing\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         markMightEncodeMapForPolicy: policy = ( |
            | 
            "This is a conservative test; it can be overridden by children
             when the type of object is known."
            policy shouldAnyObjectsBeCompact).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: testing\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         markMightEncodeMapForPolicy: policy = ( |
            | 
            policy shouldVectorsBeCompact || [policy shouldAnyMapsBeCompact]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         isBlockMap: m = ( |
            | 
            isReflecteeOf: m AMapWhoseParentSatisfies: [|:pMir| pMir = (reflect: kleinAndYoda maps blockMap parent)]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         isReflecteeOf: m AMapWhoseParentSatisfies: blk = ( |
            | 
                m isReflecteeVector
            && [|pMir|
                pMir: m primitiveContentsAt: 'parent' IfFail: [reflect: 0].
                blk value: pMir]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         isVector: m = ( |
            | 
                m isReflecteeVector
            && [(m primitiveContentsAt: 'parent' IfFail: [reflect: 0]) = (reflect: traits vector)]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldAnyMapsBeCompact = ( |
            | 
            shouldBlockMapsBeCompact || [shouldMethodMapsBeCompact]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldAnyObjectsBeCompact = ( |
            | 
                shouldBlocksBeCompact
            || [shouldBlockMapsBeCompact
            || [shouldStringsBeCompact
            || [shouldVectorsBeCompact
            || [shouldMethodMapsBeCompact]]]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBeCompact: aKleinifiedMirror = ( |
            | 
                 (shouldBlocksBeCompact         && [aKleinifiedMirror isReflecteeBlock ])
            || [ (shouldStringsBeCompact        && [aKleinifiedMirror isReflecteeString])
            || [ (shouldVectorsBeCompact        && [isVector:    aKleinifiedMirror     ])
            || [ (shouldBlockMapsBeCompact      && [isBlockMap:  aKleinifiedMirror     ])
            || [  shouldActivationMapsBeCompact && [aKleinifiedMirror s holder isReflecteeVMKitActivationMap ]]]]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBlockMapsBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBlocksBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldMarkEncodeObjVectorIndexableOrigin = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldMarkEncodeObjVectorIndexableSize = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldMethodMapsBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldStringsBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: variable-size headers\x7fCategory: compact maps\x7fModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldVectorsBeCompact = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot'
        
         vmKitVarHdrs = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrs' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitVarHdrs.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrs' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrs' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrs InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrs' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrs' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrs' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.6 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrs' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrs InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitVarHdrs postFileIn
