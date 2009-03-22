 '$Revision: 30.5 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: layout constants\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: public'
        
         assumingOriginIsEncodableCalculateIndexableOriginOfReflecteeOf: m Mapper: mapper = ( |
            | 
            [todo optimization]. "We're calculating whether the size is encodable or
             not twice - once for the size field itself, and once here."

              (   lastHeaderField     indexAfterMeForReflecteeOf: m Layout: self Mapper: mapper)
            + (indexableSizeField shouldBeIncludedForReflecteeOf: m Layout: self Mapper: mapper) asInteger
            + (                objectSlotWordCountForReflecteeOf: m              Mapper: mapper)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableOriginField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: private'
        
         valueIfNotEncodedInMarkForValue: valueThatWouldHaveBeenEncodedInTheMark = ( |
            | 
            "Since the origin turned out not to be encodable, it takes up one
             extra word in the object, and so the origin needs to be one
             higher than it would have been if it had been able to be encoded
             in the mark."
            valueThatWouldHaveBeenEncodedInTheMark succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableOriginField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: private'
        
         valueToBeEncodedInMarkForReflecteeOf: m Layout: aLayout Mapper: mapper = ( |
            | 
            aLayout assumingOriginIsEncodableCalculateIndexableOriginOfReflecteeOf: m Mapper: mapper).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableSizeField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: private'
        
         valueToBeEncodedInMarkForReflecteeOf: m Layout: aLayout Mapper: mapper = ( |
            | 
            m reflecteeSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: private'
        
         canMarkEncodeReflecteeOf: m Mapper: mapper = ( |
            | 
            [todo cleanup variableSizeHeaders]. "This method has a terrible name. And too many methods around here."
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBeIncludedForReflecteeOf: m Layout: aLayout Mapper: mapper = ( |
             markNumberIfEncodable.
             myValueIfEncodable.
            | 
            [todo cleanup variableSizeHeaders]. "Duplication with the isShouldBeIncluded... method."
            (markMightEncodeMyValueForLayout: aLayout Policy: mapper policy) ifFalse: [^ true].
            myValueIfEncodable: valueToBeEncodedInMarkForReflecteeOf: m Layout: aLayout Mapper: mapper.
            markNumberIfEncodable: markNumberForValue: myValueIfEncodable Mapper: mapper.
            (bitFieldInMark isNumberEncodable: markNumberIfEncodable) not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: private'
        
         valueIfNotEncodedInMarkForValue: valueThatWouldHaveBeenEncodedInTheMark = ( |
            | 
            valueThatWouldHaveBeenEncodedInTheMark).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: private'
        
         valueToBeEncodedInMarkForReflecteeOf: m MapOop: mapOop Layout: aLayout Mapper: mapper = ( |
            | 
            [todo cleanup variableSizeHeaders].
            "God, this is convoluted. This method needs to take the mapOop because we need
             to return it if this is mapField. But we also need the version of this method
             without the mapOop, because we call it from shouldBeIncludedForReflecteeOf:Layout:Mapper:."

            valueToBeEncodedInMarkForReflecteeOf: m Layout: aLayout Mapper: mapper).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: public'
        
         variableHeader_ifShouldBeIncludedForReflecteeOf: m MapOop: mapOop Layout: aLayout Mapper: mapper AdjustMarkValue: initialMarkValue AndDo: blk IfNotEncodedSetValueFor: o = ( |
             hasMarkField.
             isEncodedInMark.
             markNumberIfEncodable.
             mv.
             myValueIfEncodable.
             r.
            | 
            [todo cleanup variableSizeHeaders]. "This method is taking so many args. Maybe there's an object hiding here?"
            mv: initialMarkValue.
            hasMarkField: markMightEncodeMyValueForLayout: aLayout Policy: mapper policy.
            [todo cleanup variableSizeHeaders]. "This next line is out of place. No reason to get the value to be encoded if there's no mark field
                                                 - should just get the value to go in the header."
            myValueIfEncodable: valueToBeEncodedInMarkForReflecteeOf: m MapOop: mapOop Layout: aLayout Mapper: mapper.
            hasMarkField ifFalse: [
              isEncodedInMark: false.
            ] True: [
              (canMarkEncodeReflecteeOf: m Mapper: mapper) ifFalse: [
                isEncodedInMark: false.
              ] True: [
                markNumberIfEncodable: markNumberForValue: myValueIfEncodable Mapper: mapper.
                isEncodedInMark: bitFieldInMark isNumberEncodable: markNumberIfEncodable.
              ]
            ].
            hasMarkField ifTrue: [mv: bitFieldInMark setValueOfWord: mv To: bitFieldInMark valueForNumber: isEncodedInMark ifTrue: [markNumberIfEncodable]
                                                                                                                            False: [bitFieldInMark numberMeaningUnencodable]].
            r: blk value: mv.
            isEncodedInMark ifFalse: [
              [todo optimization]. "Could call setBasicValue directly, but calling setValue is an extra safety check
                                    to make sure that the mark value got set properly by blk."
              setValueFor: o To: (valueIfNotEncodedInMarkForValue: myValueIfEncodable) Layout: aLayout IfFail: raiseError.
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: public'
        
         variableHeader_indexAfterMeForReflecteeOf: m Layout: aLayout Mapper: mapper = ( |
             n <- 0.
            | 
            reverseDo: [|:f| n: n + (f shouldBeIncludedForReflecteeOf: m Layout: aLayout Mapper: mapper) asInteger].
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: private'
        
         canMarkEncodeReflecteeOf: m Mapper: mapper = ( |
            | 
            mapper policy shouldBeCompact: m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: private'
        
         markNumberForValue: mapOop Mapper: mapper = ( |
            | 
            mapper vm universe indexForCompactMap: (mapper originalObjectForOop: mapOop)
                                          WithOop: mapOop
                                           IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBeIncludedForReflecteeOf: m Layout: aLayout Mapper: mapper = ( |
            | 
            "Don't bother checking the number; we don't know what the compact map index
             will be yet, and in any case I want the mapping process to fail if we run
             out of compact map indices (at least for now), so that we can decide what
             to do about it (increase the size of the compact map table, or whatever).
             I don't want the export cycle to just continue and silently stop making
             objects compact when they really should be. -- Adam, 11/05"
            (mapper policy shouldBeCompact: m) not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: private'
        
         valueToBeEncodedInMarkForReflecteeOf: m MapOop: mapOop Layout: aLayout Mapper: mapper = ( |
            | 
            mapOop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: public'
        
         variableHeader_emptyObjectSizeForReflecteeOf: m Mapper: mapper = ( |
            | 
            lastField indexAfterMeForReflecteeOf: m Layout: self Mapper: mapper).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: private'
        
         fixupCompactMapTable = ( |
             t.
             tMap.
             tOop.
            | 
            t: vm universe compactMaps.
            tOop: oopForOriginalObject: t.
            tMap: maps map importMapFor: tOop IfFail: raiseError.
            [tMap isVector] assert.
            t do: [|:m. :i| tMap for: tOop IndexableAt: i Put: oopForOriginalObject: m].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot'
        
         vmKitVarHdrsObjMapr = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsObjMapr' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsObjMapr' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitVarHdrsObjMapr.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsObjMapr' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsObjMapr' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsObjMapr' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsObjMapr' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsObjMapr' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.5 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsObjMapr' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsObjMapr InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitVarHdrsObjMapr postFileIn
