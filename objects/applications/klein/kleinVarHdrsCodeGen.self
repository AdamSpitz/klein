 '$Revision: 30.5 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: private'
        
         generateBasicValueFor: memObjReg Into: dstReg With: cg Layout: aLayout = ( |
            | 
            ifIndexForLayout: aLayout IsFixed: [|:i|
              aLayout generateFor: memObjReg AtConstant: i                                     Into: dstReg With: cg.
            ] IfVariesByOnlyOne: [|:possiblyAbsentField. :i|
              aLayout generateFor: memObjReg AtConstant: i PlusMaybeField: possiblyAbsentField Into: dstReg With: cg.
            ] IfVariesByMoreThanOne: [|:possiblyAbsentFields. :i|
              generateIndex: i For: memObjReg PlusMaybeFields: possiblyAbsentFields Into: dstReg With: cg Layout: aLayout.
              aLayout generateFor: memObjReg At: dstReg Into: dstReg With: cg.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: private'
        
         generateIfEncodedInMarkOf: memObjReg ThenAddImm: n To: indexReg With: cg Layout: aLayout = ( |
            | 
            cg generateExit: [|:encodedFork|
              cg withTemporaryRegisterDo: [|:tempReg|
                aLayout generateIfBitField: bitFieldInMark
                         IsEncodedInMarkOf: memObjReg
                         ThenPutValueInReg: tempReg
                               AndBranchTo: encodedFork
                                      With: cg.
              ].
              cg addImm: n MaybeSetCCFrom: indexReg To: indexReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: private'
        
         generateIndex: i For: memObjReg PlusMaybeFields: possiblyAbsentFields Into: indexReg With: cg Layout: aLayout = ( |
            | 
            cg loadOop: i IntoRegister: indexReg.
            possiblyAbsentFields asVector do: [|:f|
              f generateIfEncodedInMarkOf: memObjReg ThenAddImm: (vmKit layouts smi encode: 1) To: indexReg With: cg Layout: aLayout.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSetBasicValueFor: memObjReg To: valueReg With: cg Layout: aLayout = ( |
            | 
            ifIndexForLayout: aLayout IsFixed: [|:i|
              aLayout generateFor: memObjReg AtConstant: i                                     Put: valueReg With: cg.
            ] IfVariesByOnlyOne: [|:possiblyAbsentField. :i|
              aLayout generateFor: memObjReg AtConstant: i PlusMaybeField: possiblyAbsentField Put: valueReg With: cg.
            ] IfVariesByMoreThanOne: [|:possiblyAbsentFields. :i|
              cg withTemporaryRegisterDo: [|:indexReg|
                generateIndex: i For: memObjReg PlusMaybeFields: possiblyAbsentFields Into: indexReg With: cg Layout: aLayout.
                cg withTemporaryRegisterDo: [|:tempReg|
                  aLayout generateFor: memObjReg At: indexReg Put: valueReg Temp: tempReg With: cg.
                ].
              ].
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: private'
        
         generateValueForEncodedNumberInMark: numberEncodedInMarkReg Into: dstReg With: cg Layout: aLayout = ( |
            | 
            [todo optimization]. "Get the encoded number in the right register to begin with,
                                  so we don't have to move it."
            cg moveRegister: numberEncodedInMarkReg ToRegister: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: public'
        
         variableHeader_generateCopyValueFromObject: memObjReg1 ToObject: memObjReg2 With: cg Layout: aLayout = ( |
            | 
            (markMightEncodeMyValueForLayout: aLayout) ifFalse: [
              cg withTemporaryRegisterDo: [|:tempReg|
                generateBasicValueFor:    memObjReg1 Into: tempReg With: cg Layout: aLayout.
                generateSetBasicValueFor: memObjReg2   To: tempReg With: cg Layout: aLayout.
              ].
            ] True: [
              cg generateExit: [|:encodedFork|
                cg withTemporaryRegisterDo: [|:tempReg|
                  aLayout generateIfBitField: bitFieldInMark IsEncodedInMarkOf: memObjReg1 ThenPutValueInReg: tempReg AndBranchTo: encodedFork With: cg.
                ].
                cg withTemporaryRegisterDo: [|:tempReg|
                  generateBasicValueFor:    memObjReg1 Into: tempReg With: cg Layout: aLayout.
                  generateSetBasicValueFor: memObjReg2   To: tempReg With: cg Layout: aLayout.
                ].
              ].
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: public'
        
         variableHeader_generateSetValueFor: memObjReg To: valueReg With: cg Layout: aLayout = ( |
            | 
            (markMightEncodeMyValueForLayout: aLayout) ifFalse: [
              generateSetBasicValueFor: memObjReg To: valueReg With: cg Layout: aLayout.
            ] True: [
              aLayout
                generateIfBitField: bitFieldInMark
                 IsEncodedInMarkOf: memObjReg
                              Then: [error: 'setting the value encoded in the mark is difficult and not necessary so far - we do not handle this case yet']
                              Else: [generateSetBasicValueFor: memObjReg To: valueReg With: cg Layout: aLayout]
                              With: cg.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: public'
        
         variableHeader_generateValueFor: memObjReg Into: dstReg With: cg Layout: aLayout = ( |
            | 
            (markMightEncodeMyValueForLayout: aLayout) ifFalse: [
              generateBasicValueFor: memObjReg Into: dstReg With: cg Layout: aLayout.
            ] True: [
              aLayout
                generateIfBitField: bitFieldInMark
                 IsEncodedInMarkOf: memObjReg
                              Then: [|:numberEncodedInMarkReg| generateValueForEncodedNumberInMark: numberEncodedInMarkReg Into: dstReg With: cg Layout: aLayout]
                              Else: [generateBasicValueFor: memObjReg Into: dstReg With: cg Layout: aLayout]
                              With: cg.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: reading\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: memObjReg AtConstant: sizeWithoutField PlusMaybeField: f Into: dstReg With: cg = ( |
            | 
            [memObjReg != dstReg] assert.
            "Optimization: don't bother checking the mark if there's no chance
             that the mark encodes the field."
            (f markMightEncodeMyValueForLayout: self) ifFalse: [
              generateFor: memObjReg AtConstant: sizeWithoutField succ Into: dstReg With: cg.
            ] True: [
              cg generateIf: [|:encodedFork|
                          generateIfBitField: f bitFieldInMark
                           IsEncodedInMarkOf: memObjReg
                           ThenPutValueInReg: dstReg
                                 AndBranchTo: encodedFork
                                        With: cg.
              ] Then: [
                generateFor: memObjReg AtConstant: sizeWithoutField      Into: dstReg With: cg.
              ] Else: [
                generateFor: memObjReg AtConstant: sizeWithoutField succ Into: dstReg With: cg.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: writing\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: memObjReg AtConstant: sizeWithoutField PlusMaybeField: f Put: dataObjReg With: cg = ( |
            | 
            "Optimization: don't bother checking the mark if there's no chance
             that the mark encodes the field."
            (f markMightEncodeMyValueForLayout: self) ifFalse: [
              generateFor: memObjReg AtConstant: sizeWithoutField succ Put: dataObjReg With: cg.
            ] True: [
              cg generateIf: [|:encodedFork|
                cg withTemporaryRegisterDo: [|:tempReg|
                            generateIfBitField: f bitFieldInMark
                             IsEncodedInMarkOf: memObjReg
                             ThenPutValueInReg: tempReg
                                   AndBranchTo: encodedFork
                                          With: cg.
                ].
              ] Then: [
                generateFor: memObjReg AtConstant: sizeWithoutField      Put: dataObjReg With: cg.
              ] Else: [
                generateFor: memObjReg AtConstant: sizeWithoutField succ Put: dataObjReg With: cg.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: private'
        
         generateIfBitField: f IsEncodedInMarkOf: memObjReg Then: encodedBlk Else: notEncodedBlk With: cg = ( |
            | 
            cg generateExit: [|:endFork|
              cg generateExit: [|:notEncodedFork|
                cg withTemporaryRegisterDo: [|:tempReg|
                  generateIfBitField: f IsEncodedInMarkOf: memObjReg ThenPutValueInReg: tempReg ElseBranchTo: notEncodedFork With: cg.
                  encodedBlk value: tempReg.
                  cg a branchToLabel: endFork.
                ].
              ].
              notEncodedBlk value.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: private'
        
         generateIfBitField: f IsEncodedInMarkOf: memObjReg ThenPutValueInReg: dstReg AndBranchTo: encodedFork With: cg = ( |
            | 
            generateNumberInBitField: f InMarkOf: memObjReg Into: dstReg With: cg.

            cg            generateIf: dstReg
               DoesNotEqualImmediate: (layouts smi encode: f numberMeaningUnencodable)
                  ThenLikelyBranchTo: encodedFork.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: private'
        
         generateIfBitField: f IsEncodedInMarkOf: memObjReg ThenPutValueInReg: dstReg ElseBranchTo: encodedFork With: cg = ( |
            | 
            generateNumberInBitField: f InMarkOf: memObjReg Into: dstReg With: cg.

            cg           generateIf: dstReg
                    EqualsImmediate: (layouts smi encode: f numberMeaningUnencodable)
               ThenUnlikelyBranchTo: encodedFork.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: private'
        
         generateNumberInBitField: f InMarkOf: memObjReg Into: dstReg With: cg = ( |
            | 
            generateMarkOf: memObjReg Into: dstReg With: cg.
            f generateNumberForWord: dstReg
               WhichIsShiftedLeftBy: vmKit tag size
                               Into: dstReg
                               With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( | {
         'Category: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: public'
        
         generateValueForEncodedNumberInMark: compactMapIndexReg Into: dstReg With: cg Layout: aLayout = ( |
            | 
            cg withTemporaryRegisterDo: [|:compactMapsVectReg. magicNumber_indexableOriginOfCompactMapVector = 3 |
              loadCompactMapVectorInto: compactMapsVectReg With: cg.
              [todo cleanup variableSizeHeaders]. "AAAAAAAAAAAAA!!! HACK! HACK! I happen to know that the indexable origin
                                                   of the compactMaps vector is 3, and calculating it would be slow, plus
                                                   I think maybe we run out of registers when we try to calculate it. So
                                                   I hard-coded it."
              error: 'Fix this hack before proceeding.'.
              vmKit layouts objVector generateFor: compactMapsVectReg
                                      IndexableAt: compactMapIndexReg
                AssumingIndexableOriginIsConstant: magicNumber_indexableOriginOfCompactMapVector
                                             Into: dstReg
                                             With: cg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( | {
         'Category: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: private'
        
         loadCompactMapVectorInto: dstReg With: cg = ( |
            | 
            cg loadOop: theVM universe compactMaps IntoRegister: dstReg NameForComment: 'theVM universe compactMaps'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fCategory: variable-size headers\x7fModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: objVectReg IndexableAt: indexSmiReg AssumingIndexableOriginIsConstant: origin Into: dstObjReg With: cg = ( |
            | 
            cg addImm: (layouts smi encode: origin) MaybeSetCCFrom: indexSmiReg To: dstObjReg.
            generateFor: objVectReg At: dstObjReg Into: dstObjReg With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot'
        
         kleinVarHdrsCodeGen = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVarHdrsCodeGen' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVarHdrsCodeGen' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinVarHdrsCodeGen.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVarHdrsCodeGen' -> () From: ( | {
         'ModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVarHdrsCodeGen' -> () From: ( | {
         'ModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVarHdrsCodeGen' -> () From: ( | {
         'ModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVarHdrsCodeGen' -> () From: ( | {
         'ModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVarHdrsCodeGen' -> () From: ( | {
         'ModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.5 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVarHdrsCodeGen' -> () From: ( | {
         'ModuleInfo: Module: kleinVarHdrsCodeGen InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinVarHdrsCodeGen postFileIn
