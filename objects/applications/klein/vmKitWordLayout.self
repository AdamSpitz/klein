 '$Revision: 30.7 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: word layout\x7fModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         wordLayoutMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda wordLayoutMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> () From: ( | {
         'Category: prototypes\x7fModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractBooleanField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda wordLayoutMixin abstractBooleanField.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         clearBitInWord: w = ( |
            | 
            setValueOfWord: w To: false asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         defaultValue = ( |
            | 
            false asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         isValueOfWordTrue: w = ( |
            | 
            "Use the primitive to avoid cloning. -- Adam, Mar. 2009"
            (inPlaceValueOfWord: w) _IntNE: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> () From: ( | {
         'Category: prototypes\x7fModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda wordLayoutMixin abstractField.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         setBitInWord: w = ( |
            | 
            setValueOfWord: w To: true asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForObject: o = ( |
            | 
            (booleanValueForObject: o) asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         width = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         wordForBoolean: b = ( |
            | 
            wordForValue: b asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         wordForObject: o = ( |
            | 
            wordForBoolean: booleanValueForObject: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         allOnesValue = ( |
            | 
            "Optimization - using primitives to avoid block cloning. -- Adam, 8/06"
            (1 << width) _IntSub: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         defaultValue = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         fieldsDo: blk = ( |
             f.
            | 
            f: self.
            [blk value: f.
             f precedingField isNoField] whileFalse: [f: f precedingField].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         fieldsReverseDo: blk = ( |
            | 
            precedingField fieldsReverseDo: blk.
            blk value: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         inPlaceValueOfWord: w = ( |
            | 
            w && mask).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = ( |
            | 
            vmKit layouts abstract intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         isNoField = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         mask = ( |
            | 
            wordForValue: allOnesValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         name = ( |
            | 
            asMirror creatorSlotHint name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'Category: sentinel for the chain of fields\x7fModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         noPrecedingField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> 'noPrecedingField' -> () From: ( |
             {} = 'Comment: I kinda like ending the chain of fields with this sentinel object,
rather than doing isNil checks all the time. (Saves us the trouble
of creating the ifNil: blocks, too, which might be handy until we
teach Klein and Yoda how to optimize them away.) -- Adam, 4/06\x7fModuleInfo: Creator: globals kleinAndYoda wordLayoutMixin abstractField noPrecedingField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> 'noPrecedingField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         fieldsReverseDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> 'noPrecedingField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         isNoField = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> 'noPrecedingField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> 'noPrecedingField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         shiftPast = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         setValueOfWord: w To: v = ( |
            | 
            (w && mask complement) || (wordForValue: v)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         shift = ( |
            | 
            precedingField shiftPast).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         shiftPast = ( |
            | 
            "Uses primitive directly to avoid cloning blocks,
             so that this code can be called from the Klein
             cloning code. -- Adam, 5/06"

            shift _IntAdd: width).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         valueOfWord: w = ( |
            | 
            (inPlaceValueOfWord: w) >> shift).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            kleinAndYoda).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         wordForValue: v = ( |
            | 
            v << shift).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> () From: ( | {
         'Category: prototypes\x7fModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractNumberField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda wordLayoutMixin abstractNumberField.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         defaultValue = ( |
            | 
            valueMeaningUnencodable).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         encodableNumberCount = ( |
            | 
            (1 << width) pred).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         highestEncodableNumber = ( |
            | 
            lowestEncodableNumber + encodableNumberCount pred).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         ifWord: w HasValidNumberThen: aBlock Else: elseBlock = ( |
             v.
            | 
            v: valueOfWord: w.
            v = valueMeaningUnencodable
              ifTrue: elseBlock
               False: [aBlock value: numberForValue: v]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         isNumberEncodable: n = ( |
            | 
            (n >= lowestEncodableNumber) && [n <= highestEncodableNumber]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'Comment: Children can override.\x7fModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         lowestEncodableNumber = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         numberForValue: v = ( |
            | 
            lowestEncodableNumber + v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         numberMeaningUnencodable = ( |
            | 
            numberForValue: valueMeaningUnencodable).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForNumber: n = ( |
            | 
            (isNumberEncodable: n)
              ifTrue: [n - lowestEncodableNumber]
               False: [valueMeaningUnencodable]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         valueMeaningUnencodable = ( |
            | 
            encodableNumberCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         wordForNumber: n = ( |
            | 
            wordForValue: valueForNumber: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         fieldsDo: blk = ( |
            | 
            lastField fieldsDo: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         fieldsReverseDo: blk = ( |
            | 
            lastField fieldsReverseDo: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         formatString = ( |
             s <- 'Format:   '.
            | 
            fieldsDo: [|:f|
              s: s & '<' & f width printString & ': ' & (f name copyWithoutSuffix: 'Field') & '> '.
            ].
            s flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot'
        
         vmKitWordLayout = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitWordLayout' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitWordLayout' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitWordLayout.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitWordLayout' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitWordLayout' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitWordLayout' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitWordLayout' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitWordLayout' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitWordLayout' -> () From: ( | {
         'ModuleInfo: Module: vmKitWordLayout InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitWordLayout postFileIn
