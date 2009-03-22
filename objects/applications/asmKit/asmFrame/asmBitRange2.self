 '$Revision: 30.14 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: bit ranges\x7fCategory: concrete\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         compoundBitRange = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework bitRange copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework compoundBitRange.

CopyDowns:
globals assemblerSystems framework bitRange. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: InitializeToExpression: (vector)'
        
         mySubranges <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework compoundBitRange parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         < x = ( |
            | 
            numberOfBitsToMyLeft < x numberOfBitsToMyLeft).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | 
            subranges = x subranges).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: extracting\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         asSignedIntegerFrom: word = ( |
             r.
            | 
            r: subranges first asSignedIntegerFrom: word.
            subranges copyWithoutFirst do: [|:s|
              r: s intNN shl: r With: s width.
              r: s intNN  or: r With: s asUnsignedIntegerFrom: word
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: extracting\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         asUnsignedIntegerFrom: word = ( |
             r <- 0.
            | 
            subranges do: [|:s|
              r: s intNN shl: r With: s width.
              r: s intNN  or: r With: s asUnsignedIntegerFrom: word
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: private'
        
         fromUncheckedInteger: i IsSigned: isSigned = ( |
             ii.
             r <- 0.
            | 
            ii: i.
            subranges reverseDo: [|:s|
              r: s intNN or: r
                        With: s fromUnsignedInteger: (s intNN and: ii With: s mask).
              ii: isSigned ifTrue: [ s intNN  shr: ii With: s width ]
                            False: [ s intNN ushr: ii With: s width ]
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         fromUncheckedSignedInteger: i = ( |
            | 
            fromUncheckedInteger: i IsSigned: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         fromUncheckedUnsignedInteger: i = ( |
            | 
            fromUncheckedInteger: i IsSigned: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            subranges hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: word size (override for other sizes)\x7fComment: set to int32 or int64\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = ( |
            | 
            subranges first intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         maskInPlace = ( |
             r <- 0.
            | 
            subranges do: [|:s|
              r: intNN or: r With: s maskInPlace.
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfBitsToMyLeft = ( |
            | 
            subranges first numberOfBitsToMyLeft).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfBitsToMyRight = ( |
            | 
            subranges last numberOfBitsToMyRight).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         subranges = ( |
            | mySubranges).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         subranges: subs = ( |
             r.
            | 
            "bits of operand are distributed among subs, L to R"
            r: copy mySubranges: subs asVector.
            r asMirror at: 'mask' PutContents: r computeMask asMirror.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: word size (override for other sizes)\x7fComment: override if notation is relative to some other number\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         totalNumberOfBits = ( |
            | 
            subranges first totalNumberOfBits).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'compoundBitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         width = ( |
            | 
            (subranges copyMappedBy: [|:r| r width]) sum).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: bit ranges\x7fCategory: concrete\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         omittedBitRange = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework bitRange copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework omittedBitRange.

CopyDowns:
globals assemblerSystems framework bitRange. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot'
        
         numberOfBits = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework omittedBitRange parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | 
            numberOfBits = x numberOfBits).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: extracting\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         asSignedIntegerFrom: word = ( |
            | 
            0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: extracting\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         asUnsignedIntegerFrom: word = ( |
            | 
            0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         create: numberOfBits = ( |
             r.
            | 
            r: copy.
            r asMirror at: 'numberOfBits' PutContents: numberOfBits  asMirror.
            r asMirror at: 'mask'         PutContents: r computeMask asMirror.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         fromUncheckedSignedInteger: i = ( |
            | 
            intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         fromUncheckedUnsignedInteger: i = ( |
            | 
            intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: word size (override for other sizes)\x7fComment: set to int32 or int64\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = bootstrap stub -> 'globals' -> 'int32' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         maskInPlace = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfBitsToMyLeft = ( |
            | 
            0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfBitsToMyRight = ( |
            | 
            0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         subranges = ( |
            | vector copyAddFirst: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: word size (override for other sizes)\x7fComment: override if notation is relative to some other number\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         totalNumberOfBits = 32.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         width = ( |
            | numberOfBits).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: bit ranges\x7fCategory: abstract\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         simpleBitRange = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework bitRange copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework simpleBitRange.

CopyDowns:
globals assemblerSystems framework bitRange. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: InitializeToExpression: (0)'
        
         firstBit <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: InitializeToExpression: (0)'
        
         lastBit <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: InitializeToExpression: (1)'
        
         maskInPlace <- 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( |
             {} = 'Comment: I am abstract.\x7fModuleInfo: Creator: globals assemblerSystems framework simpleBitRange parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | (firstBit = x firstBit) && [lastBit = x lastBit]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: extracting\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         asSignedIntegerFrom: word = ( |
             max.
             u.
            | 
            u: asUnsignedIntegerFrom: word.
            max: intNN shl: 1 With: width pred.
            ((intNN lt: u With: max)
             ifTrue: [u]
              False: [intNN sub: (intNN sub: u With: mask) With: 1]
            ) asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: extracting\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         asUnsignedIntegerFrom: word = ( |
            | 
            intNN  and: (intNN ushr: word With: numberOfBitsToMyRight)
                  With: mask).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: creating\x7fComment: bits is a series of from-to pairs (inclusive)
or negative numbers.
Example: bits: 2 & 4 & 30 & 31 & -2 & 18 & 18 (may be a collector)
 2 &  4: the first 3 bits are stored in bit 2 to 4.
30 & 31: the next two bits are stored in bit 30 and 31.
     -2: the next 2 bits are discarded.
18 & 18: the next bit is stored in bit 18.

\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         bits: bits = ( |
             append.
             omit.
             previous.
             r.
            | 

            append: [|:block| r
                ifNil:     [r:    block value]
                IfNotNil:  [r: r, block value]
            ].

            bits asVector do: [|:n| (0 > n)
                ifTrue: [append value: [omittedBitRange create: n absoluteValue]]
                False:  [previous
                             ifNil:     [previous: n]
                             IfNotNil:  [
                                 append value: [from: previous To: n].
                                 previous: nil.
                             ]
                ]
            ].
            [previous isNil   ] assert.
            [r        isNotNil] assert.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: private'
        
         computeMaskInPlace = ( |
            | 
            intNN shl: computeMask With: numberOfBitsToMyRight).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         from: first To: last = ( |
             r.
            | 
            r: copy.
            r firstBit: first.
            r  lastBit: last.
            "next two depend on last two"
            r mask:        r computeMask.
            r maskInPlace: r computeMaskInPlace.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         fromUncheckedSignedInteger: i = ( |
            | 
            intNN shl: (intNN and: i With: mask) With: numberOfBitsToMyRight).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         fromUncheckedUnsignedInteger: i = ( |
            | 
            intNN shl: i With: numberOfBitsToMyRight).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | firstBit ^^ lastBit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: word size (override for other sizes)\x7fComment: set to int32 or int64\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = bootstrap stub -> 'globals' -> 'int32' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: private'
        
         omittedBitRange = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'omittedBitRange' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: private'
        
         statePrintString = ( |
            | firstBit printString, ':', lastBit printString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         subranges = ( |
            | vector copyAddFirst: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> () From: ( | {
         'Category: word size (override for other sizes)\x7fComment: override if notation is relative to some other number\x7fModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         totalNumberOfBits = 32.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot'
        
         asmBitRange2 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange2' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmBitRange2.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange2' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmFrame'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange2' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange2' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange2' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange2' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.14 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange2' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange2 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'asmBitRange3
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'asmBitRange3' From: 'applications/asmKit/asmFrame'



 '-- Side effects'

 globals modules asmBitRange2 postFileIn
