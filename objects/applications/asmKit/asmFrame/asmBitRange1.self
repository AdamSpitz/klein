 '$Revision: 30.11 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: bit ranges\x7fCategory: abstract\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         bitRange = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework bitRange.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange1 InitialContents: InitializeToExpression: (1)'
        
         mask <- 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( |
             {} = 'Comment: I am abstract.\x7fModuleInfo: Creator: globals assemblerSystems framework bitRange parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: concatenating\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         , aBitRange = ( |
            | 
            assemblerSystems framework compoundBitRange subranges: subranges, aBitRange subranges).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         < x = ( |
            | childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | 
            childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: extracting\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         asSignedIntegerFrom: word = ( |
            | 
            childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: extracting\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         asUnsignedIntegerFrom: word = ( |
            | 
            childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: private'
        
         computeMask = ( |
            | 
            width = totalNumberOfBits
             ifTrue: [^ intNN zero complement].
            intNN sub: (intNN shl: 1 With: width) With: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         fromSignedInteger: i = ( |
            | 
            fromSignedInteger: i IfSucceed: [|:r| r] IfFail: [|:e| error: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         fromSignedInteger: i IfSucceed: okBlk IfFail: failBlk = ( |
            | 
            ifSignedInteger: i IsOutOfRangeThen: [|:e| ^ failBlk value: e].
            okBlk value: fromUncheckedSignedInteger: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fCategory: without range checks\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: private'
        
         fromUncheckedSignedInteger: i = ( |
            | 
            childResponsibility).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fCategory: without range checks\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: private'
        
         fromUncheckedUnsignedInteger: i = ( |
            | 
            childResponsibility).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         fromUnsignedInteger: i = ( |
            | 
            fromUnsignedInteger: i IfSucceed: [|:r| r] IfFail: [|:e| error: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         fromUnsignedInteger: i IfSucceed: okBlk IfFail: failBlk = ( |
            | 
            ifUnsignedInteger: i IsOutOfRangeThen: [|:e| ^ failBlk value: e].
            okBlk value: fromUncheckedUnsignedInteger: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: private'
        
         ifSignedInteger: i IsOutOfRangeThen: failBlk = ( |
             w.
             wPred.
            | 
            w: width.
            wPred: w pred.

            (intNN le:  (intNN shl: -1 With: wPred)  With:  i)
              ifFalse: [^ failBlk value: i printString, ' is too negative for ', w printString, ' bits.'].

            (intNN lt:  i With:  intNN shl: 1  With:  wPred)
              ifFalse: [^ failBlk value: i printString, ' is too large for '   , w printString, ' bits.'].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: private'
        
         ifUnsignedInteger: i IsOutOfRangeThen: failBlk = ( |
            | 
            0 <= i               ifFalse: [^ failBlk value: i printString, ' is not positive.'].
            i <= mask asInteger  ifFalse: [^ failBlk value: i printString, ' does not fit in ', width printString, ' bits.'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: word size (override for other sizes)\x7fComment: set to int32 or int64\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = ( |
            | childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isSignedIntegerInRange: i = ( |
             widthPred.
            | 
            widthPred: width pred.

               (intNN le:          (intNN shl: -1 With: widthPred)  With:  i)
            && [intNN lt:  i With:  intNN shl:  1 With: widthPred           ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: inserting\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isUnsignedIntegerInRange: i = ( |
            | 
            (0 <= i) && [i <= mask asInteger]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfBitsToMyLeft = ( |
            | childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfBitsToMyRight = ( |
            | childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'orderedClonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot'
        
         subranges = ( |
            | childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: word size (override for other sizes)\x7fComment: override if notation is relative to some other number\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         totalNumberOfBits = ( |
            | childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         width = ( |
            | childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot'
        
         asmBitRange1 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange1' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmBitRange1.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange1' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmFrame'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange1' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange1 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange1' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange1' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange1' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.11 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange1' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange1 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'asmBitRange2
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'asmBitRange2' From: 'applications/asmKit/asmFrame'



 '-- Side effects'

 globals modules asmBitRange1 postFileIn
