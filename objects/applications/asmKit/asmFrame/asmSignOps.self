 '$Revision: 30.6 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent intOperandField parent signDependentOperations abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'abstract' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         extract: i From: aBitRange = ( |
            | childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'abstract' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         insert: anInt Into: aBitRange IfSucceed: okBlk IfFail: failBlk = ( |
            | 
            childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'abstract' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         legalTestCasesForMin: min Max: max Grain: grain = ( |
            | 
            min & max & 0 & grain & (min + grain) & (max - grain) & (min mean: max)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'abstract' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         maxForBitRange: br = ( |
            | childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'abstract' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         minForBitRange: br = ( |
            | childShouldImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         signed = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signed' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent intOperandField parent signDependentOperations signed.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signed' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         extract: i From: aBitRange = ( |
            | 
            aBitRange asSignedIntegerFrom: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signed' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         insert: anInt Into: aBitRange IfSucceed: okBlk IfFail: failBlk = ( |
            | 
            aBitRange fromSignedInteger: anInt IfSucceed: okBlk IfFail: failBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signed' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         legalTestCasesForMin: min Max: max Grain: grain = ( |
            | 
            (resend.legalTestCasesForMin: min Max: max Grain: grain)
            & grain negate).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signed' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         maxForBitRange: br = ( |
            | 
            (br intNN ushr: br mask With: 1) asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signed' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         minForBitRange: br = ( |
            | 
            (br intNN shl: -1 With: br width pred) asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signed' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         signedOrUnsigned = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signedOrUnsigned' -> () From: ( |
             {} = 'Comment: Some fields, e.g. imm22 on SPARC for sethi,
have a high-order bit which will become the high-order bit of
the resultant value. Therefore, they can be used with either signed
or unsigned values. -- David Ungar 6/02\x7fModuleInfo: Creator: globals assemblerSystems framework generators fields parent intOperandField parent signDependentOperations signedOrUnsigned.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signedOrUnsigned' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         extract: i From: aBitRange = ( |
            | 
            unsignedOps extract: i From: aBitRange).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signedOrUnsigned' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         insert: anInt Into: aBitRange IfSucceed: okBlk IfFail: failBlk = ( |
            | 
            0 <= anInt 
              ifTrue: [unsignedOps insert: anInt Into: aBitRange IfSucceed: okBlk IfFail: failBlk]
               False: [  signedOps insert: anInt Into: aBitRange IfSucceed: okBlk IfFail: failBlk]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signedOrUnsigned' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         legalTestCasesForMin: min Max: max Grain: grain = ( |
            | 
            signedOps legalTestCasesForMin: min Max: max Grain: grain).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signedOrUnsigned' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         maxForBitRange: br = ( |
            | 
            unsignedOps maxForBitRange: br).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signedOrUnsigned' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         minForBitRange: br = ( |
            | 
            signedOps minForBitRange: br).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signedOrUnsigned' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signedOrUnsigned' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: private'
        
         signedOps = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signed' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         unsigned = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'unsigned' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent intOperandField parent signDependentOperations unsigned.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signedOrUnsigned' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: private'
        
         unsignedOps = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'unsigned' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'unsigned' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         extract: i From: aBitRange = ( |
            | 
            aBitRange asUnsignedIntegerFrom: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'unsigned' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         insert: anInt Into: aBitRange IfSucceed: okBlk IfFail: failBlk = ( |
            | 
            aBitRange fromUnsignedInteger: anInt IfSucceed: okBlk IfFail: failBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'unsigned' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         maxForBitRange: br = ( |
            | 
            br mask).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'unsigned' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         minForBitRange: br = ( |
            | 
            0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'unsigned' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         signDependentOperations = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot'
        
         asmSignOps = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmSignOps' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmSignOps' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmSignOps.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSignOps' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmFrame'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSignOps' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSignOps' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSignOps' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSignOps' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.6 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSignOps' -> () From: ( | {
         'ModuleInfo: Module: asmSignOps InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules asmSignOps postFileIn
