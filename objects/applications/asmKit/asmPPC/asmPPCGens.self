 '$Revision: 30.29 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         crFields = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'crFields' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators registers copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'crFields' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators crFields.

CopyDowns:
globals assemblerSystems framework generators registers. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'crFields' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'crFields' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators crFields parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'crFields' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         maxReg = 7.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'crFields' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         minReg = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'crFields' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'crFields' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         regPrefix = 'cr'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         do: blk = ( |
            | 
            blk value: fprs.
            blk value: gprs.
            blk value: sprs.
            blk value: crFields.
            blk value: fields.
            blk value: instructionTemplates.
            blk value: pseudoInstructionTemplates.
            blk value: instructionAssemblyMethods.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         fields = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'subcategory' From:
             globals assemblerSystems framework generators fields copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fields.

CopyDowns:
globals assemblerSystems framework generators fields. copy 
SlotsToOmit: parent subcategory.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fields parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: prototypes\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         boOperandProto = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators fields parent intOperandField copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fields parent boOperandProto.

CopyDowns:
globals assemblerSystems framework generators fields parent intOperandField. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fields parent boOperandProto parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         assemble: operand At: lc IT: instTemplate IfSucceed: okBlk IfFail: failBlk = ( |
            | 
            (legalVector at: operand IfAbsent: [^ failBlk value: 'operand out of range'])
             ifFalse: [^ failBlk value: 'reserved operand'].
            resend.assemble: operand At: lc IT: instTemplate IfSucceed: okBlk IfFail: failBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         legalVector = [ | x =  ( bootstrap setObjectAnnotationOf: vector _Clone From: ( |
                     {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fields parent boOperandProto parent legalVector.
'.
                    | ) ) _Clone: 32 Filler: 0| 
             x _At: 0  Put: ().
             x _At: 1  Put: ().
             x _At: 2  Put: ().
             x _At: 3  Put: ().
             x _At: 4  Put: ().
             x _At: 5  Put: ().
             x _At: 6  Put: ().
             x _At: 7  Put: ().
             x _At: 8  Put: ().
             x _At: 9  Put: ().
             x _At: 10  Put: ().
             x _At: 11  Put: ().
             x _At: 12  Put: ().
             x _At: 13  Put: ().
             x _At: 14  Put: ().
             x _At: 15  Put: ().
             x _At: 16  Put: ().
             x _At: 17  Put: ().
             x _At: 18  Put: ().
             x _At: 19  Put: ().
             x _At: 20  Put: ().
             x _At: 21  Put: ().
             x _At: 22  Put: ().
             x _At: 23  Put: ().
             x _At: 24  Put: ().
             x _At: 25  Put: ().
             x _At: 26  Put: ().
             x _At: 27  Put: ().
             x _At: 28  Put: ().
             x _At: 29  Put: ().
             x _At: 30  Put: ().
             x _At: 31  Put: ().
             x] value.
        } | ) 

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 0 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 1 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 10 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 11 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 12 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 13 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 14 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 15 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 16 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 17 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 18 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 19 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 2 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 20 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 21 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 22 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 23 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 24 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 25 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 26 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 27 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 28 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 29 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 3 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 30 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 31 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 4 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 5 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 6 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 7 Put: (
     bootstrap stub -> 'globals' -> 'false' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 8 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'legalVector') -> () _At: 9 Put: (
     bootstrap stub -> 'globals' -> 'true' -> ())

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         unfilteredIllegalTestCases = [ | x =  ( bootstrap setObjectAnnotationOf: vector _Clone From: ( |
                     {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fields parent boOperandProto parent unfilteredIllegalTestCases.
'.
                    | ) ) _Clone: 15 Filler: 0| 
             x _At: 0  Put: ().
             x _At: 1  Put: ().
             x _At: 2  Put: ().
             x _At: 3  Put: ().
             x _At: 4  Put: ().
             x _At: 5  Put: ().
             x _At: 6  Put: ().
             x _At: 7  Put: ().
             x _At: 8  Put: ().
             x _At: 9  Put: ().
             x _At: 10  Put: ().
             x _At: 11  Put: ().
             x _At: 12  Put: ().
             x _At: 13  Put: ().
             x _At: 14  Put: ().
             x] value.
        } | ) 

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 0 Put: (
     6)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 1 Put: (
     7)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 10 Put: (
     27)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 11 Put: (
     28)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 12 Put: (
     29)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 13 Put: (
     30)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 14 Put: (
     31)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 2 Put: (
     14)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 3 Put: (
     15)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 4 Put: (
     21)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 5 Put: (
     22)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 6 Put: (
     23)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 7 Put: (
     24)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 8 Put: (
     25)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredIllegalTestCases') -> () _At: 9 Put: (
     26)

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         unfilteredIllegalTestCasesDo: blk = ( |
            | 
            unfilteredIllegalTestCases do: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         unfilteredLegalTestCases = [ | x =  ( bootstrap setObjectAnnotationOf: vector _Clone From: ( |
                     {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fields parent boOperandProto parent unfilteredLegalTestCases.
'.
                    | ) ) _Clone: 17 Filler: 0| 
             x _At: 0  Put: ().
             x _At: 1  Put: ().
             x _At: 2  Put: ().
             x _At: 3  Put: ().
             x _At: 4  Put: ().
             x _At: 5  Put: ().
             x _At: 6  Put: ().
             x _At: 7  Put: ().
             x _At: 8  Put: ().
             x _At: 9  Put: ().
             x _At: 10  Put: ().
             x _At: 11  Put: ().
             x _At: 12  Put: ().
             x _At: 13  Put: ().
             x _At: 14  Put: ().
             x _At: 15  Put: ().
             x _At: 16  Put: ().
             x] value.
        } | ) 

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 0 Put: (
     0)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 1 Put: (
     1)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 10 Put: (
     12)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 11 Put: (
     13)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 12 Put: (
     16)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 13 Put: (
     17)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 14 Put: (
     18)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 15 Put: (
     19)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 16 Put: (
     20)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 2 Put: (
     2)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 3 Put: (
     3)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 4 Put: (
     4)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 5 Put: (
     5)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 6 Put: (
     8)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 7 Put: (
     9)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 8 Put: (
     10)

 ((bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'boOperandProto' -> 'parent') \/-> 'unfilteredLegalTestCases') -> () _At: 9 Put: (
     11)

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: prototypes\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         branchDispOperandField = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators fields parent branchDispOperandField copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fields parent branchDispOperandField.

CopyDowns:
globals assemblerSystems framework generators fields parent branchDispOperandField. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fields parent branchDispOperandField parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         isRelativeForIT: instTemplate = ( |
            | 
            instTemplate includesOptionAOfFieldAA not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: public'
        
         unfilteredLegalTestCases = ( |
            | 
            "bug in gnu asm, exclude min"
            "gnu asm mispredicts for 0 case"
            ((resend.unfilteredLegalTestCases asSet
              remove: min)
              remove: 0)
               asVector sort).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: names spaces I need\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         crFields = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'crFields' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: names spaces I need\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         fields = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'fields' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: names spaces I need\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         fprs = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'fprs' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateAll = ( |
            | 
            start.
            generateOperandFields.
            generateConstantFields.
            generateReservedFields.
            generateOptionFields.
            generateBranchOptionFields.
            finish).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: generating\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateBOField: name Bits: bits Keyword: k = ( |
             ff.
            | 
            ff: boOperandProto copyName: name Bits: bits Keyword: k Asm: myAssemblerSystem.
            at: name Put: ff.
            ff).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: generating\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateBranchDispField: name Bits: bits Keyword: k = ( |
            | 
            (resend.generateBranchDispField: name Bits: bits Keyword: k) beBackpatchable).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: option fields\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateBranchOptionFields = ( |
             bc.
             bi_eq = 2.
             bi_gt = 1.
             bi_lt = 0.
             bi_so = 3.
             bo_false = 2.
             bo_true = 6.
            | 
            "includes BO field w/o branch pred bit & bi field w/o cr portion part"
            bc: generateOptionField: 'branch_conds' At: 6 & 9 & 14 & 15.
            bc  withOption: 'lt' Value: (bo_true  << 2) || bi_lt.
            bc  withOption: 'ge' Value: (bo_false << 2) || bi_lt.
            bc  withOption: 'gt' Value: (bo_true  << 2) || bi_gt.
            bc  withOption: 'le' Value: (bo_false << 2) || bi_gt.
            bc  withOption: 'eq' Value: (bo_true  << 2) || bi_eq.
            bc  withOption: 'ne' Value: (bo_false << 2) || bi_eq.
            bc  withOption: 'so' Value: (bo_true  << 2) || bi_so.
            bc  withOption: 'ns' Value: (bo_false << 2) || bi_so.

            (((generateOptionField: 'reg_prediction' At: 10 & 10)
              withOption: 'Taken'   Value: 1 ExternalName: '+')
              withOption: 'Untaken' Value: 0 ExternalName: '-')
              withOption: ''        Value: 0.

            (generateOptionField: 'unusual_prediction' At: 10 & 10)
              withOption: 'Unusual'   Value: 1.

            ((generateOptionField: 'bo_cond_true_or_false' At: 6 & 9)
              withOption: 't' Value: 6)
              withOption: 'f' Value: 2.

            bc: generateOptionField: 'bo_simple_conds_with_bi' At: 6 & 9.
            bc withOption: 't'    Value: 6.
            bc withOption: 'f'    Value: 2.
            bc withOption: 'dnzt' Value: 4.
            bc withOption: 'dnzf' Value: 0.
            bc withOption: 'dzt'  Value: 5.
            bc withOption: 'dzf'  Value: 1.

            ((generateOptionField: 'bo_simple_conds_no_bi' At: 6 & 9)
              withOption: 'dnz' Value: 8)
              withOption: 'dz'  Value: 9.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: constant fields\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateConstantFields = ( |
            | 
            "instruction fields are wired into the operand"
            generateConstantField: 'bit_30'      Bits: 30 & 30.
            generateConstantField: 'bit_31'      Bits: 31 & 31.
            generateConstantField: 'xo_30_31'    Bits: 30 & 31.
            generateConstantField: 'opcd'        Bits:  0 &  5.
            generateConstantField: 'xo_21_29'    Bits: 21 & 29.
            generateConstantField: 'xo_21_30'    Bits: 21 & 30.
            generateConstantField: 'xo_22_30'    Bits: 22 & 30.
            generateConstantField: 'xo_27_29'    Bits: 27 & 29.
            generateConstantField: 'xo_26_30'    Bits: 26 & 30.
            generateConstantField: 'xo_27_30'    Bits: 27 & 30.
            generateConstantField: 'l'           Bits: 10 & 10).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: generating\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateIntOperandFields = ( |
            | 
            (generateIntOperandField:    'd'  Bits: 16 & 31 Keyword: 'disp')
             beSigned beBackpatchable externalSeparator: ''.
            (generateIntOperandField:    'ds' Bits: 16 & 29 Keyword: 'disp' Zeroes: 2)
             beSigned beBackpatchable externalSeparator: ''.
            ((generateZeroIsMaxOperandField: 'nb' Bits: 16 & 20 Keyword: 'size')
              withUntestableIllegalCase: 0) withUntestableIllegalCase: 32.
            (generateIntOperandField:   'si'  Bits: 16 & 31 Keyword: 'with') beSigned beBackpatchable.
            ((generateIntOperandField:  'sis' Bits: 16 & 31 Keyword: 'with') beSignedOrUnsigned beBackpatchable
              withUntestableIllegalCase: -32769) withUntestableIllegalCase: 32768.
            (generateIntOperandField:  'ui' Bits: 16 & 31 Keyword: 'with') beBackpatchable.

            (generateIntOperandField:  'to' Bits:  6 & 10 Keyword: 'tos')
              withUntestableIllegalCase: -1.

            (generateIntOperandField: 'sh64' Bits: 30 & 30  &  16 & 20 Keyword: 'by')
              withUntestableIllegalCase: 64.
            (generateIntOperandField: 'mb64' Bits: 26 & 26  &  21 & 25 Keyword: 'maskBegin')
              withUntestableIllegalCase: 64.
            (generateIntOperandField: 'me64' Bits: 26 & 26  &  21 & 25 Keyword: 'maskEnd')
              withUntestableIllegalCase: 64.

            ((generateIntOperandField: 'sh'   Bits: 16 & 20 Keyword: 'by')
              withUntestableIllegalCase: 32) withUntestableIllegalCase: -1.
            ((generateIntOperandField: 'mb'   Bits: 21 & 25 Keyword: 'maskBegin')
              withUntestableIllegalCase: 32) withUntestableIllegalCase: -1.
            ((generateIntOperandField: 'me'   Bits: 26 & 30 Keyword: 'maskEnd')
              withUntestableIllegalCase: 32) withUntestableIllegalCase: -1.

            generateIntOperandField: 'fxm'  Bits: 12 & 19 Keyword: 'fieldMask'.

            (generateBranchDispField: 'li'   Bits:  6 & 29  Keyword: 'disp')
              withUntestableIllegalCases: (| value: c = ((c value && 3) != 0) |).
            (generateBranchDispField: 'bd'   Bits: 16 & 29  Keyword: 'disp')
              withUntestableIllegalCases: (| value: c = ((c value && 3) != 0) |).

            (generateBOField:      'bo'   Bits:  6 & 10  Keyword: 'BO')
              withUntestableIllegalCases: (| legalVector = assemblerSystems ppc generators fields boOperandProto legalVector.
                                             value: c = ((legalVector at: c value) not) |).

            (generateIntOperandField: 'bi'   Bits: 11 & 15  Keyword: 'crBit')
              withUntestableIllegalCase: -1.

            (generateIntOperandField: 'bt'   Bits:  6 & 10  Keyword: 'toBit')
              withUntestableIllegalCase: -1.

            generateIntOperandField: 'ba'   Bits: 11 & 15  Keyword: 'fromBit'.

            (generateIntOperandField: 'bb'   Bits: 16 & 20  Keyword: 'withBit')
              withUntestableIllegalCase: -1.

            generateIntOperandField: 'u'    Bits: 16 & 19  Keyword: 'from'.
            generateIntOperandField: 'flm'  Bits:  7 & 14  Keyword: 'fieldMask'.

            "Should use bf, bfa, but external asm won't take the syntax."
            generateIntOperandField:  'bf_to_float'  Bits:  6 &  8  Keyword: 'toFPSCRField'.
            (generateIntOperandField:  'bfa_float'    Bits: 11 & 13  Keyword: 'fromFPSCRField')
              withUntestableIllegalCase: 8.

            "next two just for pseudos; numbers of bits"
            generateZeroIsMaxOperandField: 'numBits64' Bits: 0 & 5 Keyword: 'size'.
            generateZeroIsMaxOperandField: 'numBits32' Bits: 0 & 4 Keyword: 'size'.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: generating\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateOperandFields = ( |
            | 
            generateSymbolicOperandFields.
            generateIntOperandFields).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: option fields\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateOptionFields = ( |
            | 
            "order: 1"
            ((generateOptionField: 'oe'  At: 21 & 21)
                withOption: ''   Value: 0)
                withOption: 'o'  Value: 1.

            "order 2"
            ((generateOptionField: 'rc'  At: 31 & 31)
                withOption: ''  Value: 0)
                withOption: '_' Value: 1  ExternalName: '.'.

            "order 1"
            ((generateOptionField: 'lk'  At: 31 & 31)
                withOption: ''   Value: 0)
                withOption: 'l'  Value: 1.

            "order 2"
            ((generateOptionField: 'aa'  At: 30 & 30)
                withOption: ''   Value: 0)
                withOption: 'a'  Value: 1.

            generateOptionField: 'to_const' At: 6 & 10.
            (  'lt' & 'le' & 'eq' & 'ge' & 'gt' & 'ne'
               & 'llt' & 'lle' & 'lge' & 'lgt') asVector with:
              (16 & 20 & 4 & 12 & 8 & 24 & 2 & 6 & 5 & 1)
               asVector Do: [|:cond. :condNum|
                  result to_const withOption: cond Value: condNum
            ].

            (((generateOptionField: 'spr_opts' At: 16 & 20 & 11 & 15)
              withOption: 'xer' Value: sprs xer number)
              withOption: 'lr'  Value: sprs lr  number)
              withOption: 'ctr' Value: sprs ctr number.

            "when using option fields, sometimes need a suffix
             in the mnemonic AFTER the option field.
             use an option field with one option and no bits in it"

            generateMnemonicField: 'put_i_in_name'   Suffix: 'i'.
            generateMnemonicField: 'put_lr_in_name'  Suffix: 'lr'.
            generateMnemonicField: 'put_ctr_in_name' Suffix: 'ctr'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: generating\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateR0MeansZeroOperandField: name Bits: bits Keyword: k In: nameSpace = ( |
             f.
            | 
            f: r0MeansZeroOperandField copyName: name
                                           Bits: bits
                                        Keyword: k
                                             In: nameSpace
                                            Asm: myAssemblerSystem.
            at: name Put: f.
            f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: reserved fields\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateReservedFields = ( |
            | 
            generateReservedField: 'res_6'       Bits:  6 &  6.
            generateReservedField: 'res_6_10'    Bits:  6 & 10.
            generateReservedField: 'res_9'       Bits:  9 &  9.
            generateReservedField: 'res_9_10'    Bits:  9 & 10.
            generateReservedField: 'res_11'      Bits: 11 & 11.
            generateReservedField: 'res_11_15'   Bits: 11 & 15.
            generateReservedField: 'res_14'      Bits: 14 & 14.
            generateReservedField: 'res_14_15'   Bits: 14 & 15.
            generateReservedField: 'res_15'      Bits: 15 & 15.
            generateReservedField: 'res_16_20'   Bits: 16 & 20.
            generateReservedField: 'res_16_29'   Bits: 16 & 29.
            generateReservedField: 'res_20'      Bits: 20 & 20.
            generateReservedField: 'res_21'      Bits: 21 & 21.
            generateReservedField: 'res_21_25'   Bits: 21 & 25.
            generateReservedField: 'res_31'      Bits: 31 & 31).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: generating\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateSymbolicOperandFields = ( |
            | 
            generateSymbolicOperandField:     'ra'         Bits: 11 & 15  Keyword: 'from'   In: gprs.
            generateR0MeansZeroOperandField:  'ra0'        Bits: 11 & 15  Keyword: 'from'   In: gprs.
            generateSymbolicOperandField:     'ra_with'    Bits: 11 & 15  Keyword: 'with'   In: gprs.
            generateSymbolicOperandField:     'ra_to'      Bits: 11 & 15  Keyword: 'to'     In: gprs.
            generateSymbolicOperandField:     'ra_base'    Bits: 11 & 15  Keyword: 'base'   In: gprs.
            generateR0MeansZeroOperandField:  'ra_base0'   Bits: 11 & 15  Keyword: 'base'   In: gprs.
            generateSymbolicOperandField:     'rb'         Bits: 16 & 20  Keyword: 'with'   In: gprs.
            generateSymbolicOperandField:     'rb_from'    Bits: 16 & 20  Keyword: 'from'   In: gprs.
            generateSymbolicOperandField:     'rs_from'    Bits:  6 & 10  Keyword: 'from'   In: gprs.
            generateSymbolicOperandField:     'rt'         Bits:  6 & 10  Keyword: 'to'     In: gprs.
            generateSymbolicOperandField:     'bf'         Bits:  6 &  8  Keyword: 'crField' In: crFields.
            ((generateSymbolicOperandField:   'bf_no8'     Bits:  6 &  8  Keyword: 'crField' In: crFields)
              withUntestableIllegalCase: 8) withUntestableIllegalCase: -1.
            generateSymbolicOperandField:     'bf_to'      Bits:  6 &  8  Keyword: 'toCRField'   In: crFields.
            generateSymbolicOperandField:     'bfa'        Bits: 11 & 13  Keyword: 'fromCRField' In: crFields.
            ((generateSymbolicOperandField:   'br_cr'      Bits: 11 & 13  Keyword: 'CRField'     In: crFields)
              withUntestableIllegalCase: 8) withUntestableIllegalCase: -1.
            generateSymbolicOperandField:     'spr_to'     Bits: 16 & 20 & 11 & 15 Keyword: 'to'   In: sprs.
            generateSymbolicOperandField:     'spr_from'   Bits: 16 & 20 & 11 & 15 Keyword: 'from' In: sprs.

            generateSymbolicOperandField:  'frt'        Bits:  6 & 10  Keyword: 'to'     In: fprs.
            generateSymbolicOperandField:  'frs'        Bits:  6 & 10  Keyword: 'source' In: fprs.
            generateSymbolicOperandField:  'fra'        Bits: 11 & 15  Keyword: 'from'   In: fprs.
            generateSymbolicOperandField:  'frb_with'   Bits: 16 & 20  Keyword: 'with'   In: fprs.
            generateSymbolicOperandField:  'frb_from'   Bits: 16 & 20  Keyword: 'from'   In: fprs.
            generateSymbolicOperandField:  'frb_plus'   Bits: 16 & 20  Keyword: 'plus'   In: fprs.
            generateSymbolicOperandField:  'frb_minus'  Bits: 16 & 20  Keyword: 'minus'  In: fprs.
            generateSymbolicOperandField:  'frc_times'  Bits: 21 & 25  Keyword: 'times'  In: fprs.

            ((result ra_base  externalPrefix: '(') externalSuffix: ')') externalSeparator: ''.
            ((result ra_base0 externalPrefix: '(') externalSuffix: ')') externalSeparator: ''.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: names spaces I need\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         gprs = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'gprs' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: prototypes\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         r0MeansZeroOperandField = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'r0MeansZeroOperandField' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators fields parent symbolicOperandField copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'r0MeansZeroOperandField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fields parent r0MeansZeroOperandField.

CopyDowns:
globals assemblerSystems framework generators fields parent symbolicOperandField. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'r0MeansZeroOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'r0MeansZeroOperandField' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fields parent r0MeansZeroOperandField parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'r0MeansZeroOperandField' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         assemble: operand At: lc IT: instTemplate IfSucceed: okBlk IfFail: failBlk = ( |
            | 
            r0 = operand ifTrue: [failBlk value: 'use 0 instead of r0'].
            operand value = 0 ifTrue: [^ 0].
            resend.assemble: operand At: lc IT: instTemplate IfSucceed: okBlk IfFail: failBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'r0MeansZeroOperandField' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         isSymbolicForValue: operand = ( |
            | 
            0 !== operand).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'r0MeansZeroOperandField' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: public'
        
         operandForValue: i = ( |
            | 
            i = 0  ifTrue: [^ 0].
            resend.operandForValue: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'r0MeansZeroOperandField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'r0MeansZeroOperandField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         r0 = ( |
            | 
            assemblerSystems ppc gprs r0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'r0MeansZeroOperandField' -> 'parent' -> () From: ( | {
         'Category: generating tests & testing the assembler\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: public'
        
         unfilteredIllegalTestCasesDo: blk = ( |
            | 
            blk value: r0.
            resend.unfilteredIllegalTestCasesDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'r0MeansZeroOperandField' -> 'parent' -> () From: ( | {
         'Category: generating tests & testing the assembler\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: public'
        
         unfilteredLegalTestCases = ( |
             r.
            | 
            r: resend.unfilteredLegalTestCases.
            r removeFirst.
            r addFirst: 0.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: names spaces I need\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         sprs = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'sprs' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: prototypes\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         zeroIsMaxOperandProto = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandProto' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators fields parent zeroIsMaxOperandField copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandProto' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fields parent zeroIsMaxOperandProto.

CopyDowns:
globals assemblerSystems framework generators fields parent zeroIsMaxOperandField. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandProto' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandProto' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fields parent zeroIsMaxOperandProto parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandField' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: public'
        
         unfilteredIllegalTestCasesDo: blk = ( |
            | 
            "ppc asm accepts zero!"
            resend.unfilteredIllegalTestCasesDo: [|:i|
              blk value: (intNN eq: i With: 0)
                ifTrue: -1  False: i
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fields' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: InitializeToExpression: (\'\')'
        
         subcategory <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         fprs = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fprs' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators registers copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fprs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fprs.

CopyDowns:
globals assemblerSystems framework generators registers. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fprs' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fprs' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators fprs parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fprs' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         maxReg = 31.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fprs' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         minReg = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fprs' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'fprs' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         regPrefix = 'f'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         gprs = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'gprs' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators registers copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'gprs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators gprs.

CopyDowns:
globals assemblerSystems framework generators registers. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'gprs' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'gprs' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators gprs parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'gprs' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateConstantNamed: s Number: n = ( |
            | 
            resend.generateConstantNamed: (
                     case if: [n = 1] Then: 'sp'
                          If: [n = 2] Then: 'rtoc'
                                      Else: s )
                   Number: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'gprs' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         maxReg = 31.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'gprs' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         minReg = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'gprs' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         instructionAssemblyMethods = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionAssemblyMethods' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators instructionAssemblyMethods copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionAssemblyMethods' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators instructionAssemblyMethods.

CopyDowns:
globals assemblerSystems framework generators instructionAssemblyMethods. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionAssemblyMethods' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionAssemblyMethods' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators instructionAssemblyMethods parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionAssemblyMethods' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionAssemblyMethods' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         instructionTemplates = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators instructionTemplates copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators instructionTemplates.

CopyDowns:
globals assemblerSystems framework generators instructionTemplates. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators instructionTemplates parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: branch processor\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         asmBranchesToLRInsteadOfCTR: its = ( |
            | 
            dontExternallyTestGoodies: its.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: branch processor\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         asmPermitsIllegalCRs: its = ( |
            | 
            "gnu assembler permits cr8"
            dontExternallyTestBaddies:
              its asList copyFilteredBy: [|:it|
                it operandFields includes: br_cr ].
            its).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         fields* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'fields' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: customize for PPC\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         gen: name With: fieldCollector ExternalName: xName Pred: predBlk Bads: colOfCols = ( |
            | 
            resend.gen: name
                      With: (fieldCollector asList addFirst: opcd)
              ExternalName: xName
                      Pred: predBlk
                      Bads: colOfCols).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateAll = ( |
            | 
            start.
            generateBranches.
            generateCondRegInsts.
            generateLoadsAndStores.
            generateFixedPointArithmetics.
            generateFixedPointCompares.
            generateFixedPointTraps.
            generateFixedPointLogicals.
            generateFixedPointRotatesAndShifts.
            generateMoveToFromSystemRegisters.

            generateFloatingPointLoads.
            generateFloatingPointStores.
            generateFloatingPointMoves.
            generateFloatingPointAriths.
            generateFloatingPointRoundsAndCvts.
            generateFloatingPointCompares.
            generateFloatingPointStatusAndCRs.

            generateStorageControlInstructions.

            finish).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: branch processor\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateBranchMnemonicsIncorperatingConditions = ( |
             cr0.
             crChoices.
             its.
             rp_const.
            | 

            cr0: constantFieldFor: br_cr.

            "External asm uses +/- for branch with imm disp
             in such a weird way need pseudo ops.
             But define real op for usual (no prediction) case
             so simple case disasms nicely. -- dmu 1/2"
            rp_const: constantFieldFor: reg_prediction.

            crChoices: (list copyRemoveAll add: cr0 & 0 )
                                           add: br_cr.
            crChoices do: [|:crc|
              "beq, beql, beqla, beqa, etc:"
              asmPermitsIllegalCRs:
              gen: 'b'  With: 16 & crc & branch_conds & bd & lk & aa & rp_const & 0.

              dontExternallyTestGoodies: "asm does not support Unusual, see pseudos"
              asmPermitsIllegalCRs:
              gen: 'b'  With: 16 & crc & branch_conds & bd & lk & aa & unusual_prediction.

              "beqlr, beqlrl, etc:"
              asmPermitsIllegalCRs:
              gen: 'b'  With: 19 & crc & branch_conds & res_16_20
                              & xo_21_30 & 16 & put_lr_in_name & lk & reg_prediction.

              "beqctr, beqctrl, etc:"
              asmBranchesToLRInsteadOfCTR: "asm bug: beqctr+ asms as beqlr+"
              asmPermitsIllegalCRs:
              gen: 'b'  With: 19 & crc & branch_conds & res_16_20
                              & xo_21_30 & 528 & put_ctr_in_name & lk & reg_prediction.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: branch processor\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateBranches = ( |
            | 
            generateGeneralBranches.
            generateSimpleBranchMnemonics.
            generateBranchMnemonicsIncorperatingConditions).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fixed-point instructions\x7fCategory: fixed-point loads and stores\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateByteReversals = ( |
            | 
            subcategory: 'byte reversals'.

            gen: 'lhbrx'   With: 31 & rt      & ra0 & rb & xo_21_30 & 790 & res_31.
            gen: 'lwbrx'   With: 31 & rt      & ra0 & rb & xo_21_30 & 534 & res_31.
            gen: 'sthbrx'  With: 31 & rs_from & ra0 & rb & xo_21_30 & 918 & res_31.
            gen: 'stwbrx'  With: 31 & rs_from & ra0 & rb & xo_21_30 & 662 & res_31.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: branch processor\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateCondRegInsts = ( |
            | 
            subcategory: 'condition reg logicals'.
            gen: 'crand'  With: 19 & bt & ba & bb & xo_21_30 & 257 & res_31.
            gen: 'cror'   With: 19 & bt & ba & bb & xo_21_30 & 449 & res_31.
            gen: 'crxor'  With: 19 & bt & ba & bb & xo_21_30 & 193 & res_31.
            gen: 'crnand' With: 19 & bt & ba & bb & xo_21_30 & 225 & res_31.
            gen: 'crnor'  With: 19 & bt & ba & bb & xo_21_30 &  33 & res_31.
            gen: 'creqv'  With: 19 & bt & ba & bb & xo_21_30 & 289 & res_31.
            gen: 'crandc' With: 19 & bt & ba & bb & xo_21_30 & 129 & res_31.
            gen: 'crorc'  With: 19 & bt & ba & bb & xo_21_30 & 417 & res_31.

            gen: 'mcrf'   With: 19 & bf_to & res_9_10 & bfa & res_14_15
                              & res_16_20 & xo_21_30 & 0 & res_31.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fixed-point instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateFixedPointArithmetics = ( |
            | 
            subcategory: 'fixed-point arithmetic'.

            gen: 'addi'   With: 14 & rt & ra0 & si.

            gen: 'li'     With: 14 & rt & (constantFieldFor: ra) & 0 & si.

            gen: 'addis'  With: 15 & rt & ra0 & sis.

            gen: 'lis'    With: 15 & rt & (constantFieldFor: ra) & 0 & sis.

            gen: 'add'    With: 31 & rt & ra & rb & oe & xo_22_30 & 266 & rc.
            gen: 'addic'  With: 12 & rt & ra & si.
            gen: 'addic_' With: 13 & rt & ra & si.
            gen: 'subfic' With:  8 & rt & ra & si.
            gen: 'addc'   With: 31 & rt & ra & rb & oe & xo_22_30 &  10 & rc.
            gen: 'adde'   With: 31 & rt & ra & rb & oe & xo_22_30 & 138 & rc.
            gen: 'subfe'  With: 31 & rt & ra & rb & oe & xo_22_30 & 136 & rc.
            gen: 'addme'  With: 31 & rt & ra & res_16_20 & oe & xo_22_30 & 234 & rc.
            gen: 'subfme' With: 31 & rt & ra & res_16_20 & oe & xo_22_30 & 232 & rc.
            gen: 'addze'  With: 31 & rt & ra & res_16_20 & oe & xo_22_30 & 202 & rc.
            gen: 'subfze' With: 31 & rt & ra & res_16_20 & oe & xo_22_30 & 200 & rc.
            gen: 'neg'    With: 31 & rt & ra & res_16_20 & oe & xo_22_30 & 104 & rc.
            gen: 'sub'    With: 31 & rt & rb_from & ra_with & oe & xo_22_30 &  40 & rc.
            gen: 'subc'   With: 31 & rt & rb_from & ra_with & oe & xo_22_30 &   8 & rc.

            gen: 'mulli'  With:  7 & rt & ra & si.
            gen64s: [
            gen: 'mulld'  With: 31 & rt & ra & rb & oe & xo_22_30 & 233 & rc.
            ].
            gen: 'mullw'  With: 31 & rt & ra & rb & oe & xo_22_30 & 235 & rc.
            gen64s: [
            gen: 'mulhd' With: 31 & rt & ra & rb & res_21 & xo_22_30 &  73 & rc.
            ].
            gen: 'mulhw' With: 31 & rt & ra & rb & res_21 & xo_22_30 &  75 & rc.
            gen64s: [
            gen: 'mulhdu' With: 31 & rt & ra & rb & res_21 & xo_22_30 &   9 & rc.
            ].
            gen: 'mulhwu' With: 31 & rt & ra & rb & res_21 & xo_22_30 &  11 & rc.

            gen64s: [
            gen: 'divd'   With: 31 & rt & ra & rb & oe & xo_22_30 & 489 & rc.
            ].
            gen: 'divw'   With: 31 & rt & ra & rb & oe & xo_22_30 & 491 & rc.
            gen64s: [
            gen: 'divdu'  With: 31 & rt & ra & rb & oe & xo_22_30 & 457 & rc.
            ].
            gen: 'divwu'  With: 31 & rt & ra & rb & oe & xo_22_30 & 459 & rc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fixed-point instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateFixedPointCompares = ( |
             bf_c.
            | 
            subcategory: 'fixed-point compares'.
            bf_c: constantFieldFor: bf.

                      gen: 'cmpwi'  With: 11 & bf       & res_9 & l & 0 & ra & si.
                      gen: 'cmpwi'  With: 11 & bf_c & 0 & res_9 & l & 0 & ra & si.
            gen64s: [ gen: 'cmpdi'  With: 11 & bf       & res_9 & l & 1 & ra & si].
            gen64s: [ gen: 'cmpdi'  With: 11 & bf_c & 0 & res_9 & l & 1 & ra & si].

                      gen: 'cmpw'   With: 31 & bf       & res_9 & l & 0 & ra & rb & xo_21_30 &  0 & res_31.
                      gen: 'cmpw'   With: 31 & bf_c & 0 & res_9 & l & 0 & ra & rb & xo_21_30 &  0 & res_31.
            gen64s: [ gen: 'cmpd'   With: 31 & bf       & res_9 & l & 1 & ra & rb & xo_21_30 &  0 & res_31].
            gen64s: [ gen: 'cmpd'   With: 31 & bf_c & 0 & res_9 & l & 1 & ra & rb & xo_21_30 &  0 & res_31].

                      gen: 'cmplwi' With: 10 & bf       & res_9 & l & 0 & ra & ui.
                      gen: 'cmplwi' With: 10 & bf_c & 0 & res_9 & l & 0 & ra & ui.
            gen64s: [
                      gen: 'cmpldi' With: 10 & bf       & res_9 & l & 1 & ra & ui.
                      gen: 'cmpldi' With: 10 & bf_c & 0 & res_9 & l & 1 & ra & ui.
            ].
                      gen: 'cmplw'  With: 31 & bf       & res_9 & l & 0 & ra & rb & xo_21_30 & 32 & res_31.
                      gen: 'cmplw'  With: 31 & bf_c & 0 & res_9 & l & 0 & ra & rb & xo_21_30 & 32 & res_31.
            gen64s: [ gen: 'cmpld'  With: 31 & bf       & res_9 & l & 1 & ra & rb & xo_21_30 & 32 & res_31].
            gen64s: [ gen: 'cmpld'  With: 31 & bf_c & 0 & res_9 & l & 1 & ra & rb & xo_21_30 & 32 & res_31].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fixed-point instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateFixedPointLogicals = ( |
            | 
            subcategory: 'fixed-point logicals'.

            gen: 'andi_'   With: 28 & ra_to & rs_from & ui.
            gen: 'andis_'  With: 29 & ra_to & rs_from & ui.
            gen: 'ori'     With: 24 & ra_to & rs_from & ui.
            gen: 'nop'     With: 24 & (constantFieldFor: ra_to) & 0
                                    & (constantFieldFor: rs_from) & 0
                                    & (constantFieldFor: ui) & 0.
            gen: 'oris'    With: 25 & ra_to & rs_from & ui.
            gen: 'xori'    With: 26 & ra_to & rs_from & ui.
            gen: 'xoris'   With: 27 & ra_to & rs_from & ui.

            gen: 'and'     With: 31 & ra_to & rs_from & rb & xo_21_30 &  28 & rc.
            gen: 'or'      With: 31 & ra_to & rs_from & rb & xo_21_30 & 444 & rc.
            gen: 'xor'     With: 31 & ra_to & rs_from & rb & xo_21_30 & 316 & rc.
            gen: 'nand'    With: 31 & ra_to & rs_from & rb & xo_21_30 & 476 & rc.
            gen: 'nor'     With: 31 & ra_to & rs_from & rb & xo_21_30 & 124 & rc.
            gen: 'eqv'     With: 31 & ra_to & rs_from & rb & xo_21_30 & 284 & rc.
            gen: 'andc'    With: 31 & ra_to & rs_from & rb & xo_21_30 &  60 & rc.
            gen: 'orc'     With: 31 & ra_to & rs_from & rb & xo_21_30 & 412 & rc.

            gen: 'extsb'   With: 31 & ra_to & rs_from & res_16_20 & xo_21_30 & 954 & rc.
            gen: 'extsh'   With: 31 & ra_to & rs_from & res_16_20 & xo_21_30 & 922 & rc.
            gen64s: [
            gen: 'extsw'   With: 31 & ra_to & rs_from & res_16_20 & xo_21_30 & 986 & rc.
            ].
            gen64s: [ gen: 'cntlzd'  With: 31 & ra_to & rs_from & res_16_20 & xo_21_30 & 58 & rc ].
                      gen: 'cntlzw'  With: 31 & ra_to & rs_from & res_16_20 & xo_21_30 & 26 & rc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fixed-point instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateFixedPointRotatesAndShifts = ( |
             mb64_c.
             mb_c.
             me_c.
             sh64_c.
             sh_c.
            | 
            subcategory: 'fixed-point rotates and shifts'.
            gen64s: [
              mb64_c: constantFieldFor: mb64.
              sh64_c: constantFieldFor: sh64.
              gen: 'rldicl'  With: 30 & ra_to & rs_from & sh64 & mb64 & xo_27_29 & 0 & rc.
              gen: 'rotldi'  With: 30 & ra_to & rs_from & sh64 & mb64_c & 0 & xo_27_29 & 0 & rc.
              gen: 'clrldi'  With: 30 & ra_to & rs_from & sh64_c & 0 & mb64 & xo_27_29 & 0 & rc.

              gen: 'rldicr'  With: 30 & ra_to & rs_from & sh64 & me64 & xo_27_29 & 1 & rc.
              gen: 'rldic'   With: 30 & ra_to & rs_from & sh64 & mb64 & xo_27_29 & 2 & rc.
            ].
              mb_c: constantFieldFor: mb.
              me_c: constantFieldFor: me.
              sh_c: constantFieldFor: sh.
              gen: 'rlwinm'  With: 21 & ra_to & rs_from & sh & mb & me & rc.
              gen: 'rotlwi'  With: 21 & ra_to & rs_from & sh & mb_c & 0 & me_c & 31 & rc.
              gen: 'clrlwi'  With: 21 & ra_to & rs_from & sh_c & 0 & mb & me_c & 31 & rc.


            gen64s: [ gen: 'rldcl' With: 30 & ra_to & rs_from & rb & mb64 & xo_27_30 & 8 & rc.
                      gen: 'rotld' With: 30 & ra_to & rs_from & rb & mb64_c & 0 & xo_27_30 & 8 & rc.
                      gen: 'rldcr' With: 30 & ra_to & rs_from & rb & me64 & xo_27_30 & 9 & rc
            ].
                      gen: 'rlwnm'  With: 23 & ra_to & rs_from & rb & mb & me & rc.
                      gen: 'rotlw'  With: 23 & ra_to & rs_from & rb & mb_c & 0 & me_c & 31 & rc.


            gen64s: [ gen: 'rldimi' With: 30 & ra_to & rs_from & sh64 & mb64 & xo_27_29 & 3 & rc ].
                      gen: 'rlwimi' With: 20 & ra_to & rs_from & sh   & mb & me & rc.

            gen64s: [ gen: 'sld'   With: 31 & ra_to & rs_from & rb & xo_21_30 &  27 & rc. ].
                      gen: 'slw'   With: 31 & ra_to & rs_from & rb & xo_21_30 &  24 & rc.
            gen64s: [ gen: 'srd'   With: 31 & ra_to & rs_from & rb & xo_21_30 & 539 & rc. ].
                      gen: 'srw'   With: 31 & ra_to & rs_from & rb & xo_21_30 & 536 & rc.

            gen64s: [ gen: 'sradi' With: 31 & ra_to & rs_from & sh64 & xo_21_29 & 413 & rc ].
                      gen: 'srawi' With: 31 & ra_to & rs_from & sh   & xo_21_30 & 824 & rc.
            gen64s: [ gen: 'srad'  With: 31 & ra_to & rs_from & rb   & xo_21_30 & 794 & rc ].
                      gen: 'sraw'  With: 31 & ra_to & rs_from & rb   & xo_21_30 & 792 & rc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fixed-point instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateFixedPointTraps = ( |
             lengths.
             toConst.
            | 
            subcategory: 'fixed-point traps'.
            gen64s: [gen: 'tdi' With: 2 & to & ra & si].
                     gen: 'twi' With: 3 & to & ra & si.
            gen64s: [gen: 'td'  With: 31 & to & ra & rb & xo_21_30 & 68 & res_31].
                     gen: 'tw'  With: 31 & to & ra & rb & xo_21_30 &  4 & res_31.

            gen: 'trap' With: 31 & (constantFieldFor: to) & 31
                                 & (constantFieldFor: ra) & 0
                                 & (constantFieldFor: rb) & 0
                                 & xo_21_30 & 4 & res_31.

            lengths: list copyRemoveAll add: 'w'.
            gen64s: [lengths add: 'd'].
            lengths do: [|:len|
              gen: 't', len With: 31 & to_const & ra & rb 
                                     & xo_21_30 & (len = 'd' ifTrue: 68 False: 4)
                                     & res_31.
              gen: 't', len With:  (len = 'd' ifTrue: 2 False: 3)
                                    & to_const & ra & si & put_i_in_name.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: floating-point instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateFloatingPointAriths = ( |
            | 
            subcategory: 'floating-point arithmetics'.

            gen: 'fadd'   With: 63 & frt & fra & frb_with & res_21_25 & xo_26_30 &  21 & rc.
            gen: 'fadds'  With: 59 & frt & fra & frb_with & res_21_25 & xo_26_30 &  21 & rc.
            gen: 'fsub'   With: 63 & frt & fra & frb_with & res_21_25 & xo_26_30 &  20 & rc.
            gen: 'fsubs'  With: 59 & frt & fra & frb_with & res_21_25 & xo_26_30 &  20 & rc.
            gen: 'fmul'   With: 63 & frt & fra & res_16_20 & frc_times & xo_26_30 &  25 & rc.
            gen: 'fmuls'  With: 59 & frt & fra & res_16_20 & frc_times & xo_26_30 &  25 & rc.
            gen: 'fdiv'   With: 63 & frt & fra & frb_with & res_21_25 & xo_26_30 &  18 & rc.
            gen: 'fdivs'  With: 59 & frt & fra & frb_with & res_21_25 & xo_26_30 &  18 & rc.

            gen: 'fmadd'   With: 63 & frt & fra & frc_times & frb_plus  & xo_26_30 & 29 & rc.
            gen: 'fmadds'  With: 59 & frt & fra & frc_times & frb_plus  & xo_26_30 & 29 & rc.
            gen: 'fmsub'   With: 63 & frt & fra & frc_times & frb_minus & xo_26_30 & 28 & rc.
            gen: 'fmsubs'  With: 59 & frt & fra & frc_times & frb_minus & xo_26_30 & 28 & rc.
            gen: 'fnmadd'  With: 63 & frt & fra & frc_times & frb_plus  & xo_26_30 & 31 & rc.
            gen: 'fnmadds' With: 59 & frt & fra & frc_times & frb_plus  & xo_26_30 & 31 & rc.
            gen: 'fnmsub'  With: 63 & frt & fra & frc_times & frb_minus & xo_26_30 & 30 & rc.
            gen: 'fnmsubs' With: 59 & frt & fra & frc_times & frb_minus & xo_26_30 & 30 & rc.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: floating-point instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateFloatingPointCompares = ( |
            | 
            subcategory: 'floating-point comparisons'.
            gen: 'fcmpu'   With: 63 & bf & res_9_10 & fra & frb_with & xo_21_30 &   0 & res_31.
            gen: 'fcmpo'   With: 63 & bf & res_9_10 & fra & frb_with & xo_21_30 &  32 & res_31.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: floating-point instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateFloatingPointLoads = ( |
            | 
            subcategory: 'floating-point loads'.
            gen: 'lfs'   With: 48 & frt & d & ra_base0.
            gen: 'lfsx'  With: 31 & frt & ra0 & rb & xo_21_30 & 535 & res_31.
            gen: 'lfsu'  With: 49 & frt & d & ra_base
                         Pred: [ra_baseArg != r0] Bad: (f5 & 8 & r0).
            gen: 'lfsux' With: 31 & frt & ra & rb & xo_21_30 & 567 & res_31
                         Pred: [raArg != r0] Bad: (f6 & r0 & r3).

            gen: 'lfd'   With: 50 & frt & d & ra_base0.
            gen: 'lfdx'  With: 31 & frt & ra0 & rb & xo_21_30 & 599 & res_31.
            gen: 'lfdu'  With: 51 & frt & d & ra_base
                         Pred: [ra_baseArg != r0] Bad: (f5 & 8 & r0).
            gen: 'lfdux' With: 31 & frt & ra & rb & xo_21_30 & 631 & res_31
                         Pred: [raArg != r0] Bad: (f6 & r0 & r3).

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: floating-point instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateFloatingPointMoves = ( |
            | 
            subcategory: 'floating-point moves'.
            gen: 'fmr'   With: 63 & frt & res_11_15 & frb_from & xo_21_30 &  72 & rc.
            gen: 'fneg'  With: 63 & frt & res_11_15 & frb_from & xo_21_30 &  40 & rc.
            gen: 'fabs'  With: 63 & frt & res_11_15 & frb_from & xo_21_30 & 264 & rc.
            gen: 'fnabs' With: 63 & frt & res_11_15 & frb_from & xo_21_30 & 136 & rc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: floating-point instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateFloatingPointRoundsAndCvts = ( |
            | 
            subcategory: 'floating-point rounding & conversions'.
            gen: 'frsp'   With: 63 & frt & res_11_15 & frb_from & xo_21_30 &  12 & rc.
            gen64s: [
            gen: 'fctid'  With: 63 & frt & res_11_15 & frb_from & xo_21_30 & 814 & rc.
            gen: 'fctidz' With: 63 & frt & res_11_15 & frb_from & xo_21_30 & 815 & rc.
            ].
            gen: 'fctiw'  With: 63 & frt & res_11_15 & frb_from & xo_21_30 &  14 & rc.
            gen: 'fctiwz' With: 63 & frt & res_11_15 & frb_from & xo_21_30 &  15 & rc.
            gen64s: [
            gen: 'fcfid'  With: 63 & frt & res_11_15 & frb_from & xo_21_30 & 846 & rc.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: floating-point instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateFloatingPointStatusAndCRs = ( |
            | 
            subcategory: 'floating-point status and control regs.'.
            gen: 'mffs'    With: 63 & frt & res_11_15 & res_16_20 & xo_21_30 & 583 & rc.
            gen: 'mcrfs'   With: 63 & bf_to & res_9_10 & bfa_float & res_14
                               & res_16_20 & xo_21_30 &  64 & res_31.
            gen: 'mtfsfi'  With: 63 & bf_to_float & res_9_10 & res_11_15 & u & res_20
                               & xo_21_30 & 134 & rc.
            gen: 'mtfsf'   With: 63 & res_6 & flm & res_15 & frb_from & xo_21_30 & 711 & rc.
            gen: 'mtfsb0' With: 63 & bt & res_11_15 & res_16_20 & xo_21_30 &  70 & rc.
            gen: 'mtfsb1' With: 63 & bt & res_11_15 & res_16_20 & xo_21_30 &  38 & rc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: floating-point instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateFloatingPointStores = ( |
            | 
            subcategory: 'floating-point stores'.

            gen: 'stfs'   With: 52 & frs & d & ra_base0.
            gen: 'stfsx'  With: 31 & frs & ra0 & rb & xo_21_30 & 663 & res_31.

            gen: 'stfsu'  With: 53 & frs & d & ra_base
                          Pred: [ra_baseArg != r0] Bad: (f5 & 8 & r0).
            gen: 'stfsux' With: 31 & frs & ra & rb & xo_21_30 & 695 & res_31
                          Pred: [raArg != r0] Bad: (f6 & r0 & r3).

            gen: 'stfd'   With: 54 & frs & d & ra_base0.
            gen: 'stfdx'  With: 31 & frs & ra0 & rb & xo_21_30 & 727 & res_31.

            gen: 'stfdu'  With: 55 & frs & d & ra_base
                         Pred: [ra_baseArg != r0] Bad: (f5 & 8 & r0).
            gen: 'stfdux' With: 31 & frs & ra & rb & xo_21_30 & 759 & res_31
                         Pred: [raArg != r0] Bad: (f6 & r0 & r3).

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: branch processor\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateGeneralBranches = ( |
             cr0.
            | 
            subcategory: 'branches'.
            cr0: constantFieldFor: br_cr.

            gen: 'b'     With: 18 & li & lk & aa.

            ignoreExternalPrediction:
            gen: 'bc'    With: 16 & bo & bi & bd & lk & aa.

            gen: 'bclr'  With: 19 & bo & bi & res_16_20 & xo_21_30 &  16 & lk.

            dontExternallyTestBaddies: "asm does not fail for 12,3"
            gen: 'bcctr' With: 19 & bo & bi & res_16_20 & xo_21_30 & 528 & lk
                         Pred: [(boArg && 4) = 4]
                         Bad:  12 & 3.

            gen: 'sc'    With: 17 & res_6_10 & res_11_15 & res_16_29
                             & bit_30 & 1 & res_31.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fixed-point instructions\x7fCategory: fixed-point loads and stores\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateLoadStoreMultiple = ( |
            | 
            subcategory: 'load & store multiple'.
            gen: 'lmw' With: 46 & rt & d & ra_base0
                       Pred: [(ra_base0Arg value < rtArg value) 
                         &&  [(ra_base0Arg value != 0) || [rtArg != r0]]]
                       Bads: (r5 & 0 & r5) asVector  &  (r0 & 8 & r0) asVector.

            gen: 'stmw' With: 47 & rs_from & d & ra_base0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fixed-point instructions\x7fCategory: fixed-point loads and stores\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateLoads = ( |
            | 
            "field order is determined by external assembler order"

            subcategory: 'loads'.
            gen: 'lbz'   With: 34 & rt &  d & ra_base0.
            gen: 'lbzx'  With: 31 & rt & ra0 & rb & xo_21_30 &  87 & res_31.

            gen: 'lbzu'  With: 35 & rt &  d & ra_base
                         Pred: [(ra_baseArg != rtArg) && [ra_baseArg != r0]]
                         Bads: (r5 & 4 & r5) asVector & (r5 & 8 & r0) asVector.
            gen: 'lbzux' With: 31 & rt & ra & rb & xo_21_30 & 119 & res_31
                         Pred: [(raArg != rtArg) && [raArg != r0]]
                         Bads: (r4 & r4 & r5) asVector & (r5 & r0 & r6) asVector.

            gen: 'lhz'   With: 40 & rt &  d & ra_base0.
            gen: 'lhzx'  With: 31 & rt & ra0 & rb & xo_21_30 & 279 & res_31.

            gen: 'lhzu'  With: 41 & rt &  d & ra_base
                         Pred: [(ra_baseArg != rtArg) && [ra_baseArg != r0]]
                         Bads: (r5 & 4 & r5) asVector & (r5 & 8 & r0) asVector.
            gen: 'lhzux' With: 31 & rt & ra & rb & xo_21_30 & 311 & res_31
                         Pred: [(raArg != rtArg) && [raArg != r0]]
                         Bads: (r4 & r4 & r5) asVector & (r5 & r0 & r6) asVector.

            gen: 'lha'   With: 42 & rt &  d & ra_base0.
            gen: 'lhax'  With: 31 & rt & ra0 & rb & xo_21_30 & 343 & res_31.

            gen: 'lhau'  With: 43 & rt &  d & ra_base
                         Pred: [(ra_baseArg != rtArg) && [ra_baseArg != r0]]
                         Bads: (r5 & 4 & r5) asVector & (r5 & 8 & r0) asVector.
            gen: 'lhaux' With: 31 & rt & ra & rb & xo_21_30 & 375 & res_31
                         Pred: [(raArg != rtArg) && [raArg != r0]]
                         Bads: (r4 & r4 & r5) asVector & (r5 & r0 & r6) asVector.

            gen: 'lwz'   With: 32 & rt &  d & ra_base0.
            gen: 'lwzx'  With: 31 & rt & ra0 & rb & xo_21_30 &  23 & res_31.

            gen: 'lwzu'  With: 33 & rt &  d & ra_base
                         Pred: [(ra_baseArg != rtArg) && [ra_baseArg != r0]]
                         Bads: (r5 & 4 & r5) asVector & (r5 & 8 & r0) asVector.
            gen: 'lwzux' With: 31 & rt & ra & rb & xo_21_30 &  55 & res_31
                         Pred: [(raArg != rtArg) && [raArg != r0]]
                         Bads: (r4 & r4 & r5) asVector & (r5 & r0 & r6) asVector.

            gen64s: ["64-bit only"
            gen: 'lwa'   With: 58 & rt &  ds & ra_base0 & xo_30_31 & 2.
            gen: 'lwax'  With: 31 & rt & ra0 & rb & xo_21_30 & 341 & res_31.
            gen: 'lwaux' With: 31 & rt & ra & rb & xo_21_30 & 373 & res_31
                         Pred: [(raArg != rtArg) && [raArg != r0]]
                         Bads: (r4 & r4 & r5) asVector & (r5 & r0 & r6) asVector.

            gen: 'ld'    With: 58 & rt & ds & ra_base0 & xo_30_31 & 0.
            gen: 'ldx'   With: 31 & rt & ra0 & rb & xo_21_30 &  21 & res_31.
            gen: 'ldu'   With: 58 & rt & ds & ra_base & xo_30_31 & 1
                         Pred: [(ra_baseArg != rtArg) && [ra_baseArg != r0]]
                         Bads: (r5 & 4 & r5) asVector & (r5 & 8 & r0) asVector.
            gen: 'ldux'  With: 31 & rt & ra & rb & xo_21_30 &  53 & res_31
                         Pred: [(raArg != rtArg) && [raArg != r0]]
                         Bads: (r4 & r4 & r5) asVector & (r5 & r0 & r6) asVector.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fixed-point instructions\x7fCategory: fixed-point loads and stores\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateLoadsAndStores = ( |
            | 
            generateLoads.
            generateStores.
            generateByteReversals.
            generateLoadStoreMultiple.
            generateSynchronizationInstructions.
            generateMoveAssists).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fixed-point instructions\x7fCategory: fixed-point loads and stores\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateMoveAssists = ( |
            | 
            subcategory: 'move assists'.

            gen: 'lswi' With: 31 & rt & ra0 & nb & xo_21_30 & 597 & res_31
                        Pred: [ "ensure ra is not in range that is loaded"
                                 ( (ra0Arg != r0) || [rtArg != r0] )  
                              && [|numRegs. lastReg. wrapsAround|
                                  numRegs: (nbArg + 3) / 4.
                                  lastReg: (rtArg value + numRegs pred) % 32.
                                  wrapsAround: lastReg < rtArg value.
                                  wrapsAround ifTrue: [ (lastReg      < ra0Arg value)
                                                  &&    [ra0Arg value < rtArg  value]]
                                               False: [ (ra0Arg value < rtArg  value)
                                                  ||    [lastReg      < ra0Arg value] ]   ]
                        ]
                        Bads: (r0  & r0  & 32) asVector
                           &  (r31 & r3  & 17) asVector
                           &  (r5  & r5  &  1) asVector
                           &  (r5  & r6  &  5) asVector
                           &  (r4  & r11 & 32) asVector.

            gen: 'lswx' With: 31 & rt & ra0 & rb & xo_21_30 & 533 & res_31
                        Pred: [((ra0Arg != r0   ) || [rtArg != r0   ]) 
                           && [ (ra0Arg != rtArg) && [rbArg != rtArg]]]
                        Bads: (r0 & r0 & r4) asVector
                           &  (r5 & r5 & r6) asVector 
                           &  (r5 & r6 & r5) asVector.

            gen: 'stswi' With: 31 & rs_from & ra0 & nb & xo_21_30 & 725 & res_31.

            gen: 'stswx' With: 31 & rs_from & ra0 & rb & xo_21_30 & 661 & res_31).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fixed-point instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateMoveToFromSystemRegisters = ( |
            | 
            subcategory: 'move to/from system registers'.
            dontExternallyTestBaddies: "asm allows illegals vals for spr"
            gen: 'mtspr' With: 31 & spr_to   & rs_from & xo_21_30 & 467 & res_31.
            gen: 'mt'    With: 31 & spr_opts & rs_from & xo_21_30 & 467 & res_31.

            dontExternallyTestBaddies: "asm allows illegals vals for spr"
            gen: 'mfspr' With: 31 & rt & spr_from      & xo_21_30 & 339 & res_31.
            gen: 'mf'    With: 31 & rt & spr_opts      & xo_21_30 & 339 & res_31.

            gen: 'mtcrf' With: 31 & fxm & rs_from & res_11 & res_20
                             & xo_21_30 & 144 & res_31.
            gen: 'mcrxr' With: 31 & bf & res_9_10 & res_11_15 & res_16_20
                             & xo_21_30 & 512 & res_31.
            gen: 'mfcr'  With: 31 & rt & res_11_15 & res_16_20
                             & xo_21_30 & 19 & res_31.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: branch processor\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateSimpleBranchMnemonics = ( |
             biConst.
             boConst.
            | 
            boConst: constantFieldFor: bo.
            biConst: constantFieldFor: bi.

            "unconditionals:"
            gen: 'blr'  With: 19 & boConst & 20 & biConst & 0 & res_16_20 & xo_21_30 &  16 & lk.
            gen: 'bctr' With: 19 & boConst & 20 & biConst & 0 & res_16_20 & xo_21_30 & 528 & lk.

            gen: 'b'  With: 19 & bo_simple_conds_with_bi & bi & res_16_20
                                 & xo_21_30 & 16 & put_lr_in_name & lk & reg_prediction.
            gen: 'b'  With: 19 & bo_simple_conds_no_bi & biConst & 0 & res_16_20
                                 & xo_21_30 & 16 & put_lr_in_name & lk & reg_prediction.

            dontExternallyTestGoodies: "gnu asm asms bfctr+ 0 as bflr+ 0"
            gen: 'b'     With: 19 & bo_cond_true_or_false & bi
                                  & res_16_20 & xo_21_30 & 528
                                  & put_ctr_in_name & lk & reg_prediction.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: storage control instructions\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateStorageControlInstructions = ( |
            | 
            gen: 'icbi'   With: 31 & res_6_10 & ra0 & rb & xo_21_30 &  982 & res_31.
            gen: 'dcbt'   With: 31 & res_6_10 & ra0 & rb & xo_21_30 &  278 & res_31.
            gen: 'dcbtst' With: 31 & res_6_10 & ra0 & rb & xo_21_30 &  246 & res_31.
            gen: 'dcbz'   With: 31 & res_6_10 & ra0 & rb & xo_21_30 & 1014 & res_31.
            gen: 'dcbst'  With: 31 & res_6_10 & ra0 & rb & xo_21_30 &   54 & res_31.
            gen: 'dcbf'   With: 31 & res_6_10 & ra0 & rb & xo_21_30 &   86 & res_31.

            gen: 'isync'  With: 19 & res_6_10 & res_11_15 & res_16_20 & xo_21_30 & 150 & res_31.
            gen: 'eieio'  With: 31 & res_6_10 & res_11_15 & res_16_20 & xo_21_30 & 854 & res_31.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fixed-point instructions\x7fCategory: fixed-point loads and stores\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateStores = ( |
            | 
            subcategory: 'stores'.

            gen: 'stb'   With: 38 & rs_from & d & ra_base0.
            gen: 'stbx'  With: 31 & rs_from & ra0 & rb & xo_21_30 & 215 & res_31.
            gen: 'stbu'  With: 39 & rs_from & d & ra_base
                         Pred: [ra_baseArg != r0]  Bad:  r5 & 8 & r0.
            gen: 'stbux' With: 31 & rs_from & ra & rb & xo_21_30 & 247 & res_31
                         Pred: [raArg != r0]  Bad: r5 & r0 & r6.

            gen: 'sth'   With: 44 & rs_from & d & ra_base0.
            gen: 'sthx'  With: 31 & rs_from & ra0 & rb & xo_21_30 & 407 & res_31.
            gen: 'sthu'  With: 45 & rs_from & d & ra_base
                         Pred: [ra_baseArg != r0]  Bad:  r5 & 8 & r0.
            gen: 'sthux' With: 31 & rs_from & ra & rb & xo_21_30 & 439 & res_31
                         Pred: [raArg != r0]  Bad: r5 & r0 & r6.

            gen: 'stw'   With: 36 & rs_from & d & ra_base0.
            gen: 'stwx'  With: 31 & rs_from & ra0 & rb & xo_21_30 & 151 & res_31.
            gen: 'stwu'  With: 37 & rs_from & d & ra_base
                         Pred: [ra_baseArg != r0]  Bad:  r5 & 8 & r0.
            gen: 'stwux' With: 31 & rs_from & ra & rb & xo_21_30 & 183 & res_31
                         Pred: [raArg != r0]  Bad: r5 & r0 & r6.

            gen64s: ["64-bit only"
            gen: 'std'   With: 62 & rs_from & ds & ra_base0 & xo_30_31 & 0.
            gen: 'stdx'  With: 31 & rs_from & ra0 & rb & xo_21_30 & 149 & res_31.
            gen: 'stdu'  With: 62 & rs_from & ds & ra_base & xo_30_31 & 1
                         Pred: [ra_baseArg != r0]  Bad:  r5 & 8 & r0.
            gen: 'stdux' With: 31 & rs_from & ra & rb & xo_21_30 & 181 & res_31
                         Pred: [raArg != r0]  Bad: r5 & r0 & r6.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fixed-point instructions\x7fCategory: fixed-point loads and stores\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateSynchronizationInstructions = ( |
            | 
            gen: 'lwarx' With: 31 & rt & ra0 & rb & xo_21_30 & 20 & res_31.
            gen64s: ["64-bit"
              gen: 'ldarx' With: 31 & rt & ra0 & rb & xo_21_30 & 84 & res_31.
            ].
            gen: 'stwcx_' With: 31 & rs_from & ra0 & rb & xo_21_30 & 150 & bit_31 & 1.
            gen64s: ["64"
              gen: 'stdcx_' With: 31 & rs_from & ra0 & rb & xo_21_30 & 214 & bit_31 & 1.
            ].
            gen: 'sync' With: 31 & res_6_10 & res_11_15 & res_16_20 & xo_21_30 & 598 & res_31.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         operands* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         ppcMixins* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'ppcMixins' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators instructionTemplates parent ppcMixins.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'ppcMixins' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         gen64s: blk = ( |
            | 
            "eval blk when generating 64-bit instructions"
            blk value.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'ppcMixins' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         ignoreExternalPrediction: its = ( |
            | 
            "gnu asm is incomprehensible"
            ignoreExternalBits: 10 & 10 Templates: its.
            its).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: customize for PPC\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         proto = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators instructionTemplates parent proto copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators instructionTemplates parent proto.

CopyDowns:
globals assemblerSystems framework generators instructionTemplates parent proto. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: customize for PPC\x7fComment: Needed to disambiguate from the global ui object.\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         ui = ( |
            | fields.ui).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         pseudoInstructionTemplates = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators pseudoInstructionTemplates copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators pseudoInstructionTemplates.

CopyDowns:
globals assemblerSystems framework generators pseudoInstructionTemplates. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators pseudoInstructionTemplates parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         combineLink: hasL AndDst: dst = ( |
            | 
            dst size <= 1 ifTrue: [hasL, dst] False: [dst, hasL]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         fields* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'fields' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: branches\x7fComment: xxx These hacks here are artifacts of
     pseudoInstructionTemplate construction \"tricks\". BM 12/18/01
\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         fixBranches: its Dst: dst Link: hasL Pred: p = ( |
            | 
            "name is already set, so just add option fields"

            its do: [|:i| i addOptionField: lk.
                          i addOptionField: aa].

            "and include present options"
            dst  = 'a'   ifTrue: [ its do: [|:i| i addOption: 'a' OfField: aa ]].
            hasL = 'l'   ifTrue: [ its do: [|:i| i addOption: 'l' OfField: lk ]].

            its).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateAll = ( |
            | 
            start.
            generateBranchesWithConditions.
            generateCRLogicals.
            generateSubtracts.
            generateDblRotates.
            generateRotates.
            generateMiscs.
            finish).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateBranchWithCond: cond Dst: dst Links: hasL Pred: p = ( |
             fields.
             isU.
             linkAndDst.
             mnem.
             r1.
             r2.
            | 
            linkAndDst: hasL, dst.
            mnem: 'b', cond, linkAndDst.
            isU: isUnusualScriptForDst: dst Pred: p.

            r1:    gen: mnem, p
                 Field: bd
                Script: isU, '\n',
                        '  ifTrue: [ ', mnem, 'UnusualDisp: bdArg ]\n',
                        '   False: [',  mnem, 'Disp: bdArg]'.

            r2:    gen: mnem, p
                Fields: bf_no8 & bd
                Script: isU, '\n',
                        '  ifTrue: [ ', mnem, 'UnusualCRField: bf_no8Arg Disp: bdArg ]\n',
                        '   False: [ ', mnem,        'CRField: bf_no8Arg Disp: bdArg]'.

            fixBranches: r1, r2 Dst: dst Link: hasL Pred: p.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateBranchesWithConditions = ( |
            | 
            generatePredictedImmediateConditionalBranches.
            [generateUnAndNSBranches.]. "don't seem to need these"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: condition register logical mnemonics\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateCRLogicals = ( |
            | 
            subcategory: 'cond reg logicals'.
            gen: 'crset' Field: bt  Script: 'creqvToBit: btArg FromBit: btArg WithBit: btArg'.
            gen: 'crclr' Field: bt  Script: 'crxorToBit: btArg FromBit: btArg WithBit: btArg'.

            gen: 'crmove' Fields: bt & ba  Script: 'crorToBit:  btArg FromBit: baArg WithBit: baArg'.
            gen: 'crnot'  Fields: bt & ba  Script: 'crnorToBit: btArg FromBit: baArg WithBit: baArg'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: rotates and shifts\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateDblRotates = ( |
            | 
              subcategory: 'double word rotate & shifts'.
            gen64s: [
            ('' & '_') asVector do: [|:s|
              dontExternallyTestBaddies:
              gen: 'extldi', s  Fields:  ra_to & rs_from & numBits64 & sh64
              Script: 'rldicr', s, 'To: ra_toArg From: rs_fromArg By: sh64Arg MaskEnd: numBits64Arg - 1'.

              dontExternallyTestBaddies:
              gen: 'extrdi', s  Fields:  ra_to & rs_from & numBits64 & sh64
              Script: 'rldicl', s, 'To: ra_toArg From: rs_fromArg ',
                      'By: sh64Arg + numBits64Arg MaskBegin: 64 - numBits64Arg'.

              dontExternallyTestBaddies:
              gen: 'insrdi', s  Fields:  ra_to & rs_from & numBits64 & sh64
              Script: 'rldimi', s, 'To: ra_toArg From: rs_fromArg ',
                      'By: 64 - (sh64Arg + numBits64Arg) MaskBegin: sh64Arg'.

              dontExternallyTestBaddies:
              gen: 'rotrdi', s  Fields: ra_to & rs_from & sh64
              Script: 'rldicl', s, 'To: ra_toArg From: rs_fromArg By: 64 - sh64Arg MaskBegin: 0'.

              dontExternallyTestBaddies:
              gen: 'sldi', s  Fields: ra_to & rs_from & sh64
              Script: 'rldicr', s, 'To: ra_toArg From: rs_fromArg By: sh64Arg MaskEnd: 63 - sh64Arg'.

              dontExternallyTestBaddies:
              gen: 'srdi', s  Fields: ra_to & rs_from & sh64
              Script: 'rldicl', s, 'To: ra_toArg From: rs_fromArg By: 64 - sh64Arg MaskBegin: sh64Arg'.

              dontExternallyTestBaddies:
              gen: 'clrrdi', s  Fields: ra_to & rs_from & sh64
              Script: 'rldicr', s, 'To: ra_toArg From: rs_fromArg By: 0 MaskEnd: 63 - sh64Arg'.

              dontExternallyTestBaddies:
              gen: 'clrlsldi', s  Fields: ra_to & rs_from & mb64 & sh64
              Script: 'rldic', s, 'To: ra_toArg From: rs_fromArg By: sh64Arg MaskBegin: mb64Arg - sh64Arg'.

            ]].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: move to/from specials & miscs\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateMiscs = ( |
             g.
            | 
            subcategory: 'miscellaneous'.
            gen: 'la'  Fields: rt & d & ra_base
                       Script: 'ra_baseArg = r0 ifTrue: [^ error: \'raArg cannot be r0\'].\n',
                               'addiTo: rtArg From: ra_baseArg With: dArg'.

            ('' & '_') asVector do: [|:u|
              gen: 'mr', u   Fields: rt & ra  Script: 'or', u,  'To: rtArg From: raArg With: raArg'.
              gen: 'not', u  Fields: rt & ra  Script: 'nor', u, 'To: rtArg From: raArg With: raArg'
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generatePredictedImmediateConditionalBranches = ( |
            | 
            subcategory: 'branches mnemonics with conditions'.
            ('Taken' & 'Untaken') asVector do: [|:taken|
              ('' & 'l') asVector do: [|:hasL|
                ('' & 'a' ) asVector do: [|:dst|
                  ( 'lt' & 'le' & 'eq' & 'ge' & 'gt' & 'ne'
                  & 'so' & 'ns' ) asVector do: [|:cond|
                    generateBranchWithCond: cond Dst: dst Links: hasL Pred: taken
            ]]]].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: rotates and shifts\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateRotates = ( |
            | 
            subcategory: 'word rotate & shifts'.
            ('' & '_') asVector do: [|:s|
              dontExternallyTestBaddies:
              gen: 'extlwi', s  Fields:  ra_to & rs_from & numBits32 & sh
              Script: 'rlwinm', s, 'To: ra_toArg From: rs_fromArg By: shArg MaskBegin: 0 MaskEnd: numBits32Arg - 1'.

              dontExternallyTestBaddies:
              gen: 'extrwi', s  Fields:  ra_to & rs_from & numBits32 & sh
              Script: 'rlwinm', s, 'To: ra_toArg From: rs_fromArg ',
                      'By: shArg + numBits32Arg MaskBegin: 32 - numBits32Arg MaskEnd: 31'.

              dontExternallyTestBaddies:
              gen: 'inslwi', s  Fields:  ra_to & rs_from & numBits32 & sh
              Script: 'rlwimi', s, 'To: ra_toArg From: rs_fromArg ',
                      'By: 32 - shArg MaskBegin: shArg MaskEnd: shArg + numBits32Arg pred'.

              dontExternallyTestBaddies:
              gen: 'insrwi', s  Fields:  ra_to & rs_from & numBits32 & sh
              Script: 'rlwimi', s, 'To: ra_toArg From: rs_fromArg ',
                      'By: 32 - (shArg + numBits32Arg) MaskBegin: shArg MaskEnd: shArg + numBits32Arg pred'.

              dontExternallyTestBaddies:
              gen: 'rotrwi', s  Fields: ra_to & rs_from & sh
              Script: 'rlwinm', s, 'To: ra_toArg From: rs_fromArg By: 32 - shArg MaskBegin: 0 MaskEnd: 31'.

              dontExternallyTestBaddies:
              gen: 'slwi', s  Fields: ra_to & rs_from & sh
              Script: 'rlwinm', s, 'To: ra_toArg From: rs_fromArg By: shArg MaskBegin: 0 MaskEnd: 31 - shArg'.

              dontExternallyTestBaddies:
              gen: 'srwi', s  Fields: ra_to & rs_from & sh
              Script: 'rlwinm', s, 'To: ra_toArg From: rs_fromArg By: 32 - shArg MaskBegin: shArg MaskEnd: 31'.

              dontExternallyTestBaddies:
              gen: 'clrrwi', s  Fields: ra_to & rs_from & sh
              Script: 'rlwinm', s, 'To: ra_toArg From: rs_fromArg By: 0 MaskBegin: 0 MaskEnd: 31 - shArg'.

              dontExternallyTestBaddies:
              gen: 'clrlslwi', s  Fields: ra_to & rs_from & mb & sh
              Script: 'rlwinm', s, 'To: ra_toArg From: rs_fromArg By: shArg ',
                      'MaskBegin: mbArg - shArg MaskEnd: 31 - shArg'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: subtracts\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateSubtracts = ( |
            | 
            subcategory: 'subtracts'.
            ('' & 's' & 'c' & 'c_') asVector do: [|:suf|
              dontExternallyTestBaddies:
                gen: 'subi', suf
              Fields:  rt & ra & si
              Script: 'addi', suf, 'To: rtArg From: raArg With: siArg negate'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         isUnusualScriptForDst: dst Pred: p = ( |
             goesBack.
             valueOfOperand.
            | 
            valueOfOperand: '(bdArg value: instTemplate With: locationCounter)'.
            goesBack: case
             if: [dst = '' ] Then: '((', valueOfOperand, ' - locationCounter) < 0)'
             If: [dst = 'a'] Then: '(', valueOfOperand, ' < 0)'
                             Else: [error: 'bad dst'].

            case
             if: [p = 'Taken'  ] Then: [goesBack, ' not']
             If: [p = 'Untaken'] Then: [goesBack        ]
             Else: [error: 'what?']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         operands* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         ppcMixins* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'ppcMixins' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         proto = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators pseudoInstructionTemplates parent proto copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators pseudoInstructionTemplates parent proto.

CopyDowns:
globals assemblerSystems framework generators pseudoInstructionTemplates parent proto. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: InitializeToExpression: (false)'
        
         hasRedundantOption <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators pseudoInstructionTemplates parent proto parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         architecture = 'ppc'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         sprs = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'sprs' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators registers copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'sprs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators sprs.

CopyDowns:
globals assemblerSystems framework generators registers. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'sprs' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'sprs' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc generators sprs parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'sprs' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         generateAll = ( |
            | 
            start.
            generateConstantNamed: 'xer' Number: 1.
            generateConstantNamed: 'lr'  Number: 8.
            generateConstantNamed: 'ctr' Number: 9.
            finish).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'generators' -> 'sprs' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         asmPPCGens = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCGens' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCGens' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmPPCGens.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCGens' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmPPC'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCGens' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCGens' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCGens' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCGens' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.29 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCGens' -> () From: ( | {
         'ModuleInfo: Module: asmPPCGens InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules asmPPCGens postFileIn
