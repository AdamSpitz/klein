 '$Revision: 30.21 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: integer operand fields\x7fCategory: prototypes\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         intOperandField = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators fields parent operandField copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent intOperandField.

CopyDowns:
globals assemblerSystems framework generators fields parent operandField. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         isBackpatchable <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: InitializeToExpression: (false)'
        
         isRelative <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent intOperandField parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         adjust: operand IfRelativeAt: lc IT: instTemplate = ( |
             v.
            | 
            v: operand value: instTemplate With: lc. "might be a label"
            (isRelativeForIT: instTemplate)
              ifTrue: [intNN sub: v With: relativeOperandOffsetAt: lc IT: instTemplate]
               False: [v]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         assemble: operand At: lc IT: instTemplate IfSucceed: okBlk IfFail: failBlk = ( |
             v.
             w.
            | 
            v: adjust: operand IfRelativeAt: lc IT: instTemplate.
            w: shift: v IfFail: [|:e| ^ failBlk value: e].
            signOps insert: w Into: where IfSucceed: okBlk IfFail: failBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         beBackpatchable = ( |
            | isBackpatchable: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         beSigned = ( |
            | 
            signOps: signDependentOperations signed).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         beSignedOrUnsigned = ( |
            | 
            signOps: signDependentOperations signedOrUnsigned).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         beUnsigned = ( |
            | 
            signOps: signDependentOperations unsigned).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         disassembleValueIn: inst At: lc IT: instTemplate = ( |
             r.
            | 
            r: signOps extract: inst From: where.
            r: unshift: r.
            unadjust: r IfRelativeAt: lc IT: instTemplate).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         isInteger = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         max = ( |
            | 
            signOps maxForBitRange: where).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         min = ( |
            | 
            signOps minForBitRange: where).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'operandField' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         relativeOperandOffsetAt: lc IT: instTemplate = ( |
            | 
            intNN add: lc With: instTemplate size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         shift: i IfFail: fb = ( |
            | 
            i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         signDependentOperations = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent intOperandField parent signDependentOperations.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         unadjust: operand IfRelativeAt: lc IT: instTemplate = ( |
            | 
            (isRelativeForIT: instTemplate)
              ifTrue: [intNN  add: (relativeOperandOffsetAt: lc IT: instTemplate)
                             With: operand]
               False: [operand]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         unshift: i = ( |
            | i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: InitializeToExpression: (assemblerSystems framework generators fields intOperandField signDependentOperations unsigned)'
        
         signOps <- bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'unsigned' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> () From: ( | {
         'Category: caching operands\x7fModuleInfo: Module: asmFrame3 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedLegalOperands.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: InitializeToExpression: (nil)'
        
         max.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> () From: ( | {
         'Category: subsetting\x7fModuleInfo: Module: asmFrame3 InitialContents: InitializeToExpression: (maxSmallInt)'
        
         maxValue <- maxSmallInt.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: InitializeToExpression: (nil)'
        
         min.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> () From: ( | {
         'Category: subsetting\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         minValue <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: InitializeToExpression: (())'
        
         operandNameSpace <- ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent symbolicOperandField parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         assemble: operand At: lc IT: instTemplate IfSucceed: okBlk IfFail: failBlk = ( |
             ns.
             ons.
            | 
            ns:   operand nameSpace.
            ons:  operandNameSpace.
            "Small possible future optimization: use _Eq: to avoid creating two mirrors. -- Adam, 12/04"
            (reflect: ns) = (reflect: ons) ifFalse: [^ failBlk value: 'operand is not in the expected set of operands'].

              ifValue:  operand value
              IsLegal:  []
            IfIllegal:  [|:e| ^ failBlk value: 'this register is not allowed here: ', e].

            where fromUnsignedInteger: operand value
                            IfSucceed: okBlk
                               IfFail: failBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: generating tests & testing the assembler\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildLegalOperandDictionary = ( |
             r.
            | 
            r: dictionary copyRemoveAll.
            operands do: [|:op| r at: op value Put: op].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: accesing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         computeMax = ( |
             i <- 0.
             r.
            | 
            i: minSmallInt.
            operands do: [|:op| 
              op value > i  ifTrue: [i: op value. r: op]
            ].
            max: r.
            maxValue: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: accesing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         computeMin = ( |
             i <- 0.
             r.
            | 
            i: maxSmallInt.
            operands do: [|:op| 
              op value < i  ifTrue: [i: op value. r: op]
            ].
            min: r.
            minValue: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         copyName: n Bits: bits Keyword: k In: nameSpace Asm: myAsm = ( |
             r.
            | 
            r: copyName: n Bits: bits Keyword: k Asm: myAsm.
            r operandNameSpace: nameSpace.
            r computeMin computeMax.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: accesing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         ifValue: v IsLegal: goodBlk IfIllegal: badBlk = ( |
            | 
            minValue   <= v         ifFalse: [^ badBlk value: 'too small'].
            v          <= maxValue  ifFalse: [^ badBlk value: 'too big'  ].
            (v % radix) = 0         ifFalse: [^ badBlk value: 'not divisible by ', radix printString].

            goodBlk value: v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         isSymbolicForValue: operand = ( |
            | 
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: accesing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         isValueLegal: v = ( |
            | ifValue: v  IsLegal: true  IfIllegal: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         operandForValue: i = ( |
            | 
            operands findFirst: [|:op| op value = i]
                     IfPresent: [|:op| op]
                      IfAbsent: [resend.operandForValue: i]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: accesing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         operands = ( |
            | 
            cachedLegalOperands ifNil: [
              cachedLegalOperands:  potentialOperands copyFilteredBy: [|:op| isValueLegal: op value].
              cachedLegalOperands
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'operandField' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: accesing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         potentialOperands = ( |
            | 
            (reflect: operandNameSpace) asList copyMappedBy: [|:s| s contents reflectee]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: accesing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         setMinValue: min MaxValue: max Radix: radix = ( |
            | 
            minValue: min.
            maxValue: max.
            radix: radix.
            computeMin computeMax).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         sourceForValue: i = ( |
            | 
            operands findFirst: [|:op| op value = i]
                     IfPresent: [|:op| op name]
                      IfAbsent: [resend.sourceForValue: i]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: generating tests & testing the assembler\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         unfilteredIllegalTestCasesDo: blk = ( |
             ops.
            | 
            ops: buildLegalOperandDictionary.
            min value  to:  max value  Do: [|:i|
              (ops includesKey: i) ifFalse: [blk value: i].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> 'parent' -> () From: ( | {
         'Category: generating tests & testing the assembler\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         unfilteredLegalTestCases = ( |
            | 
            buildLegalOperandDictionary asList copySort).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> () From: ( | {
         'Category: subsetting\x7fModuleInfo: Module: asmFrame3 InitialContents: InitializeToExpression: (1)'
        
         radix <- 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         rangeFrom <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: integer operand fields\x7fCategory: prototypes\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         zeroIsMaxOperandField = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandField' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators fields parent operandField copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent zeroIsMaxOperandField.

CopyDowns:
globals assemblerSystems framework generators fields parent operandField. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandField' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent zeroIsMaxOperandField parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandField' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         assemble: operand At: lc IT: instTemplate IfSucceed: okBlk IfFail: failBlk = ( |
             v.
            | 

            v: (intNN eq: operand With: 0) ifTrue: [max] False: [operand].

            resend.assemble: v At: lc IT: instTemplate IfSucceed: okBlk IfFail: failBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandField' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         disassembleValueIn: inst At: lc IT: instTemplate = ( |
             r.
            | 
            r: where asUnsignedIntegerFrom: inst.
            r: (intNN eq: r With: 0) ifTrue: [max] False: [r].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandField' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         max = ( |
            | 
            intNN add: resend.max With: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandField' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         min = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'zeroIsMaxOperandField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'operandField' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         asmFrame3 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame3' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame3' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmFrame3.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame3' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmFrame'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame3' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame3' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame3' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame3' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.21 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame3' -> () From: ( | {
         'ModuleInfo: Module: asmFrame3 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'asmFrame4
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'asmFrame4' From: 'applications/asmKit/asmFrame'



 '-- Side effects'

 globals modules asmFrame3 postFileIn
