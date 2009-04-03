 '$Revision: 30.16 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         constants = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators abstractGenerator copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators constants.

CopyDowns:
globals assemblerSystems framework generators abstractGenerator. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators constants parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> () From: ( | {
         'Category: generating\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateConstantNamed: s Number: n = ( |
             r.
            | 
            r: (proto copy number: n) name: s canonicalize.
            at: s Put: r.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> () From: ( | {
         'Category: override these\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         proto = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators constants parent proto.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         do: blk = ( |
            | 
            blk value: registers.
            blk value: fields.
            blk value: instructionTemplates.
            blk value: pseudoInstructionTemplates.
            blk value: instructionAssemblyMethods).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         fields = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators abstractGenerator copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields.

CopyDowns:
globals assemblerSystems framework generators abstractGenerator. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: integer operand fields\x7fCategory: generating\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateBranchDispField: name Bits: bits Keyword: k = ( |
            | 
            generateField: branchDispOperandField Name: name Bits: bits Keyword: k).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: constant fields\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateConstantField: name Bits: bits = ( |
            | 
            generateField: constantField Name: name Bits: bits).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateField: proto Name: name Bits: bits = ( |
             f.
            | 
            f: proto copyName: name Bits: bits Asm: myAssemblerSystem.
            at: name Put: f.
            f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: integer operand fields\x7fCategory: generating\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateField: proto Name: name Bits: bits Keyword: k = ( |
             f.
            | 
            f: proto copyName: name Bits: bits Keyword: k Asm: myAssemblerSystem.
            at: name Put: f.
            f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: ignored fields\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateIgnoredField: name Bits: bits = ( |
            | 
            generateField: ignoredField Name: name Bits: bits).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: integer operand fields\x7fCategory: generating\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateIntOperandField: name Bits: bits Keyword: k = ( |
            | 
            generateField: intOperandField Name: name Bits: bits Keyword: k).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: integer operand fields\x7fCategory: generating\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateIntOperandField: name Bits: bits Keyword: k Zeroes: z = ( |
             f.
            | 
            f: alignedIntOperandField copyName: name Bits: bits Keyword: k Asm: myAssemblerSystem Zeroes: z.
            at: name Put: f.
            f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: option fields\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateMnemonicField: name Suffix: suffix = ( |
            | 
            "when using option fields, sometimes need a suffix
             in the mnemonic AFTER the option field.
             use an option field with one option and no bits in it"

            (generateOptionField: name At: vector copyAddLast: -1)
              withOption: suffix Value: 0.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: option fields\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateOptionField: name At: bits = ( |
            | 
            generateField: optionField Name: name Bits: bits).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: reserved fields\x7fComment: Use me for fields that are specified as reserved 
by the architecture for future instruction set expansion.
Disassemblers can assume that the value in the instruction 
matches the field object\'s \"contents\" slot
or we are looking at a new opcode that we do not know yet.\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateReservedField: name Bits: bits = ( |
            | 
            generateField: reservedField Name: name Bits: bits).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: symbolic operand fields\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateSymbolicOperandField: name Bits: bits Keyword: k In: nameSpace = ( |
             f.
            | 
            f: symbolicOperandField copyName: name
                                        Bits: bits
                                     Keyword: k
                                          In: nameSpace
                                         Asm: myAssemblerSystem.
            at: name Put: f.
            f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: integer operand fields\x7fCategory: generating\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateZeroIsMaxOperandField: name Bits: bits Keyword: k = ( |
            | 
            generateField: zeroIsMaxOperandField Name: name Bits: bits Keyword: k).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: symbolic operand fields\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         symbolicOperandField = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators fields parent operandField copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'symbolicOperandField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent symbolicOperandField.

CopyDowns:
globals assemblerSystems framework generators fields parent operandField. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         instructionAssemblyMethods = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionAssemblyMethods' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators abstractGenerator copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionAssemblyMethods' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators instructionAssemblyMethods.

CopyDowns:
globals assemblerSystems framework generators abstractGenerator. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionAssemblyMethods' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionAssemblyMethods' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators instructionAssemblyMethods parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionAssemblyMethods' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         finish = ( |
            | resend.finish.
            myAssemblerSystem disassembler buildCommentSentinel.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionAssemblyMethods' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateAbstractAssemblyMethodFor: anInstructionTemplate = ( |
            | 
            generateMethodFromSource: anInstructionTemplate sourceForAbstractAssemblyMethod
                         Subcategory: 'abstract').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionAssemblyMethods' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateAll = ( |
            | 
            start.
            myAssemblerSystem realAndPseudoInstructionTemplatesDo: [|:it|
              generateAssemblyMethodFor: it.
              generateAbstractAssemblyMethodFor: it.
            ].
            finish).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionAssemblyMethods' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateAssemblyMethodFor: anInstructionTemplate = ( |
            | 
            generateMethodFromSource: anInstructionTemplate sourceForAssemblyMethod
                         Subcategory: anInstructionTemplate category).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionAssemblyMethods' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateMethodFromSource: src Subcategory: cat = ( |
             body.
             m.
             name.
            | 
            src ifNil: [^ nil].
            m: src asSlotIfFail: [|:e| error: e].
            name: m first name.
            body: m first contents.
            subcategory: cat.
            at: name PutContents: body.
            body).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionAssemblyMethods' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         registers = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators abstractGenerator copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators registers.

CopyDowns:
globals assemblerSystems framework generators abstractGenerator. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators registers parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> 'parent' -> () From: ( | {
         'Category: generating\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         generateAll = ( |
            | 
            start.
            minReg to: maxReg Do: [|:i|
              generateConstantNamed: (nameFor: i) Number: i].
            finish).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> 'parent' -> () From: ( | {
         'Category: override these\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         maxReg = 31.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> 'parent' -> () From: ( | {
         'Category: override these\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         minReg = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> 'parent' -> () From: ( | {
         'Category: override these\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         nameFor: n = ( |
            | 
            regPrefix, n asString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'registers' -> 'parent' -> () From: ( | {
         'Category: override these\x7fModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         regPrefix = 'r'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         asmFrameGens = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameGens' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameGens' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmFrameGens.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameGens' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmFrame'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameGens' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameGens' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameGens' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameGens' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.16 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameGens' -> () From: ( | {
         'ModuleInfo: Module: asmFrameGens InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'asmFrameRegs
asmFrameTemps1
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'asmFrameRegs' From: 'applications/asmKit/asmFrame'
 bootstrap read: 'asmFrameTemps1' From: 'applications/asmKit/asmFrame'



 '-- Side effects'

 globals modules asmFrameGens postFileIn
