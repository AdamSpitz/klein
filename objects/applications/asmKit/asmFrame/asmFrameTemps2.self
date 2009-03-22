 '$Revision: 30.18 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         instructionTemplates = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators realOrPseudoInstructionTemplates copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators instructionTemplates.

CopyDowns:
globals assemblerSystems framework generators realOrPseudoInstructionTemplates. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators instructionTemplates parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: generating templates\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: private'
        
         finish = ( |
            | 
            resend.finish.
            myAssemblerSystem disassembler buildSortedTemplates.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: generating templates\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         gen: name With: fieldCollector = ( |
            | 
            gen: name With: fieldCollector ExternalName: name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: generating templates\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         gen: name With: fieldCollector ExternalName: xName = ( |
            | 
            gen: name With: fieldCollector ExternalName: xName Pred: [true] Bads: vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: generating templates\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         gen: name With: fieldCollector ExternalName: xName Pred: predicateBlock Bad: badOperandCollector = ( |
            | 
            gen: name With: fieldCollector ExternalName: xName Pred: predicateBlock Bads: vector copyAddFirst: badOperandCollector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: generating templates\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: private'
        
         gen: name With: fieldCollector ExternalName: xName Pred: predicateBlock Bads: collectorOfBadOperandCollectors = ( |
             instructions.
            | 
            instructions: genBasic: name ExternalName: xName Fields: fieldCollector.
            instructions do: [|:i| i asmPredicateBlock: predicateBlock ].
            instructions do: [|:i| i illegalTests: collectorOfBadOperandCollectors asVector copyMappedBy: [|:c| c asVector] ].
            instructions).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: generating templates\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         gen: name With: fieldCollector Pred: predicateBlock Bad: badOperandCollector = ( |
            | 
            gen: name With: fieldCollector ExternalName: name Pred: predicateBlock Bad: badOperandCollector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'Category: generating templates\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: private'
        
         gen: name With: fieldCollector Pred: predicateBlock Bads: collectorOfBadOperandCollectors = ( |
            | 
            gen: name With: fieldCollector ExternalName: name Pred: predicateBlock Bads: collectorOfBadOperandCollectors).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         proto = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators realOrPseudoInstructionTemplates parent proto copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators instructionTemplates parent proto.

CopyDowns:
globals assemblerSystems framework generators realOrPseudoInstructionTemplates parent proto. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'Category: special error conditions\x7fComment: string for assembler method; tests for legality\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: InitializeToExpression: (\'true\')'
        
         asmPredicate <- 'true'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'Category: special error conditions\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: InitializeToExpression: (vector)\x7fVisibility: public'
        
         illegalTests <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'Category: binary opcode & mask\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: InitializeToExpression: (0)'
        
         opcode <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'Category: binary opcode & mask\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: InitializeToExpression: (0)'
        
         opcodeMask <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators instructionTemplates parent proto parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         asmPredicateBlock: b = ( |
            | 
            asmPredicate: b asMirror methodSource canonicalize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fCategory: option fields\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForOption: option CanonicalRepresentative: it = ( |
            | 
            copy organizeOption: option CanonicalRepresentative: it).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembleOperandValuesIn: inst At: lc = ( |
            | 
            operandFields copyMappedBy: [|:of|
              of disassembleValueIn: inst At: lc IT: self
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembleOperandsIn: inst At: lc = ( |
            | 
            operandFields copyMappedBy: [|:of|
              of disassembleOperandIn: inst At: lc IT: self
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: override if need be\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: private'
        
         intNNString = ( |
            | intNN asMirror creatorSlotHint name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: public'
        
         isRedundant = ( |
            | 
            canonicalRepresentative isNotNil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         organizeConstantField: f Constant: c = ( |
            | 
            opcode:       intNN or:  opcode       With:  f where fromUnsignedInteger: c.
            opcodeMask:   intNN or:  opcodeMask   With:  f where maskInPlace).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         organizeFields = ( |
            | 
            opcode: intNN zero.
            opcodeMask: intNN zero.
            resend.organizeFields.
            setSpecificity).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fCategory: option fields\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: private'
        
         organizeOption: option CanonicalRepresentative: it = ( |
            | 
                    name: (        name, option         name) canonicalize.
                slotName: (    slotName, option         name) canonicalize.
            externalName: (externalName, option externalName) canonicalize.

            opcode:     intNN or: opcode     With: option field where fromUnsignedInteger: option value.
            opcodeMask: intNN or: opcodeMask With: option field where maskInPlace.
            setSpecificity.

            options add: option.

            option isRedundant ifTrue: [
                canonicalRepresentative: it.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         organizeReservedField: f = ( |
            | 
            organizeConstantField: f Constant: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         setSpecificity = ( |
            | 
            specificity: intNN numberOfOnesIn: opcodeMask).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: generating assembler\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         sourceForAssemblyBody = ( |
             r.
             ss.
            | 
            ss: intNN storeStringFor: opcode.
            r: '| instTemplate = ' & reflectiveName & '. opcode = ' & ss & '. r <- 0 |\n'.
            r: r & '"This method generated by sourceForAssemblyBody."\n'.
            r: r & '"Opcode is really: ' & ss & '"\n\n'.

            r: r & '(' & asmPredicate & ') ifFalse: [^ error: \'asmPredicate did not hold\'].\n'.

            r: r & 'r: '.

            operandFields do: [|:of|
              r: r & intNNString & ' or: (' & of sourceForAssemblyMethod & ') With:\n'.
            ].
            [intNN or: a With: b]. "browsing"
            r: r & 'opcode.\n'.
            r: r & 'data32: r.\n'.
            r: r & 'r'.
            [error: nil. data32: nil]. "browsing"
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'instructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'Category: binary opcode & mask\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: InitializeToExpression: (0)'
        
         specificity <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         pseudoInstructionTemplates = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators realOrPseudoInstructionTemplates copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators pseudoInstructionTemplates.

CopyDowns:
globals assemblerSystems framework generators realOrPseudoInstructionTemplates. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators pseudoInstructionTemplates parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: generating templates\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         gen: name Field: f Script: script = ( |
            | 
            gen: name Fields: (vector copyAddFirst: f) Script: script).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: generating templates\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         gen: name Fields: fields Script: script = ( |
             insts.
            | 
            insts: genBasic: name Fields: fields.
            insts do: [|:i| i script: script].
            insts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: generating templates\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         gen: name Script: s = ( |
            | 
            gen: name Fields: vector Script: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         proto = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators realOrPseudoInstructionTemplates parent proto copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators pseudoInstructionTemplates parent proto.

CopyDowns:
globals assemblerSystems framework generators realOrPseudoInstructionTemplates parent proto. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators pseudoInstructionTemplates parent proto parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fComment: See comment in \'fixBranches:Dst:Link:Pred:\'!\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         addOption: optionName OfField: anOptionField = ( |
            | 
            anOptionField options do: [|:o|
                o name = optionName ifTrue: [
                    options add: o.
                    ^ self.
                ].
            ].
            error: 'unknown option: ', optionName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fComment: See comment in \'fixBranches:Dst:Link:Pred:\'!\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         addOptionField: f = ( |
            | 
            allFields: allFields copyAddLast: f.
            optionFields addLast: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         illegalTests = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         isPseudo = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         organizeConstantField: f Constant: c = ( |
            | 
            error: 'pseudo instructions should not have constant fields').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: generating assembler\x7fModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         sourceForAssemblyBody = ( |
            | 
            '| instTemplate = ', reflectiveName, ' |',
             script).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'pseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: InitializeToExpression: (\'\')'
        
         script <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         asmFrameTemps2 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps2' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmFrameTemps2.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps2' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmFrame'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps2' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps2' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps2' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps2' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.18 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps2' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps2 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules asmFrameTemps2 postFileIn
