 '$Revision: 30.18 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         realOrPseudoInstructionTemplates = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators abstractGenerator copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators realOrPseudoInstructionTemplates.

CopyDowns:
globals assemblerSystems framework generators abstractGenerator. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators realOrPseudoInstructionTemplates parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: private'
        
         constantFieldFor: operandField = ( |
            | 
            myAssemblerSystem generators fields
              constantField copyFromOperandField: operandField).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: deprecated\x7fComment: Wrap all generation code for depricated stuff with a block and pass it to me.
\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         deprecated: block = ( |
            | generatingDeprecatedInstructions ifTrue: [block value]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: options\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         dontExternallyTest: its = ( |
            | 
            dontExternallyTestGoodies: 
            dontExternallyTestBaddies: its).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: options\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: private'
        
         dontExternallyTestBaddies: its = ( |
            | 
            its do: [|:i| i isExternallyTestableForFailure: false].
            its).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: options\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: private'
        
         dontExternallyTestGoodies: its = ( |
            | 
            its do: [|:i| i isExternallyTestableForSuccess: false].
            its).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: generating templates\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: private'
        
         genBasic: name ExternalName: xName Fields: fieldCollector = ( |
             i1.
             instructions.
            | 
            i1: proto copyName: name ExternalName: xName Fields: fieldCollector.
            instructions: vector copyAddFirst: i1.
            i1 optionFields do: [|:f|
                instructions: f copyAddMyOptionsTo: instructions.
            ].
            instructions do: [|:i| at: i slotName Put: i].
            instructions).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: generating templates\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: private'
        
         genBasic: name Fields: fieldCollector = ( |
            | 
            genBasic: name ExternalName: name Fields: fieldCollector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: deprecated\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         generatingDeprecatedInstructions = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'Category: options\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         ignoreExternalBits: bitCollector Templates: its = ( |
            | 
            its do: [|:i|
              i ignoreExternalBits: myAssemblerSystem bitRange bits: bitCollector
            ].
            its).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         proto = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators realOrPseudoInstructionTemplates parent proto.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: InitializeToExpression: (vector copyRemoveAll)'
        
         allFields <- vector copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: InitializeToExpression: (nil)'
        
         cachedAssemblyMethodSelector.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: InitializeToExpression: (nil)'
        
         cachedIncludesOptionAOfFieldAA.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: InitializeToExpression: (nil)'
        
         canonicalRepresentative.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'Category: whether to test\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         externalBitsToTest.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: InitializeToExpression: (\'\')'
        
         externalName <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'Category: whether to test\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: InitializeToExpression: (true)\x7fVisibility: public'
        
         isExternallyTestableForFailure <- bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'Category: whether to test\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: InitializeToExpression: (true)\x7fVisibility: public'
        
         isExternallyTestableForSuccess <- bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: InitializeToExpression: (\'\')'
        
         name <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: InitializeToExpression: (vector copyRemoveAll)'
        
         operandFields <- vector copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: InitializeToExpression: (vector copyRemoveAll)'
        
         optionFields <- vector copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: InitializeToExpression: (set copy)'
        
         options <- set copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators realOrPseudoInstructionTemplates parent proto parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: generating assembler\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: private'
        
         assemblerSystemPath = ( |
            | 
            'assemblerSystems ', architecture).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: generating assembler\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: public'
        
         assemblyMethodSelector = ( |
            | 
            cachedAssemblyMethodSelector ifNil: [|r|
              r: name.
              operandFields do: [|:of| r: r & of keyword].
              cachedAssemblyMethodSelector: r flatString canonicalize.
            ].
            cachedAssemblyMethodSelector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: generating assembler\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: public'
        
         category = ( |
            | 
            (nameSpaceMirror slotAt: slotName) categories last).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         copy = ( |
            | 
            (((resend.copy
            allFields: allFields copy)
            operandFields: operandFields copy)
            optionFields: optionFields copy)
            options: options copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         copyName: name ExternalName: xName Fields: fieldCollector = ( |
             r.
            | 

            r: copy.
            r name: name canonicalize.
            r externalName: xName canonicalize.
            r initFrom: fieldCollector.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         ignoreExternalBits: aBitRange = ( |
            | 
            externalBitsToTest: aBitRange intNN xor: aBitRange maskInPlace With: -1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: public'
        
         includesOption: optionName OfField: field = ( |
            | 
            options anySatisfy: [|:o| (o field = field) && [o name = optionName]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: public'
        
         includesOptionAOfFieldAA = ( |
            | 
            cachedIncludesOptionAOfFieldAA ifNil: [
              cachedIncludesOptionAOfFieldAA: includesOption: 'a' OfField: assemblerSystems ppc fields aa.
            ].
            cachedIncludesOptionAOfFieldAA).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         initFrom: fieldCollector = ( |
            | 
            allFields: fieldCollector asVector.
            organizeFields.
            externalBitsToTest: -1.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: override if need be\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = bootstrap stub -> 'globals' -> 'int32' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         isPseudo = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: generating assembler\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: private'
        
         nameSpaceMirror = ( |
            | 
            reflect: nameSpacePath eval).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: generating assembler\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: private'
        
         nameSpacePath = ( |
            | 
            assemblerSystemPath, ' ',
            (isPseudo ifTrue: 'pseudoInstructionTemplates'
                       False:       'instructionTemplates')).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         organizeConstantField: f Constant: c = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         organizeFields = ( |
             fl.
            | 
            slotName: name.
            operandFields: list copyRemoveAll.
            optionFields: list copyRemoveAll.
            fl: allFields copy asList.
            [fl isEmpty] whileFalse: [|f|
              f: fl removeFirst.
              case
               if:   [ f isOperandField ] Then: [ 
                       operandFields addLast: f.
                       slotName: (slotName, '_', f name) canonicalize.
               ]
               If:   [ f isOptionField  ] Then: [
                       optionFields addLast: f.
               ]
               If:   [ f isConstantField] Then: [
                       organizeConstantField: f Constant: fl removeFirst value.
               ]
               If:   [ f isReservedField] Then: [
                       organizeReservedField: f.
               ]
               If:   [ f isIgnoredField] Then: [
                       organizeIgnoredField: f.
               ]
               Else:  [ error: 'What kind of field am I?' ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         organizeIgnoredField: f = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         organizeReservedField: f = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: public'
        
         printString = ( |
            | 
            slotName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: generating assembler\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: private'
        
         reflectiveName = ( |
            | 
            nameSpacePath, ' ', slotName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: override if need be\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: public'
        
         size = ( |
            | intNN size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: generating assembler\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: private'
        
         sourceForAssemblyHead = ( |
             r.
            | 
            r: name.
            operandFields do: [|:of|
              r: r & of keyword & ' ' & of argName & ' '
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: generating assembler\x7fModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: public'
        
         sourceForAssemblyMethod = ( |
            | 
            (
              sourceForAssemblyHead & ' = ( ' &
              sourceForAssemblyBody & ' )'
            ) flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'realOrPseudoInstructionTemplates' -> 'parent' -> 'proto' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         slotName <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         asmFrameTemps1 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps1' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmFrameTemps1.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps1' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmFrame'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps1' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps1' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps1' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps1' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.18 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameTemps1' -> () From: ( | {
         'ModuleInfo: Module: asmFrameTemps1 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'asmFrameTemps2
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'asmFrameTemps2' From: 'applications/asmKit/asmFrame'



 '-- Side effects'

 globals modules asmFrameTemps1 postFileIn
