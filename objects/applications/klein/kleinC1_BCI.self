 '$Revision: 30.22 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         bytecodeInterpreter = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals abstractBytecodeInterpreter copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes bytecodeInterpreter.

CopyDowns:
globals abstractBytecodeInterpreter. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         currentBC.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         currentStackValues <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         firstNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         inlinedInterpreters <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         inliningInterpreter.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         irNodeGenerator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         irNodesByBCI.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         lastNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes bytecodeInterpreter parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         accessLocal: bc = ( |
            | 
            bc isWrite ifTrue: [
              irNodeGenerator writeLocalSlot: bc slot From: popStackValue.
              pushSelf: bc.
            ] False: [
              irNodeGenerator readLocalSlot: bc slot Into: pushStackValue.
            ].
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         branchToBCI: bci = ( |
             branchNode.
             dstStack.
            | 
            branchNode: irNodeGenerator newBranchNodeForLabel: labelNodeAtBCI: bci.
            dstStack: (stackValuesWhenBCIWas: bci) ifNil: [
              (vector copySize: currentStackValues size) mapBy: [|:x. :i|
                placeholderBranchTargetStackValue copyForStackIndex: i BranchNode: branchNode
              ]
            ].

            dstStack with: currentStackValues Do: [|:dstValue. :currValue. :i|
              irNodeGenerator move: currValue To: dstValue.
            ].
            irNodeGenerator addNode: branchNode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            ((resend.copy
               currentStackValues:  currentStackValues  copyRemoveAll)
               unresolvedMoveNodes: unresolvedMoveNodes copyRemoveAll)
               inlinedInterpreters: inlinedInterpreters copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForIRNodeGenerator: irg Allocator: sla = ( |
            | 
            (copy sourceLevelAllocator: sla) initializeForIRNodeGenerator: irg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: building scope descs for nmethods\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         createScopeDescForNMethod: nm = ( |
             sd.
            | 
            sd: nm scopeDesc copy.
            sd nmethod: nm.
            sd lookupKey: sourceLevelAllocator context lookupKey.
            sd method: slot contents.
            sd methodHolder: slot holder reflectee.
            sd incomingRcvrSPOffset: sourceLevelAllocator locationForIncomingReceiver spOffsetFor: myCompiler codeGenerator InFrame: nm frame.
            sd slotSPOffsets: findSlotSPOffsetsFor: sd Frame: nm frame.
            sd pcOffsetsByBCI: findPCOffsetsByBCI.
            sd inlinedScopes: (inlinedInterpreters copyMappedBy: [|:i| (i createScopeDescForNMethod: nm) inliningScope: sd]) asVector ifNone: vector.
            sd).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: data slots\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlot: s = ( |
             rcvrAndArgs.
            | 
            rcvrAndArgs:
              case
                if: [s isMethod    ] Then: raiseError
                If: [s isAssignable] Then: [vector copyAddFirst: sourceLevelAllocator valueForSelf                                              ]
                If: [s isAssignment] Then: [(sourceLevelAllocator valueForSelf & (sourceLevelAllocator valueForIncomingRcvrOrArgAt: 1)) asVector]
                                     Else: [vector copyAddFirst: sourceLevelAllocator valueForSelf                                              ].

            irNodeGenerator dataSlot: s InReceiverOfType: selfMirror RcvrAndArgs: rcvrAndArgs Result: pushStackValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: prologue and epilogue\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         epilogue = ( |
            | 
            currentBC: nonexistentBC copyForInterpreter: self BCI: method ifNil: 0 IfNotNil: [|:m| m codes size].

            irNodesByBCI ifNotNil: [|:v| justReachedBCI: v lastKey].
            irNodeGenerator move: popStackValue To: sourceLevelAllocator valueForOutgoingResult.

            irNodeGenerator localReturn.

            nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         fillInMissingIRNodesByBCI = ( |
             nextNode.
            | 
            irNodesByBCI ifNil: [irNodesByBCI: vector].
            irNodesByBCI isEmpty ifTrue: [^ self].

            irNodesByBCI last ifNil: [
              irNodesByBCI at: irNodesByBCI lastKey Put: irNodesByBCI at: irNodesByBCI lastKey pred.
            ].

            irNodesByBCI reverseDo: [|:n. :i|
              n ifNil: [
                irNodesByBCI at: i Put: nextNode.
              ] IfNotNil: [
                nextNode: n.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: building scope descs for nmethods\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         findPCOffsetsByBCI = ( |
            | 
            irNodesByBCI asVector copyMappedBy: [|:n| n pcOffsetIfPresent: [|:o| o] IfAbsent: -1]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: building scope descs for nmethods\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         findSlotSPOffsetsFor: sd Frame: f = ( |
             slotSPOffsets.
            | 
            slot isMethod ifFalse: [^ vector].
            slotSPOffsets: list copyRemoveAll.
            method allSlotsOnThisMethod do: [|:s|
              s isKleinSlotOffsetRecorded ifTrue: [| loc |
                loc: (sourceLevelAllocator valueForSlot: s name) location.
                loc isConstant ifFalse: [
                  slotSPOffsets addLast: loc spOffsetFor: myCompiler codeGenerator InFrame: f.
                ] True: [| index |
                  index: sd nmethod addConstant: loc oopValue.
                  slotSPOffsets addLast: sd nmethod spOffsetIndicatingConstantAt: index.
                ].
              ].
            ].
            slotSPOffsets asVector ifNone: vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         finished = ( |
            | 
            lastNode: irNodeGenerator nodeToInsertAfter.
            resolveUnresolvedMoveNodes.
            forgeControlFlowLinks.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         forgeControlFlowLinks = ( |
            | 
            irNodeGenerator
              forgeControlFlowLinksStartingFrom: nodeToStartForgingControlFlowLinksFrom
                                          Until: lastNode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         generateIf: v Is: oop ThenBranchToBCI: bci = ( |
            | 
            irNodeGenerator generateExit: [|:exitFork|
              irNodeGenerator generateIf: v DoesNotEqual: (sourceLevelAllocator valueForConstant: oop) ThenBranchTo: exitFork.
              branchToBCI: bci.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: prologue and epilogue\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         generateInitializationForLocals = ( |
             firstTime <- bootstrap stub -> 'globals' -> 'true' -> ().
             nilMirror.
             nilValue.
            | 
            sourceLevelAllocator assignableLocalSlots do: [|:s. localValue|
              firstTime ifTrue: [irNodeGenerator comment: 'initializing locals'. nilMirror: nil asMirror. firstTime: false].
              localValue: sourceLevelAllocator valueForSlot: s.
              s contents = nilMirror ifFalse: [
                irNodeGenerator moveConstantReflecteeOf: s contents To: localValue.
              ]
              True: [
                nilValue ifNil: [
                  nilValue: sourceLevelAllocator newValue.
                  irNodeGenerator loadNilInto: nilValue.
                ].
                irNodeGenerator move: nilValue To: localValue.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: auto-generating\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         generatePrimitiveTranslationMethods = ( |
            | 
            slotGenerator copyGenerateSlotsIn: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         indexedBranch: bc = ( |
             indexValue.
            | 
            indexValue: popStackValue.
            bc destinations do: [|:dBCI. :i|
              generateIf: indexValue Is: i ThenBranchToBCI: dBCI.
            ].
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForIRNodeGenerator: irg = ( |
            | 
            irNodeGenerator: irg.
            sourceLevelAllocator slot isMethod ifTrue: [| m |
              m: sourceLevelAllocator slot contents.
              initializeForMethod: m.
              stackValuesByBCI: vector copySize: m codes size succ.
                  irNodesByBCI: vector copySize: m codes size succ.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         interpret: bc = ( |
            | 
            currentBC: bc.
            justReachedBCI: bc pc.
            resend.interpret: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         interpretSlot = ( |
            | 
            case
              if: [slot isMethod] Then: [interpretMethod]
                                  Else: [dataSlot: slot].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         isInlined = ( |
            | 
            sourceLevelAllocator isInlined).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: stack values\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         justReachedBCI: bci = ( |
            | 
            [(stackValuesByBCI at: bci) isNil] assert.
              stackValuesByBCI at: bci Put: currentStackValues asVector.

            (irNodesByBCI at: bci) ifNil: [
              irNodesByBCI at: bci Put: irNodeGenerator defineLabel.
            ] IfNotNil: [|:lbl|
              irNodeGenerator bindLabel: lbl.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         labelNodeAtBCI: bci = ( |
            | 
            (irNodesByBCI at: bci) ifNil: [| lbl |
              lbl: irNodeGenerator newLabel.
              irNodesByBCI at: bci Put: lbl.
              lbl
            ] IfNotNil: [|:lbl|
              lbl
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         machineLevelAllocator = ( |
            | 
            irNodeGenerator machineLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: materializing values\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         materializeDestAndArgsOf: bc AndDo: blk = ( |
             rcvrAndArgs.
             result.
            | 
            rcvrAndArgs: popRcvrAndArgsForSend: bc.
            result: pushStackValue.
            blk value: result With: rcvrAndArgs asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         myCompiler = ( |
            | 
            irNodeGenerator myCompiler).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         nodeToStartForgingControlFlowLinksFrom = ( |
             n.
            | 
            n: firstNode.
            [n sourcePred isNil || [n sourcePred controlFlowSuccs isEmpty not]] whileFalse: [
              n: n sourcePred.
            ].
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: prologue and epilogue\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         nonexistentBC = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'nonexistentBC' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes bytecodeInterpreter parent nonexistentBC.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'nonexistentBC' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         interpreter.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'nonexistentBC' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'nonexistentBC' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes bytecodeInterpreter parent nonexistentBC parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'nonexistentBC' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForInterpreter: interp BCI: i = ( |
            | 
            (copy interpreter: interp) pc: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'nonexistentBC' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'nonexistentBC' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         pc.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         nonlocalReturn: bc = ( |
            | 
            "This also gets called for ^'s in non-block methods.
             (for syntactical compatibility with Smalltalk),
              but in that case the bytecode is a nop. -- dmu 5/05"
            method isReflecteeBlockMethod ifTrue: [
              irNodeGenerator moveNLRHomeScopeTo:     machineLevelAllocator valueForNLRHomeScope.
              irNodeGenerator move: popStackValue To:  sourceLevelAllocator valueForOutgoingResult.
              irNodeGenerator nonlocalReturn.
            ].
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         placeholderBranchTargetStackValue = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'placeholderBranchTargetStackValue' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes bytecodeInterpreter parent placeholderBranchTargetStackValue.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'placeholderBranchTargetStackValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         branchNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'placeholderBranchTargetStackValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'placeholderBranchTargetStackValue' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes bytecodeInterpreter parent placeholderBranchTargetStackValue parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'placeholderBranchTargetStackValue' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForStackIndex: i BranchNode: b = ( |
            | 
            (copy stackIndex: i) branchNode: b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'placeholderBranchTargetStackValue' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         isPlaceholder = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'placeholderBranchTargetStackValue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'placeholderBranchTargetStackValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         stackIndex.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         pop: bc = ( |
            | 
            popStackValue.
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         popRcvrAndArgsForSend: bc = ( |
             popped.
            | 
            popped: list copyRemoveAll.
            bc popCount         do:   [popped addFirst: popStackValue                    ].
            bc isSelfImplicit ifTrue: [popped addFirst: sourceLevelAllocator valueForSelf].
            popped).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: stack values\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         popStackValue = ( |
            | 
            currentStackValues removeLast).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: auto-generating\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         primitiveGenerationMethodNameForSelector: primitiveMethodName = ( |
             s.
            | 
            s: primitiveMethodName.

            "Automatically append IfFail: to selector name if not already present
             because both variants are generated identically except for the
             handling of failures. -- jb 7/03"
            ('IfFail:' isSuffixOf: s) ifFalse: [s: s, 'IfFail:'].

            'generatePrimitive', (s copyMutable mapBy: [|:c| c = ':' ifTrue: '_' False: c]), ':').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         primitiveTranslatorMixin* = bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: prologue and epilogue\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         prologue = ( |
            | 
            currentBC: nonexistentBC copyForInterpreter: self BCI: 0.

            "Don't need the start node for inlined scopes."
            isInlined ifFalse: [irNodeGenerator start].

            irNodesByBCI ifNotNil: [justReachedBCI: 0].
            generateInitializationForLocals.
            machineLevelAllocator isLeafMethod ifFalse: [irNodeGenerator interruptPoint].
            nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         pushLiteral: bc = ( |
             mir.
            | 
            mir: reflect: bc oopToPush.

            peekAtNextBytecode ifNotNil: [|:nextBC|
              nextBC isPop ifTrue: [
                "Don't bother generating any IR nodes for this BC, and skip the pop too.
                 Also, no need to compile an nmethod for the block."
                myCompiler willNotPushBlockMirror: mir.
                pc: pc succ.
                ^ bc
              ].
            ].

            mir isReflecteeBlock ifFalse: [
              irNodeGenerator moveConstant: bc oopToPush To: pushStackValue.
            ] True: [
              irNodeGenerator cloneBlockLiteral: mir Into: pushStackValue.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         pushSelf: bc = ( |
            | 
            irNodeGenerator move: sourceLevelAllocator valueForSelf To: pushStackValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: stack values\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         pushStackValue = ( |
             v.
            | 
            v: sourceLevelAllocator newStackValue.
            v addDescription: 'stack_', currentStackValues size printString.
            currentStackValues addLast: v.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         resolveUnresolvedMoveNodes = ( |
            | 
            unresolvedMoveNodes do: [|:n. ph. dBCI. dStack|
              ph: n destinationValue.
              [ph isPlaceholder] assert.
              dBCI: ph branchNode destinationNode bc pc.
              dStack: stackValuesWhenBCIWas: dBCI.
              n destinationValue: dStack at: ph stackIndex.
              [n destinationValue isNotNil] assert.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         scalarBranch: bc = ( |
            | 
            bc isConditional ifTrue: [
              generateIf: popStackValue Is: bc valueToBranchOn ThenBranchToBCI: bc destination.
            ] False: [
              branchToBCI: bc destination.
            ].
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         selfMirror = ( |
            | 
            sourceLevelAllocator context selfMirror).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         send: bc = ( |
             key.
             rcvrAndArgs.
             result.
            | 

            bc isPrimitive ifTrue: [| s |
              s: primitiveGenerationMethodNameForSelector: bc selector.
              (asMirror lookupKey: s) ifNone: [] IfOne: [s sendTo: self With: bc. ^ bc] IfMany: [error: 'was that intentional?'].
            ].

            rcvrAndArgs: popRcvrAndArgsForSend: bc.
            result: pushStackValue.

            bc isPrimitive ifTrue: [| n |
              n: (irNodeGenerator irNodeProtos primitiveProtoForBC: bc) copyBC: bc RcvrAndArgs: rcvrAndArgs Result: result.
              n setSpecialMode.
              n isSpecialCompilationMode
                      ifTrue: [irNodeGenerator addNode:     n]
                       False: [irNodeGenerator addSendNode: n].
            ] False: [
              irNodeGenerator addSendNode: irNodeGenerator irNodeProtos send copyBC: bc RcvrAndArgs: rcvrAndArgs Result: result.
            ].

            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         slot = ( |
            | 
            sourceLevelAllocator slot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: stack values\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         stackValuesWhenBCIWas: bci = ( |
            | 
            stackValuesByBCI at: bci).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         sourceLevelAllocator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         stackValuesByBCI.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         unresolvedMoveNodes <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         irNodeGenerator = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         currentBytecodeInterpreter.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         firstNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         lastNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myCompiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         nodeToInsertAfter.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         addNode: n = ( |
            | 
            nodeToInsertAfter ifNotNil: [
              n sourcePred: nodeToInsertAfter.
              n sourceSucc: nodeToInsertAfter sourceSucc.
              n sourceSucc ifNotNil: [|:succ| succ sourcePred: n].
              nodeToInsertAfter sourceSucc: n.
            ].
            nodeToInsertAfter: n.
            n sourceSucc ifNil: [ lastNode: n].
            n sourcePred ifNil: [firstNode: n].
            currentBytecodeInterpreter firstNode ifNil: [currentBytecodeInterpreter firstNode: n].
            n bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         addSendNode: n = ( |
            | 
            n lastNodeBeforeSettingUpTheSend: nodeToInsertAfter.
            n rcvrAndArgValuesToMoveTo with: n rcvrAndArgStackValues Do: [|:dstV. :srcV| move: srcV To: dstV].
            addNode: n.
            move: n valueToGetResultFrom To: n resultStackValue.
            n lastNodeSettingUpTheSendResult: nodeToInsertAfter.
            currentBC).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         bindLabel: n = ( |
            | 
            n bc: currentBC.
            addNode: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         branchToLabel: lbl = ( |
            | 
            addNode: newBranchNodeForLabel: lbl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: breakpoints\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         breakpoint: msg = ( |
            | 
            addNode: irNodeProtos breakpoint copyBC: currentBC String: msg value.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         canInlineSlot: s = ( |
            | 
            s isMethod                       ifFalse: [^  true]. "Data slots are fine."
            s contents isReflecteeBlockMethod ifTrue: [^ false]. "Don't know how to inline block methods yet."
            s contents hasBlocks              ifTrue: [^ false]. "Don't know how to inline methods containing blocks yet - gotta customize the nmethod so it can find the uplevel locations."
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         cloneBlockLiteral: mir Into: dst = ( |
             literalValue.
             memoValue.
            | 
            literalValue: sourceLevelAllocator valueForConstant: mir reflectee.
               memoValue: machineLevelAllocator valueForMemoizedBlockMirror: mir.
            addNode: irNodeProtos blockLiteral copyBC: currentBC
                                              Literal: literalValue
                                             Memoized: memoValue.
            move: memoValue To: dst.

            [aaaaa]. "Not the right solution in the long run, but for now the memoixedBlockLoc
                      needs to be a register. -- Adam, Apr. 2009"
            lastNode aaa_doNotCoalesce: true.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         comment: msg = ( |
            | 
            addNode: irNodeProtos comment copyBC: currentBC String: msg value.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         comparisons = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent comparisons.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent comparisons abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         notEqual = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> 'notEqual' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent comparisons notEqual.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> 'notEqual' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIfTrueFor: reg1 With: reg2 ThenBranchTo: lbl With: cg = ( |
            | 
            cg generateIf: reg1 DoesNotEqual: reg2 ThenBranchTo: lbl.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> 'notEqual' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForCompiler: c = ( |
            | 
            (copy myCompiler: c) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         currentBC = ( |
            | 
            currentBytecodeInterpreter currentBC).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: data slots\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlot: s InReceiverOfType: rcvrMir RcvrAndArgs: rcvrAndArgs Result: result = ( |
            | 
            case
              if: [s isMethod    ] Then: raiseError
              If: [s isAssignable] Then: [| holderValue |
                                          holderValue: sourceLevelAllocator newValue.
                                          moveHolderOfDataSlot: s InReceiverOfType: rcvrMir Value: (rcvrAndArgs at: 0) Into: holderValue.
                                          addNode: irNodeProtos dataSlotAccess copyBC: currentBC Slot: s Data: result Holder: holderValue]
              If: [s isAssignment] Then: [| holderValue |
                                          holderValue: sourceLevelAllocator newValue.
                                          moveHolderOfDataSlot: s InReceiverOfType: rcvrMir Value: (rcvrAndArgs at: 0) Into: holderValue.
                                          addNode: irNodeProtos dataSlotAssignment copyBC: currentBC Slot: s Data: (rcvrAndArgs at: 1) Holder: holderValue.
                                          move: (rcvrAndArgs at: 0) To: result]
                                   Else: [addNode: irNodeProtos constantDataSlotAccess copyBC: currentBC Slot: s Data: result]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         defineLabel = ( |
             n.
            | 
            n: irNodeProtos label copyBC: currentBC.
            addNode: n.
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         forgeControlFlowLinksStartingFrom: n1 Until: n2 = ( |
            | 
            n1 sourceNodesDo: [|:n|
              n forgeControlFlowLinks.
              n == n2 ifTrue: [^ self].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateExit: aBlock = ( |
             end.
            | 
            [aaaaa]. "Factor with the codeGenerator one."
            end: newLabel.
            aBlock value: end.
            bindLabel: end.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objValue DoesNotEqual: otherObjValue ThenBranchTo: trueFork = ( |
            | 
            generateIf: objValue Is: comparisons notEqual With: otherObjValue ThenBranchTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objValue Is: comparison With: otherObjValue ThenBranchTo: trueFork = ( |
            | 
            addNode: irNodeProtos conditionalBranch
                                     copyBC: currentBC
                                Destination: trueFork
                                       Obj1: objValue
                                       Obj2: otherObjValue
                                 Comparison: comparison.

            defineLabel. "To mark the start of the false branch."

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         ifShouldInlineSend: key To: rcvrValue Then: inlineBlk Else: noInlineBlk = ( |
             ss.
             t.
            | 
            myCompiler optimizationPolicy shouldNeverDoInlining  ifTrue: [^ noInlineBlk value].
            machineLevelAllocator method isReflecteeBlockMethod  ifTrue: [^ noInlineBlk value]. "Would require customizing the block, which we don't do yet."
            t: rcvrValue knownTypeIfFail:                                [^ noInlineBlk value].

            ss: key lookupSlotsUsing: sourceLevelAllocator context protoSlotFinder Self: t Holder: sourceLevelAllocator context outermostMethodHolder.
            ss ifNone: noInlineBlk
                IfOne: [|:s| (canInlineSlot: s) && [myCompiler optimizationPolicy shouldInlineSlot: s For: rcvrValue Key: key] ifTrue: [inlineBlk value: s With: t] False: noInlineBlk]
               IfMany: noInlineBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            pushNewInterpreterFor: machineLevelAllocator topSourceLevelAllocator.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         inlineMethodSlot: s Key: key ReceiverType: rcvrMir RcvrAndArgs: rcvrAndArgValues Result: resultValue = ( |
             c.
             sla.
            | 
            [aaaaa]. "Could probably merge this with the data-slot inlining code - it'd be a bit cleaner. Um, maybe."
            [s contents isReflecteeBlockMethod not] assert. "Don't know how to inline block methods yet - gotta get the lexical parent scopes somehow."
            comment: 'inlining call to ', s name.
            c: myCompiler prototypes compilationContext copyForSlot: (myCompiler oracleForEagerRelocation kleinifySlot: s)
                                                                Key: key
                                                               Self: rcvrMir
                                                           Receiver: rcvrMir
                                                LexicalParentScopes: vector.
            sla: myCompiler prototypes sourceLevelAllocator
                                 copyForMachineLevelAllocator: machineLevelAllocator
                                                      Context: c.
            sla initializeValues.
            sla allocateIncomingAndPreallocatedLocations. "Is it OK to put this here, or do I have to put it after the IR nodes are generated, or what?"
            rcvrAndArgValues with: sla incomingRcvrAndArgValues Do: [|:srcV. :dstV| move: srcV To: dstV].
            pushNewInterpreterFor: sla.
            currentBytecodeInterpreter prologue.
            currentBytecodeInterpreter interpretMethod.
            currentBytecodeInterpreter epilogue.
            move: sla valueForOutgoingResult To: resultValue.
            interpreterFinished.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         inlineSlot: s Key: key ReceiverType: rcvrMir RcvrAndArgs: rcvrAndArgValues Result: resultValue = ( |
            | 
            myCompiler dependsOnKey: key ResolvingToSlot: s.
            s isMethod ifTrue: [
              inlineMethodSlot: s Key: key ReceiverType: rcvrMir RcvrAndArgs: rcvrAndArgValues Result: resultValue.
            ] False: [| nodeBefore |
              nodeBefore: nodeToInsertAfter.
              dataSlot: s InReceiverOfType: rcvrMir RcvrAndArgs: rcvrAndArgValues Result: resultValue.
              forgeControlFlowLinksStartingFrom: nodeBefore Until: nodeToInsertAfter.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: scoping\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         interpreterFinished = ( |
            | 
            currentBytecodeInterpreter finished.
            currentBytecodeInterpreter: currentBytecodeInterpreter inliningInterpreter.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: interrupt points\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         interruptPoint = ( |
            | 
            addNode: irNodeProtos interruptPoint copyBC: currentBC.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         irNodeProtos = ( |
            | 
            myCompiler prototypes irNodes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         loadNilInto: dstValue = ( |
            | 
            moveConstant: nil To: dstValue NameForComment: 'nil'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: returning\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         localReturn = ( |
            | 
            addNode: irNodeProtos localReturn copyBC: currentBC.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         machineLevelAllocator = ( |
            | 
            myCompiler machineLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         move: srcValue To: dstValue = ( |
             n.
            | 
            srcValue = dstValue ifTrue: [^ self].
            n: irNodeProtos move copyBC: currentBC Source: srcValue Destination: dstValue.
            addNode: n.
            dstValue isPlaceholder ifTrue: [currentBytecodeInterpreter unresolvedMoveNodes add: n].
            currentBC).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         moveConstant: o To: dst = ( |
            | 
            move: (sourceLevelAllocator valueForConstant: o) To: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         moveConstant: o To: dst NameForComment: n = ( |
            | 
            [aaaaa]. "Make it remember the name for the comment."
            moveConstant: o To: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         moveConstantReflecteeOf: m To: dst = ( |
            | 
            moveConstant: m reflectee To: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: data slots\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         moveHolderOfDataSlot: s InReceiverOfType: rcvrMir Value: rcvrValue Into: dstValue = ( |
             holderMirror.
            | 
            [aaaaa]. "Eventually should be rcvrMap, not rcvrMir."
            holderMirror: s holder.
            holderMirror = rcvrMir 
               ifTrue: [ move:         rcvrValue              To: dstValue ]
                False: [ moveConstant: holderMirror reflectee To: dstValue ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: returning\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         moveNLRHomeScopeTo: dstValue = ( |
            | 
            addNode: irNodeProtos nlrHomeScope copyBC: currentBC Destination: dstValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         newBranchNodeForLabel: lbl = ( |
            | 
            irNodeProtos unconditionalBranch copyBC: currentBC Destination: lbl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         newLabel = ( |
             n.
            | 
            n: irNodeProtos label copyBC: currentBC.
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: returning\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         nonlocalReturn = ( |
            | 
            addNode: irNodeProtos nonlocalReturn copyBC: currentBC.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: scoping\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         pushNewInterpreterFor: aSourceLevelAllocator = ( |
             i.
            | 
            i: myCompiler prototypes bytecodeInterpreter
                                    copyForIRNodeGenerator: self
                                                 Allocator: aSourceLevelAllocator.
            currentBytecodeInterpreter ifNotNil: [|:ci|
              ci inlinedInterpreters addLast: i.
              i inliningInterpreter: ci.
            ].
            currentBytecodeInterpreter: i.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: local variables\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         readLocalSlot: s Into: dst = ( |
             localValue.
            | 
            localValue: sourceLevelAllocator valueForSlot: s.
            move: localValue To: dst.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         sourceLevelAllocator = ( |
            | 
            currentBytecodeInterpreter sourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: prologue\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
            | 
            addNode: irNodeProtos start copyBC: currentBC.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: local variables\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         writeLocalSlot: s From: srcValue = ( |
             localValue.
            | 
            localValue: sourceLevelAllocator valueForSlot: s.
            move: srcValue To: localValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         kleinC1_BCI = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinC1_BCI.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.22 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinC1_BCI postFileIn
