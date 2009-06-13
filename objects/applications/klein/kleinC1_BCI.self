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
        
         endInitBC.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         firstBB.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         inlinedBlock.
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
        
         localReturnBB.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         nlrPointEpilogueBB.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         nodeToBranchToOnNLR.
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
         'Category: static single-assignment form\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         addPhiFunctions = ( |
             bbsNeedingPhiFunctionsByValue.
             definingBBsByValue.
             originalDefinedValuesByBB.
            | 

            definingBBsByValue:         dictionary copyRemoveAll.
            originalDefinedValuesByBB:  dictionary copyRemoveAll.
            firstBB basicBlocksInControlFlowReversePostOrder do: [|:bb. orig|
              orig: bb definedValues.
              originalDefinedValuesByBB at: bb Put: orig.
              orig do: [|:v| (definingBBsByValue at: v IfAbsentPut: [set copyRemoveAll]) add: bb].
            ].

            bbsNeedingPhiFunctionsByValue: dictionary copyRemoveAll.
            definingBBsByValue do: [|:definingBBs. :v|
              [definingBBs isEmpty] whileFalse: [| bb |
                bb: definingBBs removeFirst.
                bb dominanceFrontier do: [|:frontierBB. bbsNeedingPhiFunctions|
                  bbsNeedingPhiFunctions: bbsNeedingPhiFunctionsByValue at: v IfAbsentPut: [set copyRemoveAll].
                  bbsNeedingPhiFunctions if: frontierBB IsPresentDo: [] IfAbsentAddAndDo: [
                    frontierBB insertPhiFunction: compiler prototypes irNodes phiFunction copyBC: frontierBB labelNode bc BasicBlock: frontierBB Value: v.
                    (originalDefinedValuesByBB includesKey: frontierBB) ifTrue: ["sometimes it's not, if we're doing this for an inlined scope"
                      ((originalDefinedValuesByBB at: frontierBB) includes: v) ifFalse: [
                        "We just added a new definition (i.e. the phi function) to frontierBB, so
                         we need to add phi functions to *its* dominance frontier."
                        definingBBs add: frontierBB.
                      ].
                    ].
                  ].
                ].
              ].
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         allBytecodeInterpretersDo: blk = ( |
            | 
            blk value: self.
            inlinedInterpreters do: [|:i|
              i allBytecodeInterpretersDo: blk.
            ].
            self).
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
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         calculateDominance = ( |
            | 
            (compiler prototypes dominanceCalculator copyStartingAt: firstBB) go.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         codeGeneratorForFailureHandler = ( |
            | 
            irNodeGenerator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         compiler = ( |
            | irNodeGenerator compiler).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         context = ( |
            | 
            sourceLevelAllocator context).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: static single-assignment form\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         convertToSSAForm = ( |
            | 
            addPhiFunctions.
            createNewValuesSoThatEachOneIsOnlyDefinedOnce.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            (((resend.copy
                         scopeDesc: klein nmethod scopeDesc copy)
                currentStackValues: currentStackValues  copyRemoveAll)
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
         'Category: blocks\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         createCompiledBlockFor: blockLiteral = ( |
             m.
            | 
            m: reflect: klein compiledBlock.

            [scopeDesc. scopeDesc: nil]. "browsing"
            m: m primitiveCopyAt: 'scopeDesc'
                     PutContents: (reflect: scopeDesc)
                        IsParent: false
                      IsArgument: false
                      Annotation: slotAnnotation
                          IfFail: raiseError.

            [originalBlock_replaceThisSlotWithTheValueSlot.
             originalBlock_replaceThisSlotWithTheValueSlot: nil]. "browsing"
            m: m primitiveCopyAt: 'originalBlock_replaceThisSlotWithTheValueSlot'
                     PutContents: blockLiteral
                        IsParent: false
                      IsArgument: false
                      Annotation: slotAnnotation
                          IfFail: raiseError.

            m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: static single-assignment form\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         createNewValuesSoThatEachOneIsOnlyDefinedOnce = ( |
             mostRecentDefinitions.
             redefinedValuesByBB.
             renamedValues.
            | 
            renamedValues:         dictionary copyRemoveAll.
            mostRecentDefinitions: dictionary copyRemoveAll.
            redefinedValuesByBB:   dictionary copyRemoveAll.

            machineLevelAllocator allValues do: [|:v|
              renamedValues         at: v Put: list copyRemoveAll add: v.
              mostRecentDefinitions at: v Put: list copyRemoveAll add: v.
            ].

            firstBB depthFirstDominatorTreeTraversalPreorderDo: [|:bb. redefinedValuesForThisBB|
              redefinedValuesForThisBB: list copyRemoveAll.
              redefinedValuesByBB at: bb Put: redefinedValuesForThisBB.
              bb nodesDo: [|:n. defined|
                n isPhiFunction ifFalse: [| used |
                  n usedValues do: [|:v. mostRecentV|
                    mostRecentV: (mostRecentDefinitions at: v) last.
                    v = mostRecentV ifFalse: [
                      n replaceUsedValue: v With: mostRecentV.
                    ].
                  ].
                ].

                n definedValues do: [|:v. renamedV|
                  renamedV: machineLevelAllocator newValue beRenamingOf: v.
                  (renamedValues         at: v) addLast: renamedV.
                  (mostRecentDefinitions at: v) addLast: renamedV.
                  redefinedValuesForThisBB      addLast: v.
                  n replaceDefinedValue: v With: renamedV.
                ].
              ].
              bb controlFlowSuccsDo: [|:succBB|
                succBB phiFunctionsDo: [|:phi. v. mostRecentV|
                  v: phi sourceValues at: bb.
                  mostRecentV: (mostRecentDefinitions at: v) last.
                  phi replaceSourceValueFor: bb With: mostRecentV.
                ].
              ].
            ] PostorderDo: [|:bb|
              (redefinedValuesByBB at: bb) do: [|:v|
                (mostRecentDefinitions at: v) removeLast.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: building scope descs for nmethods\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         createScopeDescForNMethod: nm = ( |
            | 
            scopeDesc nmethod: nm.
            scopeDesc incomingRcvrSPOffset: incomingRcvrSPOffsetFor: scopeDesc.
            scopeDesc slotSPOffsets:           findSlotSPOffsetsFor: scopeDesc.
            scopeDesc pcOffsetsByBCI: findPCOffsetsByBCI.
            scopeDesc inlinedScopes: (inlinedInterpreters copyMappedBy: [|:i| (i createScopeDescForNMethod: nm) inliningScope: scopeDesc]) asVector ifNone: vector.
            scopeDesc).
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

            irNodeGenerator dataSlot: s InReceiverWithMap: selfMap RcvrAndArgs: rcvrAndArgs Result: pushStackValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         endInit: bc = ( |
            | 
            endInitBC: bc.
            machineLevelAllocator isLeafMethod ifFalse: [irNodeGenerator interruptPoint].
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: prologue and epilogue\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         epilogue = ( |
             orv.
            | 
            irNodeGenerator nodeToInsertAfter canFallThrough ifFalse: ["localReturn is not necessary." ^ self].

            currentBC: nonexistentBCAt: method ifNil: 0 IfNotNil: [|:m| m codes size].

            irNodesByBCI ifNotNil: [|:v| justReachedBCI: v lastKey].
            orv: sourceLevelAllocator newValueForOutgoingResult.
            irNodeGenerator move: popStackValue To: orv.

            irNodeGenerator localReturn: orv.

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
             r.
            | 
            r: irNodesByBCI mapBy: [|:n| n pcOffsetIfPresent: [|:o| o] IfAbsent: -1]
                             Into: scopeDesc pcOffsetVector copySize: irNodesByBCI size.
            r ifNone: scopeDesc pcOffsetVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: building scope descs for nmethods\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         findSlotSPOffsetsFor: sd = ( |
             f.
             slotSPOffsets.
            | 
            slot isMethod ifFalse: [^ vector].
            f: sd nmethod frame.
            slotSPOffsets: list copyRemoveAll.
            method allSlotsOnThisMethod do: [|:s|
              s isKleinSlotOffsetRecorded ifTrue: [| loc |
                loc: (sourceLevelAllocator valueForSlot: s name) location.
                loc isConstant ifFalse: [
                  slotSPOffsets addLast: loc spOffsetFor: compiler codeGenerator InFrame: f.
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
            fillInMissingIRNodesByBCI.
            resolveUnresolvedMoveNodes.
            forgeControlFlowPredLinks.
            splitControlFlowEdges.
            calculateDominance.
            convertToSSAForm.
            recordDefinersAndUsers.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         firstNode = ( |
            | 
            firstBB labelNode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         forgeControlFlowPredLinks = ( |
            | 
            irNodeGenerator forgeControlFlowLinksStartingFrom: firstNode Until: nil.
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
         'Category: primitives\x7fCategory: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dst Receiver: rcvr RestartIfFail: fh = ( |
            | 
            branchToBCI: endInitBC pc succ.
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
         'Category: building scope descs for nmethods\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         incomingRcvrSPOffsetFor: sd = ( |
            | 
            isInlined && [sourceLevelAllocator context isForABlockMethod] ifTrue: [
              [inlinedBlock isNotNil] assert.
              sd nmethod spOffsetIndicatingConstantAt: sd nmethod addConstant: inlinedBlock.
            ] False: [
              sourceLevelAllocator locationForIncomingReceiver spOffsetFor: compiler codeGenerator InFrame: sd nmethod frame.
            ]).
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
            initializeScopeDesc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: building scope descs for nmethods\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeScopeDesc = ( |
            | 
            scopeDesc lookupKey: sourceLevelAllocator context lookupKey.
            scopeDesc lexicalParentScope: sourceLevelAllocator context lexicalParentScope.
            scopeDesc method: slot isMethod ifTrue: [slot contents] False: [slot contents reflectee].
            scopeDesc methodHolder: compiler reflecteeOfHolderOfSlot: slot.
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
            slot isMethod ifTrue: [interpretMethod]
                           False: [dataSlot: slot].
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
         'Category: accessing\x7fCategory: nodes\x7fComment: Me & my transitive successors such that no node is
included before at least one of its preds is.\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         nodesInControlFlowOrder = ( |
             ns.
            | 
            ns: list copyRemoveAll.
            firstBB depthFirstControlFlowTraversalPreorderDo: [|:bb| bb nodesDo: [|:n| ns addLast: n]] PostorderDo: [] AlreadyDid: set copyRemoveAll.
            ns).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: nodes\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         nodesInControlFlowPostOrder = ( |
             ns.
            | 
            ns: list copyRemoveAll.
            firstBB depthFirstControlFlowTraversalPreorderDo: [] PostorderDo: [|:bb| bb reverseNodesDo: [|:n| ns addLast: n]] AlreadyDid: set copyRemoveAll.
            ns).
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
         'Category: prologue and epilogue\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         nonexistentBCAt: bci = ( |
            | 
            nonexistentBC copyForInterpreter: self BCI: bci).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         nonlocalReturn: bc = ( |
            | 
            "This also gets called for ^'s in non-block methods.
             (for syntactical compatibility with Smalltalk),
              but in that case the bytecode is a nop. -- dmu 5/05"
            method isReflecteeBlockMethod ifTrue: [| ep |
              ep: nlrPointEpilogueBB endNode.
              irNodeGenerator moveNLRHomeScopeTo:     ep framePointerValue.
              irNodeGenerator moveNLRHomeScopeDescTo: ep scopeDescValue.
              irNodeGenerator move: popStackValue To: ep outgoingResultValue.
              irNodeGenerator branchToLabel: nlrPointEpilogueBB labelNode.
              [aaaaaaa irNodeGenerator nonlocalReturn: orv FramePointer: hsv ScopeDesc: hsdv].
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
            currentBC: nonexistentBCAt: 0.
            isInlined ifFalse: [irNodeGenerator start].
            irNodesByBCI ifNotNil: [justReachedBCI: 0].
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
                pushStackValue. "So that it can be popped next BC. Can't just skip both BCs because
                                 there might be a branch to the next BC."
                ^ bc
              ].
            ].

            mir isReflecteeBlock ifFalse: [
              irNodeGenerator moveConstant: bc oopToPush To: pushStackValue.
            ] True: [
              irNodeGenerator cloneBlockLiteral: (createCompiledBlockFor: mir) Into: pushStackValue.
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
         'Category: data flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         recordDefinersAndUsers = ( |
            | 
            nodesInControlFlowOrder do: [|:n| n recordDefinersAndUsers].
            self).
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
        
         selfMap = ( |
            | 
            sourceLevelAllocator context selfMap).
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
              n: (irNodeGenerator irNodeProtos primitiveProtoForBC: bc) copyBC: bc.
              n setSpecialMode.
              n isSpecialCompilationMode
                      ifTrue: [irNodeGenerator addNode:     n]
                       False: [irNodeGenerator addSendNode: n RcvrAndArgs: rcvrAndArgs Result: result].
            ] False: [
              irNodeGenerator addSendNode: (irNodeGenerator irNodeProtos send copyBC: bc) RcvrAndArgs: rcvrAndArgs Result: result.
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
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         splitControlFlowEdges = ( |
            | 
            "The idea is to make sure that no control-flow edge
             leads from a BB with multiple successors to a node
             with multiple predecessors. This makes some program
             transformations easier (because it lets us insert
             code along edges)."

            firstBB basicBlocksInControlFlowReversePostOrder do: [|:bb. succs|
              succs: bb controlFlowSuccs.
              succs size > 1 ifTrue: [
                succs do: [|:succ. preds|
                  preds: succ controlFlowPreds.
                  preds size > 1 ifTrue: [| bbToInsert |
                    bbToInsert: irNodeGenerator createNewBBBranchingTo: succ labelNode BC: bb endNode bc.
                    bb endNode replaceControlFlowSucc: succ labelNode
                                                 With: bbToInsert labelNode
                                            IfSucceed: []
                                               IfFail: raiseError.
                  ].
                ].
              ].
            ].

            self).
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
        
         scopeDesc.
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
        
         compiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         currentBB.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         currentBytecodeInterpreter.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         highestUsedBBIDNumber <- 0.
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
            n isLabel ifTrue: [| oldBB. oldBBEndNode |
              oldBB: currentBB.
              nodeToInsertAfter ifNotNil: [
                oldBBEndNode: oldBB endNode.
                nodeToInsertAfter canFallThrough ifTrue: [branchToLabel: n].
                endCurrentBasicBlock.
              ].
              currentBB: newBBStartingWith: n EndingWith: oldBBEndNode.
            ] False: [
              nodeToInsertAfter isNil || [nodeToInsertAfter canFallThrough not] ifTrue: [
                defineLabel.
              ].
            ].

            nodeToInsertAfter ifNotNil: [n insertAfter: nodeToInsertAfter].
            nodeToInsertAfter: n.
            currentBytecodeInterpreter firstBB ifNil: [currentBytecodeInterpreter firstBB: currentBB].
            n bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         addSendNode: n RcvrAndArgs: rcvrAndArgStackValues Result: resultStackValue = ( |
             lastNodeBeforeSettingUpTheSend.
            | 
            lastNodeBeforeSettingUpTheSend: nodeToInsertAfter.
            n rcvrAndArgValuesToMoveTo with: rcvrAndArgStackValues Do: [|:dstV. :srcV| move: srcV To: dstV].
            n firstNodeSettingUpTheSend: lastNodeBeforeSettingUpTheSend sourceSucc.
            addNode: n.
            move: n resultValue To: resultStackValue.
            n lastNodeSettingUpTheSendResult: nodeToInsertAfter.

            n bc isResend ifTrue: [
              compiler
                    rememberResend: n bc
                       FromContext: sourceLevelAllocator context
                    TopmostSelfMap: topBytecodeInterpreter context selfMap.
            ].

            currentBC).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         artificiallyDetachTheRestOfTheCurrentBasicBlock = ( |
             brnch.
             lbl.
             precedingBB.
            | 
            precedingBB: currentBB.
            lbl: newLabel.
            brnch: newBranchNodeForLabel: lbl.
            addNode: brnch.
            bindLabel: lbl.
            brnch forgeControlFlowPredLinks.
            lbl basicBlock recordImmediateDominator: precedingBB immediateDominator.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         artificiallyInsertANewBasicBlock = ( |
             labelOfInsertedBB.
            | 
            artificiallyDetachTheRestOfTheCurrentBasicBlock.
            labelOfInsertedBB: nodeToInsertAfter.
            artificiallyDetachTheRestOfTheCurrentBasicBlock.
            setInsertionPoint: labelOfInsertedBB.
            [labelOfInsertedBB basicBlock dominanceFrontier isEmpty] assert. "Just a sanity check."
            self).
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
         'Category: scoping\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         bytecodeInterpreterWithScope: s = ( |
            | 
            topBytecodeInterpreter allBytecodeInterpretersDo: [|:i|
              i scopeDesc == s ifTrue: [^ i].
            ].
            nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         cloneBlockLiteral: mir Into: dst = ( |
             literalValue.
             memoValue.
            | 
            literalValue: sourceLevelAllocator valueForConstant: mir reflectee.
               memoValue: sourceLevelAllocator valueForMemoizedBlockMirror: mir.
            addNode: irNodeProtos blockLiteral copyBC: currentBC
                                              Literal: literalValue
                                             Memoized: memoValue.
            move: memoValue To: dst.

            [aaaaa]. "Not the right solution in the long run, but for now the memoixedBlockLoc
                      needs to be a register. -- Adam, Apr. 2009"
            nodeToInsertAfter aaa_doNotCoalesce: true.
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
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         operatorString = '!='.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> 'notEqual' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForCompiler: c = ( |
            | 
            (copy compiler: c) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         createLocalReturnBB = ( |
             bc.
             lbl.
            | 
            bc: currentBytecodeInterpreter nonexistentBCAt: -1.
            lbl: irNodeProtos label copyBC: bc.
            currentBytecodeInterpreter localReturnBB: newBBStartingWith: lbl EndingWith: nil.
            "We'll hook it up later."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         createNLRPointEpilogue = ( |
             bc.
             ep.
             lbl.
            | 
            bc: currentBytecodeInterpreter nonexistentBCAt: -1.
            lbl: irNodeProtos label            copyBC: bc.
            ep:  irNodeProtos nlrPointEpilogue copyBC: bc.
            ep insertAfter: lbl.
            currentBytecodeInterpreter nlrPointEpilogueBB: newBBStartingWith: lbl EndingWith: ep.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         createNewBBBranchingTo: dstLabelNode BC: bc = ( |
             brnch.
             lbl.
            | 
            lbl:   irNodeProtos label               copyBC: bc.
            brnch: irNodeProtos unconditionalBranch copyBC: bc Destination: dstLabelNode.
            brnch insertAfter: lbl.
            newBBStartingWith: lbl EndingWith: brnch).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         currentBC = ( |
            | 
            currentBytecodeInterpreter currentBC).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: data slots\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlot: s InReceiverWithMap: rcvrMap RcvrAndArgs: rcvrAndArgs Result: result = ( |
            | 
            case
              if: [s isMethod    ] Then: raiseError
              If: [s isAssignable] Then: [| holderValue |
                                          holderValue: newValue.
                                          moveHolderOfDataSlot: s InReceiverOfType: rcvrMap Value: (rcvrAndArgs at: 0) Into: holderValue.
                                          addNode: irNodeProtos dataSlotAccess copyBC: currentBC Slot: s Data: result Holder: holderValue]
              If: [s isAssignment] Then: [| holderValue |
                                          holderValue: newValue.
                                          moveHolderOfDataSlot: s InReceiverOfType: rcvrMap Value: (rcvrAndArgs at: 0) Into: holderValue.
                                          addNode: irNodeProtos dataSlotAssignment copyBC: currentBC Slot: s Data: (rcvrAndArgs at: 1) Holder: holderValue.
                                          move: (rcvrAndArgs at: 0) To: result]
                                   Else: [addNode: irNodeProtos constantDataSlotAccess copyBC: currentBC Slot: s Data: result]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         defineLabel = ( |
             n.
            | 
            n: newLabel.
            addNode: n.
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         endCurrentBasicBlock = ( |
            | 
            currentBB endWith: nodeToInsertAfter.
            currentBB: nil.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         firstBB = ( |
            | 
            topBytecodeInterpreter firstBB).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         forgeControlFlowLinksStartingFrom: n1 Until: n2 = ( |
            | 
            n1 until: n2 Do: [|:n| n forgeControlFlowPredLinks].
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
            generateExit: [|:falseFork|
              addNode: irNodeProtos conditionalBranch
                                       copyBC: currentBC
                                     TrueFork: trueFork
                                    FalseFork: falseFork
                                         Obj1: objValue
                                         Obj2: otherObjValue
                                   Comparison: comparison.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         ifShouldInlineSend: key To: rcvrValue Then: inlineBlk Else: noInlineBlk = ( |
             m.
             ss.
             t.
            | 
            compiler optimizationPolicy shouldNeverDoInlining           ifTrue: [^ noInlineBlk value].
            machineLevelAllocator method isReflecteeBlockMethod         ifTrue: [^ noInlineBlk value]. "Would require customizing the block, which we don't do yet."
            t: rcvrValue mergedType.
            m: t knownMapUsingOracle: compiler oracleForEagerRelocation IfFail: [^ noInlineBlk value].

            ss: [|:exit|
                     key lookupSlotsUsing: sourceLevelAllocator context protoSlotFinder
                                     Self: m
                                   Holder: sourceLevelAllocator context outermostMethodHolder
                   IfAssignableParentSlot: [exit value: vector]
                ] exitValue.

            ss ifNone: noInlineBlk
                IfOne: [|:s| (compiler optimizationPolicy shouldInlineSlot: s For: rcvrValue Into: currentBytecodeInterpreter context Key: key) ifTrue: [
                               inlineBlk value: s With: t
                             ] False: noInlineBlk
                       ]
               IfMany: noInlineBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            topBytecodeInterpreter: pushNewInterpreterFor: machineLevelAllocator topSourceLevelAllocator InlinedSendNode: nil.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         inline: sendNode MethodSlot: s Key: key ReceiverType: rcvrType RcvrAndArgs: rcvrAndArgValues Result: resultValue = ( |
             i.
             nodeBefore.
             sla.
            | 
            sla: newSourceLevelAllocatorToInlineMethodSlot: s Key: key ReceiverType: rcvrType.
            i: pushNewInterpreterFor: sla InlinedSendNode: sendNode.
            sla context isForABlockMethod ifTrue: [i inlinedBlock: rcvrType theBlock originalBlock_replaceThisSlotWithTheValueSlot].
            i currentBC: i nonexistentBCAt: 0.
            rcvrAndArgValues with: sla incomingRcvrAndArgValues Do: [|:srcV. :dstV| move: srcV To: dstV].
            interpretCurrentSlot.
            [nodeToInsertAfter isLocalReturn] assert.
            move: nodeToInsertAfter outgoingResultValue To: resultValue.
            interpreterFinished.
            i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         inline: sendNode Slot: s Key: key ReceiverType: rcvrType RcvrAndArgs: rcvrAndArgValues Result: resultValue = ( |
            | 
            compiler dependsOnKey: key ResolvingToSlot: s.
            s isMethod ifTrue: [
              "Temporarily unhook the following nodes, so that the dominance algorithm will be able to run on
               just the inlined nodes."
              currentBB temporarilyUnhookFromSuccessorDuring: [
                [sendNode nodeToBranchToOnNLR basicBlock controlFlowSuccs soleElement == currentBytecodeInterpreter nlrPointEpilogueBB] assert.
                sendNode nodeToBranchToOnNLR basicBlock temporarilyUnhookFromSuccessorDuring: [
                  inline: sendNode MethodSlot: s Key: key ReceiverType: rcvrType RcvrAndArgs: rcvrAndArgValues Result: resultValue.
                  "Gotta move the renamed resultValue to the original resultValue."
                  move: nodeToInsertAfter destinationValue To: resultValue.
                  nodeToInsertAfter recordDefinersAndUsers. "Make sure the newly-inserted move node has its data-flow info hooked up properly."
                ].
              ].
            ] False: [| nodeBefore. rcvrMap |
              nodeBefore: nodeToInsertAfter.
              rcvrMap: rcvrType knownMapUsingOracle: compiler oracleForEagerRelocation IfFail: raiseError.
              dataSlot: s InReceiverWithMap: rcvrMap RcvrAndArgs: rcvrAndArgValues Result: resultValue.
              forgeControlFlowLinksStartingFrom: nodeBefore Until: nodeToInsertAfter.
              nodeBefore until: nodeToInsertAfter Do: [|:n|
                n forgeControlFlowPredLinks.
                n recordDefinersAndUsers.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         interpretCurrentSlot = ( |
            | 
            currentBytecodeInterpreter prologue.
            currentBytecodeInterpreter interpretSlot.
            currentBytecodeInterpreter epilogue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: scoping\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         interpreterFinished = ( |
             i.
            | 
            i: currentBytecodeInterpreter.
            i finished.
            currentBytecodeInterpreter: i inliningInterpreter.
            i).
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
            | compiler prototypes irNodes).
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
        
         localReturn: v = ( |
             lbl.
             localReturnBBEndNode.
            | 
            lbl: defineLabel.
            addNode: irNodeProtos localReturn copyBC: currentBC Result: v.
            localReturnBBEndNode: newBranchNodeForLabel: lbl.
            localReturnBBEndNode insertAfter: currentBytecodeInterpreter localReturnBB labelNode.
            currentBytecodeInterpreter localReturnBB endWith: localReturnBBEndNode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         machineLevelAllocator = ( |
            | compiler machineLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         move: srcValue To: dstValue = ( |
             n.
            | 
            srcValue = dstValue ifTrue: [^ self].
            n: newMoveNodeFrom: srcValue To: dstValue BC: currentBC.
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
             r.
            | 
            r: moveConstant: o To: dst.
            nodeToInsertAfter sourceValue location explicitNameForComment: n.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         moveConstantReflecteeOf: m To: dst = ( |
            | 
            moveConstant: m reflectee To: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: data slots\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         moveHolderOfDataSlot: s InReceiverOfType: rcvrMap Value: rcvrValue Into: dstValue = ( |
             holderMap.
            | 
            holderMap: compiler mapOfHolderOfSlot: s.
            holderMap == rcvrMap
               ifTrue: [ move:         rcvrValue          To: dstValue ]
                False: [ moveConstant: s holder reflectee To: dstValue ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: returning\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         moveNLRHomeScopeDescTo: dstValue = ( |
            | 
            moveConstant: sourceLevelAllocator context outermostScope To: dstValue NameForComment: 'outermost lexical scope'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: returning\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         moveNLRHomeScopeTo: dstValue = ( |
            | 
            addNode: irNodeProtos nlrHomeScope copyBC: currentBC Receiver: sourceLevelAllocator valueForIncomingReceiver Destination: dstValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         newBBStartingWith: s EndingWith: e = ( |
             bb.
            | 
            bb: compiler prototypes basicBlock copyStartingWith: s EndingWith: e.
            highestUsedBBIDNumber: highestUsedBBIDNumber succ.
            bb uniqueID: highestUsedBBIDNumber.
            bb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         newBranchNodeForLabel: lbl = ( |
            | 
            irNodeProtos unconditionalBranch copyBC: currentBC Destination: lbl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         newLabel = ( |
            | 
            irNodeProtos label copyBC: currentBC).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         newMoveNodeFrom: srcValue To: dstValue BC: bc = ( |
            | 
            irNodeProtos move copyBC: bc Source: srcValue Destination: dstValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         newSourceLevelAllocatorToInlineMethodSlot: s Key: key ReceiverType: rcvrType = ( |
             c.
             lps.
             rcvrMap.
             selfMap.
             sla.
            | 

            lps: s contents isReflecteeBlockMethod ifFalse: [nil] True: [rcvrType theBlock scopeDesc].
            rcvrMap: rcvrType knownMapUsingOracle: compiler oracleForEagerRelocation IfFail: raiseError.
            selfMap: s contents isReflecteeBlockMethod ifFalse: [rcvrMap] True: [(bytecodeInterpreterWithScope: lps) context selfMap].

            c: compiler prototypes compilationContext copyForSlot: (compiler oracleForEagerRelocation kleinifySlot: s)
                                                              Key: key
                                                             Self: selfMap
                                                         Receiver: rcvrMap
                                               LexicalParentScope: lps.

            sla: compiler prototypes sourceLevelAllocator
                                 copyForMachineLevelAllocator: machineLevelAllocator
                                                      Context: c.
            sla initializeValues.
            sla allocateIncomingAndPreallocatedLocations.
            sla).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         newValue = ( |
            | 
            machineLevelAllocator newValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: returning\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         nonlocalReturn: v FramePointer: fpv ScopeDesc: sdv = ( |
            | 
            addNode: irNodeProtos nonlocalReturn copyBC: currentBC Result: v FramePointer: fpv ScopeDesc: sdv.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: scoping\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         pushNewInterpreterFor: aSourceLevelAllocator InlinedSendNode: n = ( |
             i.
            | 
            i: compiler prototypes bytecodeInterpreter
                                    copyForIRNodeGenerator: self
                                                 Allocator: aSourceLevelAllocator.
            currentBytecodeInterpreter ifNotNil: [|:ci|
              ci inlinedInterpreters addLast: i.
              i inliningInterpreter: ci.
            ].
            n ifNotNil: [
              i nodeToBranchToOnNLR: n nodeToBranchToOnNLR.
              [i nodeToBranchToOnNLR controlFlowPreds isEmpty] assert.
            ].
            currentBytecodeInterpreter: i.
            createLocalReturnBB.
            createNLRPointEpilogue.
            i).
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
         'Category: building IR nodes\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         setInsertionPoint: n = ( |
            | 
            currentBytecodeInterpreter: n interpreter.
            nodeToInsertAfter: n.
            currentBB: n basicBlock.
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         topBytecodeInterpreter.
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
