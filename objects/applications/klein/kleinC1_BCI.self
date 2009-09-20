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
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         byteVectorLayout = ( |
            | 
            theVM byteVectorLayout).
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
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
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
            ((resend.copy
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

            [lexicalParentScopeDesc. lexicalParentScopeDesc: nil]. "browsing"
            m: m primitiveCopyAt: 'lexicalParentScopeDesc'
                     PutContents: (reflect: myScopeDesc)
                        IsParent: false
                      IsArgument: false
                      Annotation: slotAnnotation
                          IfFail: raiseError.

            m: m primitiveCopyAt: (theVM lens valueSlotNameFor:     blockLiteral With: self)
                     PutContents: (theVM lens valueSlotContentsFor: blockLiteral With: self)
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
            myScopeDesc nmethod: nm.
            myScopeDesc incomingRcvrSPOffset: incomingRcvrSPOffsetFor: myScopeDesc.
            myScopeDesc slotSPOffsets:           findSlotSPOffsetsFor: myScopeDesc.
            myScopeDesc pcOffsetsByBCI: findPCOffsetsByBCI.
            myScopeDesc inlinedScopes: (inlinedInterpreters copyMappedBy: [|:i| (i createScopeDescForNMethod: nm) inliningScope: myScopeDesc]) asVector ifNone: vector.
            myScopeDesc).
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
             previousNode.
            | 
            previousNode: irNodeGenerator nodeToInsertAfter.
            currentBC: nonexistentBCAt: method ifNil: 0 IfNotNil: [|:m| m codes size].
            irNodesByBCI ifNotNil: [|:v| justReachedBCI: v lastKey].
            orv: sourceLevelAllocator newValueForOutgoingResult.
            previousNode canFallThrough ifTrue: [
              irNodeGenerator move: popStackValue To: orv.
            ].
            irNodeGenerator localReturn: orv.
            nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: failure blocks\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         failBlockLiteralNode = ( |
             n.
            | 
            previousBC isNotNil && [previousBC isBlockLiteral] ifFalse: [^ nil].
            n: (irNodesByBCI at: previousBC pc) sourceSucc.
            [n isBlockLiteral] assert.
            n).
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
             previousPCO <- -1.
             r.
            | 
            r: irNodesByBCI mapBy: [|:n. :i| n pcOffsetIfPresent: [|:o| previousPCO: o. o] IfAbsent: previousPCO]
                             Into: myScopeDesc pcOffsetVector copySize: irNodesByBCI size.
            r ifNone: myScopeDesc pcOffsetVector).
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
            compiler optimizationPolicy shouldUseSSAForm ifTrue: [
              calculateDominance.
              convertToSSAForm.
            ].
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
         'Category: primitives\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: objVectReg At: indexSmiReg IfFail: fh = ( |
            | 
            fh assertVector:  objVectReg.
            fh assertInteger: indexSmiReg.
            fh assertBounds:  indexSmiReg InVector: objVectReg.
            layouts objVector
                  generateFor: objVectReg
                  IndexableAt: indexSmiReg
                         Into: dstReg
                         With: fh cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: objVectReg At: indexSmiReg Put: argReg IfFail: fh = ( |
            | 
            fh assertVector:  objVectReg.
            fh assertInteger: indexSmiReg.
            fh assertBounds:  indexSmiReg InVector: objVectReg.
            layouts objVector
                  generateFor: objVectReg
                  IndexableAt: indexSmiReg
                          Put: argReg
                         Temp: dstReg
                         With: fh cg.
            fh cg move: objVectReg To: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: byteVectReg ByteAt: indexSmiReg IfFail: fh = ( |
            | 
            fh assertByteVector:  byteVectReg.
            fh assertInteger:     indexSmiReg.
            fh assertBounds:      indexSmiReg InByteVector: byteVectReg.
            theVM byteVectorLayout
                  generateFor: byteVectReg
                  IndexableAt: indexSmiReg
                         Into: dstReg
                         With: fh cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: byteVectReg ByteAt: indexSmiReg Put: argSmiReg IfFail: fh = ( |
            | 
            fh assertByteVector:  byteVectReg.
            fh assertInteger:     indexSmiReg.
            fh assertBounds:      indexSmiReg InByteVector: byteVectReg.
            fh assert:              argSmiReg IsBetweenZeroAnd: 255.
            theVM byteVectorLayout
                  generateFor: byteVectReg
                  IndexableAt: indexSmiReg
                          Put: argSmiReg
                         Temp: dstReg
                         With: fh cg.
            fh cg move: byteVectReg To: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: comparisons\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dst Receiver: rcvr Eq: arg IfFail: fh = ( |
            | 
            irNodeGenerator binaryOp: irNodeGenerator binaryOperations objectComparisons equalTo
                                Into: dst
                            Operand1: rcvr
                            Operand2: arg
                              IfFail: fh.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: arithmetic\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dst Receiver: rcvr IntAdd: arg IfFail: fh = ( |
            | 
            fh assertInteger: rcvr.
            fh assertInteger: arg.
            irNodeGenerator binaryOp: irNodeGenerator binaryOperations integerArithmetic addition
                                Into: dst
                            Operand1: rcvr
                            Operand2: arg
                              IfFail: fh.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: arithmetic\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dst Receiver: rcvr IntDiv: arg IfFail: fh = ( |
            | 
            fh assertInteger: rcvr.
            fh assertInteger: arg.
            irNodeGenerator binaryOp: irNodeGenerator binaryOperations integerArithmetic division
                                Into: dst
                            Operand1: rcvr
                            Operand2: arg
                              IfFail: fh.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: comparisons\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dst Receiver: rcvr IntEQ: arg IfFail: fh = ( |
            | 
            fh assertInteger: rcvr.
            fh assertInteger: arg.
            irNodeGenerator binaryOp: irNodeGenerator binaryOperations objectComparisons equalTo
                                Into: dst
                            Operand1: rcvr
                            Operand2: arg
                              IfFail: fh.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: comparisons\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dst Receiver: rcvr IntGE: arg IfFail: fh = ( |
            | 
            fh assertInteger: rcvr.
            fh assertInteger: arg.
            irNodeGenerator binaryOp: irNodeGenerator binaryOperations integerComparisons greaterThanOrEqualTo
                                Into: dst
                            Operand1: rcvr
                            Operand2: arg
                              IfFail: fh.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: comparisons\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dst Receiver: rcvr IntGT: arg IfFail: fh = ( |
            | 
            fh assertInteger: rcvr.
            fh assertInteger: arg.
            irNodeGenerator binaryOp: irNodeGenerator binaryOperations integerComparisons greaterThan
                                Into: dst
                            Operand1: rcvr
                            Operand2: arg
                              IfFail: fh.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: comparisons\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dst Receiver: rcvr IntLE: arg IfFail: fh = ( |
            | 
            fh assertInteger: rcvr.
            fh assertInteger: arg.
            irNodeGenerator binaryOp: irNodeGenerator binaryOperations integerComparisons lessThanOrEqualTo
                                Into: dst
                            Operand1: rcvr
                            Operand2: arg
                              IfFail: fh.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: comparisons\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dst Receiver: rcvr IntLT: arg IfFail: fh = ( |
            | 
            fh assertInteger: rcvr.
            fh assertInteger: arg.
            irNodeGenerator binaryOp: irNodeGenerator binaryOperations integerComparisons lessThan
                                Into: dst
                            Operand1: rcvr
                            Operand2: arg
                              IfFail: fh.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: arithmetic\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dst Receiver: rcvr IntMul: arg IfFail: fh = ( |
            | 
            fh assertInteger: rcvr.
            fh assertInteger: arg.
            irNodeGenerator binaryOp: irNodeGenerator binaryOperations integerArithmetic multiplication
                                Into: dst
                            Operand1: rcvr
                            Operand2: arg
                              IfFail: fh.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: comparisons\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dst Receiver: rcvr IntNE: arg IfFail: fh = ( |
            | 
            fh assertInteger: rcvr.
            fh assertInteger: arg.
            irNodeGenerator binaryOp: irNodeGenerator binaryOperations objectComparisons notEqualTo
                                Into: dst
                            Operand1: rcvr
                            Operand2: arg
                              IfFail: fh.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: arithmetic\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dst Receiver: rcvr IntSub: arg IfFail: fh = ( |
            | 
            fh assertInteger: rcvr.
            fh assertInteger: arg.
            irNodeGenerator binaryOp: irNodeGenerator binaryOperations integerArithmetic subtraction
                                Into: dst
                            Operand1: rcvr
                            Operand2: arg
                              IfFail: fh.
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
         'Category: primitives\x7fCategory: accessing the VM\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dst Receiver: rcvr TheVMIfFail: fh = ( |
            | 
            irNodeGenerator moveConstant: theVM To: dst.
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
            myScopeDesc lookupKey: sourceLevelAllocator context lookupKey.
            myScopeDesc lexicalParentScope: sourceLevelAllocator context lexicalParentScope.
            myScopeDesc method: slot isMethod ifTrue: [slot contents] False: [slot contents reflectee].
            myScopeDesc methodHolder: compiler reflecteeOfHolderOfSlot: slot.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         interpret: bc = ( |
            | 
            currentBC ifNotNil: [currentBC isAPrefixCode ifFalse: [previousBC: currentBC]].
            currentBC: bc.
            justReachedBCI: bc pc.
            resend.interpret: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         interpretBodyOfSlot = ( |
            | 
            slot isMethod ifTrue: [interpretMethod]
                           False: [dataSlot: slot]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         interpretSlot = ( |
            | 
            prologue.
            interpretBodyOfSlot.
            epilogue.
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
            (irNodesByBCI at: bci) marksStartOfBC: true.

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
        
         layouts = ( |
            | 
            vmKit layouts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fCategory: double dispatch\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         localValueSlotContentsFor: bm = ( |
            | 
            bm valueSlot contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fCategory: double dispatch\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         localValueSlotNameFor: bm = ( |
            | 
            bm valueSlotName).
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
        
         myScopeDesc = ( |
            | 
            sourceLevelAllocator myScopeDesc).
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
         'Category: testing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         isAPrefixCode = bootstrap stub -> 'globals' -> 'false' -> ().
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
            ].
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         outermostScope = ( |
            | 
            sourceLevelAllocator outermostScope).
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
            peekAtNextBytecode ifNotNil: [|:nextBC|
              nextBC isPop ifTrue: [
                pushStackValue. "So that it can be popped next BC. Can't just skip both BCs because
                                 there might be a branch to the next BC."
                ^ bc
              ].
            ].

            mir: reflect: bc oopToPush.
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
         'Category: blocks\x7fCategory: double dispatch\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteValueSlotContentsFor: bm = ( |
            | 
            bm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fCategory: double dispatch\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteValueSlotNameFor: bm = ( |
            | 
            [originalBlock_replaceThisSlotWithTheValueSlot.
             originalBlock_replaceThisSlotWithTheValueSlot: nil]. "browsing"
            'originalBlock_replaceThisSlotWithTheValueSlot').
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
              (asMirror lookupKey: s) ifNone: [] IfOne: [
                s sendTo: self With: bc.
                ^ bc
              ] IfMany: [error: 'was that intentional?'].
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
              irNodeGenerator send: bc selector RcvrAndArgs: rcvrAndArgs Result: result For: bc.
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
             leads from a BB that doesn't end with an unconditional
             branch to a node with multiple predecessors. This makes
             some program transformations easier (because it lets us
             insert code along edges)."

            firstBB basicBlocksInControlFlowReversePostOrder do: [|:bb|
              bb endNode isUnconditionalBranch ifFalse: [| succs |
                succs: bb controlFlowSuccs.
                succs do: [|:succ. preds|
                  preds: succ controlFlowPreds.
                  preds size > 1 ifTrue: [| bbToInsert |
                    bbToInsert: irNodeGenerator createNewBBBranchingTo: succ labelNode BC: bb endNode bc.
                    bb endNode replaceControlFlowSucc: succ labelNode With: bbToInsert labelNode IfSucceed: [
                      bbToInsert labelNode controlFlowPreds add:    bb         endNode.
                      succ       labelNode controlFlowPreds remove: bb         endNode.
                      succ       labelNode controlFlowPreds add:    bbToInsert endNode.
                    ] IfFail: raiseError.
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
        
         previousBC.
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
         'Category: binary operations\x7fCategory: arithmetic\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         add: r1 From: r2 To: dst = ( |
            | 
            binaryOp: binaryOperations integerArithmetic addition Into: dst Operand1: r1 Operand2: r2 IfFail: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: binary operations\x7fCategory: arithmetic\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         addImm: anInt From: from To: dst = ( |
            | 
             add: from
            From: (sourceLevelAllocator valueForConstant: anInt)
              To: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         addNode: n = ( |
            | 
            n isLabel ifTrue: [| oldBBEndNode |
              nodeToInsertAfter ifNotNil: [
                oldBBEndNode: currentBB endNode.
                nodeToInsertAfter canFallThrough ifTrue: [branchToLabel: n].
                endCurrentBasicBlockWith: nodeToInsertAfter.
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
            n).
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
            n lastNodeSettingUpTheSendResult: move: n resultValue To: resultStackValue.

            n bc isResend ifTrue: [
              compiler
                    rememberResend: n bc
                       FromContext: sourceLevelAllocator context
                    TopmostSelfMap: topBytecodeInterpreter context selfMap.
            ].

            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         artificiallyDetachTheRestOfTheCurrentBasicBlock = ( |
             brnch.
             lbl.
             precedingBB.
            | 
            precedingBB: currentBB.
            lbl: newLabel.
            brnch: branchToLabel: lbl.
            bindLabel: lbl.
            brnch forgeControlFlowPredLinks.
            lbl basicBlock recordImmediateDominator: precedingBB immediateDominator.
            lbl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         artificiallyInsertANewBasicBlock = ( |
             labelOfInsertedBB.
            | 
            labelOfInsertedBB: artificiallyDetachTheRestOfTheCurrentBasicBlock.
                               artificiallyDetachTheRestOfTheCurrentBasicBlock.
            setInsertionPoint: labelOfInsertedBB.
            [labelOfInsertedBB basicBlock dominanceFrontier isEmpty] assert. "Just a sanity check."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: binary operations\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         binaryOp: op Into: dst Operand1: o1 Operand2: o2 IfFail: fh = ( |
            | 
            addNode: (fh ifNil: [irNodeProtos binaryOperationThatCannotFail] IfNotNil: [irNodeProtos binaryOperationThatCanFail])
                          copyBC: currentBC Operation: op Operand1: o1 Operand2: o2 Destination: dst FailureHandler: fh).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: binary operations\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         binaryOperations = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'abstract' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         layouts = ( |
            | 
            vmKit layouts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'abstract' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         integerArithmetic = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerArithmetic.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerArithmetic abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         addition = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'addition' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerArithmetic addition.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'addition' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateInto: dstReg Operand1: op1Reg ConstantOperand2: op2 With: cg = ( |
            | 
            cg addImm: op2 From: op1Reg To: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'addition' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateInto: dstReg Operand1: op1Reg Operand2: op2Reg IfFail: fh With: cg = ( |
            | 
            fh assertNoOverflow: 'addition' During: [
              cg a addo_To: dstReg From: op1Reg With: op2Reg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'addition' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateInto: dstReg Operand1: op1Reg Operand2: op2Reg With: cg = ( |
            | 
            cg add: op1Reg From: op2Reg To: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'addition' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         arithmeticShiftRight = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'arithmeticShiftRight' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerArithmetic arithmeticShiftRight.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'arithmeticShiftRight' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateInto: dstReg Operand1: op1Reg ConstantOperand2: nBits With: cg = ( |
            | 
            cg shiftRightArithImmBy: nBits From: op1Reg To: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'arithmeticShiftRight' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         division = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'division' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerArithmetic division.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'division' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateInto: dstReg Operand1: op1Reg Operand2: op2Reg IfFail: fh With: cg = ( |
            | 
            "Shifting the divisor first allows us to detect overflow
             resulting from 16r20000000 / -1 because it gets computed as
             (16r80000000 / -1) & ~3 instead of (16r80000000 / -4) & ~3.
             -- jb 6/03"
            cg withTemporaryRegisterDo: [|:tempReg|
              layouts smi generateDecode: op2Reg Into: tempReg With: cg.

              fh assertNoOverflow: 'division' During: [
                cg a divwo_To: dstReg From: op1Reg With: tempReg.
              ].
            ].

            "clear out tag bits"
            layouts object generateValueOf: dstReg Into: dstReg With: cg.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'division' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         multiplication = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'multiplication' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerArithmetic multiplication.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'multiplication' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateInto: dstReg Operand1: op1Reg Operand2: op2Reg IfFail: fh With: cg = ( |
            | 
            cg withTemporaryRegisterDo: [|:tempReg|
              layouts smi generateDecode: op1Reg Into: tempReg With: cg.
              fh assertNoOverflow: 'multiplication' During: [
                cg a mullwo_To: dstReg From: tempReg With: op2Reg.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'multiplication' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         shiftLeft = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'shiftLeft' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerArithmetic shiftLeft.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'shiftLeft' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateInto: dstReg Operand1: op1Reg ConstantOperand2: nBits With: cg = ( |
            | 
            cg shiftLeftImmBy: nBits From: op1Reg To: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'shiftLeft' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         subtraction = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'subtraction' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerArithmetic subtraction.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'subtraction' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateInto: dstReg Operand1: op1Reg Operand2: op2Reg IfFail: fh With: cg = ( |
            | 
            fh assertNoOverflow: 'subtraction' During: [
              cg a subo_To: dstReg From: op1Reg With: op2Reg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'subtraction' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerArithmetic' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         integerComparisons = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerComparisons.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerComparisons abstract.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations objectComparisons abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         greaterThan = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'greaterThan' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerComparisons greaterThan.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'greaterThan' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         generateConditionalBranchTo: trueFork With: cg = ( |
            | 
            cg a bgtDisp: trueFork.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'greaterThan' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         greaterThanOrEqualTo = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'greaterThanOrEqualTo' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerComparisons greaterThanOrEqualTo.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'greaterThanOrEqualTo' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         generateConditionalBranchTo: trueFork With: cg = ( |
            | 
            cg a bgeDisp: trueFork.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'greaterThanOrEqualTo' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         lessThan = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'lessThan' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerComparisons lessThan.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'lessThan' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         generateConditionalBranchTo: trueFork With: cg = ( |
            | 
            cg a bltDisp: trueFork.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'lessThan' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         lessThanOrEqualTo = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'lessThanOrEqualTo' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations integerComparisons lessThanOrEqualTo.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'lessThanOrEqualTo' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         generateConditionalBranchTo: trueFork With: cg = ( |
            | 
            cg a bleDisp: trueFork.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'lessThanOrEqualTo' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'integerComparisons' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         objectComparisons = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations objectComparisons.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> 'abstract' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateInto: dstReg Operand1: op1Reg Operand2: op2Reg IfFail: fh With: cg = ( |
            | 
            cg generateTest: [|:trueFork|
              [aaaaaaa platformDependent].
              cg a cmpwFrom: op1Reg With: op2Reg.
              generateConditionalBranchTo: trueFork With: cg.
            ] LoadBooleanInto: dstReg.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         equalTo = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> 'equalTo' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations objectComparisons equalTo.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> 'equalTo' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         generateConditionalBranchTo: trueFork With: cg = ( |
            | 
            cg a beqDisp: trueFork.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> 'equalTo' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         notEqualTo = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> 'notEqualTo' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent binaryOperations objectComparisons notEqualTo.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> 'notEqualTo' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         generateConditionalBranchTo: trueFork With: cg = ( |
            | 
            cg a bneDisp: trueFork.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> 'notEqualTo' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'binaryOperations' -> 'objectComparisons' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fCategory: labels\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         bindLabel: n = ( |
            | 
            n bc: currentBC.
            addNode: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fCategory: labels\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
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
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         byteVectorLayout = ( |
            | 
            theVM byteVectorLayout).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: scoping\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         bytecodeInterpreterWithScope: s = ( |
            | 
            topBytecodeInterpreter allBytecodeInterpretersDo: [|:i|
              i myScopeDesc == s ifTrue: [^ i].
            ].
            nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         cloneBlockLiteral: mir Into: dst = ( |
             blockNode.
             literalValue.
             memoValue.
            | 
            literalValue: sourceLevelAllocator valueForConstant: mir reflectee.
               memoValue: sourceLevelAllocator valueForMemoizedBlockMirror: mir.

            blockNode: addNode: irNodeProtos blockLiteral copyBC: currentBC
                                                         Literal: literalValue
                                                        Memoized: memoValue.

            moveMemoizedBlockValueOf: blockNode To: dst.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         codeGenerationMixin* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'codeGenerationMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         comment: msg = ( |
            | 
            addNode: irNodeProtos comment copyBC: currentBC String: msg value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fCategory: conditionals\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
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
        
         equal = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> 'equal' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodeGenerator parent comparisons equal.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> 'equal' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIfTrueFor: reg1 With: reg2 ThenBranchTo: lbl With: cg = ( |
            | 
            [aaaaaaa]. "Why does this and objectComparisons exist?"
            cg generateIf: reg1 Equals: reg2 ThenBranchTo: lbl.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> 'equal' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         operatorString = '='.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> 'equal' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> 'comparisons' -> 'abstract' -> ().
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
         'Category: locations\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         constantArgument: o = ( |
            | 
            sourceLevelAllocator valueForConstant: o).
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
         'Category: primitives\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         deferCloningOfPrimitiveFailBlockLiteral: n = ( |
             dstStackValue.
            | 
            dstStackValue: n sourceSucc destinationValue.
            n removeFromControlFlow.
            addNode: n.
            moveMemoizedBlockValueOf: n To: dstStackValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fCategory: labels\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         defineLabel = ( |
             n.
            | 
            n: newLabel.
            addNode: n.
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         endCurrentBasicBlock = ( |
            | 
            endCurrentBasicBlockWith: nodeToInsertAfter).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         endCurrentBasicBlockWith: n = ( |
            | 
            currentBB endWith: n.
            currentBB: nil.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         failBlockLiteralNode = ( |
            | 
            currentBytecodeInterpreter failBlockLiteralNode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         failBlockMessageArgument = ( |
            | 
            newValue).
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
         'Category: addressing objects\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateAddressOf: memObjReg Into: dstUntaggedAddressReg = ( |
            | 
            addNode: irNodeProtos address copyBC: currentBC Object: memObjReg Destination: dstUntaggedAddressReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: addressing objects\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg At: indexSmiReg Into: dstObjReg = ( |
            | 
            addNode: irNodeProtos indexedReadOrWrite copyBC: currentBC
                                                 KindOfData: irNodeProtos indexedReadOrWrite oopData
                                               KindOfAccess: irNodeProtos indexedReadOrWrite read
                                                       Base: memObjAddrReg
                                                      Index: indexSmiReg
                                                       Data: dstObjReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: addressing objects\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg At: indexSmiReg Put: dataObjReg BypassingWriteBarrier: shouldBypassWriteBarrier = ( |
            | 
            addNode: irNodeProtos indexedReadOrWrite copyBC: currentBC
                                                 KindOfData: irNodeProtos indexedReadOrWrite oopData
                                               KindOfAccess: (shouldBypassWriteBarrier
                                                                ifTrue: [irNodeProtos indexedReadOrWrite writeBypassingBarrier]
                                                                 False: [irNodeProtos indexedReadOrWrite write                ])
                                                       Base: memObjAddrReg
                                                      Index: indexSmiReg
                                                       Data: dataObjReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: addressing objects\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg AtConstant: index Into: dstObjReg = ( |
            | 
            generateForObjectAtAddress: memObjAddrReg
                                    At: (sourceLevelAllocator valueForConstant: index)
                                  Into: dstObjReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: addressing objects\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg AtConstant: index Put: dataObjReg BypassingWriteBarrier: shouldBypassWriteBarrier = ( |
            | 
            generateForObjectAtAddress: memObjAddrReg
                                    At: (sourceLevelAllocator valueForConstant: index)
                                   Put: dataObjReg
                 BypassingWriteBarrier: shouldBypassWriteBarrier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: vectors\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForVector: vectReg IfIndex: indexSmiReg Temp: tempReg VectorLayout: layout IsOutOfBoundsThenBranchTo: trueFork = ( |
            | 
            generateExit: [|:falseFork|
              addNode: irNodeProtos boundsCheck copyBC: currentBC
                                                Vector: vectReg
                                                 Index: indexSmiReg
                                                  Temp: tempReg
                                          VectorLayout: layout
                                              TrueFork: trueFork
                                             FalseFork: falseFork.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fCategory: conditionals\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objValue DoesNotEqual: otherObjValue ThenBranchTo: trueFork = ( |
            | 
            generateIf: objValue Is: comparisons notEqual With: otherObjValue ThenBranchTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fCategory: conditionals\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objValue Equals: otherObjValue ThenBranchTo: trueFork = ( |
            | 
            generateIf: objValue Is: comparisons equal With: otherObjValue ThenBranchTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fCategory: conditionals\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
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
         'Category: branches\x7fCategory: conditionals\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: smiValue IsBetweenZeroAnd: max ThenBranchTo: trueFork = ( |
            | 
            generateExit: [|:falseFork|
              addNode: irNodeProtos integerRangeTest
                                       copyBC: currentBC
                                      Integer: smiValue
                                          Max: max
                                     TrueFork: trueFork
                                    FalseFork: falseFork.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: secondary (i.e. map) type tests\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: obj Temp: temp HasAnyMapTypeIn: maps ThenBranchTo: trueFork = ( |
            | 
            generateExit: [|:falseFork|
              withTemporaryRegisterDo: [|:tempReg|
                addNode: irNodeProtos mapTypeTest copyBC: currentBC
                                                  Object: obj
                                                    Maps: maps
                                                    Temp: tempReg
                                                TrueFork: trueFork
                                               FalseFork: falseFork.
              ].
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: obj Temp: temp HasTag: t IsLikely: isLikely ThenBranchTo: trueFork = ( |
            | 
            generateExit: [|:falseFork|
              addNode: irNodeProtos tagTest copyBC: currentBC
                                            Object: obj
                                               Tag: t
                                          IsLikely: isLikely
                                          TrueFork: trueFork
                                         FalseFork: falseFork.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Temp: tempReg IsByteVectorThenBranchTo: trueFork = ( |
            | 
            generateExit: [|:falseFork|
              addNode: irNodeProtos byteVectorTest copyBC: currentBC
                                                   Object: objReg
                                                 TrueFork: trueFork
                                                FalseFork: falseFork.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         ifShouldInlineSend: key To: rcvrValue Then: inlineBlk Else: noInlineBlk = ( |
             h.
             m.
             ss.
             t.
            | 
            compiler optimizationPolicy shouldNeverDoInlining           ifTrue: [^ noInlineBlk value].
            machineLevelAllocator method isReflecteeBlockMethod         ifTrue: [^ noInlineBlk value]. "Would require customizing the block, which we don't do yet."

            t: rcvrValue mergedType.
            m: t knownMapUsingOracle: compiler objectsOracle IfFail: [^ noInlineBlk value].

            "Make sure that all nmethods on traits block are reusable."
            ('value' isPrefixOf: key selector) && [(topBytecodeInterpreter slot holder findReflecteeUsingOracle: compiler objectsOracle) _Eq: traits block] ifTrue: [^ noInlineBlk value].

            ss: [|:exit|
                     key lookupSlotsUsing: sourceLevelAllocator context protoSlotFinder
                                     Self: m
                                   Holder: sourceLevelAllocator context outermostMethodHolder
                   IfAssignableParentSlot: [exit value: vector]
                ] exitValue.

            ss ifNone: noInlineBlk
                IfOne: [|:s| s isMethod && [s contents isReflecteeBlockMethod && [(t theBlockIfFail: nil) isNil]] ifTrue: [^ noInlineBlk value].
                             (compiler optimizationPolicy shouldInlineSlot: s For: rcvrValue Into: currentBytecodeInterpreter context Key: key) ifTrue: [
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
            sla: newSourceLevelAllocatorToInlineContext: newContextForMethodSlot: s Key: key ReceiverType: rcvrType.
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
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         inline: sendNode Slot: s Key: key ReceiverType: rcvrType RcvrAndArgs: rcvrAndArgValues Result: resultValue = ( |
            | 
            (mightThisNMethodBeCustomizedForType: rcvrType) ifTrue: [
              compiler dependsOnKey: key ResolvingToSlot: s.
            ].

            s isMethod ifTrue: [
              "Temporarily unhook the following nodes, so that the dominance algorithm will be able to run on
               just the inlined nodes."
              currentBB temporarilyUnhookFromSuccessorDuring: [
                [sendNode nodeToBranchToOnNLR basicBlock controlFlowSuccs soleElement == currentBytecodeInterpreter nlrPointEpilogueBB] assert.
                sendNode nodeToBranchToOnNLR basicBlock temporarilyUnhookFromSuccessorDuring: [
                  inline: sendNode MethodSlot: s Key: key ReceiverType: rcvrType RcvrAndArgs: rcvrAndArgValues Result: resultValue.

                  "Gotta move the renamed resultValue to the original resultValue."
                  [aaaaaaa "This is confusing."].
                  nodeToInsertAfter isMove ifTrue: [
                    move: nodeToInsertAfter destinationValue To: resultValue.
                    nodeToInsertAfter recordDefinersAndUsers. "Make sure the newly-inserted move node has its data-flow info hooked up properly."
                  ] False: [
                    nodeToInsertAfter: nodeToInsertAfter sourcePred.
                    [nodeToInsertAfter isMove] assert.
                    move: nodeToInsertAfter destinationValue To: resultValue.
                    nodeToInsertAfter recordDefinersAndUsers. "Make sure the newly-inserted move node has its data-flow info hooked up properly."
                    nodeToInsertAfter: nodeToInsertAfter sourceSucc.
                  ].
                ].
              ].
            ] False: [| nodeBefore. rcvrMap |
              nodeBefore: nodeToInsertAfter.
              rcvrMap: rcvrType knownMapUsingOracle: compiler objectsOracle.
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
            currentBytecodeInterpreter interpretSlot.
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
         'Category: sends\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         isReceiverProbablyABoolean: sel = ( |
            | 
                (sel = 'ifTrue:')
            || [(sel = 'ifFalse:')
            || [(sel = 'ifTrue:False:')
            || [(sel = 'ifFalse:True:')
            || [(sel = 'not')
            || [(sel = '&&')
            || [(sel = '||')]]]]]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         isReceiverProbablyAnInteger: sel = ( |
            | 
                (sel = '+')
            || [(sel = '-')
            || [(sel = '*')
            || [(sel = '/')
            || [(sel = 'succ')
            || [(sel = 'pred')]]]]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         kindsOfPossibleTypes = ( |
            | 
            compiler prototypes dataValue kindsOfPossibleTypes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         layouts = ( |
            | 
            vmKit layouts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: addressing objects\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         loadByteAt: baseAddrReg IndexedBy: indexReg To: dstSmiReg = ( |
            | 
            addNode: irNodeProtos indexedReadOrWrite copyBC: currentBC
                                                 KindOfData: irNodeProtos indexedReadOrWrite byteData
                                               KindOfAccess: irNodeProtos indexedReadOrWrite read
                                                       Base: baseAddrReg
                                                      Index: indexReg
                                                       Data: dstSmiReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         loadConstantLocation: loc ToRegister: v = ( |
            | 
            move: (sourceLevelAllocator newValueWithLocation: loc) To: v).
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
             moveNodeForNLRResult.
            | 
            lbl: defineLabel.
            addNode: irNodeProtos localReturn copyBC: currentBC Result: v.

            moveNodeForNLRResult: newMoveNodeFrom: currentBytecodeInterpreter nlrPointEpilogueBB endNode outgoingResultValue
                                               To: v
                                               BC: currentBC.
            localReturnBBEndNode: newBranchNodeForLabel: lbl.
            moveNodeForNLRResult insertAfter: currentBytecodeInterpreter localReturnBB labelNode.
            localReturnBBEndNode insertAfter: moveNodeForNLRResult.
            currentBytecodeInterpreter localReturnBB endWith: localReturnBBEndNode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         locations = ( |
            | 
            vmKit locations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         machineLevelAllocator = ( |
            | compiler machineLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         maybeInlineSend: n = ( |
             key.
            | 
            key: vmKit lookupKey copyForSendBC: n bc.
            currentBytecodeInterpreter: n bc interpreter.
            ifShouldInlineSend: key To: n findRcvrStackValue Then: [|:s. :rcvrType. rcvrAndArgStackValues. resultStackValue|

              rcvrAndArgStackValues: n findRcvrAndArgStackValues.
              resultStackValue:      n findResultStackValue.

              n removeFromGraphInPreparationForInlining.

              comment: 'inlining call to ', s name.
              artificiallyInsertANewBasicBlock.

              inline: n Slot: s Key: key ReceiverType: rcvrType RcvrAndArgs: rcvrAndArgStackValues Result: resultStackValue.

              true
            ] Else: [
              false
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         mightThisNMethodBeCustomizedForType: t = ( |
            | 
            "I think we don't need any reusabilityConditions for inlines from any context except
             the top one, because the top one is the only thing that we customize on. -- Adam, July 2009"

            t isSelf && [t sourceLevelAllocator outermostScope == topBytecodeInterpreter outermostScope]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         move: srcValue To: dstValue = ( |
             n.
            | 
            srcValue = dstValue ifTrue: [^ nil].
            n: newMoveNodeFrom: srcValue To: dstValue BC: currentBC.
            addNode: n.
            dstValue isPlaceholder ifTrue: [currentBytecodeInterpreter unresolvedMoveNodes add: n].
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         moveConstant: o To: dst = ( |
            | 
            move: (sourceLevelAllocator valueForConstant: o) To: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         moveConstant: o To: dst NameForComment: name = ( |
             node.
            | 
            node: moveConstant: o To: dst.
            node sourceValue location explicitNameForComment: name.
            node).
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
         'Category: blocks\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         moveMemoizedBlockValueOf: blockLiteralNode To: dst = ( |
            | 
            move: blockLiteralNode outgoingMemoizedBlockValue To: dst).
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
            (bytecodeInterpreterWithScope: sourceLevelAllocator context outermostScope) ifNotNil: [|:i|
              move: machineLevelAllocator valueForStackPointer To: dstValue.
            ] IfNil: [
              addNode: irNodeProtos nlrHomeScope copyBC: currentBC Receiver: sourceLevelAllocator valueForIncomingReceiver Destination: dstValue.
            ].
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
         'Category: branches\x7fCategory: labels\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         newBranchNodeForLabel: lbl = ( |
            | 
            irNodeProtos unconditionalBranch copyBC: currentBC Destination: lbl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         newContextForMethodSlot: s Key: key ReceiverType: rcvrType = ( |
             lpScope.
             rcvrMap.
             selfMap.
            | 

            rcvrMap: rcvrType knownMapUsingOracle: compiler objectsOracle.
            lpScope: s contents isReflecteeBlockMethod ifFalse: [nil]     True: [rcvrType theBlock lexicalParentScopeDesc].
            selfMap: s contents isReflecteeBlockMethod ifFalse: [rcvrMap] True: [(bytecodeInterpreterWithScope: lpScope) context selfMap].

            compiler prototypes compilationContext copyForSlot: (compiler objectsOracle kleinifySlot: s)
                                                           Key: key
                                                          Self: selfMap
                                                      Receiver: rcvrMap
                                            LexicalParentScope: lpScope).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: branches\x7fCategory: labels\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
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
        
         newSourceLevelAllocatorToInlineContext: c = ( |
             sla.
            | 
            sla: compiler prototypes sourceLevelAllocator copyForMachineLevelAllocator: machineLevelAllocator Context: c.
            sla initializeValues.
            sla allocateIncomingAndPreallocatedLocations.
            sla).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         newValue = ( |
            | 
            machineLevelAllocator newValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         predictedTypeOfReceiver: rcvrValue ForSendsOf: sel = ( |
            | 
            compiler optimizationPolicy shouldDoTypePrediction ifFalse: [^ nil].
            rcvrValue knownType ifNotNil: [^ nil].

            (isReceiverProbablyABoolean: sel) ifTrue: [
              ^ kindsOfPossibleTypes union copyMerging:
                  ( ((locations constant copyForOop:  true) explicitNameForComment:  'true')
                  & ((locations constant copyForOop: false) explicitNameForComment: 'false')) asVector
            ].

            (isReceiverProbablyAnInteger: sel) ifTrue: [
              ^ kindsOfPossibleTypes union copyMerging:
                  vector copyAddFirst: kindsOfPossibleTypes knownMap copyForMap: vmKit maps smiMap
            ].

            nil).
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
        
         readLocalSlot: s Into: dstValue = ( |
            | 
            move: (sourceLevelAllocator valueForSlot: s) To: dstValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         send: sel RcvrAndArgs: rcvrAndArgs Result: dst For: bc = ( |
             rcvrValue.
            | 
            rcvrValue: rcvrAndArgs first.
            (predictedTypeOfReceiver: rcvrValue ForSendsOf: sel) ifNotNil: [|:t|
              t generateFor: rcvrValue TypeCasesDo: [|:newRcvrValue. n. newRcvrAndArgs|
                n: irNodeProtos send copyBC: bc Selector: sel.
                newRcvrAndArgs: rcvrAndArgs asVector copy at: 0 Put: newRcvrValue.
                addSendNode: n RcvrAndArgs: newRcvrAndArgs Result: dst.
              ] Else: [| n |
                n: irNodeProtos send copyBC: bc Selector: sel.
                addSendNode: n RcvrAndArgs: rcvrAndArgs Result: dst.
              ] With: self.
            ] IfNil: [| n |
              n: irNodeProtos send copyBC: bc Selector: sel.
              addSendNode: n RcvrAndArgs: rcvrAndArgs Result: dst.
            ].
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
         'Category: binary operations\x7fCategory: shifting\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         shiftLeftBy: shift From: src To: dst = ( |
            | 
            binaryOp: binaryOperations integerArithmetic shiftLeft Into: dst Operand1: src Operand2: shift IfFail: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: binary operations\x7fCategory: shifting\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         shiftLeftImmBy: nBits From: src To: dst = ( |
            | 
            shiftLeftBy: (sourceLevelAllocator valueForConstant: nBits)
                   From: src
                     To: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: binary operations\x7fCategory: shifting\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         shiftRightArithBy: shift From: src To: dst = ( |
            | 
            binaryOp: binaryOperations integerArithmetic arithmeticShiftRight Into: dst Operand1: src Operand2: shift IfFail: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: binary operations\x7fCategory: shifting\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         shiftRightArithImmBy: nBits From: src To: dst = ( |
            | 
            shiftRightArithBy: (sourceLevelAllocator valueForConstant: nBits)
                         From: src
                           To: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBeLazyAboutCloningPrimitiveFailBlocks = bootstrap stub -> 'globals' -> 'false' -> ().
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
         'Category: addressing objects\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         storeByteFrom: srcSmiReg To: baseAddrReg IndexedBy: indexReg = ( |
            | 
            addNode: irNodeProtos indexedReadOrWrite copyBC: currentBC
                                                 KindOfData: irNodeProtos indexedReadOrWrite byteData
                                               KindOfAccess: irNodeProtos indexedReadOrWrite writeBypassingBarrier
                                                       Base: baseAddrReg
                                                      Index: indexReg
                                                       Data: srcSmiReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKit = ( |
            | 
            compiler vmKit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         withTemporaryRegisterDo: blk = ( |
            | 
            blk value: newValue mustBeLocatedInARegister: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodeGenerator' -> 'parent' -> () From: ( | {
         'Category: local variables\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         writeLocalSlot: s From: srcValue = ( |
            | 
            move: srcValue To: sourceLevelAllocator valueForSlot: s).
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
