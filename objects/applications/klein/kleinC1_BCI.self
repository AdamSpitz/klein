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
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         irNodesByBCI.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         lastNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         myCompiler.
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
             localValue.
            | 
            localValue: sourceLevelAllocator valueForSlot: bc slot.
            bc isWrite ifTrue: [
              move: popStackValue To: localValue.
              pushSelf: bc.
            ] False: [
              move: localValue To: pushStackValue.
            ].
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: building IR\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         addNode: n = ( |
            | 
            [aaaaa]. "Duplicated. Bleh."
            machineLevelAllocator allocateOutgoingRcvrAndArgLocations: n requiredNumberOfOutgoingRcvrAndArgLocations.

            lastNode ifNotNil: [
              n sourcePred: lastNode.
              lastNode sourceSucc: n.
            ].
            lastNode: n.
            firstNode ifNil: [firstNode: n].
            n bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         addSendNodeProto: irNodeProto RcvrAndArgs: rcvrAndArgs Result: result = ( |
             n.
            | 
            n: irNodeProto copyBC: currentBC Compiler: myCompiler.
            n setSpecialMode.
            n isSpecialCompilationMode ifTrue: [^ addNode: n].

            machineLevelAllocator allocateOutgoingRcvrAndArgLocations: n requiredNumberOfOutgoingRcvrAndArgLocations.

            n rcvrAndArgValuesToMoveTo with: rcvrAndArgs Do: [|:dstV. :srcV| move: srcV To: dstV].
            addNode: n.
            move: n valueToGetResultFrom To: result.

            currentBC).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         bindLabel: n = ( |
            | 
            n bc: currentBC.
            addNode: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         branchToBCI: bci = ( |
             branchNode.
             dstStack.
            | 
            branchNode: newBranchNodeForLabel: labelNodeAtBCI: bci.
            dstStack: (stackValuesWhenBCIWas: bci) ifNil: [
              (vector copySize: currentStackValues size) mapBy: [|:x. :i|
                placeholderBranchTargetStackValue copyForStackIndex: i BranchNode: branchNode
              ]
            ].

            dstStack with: currentStackValues Do: [|:dstValue. :currValue. :i|
              move: currValue To: dstValue.
            ].
            addNode: branchNode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         branchToLabel: lbl = ( |
            | 
            addNode: newBranchNodeForLabel: lbl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         comment: msg = ( |
            | 
            [aaaaa]. "Do this."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         comparisons = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'comparisons' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes bytecodeInterpreter parent comparisons.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'comparisons' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'comparisons' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes bytecodeInterpreter parent comparisons abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'comparisons' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'comparisons' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         notEqual = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'comparisons' -> 'notEqual' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes bytecodeInterpreter parent comparisons notEqual.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'comparisons' -> 'notEqual' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIfTrueFor: reg1 With: reg2 ThenBranchTo: lbl With: cg = ( |
            | 
            cg generateIf: reg1 DoesNotEqual: reg2 ThenBranchTo: lbl.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'comparisons' -> 'notEqual' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> 'comparisons' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            (resend.copy
               currentStackValues:  currentStackValues  copyRemoveAll)
               unresolvedMoveNodes: unresolvedMoveNodes copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForCompiler: c = ( |
            | 
            copy initializeForCompiler: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: data slots\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlot: s = ( |
            | 
            case
              if: [s isMethod    ] Then: raiseError
              If: [s isAssignable] Then: [dataSlot: s InReceiverOfType: selfMirror RcvrAndArgs: (vector copyAddFirst: sourceLevelAllocator valueForSelf)                                             Result: pushStackValue]
              If: [s isAssignment] Then: [dataSlot: s InReceiverOfType: selfMirror RcvrAndArgs: (sourceLevelAllocator valueForSelf & (sourceLevelAllocator valueForIncomingRcvrOrArgAt: 1)) asVector Result: pushStackValue]
                                   Else: [dataSlot: s InReceiverOfType: selfMirror RcvrAndArgs: (vector copyAddFirst: sourceLevelAllocator valueForSelf)                                             Result: pushStackValue]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: data slots\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlot: s InReceiverOfType: rcvrMir RcvrAndArgs: rcvrAndArgs Result: result = ( |
            | 
            case
              if: [s isMethod    ] Then: raiseError
              If: [s isAssignable] Then: [| holderValue |
                                          holderValue: sourceLevelAllocator newValue.
                                          moveHolderOfDataSlot: s InReceiverOfType: rcvrMir Value: (rcvrAndArgs at: 0) Into: holderValue.
                                          addNode: irNodeProtos dataSlotAccess copyBC: currentBC Compiler: myCompiler Slot: s Data: result Holder: holderValue]
              If: [s isAssignment] Then: [| holderValue |
                                          holderValue: sourceLevelAllocator newValue.
                                          moveHolderOfDataSlot: s InReceiverOfType: rcvrMir Value: (rcvrAndArgs at: 0) Into: holderValue.
                                          addNode: irNodeProtos dataSlotAssignment copyBC: currentBC Compiler: myCompiler Slot: s Data: (rcvrAndArgs at: 1) Holder: holderValue.
                                          move: (rcvrAndArgs at: 0) To: result]
                                   Else: [addNode: irNodeProtos constantDataSlotAccess copyBC: currentBC Compiler: myCompiler Slot: s Data: result]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         defineLabel = ( |
             n.
            | 
            n: irNodeProtos label copyBC: currentBC Compiler: myCompiler.
            addNode: n.
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objValue DoesNotEqual: otherObjValue ThenBranchTo: trueFork = ( |
            | 
            addNode: irNodeProtos conditionalBranch
                                     copyBC: currentBC
                                   Compiler: myCompiler
                                Destination: trueFork
                                       Obj1: objValue
                                       Obj2: otherObjValue
                                 Comparison: comparisons notEqual).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         generateIf: v Is: oop ThenBranchToBCI: bci = ( |
            | 
            generateExit: [|:exitFork|
              generateIf: v DoesNotEqual: (sourceLevelAllocator valueForConstant: oop) ThenBranchTo: exitFork.
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
              firstTime ifTrue: [comment: 'initializing locals'. nilMirror: nil asMirror. firstTime: false].
              localValue: sourceLevelAllocator valueForSlot: s.
              s contents = nilMirror ifFalse: [
                moveConstantReflecteeOf: s contents To: localValue.
              ]
              True: [
                nilValue ifNil: [
                  nilValue: sourceLevelAllocator newValue.
                  loadNilInto: nilValue.
                ].
                move: nilValue To: localValue.
              ].
            ].
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
        
         initializeForCompiler: c = ( |
            | 
            myCompiler: c.
            c slot isMethod ifTrue: [| m |
              m: c method.
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
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         irNodeProtos = ( |
            | 
            myCompiler prototypes irNodes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: stack values\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         justReachedBCI: bci = ( |
            | 
            [(stackValuesByBCI at: bci) isNil] assert.
              stackValuesByBCI at: bci Put: currentStackValues asVector.

            (irNodesByBCI at: bci) ifNil: [
              irNodesByBCI at: bci Put: defineLabel.
            ] IfNotNil: [|:lbl|
              bindLabel: lbl.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         labelNodeAtBCI: bci = ( |
            | 
            (irNodesByBCI at: bci) ifNil: [| lbl |
              lbl: newLabel.
              irNodesByBCI at: bci Put: lbl.
              lbl
            ] IfNotNil: [|:lbl|
              lbl
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         loadNilInto: dstValue = ( |
            | 
            moveConstant: nil To: dstValue NameForComment: 'nil'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: prologue and epilogue\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         localReturn = ( |
            | 
            currentBC: nil.
            irNodesByBCI ifNotNil: [|:v| justReachedBCI: v lastKey].
            addNode: irNodeProtos localReturn copyBC: nil Compiler: myCompiler Value: popStackValue.
            nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         machineLevelAllocator = ( |
            | 
            myCompiler machineLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         move: srcValue To: dstValue = ( |
             n.
            | 
            srcValue = dstValue ifTrue: [^ self].
            n: irNodeProtos move copyBC: currentBC Compiler: myCompiler Source: srcValue Destination: dstValue.
            addNode: n.
            dstValue isPlaceholder ifTrue: [unresolvedMoveNodes add: n].
            currentBC).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         moveConstant: o To: dst = ( |
            | 
            move: (sourceLevelAllocator valueForConstant: o) To: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         moveConstant: o To: dst NameForComment: n = ( |
            | 
            [aaaaa]. "Make it remember the name for the comment."
            moveConstant: o To: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         moveConstantReflecteeOf: m To: dst = ( |
            | 
            moveConstant: m reflectee To: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         newBranchNodeForLabel: lbl = ( |
            | 
            irNodeProtos unconditionalBranch copyBC: currentBC Compiler: myCompiler Destination: lbl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         newLabel = ( |
             n.
            | 
            n: irNodeProtos label copyCompiler: myCompiler.
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         nonlocalReturn: bc = ( |
            | 
            "This also gets called for ^'s in non-block methods.
             (for syntactical compatibility with Smalltalk),
              but in that case the bytecode is a nop. -- dmu 5/05"
            method isReflecteeBlockMethod ifTrue: [
              addNode: irNodeProtos nonlocalReturn copyBC: bc Compiler: myCompiler Value: popStackValue.
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
         'Category: prologue and epilogue\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         prologue = ( |
            | 
            addNode: irNodeProtos start copyBC: nil Compiler: myCompiler.
            irNodesByBCI ifNotNil: [justReachedBCI: 0].
            generateInitializationForLocals.
            machineLevelAllocator isLeafMethod ifFalse: [addNode: irNodeProtos interruptPoint copyBC: nil Compiler: myCompiler].
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
              moveConstant: bc oopToPush To: pushStackValue.
            ] True: [| literalValue. memoValue |
              literalValue: sourceLevelAllocator valueForConstant: bc oopToPush.
              memoValue: machineLevelAllocator valueForMemoizedBlockMirror: mir.
              addNode: irNodeProtos blockLiteral copyBC: bc
                                               Compiler: myCompiler
                                                 Source: literalValue
                                            Destination: memoValue.
              move: memoValue To: pushStackValue.

              [aaa]. "Not the right solution in the long run, but for now the memoixedBlockLoc
                      needs to be a register. -- Adam, Apr. 2009"
              lastNode aaa_doNotCoalesce: true.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         pushSelf: bc = ( |
            | 
            move: sourceLevelAllocator valueForSelf To: pushStackValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: stack values\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         pushStackValue = ( |
             v.
            | 
            v: sourceLevelAllocator newStackValue.
            currentStackValues addLast: v.
            v).
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
            | myCompiler selfMirror).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         send: bc = ( |
             n.
             rcvrAndArgs.
             result.
            | 
            rcvrAndArgs: popRcvrAndArgsForSend: bc.
            result: pushStackValue.

            bc isPrimitive ifTrue: [^ addSendNodeProto: (irNodeProtos primitiveProtoForBC: bc) RcvrAndArgs: rcvrAndArgs Result: result].

            (slotToInlineForSendBC: bc) ifNil: [
              addSendNodeProto: irNodeProtos send RcvrAndArgs: rcvrAndArgs Result: result. "will be error"
            ] IfNotNil: [|:slot|
              [slot isMethod not] assert. "Don't know how to inline methods yet."
              [bc isSelfImplicit] assert. "Don't know how to inline non-self-sends yet."
              myCompiler dependsOnBC: bc ResolvingToSlot: slot.
              dataSlot: slot InReceiverOfType: selfMirror RcvrAndArgs: rcvrAndArgs Result: result.
            ].

            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldDoInlining = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         slotToInlineForSendBC: bc = ( |
             slot.
            | 
            shouldDoInlining             ifFalse: [^ nil].
            method isReflecteeBlockMethod ifTrue: [^ nil]. "Inlining things in block methods would require customizing the block, which we don't do yet."
            bc isSelfExplicit             ifTrue: [^ nil]. "Don't know how to inline non-self-sends yet."

            slot: bc asMessage lookupSoleSlotForCompiler: myCompiler.

            slot                           ifNil: [^ nil].
            slot isMethod                 ifTrue: [^ nil]. "Don't know how to inline methods yet."
            slot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         sourceLevelAllocator = ( |
            | 
            myCompiler sourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: stack values\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         stackValuesWhenBCIWas: bci = ( |
            | 
            stackValuesByBCI at: bci).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         stackValuesByBCI.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         unresolvedMoveNodes <- list copyRemoveAll.
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
