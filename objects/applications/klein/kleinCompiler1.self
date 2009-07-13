 '$Revision: 30.67 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: compiler\x7fCategory: compiler 1\x7fComment: The real deal as of 6/03 -- dmu\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         compiler1 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (\'pdp11\')'
        
         architecture <- 'pdp11'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         areCommentsEmitted <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation helpers\x7fComment: Used to allow the compiler to back out of the leaf
method optimization when it is determined for certain
that it does not apply.  -- jb 8/03\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         backoutBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation helpers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         codeGenerator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: bytecode method information\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         context.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: special complation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         hasOnNonLocalReturn <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         irNodeGenerator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         isLogUsed <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation helpers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         machineLevelAllocator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation helpers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: public'
        
         nmethodRelocators <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: special complation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         noGCAllowed <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: special complation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         noMapTest <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: special complation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         noSendsAllowed <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: oracle for eager relocation\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         oracleThatCannotDoEagerRelocation = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( |
             {} = 'Comment: Set linearizedObjects to me when you don\'t want to (or
can\'t) do eager relocation. -- Adam, 3/05\x7fModuleInfo: Creator: globals klein compiler1 parent oracleThatCannotDoEagerRelocation.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation helpers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (klein compiler1 oracleThatCannotDoEagerRelocation)\x7fVisibility: public'
        
         objectsOracle <- bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)'
        
         optimizationPolicy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: relocators\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         addNMethodRelocator: or = ( |
            | 
            nmethodRelocators add: or.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: relocators\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         addRelocator: or = ( |
            | 
            relocators addLast: or.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateIncomingAndPreallocatedLocations = ( |
            | 
            machineLevelAllocator allocateIncomingAndPreallocatedLocations.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateLocations = ( |
             locationAssigner.
            | 
            locationAssigner: prototypes locationAssigners graphColoring copyForCompiler: self.
            locationAssigner go).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentSlotsForAllocator: a = ( |
            | 
            case
              if: [slot isMethod    ] Then: [a argumentSlotsOfMethod]
              If: [slot isAssignment] Then: [vector copyAddFirst: slots argument copy]
                                      Else: [vector]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         assignableLocalSlotsForAllocator: a = ( |
            | 
            case
              if: [slot isMethod] Then: [a assignableLocalSlotsOfMethod]
              Else: [vector]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: basic blocks\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         basicBlocks = ( |
            | 
            firstBB basicBlocksInControlFlowReversePostOrder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: basic blocks\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         basicBlocksInControlFlowReversePostOrder = ( |
            | 
            firstBB basicBlocksInControlFlowReversePostOrder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         basicBlocksInOrderForCodeGeneration = ( |
            | 
            firstBB basicBlocksInControlFlowReversePostOrder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: building nmethods\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         buildNMethod = ( |
             nm.
            | 
            nm: vmKit nmethod copySize: codeGenerator generatedCodeSize.
            codeGenerator copyGeneratedCodeInto: nm.
            nmethodRelocators do: [|:rl| rl beForObject: nm].
            objectsOracle recordReusabilityConditions: reusabilityConditions ForNMethod: nm.
            nm relocators: relocators.
            nm frame: frame copy.
            nm topScope: firstBB labelNode bc interpreter createScopeDescForNMethod: nm.
            [aaaaa]. "It's not really necessary for nmethods to have their own lookupKey slot anymore,
                      except maybe as an optimization - saves the cost of that extra load during the
                      sendMessage_stub. But that might not be important."
            nm lookupKey: nm topScope lookupKey.
            nm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         calculateValueLiveness = ( |
             ns.
            | 
            ns: nodesInControlFlowPostOrder.

            ns do: [|:n| n initializeLivenessInformation].

            [| changed <- false |
             ns do: [|:n| n updateLivenessInformation ifTrue: [changed: true]].
             changed] whileTrue.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         checkThatItMakesSenseToCompileSlot: s ForSelf: selfMap AndReceiver: rcvrMap = ( |
            | 
            " There is one case in which self and rcvr are equal but
              the method is a block method:
              The receiver is a block which inherits a method, m, and also
              occurs in that same method m.
              For example: consider compiling the exitValue method as an outer method
              for the block in that method itself. -- Adam Spitz & David Ungar, 6/05"
            [
              selfMap == rcvrMap
                ifFalse: [ s contents isReflecteeBlockMethod ]
                   True: [ s contents isReflecteeBlockMethod not || [rcvrMap isBlock] ]
            ] assert: 'self is only not rcvr for block methods'.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         checkThatTheControlFlowLinksAreConsistent = ( |
            | 
            firstBB basicBlocksInControlFlowReversePostOrder do: [|:bb|
              bb controlFlowSuccsDo: [|:succ| [succ controlFlowPreds includes: bb] assert].
              bb controlFlowPredsDo: [|:pred| [pred controlFlowSuccs includes: bb] assert].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         codes = ( |
            | 
            slot isMethod ifTrue: [method codes] False: ['']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         compileForcingNonLeafIfNecessary = ( |
             compiler.
             fnl.
            | 

            "ForceNonLeaf is false at first, if the compiler
             decides to go for leaf method optimization but it fails,
             the backout block will let it abort and retry forcing non-leafyness
              -- dmu 8/03"

            "Code slightly contorted to use a loop instead of recursion, to make
             profiles come out cleaner. -- Adam, May 2009"

            fnl: shouldAttemptLeafMethodOptimization not.
            [| backoutError |
             compiler: copy.
             backoutError: [|:exit| compiler doCompileForceNonLeaf: fnl
                                                         IfBackout: [|:e| exit value: e].
                                    nil
                           ] exitValue.
             backoutError ifNil:  [^ compiler].
             fnl          ifTrue: [^ error: 'should not have backed out:', backoutError].
             fnl: true.
            ] loop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            (((((resend.copy
                                    relocators:                       relocators copy)
                             nmethodRelocators:                nmethodRelocators copy)
                         reusabilityConditions:            reusabilityConditions copy)
                    resendsByContextAndSelfMap:       resendsByContextAndSelfMap copy)
                               irNodeGenerator: nil)    "will be recreated").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForContext: cntxt Architecture: arch Oracle: oracle Debug: d Optimize: op = ( |
             c.
            | 
            checkThatItMakesSenseToCompileSlot: cntxt slot ForSelf: cntxt selfMap AndReceiver: cntxt rcvrMap.

            c: cntxt slot kleinCompilerPrototypeForMe copy.
            c architecture:                  arch.
            c context:                       cntxt.
            c debug:                         d.
            c relocators:                    list copyRemoveAll.
            c nmethodRelocators:             list copyRemoveAll.
            c objectsOracle:                 oracle.
            c optimizationPolicy:            op.

            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         debug: bool = ( |
            | 
            areCommentsEmitted:     bool.
            shouldGatherStatistics: bool.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         deferCloningBlocks = ( |
             stillBroken = bootstrap stub -> 'globals' -> 'false' -> ().
            | 
            stillBroken ifTrue: [^ self].

            optimizationPolicy shouldUseSSAForm ifFalse: [^ self].

            nodesSatisfying: [|:n| n isBlockLiteral] Do: [|:n. chains|
              chains: n outgoingMemoizedBlockValue strongUserChains.
              [chains size >= 1] assert: 'already eliminated dead code'.
              chains do: [|:chain|
                chain addFirst: n.
                chain removeLast prependACopyOfDeferredComputation: chain.
                chain do: [|:nn| nn removeFromControlFlow].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: nmethod reusability\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         dependsOnKey: k ResolvingToSlot: s = ( |
            | 
            [aaaaa]. "This could probably be factored a bit better."

            "I'm not completely sure about this, but I think that we don't need to worry about resends
             making the nmethod unreusable, since the resend lookup starts from the method holder rather
             than from self. -- Adam, Apr. 2009"
            k isResend && [(mapOfHolderOfSlot: irNodeGenerator sourceLevelAllocator slot) !== irNodeGenerator sourceLevelAllocator context selfMap] ifFalse: [
              reusabilityConditions add: reusableIfSlotIsTheSame copyForKey: k Slot: s.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         displayDebugInformationIfDesired = ( |
            | 
            shouldDisplayDebugInformation ifFalse: [^ self].
            printIRNodes.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         doCompile = ( |
            | 
            makeSureIRNodesAreBuilt.
            allocateIncomingAndPreallocatedLocations.
            doInlining.
            eliminateDeadCode.
            deferCloningBlocks.
            replacePhiFunctionsWithMoves.
            calculateValueLiveness.
            allocateLocations.
            eliminateEmptyBasicBlocks.
            displayDebugInformationIfDesired.
            generateCode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         doCompileForceNonLeaf: fnl IfBackout: blk = ( |
            | 
            backoutBlock: blk.
            initializeForceNonLeaf: fnl.
            doCompile).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         doInlining = ( |
            | 
            optimizationPolicy shouldNeverDoInlining ifTrue: [^ self].
            [tryToInlineSomething] whileTrue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         eliminateDeadCode = ( |
             nodes.
             valuesLeftToCheck.
            | 
            optimizationPolicy shouldUseSSAForm ifFalse: [^ self].
            nodes: nodesInControlFlowOrder asSet.
            valuesLeftToCheck: machineLevelAllocator allValues copy.
            [valuesLeftToCheck isEmpty] whileFalse: [| v |
              v: valuesLeftToCheck removeFirst.
              v strongUsers isEmpty ifTrue: [
                "Can't eliminate values that might be uplevel-accessed."
                v hasLocation not || [v location isRegister] ifTrue: [
                  v definers isEmpty ifFalse: [| d |
                    [v definers size = 1] assert. "Since this is SSA."
                    d: v definers first.
                    (d hasNoSideEffectsOtherThanDefining: v) ifTrue: [
                      d remove.
                      v definers removeAll.

                      "Can't call usedValuesDo: because we need to eliminate duplicates."
                      d usedValues do: [|:usedV|

                        usedV strongUsers remove: d IfAbsent: [
                          usedV weakUsers remove: d IfAbsent: [
                            "If the node isn't reachable, it won't have recorded
                             itself as a user and definer."
                            [(nodes includes: d) not] assert.
                          ].
                        ].
                        "usedV might be dead now, so gotta check it."
                        valuesLeftToCheck add: usedV.

                      ].
                    ].
                  ].
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         eliminateEmptyBasicBlocks = ( |
            | 
            basicBlocks do: [|:bb| bb eliminateIfPossible].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: basic blocks\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         firstBB = ( |
            | 
            irNodeGenerator firstBB).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         frame = ( |
            | 
            machineLevelAllocator frame).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCode = ( |
             ns.
            | 
            codeGenerator: protoCodeGenForMyPlatform copyForCompiler: self.
            ns: irNodesInOrderForCodeGeneration asVector.
            ns do: [|:n. :i|
              codeGenerator nodeToGenerate: n.
              codeGenerator nodeToBeGeneratedAfterThisOne: ns at: i succ IfAbsent: nil.
              n generateCode.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForceNonLeaf: fnl = ( |
            | 
            machineLevelAllocator:
                 protoAllocatorForMyPlatform copyForCompiler: self
                                                ForceNonLeaf: fnl.

            machineLevelAllocator topSourceLevelAllocator:
                prototypes sourceLevelAllocator copyForMachineLevelAllocator: machineLevelAllocator
                                                                     Context: context.

            machineLevelAllocator topSourceLevelAllocator initializeValues.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         inlineOneLevelDeep = ( |
            | 
            [aaaaaaa].
            nodesSatisfying: [|:n| n isSend] Do: [|:n|
              irNodeGenerator maybeInlineSend: n.
            ].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         irNodesInOrderForCodeGeneration = ( |
             ns.
            | 
            ns: list copyRemoveAll.
            basicBlocksInOrderForCodeGeneration do: [|:bb|

              "If the previous BB always branches directly to this one, the branch node is unnecessary."
              ns isEmpty not && [ns last isUnconditionalBranch && [ns last destinationNode = bb labelNode]] ifTrue: [ns removeLast].

              bb nodesDo: [|:n| ns addLast: n].

            ].
            ns).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isMethod = ( |
            | 
            method isReflecteeMethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         localSlotsForAllocator: a = ( |
            | 
            case
              if: [slot isMethod] Then: [a localSlotsOfMethod]
              Else: [vector]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: helper prototypes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         locations = ( |
            | 
            vmKit locations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         log = ( |
            | codeGenerator log).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         makeSureIRNodesAreBuilt = ( |
            | 
            irNodeGenerator ifNil: [
              irNodeGenerator: prototypes irNodeGenerator copyForCompiler: self.

              irNodeGenerator interpretCurrentSlot.
              irNodeGenerator endCurrentBasicBlock.
              irNodeGenerator interpreterFinished.

              checkThatTheControlFlowLinksAreConsistent.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: maps\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOfHolderOfSlot: s = ( |
            | s holder findVMKitMapUsingOracle: objectsOracle).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         method = ( |
            | 
            slot contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         methodStartInstruction = ( |
            | 
            (protoCodeGenForMyPlatform copyForCompiler: self) methodStartInstruction).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: nodes\x7fComment: Me & my transitive successors such that no node is
included before at least one of its preds is.\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         nodesInControlFlowOrder = ( |
             ns.
            | 
            ns: list copyRemoveAll.
            firstBB depthFirstControlFlowTraversalPreorderDo: [|:bb| bb nodesDo: [|:n| ns addLast: n]] PostorderDo: [] AlreadyDid: set copyRemoveAll.
            ns).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         nodesInControlFlowPostOrder = ( |
             ns.
            | 
            ns: list copyRemoveAll.
            firstBB depthFirstControlFlowTraversalPreorderDo: [] PostorderDo: [|:bb| bb reverseNodesDo: [|:n| ns addLast: n]] AlreadyDid: set copyRemoveAll.
            ns).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         nodesSatisfying: conditionBlk Do: blk = ( |
            | 
            "Could optimize this by keeping track of them as we generate them."
            nodesInControlFlowOrder do: [|:n|
              (conditionBlk value: n) ifTrue: [
                blk value: n.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: nmethod reusability\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         notReusable = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'notReusable' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent notReusable.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'notReusable' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isSatisfiedBy: aCompilationRequester = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'notReusable' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         oracleForEagerRelocationInsideKlein = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( |
             {} = 'Comment: Set linearizedObjects to me when you don\'t want to (or
can\'t) do eager relocation. -- Adam, 3/05\x7fModuleInfo: Creator: globals klein compiler1 parent oracleForEagerRelocationInsideKlein.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         exemplarForMap: map = ( |
            | 
            [aaaaaaa]. "WRONG."
            map).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         findOopForStubNMethodNamed: name IfPresent: pb IfAbsent: ab = ( |
            | 
            klein primitives _Map _NMethodCache
                findFirst: [|:nm| nm lookupKey selector = name]
                IfPresent: pb
                 IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedObjectForOriginalMirror: origMir = ( |
            | 
            origMir reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifySlot: s = ( |
            | 
            s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         mapForOriginalMirror: origMir = ( |
            | 
            mapForOriginalObject: origMir reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         mapForOriginalObject: o = ( |
            | 
            o _Map).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         nextOopToAllocateForObjectOfSize: nOops = ( |
            | 
            -1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOriginalObject: obj IfAbsent: fb = ( |
            | 
            obj).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         originalObjectForOop: oop IfAbsent: fb = ( |
            | 
            oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         recordReusabilityConditions: rcs ForNMethod: nm = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         relocatorAssembledPlaceholderInstructions = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         relocatorAssembledRealInstructions = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         findOopForStubNMethodNamed: name IfPresent: pb IfAbsent: ab = ( |
            | 
            ab value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         nextOopToAllocateForObjectOfSize: nOops = ( |
            | 
            -1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOriginalObject: obj IfAbsent: fb = ( |
            | 
            fb value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         originalObjectForOop: oop IfAbsent: fb = ( |
            | 
            fb value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         relocatorAssembledPlaceholderInstructions = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         relocatorAssembledRealInstructions = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         printIRNodes = ( |
            | 
            "It'd be even cooler if we had a morph so we could
             look at the real node objects."

            '' printLine.
            ('IR nodes for ', machineLevelAllocator topSourceLevelAllocator slot printString) printLine.
            basicBlocksInOrderForCodeGeneration do: [|:bb|
              '' printLine.
              bb nodesDo: [|:n| n commentString printLine].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: helper prototypes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         protoAllocatorForMyPlatform = ( |
            | 
            prototypes machineLevelAllocators prototypeForArchitecture: architecture).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: helper prototypes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         protoCodeGenForMyPlatform = ( |
            | 
            [ppc. sparc]. "browsing"
            architecture sendTo: prototypes codeGenerators).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: helper prototypes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         protoSlotFinder = ( |
            | 
            theVM exportPolicy slotFinder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot'
        
         prototypes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         basicBlock = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes basicBlock.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> () From: ( | {
         'Category: dominance\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: public'
        
         dominanceFrontier <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         endNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> () From: ( | {
         'Category: dominance\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         immediateDominator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> () From: ( | {
         'Category: dominance\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: public'
        
         immediatelyDominatedBBs <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         labelNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes basicBlock parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: static single-assignment form\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         appendMoveNode: moveNode = ( |
            | 
            [endNode isUnconditionalBranch] assert.
            moveNode insertAfter: endNode sourcePred.
            moveNode recordDefinersAndUsers.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         basicBlocksInControlFlowReversePostOrder = ( |
             bbs.
            | 
            bbs: list copyRemoveAll.
            depthFirstControlFlowTraversalPreorderDo: [] PostorderDo: [|:bb| bbs addFirst: bb] AlreadyDid: set copyRemoveAll.
            bbs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         checkInvariants = ( |
            | 
            [labelNode isLabel] assert.
            [endNode canFallThrough not] assert.
            [nodes asVector reverse = reverseNodes asVector] assert.
            nodesDo: [|:n| [n canFallThrough || [n = endNode]] assert].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: eliminating\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         containsNothingButABranch = ( |
            | 
            endNode isUnconditionalBranch && [labelNode sourceSucc = endNode]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         controlFlowPreds = ( |
            | 
            labelNode controlFlowPreds copyMappedBy: [|:n| n basicBlock]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         controlFlowPredsDo: blk = ( |
            | 
            labelNode controlFlowPredsDo: [|:n| blk value: n basicBlock].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         controlFlowSuccs = ( |
             succs.
            | 
            succs: list copyRemoveAll.
            controlFlowSuccsDo: [|:succ| succs add: succ].
            succs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         controlFlowSuccsDo: blk = ( |
            | 
            endNode controlFlowSuccsDo: [|:lbl| blk value: lbl basicBlock].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            (resend.copy
              immediatelyDominatedBBs: immediatelyDominatedBBs copy)
                    dominanceFrontier: dominanceFrontier copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyStartingWith: s EndingWith: e = ( |
             c.
            | 
            c: copy.
            c startWith: s.
            e ifNotNil: [
              c endWith: e.
            ].
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         definedValues = ( |
             vs.
            | 
            vs: set copyRemoveAll.
            definedValuesDo: [|:v| vs add: v].
            vs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         definedValuesDo: blk = ( |
            | 
            nodesDo: [|:n| n definedValuesDo: blk].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         depthFirstControlFlowTraversalPreorderDo: preBlk PostorderDo: postBlk AlreadyDid: done = ( |
            | 
            done add: self.
            preBlk value: self.
            controlFlowSuccsDo: [|:bb|
              (done includes: bb) ifFalse: [
                bb depthFirstControlFlowTraversalPreorderDo: preBlk PostorderDo: postBlk AlreadyDid: done.
              ].
            ].
            postBlk value: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: dominance\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         depthFirstDominatorTreeTraversalPreorderDo: preBlk PostorderDo: postBlk = ( |
             stuffToDo.
            | 
            "Iterative instead of recursive, because once I ran out of stack space
             compiling a long method."
            stuffToDo: list copyRemoveAll.
            stuffToDo add: self @ 'pre'.
            [stuffToDo isEmpty] whileFalse: [| thingToDo. bb |
              thingToDo: stuffToDo removeLast.
              bb: thingToDo x.
              thingToDo y = 'pre' ifTrue: [
                preBlk value: bb.
                stuffToDo addLast: bb @ 'post'.
                bb immediatelyDominatedBBs do: [|:domBB| stuffToDo addLast: domBB @ 'pre'].
              ] False: [
                postBlk value: bb.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: eliminating\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         eliminateIfPossible = ( |
            | 
            containsNothingButABranch ifTrue: [| anyFailed <- false |
              labelNode controlFlowPreds do: [|:pred|
                pred replaceControlFlowSucc: labelNode
                                       With: endNode destinationNode
                                  IfSucceed: [endNode destinationNode controlFlowPreds add: pred]
                                     IfFail: [anyFailed: true].
              ].
              anyFailed ifFalse: [endNode destinationNode controlFlowPreds remove: endNode].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         endWith: n = ( |
            | 
            [n canFallThrough not] assert.
            endNode: n.
            n basicBlock: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: static single-assignment form\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         insertPhiFunction: phi = ( |
            | 
            phi insertAfter: labelNode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: iterating over nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         nodes = ( |
             ns.
            | 
            ns: list copyRemoveAll.
            nodesDo: [|:n| ns add: n].
            ns).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: iterating over nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         nodesDo: blk = ( |
             n.
            | 
            n: labelNode.
            [blk value: n.
             n = endNode ifTrue: [^ self].
             n: n sourceSucc.
            ] loop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: static single-assignment form\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         phiFunctionsDo: blk = ( |
             n.
            | 
            n: labelNode sourceSucc.
            [n isPhiFunction] whileTrue: [
              blk value: n.
              n: n sourceSucc.
            ].
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: static single-assignment form\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         phiFunctionsVanishAndDo: blk = ( |
             nodeAfter.
            | 
            nodeAfter: phiFunctionsDo: [|:phi|
              blk value: phi.
              phi      definedValuesDo: [|:v| v definers    remove: phi IfAbsent: 'confused, but maybe OK'].
              phi stronglyUsedValuesDo: [|:v| v strongUsers remove: phi IfAbsent: 'confused, but maybe OK'].
              phi   weaklyUsedValuesDo: [|:v| v weakUsers   remove: phi IfAbsent: 'confused, but maybe OK'].
            ].
            labelNode sourceSucc: nodeAfter.
            nodeAfter sourcePred: labelNode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: dominance\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         recordImmediateDominator: d = ( |
            | 
            immediateDominator: d.
            d ifNotNil: [d immediatelyDominatedBBs add: self].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: iterating over nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         reverseNodes = ( |
             ns.
            | 
            ns: list copyRemoveAll.
            reverseNodesDo: [|:n| ns add: n].
            ns).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: iterating over nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         reverseNodesDo: blk = ( |
             n.
            | 
            n: endNode.
            [blk value: n.
             n = labelNode ifTrue: [^ self].
             n: n sourcePred.
            ] loop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         startWith: lbl = ( |
            | 
            [lbl isLabel] assert.
            labelNode: lbl.
            lbl basicBlock: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            "Put an L in front so that it's easier to search through
             printouts for references to this BB."
             'L', uniqueID printString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         temporarilyUnhookFromSuccessorDuring: blk = ( |
             brnch.
             lbl.
             r.
            | 
            brnch: endNode.
            [brnch isUnconditionalBranch] assert.
            lbl: brnch destinationNode.
            [lbl isLabel] assert.
            brnch sourceSucc: nil.
            brnch destinationNode: nil.
            lbl controlFlowPreds remove: brnch.

            r: blk value.

            brnch sourceSucc: lbl.
            brnch destinationNode: lbl.
            brnch forgeControlFlowPredLinks.

            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'basicBlock' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         uniqueID.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         compilationContext = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes compilationContext.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         lexicalParentScope.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         lookupKey.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes compilationContext parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSlot: s = ( |
            | 
            copyForSlot: s Self: s holderMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSlot: s Key: k Self: sMap Receiver: rMap LexicalParentScope: lps = ( |
            | 
            ((((copy slot: s) lookupKey: k) selfMap: sMap) rcvrMap: rMap) lexicalParentScope: lps).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSlot: s Self: sMap = ( |
            | 
            copyForSlot: s Self: sMap LexicalParentScope: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSlot: s Self: sMap LexicalParentScope: lps = ( |
            | 
            copyForSlot: s Key: (klein lookupKey copyForNormalSend: s name) Self: sMap Receiver: s holderMap LexicalParentScope: lps).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isForABlockMethod = ( |
            | 
            lexicalParentScope isNotNil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isOuterMethodForABlock = ( |
            | 
            rcvrMap isBlock && [rcvrMap == selfMap]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         lexicalParentCount = ( |
             n <- 0.
            | 
            lexicalParentScopesReverseDo: [|:s. :ll| n: ll].
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         lexicalParentScopes = ( |
             r.
            | 
            r: list copyRemoveAll.
            lexicalParentScopesReverseDo: [|:s| r addFirst: s].
            r asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         lexicalParentScopesReverseDo: blk = ( |
             ll <- 0.
             lps.
            | 
            lps: lexicalParentScope.
            [lps isNil] whileFalse: [
              ll: ll succ.
              blk value: lps With: ll.
              lps: lps lexicalParentScope.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         method = ( |
            | 
            slot contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         outermostMethodHolder = ( |
            | 
            isForABlockMethod ifFalse: [^ slot holder].
            reflect: outermostScope methodHolder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         outermostMethodSlotName = ( |
            | 
            isForABlockMethod ifFalse: [^ slot name].
            outermostScope lookupKey selector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         outermostScope = ( |
             lps.
            | 
            lexicalParentScopesReverseDo: [|:s| lps: s].
            lps).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         protoSlotFinder = ( |
            | 
            theVM exportPolicy slotFinder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceMap: m1 With: m2 = ( |
             c.
            | 
            m1 == m2 ifTrue: [^ self].
            (rcvrMap == m1) || [selfMap == m1] ifFalse: [^ self].
            c: copy.
            rcvrMap == m1 ifTrue: [c rcvrMap: m2].
            selfMap == m1 ifTrue: [c selfMap: m2].
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         rcvrMap.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         selfMap.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         slot.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         dominanceCalculator = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dominanceCalculator.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         allNodes.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         dominanceFrontiers.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         dominators.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         indicesByNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> 'parent' -> () From: ( |
             {} = 'Comment: Based on the algorithm described in \"A Simple, Fast Dominance Algorithm\",
by Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy.

http://www.hipersoft.rice.edu/grads/publications/dom14.pdf

I chose that one because I liked the philosophy - emphasize simplicity,
ease-of-correct-implementation, and practical performance over theoretical
huge-case performance.

I haven\'t yet taken the same care that they took to optimize the data
structures, but I tried to copy the general form of the algorithm so
that we can improve performance later if necessary.

-- Adam, Apr. 2009\x7fModuleInfo: Creator: globals klein compiler1 parent prototypes dominanceCalculator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> 'parent' -> () From: ( | {
         'Category: calculating dominance frontiers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         calculateDominanceFrontiers = ( |
            | 
            allNodes do: [|:b. preds|
              preds: b controlFlowPreds.
              preds size >= 2 ifTrue: [| bDom |
                bDom: dominators at: indexOf: b.
                preds do: [|:p. runner|
                  runner: p.
                  [runner == bDom] whileFalse: [| ri |
                    ri: indexOf: runner.
                    runner dominanceFrontier add: b.
                    runner: dominators at: ri.
                  ].
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> 'parent' -> () From: ( | {
         'Category: calculating the dominator tree\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         calculateDominatorTree = ( |
            | 
            iterateUntilConvergence.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> 'parent' -> () From: ( | {
         'Category: calculating the dominator tree\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         commonParentOf: b1 And: b2 = ( |
             finger1.
             finger2.
             i1.
             i2.
            | 
            "Keep a finger on each branch of the tree, and walk
             up each one until we reach their intersection point."
            finger1: b1.
            finger2: b2.
            [i1: indexOf: finger1.
             i2: indexOf: finger2.
             i1 compare: i2
                 IfLess: [finger2: dominators at: i2]
                  Equal: [^ finger1]
                Greater: [finger1: dominators at: i1]
            ] loop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyStartingAt: n = ( |
            | 
            copy startOfControlFlow: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         go = ( |
            | 
            initialize.
            calculateDominatorTree.
            calculateDominanceFrontiers.
            recordDominanceInformationForEachNode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         indexOf: n = ( |
            | 
            "Maybe we should keep these as data slots in the nodes themselves,
             or bypass the node objects altogether and just make the algorithm
             deal with the indices directly."
            indicesByNode at: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            initializeListOfAllNodes.
            initializeDominatorsArray.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeDominatorsArray = ( |
            | 
            dominators: vector copySize: allNodes size.
            dominators at: 0 Put: allNodes first.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeListOfAllNodes = ( |
            | 
            "Go in reverse postorder. Is that necessary for
             correctness, or just for performance?"

            allNodes: startOfControlFlow basicBlocksInControlFlowReversePostOrder asVector.

            indicesByNode: reflectiveIdentityDictionary copyRemoveAll.
            allNodes do: [|:n. :i| indicesByNode at: n Put: i].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> 'parent' -> () From: ( | {
         'Category: calculating the dominator tree\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         iterateUntilConvergence = ( |
             changed <- bootstrap stub -> 'globals' -> 'true' -> ().
            | 
            [changed] whileTrue: [
              changed: false.
              allNodes doFirst: ["skip the start node"] MiddleLast: [|:b. bi. oldIDom. newIDom|
                bi: indexOf: b.
                [b controlFlowPreds isEmpty not] assert.
                b controlFlowPreds do: [|:p. pi|
                  pi: indexOf: p.
                  (dominators at: pi) ifNotNil: [|:d|
                    newIDom: newIDom ifNil: [p] IfNotNil: [
                      commonParentOf: p And: newIDom.
                    ].
                  ].
                ].
                oldIDom: dominators at: bi.
                oldIDom == newIDom ifFalse: [
                  dominators at: bi Put: newIDom.
                  changed: true.
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> 'parent' -> () From: ( | {
         'Category: recording results\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         recordDominanceInformationForEachNode = ( |
            | 
            [dominators first = allNodes first] assert.
            dominators at: 0 Put: nil.

            dominators doFirst: [
              "Root does not need to be set - in fact, we don't want to overwrite any previous
               dominator info we may have for it (if these are the BBs for an inlined scope)."
            ] MiddleLast: [|:d. :i. n|
              n: allNodes at: i.
              [d isNotNil] assert.
              [n immediateDominator isNil] assert. "Haven't already calculated dominance for this guy."
              n recordImmediateDominator: d.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dominanceCalculator' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         startOfControlFlow.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         irNodes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         locationAssigners = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes locationAssigners.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes locationAssigners abstract.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         allPossibleAvailableRegisterLocations.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         compiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes locationAssigners abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildDictionaryOfPreallocatedValues = ( |
            | 
            machineLevelAllocator allValues do: [|:v|
              v hasLocation ifTrue: [
                (preallocatedValuesByLocation at: v location IfAbsentPut: [colocator copy fixedLocation: v location]) colocatedValues add: v.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         canonicalValueFor: v = ( |
            | 
            v hasLocation ifTrue: [^ preallocatedValuesByLocation at: v location].

            v originalValueBeforeRenaming ifNotNil: [|:orig|
              (valuesWhoseRenamingsNeedToBeColocated includes: orig) ifTrue: [^ orig].
            ].

            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: preallocated values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         colocator = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> 'colocator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes locationAssigners abstract parent colocator.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> 'colocator' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: public'
        
         colocatedValues <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> 'colocator' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         fixedLocation.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> 'colocator' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> 'colocator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes locationAssigners abstract parent colocator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> 'colocator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            resend.copy colocatedValues: colocatedValues copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> 'colocator' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         hasLocation = ( |
            | 
            fixedLocation isNotNil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> 'colocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         location = ( |
            | 
            [hasLocation] assert.
            fixedLocation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> 'colocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            resend.copy
              preallocatedValuesByLocation: preallocatedValuesByLocation copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         displayDebugInformationIfDesired = ( |
            | 
            compiler shouldDisplayDebugInformation ifFalse: [^ self].

            'locationAssigner info for ', compiler machineLevelAllocator topSourceLevelAllocator slot name.
            machineLevelAllocator allValues do: [|:v| printDebugInformationForValue: v].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         getAvailableRegisters = ( |
            | 
            allPossibleAvailableRegisterLocations: machineLevelAllocator availableRegisterLocations asSet.
            usedRegisterLocations: set copyRemoveAll.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            valuesWhoseRenamingsNeedToBeColocated: compiler valuesWhoseRenamingsNeedToBeColocated.
            getAvailableRegisters.
            buildDictionaryOfPreallocatedValues.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         printDebugInformationForValue: v = ( |
            | 
            v printString printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: assigning locations\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         reportOnWhichRegistersWereActuallyAssigned = ( |
            | 
            usedRegisterLocations isEmpty ifFalse: [
              machineLevelAllocator registersThatWereActuallyAssigned: usedRegisterLocations.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         preallocatedValuesByLocation <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         usedRegisterLocations.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         valuesWhoseRenamingsNeedToBeColocated.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         graphColoring = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes locationAssigners abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes locationAssigners graphColoring.

CopyDowns:
globals klein compiler1 parent prototypes locationAssigners abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         aliasesForCoalescedValues <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         coalescedMoveNodes <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         interferingValuesByValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodes <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         moveNodesByRelatedValue <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodesNotYetReadyForCoalescing <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodesThatCannotBeCoalesced <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodesThatMayBeCoalescable <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodesThatWillNotBeCoalesced <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveRelatedValuesThatHaveNotYetBeenCoalesced <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes locationAssigners graphColoring parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         aliasOf: v = ( |
            | 
            aliasesForCoalescedValues
                        if: v
               IsPresentDo: [|:alias| aliasOf: alias]
                IfAbsentDo: [v]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: interference info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         allValuesThatInterfereWith: v = ( |
            | 
            interferingValuesByValue at: v IfAbsentPut: [set copyRemoveAll]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         assignLocations = ( |
            | 
            [valuesThatHaveBeenFactoredOut isEmpty] whileFalse: [| v. badLocs |
              debugMode ifTrue: [checkInvariants].
              v: valuesThatHaveBeenFactoredOut removeLast.
              badLocs: set copyRemoveAll.
              (allValuesThatInterfereWith: v) do: [|:iv| iv hasLocation ifTrue: [badLocs add: iv location]].
              allPossibleAvailableRegisterLocations
                     findFirst: [|:loc| (badLocs includes: loc) not]
                     IfPresent: [|:loc| usedRegisterLocations add: loc. v location: loc]
                      IfAbsent: [       v location: machineLevelAllocator makeAnotherNonVolLocalMemLocation].
              valuesThatAlreadyHaveALocation add: v.
            ].

            aliasesForCoalescedValues do: [|:alias. :v. realAlias|
              realAlias: aliasOf: alias.
              v location: realAlias location.
              valuesThatAlreadyHaveALocation add: v.
            ].

            reportOnWhichRegistersWereActuallyAssigned.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildInterferenceInformation = ( |
            | 
            interferingValuesByValue: dictionary copyRemoveAll.
            compiler nodesInControlFlowOrder do: [|:n| n addInterferenceInformationToValuesWith: self].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         checkInvariants = ( |
            | 
            machineLevelAllocator allValues do: [|:v| [(   valueSetContaining: v) isNotNil] assert].
            moveNodes                       do: [|:m| [(moveNodeSetContaining: m) isNotNil] assert].

            valuesThatCanBeFactoredOut do: [|:v|
              [hasInsignificantDegree: v] assert.
              [hasAlreadyProcessedAllMoveNodesFor: v] assert.
              checkRemainingDegreeOf: v.
            ].

            moveRelatedValuesThatHaveNotYetBeenCoalesced do: [|:v|
              [hasInsignificantDegree: v] assert.
              [(hasAlreadyProcessedAllMoveNodesFor: v) not] assert.
              checkRemainingDegreeOf: v.
            ].

            valuesThatCannotBeFactoredOutYet do: [|:v|
              [hasSignificantDegree: v] assert.
              checkRemainingDegreeOf: v.
            ].

            coalescedMoveNodes do: [|:m|
              checkThatNoConstantsAreBeingAssignedToAsAResultOfCoalescing: m
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         checkRemainingDegreeOf: v = ( |
             d.
             remainingInterferingValues.
             remainingValues.
            | 
            d: remainingDegreeOf: v.
            remainingValues: set copyRemoveAll.
            remainingValues addAll: valuesThatCanBeFactoredOut.
            remainingValues addAll: moveRelatedValuesThatHaveNotYetBeenCoalesced.
            remainingValues addAll: valuesThatCannotBeFactoredOutYet.
            remainingInterferingValues: (allValuesThatInterfereWith: v) intersect: remainingValues.
            [d = remainingInterferingValues size] assert.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         checkThatNoConstantsAreBeingAssignedToAsAResultOfCoalescing: m = ( |
             src.
            | 
            src: aliasOf: m sourceValue.
            src hasLocation ifTrue: [| loc |
              loc: src location.
              loc isNotNil && [loc isConstant] ifTrue: [
                m usedAndDefinedValuesDo: [|:v|
                  (moveNodesByRelatedValue at: v) do: [|:m2|
                    m == m2 ifFalse: [| dst2 |
                      dst2: aliasOf: m2 destinationValue.
                      dst2 hasLocation ifTrue: [| y |
                        y: dst location.
                        y isNotNil && [y isConstant] ifTrue: [| src2 |
                          src2: aliasOf: m2 sourceValue.
                          src2 hsaLocation ifTrue: [| x |
                            x: src2 location.
                            [x isNotNil && [x isConstant && [x oopValue _Eq: y oopValue]]] assert: 'cannot move something into a constant location'.
                          ].
                        ].
                      ].
                    ].
                  ].
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: spilling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         chooseAValueToSpill = ( |
             s.
            | 
            [todo optimization]. "Use a better heuristic for choosing which one to spill."
            s: valuesThatCannotBeFactoredOutYet findFirst: [|:v| v mustBeLocatedInARegister not]
                                                IfPresent: [|:v| v]
                                                 IfAbsent: [valuesThatCannotBeFactoredOutYet first].
            valuesThatCannotBeFactoredOutYet remove: s.

            valuesThatHaveBeenFactoredOut addLast: s.
            remainingValuesThatInterfereWith: s Do: [|:iv| decrementDegreeOf: iv].
            freezeMovesOf: s.
            s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         combine: u With: v ForMove: m = ( |
            | 
            (moveRelatedValuesThatHaveNotYetBeenCoalesced includes: v) ifTrue: [
              moveRelatedValuesThatHaveNotYetBeenCoalesced remove: v.
            ] False: [
              valuesThatCannotBeFactoredOutYet remove: v.
            ].
            setAliasOf: v To: u.
            (moveNodesByRelatedValue at: u) addAll: moveNodesByRelatedValue at: v.
            remainingValuesThatInterfereWith: v Do: [|:t. wasInsignificantBefore|
              wasInsignificantBefore: hasInsignificantDegree: t.
              recordPostCoalescenceInterferenceBetween: t And: u.

              "Gotta decrement t's degree, because t was interfering with v but v is no longer remaining."
              wasInsignificantBefore ifTrue: [
                setRemainingDegreeOf: t To: (remainingDegreeOf: t) pred.
              ] False: [
                decrementDegreeOf: t.
              ].
            ].
            (hasSignificantDegree: u) && [moveRelatedValuesThatHaveNotYetBeenCoalesced includes: u] ifTrue: [
              moveRelatedValuesThatHaveNotYetBeenCoalesced remove: u.
              valuesThatCannotBeFactoredOutYet add: u.
            ].

            combineUsersAndDefinersOf: u And: v ForMove: m.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         combineUsersAndDefinersOf: u And: v ForMove: m = ( |
            | 
            "I think this is kind of a hack - is there
             a more elegant way? --Adam, Apr. 2009"
            u definers    addAll: v definers.    v definers:    u definers.    m ifNotNil: [u definers    remove: m].
            u strongUsers addAll: v strongUsers. v strongUsers: u strongUsers. m ifNotNil: [u strongUsers remove: m].
            u weakUsers   addAll: v weakUsers.   v weakUsers:   u weakUsers.   m ifNotNil: [u weakUsers   remove: m].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            ((((((((((((((resend.copy
              moveNodes: moveNodes copy)
              moveNodesThatMayBeCoalescable: moveNodesThatMayBeCoalescable copy)
              moveNodesThatWillNotBeCoalesced: moveNodesThatWillNotBeCoalesced copy)
              moveNodesNotYetReadyForCoalescing: moveNodesNotYetReadyForCoalescing copy)
              moveRelatedValuesThatHaveNotYetBeenCoalesced: moveRelatedValuesThatHaveNotYetBeenCoalesced copy)
              moveNodesByRelatedValue: moveNodesByRelatedValue copy)
              remainingDegreeByValue: remainingDegreeByValue copy)
              aliasesForCoalescedValues: aliasesForCoalescedValues copy)
              coalescedMoveNodes: coalescedMoveNodes copy)
              moveNodesThatCannotBeCoalesced: moveNodesThatCannotBeCoalesced copy)
              valuesThatHaveBeenFactoredOut: valuesThatHaveBeenFactoredOut copy)
              valuesThatCanBeFactoredOut: valuesThatCanBeFactoredOut copy)
              valuesThatCannotBeFactoredOutYet: valuesThatCannotBeFactoredOutYet copy)
              valuesThatAlreadyHaveALocation: valuesThatAlreadyHaveALocation copy)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForCompiler: c = ( |
            | 
            (copy compiler: c) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         debugMode = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: simplifying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         decrementDegreeOf: v = ( |
             d.
            | 
            d: remainingDegreeOf: v.
            setRemainingDegreeOf: v To: d pred.
            d = numberOfAvailableRegisterLocations ifTrue: [
              valueHasFewEnoughNeighboursNowSoWeCanFactorItOut: v.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         determineAGoodOrderForAssigningLocations = ( |
            | 
            [|:exit|
              case
                if: [valuesThatCanBeFactoredOut                   isEmpty not] Then: [|v| v: factorOutAValue.         debugMode ifTrue: [checkInvariants]]
                If: [moveNodesThatMayBeCoalescable                isEmpty not] Then: [|m| m: tryToCoalesceAMoveNode.  debugMode ifTrue: [checkInvariants]]
                If: [moveRelatedValuesThatHaveNotYetBeenCoalesced isEmpty not] Then: [|v| v: freezeAMoveRelatedValue. debugMode ifTrue: [checkInvariants]]
                If: [valuesThatCannotBeFactoredOutYet             isEmpty not] Then: [|v| v: chooseAValueToSpill.     debugMode ifTrue: [checkInvariants]]
                Else: exit.
            ] loopExit.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: interference info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         does: u InterfereWith: v = ( |
            | 
            (allValuesThatInterfereWith: v) includes: canonicalValueFor: u).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: simplifying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         enableMovesOf: v = ( |
            | 
            moveNodesRelatedTo: v Do: [|:n|
              (moveNodesNotYetReadyForCoalescing includes: n) ifTrue: [
                moveNodesNotYetReadyForCoalescing remove: n.
                moveNodesThatMayBeCoalescable add: n.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: simplifying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         factorOutAValue = ( |
             v.
            | 
            v: valuesThatCanBeFactoredOut removeFirst.
            valuesThatHaveBeenFactoredOut addLast: v.
            remainingValuesThatInterfereWith: v Do: [|:iv| decrementDegreeOf: iv].
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         findMoveNodesAndRelatedValues = ( |
            | 
            compiler nodesInControlFlowOrder do: [|:n|
              n isMove && [n aaa_doNotCoalesce not] ifTrue: [
                moveNodes add: n.
                moveNodesThatMayBeCoalescable add: n.
                n usedAndDefinedValuesDo: [|:v|
                  (moveNodesByRelatedValue at: v IfAbsentPut: [set copyRemoveAll]) add: n.
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: freezing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         freezeAMoveRelatedValue = ( |
             u.
            | 
            u: moveRelatedValuesThatHaveNotYetBeenCoalesced removeFirst.
            valuesThatCanBeFactoredOut add: u.
            freezeMovesOf: u.
            u).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: freezing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         freezeMovesOf: u = ( |
             uAlias.
            | 
            uAlias: aliasOf: u.
            moveNodesRelatedTo: u Do: [|:m. dst. v|
              dst: aliasOf: m destinationValue.
              v:  dst = uAlias ifTrue: [aliasOf: m sourceValue] False: [dst].
              moveNodesNotYetReadyForCoalescing remove: m.
              moveNodesThatWillNotBeCoalesced add: m.
              mightNotBeMoveRelatedAnymore: v.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         go = ( |
            | 
            determineAGoodOrderForAssigningLocations.
            assignLocations.
            displayDebugInformationIfDesired.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         hasAlreadyProcessedAllMoveNodesFor: v = ( |
            | 
            moveNodesRelatedTo: v Do: [^ false].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: degree\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         hasInsignificantDegree: v = ( |
            | 
            (remainingDegreeOf: v) < numberOfAvailableRegisterLocations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: degree\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         hasSignificantDegree: v = ( |
            | 
            (remainingDegreeOf: v) >= numberOfAvailableRegisterLocations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            resend.initialize.
            buildInterferenceInformation.
            shouldTryToCoalesceMoveNodes ifTrue: [findMoveNodesAndRelatedValues].
            putEachValueInTheAppropriateWorkingSet.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: move node relations\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         isMoveRelated: v = ( |
            | 
            moveNodesRelatedTo: v Do: [|:n| ^ true].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: interference info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         isNoLongerInterfering: v = ( |
            | 
            (valuesThatHaveBeenFactoredOut includes: v) || [aliasesForCoalescedValues includesKey: v]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: machine info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         machineLevelAllocator = ( |
            | 
            compiler machineLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         mightNotBeMoveRelatedAnymore: v = ( |
            | 
            v hasLocation not && [(isMoveRelated: v) not && [hasInsignificantDegree: v]] ifTrue: [
              moveRelatedValuesThatHaveNotYetBeenCoalesced remove: v.
              valuesThatCanBeFactoredOut add: v.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         moveNodeSetContaining: m = ( |
             itsInThisOne.
            | 
            moveNodeSetsDo: [|:s|
              (s includes: m) ifTrue: [
                itsInThisOne ifNotNil: [error: 'a move node should never be in two of these sets at once'].
                itsInThisOne: s.
              ].
            ].
            itsInThisOne ifNil: [error: 'a move node must belong to one of the sets']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         moveNodeSetsDo: blk = ( |
            | 
            blk value: coalescedMoveNodes.
            blk value: moveNodesThatMayBeCoalescable.
            blk value: moveNodesThatWillNotBeCoalesced.
            blk value: moveNodesThatCannotBeCoalesced.
            blk value: moveNodesNotYetReadyForCoalescing.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: move node relations\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         moveNodesRelatedTo: v Do: blk = ( |
             ns.
            | 
            ns: moveNodesByRelatedValue at: v IfAbsent: [^ self].
            ns do: [|:n|
              (moveNodesThatMayBeCoalescable includes: n) || [moveNodesNotYetReadyForCoalescing includes: n] ifTrue: [
                blk value: n.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: machine info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         numberOfAvailableRegisterLocations = ( |
            | 
            allPossibleAvailableRegisterLocations size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         printDebugInformationForValue: v = ( |
             r <- '  '.
            | 

            "This doesn't really come out right - way too much info."
            true ifTrue: [^ resend.printDebugInformationForValue: v].

            (v printString, ', interfering values:') printLine.
            (allValuesThatInterfereWith: v) do: [|:iv| r: r & iv shortPrintString] SeparatedBy: [r: r & ', '].
            r flatString printLine.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         putEachValueInTheAppropriateWorkingSet = ( |
            | 
            machineLevelAllocator allValues do: [|:v|
              v hasLocation ifTrue: [
                valuesThatAlreadyHaveALocation add: v.
              ] False: [|d|
                v originalValueBeforeRenaming isNotNil && [valuesWhoseRenamingsNeedToBeColocated includes: v originalValueBeforeRenaming] ifTrue: [|ivs|
                  setAliasOf: v To: v originalValueBeforeRenaming.
                  ivs: allValuesThatInterfereWith: v originalValueBeforeRenaming.
                  ivs addAll: allValuesThatInterfereWith: v.
                  interferingValuesByValue at: v Put: ivs. "Just share the same set."
                  ivs do: [|:iv|
                    (allValuesThatInterfereWith: iv) add: v.
                    (allValuesThatInterfereWith: iv) add: v originalValueBeforeRenaming. "OK, now I'm being paranoid. Clean this up."
                  ].
                ] False: [
                  d: (allValuesThatInterfereWith: v) countHowMany: [|:iv| iv hasLocation not].
                  setRemainingDegreeOf: v To: d.
                  case
                    if: [d >= numberOfAvailableRegisterLocations] Then: [valuesThatCannotBeFactoredOutYet             add: v]
                    If: [moveNodesByRelatedValue includesKey: v]  Then: [moveRelatedValuesThatHaveNotYetBeenCoalesced add: v]
                                                                  Else: [valuesThatCanBeFactoredOut                   add: v].
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: interference info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         recordInterferenceBetween: u And: v = ( |
             uivs.
             vivs.
            | 
            u = v ifTrue: [^ self].
            uivs: allValuesThatInterfereWith: u.
            vivs: allValuesThatInterfereWith: v.
            uivs add: canonicalValueFor: v.
            vivs add: canonicalValueFor: u.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: interference info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         recordPostCoalescenceInterferenceBetween: u And: v = ( |
             uivs.
             vivs.
            | 
            u = v ifTrue: [^ self].
            uivs: allValuesThatInterfereWith: u.
            vivs: allValuesThatInterfereWith: v.
             (uivs includes: v) ifTrue: [[vivs includes: u] assert. ^ self].
            [(vivs includes: u) not] assert.
            uivs add: v.
            vivs add: u.
            (isNoLongerInterfering: v) || [v hasLocation] ifFalse: [setRemainingDegreeOf: u To: (remainingDegreeOf: u) succ].
            (isNoLongerInterfering: u) || [u hasLocation] ifFalse: [setRemainingDegreeOf: v To: (remainingDegreeOf: v) succ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: degree\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         remainingDegreeOf: v = ( |
            | 
            remainingDegreeByValue at: v IfAbsentPut: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: interference info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         remainingValuesThatInterfereWith: v Do: blk = ( |
            | 
            (allValuesThatInterfereWith: v) do: [|:iv|
              (isNoLongerInterfering: iv) ifFalse: [
                blk value: iv.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         setAliasOf: v To: a = ( |
            | 
            v = a ifTrue: [halt].
            aliasesForCoalescedValues at: v Put: a.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: degree\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         setRemainingDegreeOf: v To: d = ( |
            | 
            remainingDegreeByValue at: v Put: d.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldTryToCoalesceMoveNodes = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         tryToCoalesceAMoveNode = ( |
             m.
             u.
             v.
             x.
             y.
            | 
            m: moveNodesThatMayBeCoalescable removeFirst.
            x: aliasOf: m      sourceValue.
            y: aliasOf: m destinationValue.
            y hasLocation ifTrue: [u: y. v: x]
                           False: [u: x. v: y].
            "So if v hasLocation then u does too."

            case
              if: [(u = v) || [v hasLocation && [u location = v location]]] Then: [
                coalescedMoveNodes add: m.
                mightNotBeMoveRelatedAnymore: u.
              ]
              If: [v hasLocation || [(does: u InterfereWith: v) || [u hasLocation && [u location isConstant && [v definers anySatisfy: [|:d| d !== m]]]]]] Then: [
                moveNodesThatCannotBeCoalesced add: m.
                mightNotBeMoveRelatedAnymore: u.
                mightNotBeMoveRelatedAnymore: v.
              ]
              If: [willNotCauseSpillsIfWeCoalesce: u With: v] Then: [
                coalescedMoveNodes add: m.
                combine: u With: v ForMove: m.
                mightNotBeMoveRelatedAnymore: u.
              ] Else: [
                moveNodesNotYetReadyForCoalescing add: m.
              ].

            m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: simplifying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         valueHasFewEnoughNeighboursNowSoWeCanFactorItOut: v = ( |
            | 
            enableMovesOf: v.
            remainingValuesThatInterfereWith: v Do: [|:iv| enableMovesOf: v].

            case
              if: [v hasLocation] Then: [
              ]
              If: [valuesThatCannotBeFactoredOutYet includes: v] Then: [
                valuesThatCannotBeFactoredOutYet remove: v.
                (isMoveRelated: v) ifTrue: [moveRelatedValuesThatHaveNotYetBeenCoalesced add: v]
                                    False: [valuesThatCanBeFactoredOut                   add: v].
              ]
              Else: [
                error: 'huh?'
              ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         valueSetContaining: v = ( |
             itsInThisOne.
            | 
            valueSetsDo: [|:s|
              (s includes: v) ifTrue: [
                itsInThisOne ifNotNil: [error: 'a value should never be in two of these sets at once'].
                itsInThisOne: s.
              ].
            ].
            itsInThisOne ifNil: [error: 'a value must belong to one of the sets']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         valueSetsDo: blk = ( |
            | 
            blk value: valuesThatHaveBeenFactoredOut.
            blk value: valuesThatCanBeFactoredOut.
            blk value: moveRelatedValuesThatHaveNotYetBeenCoalesced.
            blk value: valuesThatCannotBeFactoredOutYet.
            blk value: aliasesForCoalescedValues keys.
            blk value: valuesThatAlreadyHaveALocation.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         willNotCauseSpillsIfWeCoalesce: u With: v = ( |
            | 
            u hasLocation ifTrue: [willNotCauseSpillsIfWeCoalescePreallocated:    u With: v]
                           False: [willNotCauseSpillsIfWeCoalesceNonPreallocated: u With: v]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         willNotCauseSpillsIfWeCoalesceNonPreallocated: u With: v = ( |
             interferingValuesIfCoalesced.
            | 
            interferingValuesIfCoalesced: set copyRemoveAll.
            remainingValuesThatInterfereWith: u Do: [|:iv| interferingValuesIfCoalesced add: iv].
            remainingValuesThatInterfereWith: v Do: [|:iv| interferingValuesIfCoalesced add: iv].

            (interferingValuesIfCoalesced copyFilteredBy: [|:iv| hasSignificantDegree: iv]) size < numberOfAvailableRegisterLocations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         willNotCauseSpillsIfWeCoalescePreallocated: u With: v = ( |
            | 
            remainingValuesThatInterfereWith: v Do: [|:t|
              (hasInsignificantDegree: t) || [t hasLocation || [does: u InterfereWith: t]] ifFalse: [^ false].
            ].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         remainingDegreeByValue <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         valuesThatAlreadyHaveALocation <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         valuesThatCanBeFactoredOut <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         valuesThatCannotBeFactoredOutYet <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'graphColoring' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (orderedSet copyRemoveAll)\x7fVisibility: private'
        
         valuesThatHaveBeenFactoredOut <- orderedSet copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         linearScan = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes locationAssigners abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes locationAssigners linearScan.

CopyDowns:
globals klein compiler1 parent prototypes locationAssigners abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         active.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         availableRegisterLocations.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         colocatedRenamings.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         intervalsByValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         liveIntervals.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         nodesByNumber.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         numbersByNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( |
             {} = 'Comment: Based on http://www.research.ibm.com/jalapeno/papers/toplas99.pdf\x7fModuleInfo: Creator: globals klein compiler1 parent prototypes locationAssigners linearScan parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: assigning locations\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         activeIntervalThatShouldBeSpilledInsteadOf: i = ( |
            | 
            "Heuristic for spilling: if there's another active interval with a
             later endpoint than i's endpoint, spill that one instead of i."

            active reverseDo: [|:j. loc|
              j unfixedLocation ifNotNil: [|:loc|
                ^ j endPoint > i endPoint ifTrue: [j] False: nil.
              ].
            ].

            nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         activeIntervalsInOrderOfIncreasingEndPointDo: blk = ( |
            | 
            "We'll make sure to keep the active list sorted in order by endpoint."
            active do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: assigning locations\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         assignLocations = ( |
            | 
            liveIntervals do: [|:i. v. loc|
              v: valueFor: i.
              loc: i location.
              (colocatedRenamings at: v) do: [|:colocatedValue|
                colocatedValue hasLocation ifTrue: [
                  [colocatedValue location = loc] assert.
                ] False: [
                  colocatedValue location: loc.
                ].
              ].
            ].
            reportOnWhichRegistersWereActuallyAssigned.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         calculateLiveIntervals = ( |
            | 
            "This could be done more efficiently if we do it during calculateValueLiveness, I think."
            intervalsByValue: dictionary copyRemoveAll.
            liveIntervals: list copyRemoveAll.
            machineLevelAllocator allValues do: [|:v. cv|
              cv: canonicalValueFor: v.
              (colocatedRenamings at: cv IfAbsentPut: [set copyRemoveAll]) add: v.
              expand: (intervalForCanonicalValue: cv) IfNecessaryToAccommodate: v.
            ].
            liveIntervals: liveIntervals copySortBy: (| element: e1 Precedes: e2 = (e1 startPoint < e2 startPoint) |).
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         checkThatEverythingLooksOK = ( |
            | 
            machineLevelAllocator allValues do: [|:v|
              v hasLocation ifFalse: [| cv. i |
                cv: canonicalValueFor: v.
                i: intervalForCanonicalValue: cv.
                halt. "Something went wrong."
              ].

              v originalValueBeforeRenaming ifNotNil: [|:orig|
                (valuesWhoseRenamingsNeedToBeColocated includes: orig) ifTrue: [
                  [orig location = v location] assert.
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: assigning locations\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         chooseRegisterForInterval: i IfSucceed: sb IfFail: fb = ( |
            | 
            i fixedLocation ifNotNil: [|:fixedLoc|
              (allPossibleAvailableRegisterLocations includes: fixedLoc) ifTrue: [
                availableRegisterLocations remove: fixedLoc IfAbsent: [
                  [machineLevelAllocator isLeafMethod] assert. "Not really necessary, but I think it's true for now."
                  active do: [|:j|
                    j unfixedLocation = fixedLoc ifTrue: [
                      "Better reassign it... except, crap, how do we know which registers
                       were available then? Maybe we have to spill it? OK, let's just spill
                       it for now; I can try something more clever later."
                      j unfixedLocation: machineLevelAllocator makeAnotherNonVolLocalMemLocation.
                    ].
                  ].
                ].
              ].
              ^ sb value: fixedLoc
            ].

            availableRegisterLocations isEmpty ifTrue: [^ fb value] False: [| loc |
              loc: availableRegisterLocations removeFirst.
              usedRegisterLocations add: loc.
              i unfixedLocation: loc.
              sb value: loc
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForCompiler: c = ( |
            | 
            (copy compiler: c) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: assigning locations\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         doLinearScanToDetermineLocations = ( |
            | 
            active: list copyRemoveAll.
            liveIntervalsInOrderOfIncreasingStartPointDo: [|:i. v|
              expireOldIntervals: i.
              chooseRegisterForInterval: i IfSucceed: [|:loc|
                recordThatIntervalIsNowActive: i.
              ] IfFail: [
                spillAtInterval: i.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         expand: i IfNecessaryToAccommodate: v = ( |
            | 
            [aaaaa]. "Hack - don't count phi functions, since they've been removed. Really we should be removing them from
                      the users and definers sets."
            v definersDo: [|:n. sp| n isPhiFunction ifFalse: [sp: numberForNode: n. sp < i startPoint ifTrue: [i startPoint: sp]]].
            v    usersDo: [|:n. ep| n isPhiFunction ifFalse: [ep: numberForNode: n. ep > i   endPoint ifTrue: [i   endPoint: ep]]].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: assigning locations\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         expireOldIntervals: i = ( |
            | 
            activeIntervalsInOrderOfIncreasingEndPointDo: [|:j. loc|
              j endPoint >= i startPoint ifTrue: [^ self].
              active remove: j.
              loc: j location.
              (allPossibleAvailableRegisterLocations includes: loc) ifTrue: [
                availableRegisterLocations add: loc.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         getAvailableRegisters = ( |
            | 
            resend.getAvailableRegisters.
            availableRegisterLocations: allPossibleAvailableRegisterLocations asList.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         go = ( |
            | 
            doLinearScanToDetermineLocations.
            assignLocations.
            [aaaaaa]. checkThatEverythingLooksOK.
            displayDebugInformationIfDesired.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            resend.initialize.
            colocatedRenamings: dictionary copyRemoveAll.
            numberAllIRNodes.
            calculateLiveIntervals.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: intervals\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         intervalForCanonicalValue: cv = ( |
            | 
            intervalsByValue at: cv IfAbsentPut: [| i |
              i: liveInterval copyForValue: cv Start: maxSmallInt End: -1.
              liveIntervals add: i.
              i
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: prototypes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         liveInterval = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> 'liveInterval' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes locationAssigners linearScan parent liveInterval.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> 'liveInterval' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         endPoint.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> 'liveInterval' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> 'liveInterval' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes locationAssigners linearScan parent liveInterval parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> 'liveInterval' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForValue: v Start: s End: e = ( |
            | 
            ((copy val: v) startPoint: s) endPoint: e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> 'liveInterval' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         fixedLocation = ( |
            | 
            val hasLocation ifTrue: [val location]
                             False: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> 'liveInterval' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         location = ( |
            | 
            unfixedLocation ifNil: [fixedLocation]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> 'liveInterval' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> 'liveInterval' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         startPoint.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> 'liveInterval' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         unfixedLocation.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> 'liveInterval' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         val.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         liveIntervalsInOrderOfIncreasingStartPointDo: blk = ( |
            | 
            "We'll make sure to keep liveIntervals sorted in order by startpoint."
            liveIntervals do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: machine info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         machineLevelAllocator = ( |
            | 
            compiler machineLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         numberAllIRNodes = ( |
             i <- 0.
             ns.
            | 
            numbersByNode: dictionary copyRemoveAll.
            ns: list copyRemoveAll.
            compiler basicBlocksInControlFlowReversePostOrder do: [|:bb|
              bb nodesDo: [|:n|
                numbersByNode at: n Put: i.
                ns addLast: n.
                i: i succ.
              ].
            ].
            nodesByNumber: ns asVector.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: node numbers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         numberForNode: n = ( |
            | 
            numbersByNode at: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: machine info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         numberOfAvailableRegisterLocations = ( |
            | 
            availableRegisterLocations size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: assigning locations\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         recordThatIntervalIsNowActive: i = ( |
            | 
            active insert: i BeforeElementSatisfying: [|:i2| i2 endPoint >= i endPoint] IfAbsent: [active addLast: i].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: assigning locations\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         spillAtInterval: i = ( |
             newMemLoc.
            | 
            newMemLoc: machineLevelAllocator makeAnotherNonVolLocalMemLocation.

            (activeIntervalThatShouldBeSpilledInsteadOf: i) ifNil: [
              i unfixedLocation: newMemLoc.
            ] IfNotNil: [|:j|
              j unfixedLocation ifNil: [error: 'cannot change a fixed location'] IfNotNil: [|:loc|
                i unfixedLocation: loc.
                j unfixedLocation: newMemLoc.
              ].
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigners' -> 'linearScan' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: intervals\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         valueFor: i = ( |
            | 
            i val).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         optimizationPolicies = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes optimizationPolicies.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes optimizationPolicies abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         maxMethodSizeForInlining = 15.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldDoTypePrediction = ( |
            | 
            shouldNeverDoInlining not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         compileFastCode = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileFastCode' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes optimizationPolicies compileFastCode.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileFastCode' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileFastCode' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldInlineSlot: s For: rcvrValue Into: context Key: key = ( |
            | 
            s isMethod ifFalse: [^ true ]. "Data slots are fine."
            s contents codes size <= maxMethodSizeForInlining ifTrue: [^ true]. "For now just use bytecode count."
            ('if:Then:' isPrefixOf: s name) ifTrue: [^ true]. [aaaaaaa]. "Just a special-case hack for now."
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileFastCode' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldNeverDoInlining = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileFastCode' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldUseSSAForm = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         compileQuickly = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileQuickly' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes optimizationPolicies compileQuickly.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileQuickly' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileQuickly' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldNeverDoInlining = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileQuickly' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldUseSSAForm = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: maps\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeOfHolderOfSlot: s = ( |
            | s holder findReflecteeUsingOracle: objectsOracle).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: compiling other slots\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         rememberResend: bc FromContext: c TopmostSelfMap: m = ( |
            | 
            (resendsByContextAndSelfMap at: (c & m) asVector IfAbsentPut: [list copyRemoveAll]) add: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: static single-assignment form\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         replacePhiFunctionsWithMoves = ( |
            | 
            optimizationPolicy shouldUseSSAForm ifFalse: [^ self].
            firstBB basicBlocksInControlFlowReversePostOrder do: [|:bb|
              [bb immediateDominator isNil = (bb = firstBB)] assert.
              bb phiFunctionsVanishAndDo: [|:phi|
                phi destinationValue hasUsers ifTrue: [
                  phi sourceValues do: [|:sourceValue. :predBB|
                    predBB appendMoveNode: irNodeGenerator newMoveNodeFrom: sourceValue To: phi destinationValue BC: predBB endNode bc.
                  ].
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: nmethod reusability\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         reusableIfSlotIsTheSame = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'reusableIfSlotIsTheSame' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent reusableIfSlotIsTheSame.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'reusableIfSlotIsTheSame' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         lookupKey.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'reusableIfSlotIsTheSame' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'reusableIfSlotIsTheSame' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent reusableIfSlotIsTheSame parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'reusableIfSlotIsTheSame' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForKey: k Slot: s = ( |
            | 
            (copy lookupKey: k) slot: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'reusableIfSlotIsTheSame' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isSatisfiedBy: cr = ( |
             s.
             ss.
            | 
            ss: lookupKey lookupSlotsUsing: cr protoSlotFinder Self: cr selfMap Holder: cr outermostMethodHolder IfAssignableParentSlot: [^ false].
            s: ss ifNone: [^ false] IfOne: [|:s| s] IfMany: [^ false].
            (s name = slot name)
            && [   (s    holder findReflecteeUsingOracle: cr objectMapper objectsOracle)
              _Eq: (slot holder findReflecteeUsingOracle: cr objectMapper objectsOracle)]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'reusableIfSlotIsTheSame' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'reusableIfSlotIsTheSame' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         slot.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         selector = ( |
            | slot name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldAttemptLeafMethodOptimization = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldDisplayDebugInformation = ( |
            | 
            "(selector = 'at:') && [context selfMirror = traits mirrors abstractMirror asMirror]"
            "selector = 'whileTrue:'"
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: profiling the compiler\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldProfileCompilingThisSlot = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         slot = ( |
            | 
            context slot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         topSourceLevelAllocator = ( |
            | 
            machineLevelAllocator topSourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         tryToInlineSomething = ( |
            | 
            nodesSatisfying: [|:n| n isSend] Do: [|:n|
              (irNodeGenerator maybeInlineSend: n) ifTrue: [^ true].
            ].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: static single-assignment form\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         valuesWhoseRenamingsNeedToBeColocated = ( |
             vs.
            | 
            vs: set copyRemoveAll.
            irNodeGenerator topBytecodeInterpreter allBytecodeInterpretersDo: [|:i. sla|
              sla: i sourceLevelAllocator.
              sla slot isMethod ifTrue: [
                sla method allSlotsOnThisMethod do: [|:s|
                  s isKleinSlotOffsetRecorded ifTrue: [
                    vs add: sla valueForSlot: s.
                  ].
                ].
              ].
            ].
            vs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation helpers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: public'
        
         relocators <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compiling other slots\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (reflectiveIdentityDictionary copyRemoveAll)'
        
         resendsByContextAndSelfMap <- reflectiveIdentityDictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compiling other slots\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         reusabilityConditions <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         shouldGatherStatistics <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: special complation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         shouldSaveAllNonVolatileRegisters <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         shouldZapDeadLocations <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: special complation modes\x7fComment: A _VariableArguments method may have argument slots bound to
the first few (required) parameters.  There is currently no
provision for accessing the variadic parameters because they
are unnamed and there are certain complications involving
determining the number of variadic parameters actually supplied.
-- jb 8/03\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         variableArguments <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         valueSlotContentsFor: bm With: i = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         valueSlotNameFor: bm With: i = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         valueSlotContentsFor: bm With: i = ( |
            | 
            i localValueSlotContentsFor: bm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         valueSlotNameFor: bm With: i = ( |
            | 
            i localValueSlotNameFor: bm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         valueSlotContentsFor: bm With: i = ( |
            | 
            i remoteValueSlotContentsFor: bm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         valueSlotNameFor: bm With: i = ( |
            | 
            i remoteValueSlotNameFor: bm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot'
        
         kleinCompiler1 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler1' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinCompiler1.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler1' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler1' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler1' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler1' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
            resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler1' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.67 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler1' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'kleinC1_IRNodes
kleinC1_Values
kleinC1_Allocs
kleinC1_Gens
kleinC1_BCI
kleinC1_locs
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: exporting\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         findVMKitMapUsingOracle: oracle = ( |
            | 
            oracle mapForOriginalMirror: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'slots' -> 'plain' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isKleinSlotOffsetRecorded = ( |
            | 
            "Duplication with" [klein nmethod isSPOffsetRecordedForSlotOfType: 0].
            isAssignable || [isArgument]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'slots' -> 'plain' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinCompilerPrototypeForMe = ( |
            | 
            klein compiler1).
        } | ) 



 '-- Sub parts'

 bootstrap read: 'kleinC1_IRNodes' From: 'applications/klein'
 bootstrap read: 'kleinC1_Values' From: 'applications/klein'
 bootstrap read: 'kleinC1_Allocs' From: 'applications/klein'
 bootstrap read: 'kleinC1_Gens' From: 'applications/klein'
 bootstrap read: 'kleinC1_BCI' From: 'applications/klein'
 bootstrap read: 'kleinC1_locs' From: 'applications/klein'



 '-- Side effects'

 globals modules kleinCompiler1 postFileIn
