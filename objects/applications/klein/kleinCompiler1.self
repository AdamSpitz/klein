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
         'Category: bytecode method information\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: public'
        
         blockLiteralsThatWillNotBePushed <- list copyRemoveAll.
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
         'Category: IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)'
        
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
         'Category: IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)'
        
         myReturnNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)'
        
         myStartNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compiling methods\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         nlrPoints <- list copyRemoveAll.
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)'
        
         optimizationPolicy.
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
        
         oracleForEagerRelocation <- bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> ().
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
            | 
            (prototypes locationAssigner copyForCompiler: self) go).
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
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildAndLinkValues = ( |
            | 
            allocateIncomingAndPreallocatedLocations.
            recordDefinersAndUsers.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: building nmethods\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         buildNMethod = ( |
             nm.
            | 
            nm: vmKit nmethod copySize: codeGenerator generatedCodeSize.
            codeGenerator copyGeneratedCodeInto: nm.
            nmethodRelocators do: [|:rl| rl beForObject: nm].
            nm reusabilityConditions: reusabilityConditions.
            nm relocators: relocators.
            nm frame: frame copy.
            nm topScope: startNode bc interpreter createScopeDescForNMethod: nm.
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

            [aaaaa]. "Using reverse source order, but for maximum efficiency this should be
                      reverse control-flow order. In practice I doubt it'll matter much.
                      The problem with reverse control-flow order is that we don't have a
                      list of all the possible end nodes (do we?). -- Adam, Mar. 2009"

            ns: nodesInReverseSourceOrder.

            ns do: [|:n| n initializeLivenessInformation].

            [| changed <- false |
             ns do: [|:n| n updateLivenessInformation ifTrue: [changed: true]].
             changed] whileTrue.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         checkThatItMakesSenseToCompileSlot: s ForSelf: selfMir AndReceiver: rcvrMir = ( |
            | 
            " There is one case in which self and rcvr are equal but
              the method is a block method:
              The receiver is a block which inherits a method, m, and also
              occurs in that same method m.
              For example: consider compiling the exitValue method as an outer method
              for the block in that method itself. -- Adam Spitz & David Ungar, 6/05"
            [
              selfMir = rcvrMir
                ifFalse: [ s contents isReflecteeBlockMethod ]
                   True: [ s contents isReflecteeBlockMethod not || [rcvrMir isReflecteeBlock] ]
            ] assert: 'self is only not rcvr for block methods'.

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
            | 

            "ForceNonLeaf is false at first, if the compiler
             decides to go for leaf method optimization but it fails,
             the backout block will let it abort and retry forcing non-leafyness
              -- dmu 8/03"

            compiler: copy.
            compiler doCompileForceNonLeaf: false IfBackout: [|:e|
              compiler: copy.
              compiler doCompileForceNonLeaf: true IfBackout: [|:e| ^ error: 'should not have backed out:', e].
              ^ compiler "necessary because we need to back out"
            ].
            compiler).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            ((((((((resend.copy
                                     nlrPoints:                        nlrPoints copy)
                                    relocators:                       relocators copy)
                             nmethodRelocators:                nmethodRelocators copy)
                         reusabilityConditions:            reusabilityConditions copy)
              blockLiteralsThatWillNotBePushed: blockLiteralsThatWillNotBePushed copy)
                               irNodeGenerator: nil)    "will be recreated"
                                   myStartNode: nil)    "will be recreated"
                                  myReturnNode: nil)    "will be recreated").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForContext: cntxt Architecture: arch Oracle: oracle Debug: d Optimize: op = ( |
             c.
            | 
            checkThatItMakesSenseToCompileSlot: cntxt slot ForSelf: cntxt selfMirror AndReceiver: cntxt rcvrMirror.

            c: cntxt slot kleinCompilerPrototypeForMe copy.
            c architecture:                  arch.
            c context:                       cntxt.
            c debug:                         d.
            c relocators:                    list copyRemoveAll.
            c nmethodRelocators:             list copyRemoveAll.
            c oracleForEagerRelocation:      oracle.
            c optimizationPolicy:            op.

            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         debug: bool = ( |
            | 
            areCommentsEmitted: bool).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: nmethod reusability\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         dependsOnKey: k ResolvingToSlot: s = ( |
            | 
            [aaaaa]. "This could probably be factored a bit better."

            "I'm not completely sure about this, but I think that we don't need to worry about resends
             making the nmethod unreusable, since the resend lookup starts from the method holder rather
             than from self. -- Adam, Apr. 2009"
            k isResend && [irNodeGenerator sourceLevelAllocator slot holder != irNodeGenerator sourceLevelAllocator context selfMirror] ifFalse: [
              reusabilityConditions add: reusableIfSlotIsTheSame copyForKey: k Slot: s.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         doCompile = ( |
            | 
            makeSureIRNodesAreBuilt.
            buildAndLinkValues.
            doInlining.
            calculateValueLiveness.
            allocateLocations.
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
         'Category: optimizing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         doInlining = ( |
            | 
            optimizationPolicy shouldNeverDoInlining ifTrue: [^ self].
            sendNodesDo: [|:n. key|
              key: klein lookupKey copyForSendBC: n bc.
              irNodeGenerator currentBytecodeInterpreter: n bc interpreter.
              irNodeGenerator ifShouldInlineSend: key To: n rcvrAndArgStackValues first Then: [|:s. :rcvrMir|
                n removeFromGraphInPreparationForInlining.
                irNodeGenerator inlineSlot: s Key: key ReceiverType: rcvrMir RcvrAndArgs: n rcvrAndArgStackValues Result: n resultStackValue.
                [n lastNodeBeforeSettingUpTheSend controlFlowSuccs isEmpty not] assert.
              ] Else: [].
            ].
            self).
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
             previousNode.
            | 
            startNode controlFlowOrderDo: [|:node|
              "Need to make sure that when we fall through, we actually fall through
               to the right place. -- Adam, Mar. 2009"
              previousNode ifNotNil: [
                previousNode controlFlowSuccWhenFallingThrough ifNotNil: [|:n|
                  n = node ifFalse: [
                    codeGenerator genBranchTo: n.
                  ].
                ].
              ].
              node generateCode.
              previousNode: node.
            ].
            previousNode controlFlowSuccWhenFallingThrough ifNotNil: [|:n|
              codeGenerator genBranchTo: n.
            ].

            "The assumption that the localReturn/nlr node is the last to 
             show in control flow order is false. Must bind labels in here
             then. -- Ausch, 5/05"
            codeGenerator generating: 'generateNLRPoints' During: [codeGenerator generateNLRPoints].
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

            codeGenerator: protoCodeGenForMyPlatform copyForCompiler: self.
            self).
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

              irNodeGenerator currentBytecodeInterpreter prologue.
              myStartNode:  irNodeGenerator firstNode.

              irNodeGenerator currentBytecodeInterpreter interpretSlot.

              irNodeGenerator lastNode isNonlocalReturn ifFalse: [irNodeGenerator currentBytecodeInterpreter epilogue].
              myReturnNode: irNodeGenerator lastNode.

              irNodeGenerator currentBytecodeInterpreter fillInMissingIRNodesByBCI.
              irNodeGenerator interpreterFinished.
            ].
            self).
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
         'Category: accessing\x7fCategory: nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         nodesInControlFlowOrder = ( |
            | 
            startNode nodesInControlFlowOrder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         nodesInReverseSourceOrder = ( |
             r.
            | 
            "This is here just because it makes profiles cleaner - otherwise
             we could just call localReturnNode reverseSourceNodesDo: directly.
             -- Adam, 8/05"
            r: list copyRemoveAll.
            returnNode reverseSourceNodesDo: [|:n| r add: n].
            r).
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
        
         compilationContext = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes compilationContext.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         lexicalParentScopes.
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
            copyForSlot: s Self: s holder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSlot: s Key: k Self: sMir Receiver: rMir LexicalParentScopes: lpss = ( |
            | 
            ((((copy slot: s) lookupKey: k) selfMirror: sMir) rcvrMirror: rMir) lexicalParentScopes: lpss asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSlot: s Self: sMir = ( |
            | 
            copyForSlot: s Self: sMir LexicalParentScopes: vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSlot: s Self: sMir LexicalParentScopes: lpss = ( |
            | 
            copyForSlot: s Key: (klein lookupKey copyForNormalSend: s name) Self: sMir Receiver: s holder LexicalParentScopes: lpss).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isOuterMethodForABlock = ( |
            | 
            rcvrMirror isReflecteeBlock && [rcvrMirror = selfMirror]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         lexicalParentCount = ( |
            | 
            lexicalParentScopes size).
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
            lexicalParentScopes isEmpty ifTrue: [^ slot holder].
            reflect: outermostScope methodHolder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         outermostScope = ( |
            | lexicalParentScopes first).
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         rcvrMirror.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         selfMirror.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'compilationContext' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         slot.
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
        
         locationAssigner = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes locationAssigner.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         aliasesForCoalescedValues <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         availableRegisterLocations.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         coalescedMoveNodes <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         compiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         interferingValuesByValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodes <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         moveNodesByRelatedValue <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodesNotYetReadyForCoalescing <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodesThatCannotBeCoalesced <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodesThatMayBeCoalescable <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodesThatWillNotBeCoalesced <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveRelatedValuesThatHaveNotYetBeenCoalesced <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes locationAssigner parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         aliasOf: v = ( |
            | 
            aliasesForCoalescedValues
                        if: v
               IsPresentDo: [|:alias| aliasOf: alias]
                IfAbsentDo: [v]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: interference info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         allValuesThatInterfereWith: v = ( |
            | 
            interferingValuesByValue at: v IfAbsentPut: [set copyRemoveAll]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         assignLocations = ( |
             highestIndexUsed <- -1.
            | 
            [valuesThatHaveBeenFactoredOut isEmpty] whileFalse: [| v. badLocs |
              debugMode ifTrue: [checkInvariants].
              v: valuesThatHaveBeenFactoredOut removeLast.
              badLocs: set copyRemoveAll.
              (allValuesThatInterfereWith: v) do: [|:iv| iv hasLocation ifTrue: [badLocs add: iv location]].
              availableRegisterLocations findFirst: [|:loc    | (badLocs includes: loc) not]
                                         IfPresent: [|:loc. :i| highestIndexUsed: highestIndexUsed max: i. v location: loc]
                                          IfAbsent: [           v location: machineLevelAllocator makeAnotherNonVolLocalMemLocation].
              valuesThatAlreadyHaveALocation add: v.
            ].

            aliasesForCoalescedValues do: [|:alias. :v. realAlias|
              realAlias: aliasOf: alias.
              v location: realAlias location.
              valuesThatAlreadyHaveALocation add: v.
            ].

            highestIndexUsed >= 0 ifTrue: [
              machineLevelAllocator lastAvailableRegisterThatWasActuallyAssigned: (availableRegisterLocations at: highestIndexUsed) register.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildDictionaryOfPreallocatedValues = ( |
            | 
            machineLevelAllocator allValues do: [|:v|
              v hasLocation ifTrue: [
                (preallocatedValuesByLocation at: v location IfAbsentPut: [set copyRemoveAll]) add: v.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildInterferenceInformation = ( |
            | 
            interferingValuesByValue: dictionary copyRemoveAll.
            compiler startNode controlFlowOrderDo: [|:n| n addInterferenceInformationToValuesWith: self].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: spilling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         chooseAValueToSpill = ( |
             v.
            | 
            [todo optimization]. "Use a better heuristic for choosing which one to spill."
            v: valuesThatCannotBeFactoredOutYet removeFirst.
            valuesThatHaveBeenFactoredOut addLast: v.
            remainingValuesThatInterfereWith: v Do: [|:iv| decrementDegreeOf: iv].
            freezeMovesOf: v.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         combine: u With: v ForMove: m = ( |
            | 
            (moveRelatedValuesThatHaveNotYetBeenCoalesced includes: v) ifTrue: [
              moveRelatedValuesThatHaveNotYetBeenCoalesced remove: v.
            ] False: [
              valuesThatCannotBeFactoredOutYet remove: v.
            ].
            aliasesForCoalescedValues at: v Put: u.
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

            "Combine their definers and users. I think this is kind of a hack - is there
             a more elegant way? --Adam, Apr. 2009"
            u definers addAll: v definers. u definers remove: m. v definers: u definers.
            u users    addAll: v users.    u users    remove: m. v users:    u users.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            (((((((((((((((resend.copy
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
              valuesThatAlreadyHaveALocation: valuesThatAlreadyHaveALocation copy)
              preallocatedValuesByLocation: preallocatedValuesByLocation copy)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForCompiler: c = ( |
            | 
            (copy compiler: c) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         debugMode = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: interference info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         does: u InterfereWith: v = ( |
            | 
            (allValuesThatInterfereWith: v) includes: u).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: simplifying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         factorOutAValue = ( |
             v.
            | 
            v: valuesThatCanBeFactoredOut removeFirst.
            valuesThatHaveBeenFactoredOut addLast: v.
            remainingValuesThatInterfereWith: v Do: [|:iv| decrementDegreeOf: iv].
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         findMoveNodesAndRelatedValues = ( |
            | 
            compiler startNode sourceNodesDo: [|:n|
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: freezing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         freezeAMoveRelatedValue = ( |
             u.
            | 
            u: moveRelatedValuesThatHaveNotYetBeenCoalesced removeFirst.
            valuesThatCanBeFactoredOut add: u.
            freezeMovesOf: u.
            u).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         go = ( |
            | 
            determineAGoodOrderForAssigningLocations.
            assignLocations.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         hasAlreadyProcessedAllMoveNodesFor: v = ( |
            | 
            moveNodesRelatedTo: v Do: [^ false].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: degree\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         hasInsignificantDegree: v = ( |
            | 
            (remainingDegreeOf: v) < numberOfAvailableRegisterLocations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: degree\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         hasSignificantDegree: v = ( |
            | 
            (remainingDegreeOf: v) >= numberOfAvailableRegisterLocations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            buildDictionaryOfPreallocatedValues.
            buildInterferenceInformation.
            availableRegisterLocations: machineLevelAllocator availableRegisterLocations.
            shouldTryToCoalesceMoveNodes ifTrue: [findMoveNodesAndRelatedValues].
            putEachValueInTheAppropriateWorkingSet.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: move node relations\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         isMoveRelated: v = ( |
            | 
            moveNodesRelatedTo: v Do: [|:n| ^ true].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: interference info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         isNoLongerInterfering: v = ( |
            | 
            (valuesThatHaveBeenFactoredOut includes: v) || [aliasesForCoalescedValues includesKey: v]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: machine info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         machineLevelAllocator = ( |
            | 
            compiler machineLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         mightNotBeMoveRelatedAnymore: v = ( |
            | 
            v hasLocation not && [(isMoveRelated: v) not && [hasInsignificantDegree: v]] ifTrue: [
              moveRelatedValuesThatHaveNotYetBeenCoalesced remove: v.
              valuesThatCanBeFactoredOut add: v.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: machine info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         numberOfAvailableRegisterLocations = ( |
            | 
            availableRegisterLocations size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         putEachValueInTheAppropriateWorkingSet = ( |
            | 
            machineLevelAllocator allValues do: [|:v. s|
              s: v hasLocation ifTrue: [
                valuesThatAlreadyHaveALocation.
              ] False: [|d|
                d: (allValuesThatInterfereWith: v) countHowMany: [|:iv| iv hasLocation not].
                setRemainingDegreeOf: v To: d.
                case
                  if: [d >= numberOfAvailableRegisterLocations] Then: [valuesThatCannotBeFactoredOutYet            ]
                  If: [moveNodesByRelatedValue includesKey: v]  Then: [moveRelatedValuesThatHaveNotYetBeenCoalesced]
                                                                Else: [valuesThatCanBeFactoredOut                  ].
              ].
              s add: v.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: interference info\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         recordInterferenceBetween: u And: v = ( |
             uivs.
             vivs.
            | 
            u = v ifTrue: [^ self].
            uivs: allValuesThatInterfereWith: u.
            vivs: allValuesThatInterfereWith: v.
            uivs add: v.
            vivs add: u.

            "There can be many values with the same preallocated location. If one of them
             interferes with v, they all do."
            u hasLocation ifTrue: [vivs addAll: preallocatedValuesByLocation at: u location].
            v hasLocation ifTrue: [uivs addAll: preallocatedValuesByLocation at: v location].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: degree\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         remainingDegreeOf: v = ( |
            | 
            remainingDegreeByValue at: v IfAbsentPut: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: degree\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         setRemainingDegreeOf: v To: d = ( |
            | 
            remainingDegreeByValue at: v Put: d.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldTryToCoalesceMoveNodes = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         willNotCauseSpillsIfWeCoalesce: u With: v = ( |
            | 
            u hasLocation ifTrue: [willNotCauseSpillsIfWeCoalescePreallocated:    u With: v]
                           False: [willNotCauseSpillsIfWeCoalesceNonPreallocated: u With: v]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         willNotCauseSpillsIfWeCoalesceNonPreallocated: u With: v = ( |
             interferingValuesIfCoalesced.
            | 
            interferingValuesIfCoalesced: set copyRemoveAll.
            remainingValuesThatInterfereWith: u Do: [|:iv| interferingValuesIfCoalesced add: iv].
            remainingValuesThatInterfereWith: v Do: [|:iv| interferingValuesIfCoalesced add: iv].

            (interferingValuesIfCoalesced copyFilteredBy: [|:iv| hasSignificantDegree: iv]) size < numberOfAvailableRegisterLocations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         willNotCauseSpillsIfWeCoalescePreallocated: u With: v = ( |
            | 
            remainingValuesThatInterfereWith: v Do: [|:t|
              (hasInsignificantDegree: t) || [t hasLocation || [does: u InterfereWith: t]] ifFalse: [^ false].
            ].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         preallocatedValuesByLocation <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         remainingDegreeByValue <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         valuesThatAlreadyHaveALocation <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         valuesThatCanBeFactoredOut <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         valuesThatCannotBeFactoredOutYet <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (orderedSet copyRemoveAll)\x7fVisibility: private'
        
         valuesThatHaveBeenFactoredOut <- orderedSet copyRemoveAll.
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
        
         maxMethodSizeForInlining = 9.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
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
        
         shouldInlineSlot: s For: rcvrValue Key: key = ( |
            | 
            s isMethod                                         ifFalse: [^ true ]. "Data slots are fine."
            s contents codes size <= maxMethodSizeForInlining  ifFalse: [^ false]. "Just based on number of bytecodes for now."
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileFastCode' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldNeverDoInlining = bootstrap stub -> 'globals' -> 'false' -> ().
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         compileQuicklyButOptimizeKleinKernel = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileQuicklyButOptimizeKleinKernel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes optimizationPolicies compileQuicklyButOptimizeKleinKernel.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileQuicklyButOptimizeKleinKernel' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         hardCodedSlotNamesToInlineDo: b = ( |
            | 
            b value: 'ifTrue:'.
            b value: 'ifFalse:'.
            b value: 'ifTrue:False:'.
            b value: 'ifFalse:True:'.
            b value: '+'.
            b value: '-'.
            b value: '*'.
            b value: '/'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileQuicklyButOptimizeKleinKernel' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileQuicklyButOptimizeKleinKernel' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldInlineSlot: s For: rcvrValue Key: key = ( |
            | 
            s isMethod                                         ifFalse: [^ true ]. "Data slots are fine."
            s contents codes size <= maxMethodSizeForInlining  ifFalse: [^ false]. "Just based on number of bytecodes for now."

            hardCodedSlotNamesToInlineDo: [|:n| s name = n ifTrue: [^ true]].

            "Otherwise, don't bother inlining; more important to keep compile times down."
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'optimizationPolicies' -> 'compileQuicklyButOptimizeKleinKernel' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldNeverDoInlining = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         recordDefinersAndUsers = ( |
            | 
            startNode sourceNodesDo: [|:n| n recordDefinersAndUsers].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         returnNode = ( |
            | 
            makeSureIRNodesAreBuilt.
            myReturnNode).
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
            ss: lookupKey lookupSlotsUsing: cr protoSlotFinder Self: cr selfMirror Holder: cr outermostMethodHolder.
            s: ss ifNone: nil IfOne: [|:s| s] IfMany: nil.
            s = slot).
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
         'Category: optimizing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         sendNodesDo: blk = ( |
            | 
            "Could optimize this by keeping track of them as we generate them."
            startNode sourceNodesDo: [|:n|
              n isSend ifTrue: [
                blk value: n.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         slot = ( |
            | 
            context slot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         startNode = ( |
            | 
            makeSureIRNodesAreBuilt.
            myStartNode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         topSourceLevelAllocator = ( |
            | 
            machineLevelAllocator topSourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         willNotPushBlockMirror: bMir = ( |
            | 
            blockLiteralsThatWillNotBePushed add: bMir.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation helpers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: public'
        
         relocators <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation helpers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         reusabilityConditions <- list copyRemoveAll.
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
            | resend.postFileIn).
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
