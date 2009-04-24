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
         'Category: IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)'
        
         bytecodeInterpreter.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation helpers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         codeGenerator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: special complation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         hasOnNonLocalReturn <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)'
        
         irNodesByBCI.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         isLogUsed <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: bytecode method information\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         lexicalParentCompiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: bytecode method information\x7fCategory: resend information\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         lookupType.
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
         'Category: bytecode method information\x7fCategory: resend information\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         objectDoingTheResend.
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
            sourceLevelAllocator allocateIncomingAndPreallocatedLocations.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateRemainingLocations = ( |
            | 
            (locationAssigner copyForCompiler: self) go).
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
         'Category: building IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildAndLinkIRNodes = ( |
            | 
            makeSureIRNodesAreBuilt.
            resolveUnresolvedMoveNodes.
            forgeControlFlowLinks.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildAndLinkValues = ( |
            | 
            allocateIncomingAndPreallocatedLocations.
            recordDefinersAndUsers.
            calculateValueLiveness.
            buildInterferenceInformation.
            allocateRemainingLocations.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildBodyIRNodes = ( |
            | 
            case
              if: [slot isMethod] Then: [bytecodeInterpreter interpretMethod]
                                  Else: [bytecodeInterpreter dataSlot: slot].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildInterferenceInformation = ( |
            | 
            startNode controlFlowOrderDo: [|:n| n addInterferenceInformationToValues].
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
            nm method: method.
            nm methodHolder: slotHolderMirror reflectee.
            nm lookupKey initializeForSelector: selector
                                    LookupType: lookupType
                          ObjectDoingTheResend: objectDoingTheResend
                                    SlotHolder: [slotHolderMirror reflectee].
            nm frame: frame copy.
            nm incomingRcvrSPOffset: machineLevelAllocator topSourceLevelAllocator locationForIncomingReceiver spOffsetForNMethod: codeGenerator.
            nm slotSPOffsets:  findSlotSPOffsets.
            nm pcOffsetsByBCI: pcOffsetsByBCI.
            nm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         calculateValueLiveness = ( |
             ns.
            | 

            [aaa]. "Using reverse source order, but for maximum efficiency this should be
                    reverse control-flow order. In practice I doubt it'll matter much.
                    The problem with reverse control-flow order is that we don't have a
                    list of all the NLR nodes (do we?). -- Adam, Mar. 2009"

            ns: nodesInReverseSourceOrder.

            ns do: [|:n| n initializeLivenessInformation].

            [| changed <- false |
             ns do: [|:n| n updateLivenessInformation ifTrue: [changed: true]].
             changed] whileTrue.

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
            compiler initializeForceNonLeaf: false.
            compiler doCompileIfBackout: [|:e|
              compiler: copy.
              compiler initializeForceNonLeaf: true.
              compiler doCompileIfBackout: [|:e| ^ error: 'should not have backed out:', e].
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
                           bytecodeInterpreter: nil)    "will be recreated"
                                   myStartNode: nil)    "will be recreated"
                                  myReturnNode: nil)    "will be recreated").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSlot: slot Architecture: arch Oracle: oracle Debug: d = ( |
            | 
                      copyForSlot: slot
                             Self: slot holder
            LexicalParentCompiler: nil
                     Architecture: arch
                           Oracle: oracle
                            Debug: d).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSlot: slot Self: selfMir LexicalParentCompiler: lpc Architecture: arch Oracle: oracle Debug: d = ( |
            | 
                      copyForSlot: slot
                             Self: selfMir
                         Receiver: slot holder
                       LookupType: kleinAndYoda lookupType normal
             ObjectDoingTheResend: nil
            LexicalParentCompiler: lpc
                     Architecture: arch
                           Oracle: oracle
                            Debug: d).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSlot: slot Self: selfMir Receiver: rcvrMir LookupType: lt ObjectDoingTheResend: o LexicalParentCompiler: lpc Architecture: arch Oracle: oracle Debug: d = ( |
             c.
            | 

            " There is one case in which self and rcvr are equal but
              the method is a block method:
              The receiver is a block which inherits a method, m, and also
              occurs in that same method m.
              For example: consider compiling the exitValue method as an outer method
              for the block in that method itself. -- Adam Spitz & David Ungar, 6/05"
            [
              selfMir = rcvrMir
                ifFalse: [ slot contents isReflecteeBlockMethod ]
                   True: [ slot contents isReflecteeBlockMethod not || [rcvrMir isReflecteeBlock] ]
            ] assert: 'self is only not rcvr for block methods'.


            c: slot kleinCompilerPrototypeForMe copy.
            c architecture:                  arch.
            c selfMirror:                    selfMir.
            c slot:                          slot.
            c lookupType:                    lt.
            c objectDoingTheResend:          o.
            c debug:                         d.
            c relocators:                    list copyRemoveAll.
            c nmethodRelocators:             list copyRemoveAll.
            c lexicalParentCompiler:         lpc.
            c oracleForEagerRelocation:      oracle.

            c initializeForceNonLeaf: false.

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
        
         dependsOnBC: bc ResolvingToSlot: s = ( |
            | 
            reusabilityConditions add: reusableIfSlotIsTheSame copyForBC: bc Slot: s.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         doCompile = ( |
            | 
            buildAndLinkIRNodes.
            buildAndLinkValues.
            generateCode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         doCompileIfBackout: blk = ( |
            | 
            backoutBlock: blk.
            doCompile).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         fillInMissingIRNodesByBCI = ( |
             nextNode.
            | 
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: building nmethods\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         findSlotSPOffsets = ( |
             slotSPOffsets.
            | 
            slot isMethod ifFalse: [^ vector].
            slotSPOffsets: list copyRemoveAll.
            method allSlotsOnThisMethod do: [|:slot|
              slot isKleinSlotOffsetRecorded ifTrue: [
                slotSPOffsets addLast: (sourceLevelAllocator valueForSlot: slot name) location spOffsetForNMethod: codeGenerator.
              ].
            ].
            slotSPOffsets asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         forgeControlFlowLinks = ( |
            | 
            startNode sourceNodesDo: [|:n| n forgeControlFlowLinks].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         frame = ( |
            | 
            machineLevelAllocator frame).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: scoping\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         frameAtLexicalLevel: ll = ( |
            | 
            (machineLevelAllocatorAtLexicalLevel: ll) frame).
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

             sourceLevelAllocator:
                prototypes sourceLevelAllocator copyForCompiler: self
                                                         Method: method
                                         LexicalParentAllocator: lexicalParentCompiler ifNotNil: [|:lpc| lpc sourceLevelAllocator].

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
         'Category: accessing\x7fCategory: scoping\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         lexicalParentCount = ( |
            | 
            sourceLevelAllocator lexicalParentCount).
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
         'Category: data flow\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         locationAssigner = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent locationAssigner.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         aliasesForCoalescedValues <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         availableRegisterLocations.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         coalescedMoveNodes <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         compiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodes <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         moveNodesByRelatedValue <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodesNotYetReadyForCoalescing <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodesThatCannotBeCoalesced <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodesThatMayBeCoalescable <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: move nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveNodesThatWillNotBeCoalesced <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         moveRelatedValuesThatHaveNotYetBeenCoalesced <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent locationAssigner parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         aliasOf: v = ( |
            | 
            aliasesForCoalescedValues
                        if: v
               IsPresentDo: [|:alias| aliasOf: alias]
                IfAbsentDo: [v]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         assignLocations = ( |
             highestIndexUsed <- -1.
            | 
            [valuesThatHaveBeenFactoredOut isEmpty] whileFalse: [| v. badLocs |
              debugMode ifTrue: [checkInvariants].
              v: valuesThatHaveBeenFactoredOut removeLast.
              badLocs: set copyRemoveAll.
              v interferingValues do: [|:iv| iv hasLocation ifTrue: [badLocs add: iv location]].
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         checkInvariants = ( |
            | 
            machineLevelAllocator allValues do: [|:v| [(valueSetContaining: v) isNotNil] assert].

            moveNodes do: [|:m. itsInThisOne|
              moveNodeSetsDo: [|:s|
                (s includes: m) ifTrue: [
                  itsInThisOne ifNotNil: [error: 'a move node should never be in two of these sets at once'].
                  itsInThisOne: s.
                ].
              ].
              itsInThisOne ifNil: [error: 'a move node must belong to one of the sets'].
            ].

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

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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
            remainingInterferingValues: v interferingValues intersect: remainingValues.
            [d = remainingInterferingValues size] assert.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: spilling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         chooseAValueToSpill = ( |
             v.
            | 
            [todo optimization]. "Use a better heuristic for choosing which one to spill."
            v: valuesThatCannotBeFactoredOutYet removeFirst.
            valuesThatHaveBeenFactoredOut addLast: v.
            freezeMovesOf: v.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         combine: u With: v = ( |
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
              recordInterferenceBetween: t And: u.

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
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            (((((((((((((resend.copy
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
              valuesThatAlreadyHaveALocation: valuesThatAlreadyHaveALocation copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForCompiler: c = ( |
            | 
            (copy compiler: c) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         debugMode = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         determineAGoodOrderForAssigningLocations = ( |
            | 
            [|:exit|
              case
                if: [valuesThatCanBeFactoredOut                   isEmpty not] Then: [factorOutAValue.         debugMode ifTrue: [checkInvariants]]
                If: [moveNodesThatMayBeCoalescable                isEmpty not] Then: [tryToCoalesceAMoveNode.  debugMode ifTrue: [checkInvariants]]
                If: [moveRelatedValuesThatHaveNotYetBeenCoalesced isEmpty not] Then: [freezeAMoveRelatedValue. debugMode ifTrue: [checkInvariants]]
                If: [valuesThatCannotBeFactoredOutYet             isEmpty not] Then: [chooseAValueToSpill.     debugMode ifTrue: [checkInvariants]]
                Else: exit.
            ] loopExit.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: simplifying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         factorOutAValue = ( |
             v.
            | 
            v: valuesThatCanBeFactoredOut removeFirst.
            valuesThatHaveBeenFactoredOut addLast: v.
            remainingValuesThatInterfereWith: v Do: [|:iv| decrementDegreeOf: iv].
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: freezing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         freezeAMoveRelatedValue = ( |
             u.
            | 
            u: moveRelatedValuesThatHaveNotYetBeenCoalesced removeFirst.
            valuesThatCanBeFactoredOut add: u.
            freezeMovesOf: u.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         go = ( |
            | 
            determineAGoodOrderForAssigningLocations.
            assignLocations.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: invariants\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         hasAlreadyProcessedAllMoveNodesFor: v = ( |
            | 
            moveNodesRelatedTo: v Do: [^ false].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         hasInsignificantDegree: v = ( |
            | 
            (remainingDegreeOf: v) < numberOfAvailableRegisterLocations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         hasSignificantDegree: v = ( |
            | 
            (remainingDegreeOf: v) >= numberOfAvailableRegisterLocations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            availableRegisterLocations: machineLevelAllocator availableRegisterLocations.
            shouldTryToCoalesceMoveNodes ifTrue: [findMoveNodesAndRelatedValues].
            putEachValueInTheAppropriateWorkingSet.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         isMoveRelated: v = ( |
            | 
            moveNodesRelatedTo: v Do: [|:n| ^ true].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         isNoLongerInterfering: v = ( |
            | 
            (valuesThatHaveBeenFactoredOut includes: v) || [aliasesForCoalescedValues includesKey: v]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         machineLevelAllocator = ( |
            | 
            compiler machineLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         mightNotBeMoveRelatedAnymore: v = ( |
            | 
            v hasLocation not && [(isMoveRelated: v) not && [hasInsignificantDegree: v]] ifTrue: [
              moveRelatedValuesThatHaveNotYetBeenCoalesced remove: v.
              valuesThatCanBeFactoredOut add: v.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         numberOfAvailableRegisterLocations = ( |
            | 
            availableRegisterLocations size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         putEachValueInTheAppropriateWorkingSet = ( |
            | 
            machineLevelAllocator allValues do: [|:v. s|
              s: v hasLocation ifTrue: [
                valuesThatAlreadyHaveALocation.
              ] False: [|d|
                [v slot isNil || [v slot isArgument not]] assert.
                d: v interferingValues countHowMany: [|:iv| iv hasLocation not].
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         recordInterferenceBetween: u And: v = ( |
            | 
            u = v ifTrue: [^ self].
            (u interferingValues includes: v) ifTrue: [[v interferingValues includes: u] assert. ^ self].
            [(v interferingValues includes: u) not] assert.
            u interferingValues add: v.
            v interferingValues add: u.
            (isNoLongerInterfering: v) || [v hasLocation] ifFalse: [setRemainingDegreeOf: u To: (remainingDegreeOf: u) succ].
            (isNoLongerInterfering: u) || [u hasLocation] ifFalse: [setRemainingDegreeOf: v To: (remainingDegreeOf: v) succ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         remainingDegreeOf: v = ( |
            | 
            remainingDegreeByValue at: v IfAbsentPut: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         remainingValuesThatInterfereWith: v Do: blk = ( |
            | 
            v interferingValues do: [|:iv|
              (isNoLongerInterfering: iv) ifFalse: [
                blk value: iv.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         setRemainingDegreeOf: v To: d = ( |
            | 
            remainingDegreeByValue at: v Put: d.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldTryToCoalesceMoveNodes = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         sourceLevelAllocator = ( |
            | 
            compiler sourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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
              If: [v hasLocation || [v interferingValues includes: u]] Then: [
                moveNodesThatCannotBeCoalesced add: m.
                mightNotBeMoveRelatedAnymore: u.
                mightNotBeMoveRelatedAnymore: v.
              ]
              If: [willNotCauseSpillsIfWeCoalesce: u With: v] Then: [
                coalescedMoveNodes add: m.
                combine: u With: v.
                mightNotBeMoveRelatedAnymore: u.
              ] Else: [
                moveNodesNotYetReadyForCoalescing add: m.
              ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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
            itsInThisOne ifNil: [error: 'a value must belong to one of the sets'].
            itsInThisOne).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         willNotCauseSpillsIfWeCoalesce: u With: v = ( |
            | 
            u hasLocation ifTrue: [willNotCauseSpillsIfWeCoalescePreallocated:    u With: v]
                           False: [willNotCauseSpillsIfWeCoalesceNonPreallocated: u With: v]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         willNotCauseSpillsIfWeCoalesceNonPreallocated: u With: v = ( |
             interferingValuesIfCoalesced.
            | 
            interferingValuesIfCoalesced: set copyRemoveAll.
            remainingValuesThatInterfereWith: u Do: [|:iv| interferingValuesIfCoalesced add: iv].
            remainingValuesThatInterfereWith: v Do: [|:iv| interferingValuesIfCoalesced add: iv].

            (interferingValuesIfCoalesced copyFilteredBy: [|:iv| hasSignificantDegree: iv]) size < numberOfAvailableRegisterLocations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> 'parent' -> () From: ( | {
         'Category: coalescing moves\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         willNotCauseSpillsIfWeCoalescePreallocated: u With: v = ( |
            | 
            remainingValuesThatInterfereWith: v Do: [|:t|
              (hasInsignificantDegree: t) || [t hasLocation || [t interferingValues includes: u]] ifFalse: [^ false].
            ].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         remainingDegreeByValue <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         valuesThatAlreadyHaveALocation <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         valuesThatCanBeFactoredOut <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         valuesThatCannotBeFactoredOutYet <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'locationAssigner' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (orderedSet copyRemoveAll)\x7fVisibility: private'
        
         valuesThatHaveBeenFactoredOut <- orderedSet copyRemoveAll.
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
         'Category: accessing\x7fCategory: scoping\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         machineLevelAllocatorAtLexicalLevel: ll = ( |
            | 
            sourceLevelAllocator machineLevelAllocatorAtLexicalLevel: ll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         makeSureIRNodesAreBuilt = ( |
            | 
            bytecodeInterpreter ifNil: [
              bytecodeInterpreter: prototypes bytecodeInterpreter copyForCompiler: self.

              bytecodeInterpreter prologue.
              myStartNode:  bytecodeInterpreter firstNode.

              buildBodyIRNodes.

              bytecodeInterpreter lastNode isNonlocalReturn ifFalse: [bytecodeInterpreter localReturn].
              myReturnNode: bytecodeInterpreter lastNode.

              irNodesByBCI: bytecodeInterpreter irNodesByBCI ifNil: vector.
              fillInMissingIRNodesByBCI.
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
         'Category: accessing\x7fCategory: enclosing methods\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         outermostMethodHolder = ( |
            | 
            lexicalParentCompiler
              ifNil:    [slotHolderMirror]
              IfNotNil: [|:lpc| lpc outermostMethodHolder]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: pc offsets\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         pcOffsetsByBCI = ( |
            | 
            irNodesByBCI asVector copyMappedBy: [|:n| n pcOffsetIfPresent: [|:o| o] IfAbsent: -1]).
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
        
         irNodes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes.
'.
            | ) .
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
        
         resolveUnresolvedMoveNodes = ( |
            | 
            bytecodeInterpreter unresolvedMoveNodes do: [|:n. ph. dBCI. dStack|
              ph: n destinationValue.
              [ph isPlaceholder] assert.
              dBCI: ph branchNode destinationNode bc ifNil: [method codes size] IfNotNil: [|:bc| bc pc].
              dStack: bytecodeInterpreter stackValuesWhenBCIWas: dBCI.
              n destinationValue: dStack at: ph stackIndex.
              [n destinationValue isNotNil] assert.
            ].
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
        
         bc.
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
        
         copyForBC: aBC Slot: s = ( |
            | 
            (copy bc: aBC) slot: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'reusableIfSlotIsTheSame' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isSatisfiedBy: aCompilationRequester = ( |
            | 
            (bc asMessage lookupSoleSlotForCompiler: aCompilationRequester) = slot).
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
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         slotHolderMirror = ( |
            | 
            slot holder).
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
        
         vmKit = ( |
            | 
            codeGenerator vmKit).
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
         'Category: bytecode method information\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         selfMirror.
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
         'Category: bytecode method information\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         slot.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> () From: ( | {
         'Category: compilation helpers\x7fComment: allocator for to be compiled lexical level (contains links to parent lexical allocators)\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         sourceLevelAllocator.
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

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'message' -> () From: ( | {
         'Category: Klein\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         lookupSlotsForCompiler: c = ( |
            | 
            lookupSlotsUsing: 
              c protoSlotFinder copyForMirror:
                 isResend ifTrue: [c outermostMethodHolder] False: [c selfMirror]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'message' -> () From: ( | {
         'Category: Klein\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         lookupSoleSlotForCompiler: c = ( |
            | 
            (lookupSlotsForCompiler: c)
              ifNone: nil
               IfOne: [|:slot| slot]
              IfMany: nil).
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
