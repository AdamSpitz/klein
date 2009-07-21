 '$Revision: 30.123 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         codeGenerators = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes codeGenerators.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes codeGenerators abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> () From: ( | {
         'Comment: my assembler\x7fModuleInfo: Module: kleinC1_Gens InitialContents: InitializeToExpression: (nil)'
        
         a.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: InitializeToExpression: (dictionary copyRemoveAll)'
        
         codeSizeStatistics <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: InitializeToExpression: (nil)'
        
         compiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: InitializeToExpression: (nil)'
        
         nodeBeingGenerated.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: InitializeToExpression: (nil)'
        
         nodeToBeGeneratedAfterThisOne.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes codeGenerators abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         add: r1 From: r2 To: dst = ( |
            | 
            add: r1 MaybeSetCCFrom: r2 To: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         add: r1 MaybeSetCCFrom: r2 To: dst = ( |
            | 
            childMustImplement.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         addImm: anInt From: from To: dst = ( |
            | 
            addImm: anInt MaybeSetCCFrom: from To: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         addImm: anInt MaybeSetCCFrom: from To: to = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: temporary registers\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateTemporaryRegister = ( |
            | 
            temporaryRegisters allocateOne).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         andImmMask: anInt AndSetCCFrom: fromReg To: toReg = ( |
            | childMustImplement. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         andImmMask: mask DontSetCCFrom: fromReg To: toReg = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         andImmMask: mask MaybeSetCCFrom: from To: to = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot'
        
         areCommentsEmitted = ( |
            | 
            compiler areCommentsEmitted).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         assembleObject: o = ( |
             or.
            | 

            ifReflecteeOf: (reflect: o) IsImmediateThenDoWithEncoding: [|:encoding|
              ^ a data32: encoding
            ].
            or: vmKit relocators data
                  copyOffset: a locationCounter Object: o.
            compiler addRelocator: or.
            or assembleRealOrPlaceholderInstructionsWith: self.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: perform\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         backpatchSelectorAndDelegateeOfPerformNode: n In: sendDescReg = ( |
             previousDelegateeLoc.
             previousSelectorLoc.
            | 
            previousSelectorLoc:   locationForIndex: sendDesc  selectorIndex InSendDesc: sendDescReg.
            previousDelegateeLoc:  locationForIndex: sendDesc delegateeIndex InSendDesc: sendDescReg.

            generateIf: [|:trueFork|
              generateIf: n  selectorLoc HasChangedFrom: previousSelectorLoc  ThenBranchTo: trueFork.
              generateIf: n delegateeLoc HasChangedFrom: previousDelegateeLoc ThenBranchTo: trueFork.
            ] Then: [
              n  selectorLoc isConstant ifFalse: [moveLocation: n  selectorLoc ToLocation:  previousSelectorLoc ].
              n delegateeLoc isConstant ifFalse: [moveLocation: n delegateeLoc ToLocation:  previousDelegateeLoc].
              invalidateInlineCache: sendDescReg
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: labels\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot'
        
         bindLabel: lbl = ( |
            | 
            a bindLabel: lbl.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         branchEQLikelyTo: dst = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         branchEQUnlikelyTo: dst = ( |
            | 
            childMustImplement.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         branchNELikelyTo: dst = ( |
            | 
            childMustImplement.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         branchNEUnlikelyTo: dst = ( |
            | 
            childMustImplmeent.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: labels\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         branchToLabel: lbl = ( |
            | 
            a branchToLabel: lbl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: debugging\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         breakpoint: msg = ( |
            | 
            a breakpoint: msg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         byteVectorLayout = ( |
            | 
            theVM byteVectorLayout).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         checkForRecompilation = ( |
             nmReg.
            | 
            nmReg: r0.
            "Someday, for performance, we might be able to
             avoid doing the loadNMethodIntoRegister: twice,
             by merging this code with the other code that
             calls loadNMethodIntoRegister:. -- Adam, 11/04"
            loadNMethodIntoRegister: nmReg.
            generateExit: [|:noRecompilationFork|
              withTemporaryRegisterDo: [|:countReg|
                incrementInvocationCountForNMethod: nmReg AndPutCountInRegister: countReg.
                [todo recompilation.
                withTemporaryRegisterDo: [|:thresholdReg|
                  loadOop: invocationCountRecompilationThreshold IntoRegister: thresholdReg.
                  generateIfUnsigned: countReg IsLessThan: thresholdReg ThenLikelyBranchTo: noRecompilationFork.
                ].
                ].
              ].
              [todo recompilation].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         clearHigh: nBits BitsAndShiftLeftBy: shiftBits From: from To: to = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         clearLow: nBits BitsFrom: fromReg To: toReg = ( |
            | 
            childMustImplement.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         codeGenerationMixin* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'codeGenerationMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes codeGenerators abstract parent codeGenerationMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'codeGenerationMixin' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateExit: aBlock = ( |
             end.
            | 
            end: newLabel.
            aBlock value: end.
            bindLabel: end.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'codeGenerationMixin' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Equals: otherObjReg Then: trueBlock Else: falseBlock = ( |
            | 
            generateIf: [|:trueFork|
              generateIf: objReg Equals: otherObjReg ThenBranchTo: trueFork
            ] Then: trueBlock Else: falseBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'codeGenerationMixin' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: testBlock Then: trueBlock = ( |
            | 
            generateIf: testBlock Then: trueBlock Else: []).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'codeGenerationMixin' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: testBlock Then: trueBlock Else: falseBlock = ( |
            | 
            generateExit: [|:end. trueFork|
              trueFork:  newLabel.

              testBlock value: trueFork With: end.
              "fall-through to false fork"

              falseBlock value.
              branchToLabel: end.

              bindLabel: trueFork.
              trueBlock value: end.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'codeGenerationMixin' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: test1Block Then: then1Block If: test2Block Then: then2Block Else: elseBlock = ( |
            | 
            generateExit: [|:end. case1Fork. case2Fork|
              case1Fork:  newLabel.
              case2Fork:  newLabel.

              test1Block value: case1Fork.
              "fall-through to second case test"

              test2Block value: case2Fork.
              "fall-through to else fork"

              elseBlock value.
              branchToLabel: end.

              bindLabel: case1Fork.
              then1Block value.
              branchToLabel: end.

              bindLabel: case2Fork.
              then2Block value.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'codeGenerationMixin' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: test1Block Then: then1Block If: test2Block Then: then2Block If: test3Block Then: then3Block Else: elseBlock = ( |
            | 
            generateExit: [|:end. case1Fork. case2Fork. case3Fork|
              case1Fork:  newLabel.
              case2Fork:  newLabel.
              case3Fork:  newLabel.

              test1Block value: case1Fork.
              "fall-through to second case test"

              test2Block value: case2Fork.
              "fall-through to third case test"

              test3Block value: case3Fork.
              "fall-through to else fork"

              elseBlock value.
              branchToLabel: end.

              bindLabel: case1Fork.
              then1Block value.
              branchToLabel: end.

              bindLabel: case2Fork.
              then2Block value.
              branchToLabel: end.

              bindLabel: case3Fork.
              then3Block value.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'codeGenerationMixin' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIs: objReg EqualTo: otherObjReg Into: dstBoolReg = ( |
            | 
            generateTest: [|:trueFork|
              generateIf: objReg Equals: otherObjReg ThenBranchTo: trueFork
            ] LoadBooleanInto: dstBoolReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'codeGenerationMixin' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateSwitchForCases: cases If: testBlk Then: thenBlk Else: elseBlk = ( |
             forks.
            | 
            generateExit: [|:end|
              forks: cases copyMappedBy: [newLabel].
              cases with: forks Do: [|:c. :f|
                testBlk value: c With: f.
                "fall through to next case"
              ].
              elseBlk value.
              branchToLabel: end.

              cases with: forks Do: [|:c. :f|
                bindLabel: f.
                thenBlk value: c.
                branchToLabel: end.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'codeGenerationMixin' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateTest: testBlock LoadBooleanInto: dstBoolReg = ( |
            | 
            generateIf: testBlock
                  Then: [loadTrueInto:  dstBoolReg]
                  Else: [loadFalseInto: dstBoolReg]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         codeGeneratorForFailureHandler = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         comment: msg = ( |
            | 
            a comment: msg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         constantArgument: o = ( |
            | 
            locationForConstant: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying & initializing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            resend.copy codeSizeStatistics: codeSizeStatistics copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying & initializing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForCompiler: c = ( |
             r.
            | 
            r: (copy compiler: c) initialize.
            c codeGenerator: r.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         copyGeneratedCodeInto: gc = ( |
            | 
            a copyAssembledBytesInto: gc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: labels\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot'
        
         defineLabel = ( |
            | 
            a defineLabel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: error-handling\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         failBlockMessageArgument = ( |
            | 
            machineLevelAllocator locationForOutgoingRcvrOrArgAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         frame = ( |
            | 
            machineLevelAllocator frame).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: temporary registers\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         freeTemporaryRegister: r = ( |
            | 
            temporaryRegisters free: r.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: branch bytecodes\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         genBranchTo: node = ( |
            | 
            insertStackCheckIfBranchingBackwardsTo: node.
            branchToLabel: node label.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         genCallForKey: aLookupKey LiveOopTracker: aLiveOopTracker = ( |
            | 
            compiler noSendsAllowed ifTrue: [error: 'calling ', aLookupKey selector,
                                                    ' but no sends are allowed in this method'].

            "returns nlr"
            [sendMessage_stub].
            sendDesc generateCallStubName: 'sendMessage_stub'
                                LookupKey: aLookupKey
                           LiveOopTracker: aLiveOopTracker
                                     With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         genCallForNode: n = ( |
            | 
            genCallForKey: n lookupKey LiveOopTracker: liveOopTracker copyForNode: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: branch bytecodes\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         genIndexedBranchTo: nodes IndexedBy: loc = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         genNormalCallSelector: sel For: n = ( |
            | 
            genNormalCallSelector: sel LiveOopTracker: liveOopTracker copyForNode: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         genNormalCallSelector: sel LiveOopTracker: aLiveOopTracker = ( |
            | 
             genCallForKey: (vmKit lookupKey copyForNormalSend: sel)
            LiveOopTracker: aLiveOopTracker).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         genStackCheck = ( |
             ok.
            | 
            "Would be dangerous to preempt in unsafe primitives. -- dmu 7/05"
            compiler noGCAllowed ifTrue: [^ self].
            ok: newLabel.
            generateIfUnsigned: registerUsage spLimitRegister
                    IsLessThan: sp
            ThenLikelyBranchTo: ok.
            breakpoint: 'preempted'.
            bindLabel: ok.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: addresses\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateAddressOf: memObjReg Into: dstUntaggedAddressReg = ( |
            | 
            theVM objectLocator generateAddressOf: memObjReg Into: dstUntaggedAddressReg With: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateAssignableParentsCheck = ( |
            | 
            [todo dynamicInheritance].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: block literals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateBlockLiteralNode: node = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: epilogue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateCleanupForMemoizedBlocks: node = ( |
             first <- bootstrap stub -> 'globals' -> 'true' -> ().
             zeroReg.
            | 
            zeroReg: r0.

            node blockValuesToZap do: [|:v|
              first ifTrue: [
                comment: 'zapping memoized blocks'. 
                loadZeroIntoRegister: zeroReg.
                first: false
              ].
              materializeSource: v location AndDo: [|:blockReg|
                generateExit: [|:isAlreadyZeroLabel|
                  generateIf: blockReg EqualsImmediate: 0 ThenLikelyBranchTo: isAlreadyZeroLabel.
                  withTemporaryRegisterDo: [|:tempReg|
                    layouts block homeFramePointerField generateSetValueFor: blockReg To: zeroReg Temp: tempReg With: self Layout: layouts block.
                  ].
                ].
              ].
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: perform\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateDynamicPerformNode: n = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFakePrimitiveNode: n = ( |
            | 
            generateSendNode: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: backpatching\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFlushMachineCachesAfterBackpatching: baseReg FromOffset: startOffset ToOffset: endOffset Temp1: t1 Temp2: t2 = ( |
            | 
            addImm: startOffset * oopSize  From: baseReg To: t1.
            addImm:   endOffset * oopSize  From: baseReg To: t2.
            generateFlushMachineCachesFrom: t1 To: t2.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: addresses\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg At: indexSmiReg Into: dstObjReg = ( |
            | 
            [vmKit tag smi = 0] assert.
            loadWordAt: memObjAddrReg IndexedBy: indexSmiReg To: dstObjReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: addresses\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg At: indexSmiReg Put: dataObjReg BypassingWriteBarrier: shouldBypassWriteBarrier = ( |
            | 
            storeWordFrom: dataObjReg To: memObjAddrReg IndexedBy: indexSmiReg.
            shouldBypassWriteBarrier ifFalse: [
              writeBarrierForStoringOop: dataObjReg AtOffsetInRegister: indexSmiReg IntoObjectAtAddress: memObjAddrReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: addresses\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg AtConstant: index Into: dstObjReg = ( |
            | 
            moveLocation: (locations offsetFromOtherLocation
                                  copyForOffset: index * oopSize
                                   FromRegister: memObjAddrReg)
              ToRegister: dstObjReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: addresses\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg AtConstant: index Put: dataObjReg BypassingWriteBarrier: shouldBypassWriteBarrier = ( |
            | 
            moveRegister: dataObjReg
              ToLocation: locations offsetFromOtherLocation
                                copyForOffset: index * oopSize
                                 FromRegister: memObjAddrReg.

            shouldBypassWriteBarrier ifFalse: [
              writeBarrierForStoringOop: dataObjReg AtOffset: index * oopSize IntoObjectAtAddress: memObjAddrReg.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fCategory: bounds checks\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForVector: vectReg IfIndex: indexSmiReg Temp: tempReg VectorLayout: layout IsOutOfBoundsThenBranchTo: trueFork = ( |
            | 
            layout            generateFor: vectReg
                                  IfIndex: indexSmiReg
                                     Temp: tempReg
                IsOutOfBoundsThenBranchTo: trueFork
                                     With: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg EqualsOop: oop ThenBranchTo: trueFork = ( |
            | 
            withTemporaryRegisterDo: [|:tempReg|
              loadOop: oop IntoRegister: tempReg.
              generateIf: objReg Equals: tempReg ThenBranchTo: trueFork.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: perform\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateIf: locOfCurrentValue HasChangedFrom: locOfPreviousValue ThenBranchTo: aLabel = ( |
            | 
            locOfCurrentValue isConstant ifTrue: [^ self]. "Could not have changed."

            materializeSource: locOfPreviousValue AndDo: [|:previousReg|
              materializeSource: locOfCurrentValue AndDo: [|:currentReg|
                generateIf: currentReg DoesNotEqual: previousReg ThenBranchTo: aLabel.
              ].
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fCategory: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: secondary (i.e. map) type tests\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Temp: tempReg HasAnyMapTypeIn: maps ThenBranchTo: trueFork = ( |
            | 
            vmKit layouts object
                   generateIfMapOf: objReg
                   HasAnyMapTypeIn: maps
                              Temp: tempReg
                      ThenBranchTo: trueFork
                              With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fCategory: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Temp: tempReg HasTag: t IsLikely: isLikely ThenBranchTo: trueFork = ( |
            | 
            t = 0 ifTrue: [
              "Optimization: if we're looking for the kind of oop with tag 0,
               we can do this with fewer instructions."
              andImmMask: vmKit tag mask AndSetCCFrom: objReg To: tempReg.
              isLikely ifTrue: [branchEQLikelyTo:   trueFork]
                        False: [branchEQUnlikelyTo: trueFork].
            ] False: [
              andImmMask: vmKit tag mask MaybeSetCCFrom: objReg To: tempReg.
              isLikely ifTrue: [generateIf: tempReg EqualsImmediate: t ThenLikelyBranchTo:   trueFork]
                        False: [generateIf: tempReg EqualsImmediate: t ThenUnlikelyBranchTo: trueFork].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fCategory: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Temp: tempReg IsByteVectorThenBranchTo: trueFork = ( |
            | 
            generateIf: [|:memFork|
              layouts object generateIf: objReg Temp: tempReg IsMemoryObjectThenBranchTo: memFork With: self.
            ] Then: [
              layouts memoryObject generateIfMem: objReg Temp: tempReg IsByteVectorThenBranchTo: trueFork With: self.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIfUnsigned: reg1 IsLessThan: reg2 ThenLikelyBranchTo: ltLabel = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIfUnsigned: reg1 IsNotLessThan: reg2 ThenUnlikelyBranchTo: tooBig = ( |
            | 
            childMustImplement.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateMethodStartInstruction = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: epilogue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateNLRPoints = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: epilogue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateNLRReturningValueInLocation: returnedValueLoc = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: perform\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePerformNode: n = ( |
            | 
            n isCompletelyStatic
              ifTrue: [ generateSendNode:           n ]
               False: [ generateDynamicPerformNode: n ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: objVectReg At: indexSmiReg PutImmediate: immArgReg IfFail: fh = ( |
            | 
            fh assertVector:    objVectReg.
            fh assertInteger:   indexSmiReg.
            fh assertImmediate: immArgReg.
            fh assertBounds:    indexSmiReg InVector: objVectReg.
            layouts objVector
                  generateFor: objVectReg
                  IndexableAt: indexSmiReg
                 PutImmediate: immArgReg
                         Temp: dstReg
                         With: self.
            moveRegister: objVectReg ToRegister: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: byteVectReg ByteAt: indexSmiReg IfFail: fh = ( |
            | 
            fh assertByteVector:  byteVectReg.
            fh assertInteger: indexSmiReg.
            fh assertBounds:  indexSmiReg InByteVector: byteVectReg.
            byteVectorLayout
                  generateFor: byteVectReg
                  IndexableAt: indexSmiReg
                         Into: dstReg
                         With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: byteVectReg ByteAt: indexSmiReg Put: valueSmiReg IfFail: fh = ( |
            | 
            fh assertByteVector:  byteVectReg.
            fh assertInteger: indexSmiReg.
            fh assertBounds:  indexSmiReg InByteVector: byteVectReg.
            fh assert:        valueSmiReg IsBetweenZeroAnd: 255.
            byteVectorLayout
                  generateFor: byteVectReg
                  IndexableAt: indexSmiReg
                          Put: valueSmiReg
                         Temp: dstReg
                         With: self.
            moveRegister: byteVectReg ToRegister: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: byteVectReg ByteSizeIfFail: fh = ( |
            | 
            fh assertByteVector: byteVectReg.
            byteVectorLayout
             generateIndexableSizeOf: byteVectReg
                                Into: dstReg
                                With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fCategory: C conversion\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: byteVectReg ByteVectorForCIfFail: fh = ( |
            | 
            fh assertByteVector: byteVectReg.
             byteVectorLayout
                generateFor: byteVectReg AddressOfFirstIndexableInto: dstReg With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg CreateMemoryObjectReferenceWithValue: valueSmiReg IfFail: fh = ( |
            | 
            fh assertInteger: valueSmiReg.
            [vmKit tag smi = 0] assert.
            layouts memoryObject
              generateAddTagTo: valueSmiReg
                          Into: dstReg
                          With: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg CreateObjectReferenceWithTag: tagSmiReg AndValue: valueSmiReg IfFail: fh = ( |
            | 
            "We explicitly disallow exposing marks using this primitive
            to preserve the VM invariant that marks are only ever stored
            as the first word of an object, never in one of its fields. -- jb 6/03"

            fh assertInteger: tagSmiReg.
            fh assertInteger: valueSmiReg.
            fh assertValidNonMarkTag: tagSmiReg.
            layouts object
              generateCreateObjectReferenceWithTag: tagSmiReg
                                          AndValue: valueSmiReg
                                              Into: dstReg
                                              With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg ForMemoryObject: memObjReg At: indexSmiReg IfFail: fh = ( |
            | 
            fh assertMemoryObject: memObjReg.
            fh assertInteger:      indexSmiReg.
            layouts memoryObject
                generateFor: memObjReg
                         At: indexSmiReg
                       Into: dstReg
                       With: self.
            "We explicitly disallow exposing marks using this primitive
            to preserve the VM invariant that marks are only ever stored
            as the first word of an object, never in one of its fields. -- jb 6/03"

            fh assertNotMarkInDstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg ForMemoryObject: memObjReg At: indexSmiReg Put: dataReg IfFail: fh = ( |
            | 
            fh assertMemoryObject: memObjReg.
            fh assertInteger:      indexSmiReg.
            layouts memoryObject
              generateFor: memObjReg
                       At: indexSmiReg
                      Put: dataReg
                     Temp: dstReg
                     With: self.
            moveRegister: rcvrReg ToRegister: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg ForMemoryObject: memObjReg ByteAt: indexSmiReg IfFail: fh = ( |
            | 
            fh assertMemoryObject: memObjReg.
            fh assertInteger:      indexSmiReg.
            layouts memoryObject
                generateFor: memObjReg
                     ByteAt: indexSmiReg
                       Into: dstReg
                       With: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg ForMemoryObject: memObjReg ByteAt: indexSmiReg Put: dataSmiReg IfFail: fh = ( |
            | 
            fh assertMemoryObject: memObjReg.
            fh assertInteger:      indexSmiReg.
            fh assertInteger:      dataSmiReg.
            layouts memoryObject
              generateFor: memObjReg
                   ByteAt: indexSmiReg
                      Put: dataSmiReg
                     Temp: dstReg
                     With: self.
            moveRegister: rcvrReg ToRegister: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg ForMemoryObject: memObjReg IsMarkAt: indexSmiReg IfFail: fh = ( |
            | 
            fh assertMemoryObject: memObjReg.
            fh assertInteger:      indexSmiReg.
            layouts memoryObject
                generateFor: memObjReg
                   IsMarkAt: indexSmiReg
                       Into: dstReg
                       With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg ForMemoryObject: memObjReg SetMarkValue: markValueSmiReg IfFail: fh = ( |
            | 
            fh assertMemoryObject: memObjReg.
            fh assertInteger:      markValueSmiReg.
            layouts memoryObject
                 generateFor: memObjReg
                SetMarkValue: markValueSmiReg
                        Temp: dstReg
                        With: self.
            moveRegister: rcvrReg ToRegister: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: blocks\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: blockReg InitializeBlockAtAddress: addrReg HomeFrame: homeFPReg OID: oidReg IfFail: fh = ( |
            | 
            fh assertBlock:    blockReg.
            fh assertInteger:    oidReg.
            fh assertInt32:     addrReg.

            layouts block
               generateInitializeBlockAtAddress: addrReg
                                      FromBlock: blockReg
                                      HomeFrame: homeFPReg
                                            OID: oidReg
                                           Into: dstReg
                                           With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: immediates\x7fCategory: small integers\x7fCategory: C conversions\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg IntForCIfFail: fh = ( |
            | 
            [todo cleanup unsafePrimitives].
            "Rename primitives which are unsafe
            to contain the word unsafe in their names?
            Find a way to mark klein-only primitives?"

            fh assertInteger: rcvrReg.
            layouts smi generateDecode: rcvrReg Into: dstReg With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: immediates\x7fCategory: small integers\x7fCategory: C conversions\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg IntFromCIfFail: fh = ( |
            | 
            layouts smi
              generateEncode: rcvrReg
                        Into: dstReg
                IfDoesNotFit: [fh valueDoesNotFitIntoSmiError]
                        With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: byteVectReg IsByteVectorIfFail: fh = ( |
            | 
            generateTest: [|:trueFork|
              byteVectorLayout generateIf: byteVectReg
                                     Temp: dstReg
                 IsByteVectorThenBranchTo: trueFork
                                     With: self.
            ] LoadBooleanInto: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: immediates\x7fCategory: floats\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg IsFloatIfFail: fh = ( |
            | 
            generateTest: [|:trueFork|
              layouts object generateIf: rcvrReg
                                   Temp: dstReg
                    IsFloatThenBranchTo: trueFork
                                   With: self.
            ] LoadBooleanInto: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: immediates\x7fCategory: small integers\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg IsSmiIfFail: fh = ( |
            | 
            generateTest: [|:trueFork|
              layouts object generateIf: rcvrReg
                                   Temp: dstReg
                      IsSmiThenBranchTo: trueFork
                                   With: self.
            ] LoadBooleanInto: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: objVectReg IsVectorIfFail: fh = ( |
            | 
            generateTest: [|:trueFork|
              layouts object
                      generateIf: objVectReg
                            Temp: dstReg
                 HasAnyMapTypeIn: (vector copyAddLast: maps objVectorMap)
                    ThenBranchTo: trueFork
                            With: self.
            ] LoadBooleanInto: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg MapIfFail: fh = ( |
            | layouts object generateMapOf: rcvrReg Into: dstReg With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg MarkValueOfMemoryObject: memObjReg IfFail: fh = ( |
            | 
            fh assertMemoryObject: memObjReg.
            layouts memoryObject
              generateMarkValueOf: memObjReg
                             Into: dstReg
                             With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: lookup types\x7fComment: We could implement these primitives right inside the sendMessage_stub
(and just call the _Eq: and arithmetic primitives directly); the only
reason these primitives are here is because, for example, we\'d lose
the information that \"4\" is really \"klein lookupType normal\". But
if we had a way to preserve that information, we\'d like to get rid of
these primitives altogether. -- Adam & Alex, 4/04\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg MaskedLookupTypeIfFail: fh = ( |
            | 
            a comment: 'Masking out the modifier bits.'.
                andImmMask:  (layouts smi encode: vmKit lookupType baseMask)
            MaybeSetCCFrom: rcvrReg
                        To: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: object vectors\x7fCategory: maps\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: mapReg NMethodCache: objVectReg IfFail: fh = ( |
            | 
            fh assertMap: mapReg.
            fh assertVector: objVectReg.
            layouts map
                generateSetNMethodCacheOf: mapReg
                                       To: objVectReg
                                     Temp: dstReg
                                     With: self.
            moveRegister: mapReg ToRegister: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: object vectors\x7fCategory: maps\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: mapReg NMethodCacheIfFail: fh = ( |
            | 
            fh assertMap: mapReg.
            layouts map
                generateNMethodCacheOf: mapReg
                                  Into: dstReg
                                  With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fCategory: nmethods\x7fComment: Warning GC unsafe.
Returns an address into a bytes part masquerading as a small integer.
-- jb 7/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: nmReg NMethodEntryPointIfFail: fh = ( |
            | 
            fh assertNMethod: nmReg.
            byteVectorLayout
              generateFor:                    nmReg
                AddressOfIndexableAtConstant: vmKit maps nmethodMap firstInstructionIndex
                                        Into: dstReg
                                        With: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: objVectReg SizeIfFail: fh = ( |
            | 
            fh assertVector:  objVectReg.
            layouts objVector
              generateIndexableSizeOf: objVectReg
                                 Into: dstReg
                                 With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg TagPartOfObjectReferenceIfFail: fh = ( |
            | 
            layouts object
                generateTagOf: rcvrReg
                         Into: dstReg
                         With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: bytes part\x7fComment: Warning GC unsafe.
-- jb 7/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg UnsafeForBytesPart: bpRefReg At: indexSmiReg IfFail: fh = ( |
            | 
            fh assertBytesPart: bpRefReg. "this assertion only checks to see if bytesPart is an integer"
            fh assertInteger: indexSmiReg.
            layouts bytesPart
              generateForBytesPart: bpRefReg
                                At: indexSmiReg
                              Into: dstReg
                              With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: bytes part\x7fComment: Warning GC unsafe.
-- jb 7/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg UnsafeForBytesPart: bpRefReg At: indexSmiReg Put: valueSmiReg IfFail: fh = ( |
            | 
            fh assertBytesPart: bpRefReg.
            fh assertInteger:   indexSmiReg.
            fh assertInteger:   valueSmiReg.
            fh assert:          valueSmiReg IsBetweenZeroAnd: 255.
            withTemporaryRegisterDo: [|:tempReg|
              layouts bytesPart
                      generateForBytesPart: bpRefReg
                                        At: indexSmiReg
                                       Put: valueSmiReg
                                     Temp1: dstReg
                                     Temp2: tempReg
                                      With: self.
              moveRegister: rcvrReg ToRegister: dstReg.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: bytes part\x7fComment: Warning GC unsafe.
-- jb 7/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg UnsafeForBytesPart: bpRefReg SetIndexableSize: sizeSmiReg IfFail: fh = ( |
            | 
            moveRegister: sizeSmiReg
              ToLocation: layouts bytesPart
                            locationForIndexableSizeOfBytesPart: bpRefReg
                                                           With: self.
            moveRegister: rcvrReg ToRegister: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: bytes part\x7fComment: Warning GC unsafe.
-- jb 7/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg UnsafeIndexableSizeOfBytesPart: bpRefReg IfFail: fh = ( |
            | 
            fh assertBytesPart: bpRefReg.
            moveLocation: (layouts bytesPart
                             locationForIndexableSizeOfBytesPart: bpRefReg
                                                            With: self)
              ToRegister: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg ValuePartOfObjectReferenceIfFail: fh = ( |
            | 
            layouts object
                generateValueOf: rcvrReg
                           Into: dstReg
                           With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: byteVectReg WithoutAnySendsStringPrintIfFail: fh = ( |
             numberOfWriteSysCall = 4.
             stdoutFD = 1.
            | 

            fh assertByteVector: byteVectReg.

            withTemporaryRegisterDo: [|:fdReg|
              withTemporaryRegisterDo: [|:bytesReg. byteCountReg|
                byteCountReg: dstReg.

                a load32To: fdReg From: stdoutFD.

                byteVectorLayout
                  generateFor: byteVectReg AddressOfFirstIndexableInto: bytesReg With: self.

                byteVectorLayout
                  generateIndexableSizeOf: byteVectReg Into: byteCountReg With: self.
                layouts smi generateDecode: byteCountReg Into: byteCountReg With: self.

                generateSystemCallNumber: numberOfWriteSysCall
                                 ArgLocs: (fdReg & bytesReg & byteCountReg) asVector
                                    Into: dstReg.
              ].
            ].
            layouts smi generateEncode: dstReg Into: dstReg With: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveNode: n = ( |
            | 
            [generatePrimitive_IntAdd_IfFail_: n]. "browsing"
            "For more browsing tags, see" [browsingTagsForGeneratedSlots].

            ( primitiveGenerationMethodNameForSelector: n selector )
               sendTo: self With: n.
            "browsing"
            [undefinedSelector: nil Type: nil Delegatee: nil MethodHolder: nil Arguments: nil].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: arguments\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_ArgAt_IfFail_: node = ( |
             index.
            | 
            [_ArgAt: 0           ]. "browsing"
            [_ArgAt: 0 IfFail: fb]. "browsing"
            index: node rcvrOrArgOopValueForConstantLocAt: 1 IfFail: raiseError.
            moveLocation: (sourceLevelAllocator valueForIncomingRcvrOrArgAt: index) location
              ToLocation: node resultLoc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: debugging\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_BreakpointIfFail_: node = ( |
            | 
            [_Breakpoint          ]. "browsing"
            [_BreakpointIfFail: fb]. "browsing"
            moveLocation: node rcvrLoc ToLocation: node resultLoc.
            breakpoint: ''.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: debugging\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_Breakpoint_IfFail_: node = ( |
             cmt.
            | 
            [_Breakpoint: 'comment'           ]. "browsing"
            [_Breakpoint: 'comment' IfFail: fb]. "browsing"
            cmt: node rcvrOrArgOopValueForConstantLocAt: 1 IfFail: 'non-constant breakpoint message'.
            moveLocation: node rcvrLoc ToLocation: node resultLoc.
            breakpoint: '_Breakpoint: ', cmt.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: blocks\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_CloneBlockHomeFrame_OID_Space_IfFail_: node = ( |
            | 
            [ _CloneBlockHomeFrame: 0 OID: 0 Space: nil            ]. "browsing"
            [ _CloneBlockHomeFrame: 0 OID: 0 Space: nil IfFail: fb ]. "browsing"
            materializeLocsAndFailureHandlerOf: node AndDo: [|:dstReg. :blockReg. :homeFPReg. :oidReg. :spaceReg. :fh. retryLabel |
              fh assertBlock:    blockReg.
              fh assertInteger:  oidReg.

              retryLabel: defineLabel.
              layouts block
                    generateCloneBlock: blockReg
                             HomeFrame: homeFPReg
                                   OID: oidReg
                                 Space: spaceReg
                                  Into: dstReg
                     IfOutOfMemoryThen: [setUpSendArguments: vector copyAddFirst: locationForConstant: vmKit garbageCollector.
                                         genNormalCallSelector: 'scavenge' LiveOopTracker: liveOopTracker copyForNode: node.
                                         branchToLabel: retryLabel]
                                  With: self.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_In_WhichIsOfType_Get_IfFail_: node = ( |
             dataSlotName.
             map.
             prototype.
            | 

            [_In: target WhichIsOfType: 'blah' Get: 'parent'           ]. "browsing"
            [_In: target WhichIsOfType: 'blah' Get: 'parent' IfFail: fb]. "browsing"

            prototype:      node rcvrOrArgOopValueForConstantLocAt: 2 IfFail: raiseError.
            dataSlotName:   node rcvrOrArgOopValueForConstantLocAt: 3 IfFail: raiseError.
            map: (reflect: prototype) vmKitMapForConversion.
            comment: ['accessing slot named ', dataSlotName, ' on an object with map type ', map mapType].

            materializeLocsAndFailureHandlerOf: node AndDo: [|:dstReg. :rcvrReg. :targetReg. :prototypeReg. :dataSlotNameReg. :fh. slot|
              fh assert: targetReg HasMapType: map.
              slot:  prototype asMirror slotAt: dataSlotName.
              loadFromDataSlot: slot OfHolderRegister: targetReg IntoRegister: dstReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_In_WhichIsOfType_Set_To_IfFail_: node = ( |
             dataSlotName.
             map.
             prototype.
            | 

            [_In: target WhichIsOfType: 'blah' Set: 'parent'                 ]. "browsing"
            [_In: target WhichIsOfType: 'blah' Set: 'parent' To: 0 IfFail: fb]. "browsing"

            prototype:      node rcvrOrArgOopValueForConstantLocAt: 2 IfFail: raiseError.
            dataSlotName:   node rcvrOrArgOopValueForConstantLocAt: 3 IfFail: raiseError.
            map: (reflect: prototype) vmKitMapForConversion.
            comment: ['setting slot named ', dataSlotName, ' on an object with map type ', map mapType].

            materializeLocsAndFailureHandlerOf: node AndDo: [|:dstReg. :rcvrReg. :targetReg. :prototypeReg. :dataSlotNameReg. :objReg. :fh. slot|
              fh assert: targetReg HasMapType: map.
              slot:  prototype asMirror slotAt: dataSlotName.
              storeIntoDataSlot: slot OfHolderRegister: targetReg FromRegister: objReg IsGuaranteedNotToBeMemObj: false.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: blocks\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_OnNonLocalReturn_IfFail_: node = ( |
             doneLabel.
             nlrLabel.
             nlrLabelForProtectBlock.
            | 
            "The Self VM checks the original result for mark-ness
             to propagate an abort. We don't. -- dmu 9/04"

            [b _OnNonLocalReturn: protectBlock           ]. "browsing"
            [b _OnNonLocalReturn: protectBlock IfFail: fb]. "browsing"

            comment: 'call orig block'.
            moveLocation: node rcvrLoc ToLocation: machineLevelAllocator locationForOutgoingRcvrOrArgAt: 0.
            nlrLabel: newLabel.
            node specialLabelToBranchToOnNLR: nlrLabel.
            genNormalCallSelector: 'value' LiveOopTracker: liveOopTracker copyForNode: node.
            node specialLabelToBranchToOnNLR: nil.

            comment: 'no NLR happened, so return the result of running the block'.
            moveLocation: machineLevelAllocator locationForIncomingResult ToLocation: node resultLoc.
            doneLabel: newLabel.
            branchToLabel: doneLabel.

            comment: 'did an NLR, so run the protectBlock'.
            bindLabel: nlrLabel.

            comment: 'save orig NLR data in a safe place'.
            moveLocation: machineLevelAllocator locationForIncomingNLRHomeScope     ToLocation: node homeScopeValue     location.
            moveLocation: machineLevelAllocator locationForIncomingNLRHomeScopeDesc ToLocation: node homeScopeDescValue location.

            comment: 'call protect block'.
            moveLocation: machineLevelAllocator locationForIncomingResult ToLocation: machineLevelAllocator locationForOutgoingRcvrOrArgAt: 1.
            moveLocation: node arg1Value location                         ToLocation: machineLevelAllocator locationForOutgoingRcvrOrArgAt: 0.

            nlrLabelForProtectBlock: newLabel.
            node specialLabelToBranchToOnNLR: nlrLabelForProtectBlock.
            genNormalCallSelector: 'value:' LiveOopTracker: liveOopTracker copyForNode: node.
            node specialLabelToBranchToOnNLR: nil.

            bindLabel: nlrLabelForProtectBlock. "ignore protect block NLR"

            comment: 'resume orig NLR'.
            moveLocation: node homeScopeValue     location ToLocation: machineLevelAllocator locationForIncomingNLRHomeScope.
            moveLocation: node homeScopeDescValue location ToLocation: machineLevelAllocator locationForIncomingNLRHomeScopeDesc.

            "Continue NLRing."
            branchToLabel: node labelToBranchToOnNLR.

            bindLabel: doneLabel.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrologue: startNode = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateSendNode: n = ( |
            | 
            generating: 'genCallForNode:' During: [genCallForNode: n].
            a locationCounter).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: error-handling\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateShouldNeverHappen: reason = ( |
            | 
            breakpoint: reason).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: system calls\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateSystemCallNode: n = ( |
            | 
            generateSystemCallNumber: n systemCallNumber
                             ArgLocs: n locsOfArgsToPassIntoSystemCall
                                Into: n resultLoc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: system calls\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSystemCallNumber: systemCallNumber ArgLocs: argLocs Into: resultLoc = ( |
            | 
            setUpSendArguments: argLocs.
            generateTrapInstructionForSystemCall: systemCallNumber.
            materializeDest: resultLoc AndDo: [|:dstReg| moveSystemCallResultToReg: dstReg].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: system calls\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateTrapInstructionForSystemCall: systemCallNumber = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateUnimplementedPrimitiveNode: n = ( |
             primitiveMethodNameWithIfFail.
             primitiveMethodNameWithoutIfFail.
            | 
            primitiveMethodNameWithoutIfFail: n selector copyWithoutSuffix: 'IfFail:'.
            primitiveMethodNameWithIfFail: primitiveMethodNameWithoutIfFail, 'IfFail:'.

            materializeLocsAndFailureHandlerVectorOf: n AndDo: [|:args. fh|
              fh: args last.
              fh notImplementedYetError: primitiveMethodNameWithIfFail.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateVerifiedEntryPoint = ( |
            | 
            comment: 'verified entry point'.
            verifiedEntryPointIndex: a locationCounter.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatedCode = ( |
            | a assembledBytes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatedCodeSize = ( |
            | a assembledBytesSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generating: kindOfGeneration During: aBlock = ( |
            | 
            shouldGatherStatistics ifFalse: aBlock True: [| sizeBefore |
              sizeBefore: a assembledBytesSize.
              aBlock onReturn: [| numberOfBytesGenerated |
                numberOfBytesGenerated: a assembledBytesSize - sizeBefore.
                codeSizeStatistics if: kindOfGeneration value
                         IsPresentPut: [|:n| n + numberOfBytesGenerated] AndDo: []
                          IfAbsentPut: [         numberOfBytesGenerated] AndDo: [].
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         ifReflecteeOf: m IsImmediateThenDoWithEncoding: blk = ( |
            | 
            m isReflecteeKleinAndYodaImmediate ifTrue: [^ blk value:  m kleinAndYodaLayout encode: m reflectee].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fCategory: nmethod invocation counts\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         incrementInvocationCountForNMethod: nmReg AndPutCountInRegister: countReg = ( |
            | 
            loadFromDataSlot: nmethodInvocationCountSlot OfHolderRegister: nmReg IntoRegister: countReg.
            addImm: (layouts smi encode: 1) From: countReg To: countReg.
            storeIntoDataSlot: nmethodInvocationCountAssignmentSlot OfHolderRegister: nmReg FromRegister: countReg IsGuaranteedNotToBeMemObj: true.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying & initializing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            a: (isLogUsed ifTrue: [myAssemblerSystem loggingAssembler]
                           False: [myAssemblerSystem        assembler])
                copyOrigin: 0.
            a areCommentsEmitted: areCommentsEmitted.
            initializeTemporaryRegisterManager.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: temporary registers\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeTemporaryRegisterManager = ( |
            | 
            temporaryRegisters: resourceManager copyNamed: 'temporary registers'.
            registerUsage tempRegistersDo: [|:r| temporaryRegisters add: r].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: branch bytecodes\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         insertStackCheckIfBranchingBackwardsTo: node = ( |
            | 
            node label isResolved ifTrue: [ genStackCheck].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: perform\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         invalidateInlineCache: sendDescReg = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fCategory: nmethod invocation counts\x7fComment: We don\'t do invocation *rates* yet; we just count
the total number. -- Adam, June 2009\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         invocationCountRecompilationThreshold = 500000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot'
        
         isLogUsed = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         layouts = ( |
            | 
            vmKit layouts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         liveOopTracker = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'liveOopTracker' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes codeGenerators abstract parent liveOopTracker.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'liveOopTracker' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         node.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'liveOopTracker' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'liveOopTracker' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes codeGenerators abstract parent liveOopTracker parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'liveOopTracker' -> 'parent' -> () From: ( | {
         'Category: zapping\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         badOop = ( |
            | 
            "We could use a more informative one. Maybe the name of the
             method we're compiling? -- Adam, 8/05"
            klein tag mark).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'liveOopTracker' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForNode: n = ( |
            | 
            copy node: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'liveOopTracker' -> 'parent' -> () From: ( | {
         'Category: masks\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         gcMask = ( |
            | 
            gcMaskLayout maskForLocations: node locationsThatNeedToBePreserved).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'liveOopTracker' -> 'parent' -> () From: ( | {
         'Category: masks\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         gcMaskLayout = ( |
            | 
            node codeGenerator sendDesc gcMaskLayout).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'liveOopTracker' -> 'parent' -> () From: ( | {
         'Category: zapping\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         locationsThatShouldBeZapped = ( |
             couldZap.
             preserve.
            | 
            couldZap: node machineLevelAllocator locationsThatCouldBeDead.
            preserve: node locationsThatNeedToBePreserved.
            couldZap asSet copyFilteredBy: [|:loc|
                 (gcMaskLayout shouldLocationBeRepresentedInGCMask: loc)
              && [(preserve includes: loc) not]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'liveOopTracker' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'liveOopTracker' -> 'parent' -> () From: ( | {
         'Category: zapping\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         zapDeadLocations = ( |
            | 
            locationsThatShouldBeZapped isEmpty ifTrue: [^ self].
            node codeGenerator comment: 'zap-a-lot!'.
            locationsThatShouldBeZapped do: [|:loc| zapLocation: loc].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'liveOopTracker' -> 'parent' -> () From: ( | {
         'Category: zapping\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         zapLocation: loc = ( |
            | 
            loc isConstant ifTrue: [^ self].
            node codeGenerator moveWord: badOop ToLocation: loc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         load32BitsAtOffset: o FromAddressInRegister: addressReg ToRegister: dstReg = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         loadByteAt: addr IndexedBy: indexReg To: dst = ( |
            | 
            childMustImplement.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: loading\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadConstantLocation: loc ToRegister: r = ( |
            | 
            loadOop: loc oopValue IntoRegister: r NameForComment: [loc nameForComment].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadFalseInto: dstBoolReg = ( |
            | 
            loadOop: false IntoRegister: dstBoolReg NameForComment: 'false'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing data slots\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         loadFromConstantDataSlot: slot IntoRegister: dstReg = ( |
            | 
            [slot isAssignable not] assert.
            [slot isMethod     not] assert.
            [slot isAssignment not] assert.
            comment: 'Loading value of constant slot'.
            moveLocation: (locations constant copyForOop: slot contents reflectee)
              ToRegister: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing data slots\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         loadFromDataSlot: slot OfHolderInLocation: holderLoc IntoLocation: dstLoc = ( |
            | 
            materializeDest: dstLoc AndDo: [|:dstReg|
              materializeSource: holderLoc AndDo: [|:holderReg|
                loadFromDataSlot: slot OfHolderRegister: holderReg IntoRegister: dstReg.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing data slots\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         loadFromDataSlot: slot OfHolderRegister: holderReg IntoRegister: dstReg = ( |
            | 
            slot isAssignable ifTrue: [
              withTemporaryRegisterDo: [|:holderAddressReg|
                layouts memoryObject generateAddressOf: holderReg Into: holderAddressReg With: self.
                loadFromDataSlot: slot OfHolderWithAddress: holderAddressReg IntoRegister: dstReg.
              ].
            ] False: [
              loadFromConstantDataSlot: slot IntoRegister: dstReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing data slots\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         loadFromDataSlot: slot OfHolderWithAddress: holderAddressReg IntoRegister: dstReg = ( |
            | 
            slot isAssignable ifTrue: [| or |
              comment: 'Next instruction has its offset patched'.
              or: vmKit relocators objectSlotOffset
                    copyOffset: a locationCounter 
                      SlotName: slot name
                     HolderMap: (compiler mapOfHolderOfSlot: slot)
                       DataReg: dstReg
                       BaseReg: holderAddressReg
                        IsLoad: true.
              compiler addRelocator: or.
              or assembleRealOrPlaceholderInstructionsWith: self.
            ] False: [
              loadFromConstantDataSlot: slot IntoRegister: dstReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: loading\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadIncomingMemoryArgumentLocation: loc ToRegister: r = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadNMethodIntoRegister: r = ( |
             or.
            | 
            or:        loadOop: () _Clone
                  IntoRegister: r
                NameForComment: 'an nmethod'
                RelocatorProto: vmKit relocators loadNMethod.
            compiler addNMethodRelocator: or.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: loading\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadNonVolMemoryLocalLocation: loc ToRegister: r = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadOop: oop IntoRegister: r = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: loading\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadOutgoingMemoryArgumentLocation: loc ToRegister: r = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadReflecteeOf: m IntoLocation: loc = ( |
            | 
            materializeDest: loc
                      AndDo: [|:r| loadReflecteeOf: m IntoRegister: r ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadReflecteeOf: m IntoRegister: r = ( |
            | 
            loadOop: m reflectee IntoRegister: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: loading\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadRegisterLocation: loc ToRegister: r = ( |
            | 
            moveRegister: loc ToRegister: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadTrueInto: dstBoolReg = ( |
            | 
            loadOop: true IntoRegister: dstBoolReg NameForComment: 'true'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         loadValueAtOffset: o FromAddressInRegister: addressReg ToRegister: dstReg = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         loadWordAt: addr IndexedBy: index To: dst = ( |
            | 
            childMustImplement.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadZeroIntoRegister: r = ( |
            | 
            loadOop: 0 IntoRegister: r.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForConstant: o = ( |
            | 
            machineLevelAllocator locationForConstant: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         locations = ( |
            | 
            compiler locations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot'
        
         log = ( |
            | a log).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         machineLevelAllocator = ( |
            | 
            compiler machineLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         maps = ( |
            | 
            vmKit maps).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: materializing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         materializeDest: loc AndDo: blk = ( |
            | 
            loc isRegister ifTrue: [^ blk value: loc register].
            withTemporaryRegisterDo: [|:r|
              blk value: r.
              moveRegister: r ToLocation: loc.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: materializing\x7fComment: Materializes all non-register locations into volatile registers.
We do NOT consume temporary registers for the receiver and
arguments because this may limit the number of arguments that
can be passed to the primitive under particular circumstances. 

However, we handle the case where the destination location may
be colocated with the receiver or an argument by using an
unused argument register.  The real problem with allocating
a temporary is that there are only 2 available on the PPC
and some primitives depend on both being available for use
and may fail to compile otherwise.
-- jb 8/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         materializeDestAndArgsOf: node AndDo: blk = ( |
             dstLoc.
             dstReg.
             rcvrAndArgLocs.
             rcvrAndArgRegs.
             tempResultReg.
            | 

            [aaaaa]. "Could this be made a lot simpler now?"

            [node areVolatilesVaporized] assert.
            [node argumentCount <= registerUsage maxArgumentRegisters pred "pred for dstReg"] assert.
            tempResultReg: registerUsage lastOutgoingArgumentRegister.
            dstLoc: node resultLoc.
            dstLoc isRegister
              ifTrue: [dstReg: dstLoc register].

            rcvrAndArgLocs: node rcvrAndArgLocsToBeMaterialized.

            rcvrAndArgRegs: rcvrAndArgLocs copyMappedBy: [|:loc. :i. r |
              loc isRegister
                ifTrue: [r: loc register]
                 False: [r: (machineLevelAllocator locationForOutgoingRcvrOrArgAt: i) register.
                         moveLocation: loc ToRegister: r].

              dstReg = r ifTrue: [dstReg: nil]. "detect colocation of destination"
              [r != tempResultReg] assert.
              r
            ].

            dstReg ifNil: [dstReg: tempResultReg].

            rcvrAndArgLocs size > 5  ifTrue: [error: 'unimplemented'].

            [
              blk value: dstReg With: rcvrAndArgRegs.
            ]
            onReturn: [
              moveRegister: dstReg ToLocation: dstLoc.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: materializing\x7fComment: Materializes all non-register locations into volatile registers.
We do NOT consume temporary registers for the receiver and
arguments because this may limit the number of arguments that
can be passed to the primitive under particular circumstances. 

However, we handle the case where the destination location may
be colocated with the receiver or an argument by using an
unused argument register.  The real problem with allocating
a temporary is that there are only 2 available on the PPC
and some primitives depend on both being available for use
and may fail to compile otherwise.
-- jb 8/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         materializeLocsOf: node AndDo: blk = ( |
            | 
            materializeDestAndArgsOf: node AndDo: [|:dstReg. :rcvrAndArgRegs|
              rcvrAndArgRegs size > 5  ifTrue: [error: 'unimplemented'].
              pass: (rcvrAndArgRegs copyAddFirst: dstReg) Into: blk.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: materializing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         materializeSource: loc AndDo: blk = ( |
            | 
            loc isRegister ifTrue: [^ blk value: loc register].
            withTemporaryRegisterDo: [|:r|
              moveLocation: loc ToRegister: r.
              blk value: r.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: materializing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         materializeSource: loc UsingTempIfNecessary: tempReg AndDo: blk = ( |
            | 
            loc isRegister ifTrue: [^ blk value: loc register].
            moveLocation: loc ToRegister: tempReg.
            blk value: tempReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: materializing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         materializeSourceAndDest: loc AndDo: blk = ( |
            | 
            loc isRegister ifTrue: [^ blk value: loc register].
            withTemporaryRegisterDo: [|:r|
              moveLocation: loc ToRegister: r.
              blk value: r.
              moveRegister: r ToLocation: loc.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         methodStartInstruction = ( |
            | 
            cachedMethodStartInstruction ifNil: [
              cachedMethodStartInstruction: generateMethodStartInstruction.
            ].
            cachedMethodStartInstruction).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         move: src To: dst = ( |
            | 
            moveLocation: src ToLocation: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         moveLocation: src ToLocation: dst = ( |
            | 
            src = dst ifTrue: [^ self].
            dst isRegister ifTrue: [^ moveLocation: src ToRegister: dst register].
            materializeSource: src AndDo: [|:r| moveRegister: r ToLocation: dst]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: locations <-> registers\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         moveLocation: loc ToRegister: r = ( |
            | 
            loc tell: self ToLoadMeToRegister: r.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: locations <-> registers\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         moveRegister: r ToLocation: dst = ( |
            | 
            dst tell: self ToStoreMeFromRegister: r.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         moveRegister: rs ToRegister: rd = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         moveSendResultTo: dstLoc = ( |
            | 
            moveLocation: machineLevelAllocator locationForIncomingResult
              ToLocation: dstLoc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: system calls\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         moveSystemCallResultToReg: dstReg = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         moveWord: w ToLocation: loc = ( |
            | 
            materializeDest: loc AndDo: [|:r|
              a load32To: r From: w.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         myAssemblerSystem = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: labels\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot'
        
         newLabel = ( |
            | a newLabel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fCategory: nmethod invocation counts\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         nmethodInvocationCountAssignmentSlot = ( |
            | 
            [invocationCount: 0]. "browsing"
            vmKit nmethod asMirror slotAt: 'invocationCount:').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fCategory: nmethod invocation counts\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         nmethodInvocationCountSlot = ( |
            | 
            [aaaaaaa]. "Used to cache these, but then mapping the slot object
                        caused trouble on the Klein side. How do I get the transporter
                        to initialize the Klein-side one to nil? -- Adam, July 2009"
            [invocationCount]. "browsing"
            vmKit nmethod asMirror slotAt: 'invocationCount').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         oopSize = ( |
            | 
            layouts abstract oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         orMask: maskReg MaybeSetCCFrom: from To: to = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         origin = ( |
            | a origin).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         primitiveGenerationMethodSlots = ( |
            | 
            ( asMirror ancestorsUpTo: lobby asMirror )  gather: [|:m|
              m asList copyFilteredBy: [|:s| 'generatePrimitive_' isPrefixOf: s name]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         primitiveTranslatorMixin* = bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         registerUsage = ( |
            | 
            (machineLevelAllocator ifNil: [compiler protoAllocatorForMyPlatform]) registerUsage).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: temporary registers\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         resourceManager = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'resourceManager' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes codeGenerators abstract parent resourceManager.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'resourceManager' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: public'
        
         available <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'resourceManager' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: InitializeToExpression: (\'\')\x7fVisibility: public'
        
         name <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'resourceManager' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'resourceManager' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes codeGenerators abstract parent resourceManager parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'resourceManager' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         add: r = ( |
            | 
            available add: r.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'resourceManager' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOne = ( |
            | 
            available removeFirstIfAbsent: [error: 'ran out of ', name]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'resourceManager' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOneAndDo: blk = ( |
             r.
            | 
            [
              r: allocateOne.
              blk value: r
            ] onReturn: [free: r]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'resourceManager' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            resend.copy available: available copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'resourceManager' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         copyNamed: n = ( |
            | 
            copy name: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'resourceManager' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         free: r = ( |
            | 
            available add: r.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> 'resourceManager' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         send: sel RcvrAndArgs: rcvrAndArgRegs Result: dstReg For: node = ( |
            | 
            setUpSendArguments: rcvrAndArgRegs.
            genNormalCallSelector: sel For: node.
            moveSendResultTo: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         setUpSendArguments: rcvrAndArgLocs = ( |
            | 
            rcvrAndArgLocs asVector do: [|:loc. :i|
              moveLocation: loc ToLocation: machineLevelAllocator locationForOutgoingRcvrOrArgAt: i.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         shiftLeftImmBy: nBits From: src To: dst = ( |
            | 
            childMustImplement.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         shiftRightArithImmBy: nBits From: src To: dst = ( |
            | 
            childMustImplement.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         shiftRightImmBy: nBits From: src To: dst = ( |
            | 
            childMustImplement.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBeLazyAboutCloningPrimitiveFailBlocks = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldGatherStatistics = ( |
            | 
            compiler shouldGatherStatistics).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: sends\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldZapDeadLocations = ( |
            | 
            compiler shouldZapDeadLocations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         sourceLevelAllocator = ( |
            | 
            [aaaaa]. "This should really go away. I think it's not used much anymore. Just _ArgAt:?"
            machineLevelAllocator topSourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         storeByteFrom: src To: addr IndexedBy: indexReg = ( |
            | 
            childMustImplement.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing data slots\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         storeIntoDataSlot: slot OfHolderInLocation: holderLoc FromLocation: srcLoc = ( |
            | 
            materializeSource: srcLoc AndDo: [|:srcReg|
              materializeSource: holderLoc AndDo: [|:holderReg|
                storeIntoDataSlot: slot OfHolderRegister: holderReg FromRegister: srcReg IsGuaranteedNotToBeMemObj: false
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing data slots\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         storeIntoDataSlot: slot OfHolderRegister: holderReg FromRegister: srcReg IsGuaranteedNotToBeMemObj: definitelyNotMemObj = ( |
             or.
            | 
            [slot isAssignment] assert.
            withTemporaryRegisterDo: [|:holderAddressReg|
              layouts memoryObject generateAddressOf: holderReg Into: holderAddressReg With: self.
              comment: 'Next instruction has its offset patched'.
              or: vmKit relocators objectSlotOffset
                 copyOffset: a locationCounter 
                   SlotName: slot name
                  HolderMap: (compiler mapOfHolderOfSlot: slot)
                    DataReg: srcReg
                    BaseReg: holderAddressReg
                     IsLoad: false.
              compiler addRelocator: or.
              or assembleRealOrPlaceholderInstructionsWith: self.

              "If this isn't getting tripped, maybe we don't need these relocators at all. -- Adam, Mar. 2009"
              [aaa]. or hasCompiledOopBeenSet ifFalse: [halt].

              definitelyNotMemObj ifFalse: [
                writeBarrierForStoringOop: srcReg AtOffset: or slotByteOffset IntoObjectAtAddress: holderAddressReg Temp: holderAddressReg.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: storing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         storeRegister: r ToConstantLocation: loc = ( |
            | 
            error: 'inconceivable').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: storing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         storeRegister: r ToIncomingMemoryArgumentLocation: loc = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: storing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         storeRegister: r ToNonVolMemoryLocalLocation: loc = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: storing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         storeRegister: r ToOutgoingMemoryArgumentLocation: loc = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: storing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         storeRegister: r ToRegisterLocation: loc = ( |
            | 
            moveRegister: r ToRegister: loc register).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         storeWordFrom: src To: addr IndexedBy: index = ( |
            | 
            childMustImplement.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         storeWordInRegister: srcReg ToOffset: o FromAddressInRegister: addressReg = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         topSourceLevelAllocator = ( |
            | 
            machineLevelAllocator topSourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: auto-generating\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         undefinedSelector: sel Type: t Delegatee: d MethodHolder: mh Arguments: args = ( |
             agmn <- ''.
             n.
            | 

            ["called by" generatePrimitiveNode: nil]. "browsing"

                ( t = 'normal')
            && [  d isNil
            && [ mh isNil
            && [slotGenerator isPrimitiveGenerationSelector: sel]]]
             ifFalse: [error: sel, ' is not understood'].

            "may need to auto-generate"
            n: args first.
            agmn: slotGenerator autoGeneratingMethodNameForSelector: n selector.

            (asMirror ancestorsUpTo: traits clonable asMirror) do: [|:m|
              (m includesKey: agmn) ifTrue: [
                slotGenerator copyGenerateSlotsIn: self. "make 'em  all"
                ^ sel sendTo: self With: n
              ].
            ].

            generateUnimplementedPrimitiveNode: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: temporary registers\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         withTemporaryRegisterDo: blk = ( |
            | 
            temporaryRegisters allocateOneAndDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         zeroOop = ( |
            | layouts smi encode: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         temporaryRegisters.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: InitializeToExpression: (0)'
        
         verifiedEntryPointIndex <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         ppc = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes codeGenerators abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes codeGenerators ppc.

CopyDowns:
globals klein compiler1 parent prototypes codeGenerators abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: InitializeToExpression: (false)'
        
         haveStackFrame <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes codeGenerators ppc parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         add: r1 MaybeSetCCFrom: r2 To: dst = ( |
            | 
            a addTo: dst From: r1 With: r2.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         addImm: anInt MaybeSetCCFrom: from To: to = ( |
            | 
            a addiTo: to From: from With: anInt.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: saving and restoring the stack frame\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         addiToLRFrom: tempReg With: wordOffset = ( |
            | 
            wordOffset != 0  ifTrue: [
              a addiTo: tempReg
                  From: tempReg
                  With: wordOffset * oopSize
            ].
            a mtlrFrom: tempReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: saving and restoring the stack frame\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         addiToLRFromLRWith: wordOffset = ( |
            | 
            wordOffset != 0  ifTrue: [
              withTemporaryRegisterDo: [|:tr|
                a mflrTo: tr.
                addiToLRFrom: tr With: wordOffset
              ]
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         andImmMask: anInt AndSetCCFrom: fromReg To: toReg = ( |
            | 
            a andi_To: toReg From: fromReg With: anInt.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         andImmMask: mask DontSetCCFrom: fromReg To: toReg = ( |
            | 
            unimplemented).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         andImmMask: mask MaybeSetCCFrom: from To: to = ( |
            | 
            andImmMask: mask AndSetCCFrom: from To: to).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         andMask: maskReg AndSetCCFrom: fromReg To: toReg = ( |
            | 
            a and_To: toReg From: fromReg With: maskReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         branchEQLikelyTo: dst = ( |
            | 
            a beqTakenDisp: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         branchEQUnlikelyTo: dst = ( |
            | 
            a beqUntakenDisp: dst.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         branchNELikelyTo: dst = ( |
            | 
            a bneTakenDisp: dst.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         branchNEUnlikelyTo: dst = ( |
            | 
            a bneUntakenDisp: dst.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         branchToSendMessageStub = ( |
            | 
            withTemporaryRegisterDo: [|:tempReg|
              branchToStubName: 'sendMessage_stub'
                   UsingCTRAnd: tempReg
                       SetLink: false.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stubs\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         branchToStubName: stubName UsingCTRAnd: r SetLink: shouldSetLink = ( |
            | 
            generateEntryAddressOfStubNamed: stubName Into: r.
            usingCTRBranchToAddressIn: r SetLink: shouldSetLink.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedMethodStartInstruction.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         checkReceiverMap = ( |
            | 
            compiler noMapTest ifTrue: [^ self].

            generateExit: [|:okLabel|
              withTemporaryRegisterDo: [|:mapReg|
                layouts object generateMapOf: registerUsage outgoingReceiverRegister Into: mapReg With: self.
                withTemporaryRegisterDo: [|:previousMapReg|
                  generatePreviousMapInto: previousMapReg.
                  a cmpwFrom: mapReg With: previousMapReg.
                  branchEQLikelyTo: okLabel.
                ].
              ].
              branchToSendMessageStub.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         clearHigh: nBits BitsAndShiftLeftBy: shiftBits From: from To: to = ( |
            | 
            a clrlslwiTo: to From: from MaskBegin: nBits By: shiftBits).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         clearLow: nBits BitsFrom: fromReg To: toReg = ( |
            | 
            a clrrwiTo: toReg From: fromReg By: nBits.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: saving and restoring the stack frame\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         firstNonVolRegisterToSave = ( |
            | 
            gprFor: 32 - frame nonVolRegSaveAreaWordCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: branch bytecodes\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         genIndexedBranchTo: nodes IndexedBy: loc = ( |
            | 
            [todo optimization]. "Could make this more efficient."
            materializeSource: loc AndDo: [|:indexReg|
              nodes do: [|:n. :i|
                generateIf: indexReg EqualsImmediate: (layouts smi encode: i) ThenBranchTo: n label.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: backpatching\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateBackpatch: baseReg Offset: offset NewAddress: newAddressReg Temp1: t1 Temp2: t2 = ( |
            | 
            "high order 16 bits"
            loadValueAtOffset: offset * oopSize FromAddressInRegister: baseReg ToRegister: t1.
            clearLow: 16 BitsFrom: t1 To: t1.

            a srwiTo: t2 From: newAddressReg By: 16.

            orMask: t2 MaybeSetCCFrom: t1 To: t1.
            storeWordInRegister: t1 ToOffset: offset * oopSize FromAddressInRegister: baseReg.


            "low order 16 bits"
            loadValueAtOffset: offset succ * oopSize FromAddressInRegister: baseReg ToRegister: t1.
            clearLow: 16 BitsFrom: t1 To: t1.

            andImmMask: 16rffff MaybeSetCCFrom: newAddressReg To: t2.

            orMask: t2 MaybeSetCCFrom: t1 To: t1.
            storeWordInRegister: t1 ToOffset: offset succ * oopSize FromAddressInRegister: baseReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: block literals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateBlockLiteralNode: node = ( |
            | 
            generateIf: [|:notMemoizedYetLabel|
              materializeSource: node incomingMemoizedBlockValue location AndDo: [|:incomingMemoizedBlockReg|
                generateIf: incomingMemoizedBlockReg IsZeroThenBranchTo: notMemoizedYetLabel.
                moveRegister: incomingMemoizedBlockReg ToLocation: node outgoingMemoizedBlockValue location.
              ].
            ] Then: [
              setUpSendArguments: (node blockProtoLoc & sp) asVector.

              [cloneBlockHomeFrame_stub: fp].
              sendDesc generateCallStubName: 'cloneBlockHomeFrame_stub:'
                                  LookupKey: nil
                             LiveOopTracker: (liveOopTracker copyForNode: node)
                                       With: self.

              moveSendResultTo: node outgoingMemoizedBlockValue location.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fComment: Warning GC unsafe.
Returns an address into the caller\'s compiled code masquerading as a small integer.
-- jb 7/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCallingSendDescInto: dstReg = ( |
             offsetOfCallingPC.
            | 
            "calling pc is saved at calling sp + savedPCOffset"
            offsetOfCallingPC: frame totalWordCount + frame savedPCOffset.

                loadValueAtOffset: offsetOfCallingPC * oopSize
            FromAddressInRegister: sp
                       ToRegister: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: object table\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateChangeTagOf: r From: origTagValue To: newTagValue Into: dstReg = ( |
            | 
            addImm: newTagValue - origTagValue From: r To: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCloneLocation: srcLoc IntoLocation: dstLoc LiveOopTracker: aLiveOopTracker = ( |
            | 
            [todo gc]. "need red zone, to clone block for clone prim failure -- dmu"
            setUpSendArguments: vector copyAddFirst: srcLoc.

            [clone_stub].
            sendDesc generateCallStubName: 'clone_stub'
                                LookupKey: nil
                           LiveOopTracker: aLiveOopTracker
                                     With: self.

            moveSendResultTo: dstLoc.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: perform\x7fComment: Get the address of the sendDesc into
 a register, in order to be able to backpatch the selector and
 delegatee of the sendDesc. So we generate code that looks like this:
 (assuming a 32 bit system, where a intNN size is 4)

 N + 0: bl to next instruction                (so that N + 4 is in LR)
 N + 4: mflr to sendDescReg                   (so that N + 4 is in sendDescReg)
 N + 8: addi sendDescReg, sendDescReg, X - 4  (so that N + X is in sendDescReg)
        ...                                   (backpatching the selector and delegatee)
        ...                                   (setting up and executing the send)
 N + X: the sendDesc starts here

 Thus, we get N + X into the sendDescReg. But in order to figure out what X is,
 we need to actually generate the ... stuff. So we insert a placeholder addi instruction,
 generate the ... stuff, and then go back and insert the correct addi instruction.

 -- Adam Spitz 5/05\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateDynamicPerformNode: n = ( |
             lcAfterSendDesc.
             lcInSendDescReg.
             lcOfAddInstruction.
             lcOfSendDesc.
             nlr.
             sendDescReg.
            | 

            [n sendDescValue location isRegister] assert.
            sendDescReg: n sendDescValue location register.
            a blDisp: a locationCounter + a intNN size. "Get the address of the next instruction into the LR."
            lcInSendDescReg: a locationCounter.

            a mflrTo: sendDescReg.

            "Next instruction is just a placeholder; it will be patched later in this method."
            lcOfAddInstruction: a locationCounter.
            a addiTo: sendDescReg From: sendDescReg With: 0.

            backpatchSelectorAndDelegateeOfPerformNode: n In: sendDescReg.

            lcAfterSendDesc: generateSendNode: n.
            lcOfSendDesc: lcAfterSendDesc - (sendDesc size * oopSize).

            "patching of placeholder instruction:"
            a atLC: lcOfAddInstruction Do: [
              a addiTo: sendDescReg
                  From: sendDescReg
                  With: lcOfSendDesc - lcInSendDescReg. "(N + X) - (N + 4)"
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stubs\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateEntryAddressOfStubNamed: stubName Into: r = ( |
             or.
            | 
            or: vmKit relocators stub
              copyDstReg: r Offset: a locationCounter MethodNamed: stubName.
            compiler addRelocator: or.
            [sendDesc retryIndex]. "next instruction gets backpatched!"
            or assembleRealOrPlaceholderInstructionsWith: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: epilogue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateEpilogue: node = ( |
            | 
            generateEpilogue: node WordOffset: sendDesc normalReturnIndex).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: epilogue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateEpilogue: node WordOffset: wordOffset = ( |
            | 
            generateCleanupForMemoizedBlocks: node.
            [node sourceLevelAllocator isInlined not] assert.
            restoreFrameAndReturn: wordOffset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: backpatching\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateFlushMachineCachesFrom: startReg To: endReg = ( |
             oneMoreTime.
            | 

            "startReg will be modified."

            clearLow: 5 BitsFrom: startReg To: startReg.

            oneMoreTime: a defineLabel.
            a dcbfFrom: 0 With: startReg.
            a sync.
            a icbiFrom: 0 With: startReg.

            addImm: 32 From: startReg To: startReg.
            a cmplwFrom: startReg With: endReg.
            a bleUntakenDisp: oneMoreTime.

            a sync.
            a isync.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg DoesNotEqual: otherObjReg ThenBranchTo: trueFork = ( |
            | 
            a cmpwFrom: objReg With: otherObjReg.
            a bneDisp: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg DoesNotEqualImmediate: i ThenLikelyBranchTo: trueFork = ( |
            | 
            a cmpwiFrom: objReg With: i.
            branchNELikelyTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg DoesNotEqualImmediate: i ThenUnlikelyBranchTo: trueFork = ( |
            | 
            a cmpwiFrom: objReg With: i.
            branchNEUnlikelyTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Equals: otherObjReg ThenBranchTo: trueFork = ( |
            | 
            a cmpwFrom: objReg With: otherObjReg.
            a beqDisp: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg EqualsImmediate: i ThenBranchTo: trueFork = ( |
            | 
            a cmpwiFrom: objReg With: i.
            a beqDisp: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg EqualsImmediate: i ThenLikelyBranchTo: trueFork = ( |
            | 
            a cmpwiFrom: objReg With: i.
            branchEQLikelyTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg EqualsImmediate: i ThenUnlikelyBranchTo: trueFork = ( |
            | 
            a cmpwiFrom: objReg With: i.
            branchEQUnlikelyTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: smiReg IsBetweenZeroAnd: max ThenBranchTo: trueFork = ( |
            | 
            a cmplwiFrom: smiReg With:  layouts smi encode: max.
            a bleTakenDisp: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg IsNonZeroThenBranchTo: trueFork = ( |
            | 
            a cmpwiFrom: objReg With: zeroOop.
            branchNELikelyTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg IsZeroThenBranchTo: trueFork = ( |
            | 
            a cmpwiFrom: objReg With: zeroOop.
            branchEQUnlikelyTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIfIntegerOverflowDuring: aBlock ThenBranchTo: trueFork = ( |
            | 
            a mcrxrCrField: cr0.
            aBlock value.
            a bsoUntakenDisp: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIfTag: tagSmiReg IsMark: markBlock IsInvalidTag: invalidBlock = ( |
            | 
            generateIf: [|:trueFork. :endFork|
              a cmpwiFrom: tagSmiReg
                        With: layouts smi encode: vmKit tag mark.
              a bltTakenDisp: endFork.
              a beqDisp: trueFork.
              invalidBlock value.
            ] Then: [
              markBlock value.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIfUnsigned: reg1 IsLessThan: reg2 ThenLikelyBranchTo: ltLabel = ( |
            | 
            a cmplwFrom: reg1 With: reg2.
            a bltTakenDisp: ltLabel.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: conditionals\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIfUnsigned: reg1 IsNotLessThan: reg2 ThenUnlikelyBranchTo: tooBig = ( |
            | 
            a cmplwFrom: reg1 With: reg2.
            a bgeUntakenDisp: tooBig.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateInitializeUnnamedValuesForGC = ( |
             locs.
             sla.
            | 
            locs: set copyRemoveAll.
            machineLevelAllocator allValues do: [|:v| (locMightNeedToBeInitializedForGC: v location) ifTrue: [locs add: v location]].
            sla: machineLevelAllocator topSourceLevelAllocator.
            sla incomingRcvrAndArgValues do: [|:v| locs remove: v location IfAbsent: []].
            sla              namedValues do: [|:v| locs remove: v location IfAbsent: []].

            locs isEmpty ifTrue: [^ self].
            a liTo: r0 With: 0.
            locs do: [|:loc| moveRegister: r0 ToLocation: loc].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateInt32PrimitiveInto: dstReg Rcvr: rcvrReg Arg1: i1Reg Arg2: i2Reg IfFail: fh Do: aBlock = ( |
            | 
            fh assertInt32: i1Reg.
            fh assertInt32: i2Reg.

            moveValueOfInt32: i1Reg ToRegister: dstReg IfFail: fh.
            withTemporaryRegisterDo: [|:tempReg|
              moveValueOfInt32: i2Reg ToRegister: tempReg IfFail: fh.
              aBlock value: dstReg With: dstReg With: tempReg.
            ].

            moveValueInRegister: dstReg ToSMIOrInt32: rcvrReg AndPutResultInto: dstReg IfFail: fh.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fComment: Must return the instruction\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateMethodStartInstruction = ( |
             r.
            | 
            [a bDisp: a locationCounter + a intNN size].
            "Use a less common instruction because the codeGenerator
             sometimes issues branches of this form, here we
             issue an unconditional branch with the prediction bit
             set (will be ignored). -- jb 8/03"
            r: int32 fromHigh16Bits: 16r42a0 Low16Bits: 16r0004.
            a data32: r.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: saving and restoring incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateMoveIncomingReceiverAndArgumentsToNonVolLocations = ( |
            | 
            "could use stswi someday"

            [aaaaaaa].
            [
            compiler slot name = 'if:Then:If:Then:If:Then:Else:' ifTrue: [
              breakpoint: 'let\'s step through'.
              halt.
            ].
            ].

            topSourceLevelAllocator incomingVolatileRegRcvrAndArgLocationsToSaveDo: [|:vol. :nonVolReg. :nonVolMem|
              nonVolReg ifNotNil: [moveLocation: vol ToLocation: nonVolReg].
              nonVolMem ifNotNil: [moveLocation: vol ToLocation: nonVolMem].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateMoveNMethodToStack = ( |
            | 
            withTemporaryRegisterDo: [|:r|
              loadNMethodIntoRegister: r.
              storeWordInRegister: r ToOffset: frame nmethodOffset * oopSize FromAddressInRegister: sp.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: epilogue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateNLRHomeScopeFor: node Into: dstLoc = ( |
             sla.
            | 
            [node sourceLevelAllocator context slot contents isReflecteeBlockMethod] assert.

            sla: node sourceLevelAllocator.
            withStackPointerForLexicalLevel: sla lexicalParentCount StartingFrom: sla ReceiverValue: node blockReceiverValue Do: [|:sp|
              moveRegister: sp ToLocation: dstLoc.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: epilogue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateNLRPointEpilogue: node = ( |
            | 
            "We could be more clever about choosing which blocks
             must be zapped if there are no backwards branching
             bytecodes.  For now, all NLRs are handled the same.
             Also, we could fold the 'normal return' code into the
             epilogue code because they are identical.
             -- jb 8/03"

            node sourceLevelAllocator isInlined ifFalse: [
              generateCleanupForMemoizedBlocks: node.
            ].

            [aaaaaaa]. "I don't think this is always necessary, but I'm really confused."
            moveLocation: node outgoingResultValue location ToLocation: machineLevelAllocator locationForOutgoingResult.

            node sourceLevelAllocator context isForABlockMethod ifTrue: [
              "Optimization: block methods can't be the home scope of an NLR, so no point doing the checks."
              node sourceLevelAllocator isInlined ifFalse: [
                restoreFrameAndReturn: sendDesc nlrReturnIndex
              ] True: [
                genBranchTo: node nodeToBranchToOnNLR.
              ].
            ] False: [
              generateIf: [|:trueFork|
                generateIf: machineLevelAllocator locationForIncomingNLRHomeScope DoesNotEqual: sp ThenBranchTo: trueFork.

                [aaaaaaa]. "Possible optimization: this check isn't necessary if the topScope has no inlinedScopes, right?"
                withTemporaryRegisterDo: [|:scopeDescReg|
                  loadOop: node interpreter myScopeDesc IntoRegister: scopeDescReg NameForComment: 'NLR home scopeDesc'.
                  generateIf: machineLevelAllocator locationForIncomingNLRHomeScopeDesc DoesNotEqual: scopeDescReg ThenBranchTo: trueFork.
                ].
              ] Then: [
                node sourceLevelAllocator isInlined ifFalse: [
                  restoreFrameAndReturn: sendDesc nlrReturnIndex
                ] True: [
                  genBranchTo: node nodeToBranchToOnNLR.
                ].
              ] Else: [
                node sourceLevelAllocator isInlined ifFalse: [
                  restoreFrameAndReturn: sendDesc normalReturnIndex
                ] True: [| localReturnNode |
                  genBranchTo: node nodeToBranchToOnLR.
                ]
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generatePreviousMapInto: dstReg = ( |
            | 
            a mflrTo: dstReg. "generateCallingSendDescInto: doesn't work here, because sometimes we're
                               in a leaf method, and even when we're not, we don't want to add
                               totalWordCount. -- Adam, 5/04"
            moveLocation: (locationForIndex: sendDesc previousMapIndex InSendDesc: dstReg)
              ToRegister: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fComment: Warning GC unsafe.
-- jb 7/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: sdReg BackpatchSendDescTo: targetReg Map: mapReg IfFail: fh = ( |
            | 
            withTemporaryRegisterDo: [|:tempReg|
              sendDesc generateBackpatchAndFlushMachineCaches: sdReg
                                                     TargetTo: targetReg
                                                          Map: mapReg
                                                        Temp1: dstReg
                                                        Temp2: tempReg
                                                         With: self.
              moveRegister: sdReg ToRegister: dstReg.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fComment: Warning GC unsafe.
-- jb 7/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: sdReg BackpatchSendDescTo: targetReg PNMC: pnmcReg IfFail: fh = ( |
            | 
            withTemporaryRegisterDo: [|:tempReg|
              sendDesc generateBackpatchAndFlushMachineCaches: sdReg
                                                     TargetTo: targetReg
                                                         PNMC: pnmcReg
                                                        Temp1: dstReg
                                                        Temp2: tempReg
                                                         With: self.
              moveRegister: sdReg ToRegister: dstReg.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: nmethods\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: nmReg BranchToIndex: indexReg IfFail: fh = ( |
            | 
            fh assertNMethod: nmReg.

            [verifiedEntryPointIndex]. "browsing"

            withTemporaryRegisterDo: [|:addrReg|
              byteVectorLayout
                generateFor:            nmReg
                  AddressOfIndexableAt: indexReg
                                  Into: addrReg
                                  With: self.

              generateRestoreIncomingReceiverAndArgumentsFromNonVolRegisters.
              haveStackFrame ifTrue: [restoreFrame: 0].
              usingCTRBranchToAddressIn: addrReg SetLink: false.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fComment: Warning GC unsafe.
Returns an address into the caller\'s compiled code masquerading as a small integer.
-- jb 7/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg CallingSendDescIfFail: fh = ( |
            | 
            generateCallingSendDescInto: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: object table\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg ConvertInvalidObjectLocatorEntryToOIDIfFail: fh = ( |
            | 
            fh assertFloat: rcvrReg.
            generateChangeTagOf: rcvrReg From: vmKit tag float To: vmKit tag smi Into: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: object table\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg ConvertOIDToInvalidObjectLocatorEntryIfFail: fh = ( |
            | 
            fh assertInteger: rcvrReg.
            generateChangeTagOf: rcvrReg From: vmKit tag smi To: vmKit tag float Into: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg EntryAddressOfCompileStubIfFail: fh = ( |
            | 
            [compile_stub]. "browsing"
            generateEntryAddressOfStubNamed: 'compile_stub' Into: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg EntryAddressOfCreatePNMCStubIfFail: fh = ( |
            | 
            [createPolymorphicNMethodCache_stub]. "browsing"
            generateEntryAddressOfStubNamed: 'createPolymorphicNMethodCache_stub' Into: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg EntryAddressOfNMethodLookupStubIfFail: fh = ( |
            | 
            [nmethodCacheLookup_stub]. "browsing"
            generateEntryAddressOfStubNamed: 'nmethodCacheLookup_stub' Into: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg EntryAddressOfSendMessageStubIfFail: fh = ( |
            | 
            [aaaaaaa]. "Do I really want a separate primitive like this for each stub?"
            [sendMessage_stub]. "browsing"
            generateEntryAddressOfStubNamed: 'sendMessage_stub' Into: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: nmethods\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: nmReg FlushMachineCachesFrom: startIndexReg To: endIndexReg IfFail: fh = ( |
            | 
            fh assertByteVector: nmReg.
            fh assertInteger: startIndexReg.
            fh assertInteger: endIndexReg.
            withTemporaryRegisterDo: [|:startAddressReg|
              byteVectorLayout generateFor: nmReg AddressOfIndexableAt: startIndexReg Into: startAddressReg With: self.
              byteVectorLayout generateFor: nmReg AddressOfIndexableAt:   endIndexReg Into:          dstReg With: self.
              generateFlushMachineCachesFrom: startAddressReg To: dstReg.
            ].
            moveRegister: nmReg ToRegister: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: immediates\x7fCategory: small integers\x7fCategory: logical operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg IntAnd: argReg IfFail: fh = ( |
            | 
            fh assertInteger: rcvrReg.
            fh assertInteger: argReg.
            a and_To: dstReg From: rcvrReg With: argReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: immediates\x7fCategory: small integers\x7fCategory: bit shifts\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg IntArithmeticShiftLeft: argReg IfFail: fh = ( |
            | 
            fh assertInteger: rcvrReg.
            fh assertInteger: argReg.
            layouts smi generateDecode: argReg Into: dstReg With: self.
            fh assertNoOverflow: 'arithmetic shift left' During: [
              a slw_To: dstReg From: rcvrReg With: dstReg.
              withTemporaryRegisterDo: [|:tempReg|
                a xor_To: tempReg From: rcvrReg With: dstReg.
                a bgeTakenDisp: fh endLabel.
                fh overflowError: 'arithmetic shift left'.
              ].
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: immediates\x7fCategory: small integers\x7fCategory: bit shifts\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg IntArithmeticShiftRight: argReg IfFail: fh = ( |
            | 
            fh assertInteger: rcvrReg.
            fh assertInteger: argReg.

            layouts smi generateDecode: argReg Into: dstReg With: self.

            "note: arithmetic shift right cannot overflow"
            a srawTo: dstReg From: rcvrReg With: dstReg.

            "clear out tag bits"
            layouts object generateValueOf: dstReg Into: dstReg With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: immediates\x7fCategory: small integers\x7fCategory: bit shifts\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg IntLogicalShiftLeft: argReg IfFail: fh = ( |
            | 
            fh assertInteger: rcvrReg.
            fh assertInteger: argReg.
            layouts smi generateDecode: argReg Into: dstReg With: self.
            a slw_To: dstReg From: rcvrReg With: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: immediates\x7fCategory: small integers\x7fCategory: bit shifts\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg IntLogicalShiftRight: argReg IfFail: fh = ( |
            | 
            fh assertInteger: rcvrReg.
            fh assertInteger: argReg.

            layouts smi generateDecode: argReg Into: dstReg With: self.

            "note: logical shift right cannot overflow"
            a srwTo: dstReg From: rcvrReg With: dstReg.

            "clear out tag bits"
            layouts object generateValueOf: dstReg Into: dstReg With: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: immediates\x7fCategory: small integers\x7fCategory: arithmetic\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg IntMod: argReg IfFail: fh = ( |
            | 
            [aaaaaaa]. "Make this, and the rest of them, into translated prims."
            fh assertInteger: rcvrReg.
            fh assertInteger: argReg.

            withTemporaryRegisterDo: [|:temp1Reg|
              withTemporaryRegisterDo: [|:temp2Reg|
                layouts smi generateDecode: rcvrReg Into: temp1Reg With: self.
                layouts smi generateDecode:  argReg Into: temp2Reg With: self.

                fh assertNoOverflow: 'mod' During: [
                  a divwo_To: dstReg From: temp1Reg With: temp2Reg.
                ].
                a  mullwTo: dstReg From:   dstReg With: temp2Reg.
                a    subTo: dstReg From: temp1Reg With:   dstReg.

                layouts smi generateEncode:  dstReg Into: dstReg With: self.
              ].
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: immediates\x7fCategory: small integers\x7fCategory: logical operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg IntOr: argReg IfFail: fh = ( |
            | 
            fh assertInteger: rcvrReg.
            fh assertInteger: argReg.
            a or_To: dstReg From: rcvrReg With: argReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: immediates\x7fCategory: small integers\x7fCategory: logical operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg IntXor: argReg IfFail: fh = ( |
            | 
            fh assertInteger: rcvrReg.
            fh assertInteger: argReg.
            a xor_To: dstReg From: rcvrReg With: argReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg SetFromInt32: i1Reg Add: i2Reg IfFail: fh = ( |
            | 
            generateInt32PrimitiveInto: dstReg Rcvr: rcvrReg Arg1: i1Reg Arg2: i2Reg IfFail: fh
              Do: [|:dstReg. :reg1. :reg2| a add_To: dstReg From: reg1 With: reg2]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg SetFromInt32: i1Reg And: i2Reg IfFail: fh = ( |
            | 
            generateInt32PrimitiveInto: dstReg Rcvr: rcvrReg Arg1: i1Reg Arg2: i2Reg IfFail: fh
              Do: [|:dstReg. :reg1. :reg2| a and_To: dstReg From: reg1 With: reg2]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg SetFromInt32: i1Reg Cmp: i2Reg IfFail: fh = ( |
            | 
            generateInt32PrimitiveInto: dstReg Rcvr: rcvrReg Arg1: i1Reg Arg2: i2Reg IfFail: fh
              Do: [|:dstReg. :reg1. :reg2|
                generateIf: [|:ltFork|
                  a cmpwFrom: reg1
                        With: reg2.
                  a bltDisp: ltFork.
                ] Then: [
                  a liTo: dstReg With: -1.
                ] Else: [
                  generateIf: [|:gtFork|
                    a bgtDisp: gtFork.
                  ] Then: [
                    a liTo: dstReg With: 1.
                  ] Else: [
                    a liTo: dstReg With: 0.
                  ].
                ].
              ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg SetFromInt32: i1Reg Div: i2Reg IfFail: fh = ( |
            | 
            generateInt32PrimitiveInto: dstReg Rcvr: rcvrReg Arg1: i1Reg Arg2: i2Reg IfFail: fh
              Do: [|:dstReg. :reg1. :reg2|
                     fh assertNoOverflow: 'division' During: [
                       a divwo_To: dstReg From: reg1 With: reg2.
                     ].
                  ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg SetFromInt32: i1Reg Mul: i2Reg IfFail: fh = ( |
            | 
            generateInt32PrimitiveInto: dstReg Rcvr: rcvrReg Arg1: i1Reg Arg2: i2Reg IfFail: fh
              Do: [|:dstReg. :reg1. :reg2| a mullw_To: dstReg From: reg1 With: reg2]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg SetFromInt32: i1Reg Or: i2Reg IfFail: fh = ( |
            | 
            generateInt32PrimitiveInto: dstReg Rcvr: rcvrReg Arg1: i1Reg Arg2: i2Reg IfFail: fh
              Do: [|:dstReg. :reg1. :reg2| a or_To: dstReg From: reg1 With: reg2]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg SetFromInt32: i1Reg Rem: i2Reg IfFail: fh = ( |
            | 
            generateInt32PrimitiveInto: dstReg Rcvr: rcvrReg Arg1: i1Reg Arg2: i2Reg IfFail: fh
              Do: [|:dstReg. :reg1. :reg2|
                    fh assertNoOverflow: 'rem' During: [
                      a divwo_To: dstReg From: reg1 With: reg2.
                    ].
                    a  mullwTo: dstReg From: dstReg With: reg2.
                    a    subTo: dstReg From: reg1   With: dstReg.
                  ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg SetFromInt32: i1Reg Shl: i2Reg IfFail: fh = ( |
            | 
            generateInt32PrimitiveInto: dstReg Rcvr: rcvrReg Arg1: i1Reg Arg2: i2Reg IfFail: fh
              Do: [|:dstReg. :reg1. :reg2| a slw_To: dstReg From: reg1 With: reg2]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg SetFromInt32: i1Reg Shr: i2Reg IfFail: fh = ( |
            | 
            generateInt32PrimitiveInto: dstReg Rcvr: rcvrReg Arg1: i1Reg Arg2: i2Reg IfFail: fh
              Do: [|:dstReg. :reg1. :reg2| a sraw_To: dstReg From: reg1 With: reg2]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg SetFromInt32: i1Reg Sub: i2Reg IfFail: fh = ( |
            | 
            generateInt32PrimitiveInto: dstReg Rcvr: rcvrReg Arg1: i1Reg Arg2: i2Reg IfFail: fh
              Do: [|:dstReg. :reg1. :reg2| a sub_To: dstReg From: reg1 With: reg2]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg SetFromInt32: i1Reg Ushr: i2Reg IfFail: fh = ( |
            | 
            generateInt32PrimitiveInto: dstReg Rcvr: rcvrReg Arg1: i1Reg Arg2: i2Reg IfFail: fh
              Do: [|:dstReg. :reg1. :reg2| a srw_To: dstReg From: reg1 With: reg2]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg SetFromInt32: i1Reg Xor: i2Reg IfFail: fh = ( |
            | 
            generateInt32PrimitiveInto: dstReg Rcvr: rcvrReg Arg1: i1Reg Arg2: i2Reg IfFail: fh
              Do: [|:dstReg. :reg1. :reg2| a xor_To: dstReg From: reg1 With: reg2]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg SetInt32FromStackPointerIfFail: fh = ( |
            | 
            moveValueInRegister: sp ToSMIOrInt32: rcvrReg AndPutResultInto: dstReg IfFail: fh.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: unusedRcvrReg UnsafeIsMarkAtOffset: wordOffsetSmiReg FromAddress: baseAddrReg IfFail: fh = ( |
            | 
            fh assertInt32: baseAddrReg.
            fh assertInteger: wordOffsetSmiReg.
            moveValueOfInt32: baseAddrReg ToRegister: dstReg IfFail: fh.
            [vmKit tag smi  = 0] assert.
            [vmKit tag size = 2] assert.
            withTemporaryRegisterDo: [|:tempReg|
              loadWordAt: dstReg IndexedBy: wordOffsetSmiReg To: tempReg.
              vmKit layouts object generateIsMark: tempReg Into: dstReg With: self.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg UnsafeMarkValueAtAddressIfFail: fh = ( |
            | 
            fh assertInt32: rcvrReg.
            withTemporaryRegisterDo: [|:addrReg|
              moveValueOfInt32: rcvrReg ToRegister: addrReg IfFail: fh.
              loadValueAtOffset: 0 FromAddressInRegister: addrReg ToRegister: dstReg.
            ].
            layouts mark generateValueOfMark: dstReg Into: dstReg With: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg UnsafeOIDOfMarkAtAddressIfFail: fh = ( |
            | 
            fh assertInt32: rcvrReg.
            withTemporaryRegisterDo: [|:addrReg|
              moveValueOfInt32: rcvrReg ToRegister: addrReg IfFail: fh.
              loadValueAtOffset: 0 FromAddressInRegister: addrReg ToRegister: dstReg.
            ].
            layouts mark generateOIDOfMark: dstReg Into: dstReg With: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: unusedRcvrReg UnsafeObjectForOopAtAddress: addrReg IfFail: fh = ( |
            | 
            fh assertInt32: addrReg.
            moveValueOfInt32: addrReg ToRegister: dstReg IfFail: fh.
               load32BitsAtOffset: 0
            FromAddressInRegister: dstReg
                       ToRegister: dstReg.
            fh assertNotMarkInDstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: unusedRcvrReg UnsafeObjectForOopAtOffset: wordOffsetSmiReg FromAddress: baseAddrReg IfFail: fh = ( |
            | 
            fh assertInt32: baseAddrReg.
            fh assertInteger: wordOffsetSmiReg.
            moveValueOfInt32: baseAddrReg ToRegister: dstReg IfFail: fh.
            [vmKit tag smi  = 0] assert.
            [vmKit tag size = 2] assert.
            loadWordAt: dstReg
             IndexedBy: wordOffsetSmiReg
                    To: dstReg.
            fh assertNotMarkInDstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: mvReg UnsafePutMarkValueAtOffset: wordOffsetSmiReg FromAddress: baseAddrReg IfFail: fh = ( |
            | 
            fh assertInt32: baseAddrReg.
            fh assertInteger: wordOffsetSmiReg.
            moveValueOfInt32: baseAddrReg ToRegister: dstReg IfFail: fh.
            [vmKit tag smi  = 0] assert.
            [vmKit tag size = 2] assert.
            withTemporaryRegisterDo: [|:markReg|
              layouts mark generateAddTagTo: mvReg Into: markReg With: self.
              vmKit layouts memoryObject generateForObjectAtAddress: dstReg At: wordOffsetSmiReg Put: markReg With: self.
            ].
            moveRegister: mvReg ToRegister: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: objReg UnsafePutOopAtAddress: addrReg IfFail: fh = ( |
            | 
            fh assertInt32: addrReg.
            moveValueOfInt32: addrReg ToRegister: dstReg IfFail: fh.
            storeWordInRegister: objReg ToOffset: 0 FromAddressInRegister: dstReg.
            moveRegister: objReg ToRegister: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: objReg UnsafePutOopAtOffset: wordOffsetSmiReg FromAddress: baseAddrReg IfFail: fh = ( |
            | 
            fh assertInt32: baseAddrReg.
            fh assertInteger: wordOffsetSmiReg.
            moveValueOfInt32: baseAddrReg ToRegister: dstReg IfFail: fh.
            [vmKit tag smi  = 0] assert.
            [vmKit tag size = 2] assert.
            vmKit layouts memoryObject generateForObjectAtAddress: dstReg At: wordOffsetSmiReg Put: objReg With: self.
            moveRegister: objReg ToRegister: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg UnsafePutWordAtAddress: addrReg IfFail: fh = ( |
            | 
            fh assertInt32: rcvrReg.
            fh assertInt32: addrReg.
            withTemporaryRegisterDo: [|:tempReg|
              moveValueOfInt32: addrReg ToRegister: tempReg IfFail: fh.
              moveValueOfInt32: rcvrReg ToRegister:  dstReg IfFail: fh.
              storeWordInRegister: dstReg ToOffset: 0 FromAddressInRegister: tempReg.
            ].
            moveRegister: rcvrReg ToRegister: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: unusedRcvrReg UnsafeTagOfOopAtAddress: addrReg IfFail: fh = ( |
            | 
            fh assertInt32: addrReg.
            withTemporaryRegisterDo: [|:rawAddrReg|
              moveValueOfInt32: addrReg ToRegister: rawAddrReg IfFail: fh.
              loadValueAtOffset: 0 FromAddressInRegister: rawAddrReg ToRegister: dstReg.
              layouts object generateTagOf: dstReg Into: dstReg With: self.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: int32Reg UnsafeWordAtAddress: addrReg IfFail: fh = ( |
            | 
            fh assertInt32: addrReg.
            withTemporaryRegisterDo: [|:tempReg|
              moveValueOfInt32: addrReg ToRegister: tempReg IfFail: fh.
                 load32BitsAtOffset: 0
              FromAddressInRegister: tempReg
                         ToRegister: tempReg.
              moveValueInRegister: tempReg ToSMIOrInt32: int32Reg AndPutResultInto: dstReg IfFail: fh.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitiveInto: dstReg Receiver: rcvrReg UnsafeWriteTrailingMarkIfFail: fh = ( |
            | 
            fh assertInt32: rcvrReg.
            withTemporaryRegisterDo: [|:addrReg|
              moveValueOfInt32: rcvrReg ToRegister: addrReg IfFail: fh.
              withTemporaryRegisterDo: [|:markReg|
                layouts mark generateLoadTrailingMarkInto: markReg With: self.
                moveRegister: markReg
                  ToLocation: locations offsetFromOtherLocation copyForOffset: 0 FromRegister: addrReg.
              ].
            ].
            moveRegister: rcvrReg ToRegister: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_CloneIfFail_: node = ( |
            | 
            [_Clone          ]. "browsing"
            [_CloneIfFail: fb]. "browsing"
            generateCloneLocation: node rcvrLoc
                     IntoLocation: node resultLoc
                   LiveOopTracker: liveOopTracker copyForNode: node).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fComment: Warning GC unsafe.
-- jb 7/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_RetrySendDescIfFail_: node = ( |
            | 
            [_RetrySendDesc]. "browsing"
            [_RetrySendDescIfFail: fb]. "browsing"
            generateRestoreIncomingReceiverAndArgumentsFromNonVolRegisters.
            restoreFrameAndReturn: sendDesc retryIndex.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_SendDescDelegateeIfFail_: node = ( |
            | 
            [_SendDescDelegatee          ]. "browsing"
            [_SendDescDelegateeIfFail: fb]. "browsing"
            generateSendDescOopAtIndex: sendDesc delegateeIndex ForNode: node).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_SendDescLookupTypeIfFail_: node = ( |
            | 
            [_SendDescLookupType          ]. "browsing"
            [_SendDescLookupTypeIfFail: fb]. "browsing"
            generateSendDescOopAtIndex: sendDesc lookupTypeIndex ForNode: node).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fComment: Warning GC unsafe.
-- jb 7/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_SendDescNMethodCacheIfFail_: node = ( |
            | 
            [_SendDescNMethodCache          ]. "browsing"
            [_SendDescNMethodCacheIfFail: fb]. "browsing"
            generateSendDescOopAtIndex: sendDesc nmethodCacheIndex ForNode: node).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fComment: Warning GC unsafe.
-- jb 7/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_SendDescPreviousMapIfFail_: node = ( |
            | 
            [_SendDescPreviousMap          ]. "browsing"
            [_SendDescPreviousMapIfFail: fb]. "browsing"
            generateSendDescOopAtIndex: sendDesc previousMapIndex ForNode: node).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fComment: Warning GC unsafe.
-- jb 7/03\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrimitive_SendDescSelectorIfFail_: node = ( |
            | 
            [_SendDescSelector          ]. "browsing"
            [_SendDescSelectorIfFail: fb]. "browsing"
            generateSendDescOopAtIndex: sendDesc selectorIndex ForNode: node).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fComment: From codeGen_ppc.cpp prologue()     PPC nic:
 /*
    ; save PC link
    
    mflr    r0
    stw      r0, LinkageArea.savedPC(sp); save PC link (pc to return to)
  */
  /*      
    stmw    r28, -reg_save_len(sp) ; save 4 nonvol regs
    
    ; save non volatile int regs
    
    li      scr_a, 8 + (LinkageArea.size + reg_save_len); 2 words of args, in bytes = 8 bytes
    sub     scr_a,  sp, scr_a       ; scr_a now has sp - arg length - linkage area len - reg_save_len
    li      r0, 15
    andc    scr_a, scr_a, r0  ; round down
    stw     sp, 0(scr_a)  ; save soon-to-be-old sp
    mr      sp, scr_a ; setup sp for new frame
  */
  
  // *if not DI child
  //    <smi/float/memOop prologue>
  // _verified:                       (entry point from PICs)
  //    if necessary <check selector>
  //    if necessary <check delegatee>
  // *endif DI
  
  // _diCheck:                        (entry point after recompile)
  //    <verify assignable parents>
  
  // *if using recompilation
  //    <checkRecompilation>
  // *endif
  
  // *if haveStackFrame
  //    save sp, -frameSize*oopSize, sp
  // *endif
  
  // <flush register windows if neceessary>
  // <clear stack temporaries and excess argument locations

  // CAUTION: use only Temp1/4 for temps in prologue; other temps
  // may contain lookup parameters.
\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generatePrologue: startNode = ( |
            | 
            generating: 'generateMethodStartInstruction' During: [generateMethodStartInstruction].
            generating: 'checkReceiverMap'               During: [checkReceiverMap].
            generating: 'generateVerifiedEntryPoint'     During: [generateVerifiedEntryPoint].
            generating: 'generateAssignableParentsCheck' During: [generateAssignableParentsCheck].
            generating: 'checkForRecompilation'          During: [checkForRecompilation].
            machineLevelAllocator isLeafMethod  ifFalse: [
              generating: 'generateStackFramePrologue'   During: [generateStackFramePrologue].
            ].
            [generateLRUCodeIfNotAccessMethodAndNotRecomp. todo dynamicCompilation "LRU"].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: saving and restoring incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateRestoreIncomingReceiverAndArgumentsFromNonVolRegisters = ( |
            | 
            topSourceLevelAllocator incomingVolatileRegRcvrAndArgLocationsDo: [|:vol. :nonVol|
              moveLocation: nonVol ToLocation: vol.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSendDescOopAtIndex: i ForNode: node = ( |
            | 
            "rcvr is unencoded addr of sendDesc"
            materializeLocsOf: node AndDo: [|:dstReg. :sendDescReg|
              moveLocation: (locationForIndex: i InSendDesc: sendDescReg)
                ToRegister: dstReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateStackFramePrologue = ( |
            | 
            comment: 'stack frame prologue'.

            haveStackFrame: true.
            saveFrame.
            generateMoveIncomingReceiverAndArgumentsToNonVolLocations.
            generateMoveNMethodToStack.
            generateInitializeUnnamedValuesForGC.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: system calls\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateTrapInstructionForSystemCall: systemCallNumber = ( |
            | 
            a liTo: r0 With: systemCallNumber.
            a sc.
            a trap. "error handling, not done yet" [todo systemCallFailure].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: perform\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         invalidateInlineCache: sendDescReg = ( |
            | 
            moveLocation: (locations constant copyForOop: 0)
              ToLocation: locationForIndex: sendDesc previousMapIndex InSendDesc: sendDescReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         load32BitsAtOffset: o FromAddressInRegister: addressReg ToRegister: dstReg = ( |
            | 
                loadValueAtOffset: o
            FromAddressInRegister: addressReg
                       ToRegister: dstReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         loadByteAt: addr IndexedBy: indexReg To: dst = ( |
            | 
            a lbzxTo: dst From: addr With: indexReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadImmediateMap: immMap IntoRegister: dstMapReg = ( |
            | 
            loadOop: immMap IntoRegister: dstMapReg NameForComment: 'klein maps ', immMap mapType).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: loading\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadIncomingMemoryArgumentLocation: loc InFrame: f AtSP: sp ToRegister: r = ( |
            | 
            false ifTrue: [
              todo optimization lexicalParentFrameSize.
              loadValueAtOffset: (loc spOffsetFor: self InFrame: f) FromAddressInRegister: sp ToRegister: r.
            ]
            False: [
              loadValueAtOffset: oopSize * f savedSPOffset                          FromAddressInRegister: sp ToRegister: r.
              loadValueAtOffset: (spOffsetOfOutgoingMemoryArgument: loc InFrame: f) FromAddressInRegister: r  ToRegister: r.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: loading\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadIncomingMemoryArgumentLocation: loc ToRegister: r = ( |
            | 
            withStackPointerForMemoryLocation: loc Do: [|:sp. :f|
              loadIncomingMemoryArgumentLocation: loc InFrame: f AtSP: sp ToRegister: r.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: loading\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadNonVolMemoryLocalLocation: loc InFrame: f AtSP: sp ToRegister: r = ( |
            | 
            loadValueAtOffset: (loc spOffsetFor: self InFrame: f) FromAddressInRegister: sp ToRegister: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: loading\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadNonVolMemoryLocalLocation: loc ToRegister: r = ( |
            | 
            withStackPointerForMemoryLocation: loc Do: [|:sp. :f|
              loadNonVolMemoryLocalLocation: loc InFrame: f AtSP: sp ToRegister: r.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadOop: oop IntoRegister: r = ( |
            | 
            "Because getting the reflective name of an object is slow,
             you may want to just call loadOop:IntoRegister:NameForComment:
             directly instead of calling this method. -- Adam & Alex, 4/04"

            loadOop: oop IntoRegister: r NameForComment: [(reflect: oop) name]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadOop: oop IntoRegister: r NameForComment: nameOrBlock = ( |
            | 
            "Because getting the reflective name of an object
             is slow, you may want to just call this method
             instead of loadOop:IntoRegister:. -- Adam & Alex, 4/04"

                   loadOop: oop
              IntoRegister: r
            NameForComment: nameOrBlock
            RelocatorProto: vmKit relocators loadAddress).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: embedding & loading oops\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadOop: oop IntoRegister: r NameForComment: nameOrBlock RelocatorProto: relocatorProto = ( |
             or.
            | 
            ifReflecteeOf: (reflect: oop) IsImmediateThenDoWithEncoding: [|:encoding|
             ^ a load32To: r From: encoding
            ].
            comment: 'loadOop: ', nameOrBlock value, ' IntoRegister: ', r name.
            or:  relocatorProto copyDstReg: r Offset: a locationCounter Object: oop.
            compiler addRelocator: or.
            or assembleRealOrPlaceholderInstructionsWith: self.
            or).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: loading\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadOutgoingMemoryArgumentLocation: loc InFrame: f AtSP: sp ToRegister: r = ( |
            | 
            loadValueAtOffset: (loc spOffsetFor: self InFrame: f) FromAddressInRegister: sp ToRegister: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: loading\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         loadOutgoingMemoryArgumentLocation: loc ToRegister: r = ( |
            | 
            withStackPointerForMemoryLocation: loc Do: [|:sp. :f|
              loadOutgoingMemoryArgumentLocation: loc InFrame: f AtSP: sp ToRegister: r.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         loadValueAtOffset: o FromAddressInRegister: addressReg ToRegister: dstReg = ( |
            | 
            a lwzTo: dstReg Disp: o Base: addressReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         loadWordAt: addr IndexedBy: index To: dst = ( |
            | 
            a lwzxTo: dst From: addr With: index.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: prologue\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         locMightNeedToBeInitializedForGC: loc = ( |
            | 
            loc isConstant ifTrue: [^ false].
            loc isRegister && [(registerUsage isLocalNonVolRegister: loc) not] ifTrue: [^ false].
            loc locationTypeName = 'incomingMemoryArgument' ifTrue: [^ false].
            loc locationTypeName = 'outgoingMemoryArgument' ifTrue: [^ false].
            loc isRegister ifTrue: [^ true].
            [loc locationTypeName = 'nonVolMemoryLocal'] assert.
            loc lexicalLevel > 0 ifTrue: [^ false].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: send descs\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForIndex: i InSendDesc: sendDescReg = ( |
            | 
            locations offsetFromOtherLocation copyForOffset: i * oopSize
                                               FromRegister: sendDescReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         moveRegister: rs ToRegister: rd = ( |
            | 
            rs = rd ifTrue: [^ self].
            a mrTo: rd From: rs.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: system calls\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         moveSystemCallResultToReg: dstReg = ( |
            | 
            moveRegister: r3 ToRegister: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         moveValueInRegister: valueReg ToInt32: int32Reg Temp: tempReg = ( |
            | 
            [valueReg != tempReg] assert.

            byteVectorLayout
                               generateFor: int32Reg
               AddressOfFirstIndexableInto: tempReg
                                      With: self.

               storeWordInRegister: valueReg
                          ToOffset: 0
             FromAddressInRegister: tempReg.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         moveValueInRegister: srcReg ToSMIOrInt32: int32Reg AndPutResultInto: dstReg IfFail: fh = ( |
            | 
            layouts smi
                generateEncode: srcReg
                          Into: dstReg
                  IfDoesNotFit: [fh assertByteVector: int32Reg.
                                 dstReg = srcReg ifTrue: [
                                   withTemporaryRegisterDo: [|:tempReg|
                                     moveValueInRegister: srcReg ToInt32: int32Reg Temp: tempReg.
                                   ].
                                 ] False: [
                                     moveValueInRegister: srcReg ToInt32: int32Reg Temp:  dstReg.
                                 ].
                                 moveRegister: int32Reg ToRegister: dstReg
                                ]
                          With: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: primitives\x7fCategory: objects\x7fCategory: memory objects\x7fCategory: int32\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         moveValueOfInt32: int32Reg ToRegister: dstReg IfFail: fh = ( |
            | 
            generateIf: [|:trueFork|
              layouts object generateIf: int32Reg
                                   Temp: dstReg
                      IsSmiThenBranchTo: trueFork
                                   With: self.
            ] Then: [
              layouts smi generateDecode: int32Reg Into: dstReg With: self.
            ] Else: [

              byteVectorLayout
                                generateFor: int32Reg
                AddressOfFirstIndexableInto: dstReg
                                       With: self.

                 load32BitsAtOffset: 0
              FromAddressInRegister: dstReg
                         ToRegister: dstReg.

            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         moveWord: w ToLocation: loc = ( |
            | 
            materializeDest: loc AndDo: [|:r|
              moveWord: w ToRegister: r.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         moveWord: w ToRegister: r = ( |
            | 
            a load32To: r From: w.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         myAssemblerSystem = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: saving and restoring the stack frame\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         nonVolRegSaveAreaOffset = ( |
            | 
            oopSize negate * frame nonVolRegSaveAreaWordCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         operands* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         orMask: maskReg MaybeSetCCFrom: from To: to = ( |
            | 
            a orTo: to From: from With: maskReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: saving and restoring the stack frame\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         restoreFrame: wordOffset = ( |
            | 
            withTemporaryRegisterDo: [|:tr|
              a  laTo: sp Disp: frame callersSPOffset * oopSize Base: sp. "reset sp"
              a lwzTo: tr Disp: frame   savedPCOffset * oopSize Base: sp. "get return link"
              a lmwTo: firstNonVolRegisterToSave Disp: nonVolRegSaveAreaOffset Base: sp.
              addiToLRFrom: tr With: wordOffset
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: saving and restoring the stack frame\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         restoreFrameAndReturn: wordOffset = ( |
            | 
            haveStackFrame
              ifTrue: [ restoreFrame:       wordOffset ]
               False: [ addiToLRFromLRWith: wordOffset ].
            a blr.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: prologue & epilogue\x7fCategory: saving and restoring the stack frame\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         saveFrame = ( |
            | 
            a mflrTo:   r0.
            a stmwFrom: firstNonVolRegisterToSave
                  Disp: nonVolRegSaveAreaOffset
                  Base: sp.
            a  stwFrom: r0 
                  Disp: frame savedPCOffset * oopSize
                  Base: sp.
            a stwuFrom: sp
                  Disp: frame totalWordCount negate * oopSize
                  Base: sp.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         sendDesc = ( |
            | 
            vmKit sendDescs ppc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         shiftLeftImmBy: nBits From: src To: dst = ( |
            | 
            a slwiTo: dst From: src By: nBits.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         shiftRightArithImmBy: nBits From: src To: dst = ( |
            | 
            a srawiTo: dst From: src By: nBits.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         shiftRightImmBy: nBits From: src To: dst = ( |
            | 
            a srwiTo: dst From: src By: nBits.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: offsets\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         spOffsetOfIncomingMemoryArgument: loc InFrame: f = ( |
            | 
            (oopSize * f totalWordCount) +
              spOffsetOfOutgoingMemoryArgument: loc InFrame: f  "should really be f caller, I guess, except we don't have that info").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: offsets\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         spOffsetOfNonVolMemoryLocal: loc InFrame: f = ( |
            | 
            oopSize * (f localSPOffsetAt: loc index)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: offsets\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         spOffsetOfOutgoingMemoryArgument: loc InFrame: f = ( |
            | 
            oopSize * (f receiverAndArgumentSPOffsetAt: loc rcvrAndArgNo)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         storeByteFrom: src To: addr IndexedBy: indexReg = ( |
            | 
            a stbxFrom: src From: addr With: indexReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: storing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         storeRegister: r ToIncomingMemoryArgumentLocation: loc = ( |
            | 
            withStackPointerForMemoryLocation: loc Do: [|:sp. :f|
              false ifTrue: [
                todo optimization lexicalParentFrameSize.
                storeWordInRegister: r ToOffset: (loc spOffsetFor: self InFrame: f) FromAddressInRegister: sp.
              ]
              False: [
                withTemporaryRegisterDo: [|:tempReg|
                  loadValueAtOffset: oopSize * f savedSPOffset FromAddressInRegister: sp ToRegister: tempReg.
                  storeWordInRegister: r ToOffset: (spOffsetOfOutgoingMemoryArgument: loc InFrame: f) FromAddressInRegister: tempReg.
                ]
              ]
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: storing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         storeRegister: r ToNonVolMemoryLocalLocation: loc = ( |
            | 
            withStackPointerForMemoryLocation: loc Do: [|:sp. :f|
              storeWordInRegister: r ToOffset: (loc spOffsetFor: self InFrame: f) FromAddressInRegister: sp.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: location-specific methods\x7fCategory: storing\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         storeRegister: r ToOutgoingMemoryArgumentLocation: loc = ( |
            | 
            withStackPointerForMemoryLocation: loc Do: [|:sp. :f|
              storeWordInRegister: r ToOffset: (loc spOffsetFor: self InFrame: f) FromAddressInRegister: sp.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         storeWordFrom: src To: addr IndexedBy: index = ( |
            | 
            a stwxFrom: src From: addr With: index.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         storeWordInRegister: srcReg ToOffset: o FromAddressInRegister: addressReg = ( |
            | 
            a stwFrom: srcReg Disp: o Base: addressReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: low-level operations\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         subImm: anInt MaybeSetCCFrom: from To: to = ( |
            | 
            a subiTo: to From: from With: anInt.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: stubs\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         usingCTRBranchToAddressIn: r SetLink: shouldSetLink = ( |
            | 
            a mtctrFrom: r.
            shouldSetLink ifTrue: [a bctrl]
                           False: [a bctr ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: frame pointer\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         withStackPointerForLexicalLevel: ll StartingFrom: aSourceLevelAllocator ReceiverValue: rcvrValue Do: blk = ( |
             f.
             spReg.
            | 
            f: aSourceLevelAllocator machineLevelAllocator frame.
            spReg: sp.

            ll = 0 ifTrue: [^ blk value: spReg With: f].

            withTemporaryRegisterDo: [|:ancestorSP. lps. nm. rcvrLoc |
              lps: aSourceLevelAllocator context.
              ll do: [|:i|
                lps lexicalParentScope nmethod == nm ifFalse: [| rcvrReg |
                  rcvrLoc: i = 0 ifTrue: [rcvrValue location]
                                  False: [aSourceLevelAllocator locationForUplevel: i AccessTo: lps locationForIncomingReceiver].
                  rcvrLoc isRegister ifTrue: [
                    rcvrReg: rcvrLoc.
                  ] False: [
                    rcvrReg: ancestorSP.
                    rcvrLoc tell: self ToLoadMeFromFrame: f AtSP: ancestorSP ToRegister: rcvrReg.
                  ].
                  "Incoming receiver is the block"
                  layouts block generateHomeFramePointerOf: rcvrReg Into: ancestorSP With: self.
                  spReg: ancestorSP.
                ].

                lps: lps lexicalParentScope.
                nm: lps nmethod.
                nm ifNotNil: [f: nm frame].
              ].

              blk value: spReg With: f.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: frame pointer\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         withStackPointerForMemoryLocation: loc Do: blk = ( |
            | 
            withStackPointerForLexicalLevel: loc lexicalLevel
                               StartingFrom: loc accessingSourceLevelAllocator
                              ReceiverValue: loc accessingSourceLevelAllocator valueForIncomingReceiver
                                         Do: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: write barrier\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         writeBarrierForStoringOop: targetOopReg AtAddress: addressReg = ( |
            | 
            [addressReg != r0                               ] assert.
            [addressReg != registerUsage byteMapBaseRegister] assert.

            shiftRightImmBy: theVM universe cardTable shift From: addressReg To: addressReg.
            a liTo: r0 With: theVM universe cardTable valueForChangedCard.
            storeByteFrom: r0 To: registerUsage byteMapBaseRegister IndexedBy: addressReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: write barrier\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         writeBarrierForStoringOop: targetOopReg AtOffset: offset IntoObjectAtAddress: addressReg = ( |
            | 
            withTemporaryRegisterDo: [|:tempReg|
              writeBarrierForStoringOop: targetOopReg AtOffset: offset IntoObjectAtAddress: addressReg Temp: tempReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: write barrier\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         writeBarrierForStoringOop: targetOopReg AtOffset: offset IntoObjectAtAddress: addressReg Temp: tempReg = ( |
            | 
            addImm: offset From: addressReg To: tempReg.
            writeBarrierForStoringOop: targetOopReg AtAddress: tempReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: write barrier\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         writeBarrierForStoringOop: targetOopReg AtOffsetInRegister: offsetReg IntoObjectAtAddress: addressReg = ( |
            | 
            withTemporaryRegisterDo: [|:tempReg|
              writeBarrierForStoringOop: targetOopReg AtOffsetInRegister: offsetReg IntoObjectAtAddress: addressReg Temp: tempReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: moving data\x7fCategory: write barrier\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         writeBarrierForStoringOop: targetOopReg AtOffsetInRegister: offsetReg IntoObjectAtAddress: addressReg Temp: tempReg = ( |
            | 
            add: addressReg From: offsetReg To: tempReg.
            writeBarrierForStoringOop: targetOopReg AtAddress: tempReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> () From: ( | {
         'Comment: All SPARC trademarks are used under license and are trademarks or registered
trademarks of SPARC International, Inc. in the US and other countries.
Products bearing SPARC trademarks are based upon an architecture developed
by Sun Microsystems, Inc.\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         sparc = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'sparc' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes codeGenerators abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'sparc' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes codeGenerators sparc.

CopyDowns:
globals klein compiler1 parent prototypes codeGenerators abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'sparc' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'sparc' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes codeGenerators sparc parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'sparc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         myAssemblerSystem = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'sparc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'codeGenerators' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fCategory: generating code\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateAddressOf: memObjReg Into: dstUntaggedAddressReg With: cg = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fCategory: generating code\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateMemForAddress: untaggedAddressReg OID: oidSmiReg Into: dstMemObjReg With: cg = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'abstractObjectLocator') -> 'parent' -> () From: ( | {
         'Category: mapping oids to addresses\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateRecordAddress: untaggedAddressReg ForOID: oidReg With: cg = ( |
            | 
            [vmKit layouts abstract untaggedAddressesLookLikeSMIs] assert.
            "See" [elementForAddress: n].

            cg storeWordFrom: untaggedAddressReg
                          To: cg registerUsage objectAddressesBaseRegister
                   IndexedBy: oidReg.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fCategory: generating code\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateAddressOf: memObjReg Into: dstUntaggedAddressReg With: cg = ( |
            | 
            [todo optimization oopFormat].
            "Some callers could generate faster code if they
             knew that the oop was just a tagged pointer. Fix
             this in a way that doesn't break indirect pointers.
             -- Adam, 7/06"

            cg addImm: vmKit layouts memoryObject myTag negate
                 From: memObjReg
                   To: dstUntaggedAddressReg.

            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'directPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fCategory: generating code\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateMemForAddress: untaggedAddressReg OID: oidSmiReg Into: dstMemObjReg With: cg = ( |
            | 
            vmKit layouts memoryObject
              generateAddTagTo: untaggedAddressReg
                          Into: dstMemObjReg
                          With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fCategory: generating code\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateAddressOf: memObjReg Into: dstUntaggedAddressReg With: cg = ( |
            | 
            cg  add: memObjReg
               From: cg registerUsage objectAddressesBaseRegister
                 To: dstUntaggedAddressReg.

            cg     loadValueAtOffset: vmKit layouts memoryObject myTag negate
               FromAddressInRegister: dstUntaggedAddressReg
                          ToRegister: dstUntaggedAddressReg.

            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'indirectPointerObjectLocator') -> 'parent' -> () From: ( | {
         'Category: encoding & decoding oops\x7fCategory: generating code\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateMemForAddress: untaggedAddressReg OID: oidSmiReg Into: dstMemObjReg With: cg = ( |
            | 
            vmKit layouts memoryObject
              generateAddTagTo: oidSmiReg
                          Into: dstMemObjReg
                          With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: vectReg AddressOfFirstIndexableInto: dstAddressReg With: cg = ( |
            | 
                             generateFor: vectReg
            AddressOfIndexableAtConstant: 0
                                    Into: dstAddressReg
                                    With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractVector' -> () From: ( | {
         'Category: accessing indexable size field\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: vectReg IfIndex: indexSmiReg Temp: tempReg IsOutOfBoundsThenBranchTo: trueFork With: cg = ( |
            | 
            [tempReg != indexSmiReg] assert.

            generateIndexableSizeOf: vectReg
                               Into: tempReg
                               With: cg.

            cg generateIfUnsigned: indexSmiReg
                    IsNotLessThan: tempReg
             ThenUnlikelyBranchTo: trueFork.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( | {
         'Category: block cloning - code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateCloneBlock: blockReg HomeFrame: homeFPReg OID: oidReg Space: spaceReg Into: dstBlockReg IfOutOfMemoryThen: oomBlock With: cg = ( |
            | 
            [todo optimize blockCloning]. "I think it's kinda neat that block cloning can be
                                           done using regular Self code, but for now (until
                                           we have an optimizing compiler that can produce
                                           this same code) we're probably better using this
                                           version rather than the Self version. -- Adam, 7/06"

            theVM universe edenSpace
              generateAllocateOops: numberOfWordsInABlock
                FromSpace:          spaceReg
                Into:               dstBlockReg
                IfOutOfMemoryThen:  oomBlock
                Else: [
                  theVM objectLocator generateRecordAddress: dstBlockReg ForOID: oidReg With: cg.
                  generateInitializeBlockAtAddress: dstBlockReg FromBlock: blockReg HomeFrame: homeFPReg OID: oidReg Into: dstBlockReg With: cg.
                ]
                With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( | {
         'Category: accessing home frame pointer field\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateHomeFramePointerOf: blockReg Into: dstReg With: cg = ( |
            | 
            homeFramePointerField generateValueFor: blockReg Into: dstReg With: cg Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( | {
         'Category: block cloning - code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateInitializeBlockAtAddress: blockAddrReg From: origBlockReg HomeFrame: homeFPReg OID: oidReg With: cg = ( |
            | 
            cg comment: 'initializing block'.

            cg withTemporaryRegisterDo: [|:newMarkReg|
              markField generateValueFor: origBlockReg Into: newMarkReg With: cg Layout: self.
              theVM layouts mark oidField generateSetValueOfWord: newMarkReg To: oidReg Into: newMarkReg With: cg.
              markField generateSetValueForObjectWithAddress: blockAddrReg To: newMarkReg With: cg Layout: self.
            ].

            cg withTemporaryRegisterDo: [|:mapReg|
              mapField generateValueFor: origBlockReg Into: mapReg With: cg Layout: self.
              mapField generateSetValueForObjectWithAddress: blockAddrReg To: mapReg With: cg Layout: self.
            ].

            homeFramePointerField generateSetValueForObjectWithAddress: blockAddrReg To: homeFPReg With: cg Layout: self.

            [theVM exportPolicy shouldBlockValueSlotsBeObjectSlots not] assert. "If this ever stops being true, we'll
                                                                                 have to copy more stuff over to the
                                                                                 new block. -- Adam, 10/05"

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( | {
         'Category: block cloning - code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateInitializeBlockAtAddress: addrReg FromBlock: blockReg HomeFrame: homeFPReg OID: oidReg Into: dstBlockReg With: cg = ( |
            | 
            generateInitializeBlockAtAddress: addrReg From: blockReg HomeFrame: homeFPReg OID: oidReg With: cg.
            theVM objectLocator generateMemForAddress: addrReg OID: oidReg Into: dstBlockReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: getting address of indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: bpRefReg AddressOfIndexableAt: indexSmiReg Into: dstAddressReg With: cg = ( |
            | 
            cg withTemporaryRegisterDo: [|:tempReg|
              generateLoadOffsetOfByteAt: indexSmiReg Into: tempReg With: cg.
              cg add: tempReg From: bpRefReg To: dstAddressReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: getting address of indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: bpRefReg AddressOfIndexableAtConstant: index Into: dstAddressReg With: cg = ( |
            | 
            cg addImm: firstByteOffset + index From: bpRefReg To: dstAddressReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: reading\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForBytesPart: bpRefReg At: indexSmiReg Into: dstSmiReg With: cg = ( |
            | 
            [bpRefReg != dstSmiReg] assert.
            generateLoadOffsetOfByteAt: indexSmiReg Into: dstSmiReg With: cg.
            cg loadByteAt: bpRefReg IndexedBy: dstSmiReg To: dstSmiReg.
            layouts smi generateEncode: dstSmiReg Into: dstSmiReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: writing\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForBytesPart: bpRefReg At: indexSmiReg Put: dataSmiReg Temp1: temp1Reg Temp2: temp2Reg With: cg = ( |
            | 
            [(temp1Reg != dataSmiReg) && [temp1Reg != bpRefReg]] assert.
            [(temp2Reg != temp1Reg) && [temp2Reg != bpRefReg]] assert.

            generateLoadOffsetOfByteAt: indexSmiReg Into: temp1Reg With: cg.
            layouts smi generateDecode: dataSmiReg Into: temp2Reg With: cg.

            cg storeByteFrom: temp2Reg
                          To: bpRefReg
                   IndexedBy: temp1Reg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: addressing\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateLoadOffsetOfByteAt: indexSmiReg Into: dstOffsetReg With: cg = ( |
            | 
            layouts smi
              generateDecode: indexSmiReg Into: dstOffsetReg With: cg.
            cg addImm: firstByteOffset From: dstOffsetReg To: dstOffsetReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable size field\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIndexableSizeOfBytesPart: bpRefReg With: cg = ( |
            | 
            (cg locations offsetFromOtherLocation copyForOffset: indexableSizeFieldOffset FromRegister: bpRefReg)
                  description: 'indexable size of bytes part').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateDecode: immObjReg Into: dstUntaggedValueReg With: cg = ( |
            | 
            cg shiftRightArithImmBy: vmKit tag size
                               From: immObjReg
                                 To: dstUntaggedValueReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateEncode: untaggedValueReg Into: dstImmObjReg IfDoesNotFit: doesNotFitBlock With: cg = ( |
            | 
            cg generateIf: [|:doesNotFitLabel|
              cg withTemporaryRegisterDo: [|:tempReg|
                generateEncode: untaggedValueReg Into: tempReg With: cg.
                generateDecode: tempReg Into: tempReg With: cg.
                cg generateIf: untaggedValueReg DoesNotEqual: tempReg ThenBranchTo: doesNotFitLabel.
              ].
              generateEncode: untaggedValueReg Into: dstImmObjReg With: cg.
            ] Then: doesNotFitBlock.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateEncode: untaggedValueReg Into: dstImmObjReg With: cg = ( |
            | 
            cg shiftLeftImmBy: vmKit tag size
                         From: untaggedValueReg
                           To: dstImmObjReg.
            generateAddTagTo: dstImmObjReg
                        Into: dstImmObjReg
                        With: cg.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'Category: accessing map\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateMapOf: objReg Into: dstMapReg With: cg = ( |
            | 
            "All immediates of a particular type share the same map,
             and that map is the prototype."
            cg loadImmediateMap: mapPrototype IntoRegister: dstMapReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'map' -> () From: ( | {
         'Category: accessing nmethod cache field\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateNMethodCacheOf: mapReg Into: dstReg With: cg = ( |
            | 
                    generateFor: mapReg
            IndexableAtConstant: nmethodCacheIndex
                           Into: dstReg
                           With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'map' -> () From: ( | {
         'Category: accessing nmethod cache field\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateSetNMethodCacheOf: mapReg To: objVectReg Temp: tempReg With: cg = ( |
            | 
                    generateFor: mapReg
            IndexableAtConstant: nmethodCacheIndex
                            Put: objVectReg
                           Temp: tempReg
                           With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: isByteVector\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIfMark: markReg Temp: tempReg IsByteVectorMarkThenBranchTo: trueFork With: cg = ( |
            | 
            isByteVectorField generateIfBitInWord: markReg
                                             Temp: tempReg
                                IsSetThenBranchTo: trueFork
                                           Likely: true
                                             With: cg
                                           Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: constants\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateLoadTrailingMarkInto: dstMarkReg With: cg = ( |
            | 
            generateMarkForValue: trailingMarkValue Into: dstMarkReg With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: converting value to reference\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateMarkForValue: markValueSmiReg Into: dstMarkReg With: cg = ( |
            | 
            [vmKit tag smi = 0] assert.
            generateAddTagTo: markValueSmiReg Into: dstMarkReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: object ID\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateOIDOfMark: markReg Into: dstReg With: cg = ( |
            | 
            oidField generateValueOfWord: markReg Into: dstReg With: cg Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: converting reference to value\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateValueOfMark: markReg Into: dstSmiReg With: cg = ( |
            | 
            generateValueOf: markReg Into: dstSmiReg With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateSetValueFor: memObjReg To: valueReg Temp: tempReg With: cg Layout: aLayout = ( |
            | 
            aLayout generateAddressOf: memObjReg Into: tempReg With: cg.
            generateSetValueForObjectWithAddress: tempReg To: valueReg With: cg Layout: aLayout.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateSetValueFor: memObjReg To: valueReg With: cg Layout: aLayout = ( |
            | 
            aLayout generateFor: memObjReg AtConstant: fixedIndex Put: valueReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateSetValueForObjectWithAddress: memObjAddrReg To: valueReg With: cg Layout: aLayout = ( |
            | 
            aLayout generateForObjectAtAddress: memObjAddrReg
                                    AtConstant: fixedIndex
                                           Put: valueReg
                         BypassingWriteBarrier: canBypassWriteBarrier
                                          With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateValueFor: memObjReg Into: dstReg With: cg Layout: aLayout = ( |
            | 
            aLayout generateAddressOf: memObjReg Into: dstReg With: cg.
            generateValueForObjectWithAddress: dstReg Into: dstReg With: cg Layout: aLayout.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateValueForObjectWithAddress: memObjAddrReg Into: dstReg With: cg Layout: aLayout = ( |
            | 
            aLayout generateForObjectAtAddress: memObjAddrReg AtConstant: fixedIndex Into: dstReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: converting reference to address\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateAddressOf: memObjReg Into: dstUntaggedAddressReg With: cg = ( |
            | 
            cg generateAddressOf: memObjReg Into: dstUntaggedAddressReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: reading\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: memObjReg At: indexSmiReg Into: dstObjReg With: cg = ( |
            | 
            [dstObjReg != indexSmiReg] assert.
            generateAddressOf: memObjReg Into: dstObjReg With: cg.
            generateForObjectAtAddress: dstObjReg At: indexSmiReg Into: dstObjReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: writing\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: memObjReg At: indexSmiReg Put: dataObjReg Temp: tempReg With: cg = ( |
            | 
            [(tempReg != dataObjReg) && [tempReg != indexSmiReg]] assert.
            generateAddressOf: memObjReg Into: tempReg With: cg.
            generateForObjectAtAddress: tempReg At: indexSmiReg Put: dataObjReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: writing\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: memObjReg At: indexSmiReg PutImmediate: dataImmObjReg Temp: tempReg With: cg = ( |
            | 
            [(tempReg != dataImmObjReg) && [tempReg != indexSmiReg]] assert.
            generateAddressOf: memObjReg Into: tempReg With: cg.
            generateForObjectAtAddress: tempReg At: indexSmiReg Put: dataImmObjReg BypassingWriteBarrier: true With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: reading\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: memObjReg AtConstant: index Into: dstObjReg With: cg = ( |
            | 
            generateAddressOf: memObjReg Into: dstObjReg With: cg.
            generateForObjectAtAddress: dstObjReg AtConstant: index Into: dstObjReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: writing\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: memObjReg AtConstant: index Put: dataObjReg Temp: tempReg With: cg = ( |
            | 
            generateAddressOf: memObjReg Into: tempReg With: cg.
            generateForObjectAtAddress: tempReg AtConstant: index Put: dataObjReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: writing\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: memObjReg AtConstant: index Put: dataObjReg With: cg = ( |
            | 
            cg withTemporaryRegisterDo: [|:tempReg|
              generateFor: memObjReg AtConstant: index Put: dataObjReg Temp: tempReg With: cg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing bytes\x7fCategory: reading\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: memObjReg ByteAt: indexSmiReg Into: dstSmiReg With: cg = ( |
            | 
            [dstSmiReg != indexSmiReg] assert.
            generateAddressOf: memObjReg Into: dstSmiReg With: cg.
            cg withTemporaryRegisterDo: [|:temp2Reg|
              layouts smi generateDecode: indexSmiReg Into: temp2Reg With: cg.
              cg loadByteAt: dstSmiReg IndexedBy: temp2Reg To: dstSmiReg.
            ].
            layouts smi generateEncode: dstSmiReg Into: dstSmiReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing bytes\x7fCategory: writing\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: memObjReg ByteAt: indexSmiReg Put: dataSmiReg Temp: tempReg With: cg = ( |
            | 
            [(tempReg != dataSmiReg) && [tempReg != indexSmiReg]] assert.
            generateAddressOf: memObjReg Into: tempReg With: cg.
            cg withTemporaryRegisterDo: [|:temp2Reg|
              layouts smi generateDecode: dataSmiReg Into: temp2Reg With: cg.
              cg withTemporaryRegisterDo: [|:temp3Reg|
                layouts smi generateDecode: indexSmiReg Into: temp3Reg With: cg.
                cg storeByteFrom: temp2Reg To: tempReg IndexedBy: temp3Reg.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: memObjReg IsMarkAt: indexSmiReg Into: dstBoolReg With: cg = ( |
            | 
            generateFor: memObjReg
                     At: indexSmiReg
                   Into: dstBoolReg
                   With: cg.

            layouts mark
              generateIsMark: dstBoolReg
                        Into: dstBoolReg
                        With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: writing\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: memObjReg SetMarkValue: markValueSmiReg Temp: tempReg With: cg = ( |
            | 
            [tempReg != memObjReg] assert.
            layouts mark generateMarkForValue: markValueSmiReg Into: tempReg With: cg.
            markField generateSetValueFor: memObjReg To: tempReg With: cg Layout: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: reading\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg At: indexSmiReg Into: dstObjReg With: cg = ( |
            | 
            cg generateForObjectAtAddress: memObjAddrReg
                                       At: indexSmiReg
                                     Into: dstObjReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: writing\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg At: indexSmiReg Put: dataObjReg BypassingWriteBarrier: shouldBypassWriteBarrier With: cg = ( |
            | 
            cg generateForObjectAtAddress: memObjAddrReg
                                       At: indexSmiReg
                                      Put: dataObjReg
                    BypassingWriteBarrier: shouldBypassWriteBarrier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: writing\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg At: indexSmiReg Put: dataObjReg With: cg = ( |
            | 
            generateForObjectAtAddress: memObjAddrReg
                                    At: indexSmiReg
                                   Put: dataObjReg
                 BypassingWriteBarrier: false
                                  With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: reading\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg AtConstant: index Into: dstObjReg With: cg = ( |
            | 
            cg generateForObjectAtAddress: memObjAddrReg
                               AtConstant: index
                                     Into: dstObjReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: writing\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg AtConstant: index Put: dataObjReg BypassingWriteBarrier: shouldBypassWriteBarrier With: cg = ( |
            | 
            cg generateForObjectAtAddress: memObjAddrReg
                               AtConstant: index
                                      Put: dataObjReg
                    BypassingWriteBarrier: shouldBypassWriteBarrier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: writing\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateForObjectAtAddress: memObjAddrReg AtConstant: index Put: dataObjReg With: cg = ( |
            | 
            generateForObjectAtAddress: memObjAddrReg
                            AtConstant: index
                                   Put: dataObjReg
                 BypassingWriteBarrier: false
                                  With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: secondary (i.e. what kind of mem) tests\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIfMem: memObjReg Temp: tempReg IsByteVectorThenBranchTo: trueFork With: cg = ( |
            | 
            generateMarkOf: memObjReg Into: tempReg With: cg.
            layouts mark generateIfMark: tempReg Temp: tempReg IsByteVectorMarkThenBranchTo: trueFork With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing map\x7fCategory: reading\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateMapOf: memObjReg Into: dstReg With: cg = ( |
            | 
            mapField generateValueFor: memObjReg Into: dstReg With: cg Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateMarkOf: memObjReg Into: dstMarkReg With: cg = ( |
            | 
            markField generateValueFor: memObjReg Into: dstMarkReg With: cg Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateMarkValueOf: memObjReg Into: dstSmiReg With: cg = ( |
            | 
            generateMarkOf: memObjReg Into: dstSmiReg With: cg.
            layouts mark generateValueOfMark: dstSmiReg Into: dstSmiReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateAddUntaggedByteOffsetForIndex: indexSmiReg To: dstUntaggedByteOffsetReg With: cg = ( |
            | 
            [vmKit tag smi  = 0] assert.
            [vmKit tag size = 2] assert.
            cg add: indexSmiReg From: dstUntaggedByteOffsetReg To: dstUntaggedByteOffsetReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: vectReg IndexableAt: indexSmiReg PutImmediate: immValueReg Temp: tempReg With: cg = ( |
            | 
            [vmKit tag smi  = 0] assert.
            [vmKit tag size = 2] assert.
            generateFor: vectReg UntaggedByteOffsetForIndex: indexSmiReg Into: tempReg With: cg.
            cg withTemporaryRegisterDo: [|:temp2Reg|
              generateFor: vectReg IndexableAtUntaggedByteOffset: tempReg PutImmediate: immValueReg Temp: temp2Reg With: cg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: objVectReg IndexableAtConstant: index Into: dstObjReg With: cg = ( |
            | 
            cg withTemporaryRegisterDo: [|:indexSmiReg|
              generateIndexableOriginOf: objVectReg Into: indexSmiReg With: cg.
              (index != 0) ifTrue: [
                cg addImm: index * oopSize
                     From: indexSmiReg
                       To: indexSmiReg.
              ].
              generateFor: objVectReg At: indexSmiReg Into: dstObjReg With: cg.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: objVectReg IndexableAtConstant: index Put: valueReg Temp: tempReg With: cg = ( |
            | 
            generateIndexableOriginOf: objVectReg Into: tempReg With: cg.
            (index != 0) ifTrue: [
              cg addImm: index * oopSize 
                   From: tempReg
                     To: tempReg
            ].
            cg withTemporaryRegisterDo: [|:tempReg2|
              generateFor: objVectReg At: tempReg Put: valueReg Temp: tempReg2 With: cg.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateFor: objVectReg IndexableAtUntaggedByteOffset: untaggedByteOffsetReg Into: dstObjReg With: cg = ( |
            | 
            [vmKit tag size = 2] assert.
            generateFor: objVectReg At: untaggedByteOffsetReg Into: dstObjReg With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateFor: objVectReg IndexableAtUntaggedByteOffset: untaggedByteOffsetReg Put: valueReg Temp: tempReg With: cg = ( |
            | 
            [vmKit tag size = 2] assert.
            generateFor: objVectReg At: untaggedByteOffsetReg Put: valueReg Temp: tempReg With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateFor: objVectReg IndexableAtUntaggedByteOffset: untaggedByteOffsetReg PutImmediate: immValueReg Temp: tempReg With: cg = ( |
            | 
            [vmKit tag size = 2] assert.
            generateFor: objVectReg At: untaggedByteOffsetReg PutImmediate: immValueReg Temp: tempReg With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateAddTagTo: srcReg Into: dstReg With: cg = ( |
            | 
            (myTag = 0) && [srcReg = dstReg]  ifFalse: [
              cg addImm: myTag
                   From: srcReg
                     To: dstReg
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: getting the tag from an object reference\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCasesForTagOfObject: objReg Temp: tempReg DoWithLayout: layoutBlock With: cg = ( |
            | 
            [objReg != tempReg] assert.
            cg andImmMask: vmKit tag mask AndSetCCFrom: objReg To: tempReg.
            cg generateSwitchForCases: oneLayoutForEachObjectTag
                                   If: [|:layout. :fork| cg generateIf: tempReg EqualsImmediate: layout myTag ThenLikelyBranchTo: fork]
                                 Then: layoutBlock
                                 Else: [cg generateShouldNeverHappen: 'not an object?'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: creating an object reference\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateCreateObjectReferenceWithTag: tagSmiReg AndValue: valueSmiReg Into: dstObjReg With: cg = ( |
            | 
            [dstObjReg != valueSmiReg] assert.
            layouts smi generateDecode: tagSmiReg Into: dstObjReg With: cg.
            cg orMask: valueSmiReg MaybeSetCCFrom: dstObjReg To: dstObjReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fCategory: code generation\x7fComment: Puts into the destination register the value portion
of the source oop, with its tag removed, in an internal
machine form that the processor can manipulate directly.\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateDecode: objReg Into: dstUntaggedValueReg With: cg = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fCategory: code generation\x7fComment: The inverse of generateDecode:Into:With:.\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateEncode: untaggedValueReg Into: dstObjReg With: cg = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Temp: tempReg DoesNotHaveTag: t IsLikely: isLikely ThenBranchTo: trueFork With: cg = ( |
            | 
            t = 0 ifTrue: [
              "Optimization: if we're looking for the kind of oop with tag 0,
               we can do this with fewer instructions."
              cg andImmMask: vmKit tag mask AndSetCCFrom: objReg To: tempReg.
              isLikely ifTrue: [cg branchNELikelyTo:   trueFork]
                        False: [cg branchNEUnlikelyTo: trueFork].
            ] False: [
              cg andImmMask: vmKit tag mask MaybeSetCCFrom: objReg To: tempReg.
              isLikely ifTrue: [cg generateIf: tempReg DoesNotEqualImmediate: t ThenLikelyBranchTo:   trueFork]
                        False: [cg generateIf: tempReg DoesNotEqualImmediate: t ThenUnlikelyBranchTo: trueFork].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: secondary (i.e. what kind of mem) tests\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Temp: tempReg HasAnyMapTypeIn: maps ThenBranchTo: trueFork With: cg = ( |
            | 
            cg   generateIf: objReg
                       Temp: tempReg
            HasAnyMapTypeIn: maps
               ThenBranchTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: secondary (i.e. what kind of mem) tests\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Temp: tempReg IsByteVectorThenBranchTo: trueFork With: cg = ( |
            | 
            cg generateIf: objReg Temp: tempReg IsByteVectorThenBranchTo: trueFork.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: isBytesPart\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Temp: tempReg IsBytesPartThenBranchTo: trueFork With: cg = ( |
            | 
                   generateIf: objReg 
                         Temp: tempReg
            IsSmiThenBranchTo: trueFork
                         With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: isFloat\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Temp: tempReg IsFloatThenBranchTo: trueFork With: cg = ( |
            | 
            cg generateIf: objReg
                     Temp: tempReg
                   HasTag: vmKit tag float
                 IsLikely: true
             ThenBranchTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: isMark\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Temp: tempReg IsMarkThenBranchTo: trueFork With: cg = ( |
            | 
            cg generateIf: objReg
                     Temp: tempReg
                   HasTag: vmKit tag mark
                 IsLikely: false
             ThenBranchTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: isMem\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Temp: tempReg IsMemoryObjectThenBranchTo: trueFork With: cg = ( |
            | 
            cg generateIf: objReg
                     Temp: tempReg
                   HasTag: vmKit tag mem
                 IsLikely: true
             ThenBranchTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: isMem\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Temp: tempReg IsNotMemoryObjectThenBranchTo: trueFork With: cg = ( |
            | 
                generateIf: objReg
                      Temp: tempReg
            DoesNotHaveTag: vmKit tag mem
                  IsLikely: false
              ThenBranchTo: trueFork
                      With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: isSmi\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIf: objReg Temp: tempReg IsSmiThenBranchTo: trueFork With: cg = ( |
            | 
            cg generateIf: objReg
                     Temp: tempReg
                   HasTag: vmKit tag smi
                 IsLikely: true
             ThenBranchTo: trueFork).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: secondary (i.e. what kind of mem) tests\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIfMapOf: objReg HasAnyMapTypeIn: mapPrototypes Temp: temp1Reg ThenBranchTo: trueFork With: cg = ( |
             immediateMapProtos.
             nonImmediateMapProtos.
            | 

            nonImmediateMapProtos: mapPrototypes asList copyFilteredBy: [|:mapProto| mapProto isImmediate not].
               immediateMapProtos: mapPrototypes asList copyFilteredBy: [|:mapProto| mapProto isImmediate    ].

            "Optimization: We don't actually need to get the map if
             we can perform this test exclusively by checking the
             tag bits. -- jb 7/03, implemented by Adam in 7/05"

            generateCasesForTagOfObject: objReg Temp: temp1Reg DoWithLayout: [|:layout|
              layout isImmediate ifTrue: [
                (immediateMapProtos anySatisfy: [|:m| m myLayout = layout]) ifTrue: [
                  cg branchToLabel: trueFork.
                ].
              ] False: [
                cg withTemporaryRegisterDo: [|:temp2Reg|
                  layouts memoryObject generateMapOf: objReg Into: temp2Reg With: cg.
                  layouts map generateMapTypeOf: temp2Reg Into: temp1Reg With: cg.
                ].
                nonImmediateMapProtos do: [|:mapProto|
                  cg generateIf: temp1Reg
                      EqualsOop: mapProto mapType
                   ThenBranchTo: trueFork.
                ].
              ].
            ] With: cg.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: isMark\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIsMark: objReg Into: dstBoolReg With: cg = ( |
            | 
            cg generateTest: [|:trueFork|
              generateIf:           objReg
                              Temp: dstBoolReg
                IsMarkThenBranchTo: trueFork
                              With: cg
            ] LoadBooleanInto: dstBoolReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: accessing map\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateMapOf: objReg Into: dstMapReg With: cg = ( |
            | 
            generateCasesForTagOfObject: objReg
                                   Temp: dstMapReg
                           DoWithLayout: [|:layout| layout generateMapOf: objReg Into: dstMapReg With: cg]
                                   With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: getting the tag from an object reference\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateTagOf: objReg Into: dstSmiReg With: cg = ( |
            | 
            "extract tag (low order bits of oop) and return as a smi"

            cg           clearHigh: cg a intNN bitSize - vmKit tag size
                BitsAndShiftLeftBy: vmKit tag size
                              From: objReg
                                To: dstSmiReg.

            layouts smi
                generateAddTagTo: dstSmiReg
                            Into: dstSmiReg
                            With: cg.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: getting the value from an object reference\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateValueOf: objReg Into: dstSmiReg With: cg = ( |
            | 
            "extract value (high order bits of oop) and return as a smi"
            cg clearLow: vmKit tag size BitsFrom: objReg To: dstSmiReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: getting the tag from an object reference\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         oneLayoutForEachImmediateObjectTag = ( |
            | 
            ( layouts smi
            & layouts float) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: getting the tag from an object reference\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         oneLayoutForEachObjectTag = ( |
            | 
            ( layouts memoryObject
            & layouts smi
            & layouts float) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: getting the tag from an object reference\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         oneLayoutForEachTag = ( |
            | 
            ( layouts memoryObject
            & layouts smi
            & layouts float
            & layouts mark) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateBytesPartRefFor: byteVectReg Into: dstBPRefReg With: cg = ( |
            | 
            bytesPartRefField generateValueFor: byteVectReg Into: dstBPRefReg With: cg Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: byteVectReg AddressOfIndexableAt: indexSmiReg Into: dstAddressReg With: cg = ( |
            | 
            [indexSmiReg != dstAddressReg] assert.
            generateBytesPartRefFor: byteVectReg Into: dstAddressReg With: cg.
            layouts bytesPart
                         generateFor: dstAddressReg
                AddressOfIndexableAt: indexSmiReg
                                Into: dstAddressReg
                                With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: byteVectReg AddressOfIndexableAtConstant: index Into: dstAddressReg With: cg = ( |
            | 
            generateBytesPartRefFor: byteVectReg Into: dstAddressReg With: cg.
            layouts bytesPart
              generateFor:                    dstAddressReg
                AddressOfIndexableAtConstant: index
                                        Into: dstAddressReg
                                        With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: byteVectReg IndexableAt: indexSmiReg Into: dstSmiReg With: cg = ( |
            | 
            [(dstSmiReg != indexSmiReg) && [dstSmiReg != byteVectReg]] assert.
            cg withTemporaryRegisterDo: [|:tempReg|
              generateBytesPartRefFor: byteVectReg Into: tempReg With: cg.
              layouts bytesPart
                generateForBytesPart: tempReg
                                  At: indexSmiReg
                                Into: dstSmiReg
                                With: cg.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: byteVectReg IndexableAt: indexSmiReg Put: valueSmiReg Temp: temp1Reg With: cg = ( |
            | 
            [    (temp1Reg != indexSmiReg)
             && [(temp1Reg != byteVectReg)
             && [ temp1Reg != valueSmiReg ]]] assert.

            cg withTemporaryRegisterDo: [|:temp2Reg|
              cg withTemporaryRegisterDo: [|:temp3Reg|
                generateBytesPartRefFor: byteVectReg Into: temp1Reg With: cg.
                layouts bytesPart
                  generateForBytesPart: temp1Reg
                                    At: indexSmiReg
                                   Put: valueSmiReg
                                 Temp1: temp2Reg
                                 Temp2: temp3Reg
                                  With: cg.
              ].
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing indexable size field\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIndexableSizeOf: byteVectReg Into: dstSizeSmiReg With: cg = ( |
            | 
            generateBytesPartRefFor: byteVectReg
                               Into: dstSizeSmiReg
                               With: cg.
            cg moveLocation: (layouts bytesPart
                                locationForIndexableSizeOfBytesPart: dstSizeSmiReg
                                                               With: cg)
                 ToRegister: dstSizeSmiReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateAddUntaggedByteOffsetForIndex: indexSmiReg To: dstUntaggedByteOffsetReg With: cg = ( |
            | 
            cg withTemporaryRegisterDo: [|:tempReg|
              layouts smi generateDecode: indexSmiReg Into: tempReg With: cg.
              cg add: tempReg From: dstUntaggedByteOffsetReg To: dstUntaggedByteOffsetReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateFor: byteVectReg IndexableAtUntaggedByteOffset: untaggedByteOffsetReg Into: dstSmiReg With: cg = ( |
            | 
            cg withTemporaryRegisterDo: [|:addrReg|
              generateAddressOf: byteVectReg Into: addrReg With: cg.
              cg loadByteAt: addrReg IndexedBy: untaggedByteOffsetReg To: dstSmiReg.
            ].
            layouts smi generateEncode: dstSmiReg Into: dstSmiReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         generateFor: byteVectReg IndexableAtUntaggedByteOffset: untaggedByteOffsetReg Put: valueSmiReg Temp: tempReg With: cg = ( |
            | 
            cg withTemporaryRegisterDo: [|:addrReg|
              generateAddressOf: byteVectReg Into: addrReg With: cg.
              layouts smi generateDecode: valueSmiReg Into: tempReg With: cg.
              cg storeByteFrom: tempReg To: addrReg IndexedBy: untaggedByteOffsetReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: allocating\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         objsLimitSlotIn: spaceMir = ( |
            | 
            [bytesBottom].
            spaceMir at: 'bytesBottom').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: allocating\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateAllocateOops: size FromSpace: spaceReg Into: dstUntaggedAddressReg IfOutOfMemoryThen: oomBlock Else: elseBlock With: cg = ( |
             objsLimitSlot.
             objsTopAssignmentSlot.
             objsTopSlot.
             spaceMir.
            | 

            [objsTop. objsTop: 0. allocateOops: 0 AndBytes: -1]. "browsing"

            spaceMir:              reflect: self.
            objsTopSlot:           spaceMir at: 'objsTop'.
            objsTopAssignmentSlot: spaceMir at: 'objsTop:'.
            objsLimitSlot:         objsLimitSlotIn: spaceMir.

            "Check for out of memory"
            cg generateIf: [|:trueFork. temp2Reg|

              temp2Reg: cg allocateTemporaryRegister.
              cg loadFromDataSlot: objsTopSlot   OfHolderRegister: spaceReg IntoRegister: dstUntaggedAddressReg.
              cg loadFromDataSlot: objsLimitSlot OfHolderRegister: spaceReg IntoRegister: temp2Reg.

              cg withTemporaryRegisterDo: [|:temp1Reg|
                cg addImm: (layouts smi encode: size * oopSize)
                     From: dstUntaggedAddressReg
                       To: temp1Reg.

                cg generateIfUnsigned: temp1Reg  "objsTop + size * oopSize"
                        IsNotLessThan: temp2Reg  "objsLimit"
                 ThenUnlikelyBranchTo: trueFork.

                cg freeTemporaryRegister: temp2Reg.

                cg         storeIntoDataSlot: objsTopAssignmentSlot
                            OfHolderRegister: spaceReg
                                FromRegister: temp1Reg
                   IsGuaranteedNotToBeMemObj: true.
              ].
            ] 
            Then: oomBlock
            Else: [
              layouts smi
                  generateDecode: dstUntaggedAddressReg
                            Into: dstUntaggedAddressReg
                            With: cg.

              cg withTemporaryRegisterDo: [|:tempReg|
                layouts mark generateLoadTrailingMarkInto: tempReg With: cg.
                cg moveRegister: tempReg
                     ToLocation: cg locations offsetFromOtherLocation
                                      copyForOffset: size * oopSize
                                       FromRegister: dstUntaggedAddressReg.
              ].

              elseBlock value
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: allocating\x7fCategory: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         objsLimitSlotIn: spaceMir = ( |
            | 
            [top].
            spaceMir at: 'top').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIfBitInWord: srcReg Temp: tempReg IsSetThenBranchTo: trueFork Likely: isLikely With: cg Layout: aLayout = ( |
            | 
            generateInPlaceValueOfWord: srcReg Into: tempReg With: cg Layout: aLayout.
            isLikely ifTrue: [cg branchNELikelyTo:   trueFork]
                      False: [cg branchNEUnlikelyTo: trueFork].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateClearValueOfWord: wordReg Into: dstReg With: cg = ( |
            | 
            cg   andImmMask: (mask << vmKit tag size) complement
             MaybeSetCCFrom: wordReg
                         To: wordReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateInPlaceValueOfWord: srcReg Into: dstReg With: cg Layout: aLayout = ( |
             m.
            | 
            [vmKit tag smi = 0] assert.
            m: (aLayout encode: mask) && aLayout myTag complement.

            [aaa]. "I don't think  m < 0  is the right test here. We just need
                    to know whether we can use the immediate version. -- Adam, Mar. 2009"
            m < 0 ifFalse: [
              cg   andImmMask: m
                 AndSetCCFrom: srcReg
                           To: dstReg.
            ] True: [
              cg withTemporaryRegisterDo: [|:maskReg|
                cg moveWord: m ToRegister: maskReg.
                cg    andMask: maskReg
                 AndSetCCFrom: srcReg
                           To: dstReg.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateSetValueOfWord: wordReg To: valueSmiReg Into: dstReg With: cg = ( |
            | 
            [dstReg != valueSmiReg] assert.
            generateClearValueOfWord: wordReg Into: dstReg With: cg.
            cg withTemporaryRegisterDo: [|:inPlaceValueReg|
              [vmKit tag smi = 0] assert.
              cg shiftLeftImmBy: shift From: valueSmiReg To: inPlaceValueReg.
              cg orMask: inPlaceValueReg MaybeSetCCFrom: dstReg To: dstReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'Category: code generation\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateValueOfWord: srcReg Into: dstReg With: cg Layout: aLayout = ( |
            | 
            [vmKit tag smi = 0] assert.
            generateInPlaceValueOfWord: srcReg Into: dstReg With: cg Layout: aLayout.
            cg shiftRightImmBy: shift From: dstReg To: dstReg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         generateNumberForWord: srcReg WhichIsShiftedLeftBy: nBits Into: dstReg With: cg = ( |
            | 
            [todo cleanup]. "It'd be nice if this method didn't have to take the WhichIsShiftedLeftBy: argument.
                             We'd need to turn the tag into another explicit field in marks, so that the words
                             that you'd get when you call wordForNumber: would be mark oops, rather than mark values."
            cg andImmMask: (vmKit layouts abstract intNN shl: mask With: nBits) AndSetCCFrom: srcReg To: dstReg.
            cg shiftRightImmBy: shift + (nBits - vmKit tag size "so that we can make dstReg contain a valid smi") From: dstReg To: dstReg.
            lowestEncodableNumber = 0 ifFalse: [
              [klein layouts smi myTag = 0] assert. "If this stops being true we'll have to fix this code."
              cg addImm: (vmKit layouts smi encode: lowestEncodableNumber) From: dstReg To: dstReg.
            ].
            vmKit layouts smi generateAddTagTo: dstReg Into: dstReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot'
        
         kleinC1_Gens = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Gens' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Gens' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinC1_Gens.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Gens' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Gens' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Gens' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Gens' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
            resend.postFileIn.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Gens' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.123 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Gens' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Gens InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'kleinVarHdrsCodeGen
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'kleinVarHdrsCodeGen' From: 'applications/klein'



 '-- Side effects'

 globals modules kleinC1_Gens postFileIn
