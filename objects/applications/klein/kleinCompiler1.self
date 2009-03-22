 '$Revision: 30.67 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: compiler\x7fCategory: compiler 1\x7fComment: The real deal as of 6/03 -- dmu\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         compiler1s = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> () From: ( | {
         'Comment: Public because this is also a factory.
-- dmu, 6/03\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: compilation helpers\x7fComment: allocator for to be compiled lexical level (contains links to parent lexical allocators)\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         allocator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (\'pdp11\')'
        
         architecture <- 'pdp11'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         areCommentsEmitted <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: compilation helpers\x7fComment: Used to allow the compiler to back out of the leaf
method optimization when it is determined for certain
that it does not apply.  -- jb 8/03\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         backoutBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: compilation helpers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         codeGenerator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: special complation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         hasOnNonLocalReturn <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         isLogUsed <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)'
        
         localReturnNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: bytecode method information\x7fCategory: resend information\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         lookupType.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: compilation helpers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: public'
        
         nmethodRelocators <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: special complation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         noGCAllowed <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: special complation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         noMapTest <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: special complation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         noSendsAllowed <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: bytecode method information\x7fCategory: resend information\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         objectDoingTheResend.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: oracle for eager relocation\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         oracleThatCannotDoEagerRelocation = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( |
             {} = 'Comment: Set linearizedObjects to me when you don\'t want to (or
can\'t) do eager relocation. -- Adam, 3/05\x7fModuleInfo: Creator: globals klein compiler1s abstract parent oracleThatCannotDoEagerRelocation.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: compilation helpers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (klein compiler1s abstract oracleThatCannotDoEagerRelocation)\x7fVisibility: public'
        
         oracleForEagerRelocation <- bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: bytecode method information\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         outerNMethods.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: relocators\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         addNMethodRelocator: or = ( |
            | 
            nmethodRelocators add: or.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: relocators\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         addRelocator: or = ( |
            | 
            relocators addLast: or.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateIncomingAndPreallocatedLocations = ( |
            | 
            allocator allocateIncomingAndPreallocatedLocations.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: scoping\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         allocatorAtLexicalLevel: ll = ( |
            | 
            allocator allocatorAtLexicalLevel: ll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentSlotsForAllocator: a = ( |
            | vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         assignableLocalSlotsForAllocator: a = ( |
            | 
            vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildAndLinkValues = ( |
            | 
            startNode controlFlowOrderDo: [|:n| n allocateLocations      ].
            startNode controlFlowOrderDo: [|:n| n buildPushedValues      ].
            startNode controlFlowOrderDo: [|:n| n findPoppedValues       ].
            nodesInReverseSourceOrder do: [|:n| n allocatePoppedLocations].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: building nmethods\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         buildNMethod = ( |
             nm.
            | 
            nm: vmKit nmethod copySize: codeGenerator generatedCodeSize.
            codeGenerator copyGeneratedCodeInto: nm.
            nmethodRelocators do: [|:rl| rl beForObject: nm].
            nm relocators: relocators.
            nm method: method.
            nm methodHolder: slotHolderMirror reflectee.
            nm lookupKey initializeForSelector: selector
                                    LookupType: lookupType
                          ObjectDoingTheResend: objectDoingTheResend
                                    SlotHolder: [slotHolderMirror reflectee].
            nm frame: frame copy.
            nm nonVolLocalRegCount: allocator nonVolLocalRegCount.
            nm incomingRcvrSPOffset: allocator locationForIncomingReceiver spOffsetForNMethod: codeGenerator.
            nm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: creating then compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         compileSlot: slotHoldingMethod Self: selfMir Receiver: rcvrMir LookupType: lt ObjectDoingTheResend: o OuterNMethods: onms Architecture: arch Oracle: oracle Debug: d = ( |
            | 
                    (copyForSlot:  slotHoldingMethod
                            Self:  selfMir
                        Receiver:  rcvrMir
                      LookupType:  lt
            ObjectDoingTheResend:  o
                   OuterNMethods:  onms
                    Architecture:  arch
                          Oracle:  oracle
                           Debug:  d) compileForcingNonLeafIfNecessary).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            (resend.copy
                     relocators: relocators copy)
              nmethodRelocators: nmethodRelocators copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         copyForSlot: slot Self: selfMir Receiver: rcvrMir LookupType: lt ObjectDoingTheResend: o OuterNMethods: onms Architecture: arch Oracle: oracle Debug: d = ( |
            | 
            copyForSlot:           slot
            Self:                  selfMir
            Receiver:              rcvrMir
            LookupType:            lt
            ObjectDoingTheResend:  o
            OuterNMethods:         onms
            Architecture:          arch
            Oracle:                oracle
            Debug:                 d
            ForceNonLeaf:          false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         copyForSlot: slot Self: selfMir Receiver: rcvrMir LookupType: lt ObjectDoingTheResend: o OuterNMethods: onms Architecture: arch Oracle: oracle Debug: d ForceNonLeaf: fnl = ( |
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


            slot kleinCompilerPrototypeForMe copy
              initializeForSlot:        slot
                 Self:                  selfMir
                 Receiver:              rcvrMir
                 LookupType:            lt
                 ObjectDoingTheResend:  o
                 OuterNMethods:         onms
                 Architecture:          arch
                 Oracle:                oracle
                 Debug:                 d
                 ForceNonLeaf:          fnl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         debug: bool = ( |
            | 
            areCommentsEmitted: bool).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         doCompile = ( |
            | 
            codeGenerator nonmethodPrologue.
            compileSlot.
            codeGenerator nonmethodEpilogue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         doCompileIfBackout: blk = ( |
            | 
            backoutBlock: blk.
            doCompile).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         forgeControlFlowLinks = ( |
            | 
            startNode sourceNodesDo: [|:n| n forgeControlFlowLinks].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         frame = ( |
            | allocator frame).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: scoping\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         frameAtLexicalLevel: ll = ( |
            | 
            (allocatorAtLexicalLevel: ll) frame).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeAllocatorsForceNonLeaf: fnl = ( |
             ap.
             lca.
            | 
            ap: protoAllocatorForMyPlatform.
            allocator: ap copyForCompiler: self Method: method NMethod: nil ForceNonLeaf: fnl LexicalChildAllocator: nil.
            lca: allocator.
            method lexicalParents with: outerNMethods Do: [|:methMir. :nm| 
              lca: ap copyForCompiler: self Method: methMir NMethod: nm ForceNonLeaf: true LexicalChildAllocator: lca.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForSlot: s Self: selfMir Receiver: rcvrMir LookupType: lt ObjectDoingTheResend: o OuterNMethods: onms Architecture: arch Oracle: oracle Debug: d ForceNonLeaf: fnl = ( |
            | 
            architecture:             arch.
            selfMirror:               selfMir.
            slot:                     s.
            lookupType:               lt.
            objectDoingTheResend:     o.
            debug:                    d.
            relocators:               list copyRemoveAll.
            nmethodRelocators:        list copyRemoveAll.
            outerNMethods:            onms asVector.
            oracleForEagerRelocation: oracle.

            initializeForceNonLeaf: fnl.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForceNonLeaf: fnl = ( |
            | 
            initializeAllocatorsForceNonLeaf: fnl.
            codeGenerator: protoCodeGenForMyPlatform copyForCompiler: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isCandidateLeafMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isMethod = ( |
            | 
            method isReflecteeMethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: scoping\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         lexicalParentCount = ( |
            | 
            allocator lexicalParentCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         localSlotsForAllocator: a = ( |
            | vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: helper prototypes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         locations = ( |
            | 
            vmKit locations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot'
        
         log = ( |
            | codeGenerator log).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         method = ( |
            | 
            slot contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         methodStartInstruction = ( |
            | 
            (protoCodeGenForMyPlatform copyForCompiler: self) methodStartInstruction).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         nodesInControlFlowOrder = ( |
            | 
            startNode nodesInControlFlowOrder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         nodesInReverseSourceOrder = ( |
             r.
            | 
            "This is here just because it makes profiles cleaner - otherwise
             we could just call localReturnNode reverseSourceNodesDo: directly.
             -- Adam, 8/05"
            r: list copyRemoveAll.
            localReturnNode reverseSourceNodesDo: [|:n| r add: n].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         findOopForStubNMethodNamed: name IfPresent: pb IfAbsent: ab = ( |
            | 
            ab value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         nextOopToAllocateForObjectOfSize: nOops = ( |
            | 
            -1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOriginalObject: obj IfAbsent: fb = ( |
            | 
            fb value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         originalObjectForOop: oop IfAbsent: fb = ( |
            | 
            fb value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         relocatorAssembledPlaceholderInstructions = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'oracleThatCannotDoEagerRelocation' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         relocatorAssembledRealInstructions = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: helper prototypes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         protoAllocatorForMyPlatform = ( |
            | 
            [ppc. sparc]. "browsing"
            architecture sendTo: prototypes allocators).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: helper prototypes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         protoCodeGenForMyPlatform = ( |
            | 
            [ppc. sparc]. "browsing"
            architecture sendTo: prototypes codeGenerators).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot'
        
         prototypes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         irNodes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: special compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot'
        
         saveAllNonVolatileRegisters = ( |
            | 
            [_SaveAllNonVolatileRegisters]. "browsing"
            shouldSaveAllNonVolatileRegisters: true.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         selector = ( |
            | slot name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fComment: Runs through all the nodes and 
marks special modes for the primitives.
(sets flags on the compiler as required).
-- Dave and Alex, 5/05\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         setSpecialModes = ( |
            | 
            startNode sourceNodesDo: [|:n| n setSpecialMode].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: special compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot'
        
         setVariableArguments = ( |
            | 
            [_VariableArguments]. "browsing"
            "A _VariableArguments method may have argument slots bound to
             the first few (required) parameters.  There is currently no
             provision for accessing the variadic parameters because they
             are unnamed and there are certain complications involving
             determining the number of variadic parameters actually supplied.
             -- jb 8/03"

            variableArguments: true.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         slotHolderMirror = ( |
            | 
            slot holder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            codeGenerator vmKit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: compilation helpers\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: public'
        
         relocators <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: bytecode method information\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         selfMirror.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: special complation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         shouldSaveAllNonVolatileRegisters <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: bytecode method information\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         slot.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)'
        
         startNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> () From: ( | {
         'Category: special complation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         variableArguments <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         assignableSlot = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'assignableSlot' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'assignableSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s assignableSlot.

CopyDowns:
globals klein compiler1s abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'assignableSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'assignableSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s assignableSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'assignableSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot'
        
         compileSlot = ( |
            | 
            codeGenerator loadAssignableSlot: slot.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'assignableSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         assignmentSlot = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'assignmentSlot' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'assignmentSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s assignmentSlot.

CopyDowns:
globals klein compiler1s abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'assignmentSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'assignmentSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s assignmentSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'assignmentSlot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentSlotsForAllocator: a = ( |
            | 
            vector copyAddFirst: slots argument copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'assignmentSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot'
        
         compileSlot = ( |
            | 
            codeGenerator storeIntoAssignmentSlot: slot.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'assignmentSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         constantDataSlot = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'constantDataSlot' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'constantDataSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s constantDataSlot.

CopyDowns:
globals klein compiler1s abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'constantDataSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'constantDataSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s constantDataSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'constantDataSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot'
        
         compileSlot = ( |
            | 
            codeGenerator loadConstantSlot: slot.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'constantDataSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         methodSlot = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s methodSlot.

CopyDowns:
globals klein compiler1s abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> () From: ( | {
         'Category: compiling methods\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)'
        
         bytecodeInterpreter.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> () From: ( | {
         'Category: IR nodes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (nil)'
        
         irNodesByBCI.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> () From: ( | {
         'Category: compiling methods\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         nlrPoints <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s methodSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentSlotsForAllocator: a = ( |
            | 
            a argumentSlotsOfMethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         assignableLocalSlotsForAllocator: a = ( |
            | 
            a assignableLocalSlotsOfMethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         buildIRNodesByBCI = ( |
             bci <- 0.
            | 
            irNodesByBCI: vector copySize: method codes size succ "for epilogue"
                              FillingWith: localReturnNode.
            irNodes doFirst: ["skip prologue"] Middle: [|:n|
              bci to: n bci Do: [|:i| irNodesByBCI at: i Put: n].
              bci: n bci succ.
            ] Last: [].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: building nmethods\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         buildNMethod = ( |
             nm.
            | 
            nm: resend.buildNMethod.
            nm slotSPOffsets:  findSlotSPOffsets.
            nm pcOffsetsByBCI: pcOffsetsByBCI.
            nm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         calculateLocalLiveness = ( |
            | 
            [inProgress localLiveness]. "Right now this code is switched off. It doesn't really seem to have
                                         much effect on either export time or runtime. We could just remove it,
                                         but it might be useful later. One thing to note, though, is that the
                                         debugger hasn't yet been taught how to deal with uninitialized
                                         variables. -- Adam, 8/05"
            startNode controlFlowOrderDo: [|:n| n propagateBlockLiveness].
            startNode controlFlowOrderDo: [|:n| n namesOfLocalsThatMightBeAccessedBeforeBeingAssignedTo addAll: n namesOfLocalsThatMightBeAccessed].
            [| changed <- false |
              "Optimization: Go in reverse control-flow order, because the algorithm converges faster that
               way - the liveness information is flowing backwards from each node to its predecessors."
              nodesInControlFlowOrder reverseDo: [|:n| n calculateLocalLiveness ifTrue: [changed: true]].
              changed
            ] whileTrue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            (resend.copy
               bytecodeInterpreter: nil)    "will be recreated"
                         nlrPoints: nlrPoints copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         debug: bool = ( |
            | 
            resend.debug: bool.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         doCompile = ( |
            | 
            setPrologueAndEpilogueNodes.
            linkNodes.
            setSpecialModes.
            shouldCalculateLocalLiveness ifTrue: [calculateLocalLiveness].
            buildAndLinkValues.
            generateCode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: building nmethods\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         findSlotSPOffsets = ( |
             slotSPOffsets.
            | 
            slotSPOffsets: list copyRemoveAll.
            method allSlotsOnThisMethod do: [|:slot|
              slot isKleinSlotOffsetRecorded ifTrue: [
                slotSPOffsets addLast: (allocator locationForSlot: slot name) spOffsetForNMethod: codeGenerator.
              ].
            ].
            slotSPOffsets asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         forgeSourceOrderLinks = ( |
             n.
            | 
            irNodes doFirst: [|:nn| n: nn] MiddleLast: [|:nn|
              nn sourcePred: n.
              n  sourceSucc: nn.
              n: nn.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCode = ( |
            | 
            startNode controlFlowOrderDo: [|:sn|
              sn generateCode.
            ].
            "The assumption that the localReturn/nlr node is the last to 
             show in control flow order is false. Must bind labels in here
             then. -- Ausch, 5/05"
            codeGenerator generating: 'generateNLRPoints' During: [codeGenerator generateNLRPoints].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         irNodes = ( |
            | 
            bytecodeInterpreter ifNil: [
              bytecodeInterpreter: prototypes bytecodeInterpreter copyForCompiler: self.
              bytecodeInterpreter interpretMethod.
            ].
            bytecodeInterpreter irNodes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: method\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         isCandidateLeafMethod = ( |
            | 
            startNode sourceNodesDo: [|:n|
              n isOKInLeafMethod ifFalse: [^ false]
            ].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         linkNodes = ( |
            | 
            forgeSourceOrderLinks.
            buildIRNodesByBCI.
            forgeControlFlowLinks).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         localSlotsForAllocator: a = ( |
            | 
            a localSlotsOfMethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         outermostMethodHolder = ( |
            | 
            (outerNMethods lastIfAbsent: [^ slotHolderMirror reflectee]) methodHolder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: public'
        
         pcOffsetsByBCI = ( |
            | 
            irNodesByBCI asVector copyMappedBy: [|:n| n pcOffsetIfPresent: [|:o| o] IfAbsent: -1]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         setPrologueAndEpilogueNodes = ( |
            | 
                  startNode:  prototypes irNodes start       copyCompiler: self.
            localReturnNode:  prototypes irNodes localReturn copyCompiler: self.
            irNodes addFirst:       startNode.
            irNodes addLast:  localReturnNode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldCalculateLocalLiveness = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> 'parent' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinCompiler1 InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldOmitInitializationOfProvablyDeadLocals = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'methodSlot' -> () From: ( | {
         'Category: compilation modes\x7fModuleInfo: Module: kleinCompiler1 InitialContents: InitializeToExpression: (false)'
        
         shouldZapDeadLocations <- bootstrap stub -> 'globals' -> 'false' -> ().
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
             cs.
            | 
            cs: klein compiler1s.
            case if: [isAssignment] Then: [cs   assignmentSlot]
                 If: [isAssignable] Then: [cs   assignableSlot]
                 If: [isMethod    ] Then: [cs       methodSlot]
                                    Else: [cs constantDataSlot]).
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
