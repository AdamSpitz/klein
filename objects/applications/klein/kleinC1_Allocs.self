 '$Revision: 30.32 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocators = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes allocators.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes allocators abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAssignableLocalSlots.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: leaf method allocation policy\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedIsLeafMethod.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         compiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: scope-dependent state\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         frame.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: scope-dependent state\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         lexicalLevel <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: scope-dependent state\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         lexicalParentAllocator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: receiver, arguments, and locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         locationForIncomingReceiver.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: self\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         locationForSelf.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: memoized blocks\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (dictionary)\x7fVisibility: private'
        
         memoizedBlockLocations <- bootstrap stub -> 'globals' -> 'dictionary' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: scope-dependent state\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         method.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: receiver, arguments, and locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (dictionary)\x7fVisibility: private'
        
         namedLocations <- bootstrap stub -> 'globals' -> 'dictionary' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: scope-dependent state\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         nonVolLocalRegCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes allocators abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: expression stack\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateAnotherStackLocation = ( |
             r.
            | 
            r: makeAnotherStackLocation.
            stackLocations addLast: r.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateIncomingAndPreallocatedLocations = ( |
            | 
            "must do parents first"
            lexicalParentAllocator ifNotNil: [
              lexicalParentAllocator allocateIncomingAndPreallocatedLocations.
            ].

            setUplevelLocations.
            setNonVolatileRegSaveArea.
            setIncomingRcvrAndArgLocations.
            setLocalSlotLocations.
            setOnNonLocalReturnLocations.
            setLocationForSelf.
            setVolatileTemps).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateIncomingRcvrAndArgLocations = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: memoized blocks\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateLocationForMemoizedBlock: blockProto = ( |
            | 
            memoizedBlockLocations if: (reflect: blockProto)
                          IsPresentDo: []
                          IfAbsentPut: [makeAnotherMemoizedBlockLocation]
                                AndDo: [].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOutgoingRcvrAndArgLocations: howMany = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: expression stack\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateStackLocationFor: v = ( |
             r.
            | 
            [v hasLocation not] assert.
            v preferredScratchLocationIfPresent: [|:loc| (v isOKToAllocateTo: loc) ifTrue: [^ loc]]
                                       IfAbsent: [].
            stackLocations findFirst: [|:loc| v isOKToAllocateTo: loc ]
                           IfPresent: [|:loc| ^ loc].

            "Leaf with NLR will try to use r4, but that would interfere, so
             must loop below -- dmu 11/03.
             Is there a faster way?"
            [r: allocateAnotherStackLocation] untilTrue: [v isOKToAllocateTo: r].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateTemporaryRegister = ( |
            | 
            temps removeFirstIfAbsent: [error: 'ran out of temporary registers']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         allocatorAtLexicalLevel: ll = ( |
            | 
            ll = 0  ifTrue: [^ self].
            lexicalParentAllocator allocatorAtLexicalLevel: ll pred).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         architecture = ( |
            | compiler architecture).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         argumentCount = ( |
            | 
            argumentSlots size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         argumentSlots = ( |
            | 
            compiler argumentSlotsForAllocator: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         argumentSlotsOfMethod = ( |
            | 
            method argumentSlots).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         assignableLocalCount = ( |
            | 
            localSlots countHowMany: [|:slot| slot isAssignable]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         assignableLocalSlots = ( |
            | 
            compiler assignableLocalSlotsForAllocator: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         assignableLocalSlotsOfMethod = ( |
            | 
            cachedAssignableLocalSlots ifNil: [
              cachedAssignableLocalSlots: method assignableLocalSlots.
              cachedAssignableLocalSlots
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         backoutBlock = ( |
            | compiler backoutBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            (((((
            resend.copy
            temps:                       temps                       copyRemoveAll)
            stackLocations:              stackLocations              copyRemoveAll)
            memoizedBlockLocations:      memoizedBlockLocations      copyRemoveAll)
            namedLocations:              namedLocations              copyRemoveAll)
            locationForSelf:             nil)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForCompiler: c Method: m NMethod: nm ForceNonLeaf: fnl LexicalChildAllocator: lca = ( |
            | 
            copy initializeForCompiler: c Method: m NMethod: nm ForceNonLeaf: fnl LexicalChildAllocator: lca).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         freeTemporaryRegister: r = ( |
            | 
            temps
              if: r
              IsPresentDo: [error: 'duplicate free']
              IfAbsentPut: [r] AndDo: [].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForCompiler: c Method: m NMethod: nm ForceNonLeaf: fnl LexicalChildAllocator: lca = ( |
            | 
            compiler: c.
            method: m.
            cachedIsLeafMethod: fnl ifTrue: false False: nil.
            frame: protoFrameForMyPlatform copy.
            lca ifNotNil: [
              lca lexicalParentAllocator: self.
              lexicalLevel: lca lexicalLevel succ.
            ].
            nm ifNotNil: [
              initializeFromNMethod: nm.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeFromNMethod: nm = ( |
            | 
            [todo cleanup adam]. "nonVolLocalRegCount still makes me uneasy. It's ugly. It complicates the
                                  common case for the sake of the uncommon case."
            reserveSpaceForNonVolLocalRegs:           nm nonVolLocalRegCount.

            frame reserveSpaceToSaveNonVolRegs:       nm frame nonVolRegSaveAreaWordCount.
            frame reserveSpaceForOutgoingRcvrAndArgs: nm frame outgoingRcvrAndArgWordCount.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: leaf method allocation policy\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         isCandidateLeafMethod = ( |
            | 
            "Child should override to apply additional constraints."
            compiler isCandidateLeafMethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: leaf method allocation policy\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         isLeafMethod = ( |
            | 
            cachedIsLeafMethod ifNil: [
              cachedIsLeafMethod: isCandidateLeafMethod.
              cachedIsLeafMethod
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         isRegisterVolatile: r = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lexicalParentCount = ( |
            | 
            lexicalParentAllocator ifNil: [^ 0].
            lexicalParentAllocator lexicalParentCount succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         localSlots = ( |
            | 
            compiler localSlotsForAllocator: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         localSlotsOfMethod = ( |
            | 
            method localSlots).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationAt: name = ( |
            | 
            namedLocations at: name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: local slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForConstant: oop = ( |
            | 
            locations constant copyForOop: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: non-local return home scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingNLRHomeScope = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingRcvrOrArgAt: index = ( |
            | 
            locationForIncomingRcvrOrArgAt: index LexicalLevel: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForIncomingRcvrOrArgAt: index LexicalLevel: ll = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: message send result\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingResult = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForLocalBC: bc = ( |
            | 
            "set slot instead of bc selector
             so that bc for x: gets x not x:"
            locationForSlot: bc slot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: memoized blocks\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForMemoizedBlock: blockProto = ( |
            | 
            memoizedBlockLocations at:  reflect: blockProto).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: non-local return home scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingNLRHomeScope = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingRcvrOrArgAt: index = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingReceiver = ( |
            | locationForOutgoingRcvrOrArgAt: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: message send result\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingResult = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForSlot: slot = ( |
            | 
            locationAt: slot key).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForUnusedValue = ( |
            | 
            locations unusedValue copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: uplevel access to self, argument slots, and local slots\x7fComment: Sent to parent allocator.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForUplevelAccessTo: loc = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: uplevel access to self, argument slots, and local slots\x7fComment: Sent to parent allocator.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForUplevelAccessToSelf = ( |
            | 
            locationForUplevelAccessTo: locationForSelf).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prototypes\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locations = ( |
            | 
            compiler locations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationsForNonVolLocalMems = ( |
            | 
            frame locationsForNonVolLocals).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationsForOutgoingRcvrAndArgsUpTo: n = ( |
            | 
            (vector copySize: n) mapBy: [|:x. :i| locationForOutgoingRcvrOrArgAt: i]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationsThatCouldBeDead = ( |
            | 
            volatileRegisterLocationsThatCouldContainOops,
            locationsForNonVolLocalRegs,
            locationsForNonVolLocalMems).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationsThatNeedToBePreservedAcrossAnySend = ( |
             s.
            | 
            s: set copyRemoveAll.
            s add: locationForIncomingReceiver.
            s addAll: memoizedBlockLocations.
            locationFor_OnNLR_homeScope ifNotNil: [|:loc| s add: loc].
            s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: local slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAnotherAssignableLocalLocation = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: memoized blocks\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAnotherMemoizedBlockLocation = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: expression stack\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAnotherStackLocation = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: memoized blocks\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         memoizedBlockLocationsDo: blk = ( |
            | memoizedBlockLocations do: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: expression stack\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         memoryStackLocations = ( |
            | 
            stackLocations copyFilteredBy: [|:loc|
              loc isRegister not
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prototypes\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         protoFrameForMyPlatform = ( |
            | 
            klein stackFrames protoForArchitecture: architecture).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fComment: just a guess\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         requiredTempCount = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setIncomingRcvrAndArgLocations = ( |
            | 
            allocateIncomingRcvrAndArgLocations.
            locationForIncomingReceiver: locationForIncomingRcvrOrArgAt: 0.
            argumentSlots asVector do: [|:slot. :i. newLoc|
              newLoc: locationForIncomingRcvrOrArgAt: i + 1 "+ 1 is for the rcvr".
              namedLocations at: slot key Put: newLoc.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: local slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setLocalSlotLocations = ( |
            | 
            localSlots do: [|:slot. loc|
              loc: slot isAssignable
                     ifTrue: [makeAnotherAssignableLocalLocation]
                      False: [locationForConstant: slot contents reflectee].
              namedLocations at: slot key Put: loc
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: self\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setLocationForSelf = ( |
            | 
            locationForSelf: 
             lexicalParentAllocator  
              ifNil:    [ locationForIncomingReceiver ] 
              IfNotNil: [ lexicalParentAllocator locationForUplevelAccessToSelf ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: non-volatile registers\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setNonVolatileRegSaveArea = ( |
            | 
            compiler shouldSaveAllNonVolatileRegisters ifTrue: [
              frame reserveSpaceToSaveNonVolRegs: nonVolRegisterCount.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: non-local return home scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setOnNonLocalReturnLocations = ( |
            | 
            compiler hasOnNonLocalReturn ifFalse: [^ self].
            locationFor_OnNLR_homeScope:     makeAnotherNonVolLocalRegOrMemLocation.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: uplevel access to self, argument slots, and local slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setUplevelLocations = ( |
            | 
            lexicalParentAllocator ifNil: [ ^ self ].

            lexicalParentAllocator namedLocations do: [|:loc. :name|
                namedLocations  at:  name
                               Put:  lexicalParentAllocator locationForUplevelAccessTo: loc.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setVolatileTemps = ( |
            | 
            requiredTempCount do: [|:i| temps add: tempRegisterAt: i].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         tempRegisterAt: i = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: assertions\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyIndex: index IsWithinAllocatedLimit: limit = ( |
            | 
            [(0 <= index) && [index < limit]]  
              assert: 'this location has not been allocated'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         volatileRegisterLocationsThatCouldContainOops = ( |
            | 
            volatileRegisterLocations asSet copyFilteredBy: [|:loc| loc register != sp]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         withTemporaryRegisterDo: blk = ( |
             r.
            | 
            [ 
              r: allocateTemporaryRegister.
              blk value: r
            ] onReturn: [ freeTemporaryRegister: r]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: expression stack\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         stackLocations <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         temps <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot'
        
         ppc = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes allocators abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes allocators ppc.

CopyDowns:
globals klein compiler1s abstract parent prototypes allocators abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> () From: ( | {
         'Category: _OnNonLocalReturn\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)'
        
         locationFor_OnNLR_homeScope.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( |
             {} = 'Comment: Note about nomenclature
- makeAnotherXXX: allocate something new
                  (always returns a new location)
- locationForXXX: return the uniquely determined location for something
                  (always returns an equivalent location)\x7fModuleInfo: Creator: globals klein compiler1s abstract parent prototypes allocators ppc parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateIncomingRcvrAndArgLocations = ( |
            | 
            reserveSpaceForNonVolLocalRegs: incomingRcvrAndArgNonVolRegisterCount.
            volatileRegisterCount: incomingRcvrAndArgVolatileRegisterCount.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOutgoingRcvrAndArgLocations: howMany = ( |
            | 
            [isLeafMethod not] assert: 'leaf methods cannot allocate outgoing arguments (no stack frame)'.

            reserveSpaceForOutgoingRcvrAndArgs: howMany).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         byteMapBaseRegister = ( |
            | 
            lowestGlobalRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         firstTempRegister = ( |
            | 
            r11).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         highestGlobalRegister = ( |
            | 
            objectAddressesBaseRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         highestNonVolRegister = ( |
            | r31).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: stack\x7fComment: We don\'t actually apply this constraint at the moment.
Due to the order of allocation, stack registers will always follow
the saved incoming argument registers, and the assignable local
registers, but may be intermingled with memoized block registers.
-- jb 8/03\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         highestStackRegister = ( |
            | 
            gprFor: lastIncomingArgumentRegister number pred).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: incoming receiver and arguments\x7fComment: The number of incoming arguments that have been passed
  in volatile registers.
For _VariableArguments, we return maxArgumentRegisters
  because there there is no way to determine how many
  arguments were actually passed by the caller. -- jb 8/03\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         incomingArgVolatileRegisterCount = ( |
            | 
            compiler variableArguments
              ifTrue: [ maxArgumentRegisters                    ]
               False: [ maxArgumentRegisters min: argumentCount ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: incoming receiver and arguments\x7fComment: The number of incoming receiver and arguments in total.
For _VariableArguments, we return maxSmallInt because 
  there is no way to determine how many arguments were
  actually passed by the caller. -- jb 8/03\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         incomingRcvrAndArgCount = ( |
            | 
            compiler variableArguments
              ifTrue: [ maxSmallInt "unknown number of variadic parameters" ]
               False: [ argumentCount + 1 "for receiver" ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: incoming receiver and arguments\x7fComment: The number of incoming receiver and arguments to be copied
  to non-volatile registers.
For leaf methods, we never copy the arguments to
  non-volatile memory because that would destroy the
  caller\'s stack frame. -- jb 8/03\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         incomingRcvrAndArgNonVolRegisterCount = ( |
            | 
            isLeafMethod
              ifTrue:   0
               False: [ incomingRcvrAndArgVolatileRegisterCount ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: incoming receiver and arguments\x7fComment: The number of incoming receiver and argument registers
  to be saved to the stack for uplevel access from blocks.
For methods with blocks, we save the receiver and all
  arguments bound to slots.  In the _VariableArguments
  case, we never save the variadic arguments because
  there is no way to determine how many arguments
  were actually passed by the caller.
For methods without blocks, we do not save any
  registers because there cannot be any uplevel access.
For leaf methods, we do likewise because they cannot
  have any blocks.  -- jb 8/03\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         incomingRcvrAndArgSavedRegisterCount = ( |
            | 
            isLeafMethod        ifTrue:  [^ 0].
            method hasBlocks    ifFalse: [^ 0].
            argumentCount + 1 "for receiver").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: incoming receiver and arguments\x7fComment: Includes the receiver.
See incomingArgVolatileRegisterCount.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         incomingRcvrAndArgVolatileRegisterCount = ( |
            | 
            incomingArgVolatileRegisterCount + 1 "for receiver").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: incoming\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         incomingRcvrOrArgRegisterAt: i = ( |
            | gprFor: incomingReceiverRegister number - i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: incoming\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         incomingReceiverRegister = ( |
            | 
            r31).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         incomingVolatileRegRcvrAndArgLocationsDo: blk = ( |
            | 
            incomingRcvrAndArgVolatileRegisterCount  do: [|:i|
              blk value: (locationForIncomingVolatileRegRcvrOrArgAt: i)
                   With: i
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: leaf method allocation policy\x7fComment: ALLOCATING LOCALS AND STACK EXPRESSION LOCATIONS FOR LEAF METHODS
=================================================================
(revised transcript from an eMail conversation)

Leaf methods are part of an optimization to avoid creating a new stack
frame for very simple methods.  So instead of creating their own, they
share their caller\'s stack frame.

Constraints:

- Leaf methods receive arguments in r3-r10, and in the outgoing memory
  argument words in the current frame.
- Leaf methods cannot (in general) send messages or invoke primitives
  because that would require putting values into r3-r10, obliterating
  the incoming receiver and arguments.
- Leaf methods cannot (in general) store values into the non-volatile
  registers r14-r31 because they are not saved/restored to/from the
  stack in the method prologue/epilogue.
- Leaf methods cannot contain any blocks.
- Leaf methods may have assignable local slots.  (or should they?)
  ==> where do we allocate them?
- Leaf methods may have an expression stack. (or should they?)
  ==> where do we allocate it?

The older allocator permitted the last two cases but could produce
inappropriate allocations.  For example:

r3 - incoming receiver register
r4 - incoming arg #1
r5 - a local assignable slot
r6 - a local constant slot (reserved but unused, new allocator is smarter)
r7 - a local assignable slot
r29 - stack register #2
r30 - stack register #1
r31 - stack register #0

Observations:

1. Allocation of locals to the r3-r10 block of volatile registers
   following the incoming receiver and arguments appears to be an
   accidental side-effect of the original code.  It neither detects
   nor handles the overflow case, and will destroy the caller\'s stack
   frame when supplied a sufficient number of locals.

   We need a solution to this problem.

2. Allocation of stack locations to the r13-r31 block of non-volatile
   registers is incorrect as it will destroy the non-volatile portion
   of the caller\'s stack frame.  We can allocate a limited number
   of stack locations to the volatile registers.  However, it is 
   difficult to determine how many we will require ahead of time.

   We need a solution to this problem.


Solutions for allocating assignable local slots:

1. Add a rule to prevent leaf methods from having ANY assignable
   local slots.
   Note: Constant slots are not a problem because they do not
         require an allocation.

2. Allocate local and expression stack locations into the
   r3-10 block of volatile registers following the incoming receiver
   and arguments, somewhat like the old allocator did.
   Handling overflow:
   
   a. Detect it ahead of time and do not use the leaf method optimization.
   b. Detect it just in time and spill allocations into unallocated
      stack space at a negative offset from the current frame pointer.
      I believe the PPC ABI allows using a limited amount of this space.
   c. Prevent leaf methods from having more local assignable slots
      than will fit into the available volatile registers.
        eg. #incoming args + #assignable locals <= maxArgumentRegisters
            (verify this ahead of time)
      Furthermore, detect just in time when the allocator attempts to
      create more stack location than will fit.  Then back out of
      the optimization (very uncommon case).
        eg. #incoming args + #assignable locals + #stack regs <= ...
            (verify this just in time, and back out if necessary)

Proposal: Go with 2c because it is safe, powerful, and easy to implement.
          -- jb 8/03

Response: I agree--2c is the way to go.
          2b invites bugs.
          -- dmu 8/03\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         isCandidateLeafMethod = ( |
            | 
            "Necessary, but not sufficient condition for leaf methods..."
                 resend.isCandidateLeafMethod
            && [ (incomingArgVolatileRegisterCount + assignableLocalCount)  <=  maxArgumentRegisters ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         isRegisterVolatile: r = ( |
            | 
            r number < lowestNonVolRegister number).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: assignable local slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lastAssignableLocalRegister = ( |
            | 
            lastIncomingArgumentRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: incoming\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lastIncomingArgumentRegister = ( |
            | 
            gprFor: incomingReceiverRegister number - maxArgumentRegisters).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: memoized blocks\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lastMemoizedBlockRegister = ( |
            | 
            lastAssignableLocalRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         lastOutgoingArgumentRegister = ( |
            | 
            gprFor: outgoingReceiverRegister number + maxArgumentRegisters).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lastTempRegister = ( |
            | 
            r12).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fComment: Used by compiler to copy incoming arguments to the stack.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingMemRcvrOrArgAt: index = ( |
            | 
            [isLeafMethod not] assert.
            locationForIncomingMemRcvrOrArgAt: index LexicalLevel: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fComment: Return location for accessing index\'th rcvr or arg
in me from a frame that is ll levels down.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForIncomingMemRcvrOrArgAt: index LexicalLevel: ll = ( |
            | 
            verifyIndex: index IsWithinAllocatedLimit: incomingRcvrAndArgCount.

            locations incomingMemoryArgument copyRcvrAndArgNo: index LexicalLevel: ll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: non-local return home scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingNLRHomeScope = ( |
            | 
            locationForOutgoingNLRHomeScope).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForIncomingRcvrOrArgAt: index LexicalLevel: ll = ( |
            | 
            verifyIndex: index IsWithinAllocatedLimit: incomingRcvrAndArgCount.

            case 
              if:   [isLeafMethod] 
              Then: [
                     "Since we don't have a stack frame, we retrieve the argument
                      from its outgoing location in the current frame."
                     [ll = 0] assert. "no blocks in leaf method so how did you get here?"
                     locationForVolatileRcvrOrArgAt: index
              ]
              If:   [(ll = 0) && [index <= maxArgumentRegisters]]
              Then: [locationForIncomingRegRcvrOrArgAt: index]

              Else: [locationForIncomingMemRcvrOrArgAt: index LexicalLevel: ll]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fComment: Used by compiler to copy incoming arguments to non-volatile registers.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingRegRcvrOrArgAt: index = ( |
            | 
            [isLeafMethod not] assert.
            verifyIndex: index IsWithinAllocatedLimit: incomingRcvrAndArgNonVolRegisterCount.

            locationForNonVolLocalRegAt: index).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: message send result\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingResult = ( |
            | locationForOutgoingResult).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fComment: Used by compiler to copy incoming arguments to non-volatile registers.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForIncomingVolatileRegRcvrOrArgAt: index = ( |
            | 
            [isLeafMethod not] assert.
            verifyIndex: index IsWithinAllocatedLimit: incomingRcvrAndArgVolatileRegisterCount.

            locationForVolatileRegRcvrOrArgAt: index IfNoSuchIndex: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForNonVolLocalMemAt: index = ( |
            | 
            [isLeafMethod not] assert: 'leaf methods cannot allocate non-volatile storage (no stack frame)'.
            reserveSpaceForNonVolLocalMemWords: index succ.

            locations nonVolMemoryLocal copyIndex: index).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForNonVolLocalRegAt: index = ( |
             regNumber.
            | 
            [isLeafMethod not] assert: 'leaf methods cannot allocate non-volatile storage (no stack frame)'.
            regNumber: highestNonVolRegister number - index.
            [regNumber >= lowestNonVolRegister number] assert.

            reserveSpaceForNonVolLocalRegs: index succ.
            locations register copyForRegister: gprFor: regNumber).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: non-local return home scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingNLRHomeScope = ( |
            | 
            locationForVolatileRcvrOrArgAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingRcvrOrArgAt: index = ( |
            | 
            verifyIndex: index IsWithinAllocatedLimit: outgoingRcvrAndArgCount.

            locationForVolatileRcvrOrArgAt: index).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: message send result\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingResult = ( |
            | locations register copyForRegister: outgoingResultRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: uplevel access to self, argument slots, and local slots\x7fComment: Sent to parent allocator to create location
for lexical child scope to use to access loc.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForUplevelAccessTo: loc = ( |
             index.
            | 

            [method hasBlocks] assert.  "otherwise how did you get here?"

            loc isRegister  ifFalse: [^ loc copyForUplevelAccess].


            "Since locals of methods with blocks are allocated into memory (I hope),
             if loc isRegister it must be receiver or argument."

            index: highestNonVolRegister number - loc register number.
            verifyIndex:              index
              IsWithinAllocatedLimit: incomingRcvrAndArgSavedRegisterCount.

            locationForIncomingRcvrOrArgAt: index LexicalLevel: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: volatile incoming, leaf method incoming, and outgoing receiver and arguments (use when parts of stack frame look like caller\'s)\x7fComment: Makes a location for a memory argument used to
pass arguments out of caller frame or into leaf
method frame. \x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForVolatileRcvrOrArgAt: index = ( |
            | 
            locationForVolatileRegRcvrOrArgAt: index IfNoSuchIndex: [
              locations outgoingMemoryArgument copyRcvrAndArgNo: index
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: volatile incoming, leaf method incoming, and outgoing receiver and arguments (use when parts of stack frame look like caller\'s)\x7fComment: Makes a location for a volatile register used to
pass arguments out of caller frame or into callee frame.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForVolatileRegRcvrOrArgAt: index IfNoSuchIndex: fb = ( |
            | 
            index <= maxArgumentRegisters  ifTrue: [
              locations register copyForRegister:
                gprFor: outgoingReceiverRegister number + index
            ]
            False: [fb value: 'no such index']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationsForNonVolLocalRegs = ( |
            | 
            (vector copySize: nonVolLocalRegCount) copyMappedBy: [|:x. :i|
              locations register copyForRegister: gprFor: highestNonVolRegister number - i
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lowestGlobalRegister = ( |
            | 
            lowestNonVolRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lowestLocalNonVolRegister = ( |
            | 
            gprFor: highestGlobalRegister number succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lowestNonVolRegister = ( |
            | 
            gprFor: lastTempRegister number succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: stack\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lowestStackRegister = ( |
            | 
            gprFor: highestGlobalRegister number succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: local slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAnotherAssignableLocalLocation = ( |
            | 
            case
            if: [ isLeafMethod ]  Then: [
              "If the method is a leaf, we always allocate its locals in
               volatile registers because there is no stack frame.  -- jb 8/03"

              makeAnotherVolatileRegisterLocation
            ]
            If: [ method hasBlocks ]  Then: [
              "If the method has blocks, some locals might be uplevel accessed
               and so must not be stored in registers.  At present we do not
               try to determine the precise set of such locals so we allocate
               all of them in non-volatile memory.  -- jb 8/03"

              makeAnotherNonVolLocalMemLocation
            ]
            Else: [
              "Otherwise, we are free to allocate the local slot into
               a register for best performance in the typical case. -- jb 8/03"

              makeAnotherNonVolLocalRegOrMemLocation
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: memoized blocks\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAnotherMemoizedBlockLocation = ( |
            | makeAnotherNonVolLocalLocationLimitReg: lastMemoizedBlockRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAnotherNonVolLocalLocationLimitReg: reg = ( |
            | 
            makeAnotherNonVolLocalRegLocationLimitReg: reg IfFail: [
              makeAnotherNonVolLocalMemLocation
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAnotherNonVolLocalMemLocation = ( |
            | 
            locationForNonVolLocalMemAt: nonVolLocalMemWordCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAnotherNonVolLocalRegLocationLimitReg: limit IfFail: fb = ( |
            | 
            nonVolLocalRegCount <= (highestNonVolRegister number - limit number)  ifTrue: [
              locationForNonVolLocalRegAt: nonVolLocalRegCount
            ] False: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: local slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAnotherNonVolLocalRegOrMemLocation = ( |
            | 
            makeAnotherNonVolLocalLocationLimitReg: lastAssignableLocalRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: expression stack\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAnotherStackLocation = ( |
            | 
            isLeafMethod
              ifTrue: [ makeAnotherVolatileRegisterLocation ]
               False: [ makeAnotherNonVolLocalLocationLimitReg: lowestStackRegister ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating volatile registers\x7fComment: If the method is a leaf, we allow a certain number of
locals and stack locations to be allocated to volatile
registers.  If it turns out that we don\'t have enough
registers, then we back out of the leaf method
optimization and try again without it.  -- jb 8/03\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAnotherVolatileRegisterLocation = ( |
             index.
            | 
            [isLeafMethod] assert.
            index: volatileRegisterCount.
            volatileRegisterCount: volatileRegisterCount succ.
            locationForVolatileRegRcvrOrArgAt: index
                                IfNoSuchIndex: [ backoutBlock value: 'no more volatile registers' ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fComment: w/o receiver
both in and out\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         maxArgumentRegisters = 7.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         nonVolLocalMemWordCount = ( |
            | frame nonVolLocalWordCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         nonVolRegisterCount = ( |
            | 
            (highestNonVolRegister number - lowestNonVolRegister number) succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         objectAddressesBaseRegister = ( |
            | 
            gprFor: spLimitRegister number succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         outgoingRcvrAndArgCount = ( |
            | frame outgoingRcvrAndArgWordCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         outgoingRcvrOrArgRegisterAt: i = ( |
            | 
            gprFor: outgoingReceiverRegister number + i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         outgoingReceiverRegister = ( |
            | 
            r3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         outgoingResultRegister = ( |
            | 
            outgoingReceiverRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         registers* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         requiredTempCount = ( |
            | 
            lastTempRegister number succ - firstTempRegister number).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         reserveSpaceForNonVolLocalMemWords: howMany = ( |
            | 
            frame reserveSpaceForNonVolLocals: howMany).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         reserveSpaceForNonVolLocalRegs: howMany = ( |
            | 
            frame reserveSpaceToSaveNonVolRegs: howMany.
            nonVolLocalRegCount: nonVolLocalRegCount max: howMany.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         reserveSpaceForOutgoingRcvrAndArgs: howMany = ( |
            | frame reserveSpaceForOutgoingRcvrAndArgs: howMany).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         spLimitRegister = ( |
            | 
            gprFor: byteMapBaseRegister number succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         tempRegisterAt: i = ( |
            | 
            gprFor: firstTempRegister number + i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         volatileRegisterLocations = ( |
            | 
            (vector copySize: lowestNonVolRegister number) copyMappedBy: [|:x. :i| locations register copyForRegister: gprFor: i]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> () From: ( | {
         'Category: volatile registers\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         volatileRegisterCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot'
        
         kleinC1_Allocs = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Allocs' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Allocs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinC1_Allocs.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Allocs' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Allocs' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Allocs' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Allocs' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Allocs' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.32 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Allocs' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinC1_Allocs postFileIn
