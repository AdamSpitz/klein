 '$Revision: 30.32 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocators = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes allocators.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes allocators abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         allValues <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAssignableLocalSlots.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: receiver, arguments, and locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedValueForIncomingResult.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: receiver, arguments, and locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedValueForOutgoingResult.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: self\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         cachedValueForSelf.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         compiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: scope-dependent state\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         frame.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: leaf method allocation policy\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         isLeafMethod.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: scope-dependent state\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         lexicalParentAllocator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: memoized blocks\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (dictionary)\x7fVisibility: private'
        
         memoizedBlockValues <- bootstrap stub -> 'globals' -> 'dictionary' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: scope-dependent state\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         method.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: receiver, arguments, and locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (dictionary)\x7fVisibility: private'
        
         namedValues <- bootstrap stub -> 'globals' -> 'dictionary' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: scope-dependent state\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         nonVolLocalRegCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: receiver, arguments, and locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         outgoingRcvrAndArgValues <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes allocators abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateIncomingAndPreallocatedLocations = ( |
            | 
            setUplevelLocations.
            setNonVolatileRegSaveArea.
            setIncomingRcvrAndArgLocations.
            setLocalSlotLocations.
            setLocationForSelf.
            setVolatileTemps).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateIncomingRcvrAndArgLocations = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOutgoingRcvrAndArgLocations: howMany = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateTemporaryRegister = ( |
            | 
            temps removeFirstIfAbsent: [error: 'ran out of temporary registers']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         allocatorAtLexicalLevel: ll = ( |
            | 
            ll = 0  ifTrue: [^ self].
            lexicalParentAllocator allocatorAtLexicalLevel: ll pred).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         architecture = ( |
            | compiler architecture).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         argumentCount = ( |
            | 
            argumentSlots size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         argumentSlots = ( |
            | 
            compiler argumentSlotsForAllocator: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         argumentSlotsOfMethod = ( |
            | 
            method argumentSlots).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         assignableLocalCount = ( |
            | 
            localSlots countHowMany: [|:slot| slot isAssignable]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         assignableLocalSlots = ( |
            | 
            compiler assignableLocalSlotsForAllocator: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         assignableLocalSlotsOfMethod = ( |
            | 
            cachedAssignableLocalSlots ifNil: [
              cachedAssignableLocalSlots: method assignableLocalSlots.
              cachedAssignableLocalSlots
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         backoutBlock = ( |
            | compiler backoutBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            ((((((
            resend.copy
            temps:                       temps                       copyRemoveAll)
            allValues:                   allValues                   copyRemoveAll)
            stackValues:                 stackValues                 copyRemoveAll)
            memoizedBlockValues:         memoizedBlockValues         copyRemoveAll)
            namedValues:                 namedValues                 copyRemoveAll)
            cachedValueForSelf:          nil)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForCompiler: c Method: m ForceNonLeaf: fnl = ( |
            | 
            copy initializeForCompiler: c Method: m ForceNonLeaf: fnl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         freeTemporaryRegister: r = ( |
            | 
            temps
              if: r
              IsPresentDo: [error: 'duplicate free']
              IfAbsentPut: [r] AndDo: [].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForCompiler: c Method: m ForceNonLeaf: fnl = ( |
            | 
            compiler: c.
            method: m.
            frame: protoFrameForMyPlatform copy.
            isLeafMethod: fnl not.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         isRegisterVolatile: r = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lexicalParentCount = ( |
            | 
            lexicalParentAllocator ifNil: 0 IfNotNil: [|:lpa| lpa lexicalParentCount succ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         localSlots = ( |
            | 
            compiler localSlotsForAllocator: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         localSlotsOfMethod = ( |
            | 
            method localSlots).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: constants\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForConstant: oop = ( |
            | 
            locations constant copyForOop: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: non-local return home scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingNLRHomeScope = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingRcvrOrArgAt: index = ( |
            | 
            locationForIncomingRcvrOrArgAt: index LexicalLevel: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForIncomingRcvrOrArgAt: index LexicalLevel: ll = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingReceiver = ( |
            | 
            valueForIncomingReceiver location).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: message send result\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingResult = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: non-local return home scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingNLRHomeScope = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingRcvrOrArgAt: index = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingReceiver = ( |
            | locationForOutgoingRcvrOrArgAt: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: message send result\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingResult = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: self\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForSelf = ( |
            | 
            valueForSelf location).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForSlot: slot = ( |
            | 
            (valueForSlot: slot) location).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForUnusedValue = ( |
            | 
            locations unusedValue copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: uplevel access to self, argument slots, and local slots\x7fComment: Sent to parent allocator.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForUplevelAccessTo: loc = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: uplevel access to self, argument slots, and local slots\x7fComment: Sent to parent allocator.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForUplevelAccessToSelf = ( |
            | 
            locationForUplevelAccessTo: locationForSelf).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prototypes\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locations = ( |
            | 
            compiler locations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationsForNonVolLocalMems = ( |
            | 
            frame locationsForNonVolLocals).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationsForOutgoingRcvrAndArgsUpTo: n = ( |
            | 
            (vector copySize: n) mapBy: [|:x. :i| locationForOutgoingRcvrOrArgAt: i]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationsThatCouldBeDead = ( |
            | 
            locationsForNonVolLocalRegs,
            locationsForNonVolLocalMems).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationsThatNeedToBePreservedAcrossAnySend = ( |
             s.
            | 
            s: set copyRemoveAll.
            s add: locationForIncomingReceiver.
            s addAll: memoizedBlockLocations.
            valueFor_OnNLR_homeScope ifNotNil: [|:v| s add: v location].
            s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: local slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAnotherAssignableLocalLocation = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: memoized blocks\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         memoizedBlockLocations = ( |
            | 
            memoizedBlockValues copyMappedBy: [|:v| v location]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: memoized blocks\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         memoizedBlockLocationsDo: blk = ( |
            | memoizedBlockLocations do: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: expression stack\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         memoryStackValues = ( |
            | 
            stackValues copyFilteredBy: [|:v|
              v location isRegister not
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: expression stack\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         newStackValue = ( |
             v.
            | 
            v: newValue.
            stackValues add: v.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         newValue = ( |
             v.
            | 
            v: compiler prototypes dataValue copyCompiler: compiler.
            allValues add: v.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         newValueWithLocation: loc = ( |
             v.
            | 
            v: newValue.
            v location: loc.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: prototypes\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         protoFrameForMyPlatform = ( |
            | 
            klein stackFrames protoForArchitecture: architecture).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fComment: just a guess\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         requiredTempCount = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setIncomingRcvrAndArgLocations = ( |
            | 
            allocateIncomingRcvrAndArgLocations.
            valueForIncomingReceiver: newValueWithLocation: locationForIncomingRcvrOrArgAt: 0.
            argumentSlots asVector do: [|:slot. :i. newLoc|
              newLoc: locationForIncomingRcvrOrArgAt: i + 1 "+ 1 is for the rcvr".
              (valueForSlot: slot) myLocation: newLoc. "Override the existing location (from a lexical parent), if any."
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: local slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setLocalSlotLocations = ( |
            | 
            localSlots do: [|:slot. loc. v|
              v: valueForSlot: slot.
              slot isAssignable ifTrue: [
                ifNecessaryPreallocateLocationForAssignableLocalValue: v.
              ] False: [
                v location: locationForConstant: slot contents reflectee.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: self\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setLocationForSelf = ( |
            | 
            valueForSelf location: 
             lexicalParentAllocator  
              ifNil:    [ locationForIncomingReceiver ]
              IfNotNil: [ lexicalParentAllocator locationForUplevelAccessToSelf ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: non-volatile registers\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setNonVolatileRegSaveArea = ( |
            | 
            compiler shouldSaveAllNonVolatileRegisters ifTrue: [
              frame reserveSpaceToSaveNonVolRegs: nonVolRegisterCount.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: uplevel access to self, argument slots, and local slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setUplevelLocations = ( |
            | 
            lexicalParentAllocator ifNil: [ ^ self ].

            lexicalParentAllocator namedValues do: [|:v. :name|
              (namedValues at: name IfAbsentPut: [newValue]) location: lexicalParentAllocator locationForUplevelAccessTo: v location.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setVolatileTemps = ( |
            | 
            requiredTempCount do: [|:i| temps add: tempRegisterAt: i].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         tempRegisterAt: i = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: constants\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForConstant: oop = ( |
            | 
            newValueWithLocation: locationForConstant: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForIncomingRcvrOrArgAt: i = ( |
            | 
            newValueWithLocation: locationForIncomingRcvrOrArgAt: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: message send result\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForIncomingResult = ( |
            | 
            cachedValueForIncomingResult ifNil: [
              cachedValueForIncomingResult: newValueWithLocation: locationForIncomingResult.
              cachedValueForIncomingResult
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForLocalBC: bc = ( |
            | 
            "set slot instead of bc selector
             so that bc for x: gets x not x:"
            valueForSlot: bc slot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: memoized blocks\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForMemoizedBlockMirror: blockProtoMir = ( |
            | 
            memoizedBlockValues if: blockProtoMir
                       IsPresentDo: [|:v| v]
                       IfAbsentPut: [newValue]
                             AndDo: [|:v| v]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForOutgoingRcvrAndArgAt: index = ( |
            | 
            outgoingRcvrAndArgValues size <= index ifTrue: [
              outgoingRcvrAndArgValues: outgoingRcvrAndArgValues copySize: index succ.
            ].
            (outgoingRcvrAndArgValues at: index) ifNil: [| v |
              v: newValueWithLocation: locationForOutgoingRcvrOrArgAt: index.
              outgoingRcvrAndArgValues at: index Put: v.
              v
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForOutgoingReceiver = ( |
            | 
            valueForOutgoingRcvrAndArgAt: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: message send result\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForOutgoingResult = ( |
            | 
            cachedValueForOutgoingResult ifNil: [
              cachedValueForOutgoingResult: newValueWithLocation: locationForOutgoingResult.
              cachedValueForOutgoingResult
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: self\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForSelf = ( |
            | 
            cachedValueForSelf ifNil: [
              cachedValueForSelf: newValue.
              cachedValueForSelf
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForSlot: slot = ( |
            | 
            namedValues at: slot key IfAbsentPut: [| v |
              v: newValue.
              v slot: slot.
              v
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: assertions\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyIndex: index IsWithinAllocatedLimit: limit = ( |
            | 
            [(0 <= index) && [index < limit]]  
              assert: 'this location has not been allocated'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         volatileRegisterLocationsThatCouldContainOops = ( |
            | 
            volatileRegisterLocationsUsableForAssignableLocalsInLeafMethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         withTemporaryRegisterDo: blk = ( |
             r.
            | 
            [ 
              r: allocateTemporaryRegister.
              blk value: r
            ] onReturn: [ freeTemporaryRegister: r]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: expression stack\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         stackValues <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         temps <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> () From: ( | {
         'Category: receiver, arguments, and locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         valueForIncomingReceiver.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot'
        
         ppc = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes allocators abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes allocators ppc.

CopyDowns:
globals klein compiler1 parent prototypes allocators abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( |
             {} = 'Comment: Note about nomenclature
- makeAnotherXXX: allocate something new
                  (always returns a new location)
- locationForXXX: return the uniquely determined location for something
                  (always returns an equivalent location)\x7fModuleInfo: Creator: globals klein compiler1 parent prototypes allocators ppc parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateIncomingRcvrAndArgLocations = ( |
            | 
            reserveSpaceForNonVolLocalRegs: incomingRcvrAndArgNonVolRegisterCount.
            volatileRegisterCount: incomingRcvrAndArgVolatileRegisterCount.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOutgoingRcvrAndArgLocations: howMany = ( |
            | 
            [isLeafMethod not] assert: 'leaf methods cannot allocate outgoing arguments (no stack frame)'.

            reserveSpaceForOutgoingRcvrAndArgs: howMany).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: new register allocation algorithm\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         availableNonVolatileRegisterLocations = ( |
             highestAvailableRegNumber.
            | 
            highestAvailableRegNumber: highestNonVolRegister number - nonVolLocalRegCount.
            (vector copySize: highestAvailableRegNumber succ - lowestLocalNonVolRegister number) mapBy: [|:x. :i|
              "Start from the highest rather than the lowest, so that the used registers will be contiguous."
              locationForRegister: gprFor: highestAvailableRegNumber - i
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: new register allocation algorithm\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         availableRegisterLocations = ( |
            | 
            isLeafMethod ifTrue: [
              volatileRegisterLocationsUsableForAssignableLocalsInLeafMethod
            ] False: [
              availableNonVolatileRegisterLocations
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         byteMapBaseRegister = ( |
            | 
            lowestGlobalRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         firstTempRegister = ( |
            | 
            r11).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         highestGlobalRegister = ( |
            | 
            objectAddressesBaseRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         highestNonVolRegister = ( |
            | 
            r31).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: local slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         ifNecessaryPreallocateLocationForAssignableLocalValue: v = ( |
            | 
            method hasBlocks ifTrue: [
              "If the method has blocks, some locals might be uplevel accessed
               and so must not be stored in registers.  At present we do not
               try to determine the precise set of such locals so we allocate
               all of them in non-volatile memory.  -- jb 8/03"

              v location: makeAnotherNonVolLocalMemLocation.

            ] False: [
              "Otherwise, we are free to allocate the local slot into
               a register for best performance in the typical case. -- jb 8/03"
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
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
            method hasBlocks    ifFalse: [^ 0]. [aaa]. "Is this still right?"
            argumentCount + 1 "for receiver").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         incomingRcvrAndArgVolatileRegisterCount = ( |
            | 
            incomingArgVolatileRegisterCount + 1 "for receiver").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: incoming\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         incomingRcvrOrArgRegisterAt: i = ( |
            | 
            gprFor: incomingReceiverRegister number - i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: incoming\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         incomingReceiverRegister = ( |
            | 
            r31).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         incomingVolatileRegRcvrAndArgLocationsDo: blk = ( |
            | 
            incomingRcvrAndArgVolatileRegisterCount  do: [|:i|
              blk value: (locationForIncomingVolatileRegRcvrOrArgAt: i)
                   With: i
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         isRegisterVolatile: r = ( |
            | 
            r number < lowestNonVolRegister number).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: new register allocation algorithm\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         lastAvailableRegisterThatWasActuallyAssigned: r = ( |
            | 
            isLeafMethod ifTrue: [
              lastVolatileRegisterThatWasActuallyAssigned: r.
            ] False: [
              lastNonVolatileRegisterThatWasActuallyAssigned: r.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lastNonVolatileRegisterThatWasActuallyAssigned: r = ( |
            | 
            [isLeafMethod not] assert: 'leaf methods cannot allocate non-volatile storage (no stack frame)'.
            [(isRegisterVolatile: r) not] assert.
            reserveSpaceForNonVolLocalRegs: (highestNonVolRegister number - r number) succ.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         lastOutgoingArgumentRegister = ( |
            | 
            outgoingRcvrOrArgRegisterAt: maxArgumentRegisters).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lastTempRegister = ( |
            | 
            r12).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating volatile registers\x7fComment: If the method is a leaf, we allow a certain number of
locals and stack locations to be allocated to volatile
registers.  If it turns out that we don\'t have enough
registers, then we back out of the leaf method
optimization and try again without it.  -- jb 8/03\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lastVolatileRegisterThatWasActuallyAssigned: r = ( |
             n.
            | 
            [isLeafMethod] assert.
            n: (r number - outgoingReceiverRegister number) succ.
            n > maxArgumentRegisters succ ifTrue: [
              backoutBlock value: 'used too many volatile registers'
            ].
            volatileRegisterCount: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fComment: Used by compiler to copy incoming arguments to the stack.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingMemRcvrOrArgAt: index = ( |
            | 
            [isLeafMethod not] assert.
            locationForIncomingMemRcvrOrArgAt: index LexicalLevel: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fComment: Return location for accessing index\'th rcvr or arg
in me from a frame that is ll levels down.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForIncomingMemRcvrOrArgAt: index LexicalLevel: ll = ( |
            | 
            verifyIndex: index IsWithinAllocatedLimit: incomingRcvrAndArgCount.

            locations incomingMemoryArgument copyRcvrAndArgNo: index LexicalLevel: ll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: non-local return home scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingNLRHomeScope = ( |
            | 
            locationForOutgoingNLRHomeScope).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fComment: Used by compiler to copy incoming arguments to non-volatile registers.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingRegRcvrOrArgAt: index = ( |
            | 
            [isLeafMethod not] assert.
            verifyIndex: index IsWithinAllocatedLimit: incomingRcvrAndArgNonVolRegisterCount.

            locationForNonVolLocalRegAt: index).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: message send result\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingResult = ( |
            | locationForOutgoingResult).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fComment: Used by compiler to copy incoming arguments to non-volatile registers.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForIncomingVolatileRegRcvrOrArgAt: index = ( |
            | 
            [isLeafMethod not] assert.
            verifyIndex: index IsWithinAllocatedLimit: incomingRcvrAndArgVolatileRegisterCount.

            locationForVolatileRegRcvrOrArgAt: index IfNoSuchIndex: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForNonVolLocalMemAt: index = ( |
            | 
            [isLeafMethod not] assert: 'leaf methods cannot allocate non-volatile storage (no stack frame)'.
            reserveSpaceForNonVolLocalMemWords: index succ.

            locations nonVolMemoryLocal copyIndex: index).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForNonVolLocalRegAt: index = ( |
             regNumber.
            | 
            [isLeafMethod not] assert: 'leaf methods cannot allocate non-volatile storage (no stack frame)'.
            regNumber: highestNonVolRegister number - index.

            [regNumber >= lowestNonVolRegister number] assert.
            verifyIndex: index IsWithinAllocatedLimit: nonVolLocalRegCount.

            locationForRegister: gprFor: regNumber).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: non-local return home scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingNLRHomeScope = ( |
            | 
            locationForVolatileRcvrOrArgAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingRcvrOrArgAt: index = ( |
            | 
            [aaa verifyIndex: index IsWithinAllocatedLimit: outgoingRcvrAndArgCount.].

            locationForVolatileRcvrOrArgAt: index).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: message send result\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingResult = ( |
            | 
            locationForRegister: outgoingResultRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForRegister: r = ( |
            | 
            r isGPR ifTrue: [
              "Might as well use the cached one."
              compiler codeGenerator myAssemblerSystem allRegisterLocations at: r number
            ] False: [
              "We should probably cache these too."
              locations register copyForRegister: r
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
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
            verifyIndex: index IsWithinAllocatedLimit: incomingRcvrAndArgSavedRegisterCount.

            locationForIncomingRcvrOrArgAt: index LexicalLevel: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: volatile incoming, leaf method incoming, and outgoing receiver and arguments (use when parts of stack frame look like caller\'s)\x7fComment: Makes a location for a memory argument used to
pass arguments out of caller frame or into leaf
method frame. \x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForVolatileRcvrOrArgAt: index = ( |
            | 
            locationForVolatileRegRcvrOrArgAt: index IfNoSuchIndex: [
              locations outgoingMemoryArgument copyRcvrAndArgNo: index
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: volatile incoming, leaf method incoming, and outgoing receiver and arguments (use when parts of stack frame look like caller\'s)\x7fComment: Makes a location for a volatile register used to
pass arguments out of caller frame or into callee frame.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForVolatileRegRcvrOrArgAt: index IfNoSuchIndex: fb = ( |
            | 
            index <= maxArgumentRegisters  ifTrue: [
              locationForRegister: outgoingRcvrOrArgRegisterAt: index
            ]
            False: [fb value: 'no such index']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationsForNonVolLocalRegs = ( |
            | 
            (vector copySize: nonVolLocalRegCount) copyMappedBy: [|:x. :i|
              locationForRegister: gprFor: highestNonVolRegister number - i
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lowestGlobalRegister = ( |
            | 
            lowestNonVolRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lowestLocalNonVolRegister = ( |
            | 
            gprFor: highestGlobalRegister number succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lowestNonVolRegister = ( |
            | 
            gprFor: lastTempRegister number succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAnotherNonVolLocalMemLocation = ( |
            | 
            locationForNonVolLocalMemAt: nonVolLocalMemWordCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fComment: w/o receiver
both in and out\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         maxArgumentRegisters = 7.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         nonVolLocalMemWordCount = ( |
            | frame nonVolLocalWordCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         nonVolRegisterCount = ( |
            | 
            (highestNonVolRegister number - lowestNonVolRegister number) succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         objectAddressesBaseRegister = ( |
            | 
            gprFor: spLimitRegister number succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         outgoingRcvrAndArgCount = ( |
            | frame outgoingRcvrAndArgWordCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         outgoingRcvrOrArgRegisterAt: i = ( |
            | 
            gprFor: outgoingReceiverRegister number + i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         outgoingReceiverRegister = ( |
            | 
            r3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         outgoingResultRegister = ( |
            | 
            outgoingReceiverRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         registers* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         requiredTempCount = ( |
            | 
            lastTempRegister number succ - firstTempRegister number).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         reserveSpaceForNonVolLocalMemWords: howMany = ( |
            | 
            frame reserveSpaceForNonVolLocals: howMany).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         reserveSpaceForNonVolLocalRegs: howMany = ( |
            | 
            frame reserveSpaceToSaveNonVolRegs: howMany.
            nonVolLocalRegCount: nonVolLocalRegCount max: howMany.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         reserveSpaceForOutgoingRcvrAndArgs: howMany = ( |
            | 
            outgoingRcvrAndArgValues size < howMany ifTrue: [
              outgoingRcvrAndArgValues: outgoingRcvrAndArgValues copySize: howMany.
            ].
            frame reserveSpaceForOutgoingRcvrAndArgs: howMany).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         spLimitRegister = ( |
            | 
            gprFor: byteMapBaseRegister number succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fCategory: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         tempRegisterAt: i = ( |
            | 
            gprFor: firstTempRegister number + i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating volatile registers\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         volatileRegisterLocationsUsableForAssignableLocalsInLeafMethod = ( |
            | 
            [isLeafMethod] assert.
            (vector copySize: maxArgumentRegisters succ - incomingRcvrAndArgVolatileRegisterCount) mapBy: [|:x. :i|
              locationForVolatileRegRcvrOrArgAt: incomingRcvrAndArgVolatileRegisterCount + i IfNoSuchIndex: raiseError.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> () From: ( | {
         'Category: _OnNonLocalReturn\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)'
        
         valueFor_OnNLR_homeScope.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'allocators' -> 'ppc' -> () From: ( | {
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
