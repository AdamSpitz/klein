 '$Revision: 30.32 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForUplevel: n AccessIn: aSourceLevelAllocator = ( |
            | 
            aSourceLevelAllocator locationForUplevel: n AccessToRegister: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         machineLevelAllocators = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes machineLevelAllocators.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes machineLevelAllocators abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         allValues <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedValueForNLRHomeScope.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         compiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         frame.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (dictionary)\x7fVisibility: public'
        
         memoizedBlockValues <- bootstrap stub -> 'globals' -> 'dictionary' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes machineLevelAllocators abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: pre-allocating\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateIncomingAndPreallocatedLocations = ( |
            | 
            compiler nodesInControlFlowOrder do: [|:n| allocateOutgoingRcvrAndArgLocations: n requiredNumberOfOutgoingRcvrAndArgLocations].
            setNonVolatileRegSaveArea.
            topSourceLevelAllocator allocateIncomingAndPreallocatedLocations.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         architecture = ( |
            | compiler architecture).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         backoutBlock = ( |
            | compiler backoutBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            ((resend.copy
                              allValues:                   allValues copyRemoveAll)
                    memoizedBlockValues:         memoizedBlockValues copyRemoveAll)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForCompiler: c ForceNonLeaf: fnl = ( |
            | 
            copy initializeForCompiler: c ForceNonLeaf: fnl).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForCompiler: c ForceNonLeaf: fnl = ( |
            | 
            compiler: c.
            frame: fnl ifTrue: [protoFrameForMyPlatform copy] False: nil.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         isLeafMethod = ( |
            | 
            frame isNil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: constants\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForConstant: o = ( |
            | 
            locations constant copyForOop: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locations = ( |
            | 
            compiler locations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: non-volatile local memory\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationsForNonVolLocalMems = ( |
            | 
            frame locationsForNonVolLocals).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: liveness\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationsThatCouldBeDead = ( |
            | 
            locationsForNonVolLocalRegs,
            locationsForNonVolLocalMems).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         newValue = ( |
             v.
            | 
            v: compiler prototypes dataValue copyCompiler: compiler.
            v uniqueID: allValues size.
            allValues add: v.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: message send result\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         newValueForIncomingResult = ( |
            | 
            (newValueWithLocation: locationForIncomingResult) addDescription: 'incoming result').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         newValueForOutgoingRcvrAndArgAt: index = ( |
             v.
            | 
            v: newValueWithLocation: locationForOutgoingRcvrOrArgAt: index.
            v addDescription: 'outgoing_', index printString.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         newValueWithLocation: loc = ( |
            | 
            newValue location: loc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         protoFrameForMyPlatform = ( |
            | 
            klein stackFrames protoForArchitecture: architecture).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: pre-allocating\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setNonVolatileRegSaveArea = ( |
            | 
            compiler shouldSaveAllNonVolatileRegisters ifTrue: [
              frame reserveSpaceToSaveNonVolRegs: registerUsage nonVolRegisterCount.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForMemoizedBlockMirror: blockProtoMir = ( |
            | 
            memoizedBlockValues at: blockProtoMir IfAbsentPut: [
              newValue addDescription: 'memoized block'
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: nlr home scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForNLRHomeScope = ( |
            | 
            cachedValueForNLRHomeScope ifNil: [
              cachedValueForNLRHomeScope: (newValueWithLocation: locationForOutgoingNLRHomeScope) addDescription: 'NLR home scope'.
              cachedValueForNLRHomeScope
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: assertions\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyIndex: index IsWithinAllocatedLimit: limit = ( |
            | 
            [(0 <= index) && [index < limit]]  
              assert: 'this location has not been allocated'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> () From: ( | {
         'Comment: allocator for to be compiled lexical level (contains links to parent lexical allocators)\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         topSourceLevelAllocator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         ppc = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes machineLevelAllocators abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes machineLevelAllocators ppc.

CopyDowns:
globals klein compiler1 parent prototypes machineLevelAllocators abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( |
             {} = 'Comment: Note about nomenclature
- makeAnotherXXX: allocate something new
                  (always returns a new location)
- locationForXXX: return the uniquely determined location for something
                  (always returns an equivalent location)\x7fModuleInfo: Creator: globals klein compiler1 parent prototypes machineLevelAllocators ppc parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateOutgoingRcvrAndArgLocations: howMany = ( |
            | 
            howMany = 0 ifTrue: [^ self].

            isLeafMethod ifTrue: [
              compiler backoutBlock value: 'leaf methods cannot allocate outgoing arguments (no stack frame)'.
            ].

            reserveSpaceForOutgoingRcvrAndArgs: howMany).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         argumentCount = ( |
            | 
            topSourceLevelAllocator argumentSlots size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: new register allocation algorithm\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         availableNonVolatileRegisterLocations = ( |
             highestAvailableRegNumber.
            | 
            highestAvailableRegNumber: registerUsage highestNonVolRegister number - frame nonVolLocalRegCount.
            (vector copySize: highestAvailableRegNumber succ - registerUsage lowestLocalNonVolRegister number) mapBy: [|:x. :i|
              "Start from the highest rather than the lowest, so that the used registers will be contiguous."
              gprFor: highestAvailableRegNumber - i
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: new register allocation algorithm\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         availableRegisterLocations = ( |
            | 
            isLeafMethod ifTrue: [
              volatileRegisterLocationsUsableForAssignableLocalsInLeafMethod
            ] False: [
              availableNonVolatileRegisterLocations
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: incoming receiver and arguments\x7fComment: The number of incoming arguments that have been passed
  in volatile registers.
For _VariableArguments, we return maxArgumentRegisters
  because there there is no way to determine how many
  arguments were actually passed by the caller. -- jb 8/03\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         incomingArgVolatileRegisterCount = ( |
            | 
            compiler variableArguments
              ifTrue: [ registerUsage maxArgumentRegisters                    ]
               False: [ registerUsage maxArgumentRegisters min: argumentCount ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
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
  have any blocks.  -- jb 8/03\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         incomingRcvrAndArgSavedRegisterCount = ( |
            | 
            isLeafMethod                ifTrue: [^ 0].
            memoizedBlockValues isEmpty ifTrue: [^ 0].
            argumentCount + 1 "for receiver").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         incomingRcvrAndArgVolatileRegisterCount = ( |
            | 
            incomingArgVolatileRegisterCount + 1 "for receiver").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: new register allocation algorithm\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lastNonVolatileRegisterThatWasActuallyAssigned: r = ( |
            | 
            frame reserveSpaceForNonVolLocalRegs: (registerUsage indexOfNonVolLocalRegister: r) succ.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: new register allocation algorithm\x7fComment: If the method is a leaf, we allow a certain number of
locals and stack locations to be allocated to volatile
registers.  If it turns out that we don\'t have enough
registers, then we back out of the leaf method
optimization and try again without it.  -- jb 8/03\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lastVolatileRegisterThatWasActuallyAssigned: r = ( |
             index.
            | 
            [isLeafMethod] assert.
            index: registerUsage indexOfOutgoingArgumentRegister: r IfNoSuchIndex: [
              backoutBlock value: 'used too many volatile registers'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingMemRcvrOrArgAt: index = ( |
            | 
            verifyIndex: index IsWithinAllocatedLimit: incomingRcvrAndArgCount.

            locations incomingMemoryArgument copyRcvrAndArgNo: index LexicalLevel: 0 AccessedFrom: topSourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: general stuff\x7fCategory: locations\x7fCategory: non-local return home scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingNLRHomeScope = ( |
            | 
            locationForOutgoingNLRHomeScope).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: general stuff\x7fCategory: locations\x7fCategory: message send result\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingResult = ( |
            | locationForOutgoingResult).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: general stuff\x7fCategory: locations\x7fCategory: non-local return home scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingNLRHomeScope = ( |
            | 
            locationForVolatileRcvrOrArgAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingRcvrOrArgAt: index = ( |
            | 
            [aaaaa. "Gotta take this out because we don't know how many actual sends happen
                     until after we're done inlining. So move this check there, or something."
            verifyIndex: index IsWithinAllocatedLimit:  frame outgoingRcvrAndArgWordCount.
            ].

            locationForVolatileRcvrOrArgAt: index).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: general stuff\x7fCategory: locations\x7fCategory: message send result\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForOutgoingResult = ( |
            | 
            registerUsage outgoingResultRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: general stuff\x7fCategory: locations\x7fCategory: volatile incoming, leaf method incoming, and outgoing receiver and arguments (use when parts of stack frame look like caller\'s)\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForVolatileMemRcvrOrArgAt: index = ( |
            | 
            locations outgoingMemoryArgument copyRcvrAndArgNo: index LexicalLevel: 0 AccessedFrom: topSourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: general stuff\x7fCategory: locations\x7fCategory: volatile incoming, leaf method incoming, and outgoing receiver and arguments (use when parts of stack frame look like caller\'s)\x7fComment: Makes a location for a memory argument used to
pass arguments out of caller frame or into leaf
method frame. \x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForVolatileRcvrOrArgAt: index = ( |
            | 
            locationForVolatileRegRcvrOrArgAt: index IfNoSuchIndex: [
              locationForVolatileMemRcvrOrArgAt: index
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: general stuff\x7fCategory: locations\x7fCategory: volatile incoming, leaf method incoming, and outgoing receiver and arguments (use when parts of stack frame look like caller\'s)\x7fComment: Used by compiler to copy incoming arguments to non-volatile registers.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForVolatileRegRcvrOrArgAt: index = ( |
            | 
            locationForVolatileRegRcvrOrArgAt: index IfNoSuchIndex: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: general stuff\x7fCategory: locations\x7fCategory: volatile incoming, leaf method incoming, and outgoing receiver and arguments (use when parts of stack frame look like caller\'s)\x7fComment: Makes a location for a volatile register used to
pass arguments out of caller frame or into callee frame.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForVolatileRegRcvrOrArgAt: index IfNoSuchIndex: fb = ( |
            | 
            registerUsage outgoingRcvrOrArgRegisterAt: index
                                        IfNoSuchIndex: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locationsForNonVolLocalRegs = ( |
            | 
            (vector copySize: frame nonVolLocalRegCount) mapBy: [|:x. :i|
              registerUsage nonVolLocalRegisterAt: i
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         makeAnotherNonVolLocalMemLocation = ( |
            | 
            isLeafMethod ifTrue: [compiler backoutBlock value: 'no stack frame in leaf methods'].

            locations nonVolMemoryLocal
                        copyIndex: frame reserveSpaceForAnotherNonVolMemLocal
                     AccessedFrom: topSourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: allocating non-volatile locals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         makeAnotherNonVolLocalRegLocation = ( |
            | 
            registerUsage nonVolLocalRegisterAt: frame reserveSpaceForAnotherNonVolLocalReg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         memLocationToSaveNonVolRegister: r = ( |
            | 
            locationForIncomingMemRcvrOrArgAt: registerUsage indexOfNonVolLocalRegister: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         method = ( |
            | 
            topSourceLevelAllocator method).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: register usage\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         registerUsage = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes machineLevelAllocators ppc parent registerUsage.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         byteMapBaseRegister = ( |
            | 
            lowestGlobalRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         firstTempRegister = ( |
            | 
            r11).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         highestGlobalRegister = ( |
            | 
            objectAddressesBaseRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         highestNonVolRegister = ( |
            | 
            r31).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: incoming\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         incomingRcvrOrArgRegisterAt: i = ( |
            | 
            gprFor: incomingReceiverRegister number - i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: incoming\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         incomingReceiverRegister = ( |
            | 
            r31).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         indexOfNonVolLocalRegister: r = ( |
            | 
            indexOfNonVolLocalRegister: r IfNoSuchIndex: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         indexOfNonVolLocalRegister: r IfNoSuchIndex: fb = ( |
            | 
            r number < lowestLocalNonVolRegister number ifTrue: [^ fb value].
            highestNonVolRegister number - r number).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         indexOfOutgoingArgumentRegister: r IfNoSuchIndex: fb = ( |
             i.
            | 
            i: r number - outgoingReceiverRegister number.
            i > maxArgumentRegisters ifFalse: i True: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         isLocalNonVolRegister: r = ( |
            | 
            r number >= lowestLocalNonVolRegister number).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         lastOutgoingArgumentRegister = ( |
            | 
            outgoingRcvrOrArgRegisterAt: maxArgumentRegisters).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         lastTempRegister = ( |
            | 
            r12).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         lowestGlobalRegister = ( |
            | 
            lowestNonVolRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         lowestLocalNonVolRegister = ( |
            | 
            gprFor: highestGlobalRegister number succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         lowestNonVolRegister = ( |
            | 
            gprFor: lastTempRegister number succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: outgoing\x7fComment: w/o receiver
both in and out\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         maxArgumentRegisters = 7.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         nonVolLocalRegisterAt: index = ( |
            | 
            nonVolLocalRegisterAt: index IfNoSuchIndex: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         nonVolLocalRegisterAt: index IfNoSuchIndex: fb = ( |
             regNumber.
            | 
            regNumber: highestNonVolRegister number - index.
            regNumber >= lowestLocalNonVolRegister number
               ifTrue: [gprFor: regNumber]
                False: [fb value: 'no such index']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         nonVolLocalRegisterCount = ( |
            | 
            (highestNonVolRegister number - lowestLocalNonVolRegister number) succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: nonvolatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         nonVolRegisterCount = ( |
            | 
            (highestNonVolRegister number - lowestNonVolRegister number) succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         objectAddressesBaseRegister = ( |
            | 
            gprFor: spLimitRegister number succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         outgoingRcvrOrArgRegisterAt: i = ( |
            | 
            outgoingRcvrOrArgRegisterAt: i IfNoSuchIndex: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         outgoingRcvrOrArgRegisterAt: index IfNoSuchIndex: fb = ( |
            | 
            index <= maxArgumentRegisters
               ifTrue: [gprFor: outgoingReceiverRegister number + index]
                False: [fb value: 'no such index']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         outgoingReceiverRegister = ( |
            | 
            r3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: outgoing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         outgoingResultRegister = ( |
            | 
            outgoingReceiverRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         registers* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: globals\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         spLimitRegister = ( |
            | 
            gprFor: byteMapBaseRegister number succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> 'registerUsage' -> () From: ( | {
         'Category: temporaries\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         tempRegistersDo: blk = ( |
            | 
            firstTempRegister number to: lastTempRegister number Do: [|:i|
              blk value: gprFor: i.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         registers* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: new register allocation algorithm\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         registersThatWereActuallyAssigned: rs = ( |
            | 
            isLeafMethod ifTrue: [
              lastVolatileRegisterThatWasActuallyAssigned: rs max.
            ] False: [
              lastNonVolatileRegisterThatWasActuallyAssigned: rs min.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: outgoing receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         reserveSpaceForOutgoingRcvrAndArgs: howMany = ( |
            | 
            howMany = 0 ifTrue: [^ self].
            frame reserveSpaceForOutgoingRcvrAndArgs: howMany).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> 'ppc' -> 'parent' -> () From: ( | {
         'Category: new register allocation algorithm\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         volatileRegisterLocationsUsableForAssignableLocalsInLeafMethod = ( |
            | 
            [isLeafMethod] assert.
            (vector copySize: registerUsage maxArgumentRegisters succ - incomingRcvrAndArgVolatileRegisterCount) mapBy: [|:x. :i|
              locationForVolatileRegRcvrOrArgAt: incomingRcvrAndArgVolatileRegisterCount + i IfNoSuchIndex: raiseError.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'machineLevelAllocators' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         prototypeForArchitecture: arch = ( |
            | 
            [ppc. sparc]. "browsing"
            arch sendTo: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         sourceLevelAllocator = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes sourceLevelAllocator.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         context.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         incomingRcvrAndArgValues.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         machineLevelAllocator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: public'
        
         memoizedBlockValues <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (dictionary)\x7fVisibility: public'
        
         namedValues <- bootstrap stub -> 'globals' -> 'dictionary' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes sourceLevelAllocator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: pre-allocating\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateIncomingAndPreallocatedLocations = ( |
            | 
            extendIncomingRcvrAndArgValuesIfVariableArguments.
            setIncomingRcvrAndArgLocations.
            setLocalSlotLocations.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentSlots = ( |
            | 
            compiler argumentSlotsForAllocator: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         argumentSlotsOfMethod = ( |
            | 
            method argumentSlots).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         assignableLocalSlots = ( |
            | 
            compiler assignableLocalSlotsForAllocator: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         assignableLocalSlotsOfMethod = ( |
            | 
            method assignableLocalSlots).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         compiler = ( |
            | machineLevelAllocator compiler).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            ((resend.copy
                memoizedBlockValues: memoizedBlockValues copyRemoveAll)
                        stackValues:         stackValues copyRemoveAll)
                        namedValues:         namedValues copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForMachineLevelAllocator: mla Context: c = ( |
            | 
            (copy machineLevelAllocator: mla) context: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: pre-allocating\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         extendIncomingRcvrAndArgValuesIfVariableArguments = ( |
            | 
            compiler variableArguments ifTrue: [| oldSize |
              oldSize: incomingRcvrAndArgValues size.
              incomingRcvrAndArgValues: incomingRcvrAndArgValues copySize: oldSize max: machineLevelAllocator registerUsage maxArgumentRegisters succ.
              oldSize upTo: incomingRcvrAndArgValues size Do: [|:i. v|
                v: newValueWithLocation: preallocatedLocationForIncomingRcvrOrArgAt: i.
                v addDescription: 'variableIncoming_', i succ printString.
                incomingRcvrAndArgValues at: i Put: v.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: saving the incoming volatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         incomingVolatileRegRcvrAndArgLocationsDo: blk = ( |
            | 
            [machineLevelAllocator isLeafMethod not] assert.
            machineLevelAllocator incomingRcvrAndArgVolatileRegisterCount do: [|:i. vol. nonVol|
              vol: machineLevelAllocator locationForVolatileRegRcvrOrArgAt: i.
              nonVol: (valueForIncomingRcvrOrArgAt: i) location.
              blk value: vol With: nonVol.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: saving the incoming volatiles\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         incomingVolatileRegRcvrAndArgLocationsToSaveDo: blk = ( |
             i <- 0.
            | 
            incomingVolatileRegRcvrAndArgLocationsDo: [|:vol. :nonVol. nonVolReg. nonVolMem|
              [nonVol isRegister] assert.
              nonVolReg: nonVol.
              [aaaaa]. "I'm really confused. Let's try this hack."
              [i = (machineLevelAllocator registerUsage indexOfNonVolLocalRegister: nonVolReg)] assert. "Just a sanity check for now, doesn't need to be true in the long run."
              nonVolMem: i < machineLevelAllocator incomingRcvrAndArgSavedRegisterCount ifTrue: [
                machineLevelAllocator memLocationToSaveNonVolRegister: nonVolReg.
              ] False: nil.
              blk value: vol With: nonVolReg With: nonVolMem.
              i: i succ.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeIncomingRcvrAndArgValues = ( |
             argSlots.
            | 
            argSlots: argumentSlots asVector.
            incomingRcvrAndArgValues: (vector copySize: argSlots size succ) mapBy: [|:x. :i. v|
              v: (preallocatedLocationForIncomingRcvrOrArgAt: i) ifNil: [newValue] IfNotNil: [|:loc| newValueWithLocation: loc].
              i = 0 ifFalse: [| s |
                s: argSlots at: i pred.
                v addDescription: s key.
                namedValues if: s key IsPresentDo: [error: 'hmm'] IfAbsentPut: [v] AndDo: [].
              ].
              v addDescription: 'incoming_', i printString.
            ].  
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeLocalSlotValues = ( |
            | 
            localSlots do: [|:s. v|
              v: (preallocatedLocationForLocalSlot: s) ifNil: [newValue] IfNotNil: [|:loc| newValueWithLocation: loc].
              v addDescription: s key.
              namedValues if: s key IsPresentDo: [error: 'hmm'] IfAbsentPut: [v] AndDo: [].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeUplevelValues = ( |
            | 
            "Might as well set the locations right here, since we know them."
            lexicalParentScopes reverseDo: [|:s. :i|
              s slotSPOffsets with: s method slotNamesWithSPOffsetRecorded Do: [|:offset. :name|
                namedValues at: name IfAbsentPut: [| loc |
                  loc: locationForUplevel: lexicalParentScopes size - i AccessTo: s locationForOffset: offset.
                  (newValueWithLocation: loc) addDescription: name
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeValueForSelf = ( |
            | 
            valueForSelf:
              lexicalParentScopes isEmpty
                ifTrue: [valueForIncomingReceiver]
                 False: [newValueWithLocation:
                                         locationForUplevel: lexicalParentCount
                                                   AccessTo: context outermostScope locationForIncomingReceiver].
            valueForSelf addDescription: 'self'.

            valueForSelf possibleValues: vector copyAddFirst: valueForSelf kindsOfPossibleValues selfValue copyForAllocator: self.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeValues = ( |
            | 
            initializeIncomingRcvrAndArgValues.
            initializeLocalSlotValues.
            initializeValueForSelf.
            initializeUplevelValues.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         isInlined = ( |
            | 
            != machineLevelAllocator topSourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         lexicalParentCount = ( |
            | 
            context lexicalParentCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         lexicalParentScopes = ( |
            | 
            context lexicalParentScopes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         localSlots = ( |
            | 
            compiler localSlotsForAllocator: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         localSlotsOfMethod = ( |
            | 
            method localSlots).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: constants\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForConstant: o = ( |
            | 
            machineLevelAllocator locationForConstant: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForIncomingReceiver = ( |
            | 
            valueForIncomingReceiver location).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: uplevel access\x7fComment: Sent to child allocator to create location to use to
access parent allocator\'s loc.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForUplevel: n AccessTo: loc = ( |
            | 
            loc locationForUplevel: n AccessIn: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: uplevel access\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForUplevel: n AccessToRegister: r = ( |
            | 
            "Since locals of methods with blocks are allocated into memory (I hope),
             it must be receiver or argument."

            locations incomingMemoryArgument copyRcvrAndArgNo: (machineLevelAllocator registerUsage indexOfNonVolLocalRegister: r) LexicalLevel: n AccessedFrom: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: uplevel access\x7fComment: Sent to child allocator to create location to use to
access parent allocator\'s loc.\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForUplevelAccessTo: loc = ( |
            | 
            locationForUplevel: 1 AccessTo: loc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         locations = ( |
            | 
            compiler locations).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: values for this scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         memoryStackValues = ( |
            | 
            stackValues copyFilteredBy: [|:v|
              v location isRegister not
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         method = ( |
            | 
            context method).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: values for this scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         newStackValue = ( |
             v.
            | 
            v: newValue.
            stackValues add: v.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         newValue = ( |
            | 
            machineLevelAllocator newValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: values for this scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         newValueForOutgoingResult = ( |
             v.
            | 
            v: isInlined ifTrue: [newValue]
                          False: [newValueWithLocation: machineLevelAllocator locationForOutgoingResult].
            v addDescription: 'outgoing result'.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         newValueWithLocation: loc = ( |
            | 
            machineLevelAllocator newValueWithLocation: loc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: pre-allocating\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         preallocatedLocationForIncomingRcvrOrArgAt: i = ( |
            | 
            machineLevelAllocator isLeafMethod ifTrue: [
              isInlined ifFalse: [
                machineLevelAllocator locationForVolatileRcvrOrArgAt: i
              ] True: [
                "Doesn't need to be preallocated - let the locationAssigner do it."
                nil
              ]
            ] False: [
              isInlined ifFalse: [
                i < machineLevelAllocator incomingRcvrAndArgVolatileRegisterCount ifTrue: [
                  [aaaaa]. "Wait, what???"
                  machineLevelAllocator makeAnotherNonVolLocalRegLocation
                ] False: [
                  machineLevelAllocator locationForIncomingMemRcvrOrArgAt: i
                ].
              ] True: [
                "Wait until we" [allocateIncomingAndPreallocatedLocations]. "Until then we
                 don't know whether the value will need to be in memory or not."
                nil
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: pre-allocating\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         preallocatedLocationForLocalSlot: s = ( |
            | 
            s isAssignable ifTrue: [
              "Wait until we" [allocateIncomingAndPreallocatedLocations]. "Until then we
               don't know whether the value will need to be in memory or not."
              nil
            ] False: [
              locationForConstant: s contents reflectee
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: pre-allocating\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         preallocatedLocationForNamedValue = ( |
            | 
            slot isMethod && [slot contents hasBlocks] ifTrue: [
              "If the method has blocks, some locals might be uplevel accessed
               and so must not be stored in registers.  At present we do not
               try to determine the precise set of such locals so we allocate
               all of them in non-volatile memory.  -- jb 8/03"

              machineLevelAllocator makeAnotherNonVolLocalMemLocation

            ] False: [
              "Otherwise, we are free to allocate the local slot into
               a register for best performance in the typical case. -- jb 8/03"

              nil
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         protoSlotFinder = ( |
            | 
            compiler protoSlotFinder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: pre-allocating\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setIncomingRcvrAndArgLocations = ( |
            | 
            incomingRcvrAndArgValues do: [|:v. :i|
              v hasLocation ifFalse: [
                preallocatedLocationForNamedValue ifNotNil: [|:loc| v location: loc].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: pre-allocating\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: private'
        
         setLocalSlotLocations = ( |
            | 
            localSlots do: [|:s. :i. v|
              v: namedValues at: s key.
              v hasLocation ifFalse: [
                preallocatedLocationForNamedValue ifNotNil: [|:loc| v location: loc].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         slot = ( |
            | 
            context slot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: constants\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForConstant: oop = ( |
            | newValueWithLocation: locationForConstant: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: values for this scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForIncomingRcvrOrArgAt: i = ( |
            | 
            incomingRcvrAndArgValues at: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: locations\x7fCategory: incoming receiver and arguments\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForIncomingReceiver = ( |
            | 
            valueForIncomingRcvrOrArgAt: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: values for this scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForMemoizedBlockMirror: blockProtoMir = ( |
            | 
            memoizedBlockValues at: blockProtoMir IfAbsentPut: [
              machineLevelAllocator valueForMemoizedBlockMirror: blockProtoMir
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> 'parent' -> () From: ( | {
         'Category: values for this scope\x7fModuleInfo: Module: kleinC1_Allocs InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForSlot: s = ( |
            | 
            namedValues at: s key IfAbsentPut: [newValue addDescription: s key]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: public'
        
         stackValues <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'sourceLevelAllocator' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Allocs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         valueForSelf.
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
