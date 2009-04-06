 '$Revision: 30.13 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcess = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda foreignProcess copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignProcess.

CopyDowns:
globals kleinAndYoda foreignProcess. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> () From: ( | {
         'ModuleInfo: Module: kleinProcess InitialContents: InitializeToExpression: (true)\x7fVisibility: public'
        
         isSourceLevel <- bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> () From: ( | {
         'ModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignProcess parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: setting up global registers\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         byteMapBaseRegister = ( |
            | 
            protoAllocatorForMyPlatform byteMapBaseRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         contentsOfRegister: regName = ( |
            | 
            contentsOfRegister: regName IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         contentsOfRegister: regName IfFail: fb = ( |
            | 
            safelyDo: [
              safeProxy contentsOfRegister: regName IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         currentActivation = ( |
            | 
            [todo kleinDebugger cleanup].
            "Putting isSourceLevel in the foreignProcess is a HACK.
             We reused the Self debugger architecture which lets the process
             create the stack by creating the activations and so there was no place
             to put this. It should be the proces model creating the stack model creating
             the activation models creating the activations. This flag should then be in each
             activation model. That way, a debugger could mix machine and source level activations.
             -- dmu 7/05"
            isSourceLevel ifTrue: [currentSourceLevelActivation]
                           False: [currentMachineLevelActivation]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         currentMachineLevelActivation = ( |
            | 
            currentSourceLevelActivation asMachineLevelActivation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         currentSourceLevelActivation = ( |
            | 
            myVMKit mirrors methodActivation topmostActivationForProcess: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: scheduling\x7fCategory: retrying\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         invalidateInlineCache: pcAtSendDesc IfFail: fb = ( |
            | 
             writeWord: (myVMKit layouts smi encode: 0)
            ToMemoryAt: pcAtSendDesc + (oopSize * sendDescForMyPlatform previousMapIndex)
                IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: scheduling\x7fCategory: retrying\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         killActivationsUpTo: actNum IfFail: fb = ( |
             activationThatWasEdited.
             entryMethodWasEdited.
             regContentsByRegName.
             stack.
            | 
            stack: activationStackLimit: actNum succ IfFail: [|:e| ^ fb value: e].
            activationThatWasEdited: stack at: actNum pred.

            regContentsByRegName: registerContentsToRestoreForActivation: activationThatWasEdited
                                                                  IfFail: [|:e| ^ fb value: e].

            entryMethodWasEdited: stack size <= actNum.
            entryMethodWasEdited ifTrue: [| entrySP |
              entrySP: activationThatWasEdited myRegisterLocator senderSPIfAbsent: [|:e| ^ fb value: e].
              restoreRegisterContents: regContentsByRegName AndTrimStackBackToEntrySP: entrySP IfFail: fb
            ] False: [
              restoreRegisterContents: regContentsByRegName AndTrimStackBackToActivation: (stack at: actNum) IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fCategory: double dispatch\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         localOopInRegister: regName IfFail: fb = ( |
            | 
            _Breakpoint: 'What the heck do we do now?').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: multistepping\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         multiStep: n TimesOrToPC: limitPC = ( |
            | multiStep: n TimesOrToPC: limitPC  IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: multistepping\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         multiStep: n TimesOrToPC: limitPC IfFail: fb = ( |
            | 
            safelyDo: [safeProxy multiStep: n TimesOrToPC: limitPC IfFail: fb].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: accessing platform information\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | 
            myVM ifNotNil: [myVM    myAssemblerSystem]
                    IfNil: [myProxy myAssemblerSystem]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinProcess InitialContents: FollowSlot'
        
         myVMKit = ( |
            | klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: setting up global registers\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         objectAddressesBaseRegister = ( |
            | 
            protoAllocatorForMyPlatform objectAddressesBaseRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         objectLocatorTimestampIfFail: fb = ( |
             indexablesAddr.
             map.
             mapOop.
             olAddr.
             timestamp.
             timestampOffset.
             timestampOop.
            | 
            indexablesAddr: contentsOfRegister: objectAddressesBaseRegister name IfFail: [|:e| ^ fb value: e].
            indexablesAddr = 0 ifTrue: [^ fb value: 'objectAddressesBaseRegister has not been set yet'].
            olAddr: indexablesAddr - (myVM image objectsOracle objectLocatorIndexableOrigin * oopSize).
            mapOop:  myVMKit layouts memoryObject mapOfObjectWithAddress: olAddr.
            map: myVM image objectsOracle originalObjectForOop: mapOop IfAbsent: [|:e| ^ fb value: e].
            [myVM objectLocator timestamp]. "browsing"
            timestampOffset: map offsetOfObjectSlotNamed: 'timestamp' IfAbsent: [|:e| ^ fb value: e].
            timestampOop: map myLayout forObjectWithAddress: olAddr At: timestampOffset.
            timestamp: myVMKit layouts smi decode: timestampOop.
            timestamp).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         oopInRegisterNamed: regName IfFail: fb = ( |
            | 
            lens oopInRegister: regName InProcess: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: accessing platform information\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         protoAllocatorForMyPlatform = ( |
            | 
            [ppc. sparc]. "browsing"
            architecture sendTo: myVMKit compiler1s abstract prototypes allocators).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: scheduling\x7fCategory: retrying\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         registerContentsToRestoreForActivation: activationThatWasEdited IfFail: fb = ( |
             protoAllocator.
             regContentsByRegName.
            | 
            regContentsByRegName: dictionary copyRemoveAll.

            activationThatWasEdited nonVolatileRegisterContentsDo: [|:regContents. :regName|
              regContentsByRegName at: regName Put: regContents.
            ] IfFail: [|:e| ^ fb value: e].

            protoAllocator: protoAllocatorForMyPlatform.
            (activationThatWasEdited receiverAndArgumentOopsIfFail: [|:e| ^ fb value: e]) do: [|:rcvrOrArgOop. :i|
              regContentsByRegName at: (protoAllocator outgoingRcvrOrArgRegisterAt: i) name Put: rcvrOrArgOop.
            ].

            regContentsByRegName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fCategory: double dispatch\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteOopInRegister: regName IfFail: fb = ( |
            | 
            safelyDo: [
              safeProxy contentsOfRegister: regName IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fCategory: preemption\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot'
        
         resetPreemption = ( |
             x.
            | 
            savedSPLimits isEmpty ifTrue: [^ self].
            [x: savedSPLimits removeLast] untilFalse: [x = -1].
            setContentsOfRegister: spLimitRegister name To: x).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: scheduling\x7fCategory: retrying\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         restoreRegisterContents: regContentsByRegName = ( |
            | 
            regContentsByRegName do: [|:regContents. :regName| setContentsOfRegister: regName To: regContents].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: scheduling\x7fCategory: retrying\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         restoreRegisterContents: regContentsByRegName AndTrimStackBackToActivation: activationBelowTheOneThatWasEdited IfFail: fb = ( |
             pcAtSendDesc.
            | 
            pcAtSendDesc: activationBelowTheOneThatWasEdited pcAfterBranchIfFail: [|:e| ^ fb value: e].
            invalidateInlineCache: pcAtSendDesc                           IfFail: [|:e| ^ fb value: e].
            restoreRegisterContents: regContentsByRegName. 
            retrySend: pcAtSendDesc                                       IfFail: [|:e| ^ fb value: e].
            setSP: activationBelowTheOneThatWasEdited sp                  IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: scheduling\x7fCategory: retrying\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         restoreRegisterContents: regContentsByRegName AndTrimStackBackToEntrySP: sp IfFail: fb = ( |
            | 
            restoreRegisterContents: regContentsByRegName.

            setPC: entryAddress          IfFail: [|:e| ^ fb value: e].
            setReturnAddress: myProxy returnHandler IfFail: [|:e| ^ fb value: e].
            setSP: sp                    IfFail: [|:e| ^ fb value: e].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: scheduling\x7fCategory: retrying\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         retrySend: pcAtSendDesc IfFail: fb = ( |
            | 
             setPC: pcAtSendDesc + (oopSize * sendDescForMyPlatform retryIndex)
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: accessing platform information\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         sendDescForMyPlatform = ( |
            | 
            [ppc. sparc]. "browsing"
            architecture sendTo: myVMKit sendDescs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: setting up global registers\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         setByteMapBaseRegister = ( |
             addr.
             ctMir.
             ctOop.
            | 
            myVM setTheVMAndDo: [
              ctMir: myVM image mirrorOnTheCardTableIfFail: raiseError.
              ctOop: ctMir reflectionPrimitives reflecteeOop.
              addr:  myVM byteVectorLayout for: ctOop AddressOfIndexableAt: 0.
            ].
            setContentsOfRegister: byteMapBaseRegister name
                               To: addr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         setContentsOfRegister: regName To: aNumber = ( |
            | 
            setContentsOfRegister: regName To: aNumber IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         setContentsOfRegister: regName To: aNumber IfFail: fb = ( |
            | 
            safelyDo: [
              safeProxy
                setContentsOfRegister: regName
                To: aNumber
                IfFail: [|:e| ^ fb value: e].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: setting up global registers\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         setGlobalRegisters = ( |
            | 
            setByteMapBaseRegister.
            setSPLimitRegister.
            setObjectAddressesBaseRegister.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: setting up global registers\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         setObjectAddressesBaseRegister = ( |
             addr.
             olMir.
             olOop.
            | 
            myVM setTheVMAndDo: [
              olMir: myVM image mirrorOnTheObjectLocatorIfFail: raiseError.
              olOop: olMir reflectionPrimitives reflecteeOop.
              addr:  myVMKit layouts objVector for: olOop AddressOfIndexableAt: 0.
            ].
            setContentsOfRegister: objectAddressesBaseRegister name
                               To: addr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         setPC: x = ( |
            | setPC: x IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         setPC: pc IfFail: fb = ( |
            | 
            safelyDo: [
              safeProxy setPC: pc IfFail: fb
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fCategory: preemption\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot'
        
         setPreemption = ( |
            | 
            savedSPLimits addLast: contentsOfRegister: spLimitRegister name.
            setContentsOfRegister: spLimitRegister name To: -1.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: setting up arguments\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         setRcvrOrArg: i To: oop = ( |
            | 
            setContentsOfRegister: (protoAllocatorForMyPlatform outgoingRcvrOrArgRegisterAt: i) name
                               To: oop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: setting up arguments\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         setReceiverOop: receiverOop AndParameterOops: paramOopVect = ( |
            | 
            setRcvrOrArg: 0 To: receiverOop.
            paramOopVect do: [|:paramOop. :i|
              setRcvrOrArg: i succ To: paramOop
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         setReturnAddress: lr IfFail: fb = ( |
            | 
            myProxy setReturnAddress: lr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         setSP: sp IfFail: fb = ( |
            | 
            setContentsOfRegister: myAssemblerSystem operands sp name To: sp IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: setting up global registers\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         setSPLimitRegister = ( |
            | 
            [todo stackLimit]. "What should this be? -- dmu 7/05"
            "Should it be a primitive in the start code?"
            setContentsOfRegister: spLimitRegister name To: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot'
        
         spIfFail: fb = ( |
            | 
            contentsOfRegister: myAssemblerSystem operands sp name
                IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fCategory: preemption\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot'
        
         spLimitRegister = ( |
            | 
            protoAllocatorForMyPlatform spLimitRegister).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fComment: Returns the address of the first indexable in the
object table. -- Adam, 7/06\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         startOfObjectAddressesIfFail: fb = ( |
             mv.
             oid.
             olAddr.
             r.
            | 
            r: contentsOfRegister: objectAddressesBaseRegister name IfFail: [|:e| ^ fb value: e].

            "I'm uneasy about relying on the objectAddressesBaseRegister to be
             set correctly; what if the object table gets moved but there's a
             bug that causes the register not to be set correctly? The whole
             remote debugging environment would break. But we can at least
             check to see that it really does look like the object table is at
             this address. -- Adam, 7/06"
            olAddr: r - (myVM image objectsOracle objectLocatorIndexableOrigin * oopSize).
            mv:  myVMKit layouts memoryObject markValueOfRemoteObjectWithAddress: olAddr IfFail: raiseError.
            oid: myVMKit layouts mark oidOfMarkValue: mv.
            [| objectLocatorOID |
             objectLocatorOID: myVM image oidForOriginalObject: myVM objectLocator.
             oid = objectLocatorOID] assert.

            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         stepOneMachineInstruction = ( |
            | 
            safelyDo: [safeProxy singleStep].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         oopInRegister: regName InProcess: aProcess IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         oopInRegister: regName InProcess: aProcess IfFail: fb = ( |
            | 
            aProcess localOopInRegister: regName IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         oopInRegister: regName InProcess: aProcess IfFail: fb = ( |
            | 
            aProcess remoteOopInRegister: regName IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinProcess InitialContents: FollowSlot'
        
         kleinProcess = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProcess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProcess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinProcess.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProcess' -> () From: ( | {
         'ModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProcess' -> () From: ( | {
         'ModuleInfo: Module: kleinProcess InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProcess' -> () From: ( | {
         'ModuleInfo: Module: kleinProcess InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProcess' -> () From: ( | {
         'ModuleInfo: Module: kleinProcess InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProcess' -> () From: ( | {
         'ModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.13 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProcess' -> () From: ( | {
         'ModuleInfo: Module: kleinProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinProcess postFileIn
