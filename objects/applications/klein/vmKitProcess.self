 '$Revision: 30.11 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         allocatedRegionFor: pc InProcess: aProcess = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         isProcessAlive: p = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         oopAt: addr InProcess: aProcess IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryWordAt: addr InProcess: aProcess IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcess = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignProcess.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: InitializeToExpression: (\'\')\x7fVisibility: public'
        
         causeOfBirth <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: InitializeToExpression: (processErrors ok)\x7fVisibility: public'
        
         causeOfError <- bootstrap stub -> 'globals' -> 'processErrors' -> 'ok' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         isDebuggingPerformance <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         myProxy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         myVM.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( |
             {} = 'Comment: I implement a process running on a remote
host for debugging machine-language programs.
-- dmu 1/02\x7fModuleInfo: Creator: globals kleinAndYoda foreignProcess parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         abort = ( |
            | safelyDo: [safeProxy killIfFail: []]. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         activationStackIfFail: fb = ( |
            | 
            activationStackLimit: maxSmallInt IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         activationStackLimit: maxActivations IfFail: failBlk = ( |
             st.
            | 
            st: list copyRemoveAll.
            st addFirst: currentActivation.
            st first isLive ifFalse: [^ vector].
            maxActivations pred do: [
              st addLast: st last senderIfFail: [^ st asVector]
            ].
            st asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         allAllocatedHeapRegions = ( |
            | 
            myVM universe allAllocatedRegions).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: allocating memory\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateMemorySize: n = ( |
            | 
            allocateMemorySize: n IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: allocating memory\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateMemorySize: n IfFail: fb = ( |
            | 
            safelyDo: [
              safeProxy allocateMemorySize: n IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         allocatedRegionFor: pc = ( |
            | 
            lens allocatedRegionFor: pc InProcess: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         allocatedRegions = ( |
            | myProxy allocatedRegions).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         allowInfiniteSlopDuring: blk = ( |
            | 
            myProxy allowInfiniteSlopDuring: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: accessing platform information\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         architecture = ( |
            | 
            myVM ifNotNil: [myVM    architecture]
                    IfNil: [myProxy architecture]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: querying\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         birthEvent = ( |
            | 
            [todo unimplemented kleinDebugger].
            "This should really be the event that created the foreign
             process when the user dropped the program on the foreignHostMorph."
            process this birthEvent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         continue = ( |
            | 
            resetPreemption.
            continueWithoutResettingPreemption).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         continueAt: newPC = ( |
            | 
            safelyDo: [safeProxy continueAt: newPC].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         continueWithoutResettingPreemption = ( |
            | 
            safelyDo: [safeProxy continue].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            (resend.copy
             sema: sema copyBinary)
             savedSPLimits: savedSPLimits copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         copyAndLaunch: aVMImage OnDebugServer: serverProxy DebuggingPerformance: aBool IfFail: fb = ( |
             r.
            | 
            r: copy.
            r isDebuggingPerformance: aBool.
            aVMImage copyAndLaunchFrom: serverProxy
                               Process: r 
                                IfFail: fb.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         copyLocal = ( |
            | 
            resend.copy myVM: _TheVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         currentActivation = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         detach = ( |
             r <- ''.
            | 
            safelyDo: [safeProxy detachIfFail: [|:e| r: e]].
            r isEmpty ifFalse: [error: r].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: downloading and starting programs\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         download: aVMImage To: serverProxy IfFail: fb = ( |
            | 
            safelyDo: [
              serverProxy download: aVMImage ForForeignProcess: self IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         endPoint = ( |
            | 
            myProxy endPoint).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: scheduling\x7fCategory: retrying\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         entryAddress = ( |
            | 
            myVM setTheVMAndDo: [myVM image entryNMethodMirror entryAddress]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         entryPoint = ( |
            | myProxy entryPoint).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         finish = ( |
            | 
            finish: currentActivation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         finish: thisActivation = ( |
            | 
            "Spit out a sensible error in the meantime -- jb 5/03"
            ^ error: 'Finish not yet implemented for foreignProcess').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: getting fixed-address-buffer base and length\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         getBaseAndLength = ( |
            | getBaseAndLengthIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: getting fixed-address-buffer base and length\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         getBaseAndLengthIfFail: fb = ( |
            | 
            safelyDo: [ |p. r|
              p: safeProxy copyAndInitiateIfFail: [|:e| ^ fb value: e].
              r: p getBaseAndLengthIfFail: [|:e| ^ fb value: e].
              p terminate.
              r
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot'
        
         getDebuggeePidString = ( |
            | 
            safelyDo: [
              safeProxy debuggeePID asString
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: querying\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         hasError = ( |
            | 
            safelyDo: [myProxy runStateIfFail: [^ true]].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: accessing platform information\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         hostName = ( |
            | 
            myProxy ifNil: '' IfNotNil: [myProxy hostName]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: accessing platform information\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         intNN = ( |
            | myVMKit layouts abstract intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateCaches = ( |
            | 
            myProxy invalidateCaches.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateObsoleteCachedItemsIn: cachingWobulator = ( |
            | 
            myProxy ifNil: [^ self].
            myProxy invalidateObsoleteCachedItemsIn: cachingWobulator.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: querying\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         isActive = ( |
             rs.
            | 
            rs: safelyDo: [safeProxy runStateStringIfFail: [^ false]].
            (rs = 'running') || [rs = 'uninterruptible']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: querying\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         isAlive = ( |
            | 
            lens isProcessAlive: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: testing\x7fComment: World reifies processes, too.
So need this slot.\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         isKleinOrYodaMirror = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: querying\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         isLocalProcessAlive = ( |
            | 
            [todo processes]. "Is this object supposed to represent a Unix process or a Self process?"
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: querying\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         isRemoteProcessAlive = ( |
            | 
            safelyDo: [
              myProxy ifNil: [^ false].
              0 != (myProxy runStateIfFail: [^ false])]
            IfBusy: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         isSuspended = ( |
            | 
            myProxy ifNil: [^ false].
            myProxy isDebuggeeSuspended).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: scheduling\x7fCategory: retrying\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         killActivationsUpTo: actNum = ( |
            | 
            [todo processHierarchy]. "Copied from traits process."
            killActivationsUpTo: actNum 
                         IfFail: [|:e|  e = 'noProcessError' ifFalse: [ error: e ]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: scheduling\x7fCategory: retrying\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         killActivationsUpTo: actNum IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: downloading and starting programs\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         launch: aVMImage From: serverProxy IfFail: fb = ( |
            | 
            myVM: aVMImage myVM.
            download: aVMImage To: serverProxy IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: Klein VM export and debugging\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         lens = ( |
            | (theVMIfAbsent: [^ myVMKit memoryLens]) lens).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         localAllocatedRegionFor: pc = ( |
            | 
            myVM universe allocatedRegionFor: pc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         localOopAt: addr IfFail: fb = ( |
            | 
            _UnsafeObjectForOopAtAddress: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Comment: Children should override me. -- dmu 2/28/06\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         myVMKit = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         objectLocatorTimestamp = ( |
            | objectLocatorTimestampIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         objectLocatorTimestampIfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         oopAt: addr IfFail: fb = ( |
            | 
            lens oopAt: addr InProcess: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: accessing platform information\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         oopSize = ( |
            | myVMKit layouts abstract oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         pcIfFail: fb = ( |
            | 
            safelyDo: [
             safeProxy pcIfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         readLocalMemoryWordAt: addr IfFail: fb = ( |
            | 
            intNN copy _UnsafeWordAtAddress: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryAt: addr Size: n IfFail: fb = ( |
            | 
            safelyDo: [
             safeProxy readMemoryAt: addr Size: n IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryBypassingCacheAt: addr Size: n IfFail: fb = ( |
            | 
            safelyDo: [
             safeProxy readMemoryBypassingCacheAt: addr Size: n IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: snapshotting\x7fCategory: loading snapshots\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryFrom: fileName = ( |
            | 
            readMemoryFrom: fileName IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: snapshotting\x7fCategory: loading snapshots\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryFrom: fileName IfFail: fb = ( |
            | 
            useUnixDebugServerForSnapshots ifTrue: [ 
              safelyDo:[safeProxy readMemoryFrom: fileName IfFail: raiseError].
            ] False: [ error: 'unimp']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryWordAt: addr IfFail: fb = ( |
            | 
            [todo cleanup localMemoryInterface].
            "Should there be a localMemoryInterface? (That would make this
             method cleaner, and there are probably other methods on this
             object (like readMemoryAt:Size:IfFail:) that should be
             double-dispatched through the lens."
            lens readMemoryWordAt: addr InProcess: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryWordsAt: addr Size: n Do: blk IfFail: fb = ( |
            | 
            safelyDo: [
             safeProxy readMemoryWordsAt: addr Size: n Do: blk IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         readRemoteMemoryWordAt: addr IfFail: fb = ( |
            | 
            safelyDo: [
             safeProxy readMemoryWordAt: addr IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteAllocatedRegionFor: pc = ( |
            | 
            [todo cleanup allocatedRegions]. "Checking the universe should work in both the
                                              local case and the remote case, once Alex's
                                              universe-updating stuff is done, shouldn't
                                              it? -- Adam"

            safelyDo: [
              safeProxy allocatedRegionFor: pc
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteOopAt: addr IfFail: fb = ( |
            | 
            readMemoryWordAt: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: snapshotting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         removeFiles: fileList IfFail: fb = ( |
            | 
            "useUnixDebugServerForSnapshots ifTrue:["
              safelyDo: [ fileList do: [ |:file|
                  safeProxy removeFile: file IfFail: fb]]
            "] False: [unimp]").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         resetCauseOfError = ( |
            | 
            [todo processHierarchy]. "Copied from traits process."
            causeOfError: processErrors ok).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fCategory: preemption\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot'
        
         resetPreemption = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: synchronizing proxy access\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         safeProxy = ( |
            | 
            sema isHeldByThisProcess ifFalse: [
              error: 'my caller should be enclosed in safelyDo:[]'
            ].
            myProxy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: synchronizing proxy access\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         safelyDo: blk = ( |
            | 
            sema protect: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: synchronizing proxy access\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         safelyDo: blk IfBusy: busyBlk = ( |
            | 
            sema protect: blk TimeOut: 200 IfTimedOut: busyBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: setting up global registers\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         setGlobalRegisters = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fCategory: preemption\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot'
        
         setPreemption = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: downloading and starting programs\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         setServerProxy: serverProxy = ( |
            | 
            myProxy:
              isDebuggingPerformance ifFalse: [serverProxy]
                                        True: [longMessageLoggingSender copyForTarget: serverProxy]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         singleStep = ( |
            | 
            currentActivation isReflecteeMachineLevelForeignActivation ifTrue: [
              stepOneMachineInstruction.
            ] False: [
              stepOneBytecode.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         singleStepAt: newPC = ( |
            | 
            safelyDo: [safeProxy singleStepAt: newPC].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         stack = ( |
            | 
            stackWithLimit: maxSmallInt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         stackWithLimit: max = ( |
            | 
            myVMKit foreignProcessStack copyForProcess: self Limit: max).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         startActivation = ( |
             a.
            | 
            currentActivation meAndSendersDo: [|:sender| a: sender].
            a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fComment: Returns the address of the first indexable in the
object table. -- Adam, 7/06\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         startOfObjectAddressesIfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            myProxy ifNil: '' IfNotNil: ['on ', hostName]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: querying\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         status = ( |
            | 
            safelyDo: [
              safeProxy statusStringIfFail: [|:e| ^ 'error: ', e]
            ]
            IfBusy: 'process sema busy').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         step = ( |
            | singleStep).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: scheduling\x7fCategory: stepping\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         stepAndSkipSimpleBytecodes = ( |
            | 
            singleStep).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         stepOneBytecode = ( |
            | 
            setPreemption.
            continueWithoutResettingPreemption).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         stepOneInstruction = ( |
            | 
            singleStep).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: controlling execution\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         suspend = ( |
            | 
            safelyDo: [safeProxy suspend].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: Klein VM export and debugging\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         theVM = ( |
            | 
            theVMIfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: Klein VM export and debugging\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         theVMIfAbsent: blk = ( |
            | 
            myVM ifNil: [blk value: 'no VM']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: InitializeToExpression: (true)\x7fVisibility: private'
        
         useUnixDebugServerForSnapshots = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fComment: if size = -1, size according to next mark\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         wordsAt: addr Size: size = ( |
            | 
            myVMKit machineLevelWordVector
              copyMachineMemory: myVM machineMemory
                        Address: addr
                           Size: size = -1 ifFalse: [size]
                                              True: [myVMKit layouts memoryObject wordSizeOf:
                                                       myVMKit layouts memoryObject memForAddress: addr]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         write: bv ToMemoryAt: addr IfFail: fb = ( |
            | 
            safelyDo: [
              safeProxy write: bv ToMemoryAt: addr IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: snapshotting\x7fCategory: saving snapshots\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         writeMemoryTo: fileName AddressRanges: addrVector = ( |
            | 
            writeMemoryTo: fileName AddressRanges: addrVector IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: snapshotting\x7fCategory: saving snapshots\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         writeMemoryTo: fileName AddressRanges: addrVector IfFail: fb = ( |
            | 
            useUnixDebugServerForSnapshots ifTrue: [ |bv|
              bv: byteVector copyFromBigEndianInt32: addrVector
                                             IfFail: ['failed!?' printLine. ^ error: 'could  not build byte vector'].
              safelyDo:[safeProxy writeMemoryTo: fileName
                                  AddressRanges: bv
                                         IfFail: raiseError].
            ] False: [|integerState. memoryContents. file. writer|
              safelyDo: [
                integerState: byteVector 
                                copyFromBigEndianInt32: (safeProxy getIntegerState asVector)
                                                IfFail: fb
              ].
              file: os_file copy openForWriting: fileName.
              writer: myVMKit snapshotWriter copyForFile: file.
              writer writeData: integerState IfFail: fb.
              writer writeVector: addrVector.
              0 to: (addrVector size - 1 ) By: 2 Do: [|:i|

                safelyDo:[
                  memoryContents: 
                      safeProxy readMemoryAt: (addrVector at: i)
                                        Size: ((addrVector at: i+1) - (addrVector at: i)) asSmallInteger.
                  writer writeInteger:  (memoryContents size) IfFail: fb.
                  writer writeData:      memoryContents      IfFail: fb.
                ]
              ].
              file close
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: snapshotting\x7fCategory: saving snapshots\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         writeMemoryTo: fileName From: addr Length: n IfFail: fb = ( |
             addrVector.
            | 
            addrVector: 
               byteVector copyFromBigEndianInt32:
                          ((addr & (addr+n)) asVector copySort mapBy: [|:i| i asInt32])
                                          IfFail: [ ^ error: 'failed to create vector'].
            writeMemoryTo: fileName AddressRanges: addrVector IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> 'parent' -> () From: ( | {
         'Category: inspecting\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         writeWord: w ToMemoryAt: addr IfFail: fb = ( |
            | 
            safelyDo: [
              safeProxy writeWord: w ToMemoryAt: addr IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         savedSPLimits <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: InitializeToExpression: (recursiveSemaphore copyBinary)\x7fVisibility: private'
        
         sema <- recursiveSemaphore copyBinary.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcessStack = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'foreignProcessStack') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals processStack copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'foreignProcessStack') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignProcessStack.

CopyDowns:
globals processStack. copy 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'foreignProcessStack') -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         isSourceLevel <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'foreignProcessStack') -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'foreignProcessStack') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignProcessStack parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'foreignProcessStack') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForProcess: p Limit: maxSize = ( |
             actStack.
             r.
            | 
            actStack: p activationStackLimit: maxSize IfFail: vector.
            r: (copySize: actStack size) myProcess: p.
            actStack size do: [|:i|
              r at: i Put:  (actStack at: i) initNumber: i Process: p
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'foreignProcessStack') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'processStack' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'foreignProcessStack') -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         prototype = ( |
            | 
            kleinAndYoda foreignProcessStack).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         allocatedRegionFor: pc InProcess: aProcess = ( |
            | 
            aProcess localAllocatedRegionFor: pc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         isProcessAlive: p = ( |
            | 
            p isLocalProcessAlive).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         oopAt: addr InProcess: aProcess IfFail: fb = ( |
            | 
            aProcess localOopAt: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryWordAt: addr InProcess: aProcess IfFail: fb = ( |
            | 
            aProcess readLocalMemoryWordAt: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         allocatedRegionFor: pc InProcess: aProcess = ( |
            | 
            aProcess remoteAllocatedRegionFor: pc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         isProcessAlive: p = ( |
            | 
            p isRemoteProcessAlive).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         oopAt: addr InProcess: aProcess IfFail: fb = ( |
            | 
            aProcess remoteOopAt: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: processes\x7fModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryWordAt: addr InProcess: aProcess IfFail: fb = ( |
            | 
            aProcess readRemoteMemoryWordAt: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: FollowSlot'
        
         vmKitProcess = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProcess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProcess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitProcess.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.11 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProcess' -> () From: ( | {
         'ModuleInfo: Module: vmKitProcess InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitProcess postFileIn
