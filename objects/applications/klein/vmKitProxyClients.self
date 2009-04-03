 '$Revision: 30.10 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: interface to unixDebugServer\x7fCategory: proxy clients -- unixDebugServer protocol\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         debuggingProxyClients = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         caching = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients caching.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         any = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients caching any.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Category: cache\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (-1)\x7fVisibility: private'
        
         cachedDebuggeeTask <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Category: cache\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedIntegerState.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Category: cache\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         cachedMemoryPages <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Category: cache\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         cachedMemoryWords <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Category: cache\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedObjectLocatorTimestamp.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Category: cache\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedObjectLocatorTimestampAddress.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Category: cache\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (-1)\x7fVisibility: private'
        
         cachedRunState <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Category: cache\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedYodaWellKnownObjectsAddress.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Category: cache\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (time origin)\x7fVisibility: private'
        
         lastTimeDebuggeeWasSuspended <- time origin.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Category: cache\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (10000)\x7fVisibility: private'
        
         memoryPageSize <- 10000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Category: cache\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         mustAllowSlopPeriod <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Comment: my noncaching proxy client\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         npc.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients caching any parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: allocating memory\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateMemoryAt: addr Size: size Anywhere: any IfFail: fb = ( |
            | npc allocateMemoryAt: addr Size: size Anywhere: any IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: allocating memory\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         allocatedRegionFor: pc = ( |
            | npc allocatedRegionFor: pc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         allocatedRegions = ( |
            | npc allocatedRegions).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: my & other caches\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         allowInfiniteSlopDuring: blk = ( |
             oldValue.
            | 
            oldValue: mustAllowSlopPeriod.
            mustAllowSlopPeriod: true.
            blk onReturn: [mustAllowSlopPeriod: oldValue]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         applicationInformation = ( |
            | 
            npc applicationInformation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         architecture = ( |
            | npc architecture).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: running debuggee\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         attachIfFail: fb = ( |
            | npc attachIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fCategory: converting addresses to/from page numbers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         bytesForPageNumber: pageNumber IfFail: fb = ( |
            | 
            cachedMemoryPages at: pageNumber IfAbsentPut: [
              npc readMemoryAt: (startingAddressForPageNumber: pageNumber)
                             Size: memoryPageSize
                           IfFail: [|:e| ^ fb value: e]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: smallSelf specific\x7fCategory: layout constants\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         checkLayoutConstantsIfFail: fb = ( |
            | npc checkLayoutConstantsIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: continuing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         continueAt: newPC IfFail: fb = ( |
            | npc continueAt: newPC IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: initiating & terminating\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         copyAndInitiateIfFail: fb = ( |
            | 
            ( npc copyAndInitiateIfFail: [|:e| ^ fb value: e]) interposeCache).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         copyCaching: aNoncachingClientProxy = ( |
            | copy initializeCachedMemoryDictionaries npc: aNoncachingClientProxy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: my & other caches\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         debuggeeChanged = ( |
            | 
            invalidateCaches).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         debuggeePID = ( |
            | npc debuggeePID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         debuggeeTaskIfSucceed: sb IfFail: fb = ( |
            | 
            safelyDo: [
              invalidateMyObsoleteCachedItems.
              cachedDebuggeeTask = -1 ifTrue: [
                npc debuggeeTaskIfSucceed: [|:t| cachedDebuggeeTask: t]
                                      IfFail: [|:e| ^ fb value: e].
              ].
              sb value: cachedDebuggeeTask
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: starting the server\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         defaultPort = ( |
            | npc defaultPort).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: detaching\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         detachIfFail: fb = ( |
            | npc detachIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: downloading\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         download: aVMImage ForForeignProcess: aForeignProcess IfFail: fb = ( |
            | npc download: aVMImage ForForeignProcess: aForeignProcess IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fCategory: converting addresses to/from page numbers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         from: startAddr To: endAddr PageNumbersAndStartAndEndPositionsDo: blk = ( |
             endPageNumber.
             size.
             startPageNumber.
            | 
            size: endAddr - startAddr.
            startPageNumber: pageNumberForAddress: startAddr.
              endPageNumber: pageNumberForAddress:   endAddr pred.
            startPageNumber to: endPageNumber Do: [|:pageNumber. pageStartPos. pageEndPos|
              pageStartPos:  0              max: startAddr - (startingAddressForPageNumber: pageNumber).
              pageEndPos:    memoryPageSize min:   endAddr - (startingAddressForPageNumber: pageNumber).
              blk value: pageNumber With: pageStartPos With: pageEndPos.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: getting fixed-address-buffer base and length\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getBaseAndLengthIfFail: fb = ( |
            | npc getBaseAndLengthIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getIntegerStateIfFail: fb = ( |
            | 
            safelyDo: [
              invalidateMyObsoleteCachedItems.
              cachedIntegerState ifNil: [
                cachedIntegerState: npc getIntegerStateIfFail: fb.
              ].
              cachedIntegerState
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getObjectLocatorTimestampAddressIfFail: fb = ( |
            | 
            cachedObjectLocatorTimestampAddress ifNil: [
              [!== kleinAndYoda debuggingProxyClients caching any] assert.
              cachedObjectLocatorTimestampAddress: npc getObjectLocatorTimestampAddressIfFail: fb
            ].
            cachedObjectLocatorTimestampAddress).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: getting address of start function\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getStartFunctionAddressIfFail: fb = ( |
            | npc getStartFunctionAddressIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fCategory: converting addresses to/from page numbers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getYodaWellKnownObjectsAddressIfFail: fb = ( |
            | 
            cachedYodaWellKnownObjectsAddress ifNil: [
              [!== kleinAndYoda debuggingProxyClients caching any] assert.
              cachedYodaWellKnownObjectsAddress: npc getYodaWellKnownObjectsAddressIfFail: fb
            ].
            cachedYodaWellKnownObjectsAddress).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         hostName = ( |
            | npc hostName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         ifWaitStatus: s IsRunning: runningBlk IsExited: exitedBlk IsTerminatedBySignal: signalledBlk IsStopped: stoppedBlk = ( |
            | npc ifWaitStatus: s IsRunning: runningBlk IsExited: exitedBlk IsTerminatedBySignal: signalledBlk IsStopped: stoppedBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         inPage: bytes WhichStartsAtAddress: startAddrOfPage From: startPos UpTo: endPos WordsDo: blk = ( |
            | 
            startPos upTo: endPos By: oopSize Do: [|:i. addr. w|
              addr: startAddrOfPage + i.
              w: (myAssemblerSystem intNNFromBytes: bytes At: i) asSmallIntegerIfPossible.
              blk value: w With: addr
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fCategory: converting addresses to/from page numbers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         indexForAddress: addr InPageNumber: pageNumber = ( |
            | 
            addr - (pageNumber * memoryPageSize)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: just my own cached items\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeCachedMemoryDictionaries = ( |
            | 
            cachedMemoryPages: dictionary copyRemoveAll.
            cachedMemoryWords: dictionary copyRemoveAll.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = ( |
            | npc intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: just my own cached items\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         invalidateCachedItems = ( |
            | 
            cachedDebuggeeTask:     -1.
            cachedIntegerState:    nil.
            cachedRunState:         -1.
            cachedObjectLocatorTimestamp: nil.
            initializeCachedMemoryDictionaries.

            timestampOfOldestCachedItem: time current.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: my & other caches\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateCaches = ( |
            | 
            lastTimeDebuggeeWasSuspended: time current).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: just my own cached items\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         invalidateMyObsoleteCachedItems = ( |
            | 
            invalidateObsoleteCachedItemsIn: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: my & other caches\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateObsoleteCachedItemsIn: cachingWobulator = ( |
            | 
            "Note that this is only approximate, slop-wise. -- dmu 12/05"
            (isReasonableToCacheIfLastRetrievedAt: cachingWobulator timestampOfOldestCachedItem)
              ifFalse: [cachingWobulator invalidateCachedItems].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: my & other caches\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         isReasonableToCacheIfLastRetrievedAt: t = ( |
            | 
            (isWithinSlopPeriod: t) || [isSafeToCacheIfLastRetrievedAt: t]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: my & other caches\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         isSafeToCacheIfLastRetrievedAt: t = ( |
            | 
            isDebuggeeSuspended && [lastTimeDebuggeeWasSuspended < t]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: my & other caches\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         isWithinSlopPeriod: t = ( |
            | 
              mustAllowSlopPeriod ifTrue:  [^  true].
            shouldAllowSlopPeriod ifFalse: [^ false].

            "Slop is only to avoid constant updating while things are running.
              -- dmu 12/05"
            lastKnownWaitStatus = npc wRunning ifFalse: [^ false].

            (t + slopPeriod) >= time current).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: killing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         killIfFail: fb = ( |
            | npc killIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         lastKnownWaitStatus = ( |
            | npc lastKnownWaitStatus).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: multistepping\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         multiStep: n TimesOrToPC: limitPC At: newPC IfFail: fb = ( |
            | npc multiStep: n TimesOrToPC: limitPC At: newPC IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | 
            npc myAssemblerSystem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         objectLocatorTimestampAt: addr IfFail: fb = ( |
            | 
            cachedObjectLocatorTimestamp ifNil: [
              [!== kleinAndYoda debuggingProxyClients caching any] assert.
              cachedObjectLocatorTimestamp: npc objectLocatorTimestampAt: addr IfFail: fb.
            ].
            cachedObjectLocatorTimestamp).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         oopSize = ( |
            | 
            npc oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fCategory: converting addresses to/from page numbers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         pageNumberForAddress: addr = ( |
            | 
            addr asUnsignedInteger / memoryPageSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: pinging\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         pingIfFail: fb = ( |
            | npc pingIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: running debuggee\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         pingIfSucceed: sb IfFail: fb = ( |
            | npc pingIfSucceed: sb IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryAt: startAddr Size: s IfFail: fb = ( |
            | 
            safelyDo: [| endAddr. bytes. pos <- 0 |
              invalidateMyObsoleteCachedItems.

              bytes: byteVector copySize: s.
              endAddr: startAddr + s.
              from: startAddr To: endAddr PageNumbersAndStartAndEndPositionsDo: [|:pageNumber. :pageStartPos. :pageEndPos. len. pageBytes|
                pageBytes: bytesForPageNumber: pageNumber IfFail: [|:e| ^ fb value: e].
                len: pageEndPos - pageStartPos.
                bytes copyRangeDstPos: pos
                             SrcArray: pageBytes
                               SrcPos: pageStartPos
                                  Len: len.
                pos: pos + len.
              ].
              [pos = s] assert.
              bytes
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: snapshotting\x7fCategory: loading snapshots\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryFrom: fileName IfFail: fb = ( |
            | 
             npc readMemoryFrom: fileName IfFail: fb.
            debuggeeChanged).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryWordAt: i IfFail: fb = ( |
            | 
            safelyDo: [
              invalidateMyObsoleteCachedItems.
              wordAtAddress: i InPageDictionaryIfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryWordsAt: startAddr Size: n Do: blk IfFail: fb = ( |
            | 
            safelyDo: [| endAddr |
              invalidateMyObsoleteCachedItems.

              endAddr: startAddr + (n * oopSize).
              from: startAddr To: endAddr PageNumbersAndStartAndEndPositionsDo: [|:pageNumber. :pageStartPos. :pageEndPos|
                              inPage: (bytesForPageNumber: pageNumber IfFail: [|:e| ^ fb value: e])
                WhichStartsAtAddress: (startingAddressForPageNumber: pageNumber)
                                From: pageStartPos
                                UpTo: pageEndPos
                             WordsDo: blk.
              ].
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: snapshotting\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         removeFile: fileName IfFail: fb = ( |
            | npc removeFile: fileName IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: getting return handler address\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         returnHandlerIfFail: fb = ( |
            | npc returnHandlerIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: getting thread\'s run state\x7fCategory: run state conversions\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         runStateForString: s = ( |
            | npc runStateForString: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         runStateIfFail: fb = ( |
            | 
            safelyDo: [
              invalidateMyObsoleteCachedItems.
              cachedRunState = -1 ifTrue: [
                cachedRunState: npc runStateIfFail: [|:e| ^ fb value: e].
              ].
              cachedRunState
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: getting thread\'s run state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         runStateStringIfFail: fb = ( |
            | npc runStateStringIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: helpers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         safelyDo: blk = ( |
            | npc safelyDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         serverPID = ( |
            | npc serverPID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         setContentsOfRegister: regName To: aNumber IfFail: fb = ( |
            | 
            "When we set the contents of a register, we don't just set
             the one register - we pull out the whole integerState,
             change the one register, and then set the whole integerState.
             So we never want to allow slop when we're doing this - we
             always want to have the correct integerState before we set it."
            (isSafeToCacheIfLastRetrievedAt: timestampOfOldestCachedItem) ifFalse: [
              cachedIntegerState: nil.
            ].
            resend.setContentsOfRegister: regName To: aNumber IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         setIntegerStateFrom: anIntegerState IfFail: fb = ( |
            | 
            npc setIntegerStateFrom: anIntegerState IfFail: fb.
            debuggeeChanged).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: smallSelf specific\x7fCategory: setting C variables\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         setVMOopTo: vmOop AndObjectTableAddressTo: objectTableAddr IfFail: fb = ( |
            | npc setVMOopTo: vmOop AndObjectTableAddressTo: objectTableAddr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         shortcuts* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent shortcuts.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         shortcuts* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: signalling\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         signal: sigNo IfFail: fb = ( |
            | npc signal: sigNo IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: single stepping\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         singleStepAt: newPC IfFail: fb = ( |
            | npc singleStepAt: newPC IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: my & other caches\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         slopPeriod = ( |
            | 
            "can set this higher to test caching -- dmu 12/05"
            "I set it lower to make the debugger more responsive.."
            300).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: starting the server\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         startServerOn: hostName Port: p = ( |
            | npc startServerOn: hostName Port: p).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fCategory: converting addresses to/from page numbers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         startingAddressForPageNumber: pageNumber = ( |
            | 
            (pageNumber * memoryPageSize) asInt32).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: printing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | npc statePrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: getting thread\'s run state\x7fCategory: run state conversions\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         stringForRunState: i = ( |
            | npc stringForRunState: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         stringForWaitStatus: s = ( |
            | npc stringForWaitStatus: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: suspending\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         suspendIfFail: fb = ( |
            | npc suspendIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: getting task ID\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         taskForPID: pid IfFail: fb = ( |
            | npc taskForPID: pid IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: initiating & terminating\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         terminate = ( |
            | npc terminate).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: testing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         testDownload = ( |
            | npc testDownload).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: testing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         testPing = ( |
            | npc testPing).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         waitStatusIfFail: fb = ( |
            | 
            safelyDo: [
              |was. is|
              was: lastKnownWaitStatus.
              "If we know that the process wasn't running, and we
               haven't done anything to make it start running again,
               we don't need to send any messages over to the
               debug server to check what the wait status is - we
               know it's still whatever it was."
              was = npc wRunning ifFalse: [^ was].

              is: npc waitStatusIfFail: [|:e| ^ fb value: e].
              "was running, not running now"
              is = npc wRunning ifFalse: [invalidateCaches].
              is
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fComment: Get Unix wait status, return 
<waitpid result pid> @ status
-- dmu 1/02\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         waitStatusOfPID: p IfFail: fb = ( |
            | npc waitStatusOfPID: p IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         waitStatusStringIfFail: fb = ( |
            | npc waitStatusStringIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: cached queries\x7fCategory: converting addresses to/from page numbers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         wordAtAddress: addr InPageDictionaryIfFail: fb = ( |
             pageBytes.
             pageNumber.
            | 
            pageNumber: pageNumberForAddress: addr.
            pageBytes: bytesForPageNumber: pageNumber IfFail: [|:e| ^ fb value: e].
            (myAssemblerSystem intNNFromBytes: pageBytes 
                                            At: (indexForAddress: addr InPageNumber: pageNumber)
            ) asSmallIntegerIfPossible).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: accessing the debuggee\x7fCategory: accessing memory\x7fCategory: bytes\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         write: aByteArray ToMemoryAt: addr IfFail: fb = ( |
            | 
            npc write: aByteArray ToMemoryAt: addr IfFail: fb.
            debuggeeChanged).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: forwarding\x7fCategory: snapshotting\x7fCategory: saving snapshot\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         writeMemoryTo: fileName AddressRanges: addrVector IfFail: fb = ( |
            | npc writeMemoryTo: fileName AddressRanges: addrVector IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing memory\x7fCategory: words\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         writeWord: w ToMemoryAt: i IfFail: fb = ( |
            | 
            resend.writeWord: w ToMemoryAt: i IfFail: fb.
            debuggeeChanged).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         yoda_shortcuts* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'yoda_shortcuts' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Category: cache\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (true)\x7fVisibility: private'
        
         shouldAllowSlopPeriod <- bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'caching' -> 'any' -> () From: ( | {
         'Category: cache\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (time origin)\x7fVisibility: private'
        
         timestampOfOldestCachedItem <- time origin.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         noncaching = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> () From: ( | {
         'Category: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         allocatedRegions <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> () From: ( | {
         'Category: server state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         applicationInformation.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> () From: ( | {
         'Category: server state\x7fComment: The assembler system name
of the ISA. -- dmu 12/05\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (\'not specified\')'
        
         architecture <- 'not specified'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> () From: ( | {
         'Category: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (-1)'
        
         debuggeePID <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> () From: ( | {
         'Category: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (\'localhost\')'
        
         hostName <- 'localhost'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> () From: ( | {
         'Category: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (-1)\x7fVisibility: private'
        
         lastKnownWaitStatus <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: downloading\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         allocate: length ForDownloadIfFail: fb = ( |
             useStaticBuffer = bootstrap stub -> 'globals' -> 'true' -> ().
            | 
            allocatedRegions add: (
              useStaticBuffer ifFalse: [
                | allocStart |
                allocStart: allocateMemorySize: length IfFail: [|:e| ^ fb value: e].
                allocStart @ (start + length)
              ]
              True: [| bal |
                bal: getBaseAndLengthIfFail: [ |:e| ^ fb value: e].
                bal x @ (bal x + bal y)
              ]).
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: allocating memory\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateMemoryAt: addr Size: size Anywhere: any IfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                  | r |
                         writeMachRequestType: machRequestTypes allocate_memory IfFail: exit.
                         writePIDIfFail: exit.
                  socket writeInt: addr IfFail: exit.
                  socket writeInt: size IfFail: exit.
                  socket writeByte: any asInteger IfFail: exit.
                  socket flushOutputIfFail:  exit.

                  r: socket readIntIfFail: exit.
                            readStatusIfFail: exit.
                  endPoint: endPoint max: r + size.
                  allocatedRegions addLast: r @ (r + size).
                  r
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: allocating memory\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         allocatedRegionFor: pc = ( |
            | 
            allocatedRegions
                     findFirst: [|:r| (r x <= pc) && [pc < r y]]
                     IfPresent: [|:r| r]
                      IfAbsent: [^ pc @ pc succ ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         appInfoProtos = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent appInfoProtos.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: running debuggee\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         attachIfFail: fb = ( |
            | 
            ptraceRequest: pt_attach Address: 0 Data: 0 IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: continuing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         continueAt: newPC IfFail: fb = ( |
            | 
            safelyDo: [
              ptraceRequest: pt_continue Address: newPC Data: 0 IfFail: fb.
              mustCheckWaitStatusInFuture.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            ((((resend.copy 
               allocatedRegions: allocatedRegions copy)
               sema: sema copyBinary)
               requestSema: requestSema copyBinary)
               socket: nil)
              mustCheckWaitStatusInFuture).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initiating & terminating\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         copyAndInitiateIfFail: fb = ( |
            | 
            copy initiateIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForApplication: app = ( |
             r.
            | 
            r: copy.
            [klein. yoda]. "browsing"
            r applicationInformation: app sendTo: appInfoProtos.
            r port: r applicationInformation defaultPort.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForArchitecture: arch = ( |
            | 
            arch = 'ppc' ifFalse: [error: 'only ppc supported'].
            copy architecture: arch).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForHost: h = ( |
            | 
            copy hostName: h).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForHost: h Port: p = ( |
            | 
            (copyForHost: h) port: p).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: starting the server\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         defaultPort = ( |
            | applicationInformation defaultPort).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: detaching\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         detachIfFail: fb = ( |
            | 
            safelyDo: [
              ptraceRequest: pt_detach Address: 0 Data: 0 IfFail: fb.
              mustCheckWaitStatusInFuture.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: downloading\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         download: aVMImage ForForeignProcess: aForeignProcess IfFail: fb = ( |
            | 
            safelyDo: [
              | debuggeeProxy. debuggerProxy. bytes. allocStart. mmi. rh |

              "fork off debugggee"

              debuggeeProxy:  copyAndInitiateIfFail: [|:e| ^ fb value: e].
              debuggerProxy:  copyAndInitiateIfFail: [|:e| ^ fb value: e].

              debuggerProxy debuggeePID: debuggeeProxy serverPID.
              debuggeeProxy debuggeePID: debuggeeProxy serverPID.

              rh: debuggeeProxy returnHandler.

              debuggeeProxy allocate: aVMImage launchSize ForDownloadIfFail: [|:e| ^ fb value: e].
              allocStart: debuggeeProxy allocatedRegions first x.
              debuggerProxy allocatedRegions: debuggeeProxy allocatedRegions.

              aVMImage relocateTo: allocStart
                          WriteTo: debuggeeProxy
                    PointMemoryTo: aForeignProcess 
                           IfFail: [|:e| ^ fb value: e].
              applicationInformation sendBootstrapInfo: aVMImage Via: debuggeeProxy.

              debuggerProxy attachIfFail: [|:e| ^ fb value: e].
              debuggeeProxy socket close.
              debuggerProxy mustCheckWaitStatusInFuture.

              aForeignProcess setServerProxy: debuggerProxy interposeCache.
              aForeignProcess setGlobalRegisters.

              debuggerProxy setPC: applicationInformation startPCForImage: aVMImage Via: debuggerProxy.
              debuggerProxy setReturnAddress: rh. "ensure debuggee does not return into random place in proxy"

              self
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fCategory: not done yet\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         floatState = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'floatState' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent floatState.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'floatState' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (vector copySize: 32 FillingWith: 0.0)\x7fVisibility: public'
        
         fprs <- vector copySize: 32 FillingWith: 0.0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'floatState' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         fpscr <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'floatState' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'floatState' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent floatState parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting fixed-address-buffer base and length\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getBaseAndLengthIfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [ | base. length |
                writeRequestType: requestTypes getBaseAndLength IfFail: exit.
                        socket flushOutputIfFail: exit.
                base:   socket     readIntIfFail: exit.
                length: socket     readIntIfFail: exit.
                base @ length
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getIntegerStateIfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                       writeMachRequestType:   machRequestTypes get_integer_state
                                     IfFail:   exit.
                       writePIDIfFail:         exit.
                socket flushOutputIfFail:      exit.
                       readStatusIfFail:       exit.
                       readIntegerStateIfFail: exit.
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting address of start function\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getStartFunctionAddressIfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [ | addr |
                writeYodaRequestType: yodaRequestTypes getStartFunctionAddress IfFail: exit.
                        socket flushOutputIfFail: exit.
                addr:   socket     readIntIfFail: exit.
                readStatusIfFail:                 exit.
                addr
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         ifWaitStatus: s IsRunning: runningBlk IsExited: exitedBlk IsTerminatedBySignal: signalledBlk IsStopped: stoppedBlk = ( |
             wStatus.
            | 
            "see /usr/include/sys/wait.h"
            wStatus: s && 8r177.
            case if:   [ s = wRunning ] "my hack"
                 Then: runningBlk
                 If:   [ wStatus = wStopped ]
                 Then: [ |wStopSig|    wStopSig:    s >> 8.  stoppedBlk value: wStopSig ]
                 If:   [ wStatus = 0    ]
                 Then: [ |wExitStatus| wExitStatus: s >> 8.  exitedBlk  value: wExitStatus ]
                 Else: [ |termSig. coredump|
                         termSig: s >> 8.  coredump: s && 8r200.
                         signalledBlk value: termSig With: coredump asBoolean]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initiating & terminating\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         initiateIfFail: fb = ( |
            | 
            safelyDo: [ 
              | myfb. resp.  |
              myfb: [|:e| socket ifNotNil: [socket close]. ^ fb value: e].

              socket: vmKit bufferedDebuggingProxySocket
                           open: hostName 
                           Port: port
                         IfFail: myfb.
              socket writeBytes: initiationRequest IfFail: myfb.
              socket flushOutputIfFail: myfb.

              resp: socket readStringIfFail: myfb.
              resp = initiationResponse  ifFalse: [
                myfb value: 'bad initiation response: ', resp
              ].
              [ppc. linux]. "browsing"
              architecture:  socket readStringIfFail: myfb.
              serverPID:    socket readIntIfFail: myfb.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fCategory: protocol constants\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         initiationRequest = ( |
            | 
            'request_', applicationInformation initiationName, '_', protocolVersion).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fCategory: protocol constants\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         initiationResponse = ( |
            | 
            'respond_', applicationInformation initiationName, '_', protocolVersion).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = ( |
            | vmKit layouts abstract intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         interposeCache = ( |
            | 
            vmKit debuggingProxyClients caching any copyCaching: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: my & other caches\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateObsoleteCachedItemsIn: cachingWobulator = ( |
            | 
            cachingWobulator invalidateCachedItems.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: my & other caches\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         isReasonableToCacheIfLastRetrievedAt: t = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: killing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         killIfFail: fb = ( |
            | 
            safelyDo: [
              ptraceRequest: pt_kill Address: 0 Data: 0 IfFail: fb.
              mustCheckWaitStatusInFuture.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: protocol constants\x7fComment: Should move down to macOSX parent,
but for now allocateMemoryAt:Size:Aywhere:IfFail: uses this.
-- dmu 12/05\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         machRequestTypes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( |
             {} = 'Comment: This list needs to match up
with machDebugServer.hh.
See enum mach_request_type_t.\x7fModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent machRequestTypes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         allocate_memory = 10.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         get_integer_state = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         get_memory = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         multi_step = 9.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         read_memory_from_file = 12.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         remove_file = 13.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         run_state = 7.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         set_integer_state = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         set_memory = 6.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         single_step = 8.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         task_for_pid = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         threads_for_task = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'machRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         write_memory_to_file = 11.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: multistepping\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         multiStep: n TimesOrToPC: limitPC At: newPC IfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                writeMachRequestType: machRequestTypes multi_step
                              IfFail: exit.
                writePIDIfFail:       exit.
                socket writeInt: newPC    IfFail: exit.
                socket writeInt:       n  IfFail: exit.
                socket writeInt: limitPC  IfFail: exit.
                socket flushOutputIfFail:  exit.
                readStatusIfFail:          exit.
                mustCheckWaitStatusInFuture.
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         mustCheckWaitStatusInFuture = ( |
            | lastKnownWaitStatus: wRunning).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         myAssemblerSystem = ( |
            | 
            [ppc. sparc]. "browsing"
            architecture sendTo: assemblerSystems).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         oopSize = ( |
            | vmKit layouts abstract oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: pinging\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         pingIfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                | r |
                writeRequestType: requestTypes ping
                          IfFail:  exit.
                writePIDIfFail:    exit.
                socket flushOutputIfFail: exit.
                r: socket readIntIfFail:  exit.
                readStatusIfFail:  exit.
                r
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: running debuggee\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         pingIfSucceed: sb IfFail: fb = ( |
             p.
            | 
            p: copyAndInitiateIfFail: [|:e| ^ fb value: e].
            [sb value: p] onReturn: [p terminate]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fCategory: protocol constants\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         protocolVersion = '1.5'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: ptracing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         ptraceRequest: req Address: addr Data: data IfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                | r |
                writeRequestType: requestTypes ptrace
                          IfFail: exit.
                (req & debuggeePID & addr & data) asVector do: [|:x|
                  socket writeInt: x IfFail: exit.
                ].
                socket flushOutputIfFail: exit.
                r: socket readIntIfFail:  exit.
                readStatusIfFail:  exit.
                r
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fCategory: protocol constants\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         ptraceRequests* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'ptraceRequests' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent ptraceRequests.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fCategory: reading\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         readIntegerStateIfFail: fb = ( |
            | 
            [ppc. i386].
            (architecture sendTo: integerState)
              copyFromVector:
                socket readIntArrayIfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing memory\x7fCategory: bytes\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryAt: addr Size: n IfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                   | r |
                          writeMachRequestType:  machRequestTypes get_memory
                                        IfFail:  exit.
                          writePIDIfFail:        exit.
                   socket writeInt: addr IfFail: exit.
                   socket writeInt: n    IfFail: exit.
                   socket flushOutputIfFail:     exit.
                r: socket readBytesIfFail:       exit.
                          readStatusIfFail:      exit.
                r
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: snapshotting\x7fCategory: loading snapshots\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryFrom: fileName IfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                          writeMachRequestType:  machRequestTypes read_memory_from_file
                                                IfFail: exit.
                          writePIDIfFail:               exit.
                   socket writeString: fileName IfFail: exit.      
                   socket flushOutputIfFail:            exit.
                          readStatusIfFail:             exit.
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fCategory: reading\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         readStatusIfFail: fb = ( |
             error.
            | 
            error: socket readStringIfFail: [|:e| ^ fb value: 'could not read status: ', e].
            error isEmpty ifTrue: [self] False: [fb value: 'remote error: ', error]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: snapshotting\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         removeFile: fileName IfFail: fb = ( |
            | 
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                | r |
                          writeMachRequestType:  machRequestTypes remove_file
                                                IfFail: exit.
                   socket writeString: fileName IfFail: exit.      
                   socket flushOutputIfFail:            exit.
                          readStatusIfFail:             exit.
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fCategory: protocol constants\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         requestTypes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'requestTypes' -> () From: ( |
             {} = 'Comment: This list needs to match up
with unixDebugServer.hh.
See enum request_type_t\x7fModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent requestTypes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting return handler address\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         returnHandlerIfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                writeRequestType: requestTypes getReturnHandler IfFail: exit.
                socket flushOutputIfFail: exit.
                socket readIntIfFail: exit
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting thread\'s run state\x7fCategory: run state conversions\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         runStateForString: s = ( |
            | 
            s sendTo: runStates).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting thread\'s run state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         runStateIfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                | r |
                writeMachRequestType: machRequestTypes run_state
                         IfFail:  exit.
                writePIDIfFail:  exit.
                socket flushOutputIfFail: exit.
                r: socket readIntIfFail:  exit.
                readStatusIfFail:  exit.
                r
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting thread\'s run state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         runStateStringIfFail: fb = ( |
            | 
            stringForRunState: runStateIfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting thread\'s run state\x7fCategory: run state conversions\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         runStateStrings = bootstrap setObjectAnnotationOf: ( (('zero?')
	& ('running')
	& ('stopped')
	& ('waiting')
	& ('uninterruptible')
	& ('halted')) asVector) From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent runStateStrings.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting thread\'s run state\x7fCategory: run state conversions\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         runStates = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'runStates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent runStates.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fComment: The protocol cannot handle interleaving
by multiple threads, so be sure operations
are serialized. -- dmu 7/03\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         safelyDo: blk = ( |
            | 
            sema protect: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         safelyDoRequest: blk = ( |
            | 
            "Called when we do a single request that must be atomic.
            We use a non-recursive semaphore. -- dmu 3/04"
            requestSema isHeldByThisProcess ifTrue: [
              error: 'Should NEVER happen -- will deadlock'
            ].
            requestSema protect: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: starting the server\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         serverString = ( |
            | 
            '$SELF_WORKING_DIR/bin/mac_osx/', applicationInformation executableName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         setIntegerStateFrom: anIntegerStateOrByteVector IfFail: fb = ( |
             printChangesForDebugging = bootstrap stub -> 'globals' -> 'false' -> ().
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"

            printChangesForDebugging ifTrue: [
              |x. noChange <- true|
              x: getIntegerStateIfFail: [^ self].
              x gprs with: anIntegerStateOrByteVector gprs Do: [|:old. :new. :key|
                old = new ifFalse: [
                  process this objectID  print. '  ' print.
                  ('r', key printString) print. '  ' print.
                  (old printString, '->', new printString) printLine.
                  noChange: false
                ]
              ].
              noChange ifTrue: ['no change' printLine].
            ].


            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                  writeMachRequestType: machRequestTypes set_integer_state
                                IfFail: exit.
                  writePIDIfFail:       exit.

                  anIntegerStateOrByteVector asMirror isReflecteeByteVector
                   ifTrue: [ socket writeInt32ByteArray: anIntegerStateOrByteVector           IfFail: exit ]
                    False: [ socket writeIntArray:       anIntegerStateOrByteVector asVector  IfFail: exit ].

                  socket flushOutputIfFail:    exit.
                  readStatusIfFail:     exit.
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: allocating memory\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateMemoryAt: addr Size: size Anywhere: any = ( |
            | 
            allocateMemoryAt: addr Size: size Anywhere: any IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: allocating memory\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateMemorySize: n = ( |
            | 
            allocateMemorySize: n IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: allocating memory\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateMemorySize: n IfFail: fb = ( |
            | 
            allocateMemoryAt: 0 Size: n Anywhere: true IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         contentsOfRegister: regName = ( |
            | 
            contentsOfRegister: regName IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         contentsOfRegister: regName IfFail: fb = ( |
             s.
            | 
            s: getIntegerStateIfFail: [|:e| ^ fb value: e].
            s at: regName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: continuing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         continue = ( |
            | continueIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: continuing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         continueAt: newPC = ( |
            | 
            continueAt: newPC IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: continuing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         continueIfFail: fb = ( |
            | 
            continueAt: where_it_left_off_bypassing_trap IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: initiating & terminating\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         copyAndInitiate = ( |
            | 
            copyAndInitiateIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: detaching\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         detach = ( |
            | detachIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting fixed-address-buffer base and length\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getBaseAndLength = ( |
            | getBaseAndLengthIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getIntegerState = ( |
            | getIntegerStateIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting address of start function\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getStartFunctionAddress = ( |
            | 
            getStartFunctionAddressIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting address of Yoda active context\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getYodaWellKnownObjectsAddress = ( |
            | getYodaWellKnownObjectsAddressIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         ifDebuggeeIsRunning: rb Exited: eb WasTerminatedBySignal: tb IsStopped: sb = ( |
            | 
              ifDebuggeeIsRunning: rb 
                           Exited: eb
            WasTermiantedBySignal: tb
                        IsStopped: sb
                           IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         ifDebuggeeIsRunning: runningBlk Exited: exitedBlk WasTerminatedBySignal: sigBlk IsStopped: stoppedBlk IfFail: fb = ( |
            | 
                    ifWaitStatus: (waitStatusIfFail: [|:e| ^ fb value: e])
                       IsRunning: runningBlk
                        IsExited: exitedBlk
            IsTerminatedBySignal: sigBlk
                       IsStopped: stoppedBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         isDebuggeeSuspended = ( |
            | 
              ifDebuggeeIsRunning: false
                           Exited: true
            WasTerminatedBySignal: true
                        IsStopped: true
                           IfFail: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: killing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         kill = ( |
            | killIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: multistepping\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         multiStep: n TimesOrToPC: limitPC At: newPC = ( |
            | multiStep: n TimesOrToPC: limitPC At: newPC IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: multistepping\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         multiStep: n TimesOrToPC: limitPC IfFail: fb = ( |
            | 
            multiStep: n TimesOrToPC: limitPC At: where_it_left_off_bypassing_trap IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | theVM myAssemblerSystem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting objectLocator timestamp\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         objectLocatorTimestampIfFail: fb = ( |
             addr.
            | 
            addr: getObjectLocatorTimestampAddressIfFail: [|:e| ^ fb value: e].
            objectLocatorTimestampAt: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         pc = ( |
            | pcIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         pcIfFail: fb = ( |
            | 
            contentsOfRegister: 'pc' IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: pinging\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         ping = ( |
            | pingIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: pc values for step & continue\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         pt_where_it_left_off = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing memory\x7fCategory: bytes\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryAt: addr Size: n = ( |
            | 
            readMemoryAt: addr Size: n IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: snapshotting\x7fCategory: loading snapshots\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryFrom: fileName = ( |
            | 
            readMemoryFrom: fileName IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing memory\x7fCategory: words\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryWordAt: i = ( |
            | 
            readMemoryWordAt: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing memory\x7fCategory: words\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryWordAt: i IfFail: fb = ( |
            | 
            safelyDo: [
              | int32Result |
              int32Result:
               myAssemblerSystem intNNFromBytes: (
                readMemoryAt: i 
                Size: oopSize  
                IfFail: [|:e| ^ fb value: e]
              ) At: 0.
              int32Result + 0 "let it be smallInt if it can"
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing memory\x7fCategory: words\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryWordsAt: addr Size: n Do: blk IfFail: fb = ( |
            | 
            safelyDo: [| bytes |
              bytes: readMemoryAt: addr
                             Size: n * oopSize
                           IfFail: [|:e| ^ fb value: e].
              n do: [|:i. int32Result |
                int32Result: myAssemblerSystem intNNFromBytes: bytes At: i * oopSize.
                blk value: int32Result + 0  "let it be smallInt if it can"
                     With: addr + (i * oopSize)
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting return handler address\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         returnHandler = ( |
            | returnHandlerIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting thread\'s run state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         runState = ( |
            | runStateIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting thread\'s run state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         runStateString = ( |
            | runStateStringIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         setContentsOfRegister: regName To: aNumber = ( |
            | 
            setContentsOfRegister: regName To: aNumber IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         setContentsOfRegister: regName To: aNumber IfFail: fb = ( |
             s.
            | 
            s: getIntegerStateIfFail: [|:e| ^ fb value: e].
            s: s copyAt: regName Put: aNumber.
            setIntegerStateFrom: s IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         setIntegerStateFrom: anIntegerState = ( |
            | 
            setIntegerStateFrom: anIntegerState IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         setPC: pc = ( |
            | setPC: pc IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         setPC: pc IfFail: fb = ( |
            | 
            setContentsOfRegister: 'pc' To: pc IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         setReturnAddress: lr = ( |
            | 
            setReturnAddress: lr IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         setReturnAddress: lr IfFail: fb = ( |
            | 
            case
              if: [architecture = 'ppc' ] Then: [ setContentsOfRegister: 'lr' To: lr IfFail: fb ]
              If: [architecture = 'i386'] Then: [ writeWord: lr 
                                                 ToMemoryAt: (contentsOfRegister: 'ebp' IfFail: [|:e| ^ fb value: e]) + 4
                                                     IfFail: fb ]
              Else: [error: 'unknown architecture: ', architecture]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: signalling\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         signal: sigNo = ( |
            | signal: sigNo IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: signals\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         signalNameAndDescriptionFor: n = ( |
             bytes.
             r.
            | 
            r: (reflect: signals) findFirst: [|:s| s contents reflectee number = n]
                                  IfPresent: [|:s| s name capitalizeAll, ': ',
                                                   s contents reflectee description ]
                                   IfAbsent: ['signal: ', n printString].
            n = signals sigtrap number ifFalse: [^ r].
            bytes: readMemoryAt: pc + 4 Size: 1000 IfFail: [|:e| ^ r,  '(could not read: ', e, ')'].

            (assemblerSystems ppc disassembler copy locationCounter: pc + 4)
              disassembledInstructionsIn: bytes At: 0 Do: [|:di|
                di isComment ifTrue: [r: r, ' ', di externalSource].
                ^ r
              ].
            error: 'should never reach here').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: single stepping\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         singleStep = ( |
            | singleStepIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: single stepping\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         singleStepAt: newPC = ( |
            | singleStepAt: newPC IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: single stepping\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         singleStepIfFail: fb = ( |
            | 
            singleStepAt: where_it_left_off_bypassing_trap IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: geting status\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         statusString = ( |
            | statusStringIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: geting status\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         statusStringIfFail: fb = ( |
             s.
            | 
            s: waitStatusIfFail: [|:e| ^ fb value: e].
            stringForWaitStatus: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: suspending\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         suspend = ( |
            | suspendIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         testDownload = ( |
            | 
            copy downloadAndStartProgram: '123' IfFail: [|:e| error: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         testPing = ( |
            | 
            copy pingIfSucceed: [|:a| a] IfFail: [|:e| error: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         waitStatus = ( |
            | waitStatusIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         waitStatusOfPID: p = ( |
            | waitStatusOfPID: p IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         waitStatusString = ( |
            | 
            waitStatusStringIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         where_it_left_off_bypassing_trap = ( |
             trapInstruction = int32 fromHigh16Bits: 16r7fe0 Low16Bits: 16r8.
            | 
            (readMemoryWordAt: pcIfFail: [^ 0]) = trapInstruction ifTrue: [
              "move pc after trap"
              setPC: pc + trapInstruction size.  
            ].
            pt_where_it_left_off).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing memory\x7fCategory: bytes\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         write: aByteArray ToMemoryAt: addr = ( |
            | write: aByteArray ToMemoryAt: addr IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: snapshotting\x7fCategory: saving snapshot\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         writeMemoryTo: fileName AddressRanges: addrVector = ( |
            | 
            writeMemoryTo: fileName AddressRanges: addrVector IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing memory\x7fCategory: words\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         writeWord: w ToMemoryAt: i = ( |
            | 
            writeWord: w ToMemoryAt: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing memory\x7fCategory: words\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         writeWord: w ToMemoryAt: i IfFail: fb = ( |
            | 
            safelyDo: [
              write: (myAssemblerSystem byteVectorFromInt: w)
              ToMemoryAt: i
              IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: signalling\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         signal: sigNo IfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                writeRequestType: requestTypes signal
                         IfFail: exit.
                writePIDIfFail:   exit.
                socket writeInt: sigNo IfFail: exit.
                socket flushOutputIfFail: exit.
                readStatusIfFail: exit.
                mustCheckWaitStatusInFuture.
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: signals\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         signals = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'signals' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent signals.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: single stepping\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         singleStepAt: newPC IfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                ["was:" ptraceRequest: pt_step Address: 1 Data: 0 IfFail: fb].
                writeMachRequestType: machRequestTypes single_step
                                   IfFail: exit.
                writePIDIfFail:            exit.
                socket writeInt: newPC IfFail: exit. "continuation address"
                socket flushOutputIfFail:  exit.
                readStatusIfFail:          exit.
                mustCheckWaitStatusInFuture.
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: caching\x7fCategory: invalidating\x7fCategory: my & other caches\x7fComment: Grrrr! Should be private, and non-existant in this object.
But see waitForSlop -- 12/05, dmu\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         slopPeriod = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: starting the server\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         startServerOn: hostName Port: p = ( |
            | 
            hostName = 'localhost' 
             ifFalse: [error: 'should rsh to host, then start the process'].
            (serverString, ' -p ',  p asString) printLine.
            os command: serverString, ' -p ', p asString, ' &').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            'on ', hostName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting thread\'s run state\x7fCategory: run state conversions\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         stringForRunState: i = ( |
            | 
            runStateStrings at: i IfAbsent: [
              'state: ', i printString]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         stringForWaitStatus: s = ( |
            | 
            ifWaitStatus: s
              IsRunning: 'running'
              IsExited: [|:status|
               'exited', (status = 0 ifTrue: '' False: [': ', status printString])
              ]
              IsTerminatedBySignal: [|:sig. :dumped|
                'terminated by signal: ', (signalNameAndDescriptionFor: sig),
                (dumped ifFalse: '' True: ' coredumped')
              ]
              IsStopped: [|:sig| 'stopped by signal: ', (signalNameAndDescriptionFor: sig)]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: controlling execution\x7fCategory: suspending\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         suspendIfFail: fb = ( |
            | 
            signal: signals sigtrap number IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting task ID\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         taskForPID: pid IfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                | r |
                writeMachRequestType: machRequestTypes task_for_pid
                             IfFail: exit.
                socket writeInt: pid IfFail: exit.
                socket flushOutputIfFail:    exit.
                r: socket readIntIfFail:     exit.
                readStatusIfFail:     exit.
                r
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: initiating & terminating\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         terminate = ( |
            | 
            safelyDoRequest: [
                writeRequestType: requestTypes terminate IfFail: [].
                socket flushOutputIfFail: [].
                socket close.
                self
              ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fCategory: not done yet\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         vectorState = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'vectorState' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent vectorState.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'vectorState' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'vectorState' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent vectorState parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'vectorState' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (vector copySize: 32 FillingWith: vector copySize: 4)\x7fVisibility: public'
        
         save_vr <- vector copySize: 32 FillingWith: vector copySize: 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'vectorState' -> () From: ( | {
         'Comment: vrs that have been saved\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         save_vrvalid <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'vectorState' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (vector copySize: 4)\x7fVisibility: public'
        
         save_vscr <- vector copySize: 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            kleinAndYoda).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         wRunning = -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         wStopped = 127.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         waitStatusIfFail: fb = ( |
            | 
            safelyDo: [
                      ifWaitStatus:  lastKnownWaitStatus
                         IsRunning: [lastKnownWaitStatus: waitStatusOfPID: debuggeePID IfFail: [|:e| ^ fb value: e]]
                          IsExited: []
              IsTerminatedBySignal: []
                         IsStopped: [].

              lastKnownWaitStatus
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fComment: Get Unix wait status, return 
<waitpid result pid> @ status
-- dmu 1/02\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         waitStatusOfPID: p IfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                | resultPID <- 0 |
                writeRequestType: requestTypes waitStatus
                                    IfFail: exit.
                socket writeInt: p  IfFail: exit.
                socket flushOutputIfFail:   exit.
                resultPID: socket readIntIfFail: exit.
                case
                  if:   [resultPID = -1]
                  Then: [
                    readStatusIfFail: exit.
                    exit value: 'waitpid returned -1, with errno of zero'
                  ]
                  If:   [resultPID != 0]
                  Then: [ socket readIntIfFail: exit]
                  Else: wRunning "nothing to collect / it's still running"
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting Unix wait status info\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         waitStatusStringIfFail: fb = ( |
            | 
            stringForWaitStatus: waitStatusIfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing memory\x7fCategory: bytes\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         write: aByteArray ToMemoryAt: addr IfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [ | r |
                       writeMachRequestType: machRequestTypes set_memory
                                              IfFail: exit.
                       writePIDIfFail:                exit.
                socket writeBytes: aByteArray IfFail: exit.
                socket writeInt: addr         IfFail: exit.
                socket flushOutputIfFail:             exit.
                readStatusIfFail: exit.
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: snapshotting\x7fCategory: saving snapshot\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         writeMemoryTo: fileName AddressRanges: addrVector IfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                          writeMachRequestType:  machRequestTypes write_memory_to_file
                                                  IfFail: exit.
                          writePIDIfFail:                 exit.
                   socket writeBytes:  addrVector IfFail: exit.
                   socket writeString: fileName   IfFail: exit.      
                   socket flushOutputIfFail:              exit.
                          readStatusIfFail:               exit.
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fCategory: writing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         writePIDIfFail: fb = ( |
            | 
            socket writeInt: debuggeePID IfFail: fb.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fCategory: writing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         writeRequestType: rt IfFail: fb = ( |
            | 
            socket ensureOutputBufferIsEmpty: [|:e| ^ fb value: e].
            socket writeByte: rt
                      IfFail: [|:e| fb value: 'could not write request type: ', e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fCategory: writing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         writeYodaRequestType: ssrt IfFail: fb = ( |
            | 
            writeRequestType: requestTypes yoda IfFail: [|:e| ^ fb value: e].
            socket writeByte: ssrt IfFail: [|:e| fb value: 'could not write yoda request type: ', e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fCategory: protocol constants\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         yodaRequestTypes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'yodaRequestTypes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent yodaRequestTypes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> () From: ( | {
         'Category: debuggingProxyClient state\x7fComment: initialized to the default server port\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (9090)'
        
         port <- 9090.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> () From: ( | {
         'Category: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (semaphore copyCount: 1 Capacity: 1)\x7fVisibility: private'
        
         requestSema <- semaphore copyCount: 1 Capacity: 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> () From: ( | {
         'Category: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (recursiveSemaphore copyBinary)'
        
         sema <- recursiveSemaphore copyBinary.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> () From: ( | {
         'Category: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (-1)'
        
         serverPID <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> () From: ( | {
         'Category: debuggingProxyClient state\x7fModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (nil)'
        
         socket.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         macOSX = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'macOSX' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda debuggingProxyClients noncaching abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'macOSX' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching macOSX.

CopyDowns:
globals kleinAndYoda debuggingProxyClients noncaching abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'macOSX' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'macOSX' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching macOSX parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'macOSX' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         osx_shortcuts* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'macOSX' -> 'parent' -> 'osx_shortcuts' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching macOSX parent osx_shortcuts.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'macOSX' -> 'parent' -> 'osx_shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting task ID\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         debuggeeTask = ( |
            | 
            debuggeeTaskIfSucceed: [|:t|t] IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'macOSX' -> 'parent' -> 'osx_shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting task ID\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         debuggeeTaskIfSucceed: sb IfFail: fb = ( |
            | 
            sb value:
              taskForPID: debuggeePID
                  IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'macOSX' -> 'parent' -> 'osx_shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting task ID\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         taskForPID: pid = ( |
            | taskForPID: pid IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'macOSX' -> 'parent' -> 'osx_shortcuts' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting threads\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         threadsForTask: task = ( |
            | 
            threadsForTask: task IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'macOSX' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'macOSX' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting threads\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         threadsForTask: task IfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                | r |
                        writeMachRequestType: machRequestTypes threads_for_task
                                      IfFail: exit.
                socket writeInt: task IfFail: exit.
                socket flushOutputIfFail:     exit.
                r: socket readIntArrayIfFail: exit.
                          readStatusIfFail:   exit.
                r
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'macOSX' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fCategory: writing\x7fModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         writeMachRequestType: mrt IfFail: fb = ( |
            | 
            writeRequestType: requestTypes mach IfFail: [|:e| ^ fb value: e].
            socket writeByte: mrt IfFail: [|:e| fb value: 'could not write mach request type: ', e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         mercuryLinux = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'mercuryLinux' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda debuggingProxyClients noncaching abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'mercuryLinux' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching mercuryLinux.

CopyDowns:
globals kleinAndYoda debuggingProxyClients noncaching abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'mercuryLinux' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'mercuryLinux' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching mercuryLinux parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'mercuryLinux' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         vmKitProxyClients = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxyClients' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxyClients' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitProxyClients.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxyClients' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxyClients' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxyClients' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxyClients' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxyClients' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.10 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitProxyClients' -> () From: ( | {
         'ModuleInfo: Module: vmKitProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'integerStates
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'integerStates' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitProxyClients postFileIn
