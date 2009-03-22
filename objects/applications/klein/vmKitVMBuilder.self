 '$Revision: 30.5 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'generation' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         printSizeStatistics = ( |
            | 
            spacesDo: [|:s| s printSizeStatistics].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'segregatedSpaceMixin' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         printSizeStatistics = ( |
            | 
            printSizeStatisticsForOops: oopCount AndBytes: sizeOfAllocatedBytesRegion).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'spaceMixin' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         printSizeStatisticsForOops: nOops AndBytes: nBytes = ( |
             s.
            | 
            s: name capitalize, ' Space Size Statistics\n',
            ' - Total: ', (((nOops * 4) + nBytes) / 1024) printString, ' KBytes if 32-bit oops,\n',
            '          ', (((nOops * 2) + nBytes) / 1024) printString, ' KBytes if 16-bit oops\n',
            '   - Oops: ', (nOops / 1024) printString, ' KOops\n',
            '   - Bytes: ', (nBytes / 1024) printString, ' KBytes\n'.
            s print.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         printSizeStatistics = ( |
            | 
            generationsDo: [|:g| g printSizeStatistics].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'unsegregatedSpaceMixin' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         printSizeStatistics = ( |
             nBytes <- 0.
             nOops <- 0.
            | 
            oopsDo: [nOops: nOops succ] AndBytesPartsDo: [|:size| nBytes: nBytes + (size roundUpTo: oopSize)].

            printSizeStatisticsForOops: nOops AndBytes: nBytes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: statistics\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         printUniverseSizeStatistics = ( |
            | 
            universe printSizeStatistics.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: building VMs\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: public'
        
         vmBuilder1 = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda abstractProgramBuilder copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda vmBuilder1.

CopyDowns:
globals kleinAndYoda abstractProgramBuilder. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMBuilder InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         objectsOracle.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: public'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda vmBuilder1 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> 'parent' -> () From: ( | {
         'Category: ui hooks\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: public'
        
         buildActionName = ( |
            | 'Compile nmethods and map objects').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> 'parent' -> () From: ( | {
         'Category: building\x7fComment: Builds the necessary data structures in preparation
for launching the vmToExport in a foreignProcess.
Maps objects and compiles essential nmethods.\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         buildIfFail: fb = ( |
             bal.
             linearizedObjects.
             objectsOracle.
            | 

            "Optimization: decreasing the _MemoryTenuringThreshold speeds up the Klein build
             process significantly (10-20%). Set it back afterwards, though, because I don't
             know what effects it has on regular everyday Self stuff. -- Adam, 12/04"
            decreaseMemoryTenuringThresholdAndDo: [
              vmToBuild setTheVMAndDo: [
                | aVMImage. p |
                p: proxyForBuilding copyAndInitiate.
                bal: p getBaseAndLengthIfFail: [|:e| ^ fb value: 'Could not get base and length: ', e].
                p terminate.

                setupMemoryForBase: bal x Length: bal y.

                aVMImage:
                  isProfilingEnabled
                    ifTrue: [[mapObjects: bal] profileSlice]
                     False: [ mapObjects: bal              ].

                statusReporter show: 'ready'.
                aVMImage allowDefinesToPropagateToMe.
                aVMImage
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForBuilding: aVM = ( |
            | 
            copy vmToBuild: aVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         decreaseMemoryTenuringThresholdAndDo: blk = ( |
             factor = 10.
            | 
            _MemoryTenuringThreshold: _MemoryTenuringThreshold / factor.
            blk onReturn: [  _MemoryTenuringThreshold: _MemoryTenuringThreshold * factor ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         initVMImage: aVM FromMapper: mapper = ( |
             vmImage.
            | 
            vmImage: aVM vmKit vmImage copyForBuildingVM: theVM StatusReporter: statusReporter.
            vmImage copyInfoFrom: mapper.

            "should not do anything more with mapper from now on"
            vmImage myVM setTrailingMarkIn: mapper targetSpace.
            vmImage myVM exportHeapInformationTo: vmImage mirrorOnTheVMWhichHasNotMoved.

            vmImage).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         isProfilingEnabled = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         mapObjects: baseAndLength = ( |
             image.
             mapper.
            | 
            statusReporter show: 'compiling nmethods and mapping objects...'
                          While: [
                                    mapper: vmToBuild vmKit objectMapper1 
                                              copyPolicy: vmToBuild exportPolicy
                                                ReportTo: statusReporter.
                                    mapper expectedNumberOfObjects: vmToBuild expectedNumberOfObjects.
                                    objectsOracle: mapper mapObjectsKeepingStatistics.
                                 ].
            statusReporter show: 'initializing VM image' While: [
              printImageInitializationStatistics: [image: initVMImage: theVM FromMapper: mapper] allTimes.
            ].
            image).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractProgramBuilder' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         printImageInitializationStatistics: initializationTimes = ( |
             s.
            | 
            s: (
              '\n',
              'Image Initialization Statistics\n',
              ' - Real   time   : ', (initializationTimes   realTime / 1000) printString, ' s\n',
              ' - CPU    time   : ', (initializationTimes    cpuTime / 1000) printString, ' s\n',
              ' - User   time   : ', (initializationTimes   userTime / 1000) printString, ' s\n',
              ' - System time   : ', (initializationTimes systemTime / 1000) printString, ' s\n'
            ).
            s printLine.
            vmToBuild printUniverseSizeStatistics.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> 'parent' -> () From: ( | {
         'Category: ui hooks\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: public'
        
         rebuildActionName = ( |
            | 'Recompile nmethods and remap objects').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         setupMemoryForBase: base Length: nBytes = ( |
            | 
            statusReporter show: 'setting up memory...' While: [
              theVM lens: theVM vmKit memoryLens.
              theVM universe allocateHeapAt: base Size: nBytes.
              theVM machineMemory: theVM universe createBufferMemoryInterface.
              theVM universe getReadyToStartAllocating.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmBuilder1' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMBuilder InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         vmToBuild.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot'
        
         vmKitVMBuilder = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMBuilder' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMBuilder' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitVMBuilder.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMBuilder' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMBuilder' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMBuilder InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMBuilder' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMBuilder' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMBuilder' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.5 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMBuilder' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMBuilder InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitObjMapper
vmKitVMImage
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitObjMapper' From: 'applications/klein'
 bootstrap read: 'vmKitVMImage' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitVMBuilder postFileIn
