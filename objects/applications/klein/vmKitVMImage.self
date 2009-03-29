 '$Revision: 30.12 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: relocation\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateMemOop: oop At: addr By: delta = ( |
             r.
            | 
            theVM objectLocator ifDirect: [] IfIndirect: [
              [todo cleanup relocation oopFormat].
              halt. "will not work for object table"
            ].

            r: (oop + delta) asInt32.
            theVM machineMemory at: addr PutWord: r.
            theVM assert: [theVM universe oopsIncludesAddress: decode: r].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: building VMs\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         vmImage = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda vmImage.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (recursiveSemaphore copy)\x7fVisibility: private'
        
         definePropagationSema <- recursiveSemaphore copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> () From: ( | {
         'Category: mapping\x7fModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         heapOffset <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> () From: ( | {
         'Comment: This is a HACK. -- Adam, 10/05\x7fModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         lastKnownOopForMyVM <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myVM.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> () From: ( | {
         'Category: mapping\x7fModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         objectsOracle.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda vmImage parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         addressForTheObjectTable = ( |
            | 
            myVM setTheVMAndDo: [
              addressForOriginalObject: myVM objectLocator
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: incrementally updating\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         allowDefinesToPropagateToMe = ( |
            | 
            definePropagationSema signal.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         blockCloneCounts = ( |
             r.
            | 
            r: dictionary copyRemoveAll.
            myVM setTheVMAndDo: [
              objectsOracle ensureAddressesByOIDIsUpdated.
              objectsOracle oopsAndAddressesAndOIDsDo: [|:oop. :addr. :oid. mapOop. mt|
                mapOop: vmKit layouts memoryObject mapOf: oop.
                mt: vmKit maps map mapTypeOfRemoteMap: mapOop IfFail: raiseError.
                mt = vmKit maps blockMap mapType ifTrue: [| blockMapMir. origBlockMir. n |
                  blockMapMir:  myVM mirrorFor:  mapOop.
                  origBlockMir: objectsOracle originalMirrorForOID: oid.
                  [cloneCount]. "browsing"
                  n: (blockMapMir at: 'cloneCount') contents importReflecteeAsInteger.
                  r at: origBlockMir Put: n.
                ].
              ].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         breakDownNMethodsByHolder = ( |
             holders.
            | 
            holders: list copyRemoveAll.
            nmethodsDo: [|:nm. holder|
              holder: reflect: nm methodHolder.
              holder isReflecteeBlock ifTrue: [holder: 'block!!!' asMirror].
              holders add: holder.
            ].
            sortedOccurrenceCountsFor: holders).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         breakDownNMethodsBySlotType = ( |
             accs.
             meths.
             muts.
             rest.
            | 
            rest: list copyRemoveAll.
            meths: list copyRemoveAll.
            accs: list copyRemoveAll.
            muts: list copyRemoveAll.
            nmethodsDo: [|:nm. s|
              s:  (reflect: nm methodHolder) at: nm lookupKey selector.
              case
                if: [s isAssignable] Then: [accs add: nm]
                If: [s isAssignment] Then: [muts add: nm]
                If: [s isMethod    ] Then: [meths add: nm]
                                     Else: [rest add: nm].
            ].
            (accs & muts & meths & rest) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         breakDownObjectsBy: blk = ( |
             r.
            | 
            r: dictionary copyRemoveAll.
            myVM setTheVMAndDo: [
              objectsOracle ensureAddressesByOIDIsUpdated.
              objectsOracle oopsAndAddressesAndOriginalMirrorsAndKleinifiedMirrorsAndOIDsDo: [|:oop. :addr. :mir. :kMir. :oid. size. t. ts. os|
                t: blk value: oop With: mir With: kMir With: oid.
                size: objectSize copyWords: mir vmKitMapForConversion myLayout wordSizeOf: oop.
                ts: r at: t IfAbsentPut: [typeStat copyForType: t Image: self].
                ts objectStats add: objectStat copyForOop: oop OriginalMirror: mir KleinifiedMirror: kMir Size: size.
              ].
            ].
            [typeStat size]. "browsing"
            (r asVector copySortBySelector: 'size') reverse).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         breakDownObjectsByMap = ( |
            | 
            breakDownObjectsBy: [|:oop| vmKit layouts object mapOf: oop]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         breakDownObjectsBySelfPrototype = ( |
            | 
            breakDownObjectsBy: [|:oop. :mir. :kMir| selfPrototypeOf: kMir]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: tracking nmethods\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         cgProto = ( |
            | 
            myVM architecture sendTo: myVM compilerPrototype prototypes codeGenerators).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
             r.
            | 
            r: resend.copy.
            r objectsOracle:     objectsOracle     copy.
            r definePropagationSema: definePropagationSema copy count: 0.
            willNeedToRelocate ifTrue: [
              r copyNMethods
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         copyAndLaunchFrom: serverProxy Process: aForeignProcess IfFail: fb = ( |
             r.
            | 
            printLaunchTimes: [
              r: copyForLaunchingIfFail: [|:e| ^ fb value: e].
              r       launchFrom: serverProxy 
                InForeignProcess: aForeignProcess
                          IfFail: [|:e| ^ fb value: e].
            ] allTimes.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForBuildingVM: aVM StatusReporter: sr = ( |
             r.
            | 
            r: copyRemoveAll myVM: aVM.
            r statusReporter: sr.
            aVM image: r.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         copyForLaunchingIfFail: fb = ( |
             r.
             vm.
            | 

            definePropagationSema protect: [
              statusReporter show: 'copying image'
                             While: [r: copy].
              vm: myVM copyForLaunchingImage: r.
              r myVM: vm.
              r allowDefinesToPropagateToMe.
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: updating info from mapper\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         copyInfoFrom: mapper = ( |
            | 
            objectsOracle ifNil: [objectsOracle:               mapper objectsOracle copy]
                       IfNotNil: [objectsOracle copyInfoFrom:  mapper objectsOracle     ].

            statusReporter show: 'Inserting oops in linearized nmethods'
                          While: [insertOopsInMachineCodeWithMapper: mapper].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: tracking nmethods\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         copyNMethods = ( |
            | 
            "must copy nmethods so relocation can mutate their relocators.
             By copying, we allow another relocation to have a fresh start.
              -- dmu 4/04"
            statusReporter show: 'copying nmethods'
             While: [
              nmethodsDo: [|:nm|
                replaceObject: nm With: nm copy.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         copyRemoveAll = ( |
             r.
            | 
            r: resend.copy.
            r heapOffset: 0.
            r definePropagationSema: definePropagationSema copy count: 0.

            objectsOracle ifNotNil: [
              r objectsOracle:  objectsOracle copyRemoveAll.
            ].

            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fCategory: entry method\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         entryMethodName = ( |
            | 
            myVM entryMethodName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fCategory: entry method\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         entryNMethodMirror = ( |
            | 
            entryNMethodMirrorPrototype
              copyForVM: myVM
                    OID: entryNMethodOID
                 IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fCategory: entry method\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         entryNMethodMirrorPrototype = ( |
            | 
            vmKit mirrors nmethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fCategory: entry method\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         entryNMethodOID = ( |
            | 
            objectsOracle oidOfNMethodNamed: entryMethodName
                          ForOriginalObject: myVM
                                  IfPresent: [|:nmOID| nmOID]
                                   IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: incrementally updating\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         fixupImageAfterDefineOf: newMir = ( |
             oldOop.
            | 
            _NakedMethods: true.
            oldOop: myVM setTheVMAndDo: [oopForOriginalMirror: newMir IfAbsent: [^ self]].
            isProfilingEnabled
              ifTrue: [[fixupImageAfterDefineOf: newMir OldOop: oldOop] profileSlice]
               False: [ fixupImageAfterDefineOf: newMir OldOop: oldOop              ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: incrementally updating\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         fixupImageAfterDefineOf: newMir OldOop: oldOop = ( |
             mapper.
             newObj.
             objectsOracle.
            | 
            newObj: newMir reflectee.
            myVM setTheVMAndDo: [ | vmImage |

              myVM importHeapInformationFrom: mirrorOnTheVM.

              mapper: vmKit incrementalObjectMapper1
                                       copyPolicy: myVM exportPolicy
                                    ObjectLocator: myVM objectLocator
                                         ReportTo: statusReporter
                             ObjectThatWasDefined: newObj.

              statusReporter show: 'mapping changed objects' While: [
                objectsOracle: mapper mapObjectsKeepingStatistics.
              ].

              vmImage: self.
              printImageUpdateStatistics: [fixupImageAfterDefineOf: newMir UsingMapper: mapper] allTimes.

              myVM initializeMirrorCache.
              "do not have to worry about references to the self object, because it has already been _Define: ed"
              "Except in the klein heap:"
              [ == vmImage ] assert.
              myVM universe incrementProgrammingTimestamp.
              myVM machineMemory invalidateCaches.
              statusReporter show: 'ready'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: incrementally updating\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         fixupImageAfterDefineOf: newMir UsingMapper: mapper = ( |
             newAddr.
             newOop.
             newSize.
             oid.
             oldAddr.
             oldOID.
             oldOop.
             shouldCopyBitsIntoOldObject.
            | 

               oid: mapper objectsOracle oidForOriginalMirror: newMir.
            oldOID:                      oidForOriginalMirror: newMir.
            [oid = oldOID] assert.

            newAddr: myVM            objectLocator addressForOID: oid.
            oldAddr: mapper copyOfOldObjectLocator addressForOID: oid. "How do I get this?"

            newOop: myVM            objectLocator oopForOID: oid Address: newAddr.
            oldOop: mapper copyOfOldObjectLocator oopForOID: oid Address: oldAddr.

            newSize:  newMir vmKitMapForConversion myLayout wordSizeOf: newOop.

            statusReporter show: 'updating image' While: [
              copyInfoFrom: mapper.
              "should not do anything more with mapper from now on"
              myVM setTrailingMarkIn: mapper targetSpace.
            ].

            mapper isSameSize ifFalse: [
              statusReporter show: 'switching pointers in heap'
                            While: [myVM switchPointersFromObjectWithOop: oldOop ToHaveAddress: newAddr].
            ] True: [
              statusReporter show: 'copying bits of new object into old object'
                            While: [myVM universe copy: myVM oopSize * newSize BytesFromAddress: newAddr ToAddress: oldAddr].
            ].

            statusReporter show: 'exporting heap information'
                          While: [myVM exportHeapInformationTo: mirrorOnTheVMWhichHasNotMoved].

            statusReporter show: 'exporting changed object table entries'
                          While: [mapper exportChangedObjectTableEntriesFrom: myVM objectLocator 
                                                                          To: mirrorOnTheVMWhichHasNotMoved
                                                                                primitiveContentsAt: 'objectLocator'].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: tracking nmethods\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         inNMethodsDefine: oldMir ToBe: newMir = ( |
             r.
            | 
            statusReporter show: 'Replacing references in nmethods'
              While: [
                r: list copyRemoveAll.
                nmethodsDo: [|:nm|
                  nm if: oldMir IsFoundCopyAndReplaceWith: newMir IfFound: [|:newnm| r add: nm @ newnm].
                ].
                r do: [|:p| inOraclesReplaceNMethod: p x With: p y].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: tracking nmethods\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         inOraclesReplaceNMethod: oldNM With: newNM = ( |
            | 
            replaceObject: oldNM With: newNM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: linearizing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         insertOopsInMachineCodeWithMapper: mapper = ( |
             cg.
             compiler.
            | 
            nmethodsDo: [|:nm. rels. relsToReassemble|
              cg ifNil: [cg: cgProto].
              rels: nm reconstructRelocators.
              relsToReassemble: rels asList copyFilteredBy: [|:r| r hasCompiledOopBeenSet not || [mapper shouldAlwaysReassembleRelocatorsForObject: r originalObject]].

              relsToReassemble isEmpty ifFalse: [|nmOop|
                nmOop:  oopForOriginalObject: nm.
                relsToReassemble do: [|:r|
                  r insertOopInNMethod: nmOop Using: objectsOracle With: cg copyForCompiler: myVM compilerPrototype copy.
                ].
                nm relocators: rels.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         invocationCounts = ( |
            | 
            myVM setTheVMAndDo: [| r |
              r: list copyRemoveAll.
              nmethodsDo: [|:nm. nmOop. map|
                nmOop: oopForOriginalObject: nm.
                map:  vmKit maps nmethodMap importMapFor: nmOop IfFail: raiseError.
                r add: nm @ (map contentsOfSlotNamed: 'invocationCount' In: nmOop IfFail: raiseError).
              ].
              r
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: incrementally updating\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         isProfilingEnabled = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fComment: Launches the vmToExport in a foreignProcess.
Exports then relocates the already mapped objects.
Then, instructs the foreignProcess to jump to the
entry point of the start method with parameters. -- jb 5/03\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         launchFrom: serverProxy InForeignProcess: aForeignProcess IfFail: fb = ( |
            | 
            definePropagationSema protect: [
              aForeignProcess launch: self From: serverProxy IfFail: [|:e| ^ fb value: e].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         launchSize = ( |
            | myVM machineMemory size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorOnTheObjectLocatorIfFail: fb = ( |
            | 
            [objectLocator]. "browsing"
            (mirrorOnTheVMIfFail:                               [|:e| ^ fb value: e])
               primitiveContentsAt: 'objectLocator'     IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorOnTheVM = ( |
            | 
            mirrorOnTheVMIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorOnTheVMIfFail: fb = ( |
            | 
            myVM noncachingMirrorFor: (oopForTheVMWhichMayHaveMovedIfFail: [|:e| ^ fb value: e])
                              IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorOnTheVMWhichHasNotMoved = ( |
            | 
            myVM noncachingMirrorFor: oopForTheVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         moduleSizes = ( |
             d.
             r.
            | 
            d: dictionary copyRemoveAll.
            wellKnownSlotsDo: [|:s| (d at: s module IfAbsentPut: [list copyRemoveAll]) add: s].
            d: d copyMappedBy: [|:ss| (ss copyMappedBy: [|:s| myVM exportPolicy estimatedSizeOfSlot: s AssumingOopSizeIs: 2]) sum].
            r: list copyRemoveAll.
            d do: [|:size. :mName|
              r add: size @ mName.
            ].
            (r asVector copySortBySelector: 'x') reverse).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: tracking nmethods\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodsDo: blk = ( |
            | 
            objectsOracle nmethodsDo: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fCategory: prototypes\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         objectSize = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda vmImage parent objectSize.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( |
             {} = 'Comment: This object mostly exists because I was afraid of getting confused
about when I was talking about bytes and when I was talking about
words. I wanted something like a number, but whose printString
would say \'words\' so that I\'d never forget that I was talking about
words, not bytes.
It\'d be neat if someday this turned into a more general-purpose
\"units\" object. -- Adam, 11/05\x7fModuleInfo: Creator: globals kleinAndYoda vmImage parent objectSize parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'Category: arithmetic\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         + other = ( |
            | 
            arithmeticWith: other Do: [|:a. :b| a + b]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'Category: arithmetic\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         - other = ( |
            | 
            arithmeticWith: other Do: [|:a. :b| a - b]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'Category: arithmetic\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         / n = ( |
            | 
            copyWords: wordCount / n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'Category: arithmetic\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         /~ n = ( |
            | 
            copyWords: wordCount /~ n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         < other = ( |
            | 
            wordCount < other wordCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         = other = ( |
            | 
            wordCount = other wordCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'Category: arithmetic\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         arithmeticWith: other Do: aBlock = ( |
            | 
            copyWords: aBlock value: wordCount With: other wordCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         byteCount = ( |
            | 
            wordCount * bytesPerWord).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesPerWord = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         copyWords: n = ( |
            | 
            copy wordCount: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            wordCount hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         ordered* = bootstrap stub -> 'mixins' -> 'ordered' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            wordCount printString, ' words').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectSize' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         wordCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fCategory: prototypes\x7fComment: This is a terrible name for this object. -- Adam, 10/05\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         objectStat = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectStat' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda vmImage parent objectStat.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectStat' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         kleinifiedMirror.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectStat' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         oop.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectStat' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         originalMirror.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectStat' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectStat' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda vmImage parent objectStat parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectStat' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForOop: o OriginalMirror: m KleinifiedMirror: km Size: s = ( |
            | 
            (((copy oop: o) originalMirror: m) kleinifiedMirror: km) size: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectStat' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedObject = ( |
            | 
            kleinifiedMirror reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectStat' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         originalObject = ( |
            | 
            originalMirror reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectStat' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'objectStat' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         size.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         oopForTheVM = ( |
             oop.
            | 
            oop: objectsOracle addressesByOID oopForOID: (oidForOriginalObject: myVM) IfAbsent: [error: 'huh?'].
            [todo cleanup oraclesAndOIDs]. "lastKnownOopForMyVM is a HACK. -- Adam"
            lastKnownOopForMyVM: oop.
            oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         oopForTheVMWhichMayHaveMovedIfFail: fb = ( |
            | 
            myVM machineMemory isForForeignProcess ifFalse: [^ lastKnownOopForMyVM].

            myVM machineMemory foreignProcess startActivation receiverOopIfFail: [
              [todo cleanup oraclesAndOIDs]. "Should this fail or do this last-known thing? If we just fail,
                                              I think we'll be in trouble when the debugger pops up for the first
                                              time but we haven't hit Continue yet. -- Adam"
              "VM process may not have started running yet."
              [todo cleanup oraclesAndOIDs]. "lastKnownOopForMyVM is a HACK. -- Adam"
              lastKnownOopForMyVM
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         prepareForJumpToStartMethod = ( |
            | 
            "Tell the foreign process to jump into the running VM."
            myVM setTheVMAndDo: [
              myVM machineMemory foreignProcess
                     setReceiverOop: oopForTheVM
                   AndParameterOops: vector.
              entryNMethodMirror entryAddress
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: incrementally updating\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         printImageUpdateStatistics: updateTimes = ( |
             s.
            | 
            s: (
              '\n',
              'Image Update Statistics\n',
              ' - Real   time   : ', (updateTimes   realTime / 1000) printString, ' s\n',
              ' - CPU    time   : ', (updateTimes    cpuTime / 1000) printString, ' s\n',
              ' - User   time   : ', (updateTimes   userTime / 1000) printString, ' s\n',
              ' - System time   : ', (updateTimes systemTime / 1000) printString, ' s\n'
            ).
            s printLine.
            myVM printUniverseSizeStatistics.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         printLaunchTimes: launchingTimes = ( |
             s.
            | 
            s: (
              '\n',
              'Launching Statistics\n',
              ' - Real   time   : ', (launchingTimes   realTime / 1000) printString, ' s\n',
              ' - CPU    time   : ', (launchingTimes    cpuTime / 1000) printString, ' s\n',
              ' - User   time   : ', (launchingTimes   userTime / 1000) printString, ' s\n',
              ' - System time   : ', (launchingTimes systemTime / 1000) printString, ' s\n'
            ).
            s printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         printModuleSizes = ( |
             r.
             totalSize <- 0.
            | 
            r: list copyRemoveAll.
            moduleSizes do: [|:size. :mName|
              totalSize: totalSize + size.
              r add: size @ mName.
            ].
            (totalSize printString, ' in total') printLine.
            (r copySortBySelector: 'x') reverseDo: [|:p| ((p x printString padOnLeft: totalSize printString size), ': ', p y) printLine].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: incrementally updating\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         propagateDefineOf: newMir = ( |
            | 
            definePropagationSema protect: [
              fixupImageAfterDefineOf: newMir
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: incrementally updating\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         reassociateKleinObjectFrom: oldObj To: newObj = ( |
            | 
            "Fixes up all oracles, relocators, etc. to
            associate a different self object with the same Klein object.
            newObj had better be a copy of oldObj."

            replaceObject: oldObj With: newObj.

            willNeedToRelocate  ifTrue: [ 
              "NMethods will need to be relocated, must point to new universe, etc."
              inNMethodsDefine: (reflect: oldObj) ToBe: (reflect: newObj).
            ].
            newObj).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateIfNeededTo: start = ( |
            | 
            start = myVM machineMemory start ifFalse: [
              error: 'not sure relocation works anymore, ',
                     'my advice: rebuild the image'
            ].
            willNeedToRelocate ifTrue: [
              relocateTo: start
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: relocating\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         relocateOopDictionariesBy: delta = ( |
            | 
            heapOffset: heapOffset + delta.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fCategory: relocating\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         relocateOopsAndBytesPartsBy: delta = ( |
            | 
            myVM setTheVMAndDo: [
              theVM objectLocator ifDirect: [] IfIndirect: [
                [todo cleanup relocation oopFormat].
                halt. "will not work for object table"
              ].

              myVM universe oopsDo: [|:oop. :addr|
                vmKit tag
                  ifOop: oop
                  IsFloat: nil
                  IsSmi: nil
                  IsMark: [
                    (vmKit layouts mark isMarkForByteVector: oop)  ifTrue: [
                       vmKit layouts byteVector
                         relocateBytesPartRefAt: addr By: delta
                    ]
                  ]
                  IsMem: [
                     vmKit layouts memoryObject relocateMemOop: oop At: addr By: delta
                  ]
              ]
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: tracking nmethods\x7fComment: NMethod has just been moved.
Fixup the oops. -- dmu 3/04\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         relocateOopsInMachineCodeBy: delta = ( |
            | 
            myVM setTheVMAndDo: [ nmethodsDo: [|:nm| nm relocateOopsBy: delta ]].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fCategory: relocating\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         relocateTo: start = ( |
             delta.
            | 

            myVM machineMemory relocateTo: start.
            myVM universe spacesDo: [|:s| delta: s relocateTo: start].

            statusReporter show: 'relocating oops and bytes parts'
              While: [relocateOopsAndBytesPartsBy: delta].

            statusReporter show: 'relocating oop dictionaries'
              While: [relocateOopDictionariesBy: delta].

            statusReporter show: 'relocating oops in machine code'
              While: [relocateOopsInMachineCodeBy: delta].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateTo: start WriteTo: debuggee PointMemoryTo: debugger IfFail: fb = ( |
            | 
            relocateIfNeededTo: start.
            writeAllDataTo: debuggee IfFail: [|:e| ^ fb value: e].
            myVM pointMemoryInterfaceTo: debugger.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: tracking nmethods\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAnyNMethodsForMapOID: mapOID = ( |
            | 
            objectsOracle removeAnyNMethodsForMapOID: mapOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: tracking objects\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         replaceObject: oldObj With: newObj = ( |
            | 
            objectsOracle replaceObject: oldObj
                                   With: newObj).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         selectorsSent = ( |
             r.
            | 
            r: set copyRemoveAll.
            objectsOracle methodMirrorsAllowingDuplicates do: [|:m|
              m literalsDo: [|:lit|
                (reflect: lit) isReflecteeString ifTrue: [
                  r add: lit.
                ].
              ].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         selfPrototypeOf: m = ( |
            | 
            m prototypeIfPresent: [|:p| p]
                        IfAbsent: [case
                                          if: [m isReflecteeBlockMethod] Then: [mirrors blockMethod ]
                                          If: [m isReflecteeMethod     ] Then: [mirrors method      ]
                                          If: [m isReflecteeBlock      ] Then: [mirrors block       ]
                                          If: [m isReflecteeString     ] Then: [reflect: ''         ]
                                          If: [m isReflecteeVector     ] Then: [reflect: vector     ]
                                        Else:                                  [reflect: ()         ]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         sortedCodeSizeStats = ( |
            | 
            (objectsOracle codeSizeStatistics copyMappedBy: [|:codeSize. :kindOfGeneration| codeSize @ kindOfGeneration])
               asVector copySortBy: (| element: a Precedes: b = (a x > b x) |)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         sortedOccurrenceCountsFor: aCollection = ( |
             occurrences.
            | 
            occurrences: list copyRemoveAll.
            aCollection occurrencesOfEachElement do: [|:n. :p| occurrences add: n @ p].
            occurrences asVector copySortBy: (| element: a Precedes: b = (a x > b x) |)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         statisticsMethodNames = ( |
            | 
            ( [breakDownObjectsByMap]
            & [breakDownObjectsBySelfPrototype]
            & [moduleSizes]
            & [breakDownNMethodsByHolder]
            & [breakDownNMethodsBySlotType]
            & [sortedCodeSizeStats]
            & [unsentSlotsByHolder]
            ) asVector copyMappedBy: [|:b| b asMirror methodSource]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         trackingObjectsInMyOracle* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'trackingObjectsInMyOracle' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fCategory: prototypes\x7fComment: This is a terrible name for this object. -- Adam, 10/05\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         typeStat = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda vmImage parent typeStat.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myImage.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         objectStats <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda vmImage parent typeStat parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         averageSize = ( |
            | 
            size /~ objectCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            resend.copy objectStats: objectStats copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForType: t Image: im = ( |
            | 
            (copy type: t) myImage: im).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         example = ( |
            | 
            objectStats first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         largestExample = ( |
             r.
            | 
            r: objectStats first.
            objectStats do: [|:os| os size > r size ifTrue: [r: os]].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOops = ( |
            | 
            myImage myVM setTheVMAndDo: [
              objectStats copyMappedBy: [|:os| kleinAndYoda layouts object mapOf: os oop]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         objectCount = ( |
            | 
            objectStats size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> 'parent' -> () From: ( | {
         'Category: prototypes\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         objectSize = ( |
            | 
            [todo units]. "This is ridiculous. objectSize should be
                           some real units thing out in globals."
            myImage objectSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         originalObjects = ( |
            | 
            objectStats copyMappedBy: [|:os| os originalMirror reflectee]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         size = ( |
             r.
            | 
            r: objectSize copyWords: 0.
            objectStats do: [|:os| r: r + os size].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         summaryString = ( |
            | 
            type printString, ': ', size statePrintString, ', ', objectCount printString, ' instances').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> 'typeStat' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         type.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         uninvokedNMethods = ( |
            | 
            (invocationCounts copyFilteredBy: [|:p| p y = 0]) copyMappedBy: [|:p| p x]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         uninvokedNMethodsByHolder = ( |
             d.
            | 
            d: dictionary copyRemoveAll.
            uninvokedNMethods do: [|:nm| (d at: (reflect: nm methodHolder) IfAbsentPut: [list copyRemoveAll]) add: nm].
            d).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         unsentSlots = ( |
             r.
             sels.
            | 
            r: list copyRemoveAll.
            sels: selectorsSent asSet.
            wellKnownSlotsDo: [|:s| (sels includes: s name) ifFalse: [r add: s]].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         unsentSlotsByHolder = ( |
             d.
             r.
            | 
            d: dictionary copyRemoveAll.
            unsentSlots do: [|:s|
              (d at: s holder IfAbsentPut: [list copyRemoveAll]) add: s.
            ].

            r: list copyRemoveAll.
            d do: [|:ss. :m| r add: (ss copyMappedBy: [|:s| s name]) asVector copySort @ m].
            r asVector copySortBy: (| element: a Precedes: b = (a x size > b x size) |)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         verify = ( |
            | 
            myVM setTheVMAndDo: [
              objectsOracle verify.
              verifyOIDsInMarks.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         verifyOIDsInMarks = ( |
            | 
            objectsOracle oopsDo: [|:oop|
              [(oidForOop: oop) = (vmKit layouts mark oidOfMarkValue: vmKit layouts memoryObject markValueOf: oop)] assert.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            myVM vmKit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: incrementally updating\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         waitUntilDefineHasPropagated = ( |
            | 
            times delay: 100. "give it a chance to start"
            definePropagationSema protect: [].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: statistics\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         wellKnownSlotsDo: aBlock = ( |
            | 
            myVM setTheVMAndDo: [
              myVM exportPolicy invalidateCachedModuleNameLists.
              myVM exportPolicy modulesToMap allNamesOfIncludedModules do: [|:mName. m|
                mName isEmpty ifFalse: [
                  m: mName sendTo: modules.
                  m slots do: [|:s|
                    [|:exit. kleinMir. oop|
                      oop:  oopForOriginalMirror: s holder IfAbsent: exit.
                      kleinMir: myVM mirrorFor: oop.
                      (kleinMir includesKey: s name) ifTrue: [
                        aBlock value: s.
                      ].
                    ] exit.
                  ].
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         willNeedToRelocate = ( |
            | 
            [todo fix me relocation]. false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> 'parent' -> () From: ( | {
         'Category: launching\x7fModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         writeAllDataTo: aForeignProcess IfFail: fb = ( |
            | 
            myVM machineMemory writeAllPiecesTo: aForeignProcess IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'vmImage' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (userQuery)\x7fVisibility: public'
        
         statusReporter <- bootstrap stub -> 'globals' -> 'userQuery' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot'
        
         vmKitVMImage = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMImage' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMImage' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitVMImage.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMImage' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMImage' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMImage' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMImage' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMImage' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.12 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVMImage' -> () From: ( | {
         'ModuleInfo: Module: vmKitVMImage InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitVMImage postFileIn
