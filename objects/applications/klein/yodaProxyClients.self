 '$Revision: 30.11 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> () From: ( | {
         'Category: application information objects\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         yoda = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'yoda' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent appInfoProtos yoda.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'yoda' -> () From: ( | {
         'ModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot'
        
         defaultPort = 9091.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'yoda' -> () From: ( | {
         'ModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot'
        
         executableName = 'yoda'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'yoda' -> () From: ( | {
         'ModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot'
        
         initiationName = 'small_self'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'yoda' -> () From: ( | {
         'ModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'yoda' -> () From: ( | {
         'Category: downloading\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot'
        
         sendBootstrapInfo: aVMImage Via: proxy = ( |
            | 
            userQuery reportAndContinue: 'pid = ', proxy debuggeePID printString.
            aVMImage myVM setTheVMAndDo: [
              proxy              setVMOopTo: aVMImage oopForTheVM
                    AndObjectTableAddressTo: aVMImage addressForTheObjectTable
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'yoda' -> () From: ( | {
         'Category: downloading\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot'
        
         startPCForImage: aVMImage Via: proxy = ( |
            | 
            proxy checkLayoutConstants.
            proxy getStartFunctionAddress).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: smallSelf specific\x7fCategory: layout constants\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         bumpIDsAfter: n = ( |
            | 
            "So that it's not such a pain to insert new IDs into the middle of the list. -- Adam, 5/06"
            (reflect: yoda layouts wellKnownObjects ids) do: [|:s. i|
              i: s contents reflectee.
              i > n ifTrue: [s contents: reflect: i succ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: smallSelf specific\x7fCategory: layout constants\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         checkLayoutConstantsIfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [ | lc |
                writeYodaRequestType: yodaRequestTypes getLayoutConstants IfFail: exit.
                socket flushOutputIfFail: exit.


                readAndCheckLayoutConstant: [ vmKit tag smi   ]   IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit tag mem   ]   IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit tag float ] IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit tag mark  ]  IfFail: exit.

                readAndCheckLayoutConstant: [ vmKit tag mask  ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit tag size  ]  IfFail: exit.

                readAndCheckLayoutConstant: [ vmKit layouts object oopSize]  IfFail: exit.

                readAndCheckLayoutConstant: [ vmKit layouts mark isByteVectorField             shift ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts mark isActivationMapField          shift ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts mark isActivationField             shift ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts mark hasBeenVisitedForLookupField  shift ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts mark hasBeenVisitedForGCField      shift ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts mark isOnMarkStackForGCField       shift ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts mark isInRememberedSetForGCField   shift ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts mark oidField                      shift ]  IfFail: exit.

                readAndCheckLayoutConstant: [ vmKit layouts memoryObject markField fixedIndex        ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts memoryObject  mapField fixedIndex        ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts memoryObject lastField fixedIndexAfterMe ]  IfFail: exit.

                readAndCheckLayoutConstant: [ vmKit layouts abstractUnsegregatedVector   indexableSizeField fixedIndex        ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts abstractUnsegregatedVector indexableOriginField fixedIndex        ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts abstractUnsegregatedVector            lastField fixedIndexAfterMe ]  IfFail: exit.

                readAndCheckLayoutConstant: [ vmKit layouts block    homeFramePointerField fixedIndex       ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts block                lastField fixedIndexAfterMe]  IfFail: exit.

                readAndCheckLayoutConstant: [ vmKit slotType slotTypeField   objectSlotValue]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit slotType slotTypeField      mapSlotValue]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit slotType slotTypeField argumentSlotValue]  IfFail: exit.

                readAndCheckLayoutConstant: [ vmKit maps map       nameOffset]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit maps map       typeOffset]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit maps map       dataOffset]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit maps map annotationOffset]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit maps map     slotDescSize]  IfFail: exit.

                readAndCheckLayoutConstant: [ vmKit maps map mapTypeIndex     ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit maps map nmethodCacheIndex]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit maps map scalarValueCount ]  IfFail: exit.

                readAndCheckLayoutConstant: [ yoda virtualMachines smallImage exportPolicy expectedMapObjOffsetForActivationPartSizes]  IfFail: exit.
                readAndCheckLayoutConstant: [ yoda virtualMachines smallImage exportPolicy expectedMapObjOffsetForCodes              ]  IfFail: exit.
                readAndCheckLayoutConstant: [ yoda virtualMachines smallImage exportPolicy expectedMapObjOffsetForLiterals           ]  IfFail: exit.

                readAndCheckLayoutConstant: [ yoda virtualMachines smallImage exportPolicy expectedObjectLocatorOffsetForLastInvalidEntry ]  IfFail: exit.

                readAndCheckLayoutConstant: [ vmKit tenuredSpace lastFreeOopsListIndex ]  IfFail: exit.

                readAndCheckIDsIfFail: fb.

                readAndCheckLayoutConstant: [ yoda layouts wellKnownObjects offsets oop                    ]  IfFail: exit.
                readAndCheckLayoutConstant: [ yoda layouts wellKnownObjects offsets address                ]  IfFail: exit.
                readAndCheckLayoutConstant: [ yoda layouts wellKnownObjects offsets length                 ]  IfFail: exit.

                readAndCheckLayoutConstant: [ vmKit layouts activation               sp_field fixedIndex  ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts activation               pc_field fixedIndex  ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts activation             self_field fixedIndex  ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts activation             rcvr_field fixedIndex  ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts activation           sender_field fixedIndex  ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts activation     methodHolder_field fixedIndex  ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts activation pc_after_endInit_field fixedIndex  ]  IfFail: exit.
                readAndCheckLayoutConstant: [ vmKit layouts activation                first_stack_offset  ]  IfFail: exit.

                readStatusIfFail:                 exit.
                lc
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting objectLocator timestamp\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getObjectLocatorTimestampAddressIfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                writeYodaRequestType: yodaRequestTypes getObjectLocatorTimestampAddress IfFail: exit.
                        socket flushOutputIfFail: exit.
                        socket     readIntIfFail: exit.
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting address of Yoda active context\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         getYodaWellKnownObjectsAddressIfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                writeYodaRequestType: yodaRequestTypes getWellKnownObjectsAddress IfFail: exit.
                        socket flushOutputIfFail: exit.
                        socket     readIntIfFail: exit.
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: getting objectLocator timestamp\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         objectLocatorTimestampAt: addr IfFail: fb = ( |
            | 
            readMemoryWordAt: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: smallSelf specific\x7fCategory: layout constants\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         readAndCheckIDsIfFail: fb = ( |
             ids.
             v.
            | 
            ids: yoda layouts wellKnownObjects ids.
            [nil_object]. "browsing"
            v: (reflect: ids) asVector
                              sortBy: (|element: a Precedes: b = ( a contents reflectee < b contents reflectee)|).
            v do: [|:s| readAndCheckLayoutConstantNamed: s name Value: s contents reflectee IfFail: [|:e| ^ fb value: e]].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: smallSelf specific\x7fCategory: layout constants\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         readAndCheckLayoutConstant: shouldBeBlock IfFail: fb = ( |
             shouldBeSource.
             shouldBeValue.
            | 
            shouldBeValue: shouldBeBlock value.
            shouldBeSource: shouldBeBlock asMirror methodSource.
            readAndCheckLayoutConstantNamed: shouldBeSource Value: shouldBeValue IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: smallSelf specific\x7fCategory: layout constants\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         readAndCheckLayoutConstantNamed: shouldBeSource Value: shouldBeValue IfFail: fb = ( |
             cName.
             cValue.
            | 
            cName:  socket readStringIfFail: [|:e| fb value: 'could not read description of <', shouldBeSource, '> : ', e].
            cName = '' ifTrue: [|:e| fb value: 'got the end too soon, expected: ', shouldBeSource].
            cValue: socket readIntIfFail:    [|:e| fb value: 'could not read value of <', shouldBeSource, '>: ', e].
            cValue = shouldBeValue ifFalse: [fb value: cName, ' is ', cValue printString, ' but ', shouldBeSource, ' is ', shouldBeValue printString].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: smallSelf specific\x7fCategory: setting C variables\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         setVMOopTo: vmOop AndObjectTableAddressTo: objectTableAddr IfFail: fb = ( |
            | 
            "Convoluted to release sema before running fail block. -- dmu 8/04"
            fb value: [
              |:exit|
              ^ safelyDoRequest: [
                | r |
                writeYodaRequestType: yodaRequestTypes setBootstrapInfo
                              IfFail: exit.

                socket writeInt: vmOop IfFail: exit.
                socket writeInt: objectTableAddr   IfFail: exit.

                socket flushOutputIfFail: exit.
                readStatusIfFail:  exit.
                r
              ]
            ] exitValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         yoda_shortcuts* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'yoda_shortcuts' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent yoda_shortcuts.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'yoda_shortcuts' -> () From: ( | {
         'Category: layout constants\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         checkLayoutConstants = ( |
            | 
            checkLayoutConstantsIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'yoda_shortcuts' -> () From: ( | {
         'Category: setting C variables\x7fModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         setVMOopTo: vmOop AndObjectTableAddressTo: objectTableAddr = ( |
            | 
            setVMOopTo: vmOop AndObjectTableAddressTo: objectTableAddr IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot'
        
         yodaProxyClients = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProxyClients' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProxyClients' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaProxyClients.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProxyClients' -> () From: ( | {
         'ModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProxyClients' -> () From: ( | {
         'ModuleInfo: Module: yodaProxyClients InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProxyClients' -> () From: ( | {
         'ModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProxyClients' -> () From: ( | {
         'ModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProxyClients' -> () From: ( | {
         'ModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.11 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaProxyClients' -> () From: ( | {
         'ModuleInfo: Module: yodaProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules yodaProxyClients postFileIn
