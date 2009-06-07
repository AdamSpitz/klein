 '$Revision: 30.7 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: building VMs\x7fCategory: incremtnal update\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         incrementalObjectMapper1 = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda objectMapper1 copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda incrementalObjectMapper1.

CopyDowns:
globals kleinAndYoda objectMapper1. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         copyOfOldObjectLocator <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fComment: Optimization: If the new object is the same size as the old object
(which happens often - any time we make a change that only affects
the object\'s map, like adding a constant slot or editing a method),
don\'t bother switching all the pointers in the heap to point to the
new object; just copy the bits of the new object into the address
where the old object was. -- Adam, 5/05\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         isSameSize <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> () From: ( | {
         'Category: object mapper state\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         mapsWhoseAncestorsIncludeTheDefinee.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda incrementalObjectMapper1 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: mapping immediates\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateImmediateMaps = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateMemoryObjectTotalWordCount: totalWordCount OriginalMirror: originalMirror KleinifiedMirror: kleinifiedMirror = ( |
             isDefinee.
             oop.
            | 

            isDefinee:  originalMirror = (reflect: objectThatWasDefined).

            oop:
              isDefinee && [isSameSize] ifTrue: [
                image oopForOriginalObject: objectThatWasDefined.
              ] False: [
                resend.allocateMemoryObjectTotalWordCount: totalWordCount
                                           OriginalMirror: originalMirror
                                         KleinifiedMirror: kleinifiedMirror
              ].

            isDefinee ifTrue: [
              objectsOracle oopForObjectThatWasDefined: oop.
            ].

            oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateOIDForOriginalMirror: originalMirror = ( |
            | 
            originalMirror = (reflect: objectThatWasDefined) ifTrue: [
              ^ oidForOriginalObject: objectThatWasDefined
            ].

            resend.allocateOIDForOriginalMirror: originalMirror).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForVM: aVM ReportTo: reporter ObjectThatWasDefined: definee = ( |
             r.
            | 
            r: copy.
            r myVM: aVM.
            r copyOfOldObjectLocator: aVM objectLocator copy.
            r objectsOracle oldOopForObjectThatWasDefined: theVM image oopForOriginalObject: definee.
            r objectsOracle ensureOIDVectorsHaveSizeAtLeast: r objectsOracle addressesByOID size.
            r statusReporter: reporter.
            r objectThatWasDefined: definee.
            r initializeCanonicalizedStrings.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: object table\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         createObjectTable = ( |
            | 
            "Optimization: The object table is huge. If possible, overwrite the old one with the
             new one, rather than allocating a whole new object for it. -- Adam, 12/05"

            copyOfOldObjectLocator size = objectsOracle addressesByOID size ifTrue: [| oldObjectLocatorMir |
              oldObjectLocatorMir:  theVM image mirrorOnTheObjectLocatorIfFail: raiseError.
              exportContentsOf: (objectsOracle addressesByOID copyMappedBy: [|:x| (reflect: x) kleinAndYodaLayout oopForValue: x])
               IntoReflecteeOf: oldObjectLocatorMir.
            ].
            resend.createObjectTable).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: object table\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         desiredStartingSizeForObjectTable: t = ( |
            | 
            t size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: exporting changed objectLocator entries\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         exportChangedObjectTableEntriesFrom: anObjectLocator To: anObjectLocatorMir = ( |
            | 
            objectsOracle oidsDo: [|:oid. :oop. s|
              s: fakeSlot vectorElement copyMirror: anObjectLocatorMir ElementIndex: oid.
              s setRemoteContentsFromLocalContentsIn: anObjectLocator.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: object table\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         exportContentsOf: aVector IntoReflecteeOf: m = ( |
            | 
            m reflectionPrimitives forVectorMirror: m ExportContentsOf: aVector IntoReflecteeIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         image = ( |
            | 
            theVM image).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: canonical strings\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeCanonicalizedStrings = ( |
            | 
            canonicalizedStrings: theVM universe canonicalizedStrings copy.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: compiling nmethods\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeExemplarMirrorsLeftToCompile = ( |
            | 
            mapsAffectedByChange do: [|:m| exemplarMirrorsLeftToCompile addFirst: reflect: image objectsOracle exemplarForMap: m].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         isIncremental = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: compiling nmethods\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         mapsAffectedByChange = ( |
             r.
            | 
            r: identitySet copyRemoveAll.
            r addAll: mapsWhoseAncestorsIncludeTheDefinee.
            r addAll: mapsContainingUncompiledSlotsThatNeedToBeCompiled.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: compiling nmethods\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         mapsContainingUncompiledSlotsThatNeedToBeCompiled = ( |
             newSels.
             r.
            | 
            r: identitySet copyRemoveAll.
            newSels: objectsOracle newSelectorsToCompile.
            newSels do: [|:s|
              image objectsOracle mapOIDsContainingUncompiledSlots if: s IsPresentDo: [|:mapOIDs|
                mapOIDs do: [|:mapOID| r add: image objectsOracle originalObjectForOID: mapOID].
              ].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: compiling nmethods\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         newSelectorsToCompile = ( |
            | 
            objectsOracle selectorsToCompile asSet copyFilteredBy: [|:s| (image objectsOracle selectorsToCompile includes: s) not]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         objectThatWasDefined = ( |
            | 
            objectsOracle objectThatWasDefined).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         objectThatWasDefined: definee = ( |
             defineeMir.
            | 
            objectsOracle objectThatWasDefined: definee.

            defineeMir: reflect: definee.
            isSameSize:   (defineeMir vmKitMapForConversion myLayout wordSizeOf: image oopForOriginalObject: definee)
                        = (totalWordCountForReflecteeOf: defineeMir).

            mapsWhoseAncestorsIncludeTheDefinee: image objectsOracle mapsWhoseAncestorsIncludeTheMapOf: definee.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: oracles\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         objectsOracleProto = ( |
            | 
            vmKit incrementalUpdateObjectsOracle).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: compiling nmethods\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         requestCompilationForReceiverMirror: rMir Map: rMap MapOID: rMapOID = ( |
            | 
            image removeAnyNMethodsForMapOID: rMapOID. "so they don't get reused"

            resend.requestCompilationForReceiverMirror: rMir Map: rMap MapOID: rMapOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: nmethod invocation counts\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         setNMethodTableForUniverse: u To: nmethods = ( |
            | 
            [todo statistics incrementalUpdate].
            "What if the stuff that was just incrementally
             updated had some new nmethods in it? Do this
             the same way we do the string table?
             -- Adam, 6/05"
            halt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldAlwaysReassembleRelocatorsForObject: o = ( |
            | 
            isSameSize not && [o _Eq: objectThatWasDefined]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalObjectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         startingPoints = ( |
            | 
            vector copyAddLast: objectThatWasDefined).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: building VMs\x7fCategory: incremtnal update\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         incrementalUpdateObjectsOracle = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda objectsOracle copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda incrementalUpdateObjectsOracle.

CopyDowns:
globals kleinAndYoda objectsOracle. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         objectThatWasDefined.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         oldOopForObjectThatWasDefined.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         oopForObjectThatWasDefined.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda incrementalUpdateObjectsOracle parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         canonicalMapFor: aMap IfPresentDo: pb IfAbsentRecordExemplarOID: oid AndDo: ab = ( |
            | 
            image objectsOracle
               canonicalMapFor: aMap
                   IfPresentDo: pb
                    IfAbsentDo: [   resend.canonicalMapFor: aMap
                                               IfPresentDo: pb
                                 IfAbsentRecordExemplarOID: oid
                                                     AndDo: ab]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         exemplarOIDsDo: blk = ( |
            | 
            newExemplarOIDsDo: blk.
            image objectsOracle exemplarOIDsDo: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         image = ( |
            | 
            theVM image).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedMirrorForOID: oid IfAbsent: ab = ( |
            | 
            resend.kleinifiedMirrorForOID: oid IfAbsent: [
              image kleinifiedMirrorForOID: oid IfAbsent: ab
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         newExemplarOIDsDo: blk = ( |
            | 
            resend.exemplarOIDsDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         newNMethodOIDDictionariesByMapOIDDo: blk = ( |
            | 
            resend.nmethodOIDDictionariesByMapOIDDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         newSelectorsToCompile = ( |
            | 
            selectorsToCompile asSet copyFilteredBy: [|:s| (image objectsOracle selectorsToCompile includes: s) not]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodOIDDictionariesByMapOIDDo: blk = ( |
            | 
            newNMethodOIDDictionariesByMapOIDDo: [|:newNMOIDs. :mapOID. allNMOIDs|
              allNMOIDs:
                image objectsOracle
                   nmethodOIDDictionaryForMapOID: mapOID
                                       IfPresent: [|:existingNMOIDs| newNMOIDs, existingNMOIDs]
                                        IfAbsent: [                  newNMOIDs                ].
              blk value: allNMOIDs With: mapOID.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         objectTable = ( |
             r.
            | 
            r: image objectsOracle objectTable.
            r: r copySize: r size max: oidsByOriginalObject max succ.
            oidsByOriginalObject do: [|:oid| r at: oid Put: kleinifiedObjectForOID: oid].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOop: oop IfAbsent: ab = ( |
            | 
            resend.oidForOop: oop IfAbsent: [
              image oidForOop: oop IfAbsent: ab
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oidForOriginalObject: obj IfAbsent: ab = ( |
            | 
            resend.oidForOriginalObject: obj IfAbsent: [
              image oidForOriginalObject: obj IfAbsent: ab
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOID: oid IfAbsent: ab = ( |
            | 
            "If this OID is for the object that was defined, the
             oop in the object table may be wrong (because we may
             not have allocated the object yet). If we *have*
             already allocated the object that was defined, its
             oop will be stored in oopForObjectThatWasDefined,
             so return that. -- Adam, 12/05"
            (reflect: objectThatWasDefined) = (originalMirrorForOID: oid) ifTrue: [
              ^ oopForObjectThatWasDefined ifNil: [ab value]
            ].

            resend.oopForOID: oid IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: tracking\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         originalMirrorForOID: oid IfAbsent: ab = ( |
            | 
            resend.originalMirrorForOID: oid IfAbsent: [
              image originalMirrorForOID: oid IfAbsent: ab
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         originalMirrors = ( |
            | 
            originalMirrorsAllowingDuplicates asSet).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         originalMirrorsAllowingDuplicates = ( |
            | 
            "Optimization - converting these to a set is expensive. -- Adam, 4/06"
            resend.originalMirrorsAllowingDuplicates, image objectsOracle originalMirrorsAllowingDuplicates).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdateObjectsOracle' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectsOracle' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot'
        
         vmKitIncrObjMapper = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitIncrObjMapper' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitIncrObjMapper' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitIncrObjMapper.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitIncrObjMapper' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitIncrObjMapper' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitIncrObjMapper' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitIncrObjMapper' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitIncrObjMapper' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitIncrObjMapper' -> () From: ( | {
         'ModuleInfo: Module: vmKitIncrObjMapper InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitIncrObjMapper postFileIn
