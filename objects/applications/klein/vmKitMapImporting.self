 '$Revision: 30.10 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: importing\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         importLiteralFromOopWhenPossible: litOop = ( |
            | 
            kleinAndYoda tag
              ifOop: litOop
              IsFloat: [layouts float decode: litOop]
              IsSmi:   [layouts smi   decode: litOop]
              IsMark:  [error: 'literal oop should not be a mark']
              IsMem:   [(layouts memoryObject memIsByteVector: litOop)
                           ifTrue: [importString: litOop IfFail: [unimportableLiteral]]
                            False: [unimportableLiteral]]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: importing\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeByImportingMap: mapOop IfFail: fb = ( |
             literalsOops.
            | 
            resend.initializeByImportingMap: mapOop IfFail: [|:e| ^ fb value: e].

            [literals. codes. line. file. sourceString. activationPartSizes]. "browsing"

            "Fail softly to avoid crashing the UI. -- dmu 3/06"

              codes:               importString:       (maps mapMap hardwiredOopIn: mapOop OfSlotNamed: 'codes'       ) IfFail: ''.
              literalsOops:        importVector:       (maps mapMap hardwiredOopIn: mapOop OfSlotNamed: 'literals'    ) IfFail: vector.
              literals: literalsOops mapBy: [|:oop| importLiteralFromOopWhenPossible: oop].

            theVM exportPolicy shouldOmitLineAndFile ifFalse: [
              line:                importSmi:          (maps mapMap hardwiredOopIn: mapOop OfSlotNamed: 'line'        ) IfFail: -1.
              file:                importString:       (maps mapMap hardwiredOopIn: mapOop OfSlotNamed: 'file'        ) IfFail: ''.
            ].

            theVM exportPolicy shouldOmitSourceStrings ifFalse: [
              sourceString:        importString:       (maps mapMap hardwiredOopIn: mapOop OfSlotNamed: 'sourceString') IfFail: ''.
            ].

            theVM exportPolicy shouldOmitActivationPartSizes ifFalse: [
              activationPartSizes: importSmi:          (maps mapMap hardwiredOopIn: mapOop OfSlotNamed: 'activationPartSizes') IfFail: -1.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: importing\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         unimportableLiteral = 'unimportableLiteral'.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> 'parent' -> () From: ( | {
         'Category: importing\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         importMethod: mOop IfFail: fb = ( |
            | 
            "Do we need to import the actual activationMap, or
             can we get away with just using the oop? -- Adam, 3/06"
            mOop).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> 'parent' -> () From: ( | {
         'Category: importing\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeByImportingMap: mapOop IfFail: fb = ( |
             m.
            | 
            resend.initializeByImportingMap: mapOop IfFail: [|:e| ^ fb value: e].

            [lexicalParent. sourceOffset. sourceLength]. "browsing"

            m: maps mapMap.

            theVM exportPolicy shouldOmitLexicalParent ifFalse: [
              lexicalParent:  importMethod: (m hardwiredOopIn: mapOop OfSlotNamed: 'lexicalParent') IfFail: ().
            ].

            theVM exportPolicy shouldOmitSourceStrings ifFalse: [
               sourceOffset:  importSmi:    (m hardwiredOopIn: mapOop OfSlotNamed: 'sourceOffset')  IfFail: -1.
               sourceLength:  importSmi:    (m hardwiredOopIn: mapOop OfSlotNamed: 'sourceLength')  IfFail: -1.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: public'
        
         isBlockMethodActivation = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         hardwiredNumberOfSlotsInMap: mapOop IfFail: fb = ( |
            | 
            ((maps mapMap indexableSizeOf: mapOop
                                         IfFail: [|:e| ^ fb value: e]
            ) - scalarValueCount) / slotDescSize).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         hardwiredOopIn: holderOop OfSlotNamed: n = ( |
            | 
            "In case we did not export this slot (to save space)
             fail with a bad oop so the import will fail. -- dmu 3/06"
            hardwiredOopIn: holderOop OfSlotNamed: n IfFail: vmKit tag mark).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         hardwiredOopIn: holderOop OfSlotNamed: n IfFail: fb = ( |
             mapOop.
            | 
            mapOop: mapOf: holderOop IfFail: [|:e| ^ fb value: e].
            hardwiredSlotDataOopIn: holderOop 
                                At: (indexOfSlotNamed: n In: mapOop IfAbsent: [|:e| ^ fb value: e])
                            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         hardwiredSlotAnnotationInMap: mapOop At: i IfFail: fb = ( |
            | 
            importString: (hardwiredSlotAnnotationOopInMap: mapOop
                                                        At: i
                                                    IfFail: [|:e| ^ fb value: e])
                  IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         hardwiredSlotDataInMap: mapOop At: i IfFail: fb = ( |
             dataOop.
             type.
            | 
            type:    hardwiredSlotTypeInMap:    mapOop At: i IfFail: [|:e| ^ fb value: e].
            dataOop: hardwiredSlotDataOopInMap: mapOop At: i IfFail: [|:e| ^ fb value: e].
            (vmKit slotType isMapSlot: type)
              ifTrue: [           dataOop           ]
               False: [importSmi: dataOop IfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         hardwiredSlotDataOopIn: oop At: i IfFail: fb = ( |
             mapData.
             mapOop.
             type.
            | 
            [contentsOfSlotAt: i In: oop IfFail: fb]. "browsing"
            mapOop: mapOf: oop IfFail: [|:e| ^ fb value: e].
            mapData: hardwiredSlotDataInMap: mapOop At: i IfFail: [|:e| ^ fb value: e].
               type: hardwiredSlotTypeInMap: mapOop At: i IfFail: [|:e| ^ fb value: e].
            contentsOfSlotWithType: type
                    ContainingData: mapData
                                In: oop
                            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         hardwiredSlotNameInMap: mapOop At: i IfFail: fb = ( |
            | 
            importString: (hardwiredSlotNameOopInMap: mapOop
                                                  At: i
                                              IfFail: [|:e| ^ fb value: e])
                  IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         hardwiredSlotTypeInMap: mapOop At: i IfFail: fb = ( |
            | 
            importSmi: (hardwiredSlotTypeOopInMap: mapOop
                                               At: i
                                           IfFail: [|:e| ^ fb value: e])
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         importMap: mapOop IfFail: fb = ( |
            | 
            (copySize: hardwiredNumberOfSlotsInMap: mapOop IfFail: [|:e| ^ fb value: e])
                initializeByImportingMap: mapOop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: public'
        
         importRemoteMap: mapOop IfFail: fb = ( |
            | 
            theVM mirrorCache importedMapFor: mapOop IfAbsentPut: [| m |
              m: mapProtoForMemMapOop: mapOop IfFail: [|:e| ^ fb value: e].
              m importMap: mapOop IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: public'
        
         importRemoteMapFor: oop IfFail: fb = ( |
             mapOop.
            | 
            mapOop: mapOf: oop IfFail: [|:e| ^ fb value: e].
            theVM mirrorCache importedMapFor: mapOop IfAbsentPut: [| m |
              m: mapProtoForImporting: oop IfFail: [|:e| ^ fb value: e].
              m importMap: mapOop IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         indexOfSlotNamed: n In: mapOop IfAbsent: aBlk = ( |
             name.
             s.
            | 
            s: hardwiredNumberOfSlotsInMap: mapOop 
                                    IfFail: [|:e| ^ aBlk value: e].
            s do: [|:i. nameIndex. nameOop|
              nameIndex: nameIndexAt: i.
              nameOop: maps mapMap for: mapOop
                           IndexableAt: nameIndex
                                IfFail: [|:e| ^ aBlk value: e].
              name: importString: nameOop IfFail: [|:e| ^ aBlk value: e].
              name = n  ifTrue: [^ i].
            ].
            aBlk value: 'not found').
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeByImportingMap: mapOop IfFail: fb = ( |
            | 
            [annotation. nmethodCache]. "browsing"
            theVM exportPolicy shouldOmitObjectAnnotations ifFalse: [|annotationOop|
              annotationOop: maps mapMap hardwiredOopIn: mapOop OfSlotNamed: 'annotation'.
              annotation:  importString: annotationOop IfFail: ''.
            ].

            scalarValueIndicesToImportDo: [|:i|
              basicAt: i Put: maps mapMap for: mapOop IndexableAt: i IfFail: [|:e| ^ fb value: e].
            ].

            size do: [|:i|
                      at: i
                 PutName: (hardwiredSlotNameInMap:       mapOop At: i IfFail: [|:e| ^ fb value: e])
                    Type: (hardwiredSlotTypeInMap:       mapOop At: i IfFail: [|:e| ^ fb value: e])
                    Data: (hardwiredSlotDataInMap:       mapOop At: i IfFail: [|:e| ^ fb value: e])
              Annotation: (hardwiredSlotAnnotationInMap: mapOop At: i IfFail: [|:e| ^ fb value: e]).
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         mapProtoForImporting: oop IfFail: fb = ( |
            | 
            vmKit tag
             ifOop:   oop
             IsFloat: [theVM maps floatMap]
             IsSmi:   [theVM maps   smiMap]
             IsMark:  [fb value: 'attempted to import a mark']
             IsMem:   [ | mapOop. mt | 
                         mapOop: mapOf: oop IfFail: [|:e| ^ fb value: e].
                         mapProtoForMemMapOop: mapOop IfFail: fb
                      ]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: public'
        
         mapTypeOfRemoteMap: mapOop IfFail: fb = ( |
             mapTypeAddress.
             mapTypeStringOop.
            | 
            [mapType]. "browsing"
            mapTypeAddress: mapTypeAddressOf: mapOop
                                      IfFail: [|:e| ^ fb value: e]. 
            mapTypeStringOop: theVM machineMemory  oopAt: mapTypeAddress
                                                  IfFail: [|:e| ^ fb value: e].
            importString: mapTypeStringOop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fCategory: importing remote maps\x7fModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         scalarValueIndicesToImportDo: blk = ( |
            | 
            blk value: nmethodCacheIndex.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot'
        
         vmKitMapImporting = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMapImporting' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMapImporting' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitMapImporting.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMapImporting' -> () From: ( | {
         'ModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMapImporting' -> () From: ( | {
         'ModuleInfo: Module: vmKitMapImporting InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMapImporting' -> () From: ( | {
         'ModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMapImporting' -> () From: ( | {
         'ModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMapImporting' -> () From: ( | {
         'ModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.10 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMapImporting' -> () From: ( | {
         'ModuleInfo: Module: vmKitMapImporting InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitMapImporting postFileIn
