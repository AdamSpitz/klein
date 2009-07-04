 '$Revision: 30.14 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         importMapFor: oop IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mapTypeOfMap: mapOop IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         importMapFor: oop IfFail: fb = ( |
            | vmKit maps map importLocalMapFor: oop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mapTypeOfMap: mapOop IfFail: fb = ( |
            | vmKit maps map mapTypeOfLocalMap: mapOop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mapperForRecreatingPositionTable = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mapperForRecreatingPositionTable' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda mapperForRecreatingPositionTable.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mapperForRecreatingPositionTable' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         bytecodeTransmogrifier = ( |
            | 
            kleinAndYoda objectMapper1 bytecodeTransmogrifier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mapperForRecreatingPositionTable' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeMap: map ForReflecteeOf: m Slots: slots = ( |
            | 
            "For the purpose of recreating the position table, it is not
             necessary to do the work that the regular object mapper
             does in this method. -- Adam, 3/06"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mapperForRecreatingPositionTable' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mapperForRecreatingPositionTable' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         policy = ( |
            | 
            theVM exportPolicy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mapperForRecreatingPositionTable' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         recordSlot: s = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fCategory: maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         maps = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps map parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         = aMap = ( |
            | 
            mapType = aMap mapType  ifFalse: [^ false].

            ((reflect: annotation) = (reflect: aMap annotation))  ifFalse: [^ false].
             size = aMap size   ifFalse: [^ false].

            size do: [|:i| (equals: aMap At: i)  ifFalse: [^ false]].
            true).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         allMapProtosDo: blk = ( |
            | 
            (reflect: maps) do: [|:s|
              s isMethod || [s isParent]  ifFalse: [
               blk value: s contents reflectee
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         annotationAt: i = ( |
            | 
            basicAt: annotationIndexAt: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: indices\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         annotationIndexAt: i = ( |
            | 
            indexOfSlotDescOopWithOffset: annotationOffset At: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: layout constants\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         annotationOffset = 3.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         areSlotsMutable = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentSlotsDo: blk = ( |
            | 
            slotsSatisfying: [|:i| isArgumentSlotAt: i] Do: blk).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: mutating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutAnnotation: a = ( |
            | 
            basicAt: (annotationIndexAt: i) Put: a).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: mutating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutData: d = ( |
            | 
            basicAt: (dataIndexAt: i) Put: d).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: mutating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutName: n = ( |
            | basicAt: (nameIndexAt: i) Put: n).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: mutating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         at: i PutName: n Type: t Data: d Annotation: a = ( |
            | 
            at: i PutName: n.
            at: i PutType: t.
            at: i PutData: d.
            at: i PutAnnotation: a.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: mutating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         at: i PutType: t = ( |
            | 
            basicAt: (typeIndexAt: i) Put: t).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot'
        
         base* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'base' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: VM slots\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         baseNameForVMSlot: n = ( |
            | 
            n copyFrom: vmSlotNamePrefix size).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: low-level vector methods\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         basicAt: i = ( |
            | 
            basicAt: i IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: low-level vector methods\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         basicAt: i IfAbsent: fb = ( |
            | 
            _At: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: low-level vector methods\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         basicAt: i Put: x = ( |
            | 
            basicAt: i Put: x IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: low-level vector methods\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         basicAt: i Put: x IfAbsent: fb = ( |
            | 
            _At: i Put: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: low-level vector methods\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         basicCopySize: n Filler: f = ( |
            | 
            _Clone: n Filler: f).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: low-level vector methods\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         basicSize = ( |
            | _Size).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         constantParentsDo: blk = ( |
            | 
            slotsSatisfying: [|:i| (isParentAt: i) && [isConstantAt: i]] Do: blk).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fComment: Retrieves the contents of the slot from the specified
object.  Handles constant and object slots.\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         contentsOfConstantSlotNamed: n IfFail: fb = ( |
            | 
            dataAt:  indexOfSlotNamed: n IfAbsent: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         contentsOfSlotAt: i In: oop IfFail: fb = ( |
            | 
            [hardwiredSlotDataOopIn: 0 At: 0 IfFail: fb]. "browsing"

            contentsOfSlotWithType: (typeAt: i)
                    ContainingData: (dataAt: i)
                                In: oop
                            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fComment: Retrieves the contents of the slot from the specified
object.  Handles constant and object slots.\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         contentsOfSlotNamed: n In: oop IfFail: fb = ( |
             i.
            | 
            i: indexOfSlotNamed: n IfAbsent: [|:e| ^ fb value: e].
            contentsOfSlotAt: i In: oop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         contentsOfSlotWithType: type ContainingData: mapData In: oop IfFail: fb = ( |
            | 
            (vmKit slotType isMapSlot: type)
              ifTrue: [mapData]
               False: [layouts memoryObject for: oop 
                                             At: mapData
                                         IfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         copySize: s = ( |
            | 
            (basicCopySize: scalarValueCount + (s * slotDescSize) Filler: nil)
              nmethodCache: vector).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         copySizeOne = ( |
            | 
            "Used as the copy-down selector for smiMap and floatMap."
            copySize: 1).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         dataAt: i = ( |
            | basicAt: dataIndexAt: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: indices\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         dataIndexAt: i = ( |
            | 
            indexOfSlotDescOopWithOffset: dataOffset At: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: layout constants\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         dataOffset = 2.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: testing slot type\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         doesIndexOfSlotNamed: n Satisfy: testBlk IfAbsent: aBlk = ( |
            | 
            testBlk value:  indexOfSlotNamed: n IfAbsent: [|:e| ^ aBlk value: e]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: comparing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         equals: aMap At: i = ( |
            | 
                ((               nameAt: i) = (         aMap       nameAt: i))
            && [((               typeAt: i) = (         aMap       typeAt: i))
            && [((reflect:       dataAt: i) = (reflect: aMap       dataAt: i))
            && [ (reflect: annotationAt: i) = (reflect: aMap annotationAt: i) ]]]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fCategory: accessing reflectee\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         findReflecteeUsingOracle: oracle = ( |
            | 
            oracle exemplarForMap: self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fCategory: accessing reflectee\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         findVMKitMapUsingOracle: oracle = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing scalar values\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         for: mapOop AddressOfScalarValueAtIndex: i IfFail: fb = ( |
            | maps mapMap for: mapOop AddressOfIndexableAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         getMapProtoForMapType: mt = ( |
            | 
            [map. objVectorMap. activationMap]. "browsing (partial)"
            mt sendTo: maps).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing (wired-in, raw oops)\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         hardwiredSlotAnnotationOopInMap: mapOop At: i IfFail: fb = ( |
            | maps mapMap for: mapOop IndexableAt: (annotationIndexAt: i) IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing (wired-in, raw oops)\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         hardwiredSlotDataOopInMap: mapOop At: i IfFail: fb = ( |
            | maps mapMap for: mapOop IndexableAt: (dataIndexAt: i) IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing (wired-in, raw oops)\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         hardwiredSlotNameOopInMap: mapOop At: i IfFail: fb = ( |
            | maps mapMap for: mapOop IndexableAt: (nameIndexAt: i) IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing (wired-in, raw oops)\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         hardwiredSlotTypeOopInMap: mapOop At: i IfFail: fb = ( |
            | maps mapMap for: mapOop IndexableAt: (typeIndexAt: i) IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
             r.
            | 
            r: mapType hash ^^ (reflect: annotation) hash.
            size do: [|:i| r:  r ^^ (hashAt: i)].
            r).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: comparing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         hashAt: i = ( |
            | 
            "Optimization: use _IdentityHash instead of getting a
             mirror and getting its hash."
               (      nameAt: i) hash
            ^^ (      typeAt: i) hash
            ^^ (      dataAt: i) _IdentityHash
            ^^ (annotationAt: i) _IdentityHash).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: importing particular types of objects\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         importByteVector: oop IfFail: fb = ( |
            | 
            maps byteVectorMap bytesFrom: oop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: importing particular types of objects\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         importFloat: oop IfFail: fb = ( |
            | 
            layouts float valueOf: oop).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: importing particular types of objects\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         importImmediate: oop IfFail: fb = ( |
            | 
            layouts object if: oop
                  IsImmediate: [|:layout| layout valueOf: oop]
                       IsMark: [error: 'mark']
                        IsMem: [error: 'mem' ]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         importLocalMapFor: oop IfFail: fb = ( |
            | 
            mapOf: oop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         importMapFor: oop IfFail: fb = ( |
            | 
            theVM lens importMapFor: oop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: importing particular types of objects\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         importSmi: oop IfFail: fb = ( |
            | 
            layouts smi valueOf: oop).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: importing particular types of objects\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         importString: oop IfFail: fb = ( |
            | 
            maps stringMap bytesFrom: oop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: importing particular types of objects\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         importVector: oop IfFail: fb = ( |
            | maps objVectorMap importVectorOop: oop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: importing particular types of objects\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         importVectorOfImmediates: oop IfFail: fb = ( |
            | 
            (importVector: oop IfFail: [|:e| ^ fb value: e])
                copyMappedBy: [|:immOop| importImmediate: immOop IfFail: [|:e| ^ fb value: e]]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: indices\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         indexOfSlotDescOopWithOffset: offset At: i = ( |
            | 
            scalarValueCount + ((i * slotDescSize) + offset)).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         indexOfSlotNamed: n IfAbsent: blk = ( |
            | 
            indexOfSlotNamed: n IfPresent: [|:i| i] IfAbsent: blk).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         indexOfSlotNamed: n IfPresent: pBlk IfAbsent: aBlk = ( |
            | 
            size do: [|:i|
              n = (nameAt: i) ifTrue: [^ pBlk value: i].
            ].
            aBlk value: 'absent').
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: installing new nmethods\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         installNewNMethod: newNM = ( |
             newKey.
             oldNMC.
            | 
            newKey: newNM lookupKey.
            oldNMC: _NMethodCache.
            oldNMC findFirst: [|:nm. :i| nm lookupKey = newKey]
                   IfPresent: [|:nm. :i| oldNMC at: i Put: newNM]
                    IfAbsent: [_NMethodCache: oldNMC copyAddLast: newNM].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isActivation = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isArgumentSlotAt: i = ( |
            | 
            vmKit slotType isArgumentSlot: typeAt: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignableAt: i = ( |
            | 
            vmKit slotType isAssignable: typeAt: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignment = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignmentSlotAt: i = ( |
            | 
            vmKit slotType isAssignmentSlot: typeAt: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isBlock = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isBlockMethod = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isBlockMethodActivation = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isBlockMethodMap = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isByteVector = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isConstantAt: i = ( |
            | 
            (isAssignableAt: i) not).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isFctProxy = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isFloat = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isImmediate = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isImmutable = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isInteger = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isMapSlotAt: i = ( |
            | 
            vmKit slotType isMapSlot: typeAt: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isMethod = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isMethodActivation = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isMethodLike = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isMirror = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isNMethod = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isObjectSlotAt: i = ( |
            | 
            vmKit slotType isObjectSlot: typeAt: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isOuterMethod = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isOuterMethodMap = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isParentAt: i = ( |
            | 
            vmKit slotType isParent: typeAt: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isProcess = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isProfiler = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isProgrammableSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isProxy = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fCategory: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeMethod = ( |
            | 
            isReflecteeVMKitActivationMap).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fCategory: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeVMKitActivationMap = ( |
            | 
            isActivation).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: testing slot type\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlotArgumentNamed: n IfAbsent: blk = ( |
            | 
            doesIndexOfSlotNamed: n Satisfy: [|:i| isArgumentSlotAt: i] IfAbsent: blk).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: testing slot type\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlotAssignableNamed: n IfAbsent: blk = ( |
            | 
            doesIndexOfSlotNamed: n Satisfy: [|:i| isAssignableAt: i] IfAbsent: blk).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: testing slot type\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlotAssignmentNamed: n IfAbsent: blk = ( |
            | 
            doesIndexOfSlotNamed: n Satisfy: [|:i| isAssignmentSlotAt: i] IfAbsent: blk).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: testing slot type\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlotParentNamed: n IfAbsent: blk = ( |
            | 
            doesIndexOfSlotNamed: n Satisfy: [|:i| isParentAt: i] IfAbsent: blk).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isString = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isVMSlotAt: i = ( |
            | 
            isVMSlotName: (nameAt: i)).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: VM slots\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isVMSlotName: n = ( |
            | 
            n first = vmSlotNamePrefix).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isVector = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isVectorish = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isVframe = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mapDataOopOfSlotNamed: n IfAbsent: fb = ( |
            | 
            dataAt: indexOfSlotNamed: n IfAbsent: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mapProtoForMemMapOop: mapOop IfFail: fb = ( |
             mt.
            | 
            mt: mapTypeOfMap: mapOop IfFail: [|:e| ^ fb value: e].
            getMapProtoForMapType: mt).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mapSlotsDo: blk = ( |
            | 
            slotsSatisfying: [|:i| isMapSlotAt: i] Do: blk).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing scalar values\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mapType = ( |
            | basicAt: mapTypeIndex).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing scalar values\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mapType: mt = ( |
            | basicAt: mapTypeIndex Put: mt).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing scalar values\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mapTypeAddressOf: mapOop IfFail: fb = ( |
            | 
            [todo cleanup dave. "Just used for import, should be elsewhere.
            Also could use for:IndexableAt:IfFail: and just return the oop -- dmu 7/03"
            "Also see nmethodCacheForImportingMap:IfFail:"
            ].
            maps mapMap for: mapOop AddressOfIndexableAt: mapTypeIndex IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing scalar values\x7fCategory: indices\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot'
        
         mapTypeIndex = 0.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mapTypeOfLocalMap: m IfFail: fb = ( |
            | 
            m mapType).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         mapTypeOfMap: mapOop IfFail: fb = ( |
            | 
            theVM lens mapTypeOfMap: mapOop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: importing objects (for mirror)\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorType = ( |
            | 
            mapType copySize: mapType size - 3 "map").
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         myLayout = ( |
            | 
            layouts abstract).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         nameAt: i = ( |
            | basicAt: nameIndexAt: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: names\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         nameAt: i IfFail: fb = ( |
            | 
            (0 <= i) && [i < size] ifFalse: [^ fb value: 'invalid index'].
            nameAt: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: indices\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         nameIndexAt: i = ( |
            | 
            indexOfSlotDescOopWithOffset: nameOffset At: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: layout constants\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         nameOffset = 0.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: names\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         namesIfFail: fb = ( |
             r.
            | 
            r: list copyRemoveAll.
            size do: [|:i. n|
              n: nameAt: i.
              (isVMSlotName: n) ifFalse: [r addLast: n].
            ].
            r asVector).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing scalar values\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodCache = ( |
            | basicAt: nmethodCacheIndex).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing scalar values\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodCache: nmc = ( |
            | basicAt: nmethodCacheIndex Put: nmc).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing scalar values\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         nmethodCacheAddressOf: mapOop IfFail: fb = ( |
            | maps mapMap for: mapOop AddressOfIndexableAt: nmethodCacheIndex IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing scalar values\x7fCategory: indices\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot'
        
         nmethodCacheIndex = 1.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: special fields\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodCacheOf: o IfFail: fb = ( |
            | 
            nmethodCache).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         objectSlotsDo: blk = ( |
            | 
            slotsSatisfying: [|:i| isObjectSlotAt: i] Do: blk).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         offsetOfObjectSlotNamed: n IfAbsent: blk = ( |
             i.
            | 
            i: indexOfSlotNamed: n IfAbsent: [|:e| ^ blk value: e].
            (vmKit slotType isObjectSlot: typeAt: i) ifFalse: [^ blk value: 'not an object slot'].
            dataAt: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         parentSlotsDo: blk = ( |
            | 
            slotsSatisfying: [|:i| isParentAt: i] Do: blk).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fCategory: iterating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         parentsDo: blk = ( |
            | 
            parentSlotsDo: [|:n. :d. :a. :i|
              blk value: slot copyForMap: self Index: i.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         parentsOf: oop Do: blk = ( |
            | 
            parentSlotsDo: [|:n. :d. :a. :i|
              blk value:
                contentsOfSlotWithType: (typeAt: i)
                        ContainingData: d
                                    In: oop
                                IfFail: raiseError.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: printing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         printStringAt: i = ( |
            | 
            'sd[', (statePrintStringAt: i), ']').
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing scalar values\x7fCategory: indices\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot'
        
         scalarValueCount = 2.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         setContentsOfSlotNamed: n In: oop To: newValueOop IfFail: fb = ( |
             i.
            | 
            i: indexOfSlotNamed: n IfAbsent: [|:e| ^ fb value: e].

            (isMapSlotAt: i)
              ifTrue: [at: i PutData: newValueOop]
               False: [layouts memoryObject for: oop 
                                             At: (dataAt: i)
                                            Put: newValueOop
                                         IfFail: fb].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         size = ( |
            | 
            (basicSize - scalarValueCount) / slotDescSize).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fCategory: accessing slots\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         slot = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps map parent slot.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         holderMap.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         index <- 0.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps map parent slot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         = s = ( |
            | 
            "Hmm. I hope we don't have to be able to compare
             against normal slots."
            (holderMap == s holderMap) && [index = s index]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         annotation = ( |
            | 
            holderMap annotationAt: index).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         category = ( |
            | 
            parsedAnnotation category).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         contents = ( |
            | 
            [isMapSlot] assert.
            reflect: data).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForMap: m Index: i = ( |
            | 
            (copy holderMap: m) index: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         data = ( |
            | 
            holderMap dataAt: index).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         exists = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            holderMap identityHash ^^ index hash).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         holder = ( |
            | 
            "Maybe this isn't a good idea. But it seems to work OK - maps mostly
             don't have too much trouble pretending to be mirrors. -- Adam, May 2009"
            holderMap).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isArgument = ( |
            | 
            vmKit slotType isArgumentSlot: type).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignable = ( |
            | 
            vmKit slotType isAssignable: type).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignment = ( |
            | 
            vmKit slotType isAssignmentSlot: type).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isMapSlot = ( |
            | 
            vmKit slotType isMapSlot: type).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isMethod = ( |
            | 
            isMapSlot && [contents isReflecteeMethod]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isObjectSlot = ( |
            | 
            vmKit slotType isObjectSlot: type).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isParent = ( |
            | 
            vmKit slotType isParent: type).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinCompilerPrototypeForMe = ( |
            | 
            klein compiler1).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         module = ( |
            | 
            parsedAnnotation moduleInfo module).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         name = ( |
            | 
            holderMap nameAt: index).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parsedAnnotation = ( |
            | 
            slotAnnotation parse: annotation).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         type = ( |
            | 
            holderMap typeAt: index).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> 'slot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            holderMap vmKit).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fCategory: accessing slots\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         slotAt: slotName IfAbsent: ab = ( |
             i.
            | 
            i: indexOfSlotNamed: slotName IfAbsent: [^ ab value].
            slot copyForMap: self Index: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: layout constants\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         slotDescSize = 4.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         slotsSatisfying: filterBlock Do: blk = ( |
            | 
            size do: [|:i|
              (filterBlock value: i) ifTrue: [
                blk value: (nameAt: i) 
                     With: (dataAt: i)
                     With: (annotationAt: i)
                     With: i
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
             r <- ''.
            | 
            size do: [|:i| r: r & (printStringAt: i) & '. '].
            r flatString).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: printing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintStringAt: i = ( |
            | 
            (typeAt: i) ifNil: [^ ''].  "Should not be true normally, but can cause problems during debugging."
            ((isArgumentSlotAt: i) ifTrue: ':' False: ''),
            (nameAt: i),
            ((isParentAt: i) ifTrue: '*' False: ''),
            ((isObjectSlotAt: i) &&  [(dataAt: i) isNil]
              ifTrue: ''
               False: [
                ' ',
                ((isAssignableAt: i) ifTrue: '<- ' False: '= '),
                ((isMapSlotAt: i) ifTrue: '' False: '*'),
                (reflect: dataAt: i) name ]
            )).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         storeStringIfFail: fb = ( |
            | 
            fb value: 'Although copied from vector, maps do not do storeString').
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         typeAt: i = ( |
            | basicAt: typeIndexAt: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: indices\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         typeIndexAt: i = ( |
            | 
            indexOfSlotDescOopWithOffset: typeOffset At: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: slotDescs and basic info\x7fCategory: layout constants\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         typeOffset = 1.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         verifyEqualToImportedMap: importedMap = ( |
            | 
            [size    = importedMap size   ] assert.
            [mapType = importedMap mapType] assert.
            size do: [|:i|
              [(      nameAt: i) = (importedMap       nameAt: i)] assert.
              [(      typeAt: i) = (importedMap       typeAt: i)] assert.
              [(annotationAt: i) = (importedMap annotationAt: i)] assert.
              (isMapSlotAt: i) ifFalse: [
                [(    dataAt: i) = (importedMap       dataAt: i)] assert.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: creating maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapMapForConversion = ( |
            | 
            vmKit maps mapMap).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: VM slots\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmSlotNameFor: n = ( |
            | 
            (vmSlotNamePrefix, n) canonicalize).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: VM slots\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         vmSlotNamePrefix = '_'.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps slotsMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         myLayout = ( |
            | 
            layouts memoryObject).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         map = [ | x =  ( bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> () From: ( |
                     {} = 'ModuleInfo: Creator: globals kleinAndYoda maps map.
\x7fIsComplete: '.
                    | ) ) _Clone: 2 Filler: 0| 
             x _At: 0  Put: ().
             x _At: 1  Put: ().
             x] value.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> () From: ( | {
         'Category: map state\x7fModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (\'\')'
        
         annotation <- ''.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isVMKitMap = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         slotsMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps map copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps slotsMap.

CopyDowns:
globals kleinAndYoda maps map. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         slotsMapDeps = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps slotsMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps slotsMapDeps.

CopyDowns:
globals kleinAndYoda maps slotsMap. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isProgrammableSlots = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps slotsMapDeps parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         areSlotsMutable = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorType = 'slotsWithDeps'.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: object or byte vectors\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         abstractVectorMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps slotsMapDeps copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps abstractVectorMap.

CopyDowns:
globals kleinAndYoda maps slotsMapDeps. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> () _At: 0 Put: (
     'abstractVectorMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps abstractVectorMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: cloning\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         clone: o IndexableSize: size FillingWith: filler IfFail: fb = ( |
            | 
            size < 0  ifTrue: [^ fb value: 'indexable size must be non-negative'].
            clone: o UncheckedIndexableSize: size FillingWith: filler IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: cloning\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         clone: o UncheckedIndexableSize: size FillingWith: filler IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o AddressOfIndexableAt: i = ( |
            | for: o AddressOfIndexableAt: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o AddressOfIndexableAt: i IfFail: fb = ( |
            | 
            myLayout for: o AddressOfIndexableAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i = ( |
            | for: o IndexableAt: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i Put: x = ( |
            | for: o IndexableAt: i Put: x IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i Put: x IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: accessing indexable size field\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeOf: o = ( |
            | 
            indexableSizeOf: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: accessing indexable size field\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeOf: o IfFail: fb = ( |
            | 
            myLayout indexableSizeOf: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isVectorish = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: object or byte vectors\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         objVectorMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps abstractVectorMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps objVectorMap.

CopyDowns:
globals kleinAndYoda maps abstractVectorMap. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps objVectorMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: cloning\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         clone: o UncheckedIndexableSize: size FillingWith: filler IfFail: fb = ( |
             origin.
            | 
            origin: myLayout indexableOriginOf: o IfFail: [|:e| ^ fb value: e].

            clone: o Size: origin + size FillingWith: filler IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: forwarded to layout\x7fCategory: accessing indexables\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i IfFail: fb = ( |
            | 
            myLayout for: o IndexableAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: forwarded to layout\x7fCategory: accessing indexables\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i Put: x IfFail: fb = ( |
            | 
            myLayout for: o IndexableAt: i Put: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: forwarded to layout\x7fCategory: iterating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexablesDo: blk IfFail: fb = ( |
            | 
            myLayout for: o IndexablesDo: blk IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: forwarded to layout\x7fCategory: iterating\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o PopulateIndexablesBy: blk = ( |
            | 
            myLayout for: o PopulateIndexablesBy: blk).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: caching nmethods\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         for: map SetNMethodCache: nmc = ( |
            | 
            for: map IndexableAt: nmethodCacheIndex Put: nmc).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: importing objects (mirror)\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         importVectorOop: oop IfFail: fb = ( |
             r.
            | 
            r: vector copySize: myLayout indexableSizeOf: oop.
            for: oop IndexablesDo: [|:o. :i| r at: i Put: o] IfFail: [|:e| ^ fb value: e].
            r).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isVector = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: importing objects (mirror)\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorType = 'vector'.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         myLayout = ( |
            | 
            layouts objVector).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: caching nmethods\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodCacheOf: map = ( |
            | 
            for: map IndexableAt: nmethodCacheIndex).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: code slots-maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         codeSlotsMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeSlotsMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps objVectorMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeSlotsMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps codeSlotsMap.

CopyDowns:
globals kleinAndYoda maps objVectorMap. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeSlotsMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeSlotsMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps codeSlotsMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeSlotsMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeSlotsMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeSlotsMap') -> 'parent' -> () From: ( | {
         'Category: mapping objects (for export)\x7fCategory: adding slots\x7fCategory: counting words\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         wordsNeededForIndexableSlotsIn: mir = ( |
            | 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: code slots-maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         activationMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps codeSlotsMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps activationMap.

CopyDowns:
globals kleinAndYoda maps codeSlotsMap. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () _At: 0 Put: (
     'activationMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () From: ( | {
         'Category: map state\x7fModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (0)'
        
         activationPartSizes <- 0.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedInitialContentsBySlotIndex.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedTransmogrifier.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () From: ( | {
         'Category: map state\x7fModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (\'\')'
        
         codes <- ''.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () From: ( | {
         'Category: map state\x7fModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (\'\')'
        
         file <- ''.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isVMKitActivationMap = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () From: ( | {
         'Category: map state\x7fModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (0)'
        
         line <- 0.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () From: ( | {
         'Category: map state\x7fModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (vector)'
        
         literals <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps activationMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         = aMap = ( |
            | 
               (resend.= aMap)
            && [(sourceString = aMap sourceString)
            && [(file = aMap file)
            && [(line = aMap line)
            && [((reflect:   literals) = (reflect: aMap   literals))
            && [ (reflect:   codes   ) = (reflect: aMap   codes   ) ]]]]]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         activationMapIfFail: fb = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: activation part sizes\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         activationPartSizesLayout = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps activationMap parent activationPartSizesLayout.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         argCountField = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'argCountField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps activationMap parent activationPartSizesLayout argCountField.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'argCountField' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'argCountField' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> 'noPrecedingField' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'argCountField' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         width = 8.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         assignableLocalCountField = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'assignableLocalCountField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps activationMap parent activationPartSizesLayout assignableLocalCountField.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'assignableLocalCountField' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'assignableLocalCountField' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'argCountField' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'assignableLocalCountField' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         width = 8.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         maxStackSizeField = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'maxStackSizeField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps activationMap parent activationPartSizesLayout maxStackSizeField.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         lastField = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'maxStackSizeField' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'maxStackSizeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'maxStackSizeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'assignableLocalCountField' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> 'maxStackSizeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         width = 8.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            formatString).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'activationPartSizesLayout' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         wordLayoutMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         allSlotsOnThisMethod = ( |
            | slotObjectsForSlotsSatisfying: true).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         areSlotsMutable = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: activation part sizes\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         argCount = ( |
            | 
            activationPartSizesLayout argCountField valueOfWord: activationPartSizes).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: activation part sizes\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         argCount: n = ( |
            | 
            activationPartSizes: activationPartSizesLayout argCountField setValueOfWord: activationPartSizes To: n).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         argOrLocalFakeSlot = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps activationMap parent argOrLocalFakeSlot.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         activationMap.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (-1)\x7fVisibility: private'
        
         index <- -1.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         mirror.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (\'\')\x7fVisibility: private'
        
         name <- ''.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps activationMap parent argOrLocalFakeSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         actualValueIfFail: fb = ( |
            | 
            isAssignment ifTrue: [^ mirrors assignment].

            mirror reflectionPrimitives
                          forMethodMirror: mirror
                 PrimitiveContentsAtIndex: index
                                   IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForMirror: m Name: n Index: i = ( |
            | 
            (((copy        mirror: m)
                    activationMap: m activationMapIfFail: raiseError)
                             name: n)
                            index: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         exists = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isArgument = ( |
            | 
            vmKit slotType isArgumentSlot: type).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignable = ( |
            | 
            vmKit slotType isAssignable: type).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignment = ( |
            | 
            vmKit slotType isAssignmentSlot: type).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'fakeSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         type = ( |
            | 
            activationMap typeAt: index).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'argOrLocalFakeSlot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            activationMap vmKit).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentSlots = ( |
            | 
            slotObjectsForSlotsSatisfying: [|:i| isArgumentSlotAt: i]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: activation part sizes\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         assignableLocalCount = ( |
            | 
            activationPartSizesLayout assignableLocalCountField valueOfWord: activationPartSizes).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: activation part sizes\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         assignableLocalCount: n = ( |
            | 
            activationPartSizes: activationPartSizesLayout assignableLocalCountField setValueOfWord: activationPartSizes To: n).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         assignableLocalSlots = ( |
            | 
            slotObjectsForSlotsSatisfying: [|:i| isAssignableAt: i]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         blockLiteralsDo: b = ( |
            | 
            literals do: [|:lit. :i|
              (reflect: lit) isReflecteeBlock ifTrue: [ b value: lit With: i ]
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m PrimitiveContentsAtIndex: i IfFail: fb = ( |
            | 
            [m == self] assert.
            initialContentsAt: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         hasBlocks = ( |
            | 
            blockLiteralsDo: [^ true].
            false).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            resend.hash
            ^^           sourceString  hash
            ^^           file          hash
            ^^           line          hash
            ^^ (reflect: literals    ) hash
            ^^ (reflect: codes       ) hash).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         indexOfLiteral: lit IfFail: fb = ( |
             litMir.
            | 
            litMir: reflect: lit.
            literals findFirst: [|:x| litMir = (reflect: x)]
                     IfPresent: [|:x. :i| i]
                      IfAbsent: [fb value: 'literal not found']).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         indexOfLocalNamed: n IfFail: fb = ( |
            | 
            size do: [|:i|
              (nameAt: i) = n ifTrue: [
                (isMapSlotAt: i) ifTrue: [^ fb value: 'The local named ', n, ' is stored in the methodMap. It does not have an object offset.'].
                ^ dataAt: i
              ].
            ].
            fb value: 'No local named ', n, ' was found.').
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: initial contents\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         initialContentsAt: i = ( |
             d.
            | 
            d: dataAt: i.
            (isMapSlotAt: i) ifTrue: [reflect: d]
                              False: [initialContentsBySlotIndex at: d IfAbsent: [reflect: nil]]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: initial contents\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         initialContentsBySlotIndex = ( |
            | 
            cachedInitialContentsBySlotIndex ifNil: [
              cachedInitialContentsBySlotIndex: initialContentsInterpreter copyInterpretMethod: self.
              cachedInitialContentsBySlotIndex
            ]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: initial contents\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         initialContentsInterpreter = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals abstractBytecodeInterpreter copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps activationMap parent initialContentsInterpreter.

CopyDowns:
globals abstractBytecodeInterpreter. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isActivation = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isMethodLike = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         lexicalParents = ( |
             r.
            | 
            r: list copyRemoveAll.
            lexicalParentsDo: [|:lp| r addLast: lp ].
            r).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         literalsMirrorIfFail: fb = ( |
            | 
            reflect: literals).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         localReadSelectorLexicalLevel: ll Index: i = ( |
             m.
            | 
            m: self.
            ll do: [m: m lexicalParent].
            (m slotForArgumentOrAssignableLocalWithOffset: i) name).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         localSlotLexicalLevel: ll Index: i = ( |
             m.
            | 
            m: self.
            ll do: [m: m lexicalParent].
            m slotForArgumentOrAssignableLocalWithOffset: i).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         localSlots = ( |
            | 
            slotObjectsForSlotsSatisfying: [|:i| (isArgumentSlotAt: i) not && [(isAssignmentSlotAt: i) not]]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         localWriteSelectorLexicalLevel: ll Index: i = ( |
            | 
            (localReadSelectorLexicalLevel: ll Index: i), ':').
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: activation part sizes\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         maxStackSize = ( |
            | 
            activationPartSizesLayout maxStackSizeField valueOfWord: activationPartSizes).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: activation part sizes\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         maxStackSize: n = ( |
            | 
            activationPartSizes: activationPartSizesLayout maxStackSizeField setValueOfWord: activationPartSizes To: n).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeSlotsMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         reflectionPrimitives = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         slotForArgumentOrAssignableLocalWithOffset: offset = ( |
            | 
            slotsSatisfying: [|:i| (isAssignableAt: i) || [isArgumentSlotAt: i]] Do: [|:n. :d. :a. :i|
              d = offset ifTrue: [^ argOrLocalFakeSlot copyForMirror: self Name: n Index: i].
            ].
            error: 'no slot found in ', objectID, ' with offset ', offset printString).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         slotNamesWithSPOffsetRecorded = ( |
             ns.
            | 
            ns: list copyRemoveAll.
            size do: [|:i|
              (klein nmethod isSPOffsetRecordedForSlotOfType: typeAt: i) ifTrue: [
                ns add: nameAt: i.
              ].
            ].
            ns).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         slotObjectsForSlotsSatisfying: blk = ( |
             r.
            | 
            r: list copyRemoveAll.
            slotsSatisfying: blk Do: [|:n. :d. :a. :i|
              r add: argOrLocalFakeSlot copyForMirror: self Name: n Index: i.
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> () From: ( | {
         'Category: map state\x7fModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (\'\')'
        
         sourceString <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: object or byte vectors\x7fCategory: maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mapMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mapMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps objVectorMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mapMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps mapMap.

CopyDowns:
globals kleinAndYoda maps objVectorMap. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mapMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mapMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps mapMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mapMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: object or byte vectors\x7fCategory: maps\x7fCategory: activation maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         activationMapMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMapMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps mapMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMapMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps activationMapMap.

CopyDowns:
globals kleinAndYoda maps mapMap. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMapMap') -> () _At: 0 Put: (
     'activationMapMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMapMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMapMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMapMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps activationMapMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMapMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mapMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: code-like slots maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         codeLikeSlotsMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeLikeSlotsMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps slotsMapDeps copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeLikeSlotsMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps codeLikeSlotsMap.

CopyDowns:
globals kleinAndYoda maps slotsMapDeps. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeLikeSlotsMap') -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         areSlotsMutable = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeLikeSlotsMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeLikeSlotsMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps codeLikeSlotsMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeLikeSlotsMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isMethodLike = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeLikeSlotsMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeLikeSlotsMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: code-like slots maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         assignmentMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'assignmentMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps codeLikeSlotsMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'assignmentMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps assignmentMap.

CopyDowns:
globals kleinAndYoda maps codeLikeSlotsMap. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'assignmentMap') -> () _At: 0 Put: (
     'assignmentMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'assignmentMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'assignmentMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'assignmentMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps assignmentMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'assignmentMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         areSlotsMutable = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'assignmentMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignment = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'assignmentMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeLikeSlotsMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: code slots-maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         blockActivationMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps activationMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps blockActivationMap.

CopyDowns:
globals kleinAndYoda maps activationMap. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> () _At: 0 Put: (
     'blockActivationMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> () From: ( | {
         'Category: map state\x7fModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (nil)'
        
         lexicalParent.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps blockActivationMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isBlockMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeBlockMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         lexicalParentsDo: blk = ( |
             lp.
            | 
            lp: lexicalParent.
            blk value: lp With: self.
            lp lexicalParentsDo: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> 'parent' -> () From: ( | {
         'Category: creating maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapMapForConversion = ( |
            | 
            vmKit maps blockActivationMapMap).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> () From: ( | {
         'Category: map state\x7fModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (0)'
        
         sourceLength <- 0.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> () From: ( | {
         'Category: map state\x7fModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (0)'
        
         sourceOffset <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: object or byte vectors\x7fCategory: maps\x7fCategory: activation maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         blockActivationMapMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMapMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps activationMapMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMapMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps blockActivationMapMap.

CopyDowns:
globals kleinAndYoda maps activationMapMap. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMapMap') -> () _At: 0 Put: (
     'blockActivationMapMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMapMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMapMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMapMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps blockActivationMapMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMapMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMapMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         blockMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps slotsMapDeps copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps blockMap.

CopyDowns:
globals kleinAndYoda maps slotsMapDeps. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> () _At: 0 Put: (
     'blockMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> () From: ( | {
         'Category: map state\x7fModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (0)'
        
         cloneCount <- 0.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps blockMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isBlock = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         myLayout = ( |
            | 
            layouts block).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         valueSlotName = ( |
             n.
            | 
            n: nameAt: 1.
            ['value' isPrefixOf: n] assert.
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: code-like slots maps\x7fCategory: vframes\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vframeMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps slotsMapDeps copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps vframeMap.

CopyDowns:
globals kleinAndYoda maps slotsMapDeps. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps vframeMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         areSlotsMutable = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isActivation = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isProgrammableSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isVframe = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: code-like slots maps\x7fCategory: vframes\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         bvframeMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'bvframeMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps vframeMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'bvframeMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps bvframeMap.

CopyDowns:
globals kleinAndYoda maps vframeMap. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'bvframeMap') -> () _At: 0 Put: (
     'bvframeMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'bvframeMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'bvframeMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'bvframeMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps bvframeMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'bvframeMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isBlockMethodActivation = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'bvframeMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: object or byte vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         byteVectorMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps abstractVectorMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps byteVectorMap.

CopyDowns:
globals kleinAndYoda maps abstractVectorMap. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> () _At: 0 Put: (
     'byteVectorMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps byteVectorMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> 'parent' -> () From: ( | {
         'Category: importing objects (mirrors)\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesFrom: o IfFail: fb = ( |
            | 
            myLayout bytesFrom: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> 'parent' -> () From: ( | {
         'Category: forwarded to layout\x7fCategory: accessing indexables\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i IfFail: fb = ( |
            | 
            myLayout for: o IndexableAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> 'parent' -> () From: ( | {
         'Category: forwarded to layout\x7fCategory: accessing indexables\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i Put: x IfFail: fb = ( |
            | 
            myLayout for: o IndexableAt: i Put: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isByteVector = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         myLayout = ( |
            | 
            theVM byteVectorLayout).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> ().
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeLikeSlotsMap') -> () _At: 0 Put: (
     'codeLikeSlotsMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeLikeSlotsMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeSlotsMap') -> () _At: 0 Put: (
     'codeSlotsMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'codeSlotsMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'foreignMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'foreignMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps foreignMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'foreignMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'foreignMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: foreign maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'foreignMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps slotsMapDeps copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'foreignMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps foreignMap.

CopyDowns:
globals kleinAndYoda maps slotsMapDeps. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: foreign maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         proxyMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'proxyMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps foreignMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'proxyMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps proxyMap.

CopyDowns:
globals kleinAndYoda maps foreignMap. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'proxyMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'proxyMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps proxyMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'proxyMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isProxy = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'proxyMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'foreignMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: foreign maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         fctProxyMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'fctProxyMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps proxyMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'fctProxyMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps fctProxyMap.

CopyDowns:
globals kleinAndYoda maps proxyMap. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'fctProxyMap') -> () _At: 0 Put: (
     'fctProxyMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'fctProxyMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'fctProxyMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'fctProxyMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps fctProxyMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'fctProxyMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isFctProxy = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'fctProxyMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'proxyMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: immediate maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         immediateMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps map copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps immediateMap.

CopyDowns:
globals kleinAndYoda maps map. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps immediateMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> 'parent' -> () From: ( | {
         'Category: cloning\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         clone: o IfFail: fb = ( |
            | o).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isImmediate = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isImmutable = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         myLayout = ( |
            | 
            layouts immediate).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: immediate maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         floatMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps immediateMap copySizeOne ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps floatMap.

CopyDowns:
globals kleinAndYoda maps immediateMap. copySizeOne 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> () _At: 0 Put: (
     'floatMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> () _At: 2 Put: (
     'parent')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> () _At: 3 Put: (
     5)

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> () _At: 4 Put: (
     bootstrap stub -> 'traits' -> 'float' -> ())

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> () _At: 5 Put: (
     '')

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps floatMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isFloat = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         myLayout = ( |
            | 
            layouts float).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         recordYourselfInTheUniverse = ( |
            | 
            theVM universe floatMap: self).
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'foreignMap') -> () _At: 0 Put: (
     'foreignMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'foreignMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> () _At: 0 Put: (
     'immediateMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> () _At: 0 Put: (
     'map')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mapMap') -> () _At: 0 Put: (
     'mapMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mapMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mirrorMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps slotsMapDeps copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mirrorMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps mirrorMap.

CopyDowns:
globals kleinAndYoda maps slotsMapDeps. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mirrorMap') -> () _At: 0 Put: (
     'mirrorMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mirrorMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mirrorMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mirrorMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps mirrorMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mirrorMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isMirror = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mirrorMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mirrorMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: object or byte vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'nmethodMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps byteVectorMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'nmethodMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps nmethodMap.

CopyDowns:
globals kleinAndYoda maps byteVectorMap. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'nmethodMap') -> () _At: 0 Put: (
     'nmethodMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'nmethodMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'nmethodMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'nmethodMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps nmethodMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'nmethodMap') -> 'parent' -> () From: ( | {
         'Category: forwarded to layout\x7fCategory: accessing entry address\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         entryAddressOf: o = ( |
            | 
            entryAddressOf: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'nmethodMap') -> 'parent' -> () From: ( | {
         'Category: forwarded to layout\x7fCategory: accessing entry address\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         entryAddressOf: o IfFail: fb = ( |
            | 
            myLayout for: o AddressOfIndexableAt: firstInstructionIndex IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'nmethodMap') -> 'parent' -> () From: ( | {
         'Category: layout constants\x7fComment: Index of first instruction in byte array.\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         firstInstructionIndex = 0.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'nmethodMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isNMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'nmethodMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> 'parent' -> ().
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> () _At: 0 Put: (
     'objVectorMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> 'assignableFakeSlotForMethodPointer' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> 'assignableFakeSlotForMethodPointer' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps objVectorMap parent assignableFakeSlotForMethodPointer parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> 'assignableFakeSlotForMethodPointer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignable = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> 'assignableFakeSlotForMethodPointer' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (traits fakeSlot methodPointer)\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'fakeSlot' -> 'methodPointer' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: code slots-maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         outerActivationMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps activationMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps outerActivationMap.

CopyDowns:
globals kleinAndYoda maps activationMap. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMap') -> () _At: 0 Put: (
     'outerActivationMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps outerActivationMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isOuterMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeBlockMethod = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMap') -> 'parent' -> () From: ( | {
         'Category: pretending to be a mirror\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         lexicalParentsDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMap') -> 'parent' -> () From: ( | {
         'Category: creating maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapMapForConversion = ( |
            | 
            vmKit maps outerActivationMapMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: object or byte vectors\x7fCategory: maps\x7fCategory: activation maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         outerActivationMapMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMapMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps activationMapMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMapMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps outerActivationMapMap.

CopyDowns:
globals kleinAndYoda maps activationMapMap. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMapMap') -> () _At: 0 Put: (
     'outerActivationMapMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMapMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMapMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMapMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps outerActivationMapMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMapMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMapMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: code-like slots maps\x7fCategory: vframes\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         ovframeMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'ovframeMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps vframeMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'ovframeMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps ovframeMap.

CopyDowns:
globals kleinAndYoda maps vframeMap. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'ovframeMap') -> () _At: 0 Put: (
     'ovframeMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'ovframeMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'ovframeMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'ovframeMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps ovframeMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'ovframeMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isMethodActivation = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'ovframeMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         processMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'processMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps slotsMapDeps copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'processMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps processMap.

CopyDowns:
globals kleinAndYoda maps slotsMapDeps. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'processMap') -> () _At: 0 Put: (
     'processMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'processMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'processMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'processMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps processMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'processMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isProcess = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'processMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'processMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         profilerMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'profilerMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps slotsMapDeps copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'profilerMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps profilerMap.

CopyDowns:
globals kleinAndYoda maps slotsMapDeps. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'profilerMap') -> () _At: 0 Put: (
     'profilerMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'profilerMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'profilerMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'profilerMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps profilerMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'profilerMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isProfiler = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'profilerMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> 'parent' -> ().
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'proxyMap') -> () _At: 0 Put: (
     'proxyMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'proxyMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> () _At: 0 Put: (
     'slotsMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> () _At: 0 Put: (
     'slotsMapDeps')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMapDeps') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: immediate maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         smiMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps immediateMap copySizeOne ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps smiMap.

CopyDowns:
globals kleinAndYoda maps immediateMap. copySizeOne 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> () _At: 0 Put: (
     'smiMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> () _At: 2 Put: (
     'parent')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> () _At: 3 Put: (
     5)

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> () _At: 4 Put: (
     bootstrap stub -> 'traits' -> 'smallInt' -> ())

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> () _At: 5 Put: (
     '')

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps smiMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isInteger = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         myLayout = ( |
            | 
            layouts smi).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'immediateMap') -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         recordYourselfInTheUniverse = ( |
            | 
            theVM universe smiMap: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps' -> () From: ( | {
         'Category: slots-maps\x7fCategory: maps with dependencies\x7fCategory: object or byte vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         stringMap = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda maps byteVectorMap copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps stringMap.

CopyDowns:
globals kleinAndYoda maps byteVectorMap. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> () _At: 0 Put: (
     'stringMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps stringMap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         areSlotsMutable = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> 'parent' -> () From: ( | {
         'Category: importing objects (mirrors)\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesFrom: oop IfFail: fb = ( |
            | 
            (resend.bytesFrom: oop IfFail: [|:e| ^ fb value: e]) asString canonicalize).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isProgrammableSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isString = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> 'parent' -> ().
        } | ) 

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> () _At: 0 Put: (
     'vframeMap')

 ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> () _At: 1 Put: (
     ((bootstrap stub -> 'globals') \/-> 'vector') -> ())

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         importMapFor: oop IfFail: fb = ( |
            | vmKit maps map importRemoteMapFor: oop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: maps\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         mapTypeOfMap: mapOop IfFail: fb = ( |
            | vmKit maps map mapTypeOfRemoteMap: mapOop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot'
        
         vmKitMaps = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMaps' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMaps' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitMaps.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMaps' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMaps' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMaps' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMaps' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMaps' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.14 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMaps' -> () From: ( | {
         'ModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitMapImporting
vmKitNMethodCache
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'assignment' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps assignmentMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'block' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps blockMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'blockMethod' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps blockActivationMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'blockMethodActivation' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps bvframeMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'byteVector' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeKleinNMethod = ( |
            | 
            (includesKey: 'isKleinNMethod') && [reflectee isKleinNMethod]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'byteVector' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | 
            isReflecteeKleinNMethod
             ifTrue: [kleinAndYoda maps    nmethodMap]
              False: [kleinAndYoda maps byteVectorMap]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'canonicalString' -> () From: ( | {
         'Category: klein\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps stringMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'fctProxy' -> () From: ( | {
         'Category: klein\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps fctProxyMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'float' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | 
            kleinAndYoda maps floatMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'method' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps outerActivationMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'methodActivation' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps ovframeMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'mirror' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps mirrorMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'process' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps processMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'profiler' -> () From: ( | {
         'Category: klein\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps profilerMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'proxy' -> () From: ( | {
         'Category: klein\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps proxyMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'slots' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | 
            isReflecteeKleinCompiledBlock ifTrue: [^ kleinAndYoda maps blockMap].

            kleinAndYoda maps slotsMapDeps).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'smallInt' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | kleinAndYoda maps smiMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'vector' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeVMKitActivationMap = ( |
            | (includesKey: 'isVMKitActivationMap') && [reflectee isVMKitActivationMap]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'vector' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeVMKitMap = ( |
            | 
            (includesKey: 'isVMKitMap') && [reflectee isVMKitMap]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'vector' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKitMapForConversion = ( |
            | 
            isReflecteeVMKitMap
             ifTrue: [reflectee vmKitMapMapForConversion]
              False: [kleinAndYoda maps objVectorMap]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'slots' -> 'plain' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitMaps InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinAndYodaSlotName = ( |
            | 
            isFake ifFalse: [name]
                      True: [kleinAndYoda maps map vmSlotNameFor: name]).
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitMapImporting' From: 'applications/klein'
 bootstrap read: 'vmKitNMethodCache' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitMaps postFileIn
