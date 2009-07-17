 '$Revision: 30.13 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         doesReflecteeOf: aMirror Eq: x IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         evalNameIfNoStoreStringOrCreatorNameFor: aMirror = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         evaluate: m For: aMirror = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: aMirror PrimitiveCopyAt: n PutContents: objMirr IsParent: p IsArgument: isA Annotation: a IfFail: failBlock = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: aMirror PrimitiveDefine: newObjMir IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         importActivationMapReflecteeOf: aMirror IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorForVM: aVM Obj: o IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         nameSuffixFor: aMirror IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeIdentityHashOf: aMirror = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         fakeSlotsIterator = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda fakeSlotsIterator.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> () From: ( | {
         'Category: fake slots\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         fakeSlot = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda fakeSlotsIterator fakeSlot.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         address = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'address' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda fakeSlotsIterator fakeSlot address.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDownParentForMirrorPrototypes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda mirrors copyDownParentForMirrorPrototypes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'address' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: InitializeToExpression: (kleinAndYoda mirrors copyDownParentForMirrorPrototypes)'
        
         mirror <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'address' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         parent* <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'address' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda fakeSlotsIterator fakeSlot address parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'address' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         actualValueIfFail: failBlock = ( |
            | 
            reflect: mirror reflectionPrimitives reflecteeAddressIfFail: [|:e| ^ failBlock value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'address' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         isApplicable = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'address' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         key = 'address'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'address' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'fakeSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> () From: ( | {
         'Category: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         disassembledCodes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledCodes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda fakeSlotsIterator fakeSlot disassembledCodes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledCodes' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: InitializeToExpression: (kleinAndYoda mirrors copyDownParentForMirrorPrototypes)'
        
         mirror <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledCodes' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         parent* <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledCodes' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda fakeSlotsIterator fakeSlot disassembledCodes parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledCodes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         actualValueIfFail: fb = ( |
            | mirror disassembledCodesIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledCodes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         isApplicable = ( |
            | 
            mirror isKleinOrYodaMirror
            && [mirror isReflecteeMethod]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledCodes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         key = 'disassembledCodes'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledCodes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'fakeSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         map = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'map' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda fakeSlotsIterator fakeSlot map.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'map' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: InitializeToExpression: (kleinAndYoda mirrors copyDownParentForMirrorPrototypes)'
        
         mirror <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'map' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         parent* <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'map' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda fakeSlotsIterator fakeSlot map parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'map' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         actualValueIfFail: failBlock = ( |
            | 
            [reflect: mirror map].
            mirror mapMirrorIfFail: failBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'map' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         isApplicable = ( |
            | 
                 mirror isKleinOrYodaMirror
            && [ mirror reflectionPrimitives isMapSet ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'map' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         key = 'map'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'map' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'fakeSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         nmethodCache = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethodCache' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda fakeSlotsIterator fakeSlot nmethodCache.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethodCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: InitializeToExpression: (kleinAndYoda mirrors copyDownParentForMirrorPrototypes)'
        
         mirror <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethodCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         parent* <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethodCache' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda fakeSlotsIterator fakeSlot nmethodCache parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethodCache' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         actualValueIfFail: failBlock = ( |
            | mirror nmethodCacheIfFail: failBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethodCache' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         isApplicable = ( |
            | 
                 mirror isKleinOrYodaMirror
            && [ mirror reflectionPrimitives isMapSet]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethodCache' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         key = 'nmethodCache'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethodCache' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'fakeSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         oop = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'oop' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda fakeSlotsIterator fakeSlot oop.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'oop' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: InitializeToExpression: (kleinAndYoda mirrors copyDownParentForMirrorPrototypes)'
        
         mirror <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'oop' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         parent* <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'oop' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda fakeSlotsIterator fakeSlot oop parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'fakeSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         method: m DoKleinSlots: b = ( |
            | 
            b value: fakeSlot disassembledCodes copyMirror: m.
            fakeVectorSlots mirror: m Do: b. "Methods in Klein are vectors."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         mirror: m Do: block = ( |
            | 
            block value: fakeSlot map          copyMirror: m.
            block value: fakeSlot oop          copyMirror: m.
            block value: fakeSlot address      copyMirror: m.
            block value: fakeSlot nmethodCache copyMirror: m.
            m isReflecteeMethod     ifTrue: [ method: m DoKleinSlots: block].
            resend.mirror: m Do: block).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         mirror: m OneOfEachDo: b = ( |
            | 
            b value: fakeSlot map copyMirror: m.
            b value: fakeSlot oop copyMirror: m.
            b value: fakeSlot nmethodCache copyMirror: m.
            m isReflecteeMethod  ifTrue: [ method: m DoKleinSlots: b].
            resend.mirror: m OneOfEachDo: b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'fakeSlotsIterator' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         doesReflecteeOf: aMirror Eq: x IfFail: fb = ( |
            | 
            aMirror localReflecteeEq: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         evalNameIfNoStoreStringOrCreatorNameFor: aMirror = ( |
            | 
            aMirror localEvalNameIfNoStoreStringOrCreatorName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         evaluate: m For: aMirror = ( |
            | 
            aMirror evaluateLocal: m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: aMirror PrimitiveCopyAt: n PutContents: objMirr IsParent: p IsArgument: isA Annotation: a IfFail: failBlock = ( |
            | 
            aMirror localPrimitiveCopyAt: n PutContents: objMirr IsParent: p IsArgument: isA Annotation: a IfFail: failBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: aMirror PrimitiveDefine: newObjMir IfFail: fb = ( |
            | 
            aMirror localPrimitiveDefine: newObjMir IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         importActivationMapReflecteeOf: aMirror IfFail: fb = ( |
            | 
            aMirror importLocalReflecteeActivationMapIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorForVM: aVM Obj: o IfFail: fb = ( |
            | 
            aVM localMirrorFor: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         nameSuffixFor: aMirror IfFail: fb = ( |
            | 
            aMirror localNameSuffixIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeIdentityHashOf: aMirror = ( |
            | 
            aMirror localReflecteeIdentityHash).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'abstractVectorMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns childMustImplement).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns activation).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'assignmentMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns assignment).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns blockMethodActivation).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockActivationMapMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns blockMethod).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'blockMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns block).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'bvframeMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns blockMethodActivation).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'byteVectorMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns byteVector).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'fctProxyMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns fctProxy).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'floatMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns float).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'mirrorMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns mirror).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'nmethodMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns nmethod).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'objVectorMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns vector).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns methodActivation).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'outerActivationMapMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns method).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'ovframeMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns methodActivation).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'processMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns process).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'profilerMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns profiler).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'proxyMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns proxy).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'slotsMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns slots).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'smiMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns smallInt).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'stringMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns canonicalString).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'vframeMap') -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            ns activation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         doesReflecteeOf: aMirror Eq: x IfFail: fb = ( |
            | 
            aMirror remoteReflecteeEq: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         evalNameIfNoStoreStringOrCreatorNameFor: aMirror = ( |
            | 
            aMirror remoteEvalNameIfNoStoreStringOrCreatorName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         evaluate: m For: aMirror = ( |
            | 
            aMirror evaluateRemote: m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: aMirror PrimitiveCopyAt: n PutContents: objMirr IsParent: p IsArgument: isA Annotation: a IfFail: failBlock = ( |
            | 
            aMirror remotePrimitiveCopyAt: n PutContents: objMirr IsParent: p IsArgument: isA Annotation: a IfFail: failBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: aMirror PrimitiveDefine: newObjMir IfFail: fb = ( |
            | 
            aMirror remotePrimitiveDefine: newObjMir IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         importActivationMapReflecteeOf: aMirror IfFail: fb = ( |
            | 
            aMirror importRemoteReflecteeActivationMapIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorForVM: aVM Obj: o IfFail: fb = ( |
            | 
            aVM remoteMirrorFor: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         nameSuffixFor: aMirror IfFail: fb = ( |
            | 
            aMirror remoteNameSuffixIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeIdentityHashOf: aMirror = ( |
            | 
            aMirror remoteReflecteeIdentityHash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrors = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda mirrors.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isKleinOrYodaMirror = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         kleinAndYodaSpecific* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> 'kleinAndYodaSpecific' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda mirrors copyDownParentForMirrorPrototypes kleinAndYodaSpecific.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> 'kleinAndYodaSpecific' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForVM: aVM OID: oid IfFail: fb = ( |
             c.
            | 
            c: copy.
            c reflectionPrimitives:
               reflectionPrimitives copyForVM: aVM Mirror: c OID: oid IfFail: [|:e| ^ fb value: e].
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> 'kleinAndYodaSpecific' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForVM: aVM Oop: oop IfFail: fb = ( |
             c.
            | 
            c: copy.
            c reflectionPrimitives:
               reflectionPrimitives copyForVM: aVM Mirror: c Oop: oop IfFail: [|:e| ^ fb value: e].
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> 'kleinAndYodaSpecific' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         copyWithoutInitializingForVM: aVM OID: oid IfFail: fb = ( |
             c.
            | 
            c: copy.
            c reflectionPrimitives:
               reflectionPrimitives copyWithoutInitializingForVM: aVM Mirror: c OID: oid IfFail: [|:e| ^ fb value: e].
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> 'kleinAndYodaSpecific' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mapMirrorIfFail: fb = ( |
            | 
            reflectionPrimitives forMirror: self MapMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> 'kleinAndYodaSpecific' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         myVMKit = ( |
            | 
            reflectionPrimitives vmKit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> 'kleinAndYodaSpecific' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodCacheIfFail: fb = ( |
            | 
            reflectionPrimitives forMirror: self NMethodCacheIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda mirrors copyDownParentForMirrorPrototypes parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         reflectionPrimitives = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda reflectionPrimitives.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         reflectionPrimitives = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         prototype = ( |
            | 
            kleinAndYoda mirrors copyDownParentForMirrorPrototypes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         createPrototypeForMirrorNamed: n = ( |
             cdParentMir.
             newProtoMir.
             newSlot.
             prototypeMethod.
             targetNamespaceMir.
            | 
            prototypeMethod: ('prototype = (', (0 reflect: vmKit) name, ' mirrors ', n, ')' asSlotIfFail: 0 raiseError) first contents.

            newProtoMir: 0 reflect: (| parent* = nil. reflectionPrimitives |).
            newProtoMir: newProtoMir copyAt: 'parent'               PutContents: 0 reflect: n sendTo: 0 traits mirrors.
            newProtoMir: newProtoMir copyAt: 'reflectionPrimitives' PutContents: 0 reflect: vmKit reflectionPrimitives.
            newProtoMir: newProtoMir copyAt: 'fakeSlotsIterator'    PutContents: 0 reflect: vmKit fakeSlotsIterator.
            newProtoMir: newProtoMir copyAt: 'prototype'            PutContents: prototypeMethod.
            ((newProtoMir slotAt: 'parent'              ) module: moduleNameForGeneratedSlots) visibility: 0 visibility privateSlot.
            ((newProtoMir slotAt: 'reflectionPrimitives') module: moduleNameForGeneratedSlots) visibility: 0 visibility privateSlot.
            ((newProtoMir slotAt: 'fakeSlotsIterator'   ) module: moduleNameForGeneratedSlots) visibility: 0 visibility privateSlot.
            ((newProtoMir slotAt: 'prototype'           ) module: moduleNameForGeneratedSlots) visibility: 0 visibility  publicSlot.

            cdParentMir: 0 reflect: newProtoMir reflectee copyDownParentForVMKitMirror.
            newProtoMir copyDownFrom: cdParentMir Sending: '_Clone' Omitting: ( (cdParentMir slotAt: 'parent')
                                                                              & (cdParentMir slotAt: 'prototype')) asVector.

            targetNamespaceMir: 0 reflect: self.
            targetNamespaceMir at: n PutContents: newProtoMir.
            newSlot:  targetNamespaceMir slotAt: n.
            newSlot makeCreator.
            newSlot module: moduleNameForGeneratedSlots.
            newSlot visibility: 0 visibility publicSlot.
            newProtoMir isComplete: 0 true.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         createPrototypes = ( |
            | 
            (0 transporter moduleDictionary includesKey: moduleNameForGeneratedSlots) ifFalse: [
              0 transporter moduleDictionary add: moduleNameForGeneratedSlots.
            ].
            (0 reflect: 0 mirrors) do: [|:s| createPrototypeForMirrorNamed: s name].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         moduleNameForGeneratedSlots = 'vmKitMirrorsGen'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> () From: ( | {
         'Category: mirror state\x7fModuleInfo: Module: vmKitReflection InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedReflecteeOop.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> () From: ( | {
         'Category: mirror state\x7fModuleInfo: Module: vmKitReflection InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         isCachedReflecteeOopValid <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isKleinOrYodaPrimitiveMirror = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> () From: ( | {
         'Category: mirror state\x7fModuleInfo: Module: vmKitReflection InitialContents: InitializeToExpression: (nil)'
        
         map.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> () From: ( | {
         'Category: mirror state\x7fModuleInfo: Module: vmKitReflection InitialContents: InitializeToExpression: (-1)'
        
         mapProgrammingTimestamp <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> () From: ( | {
         'Category: mirror state\x7fModuleInfo: Module: vmKitReflection InitialContents: InitializeToExpression: (nil)'
        
         myMirror.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> () From: ( | {
         'Category: mirror state\x7fModuleInfo: Module: vmKitReflection InitialContents: InitializeToExpression: (nil)'
        
         myVM.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda reflectionPrimitives parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForVM: aVM Mirror: m OID: oid IfFail: fb = ( |
            | 
            aVM setTheVMAndDo: [|c|
              c: copyWithoutInitializingForVM: aVM Mirror: m OID: oid IfFail: [|:e| ^ fb value: e].
              c setMapIfFail: [|:e| ^ fb value: e].
              c
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForVM: aVM Mirror: m Oop: oop IfFail: fb = ( |
            | 
            aVM setTheVMAndDo: [|c|
              c: copy.
              c invalidateCachedItems.
              c myVM: aVM.
              c myMirror: m.
              c reflecteeOop: oop IfFail: [|:e| ^ fb value: e].
              c setMapIfFail: [|:e| ^ fb value: e].
              c
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         copyWithoutInitializingForVM: aVM Mirror: m OID: oid IfFail: fb = ( |
            | 
            aVM setTheVMAndDo: [
              | c |
              c: copy.
              c invalidateCachedItems.
              c myVM: aVM.
              c myMirror: m.
              c reflecteeOID: oid.
              c
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: evaluating\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         evaluateLocal: m = ( |
            | 
            resend.evaluate: m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: evaluating\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         evaluateRemote: m = ( |
             c.
             nm.
             s.
            | 
            s: (() _Mirror copyAt: 'doit' PutContents: m) slotAt: 'doit'.

            c: myVM compilerPrototype.
            c: c copyForContext: (c prototypes compilationContext copyForSlot: s)
                   Architecture: myVM architecture
                         Oracle: myVM image
                          Debug: true
                       Optimize: c prototypes optimizationPolicies compileQuickly.

            myVM setTheVMAndDo: [
              nm: c compileForcingNonLeafIfNecessary buildNMethod.
            ].

            "Need something like incremental update to finish implementing this."
            halt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m AnnotationAt: n IfFail: fb = ( |
            | 
            forMirror: m AnnotationAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m CodesIfFail: fb = ( |
            | 
            (m methodMirrorIfFail: [|:e| ^ fb value: e])
               codesIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m CodesMirrorIfFail: fb = ( |
            | 
            (m methodMirrorIfFail: [|:e| ^ fb value: e])
               codesMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m LiteralsIfFail: fb = ( |
            | 
            (m methodMirrorIfFail: [|:e| ^ fb value: e])
               literalsIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m LiteralsMirrorIfFail: fb = ( |
            | 
            (m methodMirrorIfFail: [|:e| ^ fb value: e])
               literalsMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fCategory: block methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forBlockMethodMirror: m CreateBlockIfFail: fb = ( |
            | 
            notImplementedYet).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fCategory: block methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forBlockMethodMirror: m LexicalParentIfFail: fb = ( |
            | 
            [lexicalParent]. "browsing"
            withMapDo: [m primitiveContentsAt: 'lexicalParent' IfFail: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: blocks\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forBlockMirror: m LexicalParentIfFail: fb = ( |
            | 
            notImplementedYet).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forByteVectorMirror: m ReflecteeAt: idx IfFail: fb = ( |
            | 
            (m reflecteeMirrorAt: idx IfFail: [|:e| ^ fb value: e])
              reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forByteVectorMirror: m ReflecteeAt: idx Put: val IfFail: fb = ( |
            | 
            notImplementedYet).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forByteVectorMirror: m ReflecteeBytesIfFail: fb = ( |
            | 
            withMapDo: [
              map bytesFrom: (reflecteeOopIfFail: [|:e| ^ fb value: e])
                     IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forByteVectorMirror: m ReflecteeMirrorAt: idx IfFail: fb = ( |
            | 
            withMapDo: [reflect:
                          map for: (reflecteeOopIfFail: [|:e| ^ fb value: e])
                              IndexableAt: idx
                              IfFail: [|:e| ^ fb value: e]]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forByteVectorMirror: m ReflecteeSizeIfFail: fb = ( |
            | 
            forObjOrByteVectorMirror: m ReflecteeSizeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m AllSlotsOnThisMethodIfFail: fb = ( |
             actMap.
             r.
            | 
            m isReflecteeActivation ifTrue: [^ (m methodMirrorIfFail: [|:e| ^ fb value: e]) allSlotsOnThisMethodIfFail: fb].

            actMap: m reflectionPrimitives importReflecteeActivationMapIfFail: [|:e| ^ fb value: e].
            r: list copyRemoveAll.
            actMap slotsSatisfying: true Do: [|:n. :d. :a. :i|
              r add: actMap argOrLocalFakeSlot copyForMirror: m Name: n Index: i.
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m CodesIfFail: fb = ( |
            | 
            (m codesMirrorIfFail: [|:e| ^ fb value: e])
                reflecteeStringIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m CodesMirrorIfFail: fb = ( |
            | 
            [codes]. "browsing"
            withMapDo: [m primitiveContentsAt: 'codes' IfFail: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m FileIfFail: fb = ( |
            | 
            (m fileMirrorIfFail: [|:e| ^ fb value: e])
                 reflecteeStringIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m FileMirrorIfFail: fb = ( |
            | 
            [file]. "browsing"
            withMapDo: [m primitiveContentsAt: 'file' IfFail: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         forMethodMirror: m InitialContentsBySlotIndexIfFail: fb = ( |
            | 
            vmKit maps activationMap initialContentsInterpreter copyInterpretMethod: m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m LineIfFail: fb = ( |
            | 
            (m lineMirrorIfFail: [|:e| ^ fb value: e])
                 importReflecteeAsInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m LineMirrorIfFail: fb = ( |
            | 
            [line]. "browsing"
            withMapDo: [m primitiveContentsAt: 'line' IfFail: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m LiteralsIfFail: fb = ( |
             litsMir.
             r.
            | 
            litsMir: m literalsMirrorIfFail: [|:e| ^ fb value: e].
            r: vector copySize: litsMir reflecteeSizeIfFail: [|:e| ^ fb value: e].
            r size do: [|:i. litMir|
              litMir: litsMir reflecteeMirrorAt: i IfFail: [|:e| ^ fb value: e].
              r at: i
               Put: litMir isReflecteeString ifTrue: [litMir reflecteeStringIfFail: [|:e| ^ fb value: e]]
                                              False: [litMir reflecteeIfFail: [litMir]].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m LiteralsMirrorIfFail: fb = ( |
            | 
            [literals]. "browsing"
            withMapDo: [m primitiveContentsAt: 'literals' IfFail: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m NameAt: index IfFail: fb = ( |
             actMap.
             mm.
            | 
            "Put in for local access bytecodes; should only be used by method mirrors."
            mm: m isReflecteeActivation ifTrue: [m methodMirrorIfFail: [|:e| ^ fb value: e]] False: [m].
            actMap: mm reflectionPrimitives importReflecteeActivationMapIfFail: [|:e| ^ fb value: e].
            actMap size do: [|:i. t|
              t: actMap typeAt: i.
              (myVM vmKit slotType isObjectSlot: t) || [myVM vmKit slotType isArgumentSlot: t] ifTrue: [
                (actMap dataAt: i) = index ifTrue: [^ actMap nameAt: i].
              ].
            ].
            fb value: 'nameAt: ', index printString, ' not found').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m PositionForPositionTableAtBCI: bci = ( |
             foreignInnerMethodMir.
             innerActMap.
            | 
            foreignInnerMethodMir: m methodMirrorIfFail: [^ 0].
            innerActMap: foreignInnerMethodMir reflectionPrimitives importReflecteeActivationMap.

            innerActMap cachedTransmogrifier ifNil: [| recreatedInnerActMap |
              myVM setTheVMAndDo: [
                recreatedInnerActMap:
                   innerActMap kleinifyReflecteeOf: (recreateSelfMethodFor: foreignInnerMethodMir)
                                            Mapper: vmKit mapperForRecreatingPositionTable.
              ].
              innerActMap cachedTransmogrifier: recreatedInnerActMap cachedTransmogrifier.
            ].

            innerActMap cachedTransmogrifier positionTableIndicesByKleinBCI at: bci).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m PrimitiveContentsAtIndex: i IfFail: fb = ( |
             actMap.
             d.
            | 
            actMap: importReflecteeActivationMapIfFail: [|:e| ^ fb value: e].
            d: actMap dataAt: i.
            (actMap isMapSlotAt: i)
              ifTrue: [mirrorFor: d IfFail: fb]
               False: [(forMethodMirror: m InitialContentsBySlotIndexIfFail: [|:e| ^ fb value: e])
                              at: d IfAbsent: [reflect: nil]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m ReflecteeAt: i IfFail: fb = ( |
            | 
            forVectorMirror: m ReflecteeAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m ReflecteeAt: i Put: val IfFail: fb = ( |
            | 
            forVectorMirror: m ReflecteeAt: i Put: val IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m ReflecteeMirrorAt: i IfFail: fb = ( |
            | 
            forVectorMirror: m ReflecteeMirrorAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m ReflecteeSizeIfFail: fb = ( |
            | 
            forVectorMirror: m ReflecteeSizeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m SourceIfFail: fb = ( |
            | 
            (m sourceStringMirrorIfFail: [|:e| ^ fb value: e])
                reflecteeStringIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m SourceLengthIfFail: fb = ( |
            | 
            (m sourceLengthMirrorIfFail: [|:e| ^ fb value: e])
                 importReflecteeAsInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m SourceLengthMirrorIfFail: fb = ( |
            | 
            "Could make forBlockMethodActivationMirror:SourceLengthMirrorIfFail:, etc. -- Adam, 4/06"

            case
              if:  m isReflecteeBlockMethodActivation Then: [(m methodMirrorIfFail: [|:e| ^ fb value: e])
                                                                  sourceLengthMirrorIfFail: fb]
              If: [m isReflecteeMethodActivation]     Then: [0 asMirror]
                                                      Else: [[sourceLength]. "browsing"
                                                             withMapDo: [m primitiveContentsAt: 'sourceLength' IfFail: fb]
                                                                IfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m SourceMirrorIfFail: fb = ( |
            | 
            [sourceString]. "browsing"
            withMapDo: [m primitiveContentsAt: 'sourceString' IfFail: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m SourceOffsetIfFail: fb = ( |
            | 
            (m sourceOffsetMirrorIfFail: [|:e| ^ fb value: e])
                importReflecteeAsInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m SourceOffsetMirrorIfFail: fb = ( |
            | 
            m isReflecteeMachineLevelForeignActivation ifTrue: [^ 0 asMirror].

            case
              if:  m isReflecteeBlockMethodActivation Then: [(m methodMirrorIfFail:         [|:e| ^ 0 asMirror])
                                                                  sourceOffsetMirrorIfFail: [|:e| ^ 0 asMirror]]
              If: [m isReflecteeMethodActivation]     Then: [0 asMirror]
                                                      Else: [[sourceOffset]. "browsing"
                                                             withMapDo: [m primitiveContentsAt: 'sourceOffset' IfFail: fb]
                                                                IfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: slot-like primitives\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m AnnotationAt: n IfFail: fb = ( |
            | 
            withMapDo: [
              map annotationAt: map indexOfSlotNamed: n IfAbsent: [|:e| ^ fb value: e]
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: annotation\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m AnnotationIfFail: fb = ( |
            | 
            isMapSet && [map annotation isNotNil]
              ifTrue: [map annotation]
               False: [fb value: 'no annotation']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: slot-like primitives\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m ContentsAt: n IfFail: fb = ( |
             assignmentMir = bootstrap stub -> 'globals' -> 'mirrors' -> 'assignment' -> ().
            | 
            withMapDo: [
              (m primitiveIsAssignmentAt: n IfFail: [|:e| ^ fb value: e]) ifTrue: [^ assignmentMir].
              mirrorFor:  (map contentsOfSlotNamed: n
                                                In: (reflecteeOopIfFail: [|:e| ^ fb value: e])
                                            IfFail: [|:e| ^ fb value: e])
                 IfFail:  fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: annotation\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m CopyAnnotation: a IfFail: fb = ( |
            | 
            [notImplementedYet].
            fb value: 'not implemented yet').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: programming primitives\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m CopyAt: n Put: objMirr IsParent: p IsArgument: isA Annotation: a IfFail: fb = ( |
            | 
            myVM lens  forMirror: self
                 PrimitiveCopyAt: n
                     PutContents: objMirr
                        IsParent: p
                      IsArgument: isA
                      Annotation: a
                          IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: programming primitives\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m CopyRemoveSlot: n IfFail: fb = ( |
            | 
            notImplementedYet).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: replacing one object with another\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m Define: newObjMir IfFail: fb = ( |
            | 
            myVM lens forMirror: self
                PrimitiveDefine: newObjMir
                         IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: naming\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m EvalNameIfNoStoreStringOrCreatorNameIfFail: fb = ( |
            | myVM lens evalNameIfNoStoreStringOrCreatorNameFor: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: evaluating\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m Evaluate: otherMir = ( |
            | myVM lens evaluate: otherMir For: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: slot-like primitives\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m IsArgumentAt: n IfFail: fb = ( |
            | 
            withMapDo: [map isSlotArgumentNamed: n IfAbsent: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: slot-like primitives\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m IsAssignableAt: n IfFail: fb = ( |
            | 
            withMapDo: [map isSlotAssignableNamed: n IfAbsent: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: slot-like primitives\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m IsAssignmentAt: n IfFail: fb = ( |
            | 
            withMapDo: [map isSlotAssignmentNamed: n IfAbsent: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m IsOKToSend: selector = ( |
            | 
            m isReflecteeKleinAndYodaImmediate && [isMapSet]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: slot-like primitives\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m IsParentAt: n IfFail: fb = ( |
            | 
            withMapDo: [map isSlotParentNamed: n IfAbsent: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m MapMirrorIfFail: fb = ( |
            | 
            myVM setTheVMAndDo: [
              mirrorFor: (vmKit layouts object mapOf: (reflecteeOopIfFail: [|:e| ^ fb value: e])
                                              IfFail: [|:e| ^ fb value: e])
                 IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m NMethodCacheIfFail: fb = ( |
            | 
            withMapDo: [
              mirrorFor: (map nmethodCacheOf: (reflecteeOopIfFail: [|:e| ^ fb value: e])
                                      IfFail: [|:e| ^ fb value: e])
                 IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: naming\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m NameSuffixIfFail: fb = ( |
            | 
            myVM lens nameSuffixFor: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: names\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m NamesIfFail: fb = ( |
            | 
            withMapDo: [map namesIfFail: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m ReflecteeEq: x IfFail: fb = ( |
            | myVM isNotNil && [myVM lens doesReflecteeOf: self Eq: x IfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: naming\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m ReflecteeIDIfFail: fb = ( |
            | 
            reflecteeTaggedOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m ReflecteeIdentityHashIfFail: fb = ( |
            | myVM lens reflecteeIdentityHashOf: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: reflectee\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m ReflecteeIfFail: fb = ( |
            | 
            "Implemented as a fake primitive if m is a local mirror
             inside Klein; does not make sense if m is a remote mirror,
             unless it's an immediate."
            m isReflecteeKleinAndYodaImmediate ifTrue: [withMapDo: [^ m kleinAndYodaLayout valueOf: reflecteeOop] IfFail: fb].
            m _MirrorReflecteeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         forObjOrByteVectorMirror: m ReflecteeSizeIfFail: fb = ( |
            | 
            withMapDo: [map indexableSizeOf: (reflecteeOopIfFail: [|:e| ^ fb value: e])
                                     IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ExportContentsOf: aVector IntoReflecteeIfFail: fb = ( |
            | 
            withMapDo: [
              [aVector size = (m reflecteeSizeIfFail: [|:e| ^ fb value: e])] assert.
              map for: (reflecteeOopIfFail: [|:e| ^ fb value: e]) PopulateIndexablesBy: [|:i|
                aVector at: i
              ].
            ] IfFail: fb.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ImportReflecteeAsVectorOfImmediatesIfFail: fb = ( |
            | 
            withMapDo: [map importVectorOfImmediates: (reflecteeOopIfFail: [|:e| ^ fb value: e]) IfFail: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m MirrorsOnIndexablesIfFail: fb = ( |
            | 
            withMapDo: [| mirs. oop |
              oop: reflecteeOopIfFail: [|:e| ^ fb value: e].
              mirs: vector copySize: numberOfMirrorsToImportIfFail: [|:e| ^ fb value: e].
              mirs mapBy: [|:x. :i| mirrorFor: (map for: oop IndexableAt: i IfFail: [|:e| ^ fb value: e])
                                       IfFail: [|:e| ^ fb value: e]].
              mirs
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ReflecteeAt: idx IfFail: fb = ( |
            | 
            (m reflecteeMirrorAt: idx IfFail: [|:e| ^ fb value: e]) reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ReflecteeAt: idx Put: val IfFail: fb = ( |
            | 
            notImplementedYet).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ReflecteeMethodPointerIfFail: fb = ( |
            | 
            vmSlotNamed: 'methodHolder' IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ReflecteeMirrorAt: idx IfFail: fb = ( |
            | 
            withMapDo: [
              mirrorFor: (map for: (reflecteeOopIfFail: [|:e| ^ fb value: e])
                              IndexableAt: idx
                              IfFail: [|:e| ^ fb value: e])
                 IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ReflecteeSizeIfFail: fb = ( |
            | 
            forObjOrByteVectorMirror: m ReflecteeSizeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fCategory: importing activation map\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         importLocalReflecteeActivationMapIfFail: fb = ( |
            | 
            [myMirror isReflecteeMethod] assert.
            myMirror reflecteeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fCategory: importing activation map\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         importReflecteeActivationMap = ( |
            | 
            importReflecteeActivationMapIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fCategory: importing activation map\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         importReflecteeActivationMapIfFail: fb = ( |
            | 
            myVM lens importActivationMapReflecteeOf: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fCategory: importing activation map\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         importRemoteReflecteeActivationMapIfFail: fb = ( |
             mapOop.
            | 
            mapOop: reflecteeOopIfFail: [|:e| ^ fb value: e].
            myVM setTheVMAndDo: [myVM vmKit maps map importRemoteMap: mapOop IfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = ( |
            | 
            myVM vmKit layouts object intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateCachedItems = ( |
            | 
            myVM ifNotNil: [
              myVM objectLocator ifDirect: [
                myMirror isNotNil && [myMirror isReflecteeKleinAndYodaImmediate] ifFalse: [
                  isCachedReflecteeOopValid: false.
                  cachedReflecteeOop: nil.
                ].
              ] IfIndirect: [].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateMyObsoleteCachedItems = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isForVM: aVM = ( |
            | 
            myVM == aVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         isMapCurrent = ( |
            | 
            myVM universe programmingTimestamp = mapProgrammingTimestamp).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         isMapSet = ( |
            | 
            map isNotNil ifFalse: [^ false].
            isMapCurrent ifTrue:  [^ true].
            myVM setTheVMAndDo: [setMapIfFail: [map: nil. ^ false]].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: annotation\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isOKToTransformAnnotationOfAbstractMirrorAfterParsing = ( |
            | 
            [todo optimization kleinDebugger kleinReflection]. "too slow?"
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: naming\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         localEvalNameIfNoStoreStringOrCreatorName = ( |
            | 
            [todo localReflection]. "Implement this properly."
            globals reflectionPrimitives forMirror: myMirror EvalNameIfNoStoreStringOrCreatorName: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: naming\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         localNameSuffixIfFail: fb = ( |
            | 
            '').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: programming primitives\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         localPrimitiveCopyAt: n PutContents: objMirr IsParent: p IsArgument: isA Annotation: a IfFail: failBlock = ( |
            | 
            resend.primitiveCopyAt: n
                       PutContents: objMirr
                          IsParent: p
                        IsArgument: isA
                        Annotation: a
                            IfFail: failBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: programming primitives\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         localPrimitiveDefine: newObjMir IfFail: fb = ( |
            | 
            resend.primitiveDefine: newObjMir
                            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         localReflecteeEq: x IfFail: fb = ( |
            | 
               x isKleinOrYodaMirror
            && [(myVM = x reflectionPrimitives myVM)
            && [reflecteeOop _Eq: x reflectionPrimitives reflecteeOop]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         localReflecteeIdentityHash = ( |
            | 
            reflecteeOop _IdentityHash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         mirrorFor: o IfFail: fb = ( |
            | 
            myMirror reflectionPrimitives thisKindOfMirrorFor: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         numberOfMirrorsToImportIfFail: fb = ( |
            | 
            myMirror reflecteeSizeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         primitiveContentsAt: n IfFail: fb = ( |
            | 
            myMirror primitiveContentsAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fCategory: mapping bci\'s to source position\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         recreateSelfMethodFor: foreignInnerMethodMir = ( |
             fullMethodText.
             methodBodyText.
             ols.
            | 
            ols: myMirror outermostLexicalScope.
            methodBodyText: selfMethodText copyForMethod: ols methodMirrorIfFail: raiseError.
            fullMethodText: ols selectorAndArguments, ' = ', methodBodyText asMethod asString.

            selfMethodFor: foreignInnerMethodMir
                       In: (fullMethodText asSlotIfFail: raiseError) first contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeAddress = ( |
            | reflecteeAddressIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeAddressIfFail: fb = ( |
            | 
            myVM ifNil: [^ fb value: 'no vm'].
            myVM setTheVMAndDo: [| oop |
              oop: reflecteeOopIfFail: [|:e| ^ fb value: e].
              myVM objectLocator addressOfRemoteMem: oop
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeOID = ( |
            | 
            intNN shr: reflecteeTaggedOID With: myVM vmKit tag size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeOID: oid = ( |
            | 
            reflecteeTaggedOID: (intNN shl: oid With: myVM vmKit tag size) + myVM vmKit tag mem.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeOop = ( |
            | 
            reflecteeOopIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         reflecteeOop: oop IfFail: fb = ( |
            | 
            myVM setTheVMAndDo: [
              cachedReflecteeOop: oop.
              isCachedReflecteeOopValid: true.

              [todo cleanup oraclesAndOIDs]. "How should mirrors work when they're inside Klein? -- Adam"
              myVM lens == myVM vmKit localObjectLens ifFalse: [
                myVM vmKit layouts object
                   if:          oop
                   IsFloat:     [setReflecteeToImmediateOop: oop]
                   IsSmi:       [setReflecteeToImmediateOop: oop]
                   IsMark:      [^ fb value: 'attempted to set the reflecteeOop to a mark']
                   IsMem:       [reflecteeOID: myVM oidForOop: oop IfAbsent: [|:e| ^ fb value: e]].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeOopIfFail: fb = ( |
            | 
            myMirror reflectionPrimitives invalidateMyObsoleteCachedItems. "might be a cachingReflectionPrimitives"
            isCachedReflecteeOopValid ifFalse: [
              myVM ifNotNil: [
                myVM setTheVMAndDo: [
                  cachedReflecteeOop:  myVM oopForOID: reflecteeOID IfAbsent: [^ fb value: 'Could not find oop for reflectee'].
                  isCachedReflecteeOopValid: true.
                ].
              ].
            ].
            cachedReflecteeOop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: naming\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         remoteEvalNameIfNoStoreStringOrCreatorName = ( |
            | 
            "Calling _AsObject won't work here, since the object is over in Klein.
             Use the corresponding object on the Self side, if there is one. That
             way at least senders/implementors will work. -- Adam, 3/05"

            myVM setTheVMAndDo: [
              (myVM image objectsOracle kleinifiedMirrorForOID: reflecteeOID
                                                      IfAbsent: [reflect: lobby]) evalName
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: naming\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         remoteNameSuffixIfFail: fb = ( |
            | 
            isMapSet
              ifTrue: [' ', myVM machineMemory statePrintString]
               False: '').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: programming primitives\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         remotePrimitiveCopyAt: n PutContents: objMirr IsParent: p IsArgument: isA Annotation: a IfFail: failBlock = ( |
            | 
            [notImplementedYet].
            failBlock value: 'not implemented yet').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: programming primitives\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         remotePrimitiveDefine: newObjMir IfFail: fb = ( |
             selfMir.
            | 
            selfMir: myVM image objectsOracle kleinifiedMirrorForOID: reflecteeOID IfAbsent: [
              error: 'There is no corresponding Self object. We do not handle this case yet.'
            ].
            selfMir primitiveDefine: newObjMir IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteReflecteeEq: x IfFail: fb = ( |
            | 
               x isKleinOrYodaMirror
            && [(myVM = x reflectionPrimitives myVM)
            && [reflecteeTaggedOID _Eq: x reflectionPrimitives reflecteeTaggedOID]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteReflecteeIdentityHash = ( |
            | 
            myVM hash ^^ reflecteeTaggedOID hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fCategory: mapping bci\'s to source position\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         selfMethodFor: foreignMethodMir In: selfMethodMir = ( |
             fNames.
             fSource.
             foreignLPMethodMir.
             lp.
             selfLPMethodMir.
            | 

            [aaaaaaa]. "Experimenting, because there was a bug where the text wouldn't quite match because of a <- nil."
            [fSource: (selfMethodText copyForMethod: foreignMethodMir) asMethod asString.].
            fSource: (selfMethodText copyForMethod: foreignMethodMir) copy formatMethodBodyWithoutSlots asString.
            foreignMethodMir isReflecteeOuterMethod ifTrue: [^ selfMethodMir].
            lp: myMirror lexicalParent.
            foreignLPMethodMir: lp methodMirrorIfFail: raiseError.
               selfLPMethodMir: lp reflectionPrimitives selfMethodFor: foreignLPMethodMir In: selfMethodMir.

            fNames: foreignMethodMir allSlotsOnThisMethod copyMappedBy: [|:s| s name].
            selfLPMethodMir literalsDo: [|:lit. litMir|
              litMir:  reflect: lit.
              litMir isReflecteeBlock ifTrue: [| m. mNames. mSource |
                m: litMir method.
                mNames: m allSlotsOnThisMethod copyMappedBy: [|:s| s name].
                mSource: (selfMethodText copyForMethod: m) copy formatMethodBodyWithoutSlots asString.
                (fNames = mNames) && [fSource = mSource] ifTrue: [^ m].
              ].
            ].

            error: 'could not find matching Self method').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: importing the map\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         setMapIfFail: fb = ( |
            | 
            myVM setTheVMAndDo: [
              mapProgrammingTimestamp: myVM universe programmingTimestamp.
              map:  myVM vmKit maps map importMapFor: (reflecteeOopIfFail: [|:e| ^ fb value: e])
                                              IfFail: fb.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         setReflecteeToImmediateOop: immOop = ( |
            | 
            reflecteeTaggedOID: immOop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         thisKindOfMirrorFor: o IfFail: fb = ( |
            | 
            myVM noncachingMirrorFor: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKit = ( |
            | kleinAndYoda).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: slots\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         vmSlotNamed: n IfFail: fb = ( |
            | 
            withMapDo: [myMirror primitiveContentsAt: (map vmSlotNameFor: n) IfFail: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: importing the map\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         withMapDo: blk IfFail: failBlk = ( |
            | 
            isMapSet ifFalse: [^ failBlk value: 'map not set'].
            myVM setTheVMAndDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            kleinAndYoda reflectionPrimitives).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> () From: ( | {
         'Category: mirror state\x7fModuleInfo: Module: vmKitReflection InitialContents: InitializeToExpression: (-1)\x7fVisibility: private'
        
         reflecteeTaggedOID <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         vmKitReflection = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitReflection' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitReflection' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitReflection.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitReflection' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitReflection' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitReflection' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitReflection' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
             resend.postFileIn.
            kleinAndYoda cachingReflectionPrimitives initializePrototype.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitReflection' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.13 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitReflection' -> () From: ( | {
         'ModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitActivations
vmKitProcess
vmKitCachingMirror
vmKitSelectorFinder
vmKitMirrorCache
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'fakeSlot' -> 'vectorElement' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         localContentsIn: localHolder = ( |
            | 
            "Use the primitive because localHolder might not inherit from traits vector."
            localHolder _At: elementIndex).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'fakeSlot' -> 'vectorElement' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         setLocalContentsIn: localHolder To: c = ( |
            | 
            "Use the primitive because localHolder might not inherit from traits vector."
            localHolder _At: elementIndex Put: c.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'fakeSlot' -> 'vectorElement' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         setRemoteContentsIn: holderOop To: contentsOop = ( |
            | 
            kleinAndYoda layouts objVector
                       for: holderOop
               IndexableAt: elementIndex
                       Put: contentsOop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: constructing prototypes\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDownParentForVMKitMirror = ( |
            | 
            reflectionPrimitives vmKit mirrors copyDownParentForMirrorPrototypes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isKleinOrYodaMirror = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeKleinAndYodaImmediate = ( |
            | 
            isReflecteeInteger || [isReflecteeFloat]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeKleinCompiledBlock = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeKleinNMethod = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeKleinOrYodaPrimitiveMirror = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeModule = ( |
            | 
            creatorSlotHint holder = (reflect: modules)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeNMethod = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeVMKitActivationMap = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: testing\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeVMKitMap = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: converting\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         mirrorPrototypeCreatorSlotName = ( |
            | 
            type).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: converting\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorPrototypeFromNamespace: ns = ( |
            | 
            mirrorPrototypeCreatorSlotName sendTo: ns).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'abstractMirror' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: slot-like primitives\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         primitiveIsAssignmentAt: n IfFail: fb = ( |
            | 
            reflectionPrimitives forMirror: self IsAssignmentAt: n asString canonicalize IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'byteVector' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         importReflecteeAsImmediate = ( |
            | 
            importReflecteeAsInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'byteVector' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         importReflecteeAsInteger = ( |
            | 
            theVM assemblerSystem intNNFromBytes: reflecteeBytesIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'float' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         importReflecteeAsFloat = ( |
            | 
            reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'float' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         importReflecteeAsImmediate = ( |
            | 
            importReflecteeAsFloat).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'method' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: importing activation map\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         activationMapIfFail: fb = ( |
            | 
            [inProgress adam aaaa refactoringMirrors]. "Get this method off of here."
            reflectionPrimitives importReflecteeActivationMapIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'smallInt' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         importReflecteeAsImmediate = ( |
            | 
            importReflecteeAsInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'smallInt' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         importReflecteeAsInteger = ( |
            | 
            reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'smallInt' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         mirrorPrototypeCreatorSlotName = ( |
            | 
            "Why doesn't the 'type' method return 'smallInt'?"
            'smallInt').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'slots' -> 'plain' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         fromRemoteContentsSetLocalContentsIn: localHolder = ( |
            | 
            setLocalContentsIn: localHolder To: contents importReflecteeAsImmediate.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'slots' -> 'plain' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         localContentsIn: localHolder = ( |
            | 
            name sendTo: localHolder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'slots' -> 'plain' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         setLocalContentsIn: localHolder To: c = ( |
            | 
            isVectorElement ifFalse: [
              (name, ':') sendTo: localHolder With: c.
            ] True: [
              "Use the primitive because localHolder might not inherit from traits vector."
              localHolder _At: elementIndex Put: c.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'slots' -> 'plain' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         setRemoteContentsFromLocalContentsIn: localHolder = ( |
             c.
             cMir.
             cOop.
            | 
            c: localContentsIn: localHolder.
            cMir: reflect: c.
            [cMir isReflecteeKleinAndYodaImmediate] assert: 'Try shrinking "fixed_address_buffer_length" in the klein_C_code.'.
            cOop: cMir kleinAndYodaLayout oopForValue: c.
            setRemoteContentsIn: holder reflectionPrimitives reflecteeOop
                             To: cOop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'slots' -> 'plain' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         setRemoteContentsIn: holderOop To: contentsOop = ( |
            | 
            holder reflectionPrimitives map
                 setContentsOfSlotNamed: name
                                     In: holderOop
                                     To: contentsOop
                                 IfFail: raiseError.
            self).
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitActivations' From: 'applications/klein'
 bootstrap read: 'vmKitProcess' From: 'applications/klein'
 bootstrap read: 'vmKitCachingMirror' From: 'applications/klein'
 bootstrap read: 'vmKitSelectorFinder' From: 'applications/klein'
 bootstrap read: 'vmKitMirrorCache' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitReflection postFileIn
