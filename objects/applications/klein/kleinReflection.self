 '$Revision: 30.82 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         fakeSlotsIterator = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein fakeSlotsIterator.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         activation: a DoKleinSlots: b = ( |
            | 
            b value: fakeSlot nmethod copyMirror: a.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> () From: ( | {
         'Category: fake slots\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         fakeSlot = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein fakeSlotsIterator fakeSlot.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> () From: ( | {
         'Category: nmethods\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         disassembledMachineCode = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledMachineCode' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein fakeSlotsIterator fakeSlot disassembledMachineCode.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledMachineCode' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: InitializeToExpression: (kleinAndYoda mirrors copyDownParentForMirrorPrototypes)'
        
         mirror <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledMachineCode' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         parent* <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledMachineCode' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein fakeSlotsIterator fakeSlot disassembledMachineCode parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledMachineCode' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         actualValueIfFail: fb = ( |
            | mirror disassembledMachineCodeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledMachineCode' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         isApplicable = ( |
            | 
            mirror isKleinOrYodaMirror
            && [mirror isReflecteeNMethod]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledMachineCode' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         key = 'disassembledMachineCode'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'disassembledMachineCode' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'fakeSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> () From: ( | {
         'Category: nmethods\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         entryAddress = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'entryAddress' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein fakeSlotsIterator fakeSlot entryAddress.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'entryAddress' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: InitializeToExpression: (kleinAndYoda mirrors copyDownParentForMirrorPrototypes)'
        
         mirror <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'entryAddress' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         parent* <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'entryAddress' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein fakeSlotsIterator fakeSlot entryAddress parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'entryAddress' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         actualValueIfFail: fb = ( |
            | mirror entryAddressMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'entryAddress' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         isApplicable = ( |
            | 
            mirror isKleinOrYodaMirror
            && [mirror isReflecteeNMethod]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'entryAddress' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         key = 'entryAddress'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'entryAddress' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'fakeSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> () From: ( | {
         'Category: nmethods\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         machineCode = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'machineCode' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein fakeSlotsIterator fakeSlot machineCode.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'machineCode' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: InitializeToExpression: (kleinAndYoda mirrors copyDownParentForMirrorPrototypes)'
        
         mirror <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'machineCode' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         parent* <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'machineCode' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein fakeSlotsIterator fakeSlot machineCode parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'machineCode' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         actualValueIfFail: fb = ( |
            | mirror machineCodeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'machineCode' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         isApplicable = ( |
            | 
            mirror isKleinOrYodaMirror
            && [mirror isReflecteeNMethod]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'machineCode' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         key = 'machineCode'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'machineCode' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'fakeSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> () From: ( | {
         'Category: activations\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         nmethod = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethod' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein fakeSlotsIterator fakeSlot nmethod.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethod' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: InitializeToExpression: (kleinAndYoda mirrors copyDownParentForMirrorPrototypes)'
        
         mirror <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> 'copyDownParentForMirrorPrototypes' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethod' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         parent* <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethod' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein fakeSlotsIterator fakeSlot nmethod parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethod' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         actualValueIfFail: fb = ( |
            | 
            mirror nmethodMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethod' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         isApplicable = ( |
            | 
            mirror isKleinOrYodaMirror
            && [mirror isReflecteeMethod]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethod' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         key = 'nmethod'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'nmethod' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'fakeSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> 'fakeSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         mirror: m Do: block = ( |
            | 
            m isReflecteeNMethod    ifTrue: [nmethod:    m DoKleinSlots: block].
            m isReflecteeActivation ifTrue: [activation: m DoKleinSlots: block].
            resend.mirror: m Do: block).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         mirror: m OneOfEachDo: b = ( |
            | 
            m isReflecteeNMethod    ifTrue: [nmethod:    m DoKleinSlots: b].
            m isReflecteeActivation ifTrue: [activation: m DoKleinSlots: b].
            resend.mirror: m OneOfEachDo: b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         nmethod: m DoKleinSlots: b = ( |
            | 
            b value: fakeSlot entryAddress            copyMirror: m.
            b value: fakeSlot machineCode             copyMirror: m.
            b value: fakeSlot disassembledMachineCode copyMirror: m.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'fakeSlotsIterator' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fCategory: mirrors\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrors = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein mirrors.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDownParentForActivationPrototypes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein mirrors copyDownParentForActivationPrototypes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         createPrototypes = ( |
            | 
            resend.createPrototypes.
            createPrototypeForMirrorNamed: 'nmethod'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = bootstrap stub -> 'globals' -> 'klein' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fCategory: mirrors\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         reflectionPrimitives = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda reflectionPrimitives copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein reflectionPrimitives.

CopyDowns:
globals kleinAndYoda reflectionPrimitives. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein reflectionPrimitives parent.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: nmethods\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forNMethodMirror: m DisassembledMachineCodeIfFail: fb = ( |
             d.
            | 
            d: assemblerSystems ppc disassembler 
              copyOrigin: m entryAddressIfFail: [|:e| ^ fb value: e].
            d beSpecific.
            reflect:
              d disassembleAllExternalSource: 
                (m machineCodeIfFail: [|:e| ^ fb value: e]) reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: nmethods\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forNMethodMirror: m EntryAddressIfFail: fb = ( |
            | 
            withMapDo: [map entryAddressOf: (reflecteeOopIfFail: [|:e| ^ fb value: e])
                                    IfFail: fb]
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: nmethods\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forNMethodMirror: m MethodHolderIfFail: fb = ( |
            | 
            [m reflectee topScope methodHolder]. "browsing"
            withMapDo: [
              (primitiveContentsAt: 'topScope'     IfFail: [|:e| ^ fb value: e])
               primitiveContentsAt: 'methodHolder' IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: nmethods\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forNMethodMirror: m MethodMirrorIfFail: fb = ( |
            | 
            [todo optimize debuggingEnvironment].
            "There's often no need to create a whole mirror on the
             method, since we often only need it for its
             reflecteeOop. -- Adam, 2/06"

            [m reflectee topScope method]. "browsing"
            withMapDo: [
              (primitiveContentsAt: 'topScope' IfFail: [|:e| ^ fb value: e])
               primitiveContentsAt: 'method'   IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: nmethods\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forNMethodMirror: m SelectorMirrorIfFail: fb = ( |
            | 
            [m reflectee lookupKey selector]. "browsing"
            withMapDo: [ 
              (primitiveContentsAt: 'lookupKey' IfFail: [|:e| ^ fb value: e])
               primitiveContentsAt: 'selector'  IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            klein reflectionPrimitives).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeOopIfFail: fb = ( |
            | 
            myMirror isReflecteeActivation ifTrue: [isThisEverCalled. ^ sp].
            resend.reflecteeOopIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'oop' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         actualValueIfFail: failBlock = ( |
            | 
            reflect: mirror reflectionPrimitives reflecteeOopIfFail: [|:e| ^ failBlock value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'oop' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         isApplicable = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'oop' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         key = 'oop'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'fakeSlotsIterator' -> 'fakeSlot' -> 'oop' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'fakeSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         hasReachedEndOfInitSection <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         initialContentsBySlotIndex <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         lastLiteralPushed.
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda maps activationMap parent initialContentsInterpreter parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         accessLocal: bc = ( |
            | 
            [bc isWrite] assert.
            initialContentsBySlotIndex at: bc indexOfLocal Put: lastLiteralPushed.
            bc).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         endInit: bc = ( |
            | 
            hasReachedEndOfInitSection: true.
            bc).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            hasReachedEndOfInitSection: false.
            initialContentsBySlotIndex: initialContentsBySlotIndex copyRemoveAll.
            resend.initialize).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         initializeForMethod: m = ( |
            | 
            [inProgress adam aaaa refactoringMirrors importingLiterals showDave].
            "This feels like a hack. It bothers me that some abstractBytecodeInterpreters
             deal directly with literals but this one needs to use literal mirrors.
             But would changing all of them to use mirrors make them too slow?

             (The simpleLiterals thing won't help in this case - this interpreter
             is supposed to return a dictionary mapping local-variable indices
             to mirrors on the literals.)

             Look for other things tagged importingLiterals."

            method:   m.
            initializeForCodes: m codes Literals: m literalsMirrorIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         interpret: aBytecode = ( |
            | 
            resend.interpret: aBytecode.
            initialContentsBySlotIndex).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         interpretUpTo: finalPC = ( |
             r.
            | 
            [(pc = finalPC) || [hasReachedEndOfInitSection]] whileFalse: [r: interpretBytecode].
            r).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> 'parent' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         literalAt: i = ( |
            | 
            [inProgress adam aaaa importingLiterals].
            literals reflecteeMirrorAt: i IfFail: [|:e| error: 'bad index']).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'activationMap') -> 'parent' -> 'initialContentsInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         pushLiteral: bc = ( |
            | 
            lastLiteralPushed: bc oopToPush.
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         kleinReflection = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinReflection' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinReflection' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinReflection.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinReflection' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinReflection' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinReflection' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinReflection' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
            resend.postFileIn.
            klein mirrors createPrototypes.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinReflection' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.82 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinReflection' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'kleinMirrorCache
kleinCachingMirror
kleinActivations
kleinWordVector
kleinProcess
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethod = bootstrap setObjectAnnotationOf: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( |
             {} = 'ModuleInfo: Creator: traits mirrors nmethod.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembledMachineCode = ( |
            | disassembledMachineCodeIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembledMachineCodeIfFail: fb = ( |
            | 
            reflectionPrimitives forNMethodMirror: self DisassembledMachineCodeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         entryAddress = ( |
            | entryAddressIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         entryAddressIfFail: fb = ( |
            | 
            reflectionPrimitives forNMethodMirror: self EntryAddressIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         entryAddressMirrorIfFail: fb = ( |
            | 
            reflect:
              entryAddressIfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: scopes\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         inlinedScopeIn: scopeMir ForPC: pc IfFail: fb = ( |
             failBlk.
             inlinedScopesMir.
            | 
            failBlk: [|:e| ^ fb value: e].
            [scopeMir reflectee inlinedScopes]. "browsing"
            inlinedScopesMir: scopeMir primitiveContentsAt: 'inlinedScopes' IfFail: failBlk.
            (inlinedScopesMir reflecteeSizeIfFail: failBlk) do: [|:i. ism|
              ism: inlinedScopesMir reflecteeMirrorAt: i IfFail: failBlk.
              (scope: ism IncludesPC: pc IfFail: failBlk) ifTrue: [
                ^ inlinedScopeIn: ism ForPC: pc IfFail: fb
              ].
            ].
            scopeMir).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeNMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         machineCodeIfFail: fb = ( |
            | 
            reflect: reflecteeBytesIfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         methodHolderIfFail: fb = ( |
            | 
            reflectionPrimitives forNMethodMirror: self MethodHolderIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         methodMirrorIfFail: fb = ( |
            | 
            reflectionPrimitives forNMethodMirror: self MethodMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'ModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'mirrors' -> 'byteVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: scopes\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         scope: scopeMir IncludesPC: pc IfFail: fb = ( |
             failBlk.
             firstPCO.
             lastPCO.
             pcOffsetsMir.
             pco.
            | 
            failBlk: [|:e| ^ fb value: e].
            pco: pc - (entryAddressIfFail: failBlk).
            [scopeMir reflectee pcOffsetsByBCI]. "browsing"
            pcOffsetsMir: scopeMir primitiveContentsAt: 'pcOffsetsByBCI' IfFail: failBlk.
            firstPCO: pcOffsetsMir reflecteeAt: 0 IfFail: failBlk.
            pco < firstPCO ifTrue: [^ false].
             lastPCO: pcOffsetsMir reflecteeAt: (pcOffsetsMir reflecteeSizeIfFail: failBlk) pred IfFail: failBlk.
            pco >  lastPCO ifTrue: [^ false].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: scopes\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         scopeMirrorForPC: pc IfFail: fb = ( |
             sm.
            | 
            [reflectee topScope]. "browsing"
            sm: primitiveContentsAt: 'topScope' IfFail: [|:e| ^ fb value: e].
            inlinedScopeIn: sm ForPC: pc IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         selectorMirrorIfFail: fb = ( |
            | 
            reflectionPrimitives forNMethodMirror: self SelectorMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'nmethod' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         type = 'nmethod'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'slots' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isReflecteeKleinOrYodaPrimitiveMirror = ( |
            | 
            (includesKey: 'isKleinOrYodaPrimitiveMirror') && [reflectee isKleinOrYodaPrimitiveMirror]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'process' -> () From: ( | {
         'Category: testing\x7fComment: World reifies processes, too.
So need this slot.\x7fModuleInfo: Module: kleinReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         isKleinOrYodaMirror = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 



 '-- Sub parts'

 bootstrap read: 'kleinMirrorCache' From: 'applications/klein'
 bootstrap read: 'kleinCachingMirror' From: 'applications/klein'
 bootstrap read: 'kleinActivations' From: 'applications/klein'
 bootstrap read: 'kleinWordVector' From: 'applications/klein'
 bootstrap read: 'kleinProcess' From: 'applications/klein'



 '-- Side effects'

 globals modules kleinReflection postFileIn
