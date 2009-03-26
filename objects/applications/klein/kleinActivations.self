 '$Revision: 30.17 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fCategory: slot-like\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         abstractMachineLevelForeignSlot = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractMachineLevelForeignSlot.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         mirror <- bootstrap stub -> 'globals' -> 'mirrors' -> 'slots' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractMachineLevelForeignSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         isApplicable = ( |
            | 
            mirror isReflecteeMachineLevelForeignActivation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignable = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'fakeSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fCategory: slot-like\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignMemorySlot = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlot' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein abstractMachineLevelForeignSlot copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignMemorySlot.

CopyDowns:
globals klein abstractMachineLevelForeignSlot. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlot' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: InitializeToExpression: (0)'
        
         address <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlot' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         parent* <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignMemorySlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         actualValueIfFail: failBlock = ( |
            | 
            reflect: mirror readMemoryWordAt: address
                                      IfFail: [|:e| ^ failBlock value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         contents: newMir IfFail: fb = ( |
            | 
            mirror memoryAt: address
                        Put: newMir reflectee
                     IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         copyMirror: aMachineLevelForeignActivation Address: a = ( |
            | 
            (resend.copyMirror: aMachineLevelForeignActivation)
            address: a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         key = ( |
            | 
            'mem 16r', address hexPrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlot' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fCategory: slot-like\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignRegisterSlot = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlot' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein abstractMachineLevelForeignSlot copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignRegisterSlot.

CopyDowns:
globals klein abstractMachineLevelForeignSlot. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: InitializeToExpression: (\'r0\')\x7fVisibility: private'
        
         myKey <- 'r0'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignRegisterSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         actualValueIfFail: failBlock = ( |
            | reflect: mirror contentsOfRegister: key IfFail: [|:e| ^ failBlock value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         contents: newMir IfFail: fb = ( |
            | 
            mirror setContentsOfRegister: key
                                      To: newMir reflectee
                                  IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         copyMirror: aMachineLevelForeignActivation Name: n = ( |
            | 
            (resend.copyMirror: aMachineLevelForeignActivation) myKey: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         key = ( |
            | myKey).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlot' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinActivations InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         isReflecteeMachineLevelForeignActivation <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         kleinSpecific* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein mirrors copyDownParentForActivationPrototypes kleinSpecific.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: register locator prototypes\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         abstractRegisterLocator = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein mirrors copyDownParentForActivationPrototypes kleinSpecific abstractRegisterLocator.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> () From: ( | {
         'Comment: 0 = reg has not been saved
-1 = reg is unknown\x7fModuleInfo: Module: kleinActivations InitialContents: InitializeToExpression: (vector copySize: 32 FillingWith: 0)'
        
         gprAddresses <- vector copySize: 32 FillingWith: 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: InitializeToExpression: (nil)'
        
         myProcess.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein mirrors copyDownParentForActivationPrototypes kleinSpecific abstractRegisterLocator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'Category: creating activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         activationMirrorProtoIfFail: fb = ( |
             mm.
             nmm.
            | 
            nmm:    nmethodMirrorIfFail: [|:e| ^ fb value: e].
             mm: nmm methodMirrorIfFail: [|:e| ^ fb value: e].

            mm isReflecteeBlockMethod
              ifTrue: [myProcess myVM mirrorPrototypes blockMethodActivation]
               False: [myProcess myVM mirrorPrototypes      methodActivation]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         callAddressIfAbsent: fb = ( |
            | 
            (senderPCIfAbsent: [|:e| ^ fb value: e]) - oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         contentsOfRegister: regName IfFail: fb = ( |
            | 
            ifContentsOfRegister: regName
                     AreInMemory: [|:a| myProcess readMemoryWordAt: a IfFail: fb]
                   AreInRegister: [myProcess contentsOfRegister: regName IfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | resend.copy gprAddresses: gprAddresses copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'Category: creating activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         createActivationMirror = ( |
             p.
            | 
            "Should this be ^ deadActivation? The problem is that hitting Step on a
             not-yet-Continued debugger depends on having an activation in the stack.
             -- Adam, 4/06"
            p:  activationMirrorProtoIfFail: [vmKit mirrors methodActivation].
            p copyForRegisterLocator: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         frameProto = ( |
            | klein stackFrames protoForArchitecture: myProcess architecture).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         gprAddressFor: regName = ( |
            | 
            gprAddresses at:
              myProcess myAssemblerSystem operands
                 gprNumberFor: regName
                    IfPresent: [|:i| i]
                    IfAbsent:  0 "not a GPR, no stored address").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         ifContentsOfRegister: regName AreInMemory: memBlk AreInRegister: regBlk = ( |
             a.
            | 
            a: gprAddressFor: regName.
            0 = a  ifTrue: [regBlk value: regName]
                    False: [memBlk value: a      ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'Category: byte offsets\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         lastSavedRegisterByteOffset = ( |
            | 
            frameProto lastSavedRegisterOffset * oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         myProcess = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         myVM = ( |
            | 
            myProcess myVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'Category: byte offsets\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         nmethodByteOffset = ( |
            | 
            frameProto nmethodOffset * oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodMirrorIfFail: fb = ( |
             nmOop.
            | 

            [todo optimize debuggingEnvironment].
            "We could save time by just passing around the nmOop,
             rather than creating a whole mirror for it. -- Adam, 2/06"

            nmOop: myProcess oopAt: nmethodByteOffset + sp
                            IfFail: [|:e| ^ fb value: e].

            myVM ifNil: [^ fb value: 'no VM'].
            myVM setTheVMAndDo: [
              [todo cleanup asmPrograms]. "Doesn't work for asm programs - there's no VM."
              (myVM vmKit layouts object isSmi: nmOop) ifTrue: [^ fb value: 'nmethod not set yet'].
              myVM mirrorFor: nmOop IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         nonVolRegSaveAreaWordCountIfAbsent: fb = ( |
             frameMir.
             nmMir.
             r.
             s.
            | 
            nmMir: nmethodMirrorIfFail: [|:e| ^ fb value: e].
            [frame nonVolRegSaveAreaWordCount]. "browsing"
            frameMir: (nmMir at: 'frame' IfAbsent: [^ fb value: 'not set yet']) contents.
            s: frameMir at: 'nonVolRegSaveAreaWordCount' IfAbsent: [^ fb value: 'not set yet'].
            r: s contents reflectee.
            (0 <= r)  &&  [r < 32]  ifTrue: [r]  False: [ fb value: 'out of range: ', r printString ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         oopInRegister: regName IfFail: fb = ( |
            | 
            ifContentsOfRegister: regName
                     AreInMemory: [|:a| myProcess oopAt: a IfFail: fb]
                   AreInRegister: [myProcess oopInRegister: regName IfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         oopSize = ( |
            | 
            klein layouts abstract oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'Category: byte offsets\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         pcByteOffset = ( |
            | 
            frameProto savedPCOffset * oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         senderPCIfAbsent: fb = ( |
            | 
            myProcess readMemoryWordAt: pcByteOffset + (senderSPIfAbsent: [|:e| ^ fb value: e])
                                IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         senderSPIfAbsent: blk = ( |
            | 
            myProcess
              readMemoryWordAt: spByteOffset + sp
                        IfFail: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         sendersGPRAddressesIfAbsent: fb = ( |
             c.
             off.
             r.
            | 
            c: nonVolRegSaveAreaWordCountIfAbsent: [|:e| ^ fb value: e].
            off: (senderSPIfAbsent: [|:e| ^ fb value: e])
               + lastSavedRegisterByteOffset.
            r: gprAddresses copy.
            c do: [|:i|
              r at: r lastKey - i Put: off - (i * oopSize)
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         setContentsOfRegister: regName To: aNumber IfFail: fb = ( |
             a.
            | 
            a: gprAddressFor: regName.
            a = 0 ifTrue: [
              ^ myProcess setContentsOfRegister: regName To: aNumber IfFail: fb
            ].
            myProcess writeWord: aNumber ToMemoryAt: a IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'Category: byte offsets\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         spByteOffset = ( |
            | 
            frameProto savedSPOffset * oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         vmKit = ( |
            | 
            myVM ifNil: [
              "is an asm program"
              klein
            ] IfNotNil: [|:vm|
              vm vmKit
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: creating Klein activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         activationForLocalProcess: p SP: sp = ( |
            | 
            [todo cleanup gc stackRoots]. "Should be using topmostRegisterLocator, I think. -- Adam"
            (registerLocator copyForSP: sp Process: p) createActivationMirror).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         activationMapIfFail: fb = ( |
             mir.
            | 
            mir: methodMirrorIfFail: [|:e| ^ fb value: e].
            mir reflectionPrimitives importReflecteeActivationMapIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: send desc\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         addressOfSendDescWordAt: indexBlk IfFail: fb = ( |
             byteOffset.
            | 
            byteOffset: (indexBlk value: myProcess sendDescForMyPlatform) * myProcess oopSize.
            (pcAfterBranchIfFail: [|:e| ^ fb value: e]) + byteOffset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: process info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         allocatedRegionFor: pc = ( |
            | 
            myProcess allocatedRegionFor: pc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: process info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         allocatedRegions = ( |
            | myProcess allocatedRegions).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: sender\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         ancestorSenderWithSP: desiredSP IfAbsent: fb = ( |
             rl.
             senderRL.
            | 
            rl: myRegisterLocator.
            [senderRL: registerLocator senderOf: rl IfAbsent: [|:e| ^ fb value: e].
             senderRL sp = desiredSP ifTrue: [^ senderRL createActivationMirror].
             rl: senderRL] loop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: converting\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         asMachineLevelActivation = ( |
            | 
            copy isReflecteeMachineLevelForeignActivation: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: registers\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         contentsOfRegister: regName IfFail: fb = ( |
            | 
            myRegisterLocator ifNil: [^ fb value: 'no register locator'].
            myRegisterLocator contentsOfRegister: regName IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: creating Klein activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForRegisterLocator: rl = ( |
             c.
            | 
            c: copy.
            c myRegisterLocator:    rl.
            c myProcess:            rl myProcess.
            c reflectionPrimitives: (reflectionPrimitives copy
                                          myMirror: c)
                                              myVM: rl myVM.
            rl myVM ifNotNil: [|:vm| vm lens initializeActivation: c].
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: process info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         endPoint = ( |
            | myProcess endPoint).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: entry address\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         entryAddressIfFail: fb = ( |
            | 
            (nmethodMirrorIfFail: [|:e| ^ fb value: 'no nmethod: ', e])
              entryAddressIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: entry address\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         entryAddressMirrorIfFail: fb = ( |
            | 
            (nmethodMirrorIfFail: [|:e| ^ fb value: 'no nmethod: ', e])
              entryAddressMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: process info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         entryPoint = ( |
            | myProcess entryPoint).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: stack frame\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         frameSizeIfFail: fb = ( |
            | 
            ((senderSPIfAbsent: [^ fb value: 'no sender']) - sp) absoluteValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: register masks\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         gcMaskIfFail: fb = ( |
            | 
            sendDescWordAt: [|:sd| sd gcMaskIndex] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: sp offsets\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         ifSPOffset: spo IsForARegisterThen: regBlk IsForAStackLocationThen: memBlk IfFail: fb = ( |
            | 
            spo < 0  "negative <-> register number, positive <-> stack offset"
              ifTrue: [ regBlk value: spo negate ]
               False: [ memBlk value: sp + spo   ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: creating Klein activations\x7fCategory: double dispatch\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeLocalActivation = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: creating Klein activations\x7fCategory: double dispatch\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeRemoteActivation = ( |
            | 
            reflectionPrimitives:
              myVM vmKit cachingReflectionPrimitives
                copyForNoncachingReflectionPrimitives: reflectionPrimitives.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: lexical parent\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         lexicalParentSPIfFail: fb = ( |
            | 
            myVM setTheVMAndDo: [
              klein layouts block
                homeFramePointerOf: (receiverOopIfFail: [|:e| ^ fb value: e])
                            IfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: stack frame\x7fCategory: double dispatch\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         localStackFrameIfAbsent: ab = ( |
            | 
            (nmethodMirrorIfFail: ab) reflectee frameIfNil: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: memory slots\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         memorySlotForAddress: a = ( |
            | 
            myVMKit foreignMemorySlot copyMirror: self Address: a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         methodMirrorIfFail: fb = ( |
            | 
            reflectionPrimitives forActivationMirror: self MethodMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         myAssemblerSystem = ( |
            | myProcess myAssemblerSystem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         myVM = ( |
            | 
            myProcess myVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: nmethod\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodMirrorIfFail: fb = ( |
            | 
            reflectionPrimitives forActivationMirror: self NMethodMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: registers\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         nonVolatileRegisterContentsDo: blk IfFail: fb = ( |
            | 
            myRegisterLocator gprAddresses do: [|:gprAddr. :i|
              gprAddr = 0 ifFalse: [
                [todo cleanup adam]. "Possible use for location objects?"
                blk value:  (myProcess readMemoryWordAt: gprAddr IfFail: [|:e| ^ fb value: e])
                     With:  registerNameForNumber: i.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: process info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         oopAt: i IfFail: fb = ( |
            | 
            myProcess oopAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: sp offsets\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         oopAtSPOffset: spo IfFail: fb = ( |
            | 
                         ifSPOffset: spo
                 IsForARegisterThen: [|:regNum| oopInRegister: (registerNameForNumber: regNum) IfFail: fb]
            IsForAStackLocationThen: [|:addr| oopAt: addr IfFail: fb]
                             IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: source-level info\x7fCategory: slot contents\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         oopForArgumentOrAssignableLocalAt: n IfFail: fb = ( |
            | 
            oopAtSPOffset: (spOffsetForSlotAt: n IfFail: [|:e| ^ fb value: e])
                   IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: registers\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         oopInRegister: regName IfFail: fb = ( |
            | 
            myRegisterLocator ifNil: [^ fb value: 'no register locator'].
            myRegisterLocator oopInRegister: regName IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: program counter\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         pcAfterBranchIfFail: fb = ( |
            | 
            myRegisterLocator pcAfterBranchIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: program counter\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         pcIfFail: fb = ( |
            | 
            myRegisterLocator pcIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: program counter\x7fCategory: offsets\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         pcOffsetsByBCIIfFail: fb = ( |
             m.
            | 
            m: pcOffsetsByBCIMirrorIfFail: [|:e| ^ fb value: e].
            (vector copySize: m reflecteeSize)
              mapBy: [|:unused. :i| m reflecteeAt: i]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: program counter\x7fCategory: offsets\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         pcOffsetsByBCIMirrorIfFail: fb = ( |
             nmm.
            | 
            nmm: nmethodMirrorIfFail: [|:e| ^ fb value: e].
            [nmm reflectee pcOffsetsByBCI]. "browsing"
            (nmm at: 'pcOffsetsByBCI' IfAbsent: [|:e| ^ fb value: e]) contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: register masks\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         possiblyLiveLocationsDo: blk = ( |
             gcMaskBooleanVector.
             gcMaskLayout.
             memoryLocs.
             registerLocs.
            | 

            [todo cleanup stackRoots].
            "This is only accidentally correct, because it happens to be true right now that the gcMask
             represents all the registers. If we ever change it so that it doesn't represent the sp, or
             maybe the rtoc?, or the non-vol regs that aren't being used by the nmethod, this'll break
             because we shouldn't try to follow those registers. -- Adam"

            registerLocs: myAssemblerSystem allRegisterLocations.
            memoryLocs: (stackFrameIfAbsent: raiseError) locationsForNonVolLocals.

            gcMaskLayout: myProcess sendDescForMyPlatform gcMaskLayout.
            gcMaskBooleanVector: myProcess intNN booleanVectorFor: gcMaskIfFail: raiseError.

            registerLocs, memoryLocs  do: [|:loc|
              gcMaskLayout
                                  ifLocation: loc
                 ShouldBeRepresentedInGCMask: [|:bitNumber| (gcMaskBooleanVector at: bitNumber) ifTrue: [blk value: loc]]
                                        Else: ["Assume that it's live, since the GC mask can't tell us."
                                               blk value: loc].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: register masks\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         possiblyLiveOopsDo: blk = ( |
             f.
            | 
            "Passing in the frame is just an optimization - we don't want each
             location to need to re-find the frame object. An alternative might
             be to have the activation object cache the frame. -- Adam, 9/05"
            f: stackFrameIfAbsent: raiseError.
            possiblyLiveLocationsDo: [|:loc|
              blk value:  loc oopInActivation: self Frame: f IfFail: raiseError.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: process info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryAt: x Size: s IfFail: fb = ( |
            | 
            myProcess readMemoryAt: x Size: s IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: process info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         readMemoryWordAt: i IfFail: fb = ( |
            | 
            myProcess readMemoryWordAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: source-level info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         receiverAndArgumentOopsIfFail: fb = ( |
             r.
            | 
            r: list copyRemoveAll.
            r add: receiverOopIfFail: [|:e| ^ fb value: e].
            (reflectionPrimitives forActivationMirror: self ActivationMapIfFail: [|:e| ^ fb value: e]) argumentSlotsDo: [|:n|
              r add: oopForArgumentOrAssignableLocalAt: n IfFail: [|:e| ^ fb value: e].
            ].
            r asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: source-level info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         receiverOopIfFail: fb = ( |
            | 
            reflectionPrimitives forActivationMirror: self ReceiverOopIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: sp offsets\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         receiverSPOffsetIfFail: fb = ( |
             nmm.
            | 
            nmm: nmethodMirrorIfFail: [|:e| ^ fb value: e].

            [nmm reflectee incomingRcvrSPOffset]. "browsing"
            (nmm primitiveContentsAt: 'incomingRcvrSPOffset' IfFail: [|:e| ^ fb value: e]) reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: register locator prototypes\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         registerLocator = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'registerLocator' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein mirrors copyDownParentForActivationPrototypes kleinSpecific abstractRegisterLocator copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'registerLocator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein mirrors copyDownParentForActivationPrototypes kleinSpecific registerLocator.

CopyDowns:
globals klein mirrors copyDownParentForActivationPrototypes kleinSpecific abstractRegisterLocator. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'registerLocator' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'registerLocator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein mirrors copyDownParentForActivationPrototypes kleinSpecific registerLocator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'registerLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSP: sp Process: p = ( |
            | 
            (copy sp: sp) myProcess: p).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'registerLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'registerLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         pcAfterBranchIfFail: fb = ( |
            | 
            myProcess readMemoryWordAt: pcByteOffset + sp
                                IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'registerLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         pcIfFail: fb = ( |
            | 
            (pcAfterBranchIfFail: fb) - int32 size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'registerLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         senderOf: rl IfAbsent: blk = ( |
             senderSP.
            | 
            senderSP: rl senderSPIfAbsent: [|:e| ^ blk value: e].
            ((copy
               sp: senderSP)
               myProcess: rl myProcess)
               gprAddresses: rl sendersGPRAddressesIfAbsent: [|:e| ^ blk value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'registerLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         setPC: position IfFail: fb = ( |
            | 
            myProcess
               writeWord: position
              ToMemoryAt: pcByteOffset + sp
                  IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'registerLocator' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: InitializeToExpression: (nil)'
        
         sp.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: registers\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         registerNameForNumber: n = ( |
             adam.
             realWay.
            | 
            adam: (myAssemblerSystem operands gprFor: n) name.
            realWay: (assemblerSystems ppc fields ra operandForValue: n) name.
            [adam = realWay] assert.
            realWay).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: registers\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         registerSlotNamed: n = ( |
            | 
            klein foreignRegisterSlot copyMirror: self Name: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: registers\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         registerSlotsForOperands: symbolicOperands = ( |
             regs.
            | 
            regs: myAssemblerSystem allRegisters.
            ( symbolicOperands asList copyFilteredBy: [|:op| regs includes: op])
                                        copyMappedBy: [|:reg| registerSlotNamed: reg name]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: stack frame\x7fCategory: double dispatch\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         remoteStackFrameIfAbsent: ab = ( |
            | 
            (nmethodMirrorIfFail: ab) primitiveContentsAt: 'frame' IfFail: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: send desc\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         sendDescWordAt: indexBlk IfFail: fb = ( |
            | 
            readMemoryWordAt: (addressOfSendDescWordAt: indexBlk IfFail: [|:e| ^ fb value: e])
                      IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: sender\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         senderSPIfAbsent: fb = ( |
            | 
            myRegisterLocator senderSPIfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: registers\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         setContentsOfRegister: regName To: aNumber IfFail: fb = ( |
            | 
            myRegisterLocator setContentsOfRegister: regName To: aNumber IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: program counter\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         setPC: pc IfFail: fb = ( |
            | myRegisterLocator setPC: pc IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         sp = ( |
            | myRegisterLocator sp).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: source-level info\x7fCategory: slot contents\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         spOffsetForSlotAt: n IfFail: fb = ( |
             index.
             map.
             mm.
             nmm.
            | 
            nmm:     nmethodMirrorIfFail:                                      [|:e| ^ fb value: e].
             mm:  nmm methodMirrorIfFail:                                      [|:e| ^ fb value: e].
            map:  mm reflectionPrimitives importReflecteeActivationMapIfFail:  [|:e| ^ fb value: e].

            index: spOffsetsIndexForSlotAt: n InActivationMap: map IfFail: [|:e| ^ fb value: e].

            [nmm reflectee slotSPOffsets]. "browsing"
            ( (nmm at: 'slotSPOffsets') contents 
                 reflecteeMirrorAt: index IfFail: [|:e| ^ fb value: e]
             ) reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: source-level info\x7fCategory: slot contents\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         spOffsetsIndexForSlotAt: n InActivationMap: map IfFail: fb = ( |
             index <- 0.
            | 
            map size do: [|:i|
              (map nameAt: i) = n ifTrue: [^ index].
              (klein nmethod isSPOffsetRecordedForSlotOfType: map typeAt: i) ifTrue: [index: index succ].
            ].
            fb value: n, ' not found').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: stack frame\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         stackFrameIfAbsent: ab = ( |
            | 
            myProcess lens stackFrameForActivation: self IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: creating Klein activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         topmostActivationForProcess: fp = ( |
            | 
            (topmostRegisterLocator copyForProcess: fp) createActivationMirror).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: machine-level info\x7fCategory: register locator prototypes\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         topmostRegisterLocator = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'topmostRegisterLocator' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein mirrors copyDownParentForActivationPrototypes kleinSpecific abstractRegisterLocator copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'topmostRegisterLocator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein mirrors copyDownParentForActivationPrototypes kleinSpecific topmostRegisterLocator.

CopyDowns:
globals klein mirrors copyDownParentForActivationPrototypes kleinSpecific abstractRegisterLocator. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'topmostRegisterLocator' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'topmostRegisterLocator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein mirrors copyDownParentForActivationPrototypes kleinSpecific topmostRegisterLocator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'topmostRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForProcess: p = ( |
            | 
            copy myProcess: p).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'topmostRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'abstractRegisterLocator' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'topmostRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         pcIfFail: fb = ( |
            | 
            myProcess pcIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'topmostRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         setPC: position IfFail: fb = ( |
            | 
            myProcess setPC: position IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> 'topmostRegisterLocator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         sp = ( |
            | 
            myProcess contentsOfRegister: 'sp' IfFail: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: process info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         wordsAt: addr Size: size = ( |
            | 
            myProcess wordsAt: addr Size: size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: process info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         write: bv ToMemoryAt: addr IfFail: fb = ( |
            | 
            myProcess write: bv ToMemoryAt: addr IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'kleinSpecific' -> () From: ( | {
         'Category: accessing\x7fCategory: process info\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         writeWord: w ToMemoryAt: i IfFail: fb = ( |
            | 
            myProcess writeWord: w ToMemoryAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         liveParent = bootstrap stub -> 'traits' -> 'mirrors' -> 'methodActivation' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: InitializeToExpression: (klein foreignProcess)\x7fVisibility: public'
        
         myProcess <- bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcess' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: InitializeToExpression: (nil)'
        
         myRegisterLocator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: InitializeToExpression: (0)\x7fVisibility: public'
        
         number <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein mirrors copyDownParentForActivationPrototypes parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         reflectionPrimitives = ( |
            | 
            klein reflectionPrimitives).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            klein mirrors copyDownParentForActivationPrototypes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fCategory: source\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         disassembledSourceStringIfFail: fb = ( |
             pc.
            | 
            pc: myMirror pcIfFail: [|:e| ^ fb value: 'could not get pc: ', e].
            pcAndInstructionAt: pc IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         forActivationMirror: m ActivationMapIfFail: fb = ( |
             mir.
            | 
            mir: m methodMirrorIfFail: [|:e| ^ fb value: e].
            mir reflectionPrimitives importReflecteeActivationMapIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         forActivationMirror: m ByteCodePositionIfFail: fb = ( |
             delta <- 536870911.
             off.
             pc.
             pco.
             r <- -1.
            | 
            pco: m pcOffsetsByBCIIfFail: [|:e| ^ fb value: e].
            pc:  m pcIfFail: [|:e| ^ fb value: e].
            off: pc - (m entryAddressIfFail: [|:e| ^ fb value: e]).
            pco do: [
              |:offset. :bci. d|
              d: off - offset.
              case if: [d < 0    ] Then: []
                   If: [d < delta] Then: [delta: d. r:        bci]
                   If: [d = delta] Then: [          r: r max: bci].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ContentsAt: n IfFail: fb = ( |
             assignmentMir = bootstrap stub -> 'globals' -> 'mirrors' -> 'assignment' -> ().
            | 
            (m primitiveIsAssignmentAt: n IfFail: [|:e| ^ fb value: e]) ifTrue: [^ assignmentMir].

               ( m primitiveIsAssignableAt: n  IfFail: [|:e| ^ fb value: e] )
            || [ m primitiveIsArgumentAt:   n  IfFail: [|:e| ^ fb value: e] ]

             ifTrue: [ forActivationMirror: m ContentsOfArgumentOrAssignableSlot: n IfFail: fb ]
              False: [ forActivationMirror: m ContentsOfConstantLocalSlotAt:      n IfFail: fb ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         forActivationMirror: m ContentsOfArgumentOrAssignableSlot: n IfFail: fb = ( |
            | 
            mirrorFor: (m oopForArgumentOrAssignableLocalAt: n
                                                     IfFail: [|:e| ^ fb value: e])
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         forActivationMirror: m ContentsOfConstantLocalSlotAt: n IfFail: fb = ( |
             actMap.
             contentsOop.
            | 
            actMap: m reflectionPrimitives forActivationMirror: m ActivationMapIfFail: [|:e| ^ fb value: e].
            contentsOop: actMap contentsOfConstantSlotNamed: n IfFail: [|:e| ^ fb value: e].

            mirrorFor: contentsOop IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ExpressionStackIfFail: fb = ( |
            | 
            error: 'not implemented yet').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IfDead: blk = ( |
            | 
            m myProcess isAlive not || [isActivationObsolete]
               ifTrue: blk
                False: m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IsArgumentAt: n IfFail: fb = ( |
            | 
            (m reflectionPrimitives forActivationMirror: m ActivationMapIfFail: [|:e| ^ fb value: e])
                  isSlotArgumentNamed: n IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IsAssignableAt: n IfFail: fb = ( |
            | 
            (m reflectionPrimitives forActivationMirror: m ActivationMapIfFail: [|:e| ^ fb value: e])
                  isSlotAssignableNamed: n IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IsAssignmentAt: n IfFail: fb = ( |
            | 
            (m reflectionPrimitives forActivationMirror: m ActivationMapIfFail: [|:e| ^ fb value: e])
                  isSlotAssignmentNamed: n IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IsParentAt: n IfFail: fb = ( |
            | 
            (m reflectionPrimitives forActivationMirror: m ActivationMapIfFail: [|:e| ^ fb value: e])
                  isSlotParentNamed: n IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m LexicalParentIfFail: fb = ( |
            | 
            m isReflecteeBlockMethodActivation ifFalse: [^ fb value: 'noParentError'].
            m ancestorSenderWithSP: (m lexicalParentSPIfFail: [|:e| ^ fb value: e])
                          IfAbsent: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m MethodHolderIfFail: fb = ( |
            | 
            (m nmethodMirrorIfFail: [|:e| ^ fb value: e])
                methodHolderIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m MethodMirrorIfFail: fb = ( |
            | 
            (m nmethodMirrorIfFail: [|:e| ^ fb value: 'no nmethod: ', e])
                methodMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m NMethodMirrorIfFail: fb = ( |
            | 
            m ifDead: [^ fb value: 'dead'].
            m myRegisterLocator ifNil: [^ fb value: 'no nmethod'].
            m myRegisterLocator nmethodMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m NamesIfFail: fb = ( |
            | 
            "for machine-level activations, only slots are fake slots (registers)"
            m isReflecteeMachineLevelForeignActivation ifTrue: [^ vector].

            (m reflectionPrimitives forActivationMirror: m ActivationMapIfFail: [|:e| ^ fb value: e])
                  namesIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m PositionIfFail: fb = ( |
            | 
            m isReflecteeMachineLevelForeignActivation ifTrue: [^ m pcIfFail: fb].

            forActivationMirror: m ByteCodePositionIfFail: [m pcIfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ReceiverIfFail: fb = ( |
            | 
            mirrorFor: (m receiverOopIfFail: [|:e| ^ fb value: e])
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ReceiverOopIfFail: fb = ( |
            | 
            m oopAtSPOffset: (m receiverSPOffsetIfFail: [|:e| ^ fb value: e])
                     IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ReflecteeEq: x IfFail: fb = ( |
            | 
            (m myProcess == x myProcess)
            && [m number = x number]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ReflecteeIdentityHashIfFail: fb = ( |
            | 
            m myProcess hash ^^ m number hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SelectorIfFail: fb = ( |
            | 
            (m selectorMirrorIfFail: [|:e| ^ fb value: e])
              reflecteeStringIfFail:   fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SelectorMirrorIfFail: fb = ( |
            | 
            (m nmethodMirrorIfFail: [|:e| ^ fb value: e])
              selectorMirrorIfFail:    fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SenderIfFail: fb = ( |
             r.
             reg.
             senderRL.
            | 
            senderRL:  m registerLocator senderOf: m myRegisterLocator 
                                         IfAbsent: [|:e| ^ fb value: e].
            r:  senderRL createActivationMirror.
            r isReflecteeMachineLevelForeignActivation: m isReflecteeMachineLevelForeignActivation.
            reg: m allocatedRegionFor:  r pcIfFail: [|:e| ^ fb value: e].
            reg y pred = reg x  ifTrue: [^ fb value: 'not Klein'].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SourceIfFail: fb = ( |
            | 
            (m sourceStringMirrorIfFail: [|:e| ^ fb value: e])
                reflecteeStringIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SourceMirrorIfFail: fb = ( |
            | 
            m isReflecteeMachineLevelForeignActivation ifTrue: [^ reflect: m disassembledSourceStringIfFail: fb].

            (m methodMirrorIfFail:  [|:e| ^ reflect: disassembledSourceStringIfFail: fb])
                sourceStringMirrorIfFail:  [reflect: disassembledSourceStringIfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fCategory: testing\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         isActivationObsolete = ( |
            | 
            myMirror myProcess lens isActivationObsolete: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fCategory: testing\x7fCategory: double dispatch\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         isLocalActivationObsolete = ( |
            | 
            [todo killingActivations].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fCategory: testing\x7fCategory: double dispatch\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         isRemoteActivationObsolete = ( |
            | 
            (myMirror myProcess myProxy isReasonableToCacheIfLastRetrievedAt:
                         myMirror reflectionPrimitives timestampOfOldestCachedItem) not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fCategory: source\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         pcAndInstructionAt: pc IfFail: fb = ( |
             text.
            | 
            text: vmKit disassembledMethodText copyForActivation: myMirror.
            text asString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         stackFrameForActivation: a IfAbsent: ab = ( |
            | 
            a localStackFrameIfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: activations\x7fModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         stackFrameForActivation: a IfAbsent: ab = ( |
            | 
            a remoteStackFrameIfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         kleinActivations = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinActivations' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinActivations' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinActivations.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.17 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinActivations postFileIn
