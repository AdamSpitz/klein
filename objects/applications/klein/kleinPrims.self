 '$Revision: 30.55 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primitives = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein primitives.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: stubs (receiver may not be klein primitives)\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         blockCloneFailureHandler = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'blockCloneFailureHandler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein primitives blockCloneFailureHandler.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'blockCloneFailureHandler' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'blockCloneFailureHandler' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         value: e With: p = ( |
            | 
            kleinAndYoda garbageCollector scavenge).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: cloning objects\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         clone: o = ( |
            | clone: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: cloning objects\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         clone: o IfFail: fb = ( |
            | 
            (mapOf: o) clone: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: cloning objects\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         clone: o IndexableSize: size FillingWith: filler IfFail: fb = ( |
            | 
            (mapOf: o) clone: o IndexableSize: size FillingWith: filler IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: stubs (receiver may not be klein primitives)\x7fComment: Receiver is block to be cloned.\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         cloneBlockHomeFrame_stub: fp = ( |
             addr.
             b.
             blockCloneFailureHandler = bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'blockCloneFailureHandler' -> ().
             numberOfWordsInABlock = 3.
             oid.
             space.
             vm.
            | 
            _NoMapTest.
            vm: _TheVM.
            oid: vm getANewOID.
            space: vm universe allocationSpace.

            "Could use this code to do block cloning at the Self level, but for now is inefficient. -- Adam, 7/06"
            [[todo blockCloning].
              [todo cleanup blockCloning should be calling kleinAndYoda layouts block numberOfWordsInABlock].
              addr: space allocateOops: numberOfWordsInABlock IfFail: blockCloneFailureHandler.

              vm objectLocator withoutCloningAnythingRecordAddress: addr ForOID: oid.
              [todo optimize untaggedAddresses].
              _InitializeBlockAtAddress: (addr _IntLogicalShiftRight: 2) HomeFrame: fp OID: oid IfFail: blockCloneFailureHandler
            ].

            _CloneBlockHomeFrame: fp OID: oid Space: space IfFail: blockCloneFailureHandler).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: stubs (receiver may not be klein primitives)\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         clone_stub = ( |
             kleinPrimitives = bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> ().
            | 
            _NoMapTest.
            kleinPrimitives clone: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: VMs\x7fModuleInfo: Module: kleinPrims InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myTheVM.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: byte vectors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: bv1 ByteVectorCompare: bv2 IfFail: fb = ( |
            | 
            ['' _ByteVectorCompare: ''           ]. "browsing"
            ['' _ByteVectorCompare: '' IfFail: fb]. "browsing"

            bv1   _IsByteVector ifFalse: [^ fail: 'badTypeError' Name: '_ByteVectorCompare:' Receiver: bv1 FailBlock: fb].
            bv2   _IsByteVector ifFalse: [^ fail: 'badTypeError' Name: '_ByteVectorCompare:' Receiver: bv1 FailBlock: fb].

            "This is just duplicated from compare:IfLess:Equal:Greater:.
             Maybe we can find a way to get rid of the duplication. -- Adam, 5/04"
            bv1 == bv2 ifTrue: [ ^ 0 ]. "optimization"
            bv1 with: bv2 Do: [ |:v1. :v2. :k1. :k2|
              v1 asByte compare: v2 asByte
                         IfLess: [^ -1]
                          Equal:     0  "not used"
                        Greater: [^  1].
            ].
            bv1 size compare: bv2 size IfLess: -1 Equal: 0 Greater: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: byte vectors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: bv1 ByteVectorConcatenate: bv2 Prototype: proto IfFail: fb = ( |
             c.
            | 
            ['' _ByteVectorConcatenate: '' Prototype: ''           ]. "browsing"
            ['' _ByteVectorConcatenate: '' Prototype: '' IfFail: fb]. "browsing"

            bv1   _IsByteVector ifFalse: [^ fail: 'badTypeError' Name: '_ByteVectorConcatenate:Prototype:' Receiver: bv1 FailBlock: fb].
            bv2   _IsByteVector ifFalse: [^ fail: 'badTypeError' Name: '_ByteVectorConcatenate:Prototype:' Receiver: bv1 FailBlock: fb].
            proto _IsByteVector ifFalse: [^ fail: 'badTypeError' Name: '_ByteVectorConcatenate:Prototype:' Receiver: bv1 FailBlock: fb].

            c: proto copySize: bv1 size + bv2 size.
            c copyRangeDstPos: 0        SrcArray: bv1 SrcPos: 0 Len: bv1 size IfFail: [|:e. :p| ^ fail: e Name: '_ByteVectorConcatenate:Prototype:' Receiver: bv1 FailBlock: fb].
            c copyRangeDstPos: bv1 size SrcArray: bv2 SrcPos: 0 Len: bv2 size IfFail: [|:e. :p| ^ fail: e Name: '_ByteVectorConcatenate:Prototype:' Receiver: bv1 FailBlock: fb].
            c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: vectors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: v Clone: n Filler: filler IfFail: fb = ( |
            | 
            [ v _Clone: 0 Filler: nil            ]. "browsing"
            [ v _Clone: 0 Filler: nil IfFail: fb ]. "browsing"

            v _IsVector ifFalse: [^ fail: 'badTypeError' Name: '_Clone:Filler:' Receiver: v FailBlock: fb].
            (isSmi: n)  ifFalse: [^ fail: 'badTypeError' Name: '_Clone:Filler:' Receiver: v FailBlock: fb].

            (mapOf: v) clone: v IndexableSize: n FillingWith: filler IfFail: [|:e. :p| ^ fail: e Name: '_Clone:Filler:' Receiver: v FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: byte vectors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: bv CloneBytes: n Filler: filler IfFail: fb = ( |
            | 
            ['' _CloneBytes: 0 Filler: 0           ]. "browsing"
            ['' _CloneBytes: 0 Filler: 0 IfFail: fb]. "browsing"

            bv _IsByteVector ifFalse: [^ fail: 'badTypeError' Name: '_CloneBytes:Filler:' Receiver: bv FailBlock: fb].
            (isSmi: n)       ifFalse: [^ fail: 'badTypeError' Name: '_CloneBytes:Filler:' Receiver: bv FailBlock: fb].

            (mapOf: bv) clone: bv IndexableSize: n FillingWith: filler asByte IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: byte vectors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: dstByteVector CopyByteRangeDstPos: dstPos Src: srcByteVector SrcPos: srcPos Length: len IfFail: fb = ( |
            | 
            ['' _CopyByteRangeDstPos: 0 Src: '' SrcPos: 0 Length: 0           ]. "browsing"
            ['' _CopyByteRangeDstPos: 0 Src: '' SrcPos: 0 Length: 0 IfFail: fb]. "browsing"

            dstByteVector _IsByteVector ifFalse: [^ fail: 'badTypeError' Name: '_CopyByteRangeDstPos:Src:SrcPos:Length:' Receiver: dstByteVector FailBlock: fb].
            srcByteVector _IsByteVector ifFalse: [^ fail: 'badTypeError' Name: '_CopyByteRangeDstPos:Src:SrcPos:Length:' Receiver: dstByteVector FailBlock: fb].
            (isSmi: srcPos)             ifFalse: [^ fail: 'badTypeError' Name: '_CopyByteRangeDstPos:Src:SrcPos:Length:' Receiver: dstByteVector FailBlock: fb].
            (isSmi: dstPos)             ifFalse: [^ fail: 'badTypeError' Name: '_CopyByteRangeDstPos:Src:SrcPos:Length:' Receiver: dstByteVector FailBlock: fb].
            (isSmi: len   )             ifFalse: [^ fail: 'badTypeError' Name: '_CopyByteRangeDstPos:Src:SrcPos:Length:' Receiver: dstByteVector FailBlock: fb].

            len do: [|:i|
              dstByteVector _ByteAt: dstPos + i
                                Put: (srcByteVector byteAt: srcPos + i)
                             IfFail: [|:e. :p| ^ fail: 'badIndexError' Name: '_CopyByteRangeDstPos:Src:SrcPos:Length:' Receiver: dstByteVector FailBlock: fb].
            ].
            dstByteVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: vectors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: dstVector CopyRangeDstPos: dstPos Src: srcVector SrcPos: srcPos Length: len IfFail: fb = ( |
            | 
            [v _CopyRangeDstPos: 0 Src: v2 SrcPos: 0 Length: 0           ]. "browsing"
            [v _CopyRangeDstPos: 0 Src: v2 SrcPos: 0 Length: 0 IfFail: fb]. "browsing"

            dstVector _IsVector ifFalse: [^ fail: 'badTypeError' Name: '_CopyRangeDstPos:Src:SrcPos:Length:' Receiver: dstVector FailBlock: fb].
            srcVector _IsVector ifFalse: [^ fail: 'badTypeError' Name: '_CopyRangeDstPos:Src:SrcPos:Length:' Receiver: dstVector FailBlock: fb].
            (isSmi: srcPos)     ifFalse: [^ fail: 'badTypeError' Name: '_CopyRangeDstPos:Src:SrcPos:Length:' Receiver: dstVector FailBlock: fb].
            (isSmi: dstPos)     ifFalse: [^ fail: 'badTypeError' Name: '_CopyRangeDstPos:Src:SrcPos:Length:' Receiver: dstVector FailBlock: fb].
            (isSmi: len   )     ifFalse: [^ fail: 'badTypeError' Name: '_CopyRangeDstPos:Src:SrcPos:Length:' Receiver: dstVector FailBlock: fb].

            len do: [|:i|
              dstVector at: dstPos + i
                       Put: (srcVector at: srcPos + i)
                  IfAbsent: [^ fail: 'badIndexError' Name: '_CopyRangeDstPos:Src:SrcPos:Length:' Receiver: dstVector FailBlock: fb].
            ].
            dstVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: mirrors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: rcvr CreateBlockMethodBytecodes: bcs Literals: ls File: file Line: line Source: source IfFail: fb = ( |
            | 
            [ _CreateBlockMethodBytecodes: bcs Literals: ls File: f Line: l Source: s            ]. "browsing"
            [ _CreateBlockMethodBytecodes: bcs Literals: ls File: f Line: l Source: s IfFail: fb ]. "browsing"
            [todo mutableReflection].
            _Breakpoint: 'not implemented yet').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: mirrors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: rcvr CreateOuterMethodBytecodes: bcs Literals: ls File: file Line: line Source: source IfFail: fb = ( |
            | 
            [ _CreateOuterMethodBytecodes: bcs Literals: ls File: f Line: l Source: s            ]. "browsing"
            [ _CreateOuterMethodBytecodes: bcs Literals: ls File: f Line: l Source: s IfFail: fb ]. "browsing"
            [todo mutableReflection].
            _Breakpoint: 'not implemented yet').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: objects\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: rcvr CurrentHash: h IfFail: fb = ( |
            | 
            [ p _CurrentHash: h            ]. "browsing"
            [ p _CurrentHash: h IfFail: fb ]. "browsing"
            theVM lastUsedIdentityHash: h _IntSub: 1 IfFail: [|:e. :p| ^ fail: e Name: '_CurrentHash:' Receiver: rcvr FailBlock: fb].
            rcvr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: objects\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: rcvr IdentityHashIfFail: fb = ( |
             markValue.
             newMarkValue.
            | 
            [ _IdentityHash           ]. "browsing"
            [ _IdentityHashIfFail: fb ]. "browsing"

            (isSmi:   rcvr) ifTrue: [^ rcvr].
            (isFloat: rcvr) ifTrue: [^ rcvr asSmallInteger].
            markValue: _MarkValueOfMemoryObject: rcvr.
            klein layouts mark hashOfMarkValue: markValue IfNoneCreateANewOneAndDo: [|:newMarkValue. :newIdentityHash|
              _ForMemoryObject: rcvr SetMarkValue: newMarkValue.
              newIdentityHash
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Add: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Add: 0           ]. "browsing"
            [_Int32: 0 Add: 0 IfFail: fb]. "browsing"
            int32 _Clone _SetFromInt32: a Add: b IfFail: [|:e. :p| fail: e Name: '_Int32:Add:' Receiver: unused FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a And: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 And: 0           ]. "browsing"
            [_Int32: 0 And: 0 IfFail: fb]. "browsing"
            int32 _Clone _SetFromInt32: a And: b IfFail: [|:e. :p| fail: e Name: '_Int32:And:' Receiver: unused FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Cmp: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Cmp: 0           ]. "browsing"
            [_Int32: 0 Cmp: 0 IfFail: fb]. "browsing"
            int32 _Clone _SetFromInt32: a Cmp: b IfFail: [|:e. :p| fail: e Name: '_Int32:Cmp:' Receiver: unused FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Div: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Div: 1           ]. "browsing"
            [_Int32: 0 Div: 1 IfFail: fb]. "browsing"
            int32 _Clone _SetFromInt32: a Div: b IfFail: [|:e. :p| fail: e Name: '_Int32:Div:' Receiver: unused FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Mul: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Mul: 0           ]. "browsing"
            [_Int32: 0 Mul: 0 IfFail: fb]. "browsing"
            int32 _Clone _SetFromInt32: a Mul: b IfFail: [|:e. :p| fail: e Name: '_Int32:Mul:' Receiver: unused FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Or: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Or: 0           ]. "browsing"
            [_Int32: 0 Or: 0 IfFail: fb]. "browsing"
            int32 _Clone _SetFromInt32: a Or: b IfFail: [|:e. :p| fail: e Name: '_Int32:Or:' Receiver: unused FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Rem: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Rem: 1           ]. "browsing"
            [_Int32: 0 Rem: 1 IfFail: fb]. "browsing"
            int32 _Clone _SetFromInt32: a Rem: b IfFail: [|:e. :p| fail: e Name: '_Int32:Rem:' Receiver: unused FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Shl: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Shl: 0           ]. "browsing"
            [_Int32: 0 Shl: 0 IfFail: fb]. "browsing"
            int32 _Clone _SetFromInt32: a Shl: b IfFail: [|:e. :p| fail: e Name: '_Int32:Shl:' Receiver: unused FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Shr: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Shr: 0           ]. "browsing"
            [_Int32: 0 Shr: 0 IfFail: fb]. "browsing"
            int32 _Clone _SetFromInt32: a Shr: b IfFail: [|:e. :p| fail: e Name: '_Int32:Shr:' Receiver: unused FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Sub: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Sub: 0           ]. "browsing"
            [_Int32: 0 Sub: 0 IfFail: fb]. "browsing"
            int32 _Clone _SetFromInt32: a Sub: b IfFail: [|:e. :p| fail: e Name: '_Int32:Sub:' Receiver: unused FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Ushr: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Ushr: 0           ]. "browsing"
            [_Int32: 0 Ushr: 0 IfFail: fb]. "browsing"
            int32 _Clone _SetFromInt32: a Ushr: b IfFail: [|:e. :p| fail: e Name: '_Int32:Ushr:' Receiver: unused FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Xor: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Xor: 0           ]. "browsing"
            [_Int32: 0 Xor: 0 IfFail: fb]. "browsing"
            int32 _Clone _SetFromInt32: a Xor: b IfFail: [|:e. :p| fail: e Name: '_Int32:Xor:' Receiver: unused FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: integers\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: n IntComplementIfFail: fb = ( |
            | 
            [ _IntComplement           ]. "browsing"
            [ _IntComplementIfFail: fb ]. "browsing"
            (isSmi: n) ifFalse: [^ fail: 'badTypeError' Name: '_IntComplement' Receiver: n FailBlock: fb].
            -1 - n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: snapshotting\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: rcvr MemoryWriteSnapshot: n Compress: c Sizes: sizes SaveCode: saveCode IfFail: fb = ( |
            | 
            [ _MemoryWriteSnapshot: n Compress: c Sizes: sizes SaveCode: saveCode            ]. "browsing"
            [ _MemoryWriteSnapshot: n Compress: c Sizes: sizes SaveCode: saveCode IfFail: fb ]. "browsing"
            [todo snapshotting].
            _Breakpoint: 'not implemented yet').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: mirrors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: rcvr MirrorCopyAt: k Put: v IsParent: isParent IsArgument: isArgument Annotation: a IfFail: fb = ( |
            | 
            [ _MirrorCopyAt: k Put: v IsParent: isP IsArgument: isA Annotation: a            ]. "browsing"
            [ _MirrorCopyAt: k Put: v IsParent: isP IsArgument: isA Annotation: a IfFail: fb ]. "browsing"
            [todo mutableReflection].
            _Breakpoint: 'not implemented yet').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: mirrors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: aMirror MirrorReflecteeIfFail: fb = ( |
            | 
            [ m _MirrorReflectee           ]. "browsing"
            [ m _MirrorReflecteeIfFail: fb ]. "browsing"
            aMirror reflectionPrimitives reflecteeOop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: mirrors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: whoCares NakedMethods: b IfFail: fb = ( |
             oldValue.
            | 
            [ m _NakedNethods: b            ]. "browsing"
            [ m _NakedNethods: b IfFail: fb ]. "browsing"
            oldValue: theVM nakedMethods.
            theVM nakedMethods: b.
            oldValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: mirrors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: whoCares NakedMethodsIfFail: fb = ( |
            | 
            [ m _NakedNethods           ]. "browsing"
            [ m _NakedNethodsIfFail: fb ]. "browsing"
            theVM nakedMethods).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: processes\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: rcvr NewProcessSize: n Receiver: r Selector: s Arguments: a IfFail: fb = ( |
            | 
            [ _NewProcessSize: n Receiver: r Selector: s Arguments: a            ]. "browsing"
            [ _NewProcessSize: n Receiver: r Selector: s Arguments: a IfFail: fb ]. "browsing"
            [todo processes].
            _Breakpoint: 'not implemented yet').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: strings\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: str StringCanonicalizeIfFail: fb = ( |
            | 
            [ _StringCanonicalize           ]. "browsing"
            [ _StringCanonicalizeIfFail: fb ]. "browsing"

            "I don't think we can use at:IfAbsentPut: here. We
             need to make sure that what gets put into the set
             is the canonical string, not str. -- Adam, 6/04"
            theVM universe canonicalizedStrings at: str IfAbsent: [|newStr|
              newStr:  '' cloneSize: str size FillingWith: 0.
              newStr copyRangeDstPos: 0 SrcArray: str SrcPos: 0 Len: str size IfFail: [|:e| ^ fail: e Name: '_StringCanonicalize' Receiver: str FailBlock: fb].
              theVM universe canonicalizedStrings add: newStr.
              newStr
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: strings\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: str StringPrintIfFail: fb = ( |
             numberOfWriteSysCall = 4.
             stdoutFD = 1.
            | 
            _NoGCAllowed.

            [ _StringPrint           ]. "browsing"
            [ _StringPrintIfFail: fb ]. "browsing"

            (_SystemCall: numberOfWriteSysCall With: (stdoutFD _IntForC)  With: (str  _ByteVectorForC) With: (str _ByteSize _IntForC)) _IntFromC).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: VMs\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: whoCares TheVM: vm IfFail: fb = ( |
            | 
            [ _TheVM: vm ]. "browsing"
            [ _TheVM: vm IfFail: fb ]. "browsing"
            myTheVM: vm.
            whoCares).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: VMs\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: whoCares TheVMIfFail: fb = ( |
            | 
            [ _TheVM ]. "browsing"
            [ _TheVMIfFail: fb ]. "browsing"
            myTheVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: stubs (receiver may not be klein primitives)\x7fComment: Receiver is callee of message to be sent.\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         sendMessage_stub = ( |
             del.
             i <- 0.
             key.
             lookupKeyPrototype = bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> ().
             lt.
             map.
             maskedLT.
             n.
             nilOop = bootstrap stub -> 'globals' -> 'nil' -> ().
             nm.
             nmc.
             nmethodPrototype = bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> ().
             sd.
             sel.
            | 

            "Keep the sendMessage_stub in sync with klein compilerTestPrograms lookupStub."

            _NoMapTest.
            _VariableArguments.
            _NoGCAllowed.
            _NoSendsAllowed.

            sd: _CallingSendDesc.
            sel: sd _SendDescSelector.

            " for debugging
              _SystemCall: 4 With: (1 _IntForC)  With: (sel  _ByteVectorForC) With: (sel _ByteSize _IntForC).
              _SystemCall: 4 With: (1 _IntForC)  With: ('\n' _ByteVectorForC) With: (1             _IntForC).
            "

            lt:  sd _SendDescLookupType.
            del: sd _SendDescDelegatee.
            map: _Map.
            nmc: map _NMethodCache.
            n: nmc _Size.

            "Maybe use AltiVec for this loop someday. -- Dave and Adam and Alex, 3/04"

            "We're doing a lot of boolean-loading. -- Adam & Alex, 4/04"

            "Loop through the nmethod cache until we find an nmethod
             with a matching lookupKey. We need to compare the selector,
             lookup type, and delegatee. (Note that the 'delegatee'
             for an undirected resend is really the object that holds
             the method that is doing the resend. And for a normal
             lookup, it's always nil. We should really name it
             something better.)"

            maskedLT: lt _MaskedLookupType.

            __DefineLabel: 'loopHead'.
            __BranchIfTrue: (i _IntEQ: n) To: 'miss'.
            nm:  nmc _At: i.
            key:  _In: nm WhichIsOfType: nmethodPrototype Get: 'lookupKey'.
            __BranchIfFalse: ((_In: key WhichIsOfType: lookupKeyPrototype Get: 'selector'  )                   _Eq: sel     ) To: 'notFoundYet'.
            __BranchIfFalse: ((_In: key WhichIsOfType: lookupKeyPrototype Get: 'lookupType') _MaskedLookupType _Eq: maskedLT) To: 'notFoundYet'.
            __BranchIfFalse: ((_In: key WhichIsOfType: lookupKeyPrototype Get: 'delegatee' )                   _Eq: del     ) To: 'notFoundYet'.
            __BranchTo: 'found'.

            __DefineLabel: 'notFoundYet'.
            i: i _IntAdd: 1.
            __BranchTo: 'loopHead'.


            __DefineLabel: 'found'.

            sd _BackpatchSendDescTo: nm _NMethodEntryPoint Map: map.
            "_Breakpoint: 'hit: about to retry sendDesc'."
            sd _RetrySendDesc.
            _Breakpoint: 'should never reach here'.

            __DefineLabel: 'miss'.
            _Breakpoint: 'miss, about to return selector'.
            sel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         stubAndFakePrimitiveMethodHoldersDo: blk = ( |
            | 
            blk value: self.
            parent stubAndFakePrimitiveMethodHoldersDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = bootstrap stub -> 'globals' -> 'klein' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> 'errorsMixin' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         badValueError: expected = ( |
            | 
            fail: 'bad value').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> 'errorsMixin' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         boundsError = ( |
            | 
            fail: 'badIndexError').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> 'errorsMixin' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         fail: errorMessage = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> 'errorsMixin' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         mapTypeError: maps = ( |
             types <- ''.
            | 

            maps do: [|:m| types: types & m mapType] SeparatedBy: [types: types & ' or '].

            typeError: types flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> 'errorsMixin' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         markAccessError = ( |
            | 
            fail: 'requested operation would expose a mark').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> 'errorsMixin' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         notImplementedYetError: name = ( |
            | 
            fail: 'primitiveNotDefinedError').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> 'errorsMixin' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         outOfMemoryError = ( |
            | 
            "This should be the exact string 'outOfMemoryError', for
             compatibility with the Self VM. -- Adam, 5/04"
            fail: 'outOfMemoryError').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> 'errorsMixin' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         overflowError: operationName = ( |
            | 
            "We could provide a more descriptive string here, but
             some existing stuff depends on the string being
             'overflowError'. Is there some other way we could get
             the operationName out to the user? -- Adam, 5/04"
            fail: 'overflowError').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> 'errorsMixin' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         typeError: typeName = ( |
            | 
            "We could provide a more descriptive string here, but
             some existing stuff depends on the string being
             'badTypeError'. Is there some other way we could get
             the typeName out to the user? -- Adam, 5/04"
            fail: 'badTypeError').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> 'errorsMixin' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         valueDoesNotFitIntoSmiError = ( |
            | 
            fail: 'value does not fit into smi').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot'
        
         kleinPrims = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinPrims' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinPrims' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinPrims.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinPrims' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinPrims' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinPrims' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinPrims' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinPrims' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.55 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinPrims' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinPrims postFileIn
