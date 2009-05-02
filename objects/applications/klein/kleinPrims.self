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
            kleinAndYoda garbageCollector scavenge.
            _Breakpoint: 'uh-oh, how do we retry the block clone?').
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
             numberOfWordsInABlock = 3.
             oid.
             space.
             vm.
            | 
            _NoMapTest.
            vm: _TheVM.
            oid: vm withoutCloningAnythingGetANewOID.
            space: vm universe allocationSpace.

            "Could use this code to do block cloning at the Self level, but for now is inefficient. -- Adam, 7/06"
            [[todo blockCloning].
              [todo cleanup blockCloning should be calling kleinAndYoda layouts block numberOfWordsInABlock].
              addr: space allocateOops: numberOfWordsInABlock IfFail: blockCloneFailureHandler.

              vm objectLocator withoutCloningAnythingRecordAddress: addr ForOID: oid.
              [todo optimize untaggedAddresses].
              _InitializeBlockAtAddress: (addr _IntLogicalShiftRight: 2) HomeFrame: fp OID: oid IfFail: blockCloneFailureHandler
            ].

            _CloneBlockHomeFrame: fp OID: oid Space: space).
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
         'Category: stubs (receiver may not be klein primitives)\x7fComment: Receiver is callee of message to be sent.\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         compileSlot_stub = ( |
             holderMir.
             key.
             rcvrMir.
             sd.
             ss.
            | 
            _NoMapTest.
            _VariableArguments.

            _Breakpoint: 'nmethod cache miss, about to try a dynamic lookup and compile'.

            [aaaaa]. "Fix the duplication between here and selfVM tests compiling."

            sd: _CallingSendDesc.
            key: klein lookupKey copyForSelector: sd _SendDescSelector
                                      LookupType: sd _SendDescLookupType
                                       Delegatee: sd _SendDescDelegatee.

            key isResend ifTrue: [_Breakpoint: 'Hmm. Do we have enough information to do this?'].

            rcvrMir: _Mirror.

            rcvrMir isReflecteeBlock ifTrue: [_Breakpoint: 'Not gonna work yet; gotta find the selfMir and enclosing scopes.'].

            ss: key lookupSlotsUsing: rcvrMir slotFinder Self: rcvrMir Holder: holderMir.

            ss ifNone: [_Breakpoint: 'lookup failure: no slot']
                IfOne: [|:s. c. nm. map|
                        c: theVM compilerPrototype.
                        c: c copyForContext: (c prototypes compilationContext
                                                         copyForSlot: s
                                                                 Key: key
                                                                Self: rcvrMir
                                                            Receiver: rcvrMir
                                                 LexicalParentScopes: vector)
                                Architecture: theVM architecture
                                      Oracle: oracleForEagerRelocationInsideKlein
                                       Debug: true
                                    Optimize: c prototypes optimizationPolicies compileQuickly.
                        nm: c compileForcingNonLeafIfNecessary buildNMethod.

                        "It would sure make me more comfortable to have
                         a test case that demonstrates that this flushing
                         stuff is actually working. -- Adam, 5/05"
                        nm flushWhateverCachesAreNecessaryAfterModifyingMe.

                        map: _Map.
                        map installNewNMethod: nm.
                        sd _BackpatchSendDescTo: nm _NMethodEntryPoint Map: map.
                        _Breakpoint: 'just dynamically compiled something; about to retry the sendDesc'.
                        sd _RetrySendDesc.
                        _Breakpoint: 'should never reach here'.
                       ]
               IfMany: [_Breakpoint: 'lookup failure: found multiple slots']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: compilation\x7fModuleInfo: Module: kleinPrims InitialContents: InitializeToExpression: (vector copySize: 1 FillingWith: \'nic\')\x7fVisibility: private'
        
         myCompilers <- vector copySize: 1 FillingWith: 'nic'.
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
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: compilation\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: whoCares CompilersIfFail: fb = ( |
            | 
            [ _Compilers           ]. "browsing"
            [ _CompilersIfFail: fb ]. "browsing"
            "Not implemented yet. Just here so that we can run
             the Self tests (which call withAndWithoutInlining)."
            myCompilers).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: byte vectors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: dstByteVector CopyByteRangeDstPos: dstPos Src: srcByteVector SrcPos: srcPos Length: len IfFail: fb = ( |
             byteAtPutFailBlock.
             failBlock.
            | 
            ['' _CopyByteRangeDstPos: 0 Src: '' SrcPos: 0 Length: 0           ]. "browsing"
            ['' _CopyByteRangeDstPos: 0 Src: '' SrcPos: 0 Length: 0 IfFail: fb]. "browsing"

            failBlock:  [^ fail: 'badTypeError'  Name: '_CopyByteRangeDstPos:Src:SrcPos:Length:' Receiver: dstByteVector FailBlock: fb].

            dstByteVector _IsByteVector ifFalse: failBlock.
            srcByteVector _IsByteVector ifFalse: failBlock.
            (isSmi: srcPos)             ifFalse: failBlock.
            (isSmi: dstPos)             ifFalse: failBlock.
            (isSmi: len   )             ifFalse: failBlock.

            byteAtPutFailBlock:  [|:e. :p| ^ fail: 'badIndexError' Name: '_CopyByteRangeDstPos:Src:SrcPos:Length:' Receiver: dstByteVector FailBlock: fb].

            0 upTo: len By: 1 WithoutCloningDo: [|:i|
              dstByteVector _ByteAt: (                       dstPos _IntAdd: i)
                                Put: (srcByteVector _ByteAt: srcPos _IntAdd: i)
                             IfFail: byteAtPutFailBlock.
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

            "Optimization: try it first without cloning an int32
             to stick the value into, in case the result will
             fit into a smi. Only create an int32 if that fails.
             (In the long run, make the primitive do this
             automatically. But that'll be easier with a
             compiler that's a bit smarter about allocating
             registers, because right now it's inconvenient to
             use more than two temp registers. -- Adam, Mar. 2009"

            0              _SetFromInt32: a Add: b IfFail: [|:e. :p|
              int32 _Clone _SetFromInt32: a Add: b IfFail: [|:e. :p|
                fail: e Name: '_Int32:Add:' Receiver: unused FailBlock: fb
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a And: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 And: 0           ]. "browsing"
            [_Int32: 0 And: 0 IfFail: fb]. "browsing"
            0              _SetFromInt32: a And: b IfFail: [|:e. :p|
              int32 _Clone _SetFromInt32: a And: b IfFail: [|:e. :p|
                fail: e Name: '_Int32:And:' Receiver: unused FailBlock: fb
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Cmp: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Cmp: 0           ]. "browsing"
            [_Int32: 0 Cmp: 0 IfFail: fb]. "browsing"
            0              _SetFromInt32: a Cmp: b IfFail: [|:e. :p|
              int32 _Clone _SetFromInt32: a Cmp: b IfFail: [|:e. :p|
                fail: e Name: '_Int32:Cmp:' Receiver: unused FailBlock: fb
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Div: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Div: 0           ]. "browsing"
            [_Int32: 0 Div: 0 IfFail: fb]. "browsing"
            0              _SetFromInt32: a Div: b IfFail: [|:e. :p|
              int32 _Clone _SetFromInt32: a Div: b IfFail: [|:e. :p|
                fail: e Name: '_Int32:Div:' Receiver: unused FailBlock: fb
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Mul: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Mul: 0           ]. "browsing"
            [_Int32: 0 Mul: 0 IfFail: fb]. "browsing"
            0              _SetFromInt32: a Mul: b IfFail: [|:e. :p|
              int32 _Clone _SetFromInt32: a Mul: b IfFail: [|:e. :p|
                fail: e Name: '_Int32:Mul:' Receiver: unused FailBlock: fb
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Or: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Or: 0           ]. "browsing"
            [_Int32: 0 Or: 0 IfFail: fb]. "browsing"
            0              _SetFromInt32: a Or: b IfFail: [|:e. :p|
              int32 _Clone _SetFromInt32: a Or: b IfFail: [|:e. :p|
                fail: e Name: '_Int32:Or:' Receiver: unused FailBlock: fb
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Rem: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Rem: 0           ]. "browsing"
            [_Int32: 0 Rem: 0 IfFail: fb]. "browsing"
            0              _SetFromInt32: a Rem: b IfFail: [|:e. :p|
              int32 _Clone _SetFromInt32: a Rem: b IfFail: [|:e. :p|
                fail: e Name: '_Int32:Rem:' Receiver: unused FailBlock: fb
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Shl: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Shl: 0           ]. "browsing"
            [_Int32: 0 Shl: 0 IfFail: fb]. "browsing"
            0              _SetFromInt32: a Shl: b IfFail: [|:e. :p|
              int32 _Clone _SetFromInt32: a Shl: b IfFail: [|:e. :p|
                fail: e Name: '_Int32:Shl:' Receiver: unused FailBlock: fb
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Shr: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Shr: 0           ]. "browsing"
            [_Int32: 0 Shr: 0 IfFail: fb]. "browsing"
            0              _SetFromInt32: a Shr: b IfFail: [|:e. :p|
              int32 _Clone _SetFromInt32: a Shr: b IfFail: [|:e. :p|
                fail: e Name: '_Int32:Shr:' Receiver: unused FailBlock: fb
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Sub: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Sub: 0           ]. "browsing"
            [_Int32: 0 Sub: 0 IfFail: fb]. "browsing"
            0              _SetFromInt32: a Sub: b IfFail: [|:e. :p|
              int32 _Clone _SetFromInt32: a Sub: b IfFail: [|:e. :p|
                fail: e Name: '_Int32:Sub:' Receiver: unused FailBlock: fb
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Ushr: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Ushr: 0           ]. "browsing"
            [_Int32: 0 Ushr: 0 IfFail: fb]. "browsing"
            0              _SetFromInt32: a Ushr: b IfFail: [|:e. :p|
              int32 _Clone _SetFromInt32: a Ushr: b IfFail: [|:e. :p|
                fail: e Name: '_Int32:Ushr:' Receiver: unused FailBlock: fb
              ]
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: int32\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: unused Int32: a Xor: b IfFail: fb = ( |
             int32 = bootstrap stub -> 'globals' -> 'int32' -> ().
            | 
            [_Int32: 0 Xor: 0           ]. "browsing"
            [_Int32: 0 Xor: 0 IfFail: fb]. "browsing"
            0              _SetFromInt32: a Xor: b IfFail: [|:e. :p|
              int32 _Clone _SetFromInt32: a Xor: b IfFail: [|:e. :p|
                fail: e Name: '_Int32:Xor:' Receiver: unused FailBlock: fb
              ]
            ]).
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
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: compilation\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: whoCares InterpretIfFail: fb = ( |
            | 
            [ _Interpret           ]. "browsing"
            [ _InterpretIfFail: fb ]. "browsing"
            "Not implemented yet. Just here so that we can run
             the Self tests (which call withAndWithoutInlining)."
            false).
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
             stdoutFD = 1.
            | 
            _NoGCAllowed.

            [ _StringPrint           ]. "browsing"
            [ _StringPrintIfFail: fb ]. "browsing"

            (_SystemCall: 4 With: (stdoutFD _IntForC)  With: (str  _ByteVectorForC) With: (str _ByteSize _IntForC)) _IntFromC).
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
            sd _RetrySendDesc.
            _Breakpoint: 'should never reach here'.

            __DefineLabel: 'miss'.

            _Breakpoint: 'miss, about to backpatch to the compileSlot_stub'.
            sd _BackpatchSendDescTo: _EntryAddressOfCompileSlotStub Map: map.
            sd _RetrySendDesc.
            _Breakpoint: 'should never reach here').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         stubAndFakePrimitiveMethodHoldersDo: blk = ( |
            | 
            blk value: self.
            parent stubAndFakePrimitiveMethodHoldersDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> () From: ( | {
         'Category: compiling\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         translatorMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein primitives translatorMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> () From: ( | {
         'Category: failure handling\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         failureHandler = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein primitives translatorMixin failureHandler.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cg.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         dstReg.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         endLabel.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         fbReg.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: InitializeToExpression: (\'\')\x7fVisibility: private'
        
         node <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein primitives translatorMixin failureHandler parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assert: reg1 Equals: reg2 ErrorMessage: m = ( |
            | 
            cg generateExit: [|:okLabel|
              cg generateIf: reg1 Equals: reg2 ThenBranchTo: okLabel.
              fail: m.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assert: reg HasAnyMapTypeIn: maps = ( |
            | 
            cg generateExit: [|:okLabel|
              layouts object generateIf: reg
                                   Temp: dstReg
                        HasAnyMapTypeIn: maps
                           ThenBranchTo: okLabel
                                   With: cg.
              mapTypeError: maps.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assert: reg HasMapType: map = ( |
            | 
            assert: reg HasAnyMapTypeIn:  vector copyAddFirst: map).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assert: smiReg IsBetweenZeroAnd: max = ( |
            | 
            cg generateExit: [|:okLabel|
              cg generateIf: smiReg IsBetweenZeroAnd: max ThenBranchTo: okLabel.
              badValueError: 'a value between 0 and ', max printString, ' inclusive'
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertBlock: reg = ( |
            | 
            assert: reg HasMapType: maps blockMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertBounds: indexSmiReg InByteVector: byteVectReg = ( |
            | 
            cg generateIf: [|:trueFork|
              cg byteVectorLayout generateFor:  byteVectReg
                                      IfIndex:  indexSmiReg
                                         Temp:  dstReg
                    IsOutOfBoundsThenBranchTo:  trueFork
                                         With:  cg.
            ] Then: [
              boundsError.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertBounds: indexSmiReg InVector: objVectReg = ( |
            | 
            cg generateIf: [|:trueFork|
              layouts objVector generateFor:  objVectReg
                                    IfIndex:  indexSmiReg
                                       Temp:  dstReg
                  IsOutOfBoundsThenBranchTo:  trueFork
                                       With:  cg.
            ] Then: [
              boundsError.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertByteVector: reg = ( |
            | 
            cg generateExit: [|:okLabel|
              cg withTemporaryRegisterDo: [|:tempReg|
                cg byteVectorLayout
                                generateIf: reg
                                      Temp: tempReg
                  IsByteVectorThenBranchTo: okLabel
                                      With: cg.
              ].
              typeError: 'byteVector'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertBytesPart: reg = ( |
            | 
            cg generateExit: [|:okLabel|
              layouts object generateIf: reg
                                   Temp: dstReg
                IsBytesPartThenBranchTo: okLabel
                                   With: cg.
              typeError: 'bytesPart'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertFloat: reg = ( |
            | 
            cg generateExit: [|:okLabel|
              layouts object
                         generateIf: reg
                               Temp: dstReg
                IsFloatThenBranchTo: okLabel
                               With: cg.
              typeError: 'float'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertImmediate: reg = ( |
            | 
            cg generateExit: [|:okLabel|
              layouts object
                         generateIf: reg
                               Temp: dstReg
                  IsSmiThenBranchTo: okLabel
                               With: cg.

              layouts object
                         generateIf: reg
                               Temp: dstReg
                IsFloatThenBranchTo: okLabel
                               With: cg.

              typeError: 'immediate'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertInt32: reg = ( |
            | 
            cg generateExit: [|:okLabel|
              layouts object generateIf: reg
                                   Temp: dstReg
                      IsSmiThenBranchTo: okLabel
                                   With: cg.

              cg byteVectorLayout generateIf: reg
                                        Temp: dstReg
                    IsByteVectorThenBranchTo: okLabel
                                        With: cg.

              typeError: 'int32'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertInteger: reg = ( |
            | 
            cg generateExit: [|:okLabel|
              layouts object
                         generateIf: reg
                               Temp: dstReg
                  IsSmiThenBranchTo: okLabel
                               With: cg.
              typeError: 'integer'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertMap: reg = ( |
            | 
            assert: reg HasAnyMapTypeIn: ( maps mapMap
                                         & maps outerActivationMapMap
                                         & maps blockActivationMapMap) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertMemoryObject: reg = ( |
            | 
            cg generateExit: [|:okLabel|
              layouts object
                                 generateIf: reg
                                       Temp: dstReg
                 IsMemoryObjectThenBranchTo: okLabel
                                       With: cg.
              typeError: 'memoryObject'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertNMethod: reg = ( |
            | 
            assert: reg HasAnyMapTypeIn:  vector copyAddFirst: maps nmethodMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertNoOverflow: operationName During: aBlock = ( |
            | 
            cg generateIf: [|:trueFork|
              cg generateIfIntegerOverflowDuring: aBlock ThenBranchTo: trueFork.
            ] Then: [
              overflowError: operationName
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertNotMarkInDstReg = ( |
            | 
            cg generateIf: [|:trueFork|
              cg withTemporaryRegisterDo: [|:tempReg|
                layouts object generateIf: dstReg
                                     Temp: tempReg
                       IsMarkThenBranchTo: trueFork
                                     With: cg.
              ].
            ] Then: [
              markAccessError.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertValidNonMarkTag: tagSmiReg = ( |
            | 
            cg generateIfTag: tagSmiReg
                      IsMark: [markAccessError]
                IsInvalidTag: [badValueError: 'a valid non-mark tag'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         assertVector: reg = ( |
            | 
            assert: reg HasAnyMapTypeIn: ( maps objVectorMap
                                         & maps mapMap
                                         & maps outerActivationMapMap
                                         & maps blockActivationMapMap) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         copyFor: aCodeGenerator Node: n Receiver: r FailBlock: f Dest: d = ( |
            | 
            (((((copy cg: aCodeGenerator) node: n) rcvrReg: r) fbReg: f) dstReg: d) endLabel: aCodeGenerator newLabel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         errors* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> 'errorsMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: errors\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         fail: errorMessage = ( |
            | 
            cg sendErrorTo: fbReg PutResultInto: dstReg Name: node selector Message: errorMessage Node: node.
            cg branchToLabel: endLabel.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         layouts = ( |
            | 
            vmKit layouts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         maps = ( |
            | 
            vmKit maps).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            "Should we do this statically instead? -- Adam, 2/06"
            cg vmKit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'failureHandler' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         rcvrReg.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> () From: ( | {
         'Category: materializing\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         materializeLocsAndFailureHandlerOf: node AndDo: blk = ( |
            | 
            materializeLocsAndFailureHandlerVectorOf: node AndDo: [|:argsToPassIn|
              pass: argsToPassIn Into: blk.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> () From: ( | {
         'Category: materializing\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         materializeLocsAndFailureHandlerVectorOf: node AndDo: blk = ( |
            | 
            materializeDestAndArgsOf: node AndDo: [|:dstReg. :rcvrAndArgRegs. rcvrReg. fbReg. hasExplicitFailBlock|
              rcvrAndArgRegs size > 5  ifTrue: [error: 'unimplemented'].
              hasExplicitFailBlock: 'IfFail:' isSuffixOf: node selector.
              rcvrReg: rcvrAndArgRegs first.
              fbReg: hasExplicitFailBlock ifTrue: [rcvrAndArgRegs last] False: [rcvrReg].
              try: [|:fh. argsToPassIn|
                argsToPassIn:  rcvrAndArgRegs.
                hasExplicitFailBlock ifTrue: [argsToPassIn: argsToPassIn copyWithoutLast].
                argsToPassIn: argsToPassIn copyAddLast: fh.
                argsToPassIn: argsToPassIn copyAddFirst: dstReg.
                blk value: argsToPassIn.
              ] Node: node Receiver: rcvrReg FailBlock: fbReg Dest: dstReg.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> () From: ( | {
         'Category: materializing\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         pass: argsToPassIn Into: blk = ( |
            | 
            blk value: (argsToPassIn at: 0              )
                 With: (argsToPassIn at: 1              )
                 With: (argsToPassIn at: 2 IfAbsent: nil)
                 With: (argsToPassIn at: 3 IfAbsent: nil)
                 With: (argsToPassIn at: 4 IfAbsent: nil)
                 With: (argsToPassIn at: 5 IfAbsent: nil)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> () From: ( | {
         'Category: failure handling\x7fComment: Send an error message to the specified fail block.
\'fbReg\' may be \'nil\' in the case that no fail block
was supplied by the user program.\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         sendErrorTo: fbReg PutResultInto: dstReg Name: name Message: message Node: node = ( |
            | 
            compiler noSendsAllowed ifTrue: [
              breakpoint: message.
            ] False: [|nlr. sel|
              setUpSendArguments: (fbReg
                                & (locationForConstant: message canonicalize)
                                & (locationForConstant:    name canonicalize)) asVector.

              [value: ''                With: '']. "browsing"
              [primitiveFailedError: '' Name: '']. "browsing"
              sel: ('IfFail:' isSuffixOf: node selector) ifTrue: 'value:With:' False: 'primitiveFailedError:Name:'.
              nlr: genNormalCallSelector: sel LiveOopTracker: liveOopTracker copyForNode: node.
              compiler nlrPoints add: nlr.

              moveSendResultTo: dstReg.
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         slotGenerator = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein primitives translatorMixin slotGenerator.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein primitives translatorMixin slotGenerator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: generating\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         autoGeneratePrimitiveMethodForSelector: primitiveMethodName ArgumentCountIncludingReceiver: argumentCountIncludingReceiver On: mir = ( |
             args.
             body.
             destinationMethodName.
             primitiveMethodNameWithIfFail.
             primitiveMethodNameWithoutIfFail.
             r <- ''.
             sourceMethodName.
            | 

            [materializeLocsAndFailureHandlerOf: n AndDo: b]. "browsing"

            destinationMethodName: mir reflectee primitiveGenerationMethodNameForSelector: primitiveMethodName.
            (mir lookupKey: destinationMethodName) isEmpty ifFalse: [^ self].

            primitiveMethodNameWithoutIfFail: primitiveMethodName copyWithoutSuffix: 'IfFail:'.
            primitiveMethodNameWithIfFail: primitiveMethodNameWithoutIfFail, 'IfFail:'.

            sourceMethodName:  autoGeneratingMethodNameForSelector: primitiveMethodName.

            r: r & destinationMethodName & ' n = (\n'.

            r: r & '"this method was auto-generated by" '
                 & '[autoGeneratePrimitiveMethodForSelector: abc
                    ArgumentCountIncludingReceiver: 1
                                                On: mir].\n'.
            r: r & '[ ' & (browsingTagFor: primitiveMethodNameWithoutIfFail) & '              ]. "browsing"\n'.
            r: r & '[ ' & (browsingTagFor: primitiveMethodNameWithIfFail   ) &              ' ]. "browsing"\n'.

            r: r & 'materializeLocsAndFailureHandlerOf: n AndDo: [|'.

            (mir lookupKey: sourceMethodName) ifNone: [
              args: (vector copySize: argumentCountIncludingReceiver succ) copyMappedBy: [|:v. :i| 'arg', i asString].
              body: args last & ' notImplementedYetError: \'' & primitiveMethodNameWithIfFail & '\''.
            ] IfOne: [|:s|
              args: s contents arguments.
              body: (selector copyStr: s name) intersperse: args.
            ] IfMany: [
              error: 'The automatic primitive generator got confused. Please resolve the problem manually.'.
            ].

            args do: [|:a| r: r & ':' & a] SeparatedBy: [r: r & '. '].
            r: r & '|\n'.
            r: r & '  ' & body & '. \n'.

            r: r & '].\n'.
            r: r & 'self)'.

            (reflect: ('(| ' & r & ' |)') flatString eval) do: [|:slot|
              slot category: generatedSlotCategory.
              slot module: 'init'.
              slot comment: 'this method was auto-generated by autoGeneratePrimitiveMethodForSelector:ArgumentCountIncludingReceiver:On:'.
              slot visibility: visibility privateSlot.
              mir addSlot: slot.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: slot names\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         autoGeneratingMethodNameForSelector: primitiveMethodName = ( |
            | 
            autoGeneratingPrefix,
            (primitiveMethodName copyFrom: 1), "rm _"
            (
              ( 'IfFail:' isSuffixOf: primitiveMethodName )
                            ifTrue: ''
                             False: 'IfFail:'
            )).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: slot names\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         autoGeneratingPrefix = ( |
            | 'generatePrimitiveInto:Receiver:').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: browsing tags\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         browsingTagFor: primitiveMethodName = ( |
             sel.
            | 
            sel: selector copyStr: primitiveMethodName.
            sel ifUnary: [primitiveMethodName]
                 Binary: [error: 'primitive method name should not be binary']
                Keyword: [sel intersperse: (sel keywords asVector copyMappedBy: [|:k. :i| 'arg', i asString])]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         copyGenerateSlotsIn: o = ( |
            | 
            (copy targetMirror: o asMirror) generateSlots).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: generating\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         generateBrowsingTagsForGeneratedSlots = ( |
             s <- 'browsingTagsForGeneratedSlots = (
'.
            | 
            primitiveGenerationMethods do: [|:slot| s: s & '[ ' & slot name & ' n ]. "browsing"\n'].
            s: s & 'self)'.

            (reflect: ('(| ' & s & ' |)') flatString eval) do: [|:slot|
              slot category: generatedSlotCategory.
              slot module: 'kleinC1_Gens'.
              slot comment: 'this method was auto-generated by generateBrowsingTagsForGeneratedSlots'.
              slot visibility: visibility privateSlot.
              asMirror addSlot: slot.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: generating\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSlots = ( |
            | 
            (targetMirror ancestorsUpTo: traits clonable asMirror) do: [|:m|
              m do: [|:s|
                (autoGeneratingPrefix isPrefixOf: s name) ifTrue: [|primName|
                  primName: primitiveNameForSourceMethod: s.
                  autoGeneratePrimitiveMethodForSelector: primName
                          ArgumentCountIncludingReceiver: (selector copyStr: primName) numberOfArguments
                                                      On: m.
                ].
              ].
            ].
            modules init beClean.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: categories\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         generatedSlotCategory = 'auto-generated'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: accessing generated slots\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         generatedSlots = ( |
            | 
            (vector copyAddLast: asMirror), (browse descendantsOf: self) gather: [|:m| m asList copyFilteredBy: [|:s| isSlotGenerated: s]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: slot names\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         isPrimitiveGenerationMethod: aSlot = ( |
            | 
            isPrimitiveGenerationSelector: aSlot name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: slot names\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         isPrimitiveGenerationSelector: sel = ( |
            | 
            'generatePrimitive_' isPrefixOf: sel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: categories\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         isSlotGenerated: s = ( |
            | 
            s categories includes: generatedSlotCategory).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: generating\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         primitiveGenerationMethods = ( |
            | 
            (vector copyAddLast: targetMirror), (browse descendantsOf: targetMirror reflectee)
               gather: [|:m| m asList copyFilteredBy: [|:s| isPrimitiveGenerationMethod: s]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: slot names\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         primitiveNameForSourceMethod: s = ( |
            | 
            '_', (s name copyWithoutPrefix: autoGeneratingPrefix)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'Category: removing\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         removeGeneratedSlots = ( |
            | 
            generatedSlots do: [|:s| s holder removeSlot: s name IfFail: []].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> 'slotGenerator' -> () From: ( | {
         'ModuleInfo: Module: kleinPrims InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         targetMirror.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'primitives' -> 'translatorMixin' -> () From: ( | {
         'Category: failure handling\x7fModuleInfo: Module: kleinPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         try: aBlock Node: node Receiver: rcvrReg FailBlock: fbReg Dest: dstReg = ( |
             fh.
             result.
            | 
            fh: failureHandler copyFor: self Node: node Receiver: rcvrReg FailBlock: fbReg Dest: dstReg.
            result:  aBlock value: fh.
            bindLabel: fh endLabel.
            result).
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
