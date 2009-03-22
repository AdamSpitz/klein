 '$Revision: 30.4 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: primitives\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primitives = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda primitives.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'ModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         errorsMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> 'errorsMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda primitives errorsMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         fail: e Name: p Receiver: r FailBlock: fb = ( |
            | 
            (fb _Eq: objectToUseIfNoExplicitFailBlock)
               ifTrue: [r  primitiveFailedError: e Name: p]
                False: [fb value:                e With: p]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         fakePrimitiveMethodNameForSelector: s = ( |
             r.
            | 
            r: fakePrimitivePrefix, (s copyWithoutPrefix: '_').
            ('IfFail:' isSuffixOf: r) ifFalse: [r: r, 'IfFail:'].
            r canonicalize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         fakePrimitiveMethodSlots = ( |
             r.
            | 
            r: list copyRemoveAll.
            stubAndFakePrimitiveMethodHoldersDo: [|:h|
              (reflect: h) filterBy: [|:s| fakePrimitivePrefix isPrefixOf: s name] Into: r.
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         fakePrimitivePrefix = 'primReceiver:'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         hasFakePrimitiveForSelector: s = ( |
            | 
            ifSelector: s IsAFakePrimitiveThen: true Else: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         hasFakePrimitiveMethodNamed: n = ( |
            | 
            stubAndFakePrimitiveMethodHoldersDo: [|:h|
              ((reflect: h) includesName: n) ifTrue: [^ true].
            ].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         ifSelector: s IsAFakePrimitiveThen: fpBlk Else: eBlk = ( |
             n.
            | 
            n: fakePrimitiveMethodNameForSelector: s.
            (hasFakePrimitiveMethodNamed: n) ifTrue: [fpBlk value: n]
                                              False:   eBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         isFakePrimitiveSelector: n = ( |
            | 
            fakePrimitivePrefix isPrefixOf: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         isFloat: f = ( |
            | 
            vmKit layouts object isFloat: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         isSmi: n = ( |
            | 
            vmKit layouts object isSmi: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: stubs\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         isStubSelector: n = ( |
            | 
            ('_stub' isSuffixOf: n) || ['_stub:' isSuffixOf: n]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         mapOf: o = ( |
            | o _Map).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         objectToUseIfNoExplicitFailBlock = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> 'objectToUseIfNoExplicitFailBlock' -> () From: ( |
             {} = 'Comment: I am the object passed in as the \'IfFail:\' argument to
a fake primitive if no explicit failblock was passed
in. -- Adam, 4/06\x7fModuleInfo: Creator: globals kleinAndYoda primitives objectToUseIfNoExplicitFailBlock.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'ModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         primReceiver: rcvr MirrorIfFail: fb = ( |
            | 
            [ _Mirror           ]. "browsing"
            [ _MirrorIfFail: fb ]. "browsing"
            _TheVM noncachingMirrorFor: rcvr IfFail: [|:e| fail: e Name: '_Mirror' Receiver: rcvr FailBlock: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         stubAndFakePrimitiveMethodHoldersDo: blk = ( |
            | 
            blk value: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: fake primitives (primitives that are just normal methods)\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         stubAndFakePrimitiveSelectorsDo: blk = ( |
            | 
            stubAndFakePrimitiveMethodHoldersDo: [|:h|
              (reflect: h) names do: [|:n|
                (isFakePrimitiveSelector: n) || [isStubSelector: n] ifTrue: [blk value: n].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'primitives' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitPrims InitialContents: FollowSlot'
        
         vmKitPrims = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitPrims' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitPrims' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitPrims.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitPrims' -> () From: ( | {
         'ModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitPrims' -> () From: ( | {
         'ModuleInfo: Module: vmKitPrims InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitPrims' -> () From: ( | {
         'ModuleInfo: Module: vmKitPrims InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitPrims' -> () From: ( | {
         'ModuleInfo: Module: vmKitPrims InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitPrims' -> () From: ( | {
         'ModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.4 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitPrims' -> () From: ( | {
         'ModuleInfo: Module: vmKitPrims InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitPrims postFileIn
