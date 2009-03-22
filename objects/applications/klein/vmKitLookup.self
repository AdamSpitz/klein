 '$Revision: 30.2 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: klein\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinBaseLookupType = ( |
            | 
            case
              if: [isUndirectedResend] Then: [klein lookupType         resendBase]
              If: [hasDelegatee      ] Then: [klein lookupType directedResendBase]
              If: [isDelegatedPerform] Then: [klein lookupType      delegatedBase]
                                       Else: [klein lookupType         normalBase]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fCategory: lookup info\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         lookup = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( |
             {} = 'Comment: Not used yet. A Self version of the lookup
algorithm used by the Yoda C interpreter, so
that maybe we can someday make a Klein
interpreter.\x7fModuleInfo: Creator: globals kleinAndYoda lookup.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: result\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         addSlotAtIndex: i On: holder WithMap: holderMap = ( |
            | 
            resultType = foundNone ifTrue: [
              slotReference1 index:     i.
              slotReference1 holder:    holder.
              slotReference1 holderMap: holderMap.
              resultType: foundOne.
              ^ self
            ].

            [resultType = foundOne] assert.

                (slotReference1 index      =   i     )
            && [(slotReference1 holder    _Eq: holder)
            && [ slotReference1 holderMap _Eq: holderMap]] ifTrue: [^ self].

            slotReference2 index:     i.
            slotReference2 holder:    holder.
            slotReference2 holderMap: holderMap.
            resultType: foundTwo.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         badOop = ( |
            | 
            vmKit tag mark).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: result\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         contentsOfFoundSlot = ( |
            | 
            contentsOfSlotAtIndex: slotReference1 index In: slotReference1 holder WithMap: slotReference1 holderMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         contentsOfSlotAtIndex: i In: o WithMap: m = ( |
            | 
            m contentsOfSlotAt: i In: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         contentsOfSlotNamed: n In: o = ( |
            | 
            (mapOf: o) contentsOfSlotNamed: n In: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: lookup\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         findObjectOnReceiver: rcvr Selector: sel Holder: holderOfSenderMethod IsUndirectedResend: isUndirectedResend Delegatee: delegatee IsSelfImplicit: isSelfImplicit = ( |
             blt.
             lookupStart.
             lt.
            | 
            case if: isUndirectedResend   Then: [blt: vmKit lookupType         resendBase.  lookupStart:                                    holderOfSenderMethod]
                 If: [delegatee isNotNil] Then: [blt: vmKit lookupType directedResendBase.  lookupStart: contentsOfSlotNamed: delegatee In: holderOfSenderMethod]
                                          Else: [blt: vmKit lookupType         normalBase.  lookupStart:                                                    rcvr].

            lt: blt || (isSelfImplicit ifTrue: [vmKit lookupType implicitSelfBit] False: 0).

            findSlotsOn: lookupStart Selector: sel LookupType: lt.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: lookup\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         findSlotsOn: rcvr Selector: sel LookupType: lt = ( |
            | 
            reentered ifTrue: [_Breakpoint: 'fatal: reentered when should not have in lookup algorithm'].
            reentered: true.

            initialize.

            (vmKit lookupType isUndirectedResend: lt)
               ifTrue: [recursivelyFindSlotsOnParentsOf: rcvr Map: (mapOf: rcvr) Selector: sel]
                False: [recursivelyFindSlotsOn:          rcvr Map: (mapOf: rcvr) Selector: sel].

            reentered: false.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: result\x7fCategory: types\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         foundNone = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'foundNone' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda lookup foundNone.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'foundNone' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         asString = 'foundNone'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'foundNone' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         errorSelector = 'undefinedSelector:Receiver:Type:Delegatee:MethodHolder:Arguments:'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'foundNone' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: result\x7fCategory: types\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         foundOne = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'foundOne' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda lookup foundOne.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'foundOne' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         asString = 'foundOne'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'foundOne' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: result\x7fCategory: types\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         foundTwo = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'foundTwo' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda lookup foundTwo.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'foundTwo' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         asString = 'foundTwo'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'foundTwo' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         errorSelector = 'ambiguousSelector:Receiver:Type:Delegatee:MethodHolder:Arguments:'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'foundTwo' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            resultType: foundNone.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: marking\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         isMarkedAsVisited: o = ( |
             mv.
            | 
            mv: vmKit layouts memoryObject markValueOf: o.
            vmKit layouts mark hasBeenVisitedForLookupField isValueOfWordTrue: mv).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         mapOf: o = ( |
            | 
            "Could dispatch through the lens if we want to run this remotely."
            o _Map).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: marking\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         markAsUnvisited: o = ( |
             mv.
             newMV.
            | 
            mv: vmKit layouts memoryObject markValueOf: o.
            newMV: vmKit layouts mark hasBeenVisitedForLookupField clearBitInWord: mv.
            vmKit layouts memoryObject for: o SetMarkValue: newMV).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: marking\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         markAsVisited: o = ( |
             mv.
             newMV.
            | 
            [todo optimize]. "Could make primitives to do these quicker."
            mv: vmKit layouts memoryObject markValueOf: o.
            newMV: vmKit layouts mark hasBeenVisitedForLookupField setBitInWord: mv.
            vmKit layouts memoryObject for: o SetMarkValue: newMV).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: lookup\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         recursivelyFindSlotsOn: rcvr Map: rcvrMap Selector: sel = ( |
            | 
            (isMarkedAsVisited: rcvrMap) ifTrue: [^ self].
            markAsVisited: rcvrMap.
            rcvrMap indexOfSlotNamed: sel
                           IfPresent: [|:i| addSlotAtIndex: i On: rcvr WithMap: rcvrMap]
                            IfAbsent: [recursivelyFindSlotsOnParentsOf: rcvr Map: rcvrMap Selector: sel].
            markAsUnvisited: rcvrMap.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: lookup\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         recursivelyFindSlotsOnParentsOf: rcvr Map: rcvrMap Selector: sel = ( |
            | 
            rcvrMap parentsOf: rcvr Do: [|:p|
              recursivelyFindSlotsOn: p Map: (mapOf: p) Selector: sel.
              resultType = foundTwo ifTrue: [^ self].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: lookup\x7fModuleInfo: Module: vmKitLookup InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         reentered <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: result\x7fModuleInfo: Module: vmKitLookup InitialContents: InitializeToExpression: (kleinAndYoda lookup foundNone)\x7fVisibility: private'
        
         resultType <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'foundNone' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: result\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         slotReference1 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'slotReference1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda lookup slotReference1.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'slotReference1' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         holder.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'slotReference1' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         holderMap.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'slotReference1' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         index <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'slotReference1' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: result\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         slotReference2 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'slotReference2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda lookup slotReference2.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'slotReference2' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         holder.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'slotReference2' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         holderMap.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'slotReference2' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         index <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> 'slotReference2' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         succeeded = ( |
            | 
            resultType = foundOne).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookup' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fCategory: lookup info\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         lookupType = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda lookupType.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         base: t = ( |
            | 
            t && baseMask).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: constants\x7fCategory: base types\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         baseMask = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: constants\x7fCategory: base types\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         delegatedBase = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: perform types\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         delegatedPerform = ( |
            | 
            delegatedBase).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: constants\x7fCategory: modifier bits\x7fComment: Should be set for DirectedResendLookupType.
Used by the Self VM for:
(1) Error checking (ensuring the delegatee
is static where it should be) 
(2) optimizations - static delegatees need
extra space in the send desc, and lookups
can be short circuited if we know the 
delegatee is static (see senders of
DelegateeStaticBit, isDelegateeStatic in the
Self VM)
-- Ausch 5/05\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         delegateeStaticBit = 8.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: common cases\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         directedResend = ( |
            | 
            directedResendBase || selectorStaticBit || receiverStaticBit || delegateeStaticBit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: constants\x7fCategory: base types\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         directedResendBase = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: for abstractBytecodeInterpreter bc\'s\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytecode: bc = ( |
             b.
             ss.
            | 
            b: case 
             if: [bc hasDelegatee      ] Then: [directedResendBase]
             If: [bc isUndirectedResend] Then: [resendBase]
                                         Else: [normalBase].
            ss:  bc isSelfImplicit ifTrue: implicitSelfBit False: 0.
            b  || selectorStaticBit || ss).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         if: t IsNormalSend: nBlk IsUndirectedResend: uBlk IsDirectedResend: dBlk IsDelegatedPerform: pBlk = ( |
             b.
            | 
            b: base: t.
            case
              if: [b =         normalBase] Then: nBlk
              If: [b =         resendBase] Then: uBlk
              If: [b = directedResendBase] Then: dBlk
              If: [b =      delegatedBase] Then: pBlk
                                           Else: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: common cases\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         implicitSelf = ( |
            | 
            normalBase || implicitSelfBit || selectorStaticBit || receiverStaticBit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: constants\x7fCategory: modifier bits\x7fComment: Used by the Self VM for:
(1) Hashing method lookup keys (for
codeTable lookups)
(2) Used for testing look up type
and entry point into method - make sure
that the settings between look up type
and entry point match
(3) Used throughout the compiler,
interpreter, send desc, to do the
appropriate thing when self is implicit
(look for senders of isSelfImplicit and
isImplicitSelf inside the Self VM)
-- Ausch 5/05\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         implicitSelfBit = 32.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         isDirectedResend: t = ( |
            | 
            (base: t) = directedResendBase).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         isPerform: t = ( |
            | 
            (t && selectorStaticBit) asBoolean).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         isResend: t = ( |
             b.
            | 
            b: base: t.
            (b = resendBase) || [b = directedResendBase]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         isUndirectedResend: t = ( |
            | 
            (base: t) = resendBase).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         lookupReceiverIsSelf: t = ( |
            | 
            (t && implicitSelfBit)
            || [|b|
                b: base: t.
                (b = resendBase) || [b = directedResendBase]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         needsDelegatee: t = ( |
             b.
            | 
            b: base: t.
            (b = directedResendBase) || [b = delegatedBase]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: common cases\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         normal = ( |
            | normalBase || selectorStaticBit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: constants\x7fCategory: base types\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         normalBase = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: perform types\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         normalPerform = ( |
            | 
            normalBase).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: constants\x7fCategory: modifier bits\x7fComment: Used by the Self VM for:
(1) Lookup type testing (some optimizations
in there)
(2) Used to decide whether to generate 
a map check in the prologue node
(3) Used to decide whether we need a real
send or can reuse cached method.\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         receiverStaticBit = 16.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: constants\x7fCategory: base types\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         resendBase = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: perform types\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         resendPerform = ( |
            | 
            resendBase).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: constants\x7fCategory: modifier bits\x7fComment: All regular sends take static selectors,
and all of the performs take non-static ones.
-- Ausch 5/05\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         selectorStaticBit = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: common cases\x7fComment: Used by the self VM exclusively as the type
for the first send desc. Might not be needed
for the Klein VM. -- Ausch 5/05\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         staticNormal = ( |
            | 
            normalBase || selectorStaticBit || receiverStaticBit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'lookupType' -> () From: ( | {
         'Category: common cases\x7fModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         undirectedResend = ( |
            | 
            resendBase || selectorStaticBit || receiverStaticBit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot'
        
         vmKitLookup = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLookup' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLookup' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitLookup.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLookup' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLookup' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLookup' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLookup' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLookup' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.2 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLookup' -> () From: ( | {
         'ModuleInfo: Module: vmKitLookup InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitLookup postFileIn
