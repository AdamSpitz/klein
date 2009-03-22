 '$Revision: 30.51 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fCategory: prototypes\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         lookupKey = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein lookupKey.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> () From: ( | {
         'Category: these slots must be listed in if:IsFoundCopyAndReplaceWith:IfFound:\x7fComment: For a    directed resend, this is the object that the resend is aimed at.
For an undirected resend, this is the object holding the method that does the resend.
For a      normal   send, this is 0 instead of nil because 0 is immediate, and so it
                          won\'t create a data relocator, which is expensive (for now :).
                          We could also go back to having the delegateeOrMethodHolder
                          in the sendDesc be optional, but then then there are more
                          dependencies... I don\'t know which is better. -- Adam, 5/04\"\x7fModuleInfo: Module: kleinNMethod InitialContents: InitializeToExpression: (0)'
        
         delegatee <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> () From: ( | {
         'Category: these slots must be listed in if:IsFoundCopyAndReplaceWith:IfFound:\x7fModuleInfo: Module: kleinNMethod InitialContents: InitializeToExpression: (klein lookupType normal)'
        
         lookupType <- klein lookupType normal.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein lookupKey parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         = o = ( |
            | 
                (           selector   =            o selector        )
            && [(         lookupType   =            o lookupType      )
            && [((reflect: delegatee ) =  (reflect: o delegatee)      )]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForNormalSend: sel = ( |
            | 
            copyForSelector: sel
                 LookupType: klein lookupType normal
                  Delegatee: delegateeOrMethodHolderForNormalSend).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSelector: sel LookupType: lt Delegatee: del = ( |
            | 
            ((copy selector: sel)
                 lookupType: lt )
                  delegatee: del).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForSelector: sel LookupType: lt ObjectDoingTheResend: o SlotHolder: h = ( |
            | 
            copy initializeForSelector: sel
                            LookupType: lt
                  ObjectDoingTheResend: o
                            SlotHolder: h).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: private'
        
         delegateeOrMethodHolderForNormalSend = ( |
            | 
            "Just be whatever the lookupKey prototype's default
             delegatee is. -- Adam, 5/04"
            prototype delegatee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            selector hash ^^ lookupType hash ^^ (reflect: delegatee) hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> 'parent' -> () From: ( | {
         'Category: incremental updating\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         if: oldMir IsFoundCopyAndReplaceWith: newMir IfFound: blk = ( |
             r.
            | 
            r: self.

            (reflect: delegatee ) = oldMir  ifTrue: [r: r copy  delegatee: newMir reflectee].
            (reflect: lookupType) = oldMir  ifTrue: [r: r copy lookupType: newMir reflectee].
            (reflect: selector  ) = oldMir  ifTrue: [r: r copy   selector: newMir reflectee].

            == r ifTrue: [self] False: [blk value: r]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeForSelector: sel LookupType: lt ObjectDoingTheResend: o SlotHolder: h = ( |
            | 
            selector: sel.
            lookupType: lt.
            klein lookupType
                    if:                 lt
                    IsNormalSend:       []
                    IsUndirectedResend: [delegatee:  o]
                    IsDirectedResend:   [delegatee:  h value] "Optimization: Let h be a block because looking
                                                               up the slot is slow. -- Adam & Alex, 5/04"
                    IsDelegatedPerform: raiseError.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         isResend = ( |
            | klein lookupType isResend: lookupType).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: private'
        
         prototype = ( |
            | 
            klein lookupKey).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | selector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> () From: ( | {
         'Category: these slots must be listed in if:IsFoundCopyAndReplaceWith:IfFound:\x7fModuleInfo: Module: kleinNMethod InitialContents: InitializeToExpression: (\'\')'
        
         selector <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fCategory: prototypes\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethod = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals byteVector copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein nmethod.

CopyDowns:
globals byteVector. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( | {
         'Category: allocated locations\x7fModuleInfo: Module: kleinNMethod InitialContents: InitializeToExpression: (nil)'
        
         frame.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( | {
         'Category: allocated locations\x7fModuleInfo: Module: kleinNMethod InitialContents: InitializeToExpression: (nil)'
        
         incomingRcvrSPOffset.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( | {
         'Category: recompilation\x7fModuleInfo: Module: kleinNMethod InitialContents: InitializeToExpression: (0)'
        
         invocationCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( | {
         'Category: testing\x7fComment: for mapping; see kleinMapForConversion\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         isKleinNMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: InitializeToExpression: (klein lookupKey)'
        
         lookupKey <- bootstrap stub -> 'globals' -> 'klein' -> 'lookupKey' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( | {
         'Comment: should use slotMethod instead but can\'t get a mirror on Klein side\x7fModuleInfo: Module: kleinNMethod InitialContents: InitializeToExpression: (nil)'
        
         method.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: InitializeToExpression: (nil)'
        
         methodHolder.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( | {
         'Category: allocated locations\x7fModuleInfo: Module: kleinNMethod InitialContents: InitializeToExpression: (0)'
        
         nonVolLocalRegCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein nmethod parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'Category: relocating\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         buildRelocationInfoFrom: rels = ( |
             info <- bootstrap stub -> 'globals' -> 'list' -> ().
            | 
            info: info copyRemoveAll.
            rels do: [|:r| r addRelocationInfoTo: info].
            info asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         collectionName = ( |
            | 
            [klein nmethod]. "browsing"
            'klein nmethod').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            resend.copy copyRelocators lookupKey: lookupKey copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: private'
        
         copyRelocators = ( |
            | 
            relocationInfo: relocationInfo copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         copySize: n FillingWith: f = ( |
            | 
            ((resend.copySize: n FillingWith: f)
             relocationInfo: relocationInfo copy)
                  lookupKey: lookupKey copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'Category: flushing instruction cache\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         flushWhateverCachesAreNecessaryAfterModifyingMe = ( |
            | 
            _FlushMachineCachesFrom: firstKey To: lastKey succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         frameIfNil: ab = ( |
            | 
            frame ifNil: [ ^ ab value].
            frame).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'Category: incrementally updating\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         if: oldMir IsFoundCopyAndReplaceWith: newMir IfFound: blk = ( |
             changedARelocator <- bootstrap stub -> 'globals' -> 'false' -> ().
             newnm.
             rels.
            | 
            "fixup compiledOop in relocators so that the define propagation will work -- dmu 6/04"
            rels: reconstructRelocators.
            rels do: [|:r. :i|
              r if: oldMir IsFoundCopyAndReplaceWith: newMir 
                IfFound: [|:rr| 
                  changedARelocator: true.
                  rels at: i Put: rr
                ]
            ].
            changedARelocator ifTrue: [newnm: copy relocators: rels].
            lookupKey if: oldMir IsFoundCopyAndReplaceWith: newMir
                 IfFound: [|:nlk| newnm: (newnm ifNil: [copy]) lookupKey: nlk].

            (reflect: methodHolder) = oldMir  ifTrue: [ 
                 newnm: (newnm ifNil: [copy]) methodHolder: newMir reflectee 
            ].
            newnm ifNotNil: [^ blk value: newnm ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'Category: sp offsets\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         isSPOffsetRecordedForSlotOfType: slotType = ( |
            | 
            "Duplication with" [isKleinSlotOffsetRecorded].

               (klein slotType isObjectSlot:   slotType)
            || [klein slotType isArgumentSlot: slotType]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'byteVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'Category: relocating\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: private'
        
         reconstructRelocators = ( |
             rels.
             s.
            | 
            rels: list copyRemoveAll.
            s: relocationInfo asSequence.
            [s isEmpty] whileFalse: [|r|
              r: s removeFirst copy.
              r reconstructFromRelocationInfo: s.
              rels add: r.
            ].
            rels asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'Category: relocating\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateOopsBy: delta = ( |
             cg.
             nmOop.
             rels.
            | 
            cg: theVM image cgProto.
            nmOop: theVM image oopForOriginalObject: self IfAbsent: [
              ^ error: 'NMethod cannot be relocated because it has not been linearized'
            ].
            rels: reconstructRelocators.
            rels do: [|:r|
              r relocateOopInNMethod: nmOop By: delta With: cg copyForCompiler: theVM compilerPrototype copy.
            ].
            relocators: rels.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'Category: relocating\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         relocators: rels = ( |
            | 
            relocationInfo: buildRelocationInfoFrom: rels).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            lookupKey selector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> 'parent' -> () From: ( | {
         'Category: filing out\x7fModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         storeStringNeeds = ( |
            | 
            prototype).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: InitializeToExpression: (nil)'
        
         pcOffsetsByBCI.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: FollowSlot'
        
         prototype = ( |
            | klein nmethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'nmethod' -> () From: ( | {
         'Category: allocated locations\x7fModuleInfo: Module: kleinNMethod InitialContents: InitializeToExpression: (vector)'
        
         slotSPOffsets <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: FollowSlot'
        
         kleinNMethod = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinNMethod' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinNMethod' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinNMethod.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinNMethod' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinNMethod' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinNMethod' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinNMethod' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinNMethod' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.51 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinNMethod' -> () From: ( | {
         'ModuleInfo: Module: kleinNMethod InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinNMethod postFileIn
