 '$Revision: 30.12 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> () From: ( | {
         'Category: mapping objects\x7fCategory: transmogrifying bytecodes\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         blockInliningBytecodeTransmogrifier = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda objectMapper1 parent bytecodeTransmogrifier copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier.

CopyDowns:
globals kleinAndYoda objectMapper1 parent bytecodeTransmogrifier. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         matches <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: templates\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractTemplate = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'abstractTemplate' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent abstractTemplate.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'abstractTemplate' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'abstractTemplate' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent abstractTemplate parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'abstractTemplate' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: public'
        
         = t = ( |
            | 
            (selector = t selector) && [positionsThatMustBeBlocks = t positionsThatMustBeBlocks]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'abstractTemplate' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: public'
        
         copySelector: s PositionsThatMustBeBlocks: v = ( |
            | 
            (copy selector: s) positionsThatMustBeBlocks: v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'abstractTemplate' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: public'
        
         createMethodWithInlinedBlocksForSendBC: bc Args: rcvrAndArgBCs = ( |
             interpretersByPosition.
             literals.
             locals.
             m.
            | 
            literals: list copyRemoveAll.
            locals:   list copyRemoveAll.
            interpretersByPosition: dictionary copyRemoveAll.
            positionsThatMustBeBlocks do: [|:i. f|
              f: bc interpreter copyForMethod: (rcvrAndArgBCs at: i) oopToPush asMirror method.
              f interpretMethod.
              interpretersByPosition at: i Put: f.
            ].
            interpretersByPosition do: [|:f|
              f originalBytecodes do: [|:fbc| fbc isLiteral                           ifTrue: [literals add: fbc oopToPush]].
              f originalBytecodes do: [|:fbc| fbc isLocal   && [fbc lexicalLevel = 0] ifTrue: [locals   add: fbc slot     ]].
            ].
            m: bc interpreter method.
            literals isEmpty ifFalse: [| combinedLits. oldLits |
              oldLits: m literalsIfFail: [|:e| ^ fb value: e].
              combinedLits: oldLits, (literals copyFilteredBy: [|:lit| oldLits includes: lit]).
              halt. "This doesn't work."
              m: m copyBytecodes: (bc interpreter recreateCodesStringForMethod: m)
                        Literals: combinedLits
                            File: (m     fileIfFail: [|:e| ^ fb value: e])
                            Line: (m     lineIfFail: [|:e| ^ fb value: e])
                          Source: (m   sourceIfFail: [|:e| ^ fb value: e])
                          IfFail:                    [|:e| ^ fb value: e].
            ].
            locals isEmpty ifFalse: [
              m: m copyAddingLocals: locals IfFail: raiseError.
            ].
            [todo blockInlining].
            halt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'abstractTemplate' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            selector hash ^^ positionsThatMustBeBlocks hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'abstractTemplate' -> 'parent' -> () From: ( | {
         'Category: matching\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: public'
        
         isAMatchFor: bc = ( |
            | 
                (bc selector = selector)
            && [| positions |
                positions: bc interpreter receiverAndArgumentBCsOfSendBC: bc.
                positionsThatMustBeBlocks allSatisfy: [|:i| (positions at: i) ifNil: false IfNotNil: [|:argBC| argBC isBlockLiteral]]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'abstractTemplate' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'abstractTemplate' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         positionsThatMustBeBlocks <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'abstractTemplate' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: InitializeToExpression: (\'\')\x7fVisibility: private'
        
         selector <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: templates\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         allTemplates = ( |
            | 
            [todo blockInlining]. "These should probably all be separate kinds of templates, not just abstractTemplate,
                                   so that they can each implement their inlining the way they want to."

            ( (  ifTrueTemplate copySelector: 'ifTrue:'                       PositionsThatMustBeBlocks: vector copyAddLast: 1)
            & ( ifFalseTemplate copySelector: 'ifFalse:'                      PositionsThatMustBeBlocks: vector copyAddLast: 1)
            & (abstractTemplate copySelector: 'ifTrue:False:'                 PositionsThatMustBeBlocks: (1 & 2) asVector)
            & (abstractTemplate copySelector: 'ifFalse:True:'                 PositionsThatMustBeBlocks: (1 & 2) asVector)
            & (abstractTemplate copySelector: 'ifTrue:False:'                 PositionsThatMustBeBlocks: vector copyAddLast: 1)
            & (abstractTemplate copySelector: 'ifTrue:False:'                 PositionsThatMustBeBlocks: vector copyAddLast: 2)
            & (abstractTemplate copySelector: 'ifFalse:True:'                 PositionsThatMustBeBlocks: vector copyAddLast: 1)
            & (abstractTemplate copySelector: 'ifFalse:True:'                 PositionsThatMustBeBlocks: vector copyAddLast: 2)
            & (abstractTemplate copySelector: 'whileTrue:'                    PositionsThatMustBeBlocks: (0 & 1) asVector)
            & (abstractTemplate copySelector: 'whileFalse:'                   PositionsThatMustBeBlocks: (0 & 1) asVector)
            & (abstractTemplate copySelector: 'whileTrue'                     PositionsThatMustBeBlocks: vector copyAddLast: 0)
            & (abstractTemplate copySelector: 'whileFalse'                    PositionsThatMustBeBlocks: vector copyAddLast: 0)
            & (abstractTemplate copySelector: 'loop'                          PositionsThatMustBeBlocks: vector copyAddLast: 0)
            & (abstractTemplate copySelector: 'to:Do:'                        PositionsThatMustBeBlocks: vector copyAddLast: 2)
            & (abstractTemplate copySelector: 'to:By:Do:'                     PositionsThatMustBeBlocks: vector copyAddLast: 3)
            & (abstractTemplate copySelector: '&&'                            PositionsThatMustBeBlocks: vector copyAddLast: 1)
            & (abstractTemplate copySelector: '||'                            PositionsThatMustBeBlocks: vector copyAddLast: 1)
            & (abstractTemplate copySelector: 'if:Then:'                      PositionsThatMustBeBlocks: vector copyAddLast: 2)
            & (abstractTemplate copySelector: 'if:Then:'                      PositionsThatMustBeBlocks: (1 & 2) asVector)
            & (abstractTemplate copySelector: 'if:Then:Else:'                 PositionsThatMustBeBlocks: (    2 & 3) asVector)
            & (abstractTemplate copySelector: 'if:Then:Else:'                 PositionsThatMustBeBlocks: (1 & 2 & 3) asVector)
            & (abstractTemplate copySelector: 'if:Then:If:Then:'              PositionsThatMustBeBlocks: (    2 & 3 & 4) asVector)
            & (abstractTemplate copySelector: 'if:Then:If:Then:'              PositionsThatMustBeBlocks: (1 & 2 & 3 & 4) asVector)
            & (abstractTemplate copySelector: 'if:Then:If:Then:Else:'         PositionsThatMustBeBlocks: (    2 & 3 & 4 & 5) asVector)
            & (abstractTemplate copySelector: 'if:Then:If:Then:Else:'         PositionsThatMustBeBlocks: (1 & 2 & 3 & 4 & 5) asVector)
            & (abstractTemplate copySelector: 'if:Then:If:Then:If:Then:'      PositionsThatMustBeBlocks: (    2 & 3 & 4 & 5 & 6) asVector)
            & (abstractTemplate copySelector: 'if:Then:If:Then:If:Then:'      PositionsThatMustBeBlocks: (1 & 2 & 3 & 4 & 5 & 6) asVector)
            & (abstractTemplate copySelector: 'if:Then:If:Then:If:Then:Else:' PositionsThatMustBeBlocks: (    2 & 3 & 4 & 5 & 6 & 7) asVector)
            & (abstractTemplate copySelector: 'if:Then:If:Then:If:Then:Else:' PositionsThatMustBeBlocks: (1 & 2 & 3 & 4 & 5 & 6 & 7) asVector)
            ) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: templates\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         conditionalTemplate = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'conditionalTemplate' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent abstractTemplate copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'conditionalTemplate' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent conditionalTemplate.

CopyDowns:
globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent abstractTemplate. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'conditionalTemplate' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'conditionalTemplate' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent conditionalTemplate parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'conditionalTemplate' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'abstractTemplate' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: templates\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         ifFalseTemplate = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'ifFalseTemplate' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent conditionalTemplate copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'ifFalseTemplate' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent ifFalseTemplate.

CopyDowns:
globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent conditionalTemplate. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'ifFalseTemplate' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'ifFalseTemplate' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent ifFalseTemplate parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'ifFalseTemplate' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'conditionalTemplate' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: templates\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         ifTrueTemplate = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'ifTrueTemplate' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent conditionalTemplate copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'ifTrueTemplate' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent ifTrueTemplate.

CopyDowns:
globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent conditionalTemplate. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'ifTrueTemplate' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'ifTrueTemplate' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent ifTrueTemplate parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'ifTrueTemplate' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'conditionalTemplate' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            resend.initialize.
            matches:                              matches                              copyRemoveAll.
            poppedBlockLiteralBCs:                poppedBlockLiteralBCs                copyRemoveAll.
            unmatchedSendBCsWithBlockLiteralArgs: unmatchedSendBCsWithBlockLiteralArgs copyRemoveAll.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: matches\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         matchProto = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'matchProto' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent matchProto.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'matchProto' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'matchProto' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent blockInliningBytecodeTransmogrifier parent matchProto parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'matchProto' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForTemplate: t SendBC: bc ReceiverAndArgBCs: args = ( |
            | 
            ((copy template: t) sendBC: bc) receiverAndArgBCs: args).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'matchProto' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: public'
        
         createMethodWithInlinedBlocks = ( |
            | 
            template createMethodWithInlinedBlocksForSendBC: sendBC Args: receiverAndArgBCs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'matchProto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'matchProto' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         receiverAndArgBCs <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'matchProto' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         sendBC.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> 'matchProto' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         template.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'bytecodeTransmogrifier' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fCategory: interpreting particular codes\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot'
        
         pop: bc = ( |
             poppedBC.
            | 
            poppedBC: (poppedBytecodesFor: bc) soleElement.
            poppedBC isBlockLiteral ifTrue: [poppedBlockLiteralBCs add: poppedBC].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> 'parent' -> () From: ( | {
         'Category: interpreting\x7fCategory: interpreting particular codes\x7fModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot'
        
         send: bc = ( |
            | 
            allTemplates findFirst: [|:t| t isAMatchFor: bc]
                         IfPresent: [|:t| matches add: matchProto copyForTemplate: t SendBC: bc ReceiverAndArgBCs: receiverAndArgumentBCsOfSendBC: bc]
                          IfAbsent: [| args. blockArgs |
                                     args: receiverAndArgumentBCsOfSendBC: bc.
                                     blockArgs: args asList copyFilteredBy: [|:argBC| argBC isNotNil && [argBC isBlockLiteral]].
                                     blockArgs isEmpty ifFalse: [unmatchedSendBCsWithBlockLiteralArgs add: bc @ blockArgs]].

            resend.send: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         poppedBlockLiteralBCs <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'blockInliningBytecodeTransmogrifier' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         unmatchedSendBCsWithBlockLiteralArgs <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot'
        
         kleinBlockInlining = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinBlockInlining.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.12 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBlockInlining' -> () From: ( | {
         'ModuleInfo: Module: kleinBlockInlining InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinBlockInlining postFileIn
