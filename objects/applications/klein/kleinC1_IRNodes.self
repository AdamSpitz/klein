 '$Revision: 30.73 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes abstract.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Comment: The object representing the
source (bytecode) operation
from the bytecode interpreter.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         bc.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         incomingLiveValues.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         outgoingLiveValues.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         < n = ( |
            | bci < n bci).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         = n = ( |
            | == n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         addComment = ( |
            | 
            codeGenerator comment: [commentString].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addInterferenceInformationToValuesWith: la = ( |
            | 
            definedValuesDo: [|:defV|
              outgoingLiveValues do: [|:liveOutV|
                tell: la ToRecordInterferenceBetweenDefinedValue: defV And: liveOutV.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         areVolatilesVaporized = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         basicBlock = ( |
            | 
            "OK, maybe we should just have every node know its basicBlock. Right now
             only the labels know it, so we've gotta backtrack until we hit it."
            sourcePred basicBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         bci = ( |
            | 
            bc ifNil: -1 IfNotNil: [bc pc]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         bindLabel = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         canFallThrough = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         codeGenerator = ( |
            | compiler codeGenerator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         commentString = ( |
            | 
            nodeName, ' ', statePrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         compiler = ( |
            | 
            sourceLevelAllocator compiler).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         controlFlowPredsDo: blk = ( |
            | 
            sourcePred ifNotNil: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         controlFlowSuccsDo: blk = ( |
            | 
            canFallThrough ifTrue: [blk value: sourceSucc].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC = ( |
            | 
            [aBC isNotNil] assert.
            copy bc: aBC).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definesValue: v = ( |
            | 
            definedValuesDo: [|:v2| v = v2 ifTrue: [^ true]].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         doSpecialLivenessUpdating = ( |
            | 
            [aaa]. "Well, this method name is ugly."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         forgeControlFlowLinksUntil: end AlreadySeen: seen = ( |
             leftToDo.
            | 
            "Do this iteratively because I think we overflowed the stack
             doing it recursively on a particularly long method."
            leftToDo: list copyRemoveAll.
            leftToDo add: self.
            [leftToDo isEmpty] whileFalse: [| n |
              n: leftToDo removeFirst.
              n forgeControlFlowPredLinks.
              seen add: n.
              n == end ifFalse: [
                n controlFlowSuccsDo: [|:succ|
                  (seen includes: succ) ifFalse: [
                    leftToDo addFirst: succ.
                  ].
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         forgeControlFlowPredLinks = ( |
            | 
            "Must be overridden for nodes that branch somewhere
             other than to their sourceSucc."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         generateCode = ( |
             n.
            | 
            n: nodeName.
            bindLabel.
            codeGenerator generating: n, ' addComment'           During: [addComment          ].
            codeGenerator generating: n, ' generateSpecificCode' During: [generateSpecificCode].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | identityHash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeLivenessInformation = ( |
            | 
            incomingLiveValues: set copyRemoveAll.
            outgoingLiveValues: set copyRemoveAll.
            valuesThatAreInitiallyKnownToBeLiveDo: [|:v| incomingLiveValues add: v].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: source-order links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         insertAfter: nodeToInsertAfter = ( |
            | 
            sourcePred: nodeToInsertAfter.
            sourceSucc: nodeToInsertAfter sourceSucc.
            sourceSucc ifNotNil: [|:succ| succ sourcePred: self].
            nodeToInsertAfter sourceSucc: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         interpreter = ( |
            | 
            bc interpreter).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         irNodeGenerator = ( |
            | 
            interpreter irNodeGenerator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isBlockLiteral = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isBranch = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isConditionalBranch = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isLabel = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isLocalReturn = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isMove = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isNLRPointEpilogue = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isNonlocalReturn = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isPhiFunction = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isPrimitive = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isReturn = ( |
            | 
            isLocalReturn || [isNonlocalReturn]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isSend = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isUnconditionalBranch = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForConstant: o = ( |
            | 
            machineLevelAllocator locationForConstant: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         locationsThatNeedToBePreserved = ( |
             s.
            | 
            s: set copyRemoveAll.

            outgoingLiveValues do: [|:v| s add: v location].

            "These need to stay live no matter what, I think, because
             the debugger might need to see them. -- Adam, Apr. 2009"
            s add: sourceLevelAllocator locationForIncomingReceiver.
            sourceLevelAllocator namedValues do: [|:v. :n| s add: v location].

            s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         machineLevelAllocator = ( |
            | 
            compiler machineLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         method = ( |
            | 
            sourceLevelAllocator slot contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         newValue = ( |
            | 
            machineLevelAllocator newValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         nodeName = ( |
            | 
            asMirror creatorSlotHint name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'orderedClonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         pcOffsetIfPresent: pb IfAbsent: ab = ( |
            | 
            label isResolved
             ifTrue: [pb value: label resolvedValue - codeGenerator origin]
              False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         recordDefinersAndUsers = ( |
            | 
            definedValuesDo: [|:v| v definers add: self].
               usedValuesDo: [|:v| v users    add: self].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         requiredNumberOfOutgoingRcvrAndArgLocations = ( |
            | 
            0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: special compilation modes\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         setSpecialMode = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         sourceLevelAllocator = ( |
            | 
            interpreter sourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         tell: aLocationAssigner ToRecordInterferenceBetweenDefinedValue: defV And: liveOutV = ( |
            | 
            aLocationAssigner recordInterferenceBetween: defV And: liveOutV.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         updateLivenessInformation = ( |
             changed.
             i.
             o.
            | 
            i: incomingLiveValues size.
            o: outgoingLiveValues size.
            controlFlowSuccsDo: [|:n| outgoingLiveValues addAll: n incomingLiveValues].
            outgoingLiveValues do: [|:v| (definesValue: v) ifFalse: [incomingLiveValues add: v]].
            doSpecialLivenessUpdating.
            changed: (i != incomingLiveValues size) || [o != outgoingLiveValues size].
            changed).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         usedAndDefinedValuesDo: blk = ( |
            | 
               usedValuesDo: blk.
            definedValuesDo: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedLocationsDo: b = ( |
            | 
            usedValuesDo: [|:v| v hasLocation ifTrue: [b value: v location]].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         valuesThatAreInitiallyKnownToBeLiveDo: blk = ( |
            | 
            usedValuesDo: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            compiler vmKit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Category: source-order links\x7fComment: Node preceding me in source (bytecode) order.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         sourcePred.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Category: source-order links\x7fComment: Node succeeding me in source (bytecode) order.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         sourceSucc.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractBranch = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractBranch' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractBranch' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes abstractBranch.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractBranch' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         basicBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractBranch' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractBranch' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes abstractBranch parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractBranch' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         canFallThrough = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractBranch' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractBranch' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         forgeControlFlowPredLinks = ( |
            | 
            controlFlowSuccsDo: [|:n| n controlFlowPreds add: self].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractBranch' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isBranch = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractBranch' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         sendOrPrimitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes sendOrPrimitive.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         basicBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> () From: ( | {
         'Category: source-order links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         firstNodeSettingUpTheSend.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> () From: ( | {
         'Category: source-order links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         lastNodeSettingUpTheSendResult.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> () From: ( | {
         'Category: control flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         nodeToBranchToOnNLR.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes sendOrPrimitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         areVolatilesVaporized = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         arg1Value = ( |
            | 
            rcvrOrArgValueAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         argIndexForReceiver = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentCount = ( |
            | bc argumentCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         canFallThrough = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         controlFlowSuccsDo: blk = ( |
            | 
            [sourceSucc          isNotNil] assert.
            [nodeToBranchToOnNLR isNotNil] assert.
            blk value: sourceSucc.
            blk value: nodeToBranchToOnNLR.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC = ( |
            | 
            (resend.copyBC: aBC) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: nonlocal returns\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         createNodeToBranchToOnNLR = ( |
            | 
            (irNodeGenerator createNewBBBranchingTo: interpreter nlrPointEpilogueBB labelNode BC: bc) labelNode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         createRcvrAndArgValues = ( |
            | 
            (vector copySize: rcvrAndArgCountForGeneratedSend) mapBy: [|:x. :i|
              machineLevelAllocator newValueForOutgoingRcvrAndArgAt: i
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         createResultValue = ( |
            | 
            machineLevelAllocator newValueForIncomingResult).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            blk value: resultValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         findRcvrAndArgStackValues = ( |
             n.
             vs.
            | 
            vs: list copyRemoveAll.
            n: firstNodeSettingUpTheSend.
            [n isMove] whileTrue: [
              vs addLast: n sourceValue.
              n: n sourceSucc.
            ].
            [n = self] assert.
            vs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         findRcvrStackValue = ( |
            | 
            firstNodeSettingUpTheSend sourceValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         findResultStackValue = ( |
            | 
            lastNodeSettingUpTheSendResult destinationValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         forgeControlFlowPredLinks = ( |
            | 
            controlFlowSuccsDo: [|:n| n controlFlowPreds add: self].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            rcvrAndArgValues: createRcvrAndArgValues.
            resultValue: createResultValue.
            nodeToBranchToOnNLR: createNodeToBranchToOnNLR.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = ( |
            | 
            codeGenerator a intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         isSelfExplicit = ( |
            | bc isSelfExplicit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         isSelfImplicit = ( |
            | bc isSelfImplicit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (false)\x7fVisibility: public'
        
         isSpecialCompilationMode = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: nonlocal returns\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         labelToBranchToOnNLR = ( |
            | 
            nodeToBranchToOnNLR label).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         lookupKey = ( |
             del.
             lt.
             sel.
            | 
            sel: selectorForGeneratedSend.
            lt:  lookupType.
            del:
              vmKit lookupType
                      if:                 lt
                      IsNormalSend:       [vmKit lookupKey delegateeOrMethodHolderForNormalSend]
                      IsUndirectedResend: [sourceLevelAllocator context outermostMethodHolder reflectee]
                      IsDirectedResend:   [delegateeObject]
                      IsDelegatedPerform: [delegateeObject].

            vmKit lookupKey copyForSelector: sel
                                 LookupType: lt
                                  Delegatee: del).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         possibleValuesFor: v Do: blk AlreadySeen: seen = ( |
            | 
            [v == resultValue] assert.

            "Later on we could start making particular kinds of primitives
             a bit smarter about this."
            blk value: v kindsOfPossibleValues couldBeAnything).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         rcvrAndArgCountForGeneratedSend = ( |
            | 
            argumentCount + 1 "for receiver").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrAndArgValuesForGeneratedSend = ( |
            | 
            (vector copySize: rcvrAndArgCountForGeneratedSend) mapBy: [|:e. :i|
              machineLevelAllocator newValueForOutgoingRcvrAndArgAt: i
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrAndArgValuesToMoveTo = ( |
            | 
            rcvrAndArgValues).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrLoc = ( |
            | 
            rcvrOrArgLocAt: argIndexForReceiver).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrOrArgLocAt: i = ( |
            | 
            (rcvrOrArgValueAt: i) location).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrOrArgOopValueForConstantLocAt: i = ( |
            | 
            (rcvrOrArgValueAt: i) knownConstantValueIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrOrArgSmiAt: i = ( |
             oop.
            | 
            oop: rcvrOrArgOopValueForConstantLocAt: i.
            (reflect: oop) isReflecteeInteger ifFalse: [error: 'must be integer'].
            oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrOrArgValueAt: i = ( |
            | 
            rcvrAndArgValues at: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         removeFromGraphInPreparationForInlining = ( |
             bb1.
             bb2.
             lastNodeBeforeSettingUpTheSend.
             nodeAfterSettingUpTheSendResult.
            | 
            lastNodeBeforeSettingUpTheSend:  firstNodeSettingUpTheSend      sourcePred.
            nodeAfterSettingUpTheSendResult: lastNodeSettingUpTheSendResult sourceSucc.

            bb1: lastNodeBeforeSettingUpTheSend basicBlock.
            bb2: lastNodeSettingUpTheSendResult basicBlock.
            [bb2 controlFlowPreds size  = 1  ] assert.
            [bb2 controlFlowPreds first = bb1] assert.
            [bb2 immediateDominator     = bb1] assert.

            lastNodeBeforeSettingUpTheSend sourceSucc: nodeAfterSettingUpTheSendResult.
            nodeAfterSettingUpTheSendResult sourcePred: lastNodeBeforeSettingUpTheSend.

            bb1 immediatelyDominatedBBs remove: bb2.
            bb2 immediatelyDominatedBBs do: [|:domBB| domBB recordImmediateDominator: bb1].
            bb1 endWith: bb2 endNode.

            irNodeGenerator setInsertionPoint: lastNodeBeforeSettingUpTheSend.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceDefinedValue: v With: newV = ( |
            | 
            [v = resultValue] assert.
            resultValue: newV.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceUsedValue: v With: newV = ( |
            | 
            [rcvrAndArgValues includes: v] assert.
            rcvrAndArgValues:
               rcvrAndArgValues
                  replaceAllSatisfying: [|:argV| v = argV] With: [newV].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         requiredNumberOfOutgoingRcvrAndArgLocations = ( |
            | 
            rcvrAndArgCountForGeneratedSend).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: selector\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         selector = ( |
            | bc selector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: selector\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         selectorForGeneratedSend = ( |
            | 
            selector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            bc ifNil: '' IfNotNil: [| rcvrAndArgStrings |
              rcvrAndArgStrings: rcvrAndArgValues copyMappedBy: [|:v| v shortPrintString].
              (rcvrAndArgStrings first & ' ' & ((globals selector copyStr: selector) intersperse: rcvrAndArgStrings copyFrom: 1)) flatString
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            rcvrAndArgValuesToMoveTo do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         rcvrAndArgValues.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         resultValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractPrimitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes sendOrPrimitive copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes abstractPrimitive.

CopyDowns:
globals klein compiler1 parent prototypes irNodes sendOrPrimitive. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (false)'
        
         isSpecialCompilationMode <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> () From: ( |
             {} = 'Comment: Notes on the allocation policy:

We used to impose the constraint that the result could not be colocated
with any arguments (enforced in doesPopperInterfereWith:At:), and
furthermore that it could not be assigned any volatile location
in case an argument was materialized into that location (enforced in
doesPusherInterfereWith:At:).

Removing these constraints requires primitives to be more \"well-behaved\"
in their use of the destination locations.  It must only be written to
when the computation has been completed.  However, this simplifies
allocation and can reduce register pressure, yielding better code.
-- jb 8/03\x7fModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes abstractPrimitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            isSpecialCompilationMode ifTrue: [^ self].
            generateCodeForThisKindOfPrimitive.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isPrimitive = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> () From: ( | {
         'Category: special compilation modes\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         setSpecialMode = ( |
             s.
            | 
            [_NoMapTest. _VariableArguments. _SaveAllNonVolatileRegisters. _NoGCAllowed. _NoSendsAllowed]. "browsing"
            s: selector.
            isSpecialCompilationMode: true.
            case if: [s = '_NoMapTest'                  ]  Then: [compiler noMapTest:                         true]
                 If: [s = '_VariableArguments'          ]  Then: [compiler variableArguments:                 true]
                 If: [s = '_SaveAllNonVolatileRegisters']  Then: [compiler shouldSaveAllNonVolatileRegisters: true]
                 If: [s = '_NoGCAllowed'                ]  Then: [compiler noGCAllowed:                       true]
                 If: [s = '_NoSendsAllowed'             ]  Then: [compiler noSendsAllowed:                    true]
                 Else: [
                        isSpecialCompilationMode: false.
                        case if: [s = '_OnNonLocalReturn:'       ]  Then: [compiler hasOnNonLocalReturn: true]
                             If: [s = '_OnNonLocalReturn:IfFail:']  Then: [compiler hasOnNonLocalReturn: true].
                 ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: returns - includes epilogue\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractReturn = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes abstractReturn.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         basicBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         cachedBlockValuesToZap.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         outgoingResultValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes abstractReturn parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         blockValuesToZap = ( |
            | 
            cachedBlockValuesToZap ifNil: [
              cachedBlockValuesToZap: set copyContaining: machineLevelAllocator memoizedBlockValues.
              cachedBlockValuesToZap
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC Result: r = ( |
            | 
            ((copyBC: aBC) outgoingResultValue: r) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
             vs.
            | 
            vs: set copyRemoveAll.
            [aaaaa]. "Hack - I don't think this is the right way to make
                      sure that these values are kept alive."
            vs add: sourceLevelAllocator valueForSelf.
            vs addAll: sourceLevelAllocator namedValues.
            valuesThatNeedToBeVisibleToTheDebuggerAndToBlocks: vs.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceUsedValue: v With: newV = ( |
            | 
            case
              if: [v = outgoingResultValue] Then: [outgoingResultValue: newV]
              If: [valuesThatNeedToBeVisibleToTheDebuggerAndToBlocks includes: v] Then: [
                valuesThatNeedToBeVisibleToTheDebuggerAndToBlocks remove: v.
                valuesThatNeedToBeVisibleToTheDebuggerAndToBlocks add: newV.
              ]
              Else: [
                [willZapMemoizedBlocks] assert.
                blockValuesToZap remove: v.
                blockValuesToZap add: newV.
              ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            blk value: outgoingResultValue.

            valuesThatNeedToBeVisibleToTheDebuggerAndToBlocks do: blk.

            willZapMemoizedBlocks ifTrue: [
              blockValuesToZap do: blk.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         valuesThatNeedToBeVisibleToTheDebuggerAndToBlocks.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: blocks\x7fComment: When pushing a block literal, even though
the bytecodes are the same as pushing 17,
it is necessary to clone the block and set its home scope.
So, make a specialized irNode. -- dmu 2/03\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         blockLiteral = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes blockLiteral.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         aaa_tempValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         incomingMemoizedBlockValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         outgoingMemoizedBlockValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes blockLiteral parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fComment: We might want to send messages while cloning the block.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         areVolatilesVaporized = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         blockMethodMirror = ( |
            | 
            (reflect: blockProto) method).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         blockProto = ( |
            | 
            sourceValue location oopValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         blockProtoLoc = ( |
            | 
            sourceValue location).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC Literal: lit Memoized: m = ( |
            | 
            ((((copyBC: aBC) sourceValue: lit) incomingMemoizedBlockValue: m) outgoingMemoizedBlockValue: m) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            blk value: outgoingMemoizedBlockValue.
            blk value: aaa_tempValue.
            rcvrAndArgValuesForCloneStub do: blk.
            blk value: resultValueForCloneStub.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator generateBlockLiteralNode: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            aaa_tempValue: newValue.
            "Arguments and result of" [cloneBlockHomeFrame_stub: 0].
            rcvrAndArgValuesForCloneStub: (vector copySize: 2) mapBy: [|:x. :i| machineLevelAllocator newValueForOutgoingRcvrAndArgAt: i].
                 resultValueForCloneStub: machineLevelAllocator newValueForIncomingResult.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isBlockLiteral = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: local liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfLocalsAccessedIn: mm = ( |
             f.
            | 
            f: localAccessFinder copyForMethod: mm.
            f interpretMethod.
            f namesOfLocalsAccessed).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: local liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfUplevelLocalsAccessedIn: mm = ( |
             mmNames.
             s.
            | 
            s: (namesOfLocalsAccessedIn: mm) asSet.
            mm blockLiteralsDo: [|:b|
              s addAll: namesOfUplevelLocalsAccessedIn: (reflect: b) method.
            ].
            mmNames: mm names.
            s copyFilteredBy: [|:n| (mmNames includes: n) not]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         possibleValuesFor: v Do: blk AlreadySeen: seen = ( |
            | 
            case
              if: v == outgoingMemoizedBlockValue Then: [blk value: v kindsOfPossibleValues clonedBlock copyForLiteral: sourceValue]
              Else: [halt] "what is it?").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         rcvrAndArgCountForGeneratedSend = ( |
            | 
            "receiver and arg for the call to" [cloneBlockHomeFrame_stub: 0].
            2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceDefinedValue: v With: newV = ( |
            | 
            [aaaaa]. "It'd be nice if I didn't have to write these methods separately
                      for each kind of node."

            case
              if: [v = outgoingMemoizedBlockValue] Then: [outgoingMemoizedBlockValue: newV]
              If: [v =              aaa_tempValue] Then: [             aaa_tempValue: newV]
              If: [v =    resultValueForCloneStub] Then: [   resultValueForCloneStub: newV]
              Else: [[rcvrAndArgValuesForCloneStub includes: v] assert.
                     rcvrAndArgValuesForCloneStub:
                      rcvrAndArgValuesForCloneStub replaceAllSatisfying: [|:argV| argV = v] With: [newV]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceUsedValue: v With: newV = ( |
            | 
            case
              if: [v = incomingMemoizedBlockValue] Then: [incomingMemoizedBlockValue: newV]
              If: [v =                sourceValue] Then: [               sourceValue: newV]
              Else: [error: 'what value is that?']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: allocating\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         requiredNumberOfOutgoingRcvrAndArgLocations = ( |
            | 
            "rcvr + arg for" [cloneBlockHomeFrame_stub: fp]. "browsing"
            2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            blk value: incomingMemoizedBlockValue.
            blk value: sourceValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         rcvrAndArgValuesForCloneStub.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         resultValueForCloneStub.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         sourceValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         breakpoint = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'breakpoint' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'breakpoint' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes breakpoint.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'breakpoint' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'breakpoint' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes breakpoint parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'breakpoint' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         addComment = ( |
            | 
            "Don't bother, there'll be one right after the trap."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'breakpoint' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC String: s = ( |
            | (copyBC: aBC) stringToDisplay: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'breakpoint' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'breakpoint' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator breakpoint: stringToDisplay.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'breakpoint' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'breakpoint' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            ('"' & stringToDisplay & '"') flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'breakpoint' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'breakpoint' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (\'\')'
        
         stringToDisplay <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         comment = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'comment' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'comment' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes comment.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'comment' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (\'\')'
        
         commentString <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'comment' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'comment' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes comment parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'comment' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC String: s = ( |
            | (copyBC: aBC) commentString: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'comment' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'comment' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'comment' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'comment' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            ('"' & commentString & '"') flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'comment' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         conditionalBranch = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstractBranch copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes conditionalBranch.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstractBranch. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         comparison.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> () From: ( | {
         'Category: control flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         falseForkNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         objValue1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         objValue2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes conditionalBranch parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         controlFlowSuccsDo: blk = ( |
            | 
            blk value:  trueForkNode.
            blk value: falseForkNode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC TrueFork: tf FalseFork: ff Obj1: o1 Obj2: o2 Comparison: comp = ( |
            | 
            (((((copyBC: aBC)
                   trueForkNode: tf)
                  falseForkNode: ff)
                      objValue1: o1)
                      objValue2: o2)
                     comparison: comp).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            trueForkNode label isResolved || [falseForkNode label isResolved] ifTrue: [genStackCheck].
            codeGenerator materializeSource: objValue1 location AndDo: [|:reg1|
              codeGenerator materializeSource: objValue2 location AndDo: [|:reg2|
                comparison generateIfTrueFor: reg1 With: reg2 ThenBranchTo: trueForkNode label With: codeGenerator.
                [aaaaa]. "Need to optimize this away when the falseForkNode is next."
                codeGenerator branchToLabel: falseForkNode label.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isConditionalBranch = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractBranch' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceUsedValue: v With: newV = ( |
            | 
            case
              if: [v = objValue1] Then: [objValue1: newV]
              If: [v = objValue2] Then: [objValue2: newV]
              Else: [error: 'what value is that?']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
             r <- ''.
            | 
            r: r & (objValue1 ifNil: 'argle' IfNotNil: [|:v| v shortPrintString]).
            r: r & ' ' & (comparison ifNil: 'op' IfNotNil: [|:c| c operatorString]) & ' '.
            r: r & (objValue2 ifNil: 'bargle' IfNotNil: [|:v| v shortPrintString]).
            r: r & ' ? ' & ( trueForkNode ifNil: 'T' IfNotNil: [|:n| n statePrintString]).
            r: r & ' : ' & (falseForkNode ifNil: 'F' IfNotNil: [|:n| n statePrintString]).
            r flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            blk value: objValue1.
            blk value: objValue2.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'conditionalBranch' -> () From: ( | {
         'Category: control flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         trueForkNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data slots\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         dataSlotAccessOrAssignment = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccessOrAssignment' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccessOrAssignment' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes dataSlotAccessOrAssignment.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccessOrAssignment' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         dataValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccessOrAssignment' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         holderValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccessOrAssignment' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         mySlot.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccessOrAssignment' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccessOrAssignment' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes dataSlotAccessOrAssignment parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccessOrAssignment' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: bc Slot: s Data: d Holder: h = ( |
            | (((copyBC: bc) mySlot: s) dataValue: d) holderValue: h).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccessOrAssignment' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data slots\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         constantDataSlotAccess = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'constantDataSlotAccess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'holderValue' From:
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes dataSlotAccessOrAssignment copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'constantDataSlotAccess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes constantDataSlotAccess.

CopyDowns:
globals klein compiler1 parent prototypes irNodes dataSlotAccessOrAssignment. copy 
SlotsToOmit: holderValue parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'constantDataSlotAccess' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'constantDataSlotAccess' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes constantDataSlotAccess parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'constantDataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: bc Slot: s Data: d = ( |
            | ((copyBC: bc) mySlot: s) dataValue: d).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'constantDataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         dataConstantLocation = ( |
            | 
            locationForConstant: mySlot contents reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'constantDataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            blk value: dataValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'constantDataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator
                moveLocation: dataConstantLocation
                  ToLocation: dataValue location.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'constantDataSlotAccess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccessOrAssignment' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'constantDataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         possibleValuesFor: v Do: blk AlreadySeen: seen = ( |
            | 
            [v == dataValue] assert.

            blk value: dataConstantLocation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'constantDataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceDefinedValue: v With: newV = ( |
            | 
            case
              if: [v = dataValue] Then: [dataValue: newV]
              Else: [error: 'what value is that?']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'constantDataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            mySlot ifNil: '' IfNotNil: [|:s|
              (dataValue statePrintString & ': ' & s name) flatString
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'constantDataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data slots\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlotAccess = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes dataSlotAccessOrAssignment copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes dataSlotAccess.

CopyDowns:
globals klein compiler1 parent prototypes irNodes dataSlotAccessOrAssignment. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccess' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccess' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes dataSlotAccess parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            blk value: dataValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator
                loadFromDataSlot: mySlot
              OfHolderInLocation: holderValue location
                    IntoLocation: dataValue location.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccessOrAssignment' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         possibleValuesFor: v Do: blk AlreadySeen: seen = ( |
            | 
            [v == dataValue] assert.

            blk value: v kindsOfPossibleValues couldBeAnything).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceDefinedValue: v With: newV = ( |
            | 
            case
              if: [v = dataValue] Then: [dataValue: newV]
              Else: [error: 'what value is that?']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceUsedValue: v With: newV = ( |
            | 
            case
              if: [v = holderValue] Then: [holderValue: newV]
              Else: [error: 'what value is that?']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            mySlot ifNil: '' IfNotNil: [|:s|
              (dataValue statePrintString & ': ' & s name & ' of holder ' & holderValue statePrintString) flatString
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccess' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            blk value: holderValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data slots\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlotAssignment = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAssignment' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes dataSlotAccessOrAssignment copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAssignment' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes dataSlotAssignment.

CopyDowns:
globals klein compiler1 parent prototypes irNodes dataSlotAccessOrAssignment. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAssignment' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAssignment' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes dataSlotAssignment parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAssignment' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAssignment' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator storeIntoDataSlot: mySlot
                         OfHolderInLocation: holderValue location
                               FromLocation: dataValue location.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAssignment' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAccessOrAssignment' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAssignment' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceUsedValue: v With: newV = ( |
            | 
            case
              if: [v =   dataValue] Then: [  dataValue: newV]
              If: [v = holderValue] Then: [holderValue: newV]
              Else: [error: 'what value is that?']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAssignment' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            mySlot ifNil: '' IfNotNil: [|:s|
              (s name & ' of holder ' & holderValue statePrintString & ': ' & dataValue statePrintString) flatString
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'dataSlotAssignment' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            blk value: holderValue.
            blk value: dataValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: interrupt points\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         interruptPoint = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'interruptPoint' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'interruptPoint' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes interruptPoint.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'interruptPoint' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'interruptPoint' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes interruptPoint parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'interruptPoint' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'interruptPoint' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator genStackCheck.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'interruptPoint' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'interruptPoint' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         label = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'sourcePred' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes label.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent sourcePred.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         basicBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         cachedLabel.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> () From: ( | {
         'Category: control flow links\x7fComment: Set of irNodes that branch to me.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         controlFlowPreds <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes label parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         addComment = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         bindLabel = ( |
            | 
            codeGenerator bindLabel: label.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         controlFlowPredsDo: blk = ( |
            | 
            controlFlowPreds do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            resend.copy controlFlowPreds: controlFlowPreds copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isLabel = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         label = ( |
            | 
            cachedLabel ifNil: [
              cachedLabel: codeGenerator newLabel whereDidIComeFrom: self.
              cachedLabel
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         sourcePred: n = ( |
            | 
            "Labels have controlFlowPreds instead of a sourcePred."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            basicBlock statePrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'label' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: returns - includes epilogue\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         localReturn = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstractReturn copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes localReturn.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstractReturn. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes localReturn parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         canFallThrough = ( |
            | 
            sourceLevelAllocator isInlined).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            needsEpilogue ifTrue: [codeGenerator generateEpilogue: self].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isLocalReturn = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needsEpilogue = ( |
            | 
            sourceLevelAllocator isInlined not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         willZapMemoizedBlocks = ( |
            | 
            sourceLevelAllocator isInlined not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: moves\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         move = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes move.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (false)'
        
         aaa_doNotCoalesce <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         destinationValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes move parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC Source: s Destination: d = ( |
            | ((copyBC: aBC) sourceValue: s) destinationValue: d).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            blk value: destinationValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         doSpecialLivenessUpdating = ( |
            | 
            (outgoingLiveValues includes: destinationValue) ifTrue: [
              incomingLiveValues add: sourceValue.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator moveLocation:      sourceValue location
                            ToLocation: destinationValue location.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isMove = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         possibleValuesFor: v Do: blk AlreadySeen: seen = ( |
            | 
            [v == destinationValue] assert.
            sourceValue possibleValuesDo: blk AlreadySeen: seen.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceDefinedValue: v With: newV = ( |
            | 
            case
              if: [v = destinationValue] Then: [destinationValue: newV]
              Else: [error: 'what value is that?']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceUsedValue: v With: newV = ( |
            | 
            case
              if: [v = sourceValue] Then: [sourceValue: newV]
              Else: [error: 'what value is that?']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            (sourceValue ifNil: '?' IfNotNil: [|:sv| sv statePrintString]),
            ' to ',
            (destinationValue ifNil: '?' IfNotNil: [|:dv| dv statePrintString])).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         tell: aLocationAssigner ToRecordInterferenceBetweenDefinedValue: defV And: liveOutV = ( |
            | 
            sourceValue = liveOutV ifTrue: [^ self].
            resend.tell: aLocationAssigner ToRecordInterferenceBetweenDefinedValue: defV And: liveOutV).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            blk value: sourceValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         valuesThatAreInitiallyKnownToBeLiveDo: blk = ( |
            | 
            "sourceValue is not necessarily live."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'move' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         sourceValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: returns - includes epilogue\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nlrHomeScope = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrHomeScope' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrHomeScope' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes nlrHomeScope.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrHomeScope' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         destinationValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrHomeScope' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrHomeScope' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes nlrHomeScope parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrHomeScope' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC Destination: dst = ( |
            | (copyBC: aBC) destinationValue: dst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrHomeScope' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            blk value: destinationValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrHomeScope' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator generateNLRHomeScopeOf: sourceLevelAllocator Into: destinationValue location.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrHomeScope' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrHomeScope' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceDefinedValue: v With: newV = ( |
            | 
            case
              if: [v = destinationValue] Then: [destinationValue: newV]
              Else: [error: 'what value is that?']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrHomeScope' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            blk value: sourceLevelAllocator valueForIncomingReceiver.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: returns - includes epilogue\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nlrPointEpilogue = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes nlrPointEpilogue.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         basicBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         cachedBlockValuesToZap.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes nlrPointEpilogue parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         blockValuesToZap = ( |
            | 
            cachedBlockValuesToZap ifNil: [
              cachedBlockValuesToZap: set copyContaining: machineLevelAllocator memoizedBlockValues.
              cachedBlockValuesToZap
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         canFallThrough = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC = ( |
            | 
            (resend.copyBC: aBC) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator generateNLRPoints: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isNLRPointEpilogue = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceUsedValue: v With: newV = ( |
            | 
            blockValuesToZap remove: v.
            blockValuesToZap add: newV.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nlrPointEpilogue' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            blockValuesToZap do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: returns - includes epilogue\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nonlocalReturn = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstractReturn copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes nonlocalReturn.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstractReturn. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes nonlocalReturn parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         canFallThrough = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator generateNLR: self.
            "normally the finish node would generateNLRPoints.
             But this node has no control flow successor, so the finish node
             will never generate code. -- dmu 5/04"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isNonlocalReturn = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractReturn' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         willZapMemoizedBlocks = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fCategory: primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         primitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstractPrimitive copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes primitive.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstractPrimitive. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes primitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         createRcvrAndArgValues = ( |
            | 
            (vector copySize: argumentCount + 1) mapBy: [newValue]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         createResultValue = ( |
            | 
            newValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCodeForThisKindOfPrimitive = ( |
            | 
            codeGenerator generatePrimitiveNode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         locationsThatNeedToBePreserved = ( |
             s.
            | 
            s: resend.locationsThatNeedToBePreserved.

            "For primitives, popped values don't get moved to
             the outgoing registers; they're just used directly.
             So they need to be considered live. -- Adam, Mar. 2009"
            usedLocationsDo: [|:loc| s add: loc].

            s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: allocating data locations\x7fComment: Failure blocks take two arguments: an error string and the
name of the primitive.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         numberOfLocationsNeededForTheFailBlockAndItsArguments = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: materializing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         rcvrAndArgCountToBeMaterialized = ( |
            | 
            argumentCount + 1 "for receiver").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: materializing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrAndArgLocsToBeMaterialized = ( |
            | rcvrAndArgValues copyMappedBy: [|:v| v location]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: allocating data locations\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         requiredNumberOfOutgoingRcvrAndArgLocations = ( |
            | 
            numberOfLocationsNeededForTheFailBlockAndItsArguments).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         resultLoc = ( |
            | 
            resultValue location).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fCategory: primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         onNonLocalReturnPrimitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'onNonLocalReturnPrimitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes primitive copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'onNonLocalReturnPrimitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes onNonLocalReturnPrimitive.

CopyDowns:
globals klein compiler1 parent prototypes irNodes primitive. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'onNonLocalReturnPrimitive' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         homeScopeValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'onNonLocalReturnPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'onNonLocalReturnPrimitive' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes onNonLocalReturnPrimitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'onNonLocalReturnPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addInterferenceInformationToValuesWith: la = ( |
            | 
            resend.addInterferenceInformationToValuesWith: la.
            [aaaaa]. "Hack; I think this won't be necessary after we make this
                      primitive translate to lower-level IR nodes. But for now,
                      gotta make sure the homeScopeValue isn't allocated to the
                      same register as the rcvr and arg values. -- Adam, Apr. 2009"
            rcvrAndArgValuesToMoveTo do: [|:v| tell: la ToRecordInterferenceBetweenDefinedValue: homeScopeValue And: v].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'onNonLocalReturnPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            blk value: homeScopeValue.
            resend.definedValuesDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'onNonLocalReturnPrimitive' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            homeScopeValue: newValue.
            resend.initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'onNonLocalReturnPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isTheRightKindOfNodeForSelector: s = ( |
            | 
            ('_OnNonLocalReturn:' = s) || ['_OnNonLocalReturn:IfFail:' = s]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'onNonLocalReturnPrimitive' -> 'parent' -> () From: ( | {
         'Category: nonlocal returns\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         labelToBranchToOnNLR = ( |
            | 
            specialLabelToBranchToOnNLR ifNil: [resend.labelToBranchToOnNLR]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'onNonLocalReturnPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'onNonLocalReturnPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceDefinedValue: v With: newV = ( |
            | 
            case
              if: [v = homeScopeValue] Then: [homeScopeValue: newV]
              Else: [resend.replaceDefinedValue: v With: newV]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'onNonLocalReturnPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         specialLabelToBranchToOnNLR.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fCategory: primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         performPrimitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstractPrimitive copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes performPrimitive.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstractPrimitive. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         delegateeValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (0)'
        
         lookupType <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes performPrimitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         addInterferenceInformationToValuesWith: la = ( |
            | 
            resend.addInterferenceInformationToValuesWith: la.
            [aaaaa]. "This could probably be eliminated if we break down the perform node into multiple nodes."
            tell: la ToRecordInterferenceBetweenDefinedValue: sendDescValue And: selectorValue.
            tell: la ToRecordInterferenceBetweenDefinedValue: sendDescValue And: delegateeValue.
            tell: la ToRecordInterferenceBetweenDefinedValue: selectorValue And: delegateeValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            blk value: sendDescValue.
            resend.definedValuesDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         delegateeArgIndex = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         delegateeLoc = ( |
            | 
            delegateeValue location).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         delegateeObject = ( |
            | 
            delegateeLoc isConstant ifTrue: [delegateeLoc oopValue]
                                     False: [0 "will be patched"]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         firstArgIndex = ( |
            | 
            isDirectedResend ifTrue: [delegateeArgIndex succ]
                              False: [ selectorArgIndex succ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCodeForThisKindOfPrimitive = ( |
            | 
            codeGenerator generatePerformNode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            initializeLookupType.
            initializeValues.
            resend.initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeLookupType = ( |
            | 
            lookupType:
              (globals selector copyStr: selector)
                      ifNormalPerform: [vmKit lookupType    normalPerform]
                      IfResendPerform: [vmKit lookupType    resendPerform]
                   IfDelegatedPerform: [vmKit lookupType delegatedPerform]
                                 Else: [vmKit lookupType           normal].

            [todo delegatedPerform].
            lookupType = vmKit lookupType delegatedPerform  ifTrue: [
              warning: 'Compiling a delegated perform.
                        Since this is not yet implemented, it won\'t work at runtime'
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeValues = ( |
            | 
            sendDescValue: newValue.
            selectorValue: newValue.
            delegateeValue:
              case
               if: [isDirectedResend  ] Then: [newValue]
               If: [isUndirectedResend] Then: [sourceLevelAllocator valueForConstant: sourceLevelAllocator context outermostMethodHolder reflectee]
                                        Else: [sourceLevelAllocator valueForConstant: 0].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isCompletelyStatic = ( |
            | 
            selectorLoc isConstant && [delegateeLoc isConstant]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         isDirectedResend = ( |
            | 
            vmKit lookupType isDirectedResend: lookupType).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isTheRightKindOfNodeForSelector: s = ( |
            | 
            (globals selector copyStr: s) isAPerform).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         isUndirectedResend = ( |
            | 
            vmKit lookupType isUndirectedResend: lookupType).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         rcvrAndArgCountForGeneratedSend = ( |
            | 
              resend.rcvrAndArgCountForGeneratedSend
            - 1                          "for the selector"
            - isDirectedResend asInteger "for the delegatee").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrAndArgValuesToMoveTo = ( |
             r.
             rcvrValue.
            | 
            r: rcvrAndArgValues.
            rcvrValue: r first.
            r: r copyFrom: 1.
            isDirectedResend ifTrue: [r: r copyAddFirst: delegateeValue].
            r: r copyAddFirst: selectorValue.
            r: r copyAddFirst: rcvrValue.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceDefinedValue: v With: newV = ( |
            | 
            case
              if: [v = sendDescValue] Then: [sendDescValue: newV]
              Else: [resend.replaceDefinedValue: v With: newV]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceUsedValue: v With: newV = ( |
            | 
            case
              if: [v =  selectorValue] Then: [ selectorValue: newV]
              If: [v = delegateeValue] Then: [delegateeValue: newV]
              Else: [resend.replaceUsedValue: v With: newV]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         selectorArgIndex = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fComment: For example,  _Perform: \'wobulate\'  is compiled into a message send to
wobulate. And  _Perform: x  is compiled into a message send whose
selector is patched at runtime (see generateDynamicPerformNode:).\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         selectorForGeneratedSend = ( |
            | 
            [cg generateDynamicPerformNode: n]. "Browsing. When selector is not constant, it will be patched at
                                                 runtime; see generateDynamicPerformNode:. -- Adam, 5/05"
            selectorLoc isConstant ifTrue: [selectorLoc oopValue]
                                    False: [theVM placeholderSelectorForDynamicPerform]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         selectorLoc = ( |
            | 
            selectorValue location).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            blk value: selectorValue.
            blk value: delegateeValue.
            resend.usedValuesDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         selectorValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         sendDescValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: static single-assignment form\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         phiFunction = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes phiFunction.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         basicBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         destinationValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         originalValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes phiFunction parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC BasicBlock: bb Value: v = ( |
            | 
            (((copyBC: aBC) basicBlock: bb) originalValue: v) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            blk value: destinationValue.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         generateCode = ( |
            | 
            [replacePhiFunctionsWithMoves]. "browsing"
            error: 'should never be called; phi functions should be replaced with move nodes').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
             bb.
             d.
            | 
            destinationValue: originalValue. "Is this right?"
            d: dictionary copyRemoveAll.
            bb: basicBlock.
            bb controlFlowPredsDo: [|:predBB|
              d at: predBB Put: originalValue.
            ].
            sourceValues: d.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isPhiFunction = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         possibleValuesFor: v Do: blk AlreadySeen: seen = ( |
            | 
            [v == destinationValue] assert.

            sourceValues do: [|:sv| sv possibleValuesDo: blk AlreadySeen: seen]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceDefinedValue: v With: newV = ( |
            | 
            case
              if: [v = destinationValue] Then: [destinationValue: newV]
              Else: [error: 'what value is that?']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceSourceValueFor: bb With: newV = ( |
            | 
            [sourceValues includesKey: bb] assert.
            sourceValues at: bb Put: newV.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
             r <- ''.
            | 
            sourceValues ifNotNil: [sourceValues do: [|:v| r: r & v shortPrintString] SeparatedBy: [r: r & ', ']].
            destinationValue ifNotNil: [r: r & ' -> ' & destinationValue statePrintString].
            r flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> 'parent' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            sourceValues do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'phiFunction' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         sourceValues.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fCategory: primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         primitiveProtoForBC: bc = ( |
            | 
            "I don't know if this method should be here, or
             whether we ought to be figuring this stuff out
             in the caller, or what. For now this doesn't
             bother me. -- Adam, 5/05"

            specialPrimitiveNodeProtos
               findFirst: [|:n| n isTheRightKindOfNodeForSelector: bc selector]
               IfPresent: [|:n| n]
                IfAbsent: [primitive]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         send = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes sendOrPrimitive copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes send.

CopyDowns:
globals klein compiler1 parent prototypes irNodes sendOrPrimitive. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes send parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         delegatee = ( |
            | bc delegatee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         delegateeObject = ( |
            | 
            (((sourceLevelAllocator context outermostMethodHolder slotAt: delegatee) contents) lookupSoleSlotNamed: selector) holder reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator generateSendNode: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isSend = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         lookupType = ( |
            | 
            vmKit lookupType forBytecode: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fCategory: primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         specialPrimitiveNodeProtos = ( |
            | 
            (performPrimitive & systemCallPrimitive & onNonLocalReturnPrimitive) asList).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: prologue\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         start = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes start.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         cachedBlockValuesToInitialize.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes start parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         addComment = ( |
            | 
            "don't add anything before method start sentinel"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         bci = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         blockValuesToInitialize = ( |
            | 
            cachedBlockValuesToInitialize ifNil: [
              cachedBlockValuesToInitialize: set copyContaining: machineLevelAllocator memoizedBlockValues.
              cachedBlockValuesToInitialize
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         definedValuesDo: blk = ( |
            | 
            blockValuesToInitialize do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator generatePrologue: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         possibleValuesFor: v Do: blk AlreadySeen: seen = ( |
            | 
            [blockValuesToInitialize includes: v] assert.

            blk value: locationForConstant: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         replaceDefinedValue: v With: newV = ( |
            | 
            blockValuesToInitialize remove: v.
            blockValuesToInitialize add: newV.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fCategory: primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         systemCallPrimitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes primitive copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes systemCallPrimitive.

CopyDowns:
globals klein compiler1 parent prototypes irNodes primitive. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes systemCallPrimitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: system call number\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         argIndexOfSystemCallNumber = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCodeForThisKindOfPrimitive = ( |
            | 
            codeGenerator generateSystemCallNode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         indexOfFirstArgToPassIntoSystemCall = ( |
            | 
            argIndexOfSystemCallNumber
                + 1 "for the receiver, which is unused").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isTheRightKindOfNodeForSelector: s = ( |
            | 
            '_SystemCall:' isPrefixOf: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         locsOfArgsToPassIntoSystemCall = ( |
            | 
            (vector copySize: numberOfArgsToPassIntoSystemCall) mapBy: [|:x. :i|
              rcvrOrArgLocAt: indexOfFirstArgToPassIntoSystemCall + i
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         numberOfArgsToPassIntoSystemCall = ( |
            | 
            rcvrAndArgCountToBeMaterialized - indexOfFirstArgToPassIntoSystemCall).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: system call number\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         systemCallNumber = ( |
            | 
            rcvrOrArgSmiAt: argIndexOfSystemCallNumber).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         unconditionalBranch = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'unconditionalBranch' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1 parent prototypes irNodes abstractBranch copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'unconditionalBranch' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes unconditionalBranch.

CopyDowns:
globals klein compiler1 parent prototypes irNodes abstractBranch. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'unconditionalBranch' -> () From: ( | {
         'Category: control flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         destinationNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'unconditionalBranch' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'unconditionalBranch' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes irNodes unconditionalBranch parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'unconditionalBranch' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         controlFlowSuccsDo: blk = ( |
            | 
            destinationNode ifNotNil: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'unconditionalBranch' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC Destination: d = ( |
            | 
            (copyBC: aBC) destinationNode: d).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'unconditionalBranch' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator genBranchTo: destinationNode.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'unconditionalBranch' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isUnconditionalBranch = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'unconditionalBranch' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractBranch' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'unconditionalBranch' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            destinationNode statePrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'unconditionalBranch' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         usedValuesDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         kleinC1_IRNodes = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_IRNodes' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_IRNodes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinC1_IRNodes.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_IRNodes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_IRNodes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_IRNodes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_IRNodes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_IRNodes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.73 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_IRNodes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinC1_IRNodes postFileIn
