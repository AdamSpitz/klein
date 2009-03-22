 '$Revision: 30.21 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         dataValues = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         colocatingValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         compiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         intermediateNodes <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         myLocation.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         < x = ( |
            | compare: x IfLess: true Equal: false Greater: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | compare: x IfLess: false Equal: true Greater: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: allocation\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         addIntermediateNode: n = ( |
            | 
            intermediateNodes add: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: allocation\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         addPopper: aNode At: index = ( |
            | 
            poppingNodesAndIndices addLast: aNode @ index.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: allocation\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateLocation = ( |
            | 
            isColocated ifTrue: [^ colocatingValue allocateLocation].
            hasLocation ifTrue: [^ location].
            myLocation: 
              withImmutableLocationDo: [|:il| il]
                               IfNone: [allocator allocateStackLocationFor: self].
            myLocation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         allocator = ( |
            | compiler allocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: block = ( |
            | 
            [todo assertions refactor]. "We could do with 
              (1) a generic way to do assertions 
              (2) a klein-wide assertion flag. -- Ausch 7/05"
            assert: block Description: 'assertion failure in dataValues').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: block Description: string = ( |
            | 
            checkAssertions ifTrue: [block assert: string].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (true)\x7fVisibility: private'
        
         checkAssertions <- bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         colocatingValueOrMyself = ( |
            | 
            colocatingValue ifNil:[^self].
            colocatingValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         compare: aValue IfLess: lb Equal: eb Greater: gb = ( |
            | 
            pushingNode bci
              compare: aValue pushingNode bci
               IfLess: lb
                Equal: [ pushingIndex compare: aValue pushingIndex 
                                       IfLess: lb
                                        Equal: eb
                                      Greater: gb ]
              Greater: gb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         copy = ( |
            | 
            (( resend.copy
            intermediateNodes: intermediateNodes copy)
            poppingNodesAndIndices: poppingNodesAndIndices copy)
            colocatingValue: colocatingValue copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         copyCompiler: c Pusher: p At: i = ( |
            | 
            (((copy compiler: c) 
                    pushingNode: p)
                    pushingIndex: i)
                    initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: allocation\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         hasLocation = ( |
            | 
            myLocation isNotNil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | pushingNode bci hash ^^ pushingIndex hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         initialize = ( |
            | 
            intermediateNodes removeAll.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: allocation\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         isColocated = ( |
            | 
            colocatingValue isNotNil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         isConstant = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: allocation\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         isOKToAllocateJustMeTo: loc = ( |
            | 
            intermediateNodes findFirst: [|:n| n doesIntermediateInterfereWith: loc]
                              IfPresent: [^ false].

            (pushingNode doesPusherInterfereWith: loc At: pushingIndex) ifTrue: [^ false].
            poppingNodesAndIndicesDo: [|:n. :i|
              (n doesPopperInterfereWith: loc At: i) ifTrue: [^ false]].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: allocation\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         isOKToAllocateTo: loc = ( |
            | 
            (isOKToAllocateJustMeTo: loc) ifFalse: [^ false].
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: allocation\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         location = ( |
            | 
            hasLocation ifFalse: [error: 'should have been allocated'].
            myLocation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'orderedClonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: allocation\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         poppingNodesAndIndicesDo: blk = ( |
            | 
            poppingNodesAndIndices do: [|:pt| blk value: pt x With: pt y].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: allocation\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         preferredScratchLocationIfPresent: pb IfAbsent: ab = ( |
             foundOne <- bootstrap stub -> 'globals' -> 'false' -> ().
             res.
            | 
            poppingNodesAndIndicesDo: [|:n. :i|
              n preferredLocationForPoppedValueAt: i
                IfPresent: [|:loc|
                             case if:  foundOne not        Then: [res: loc.  foundOne: true]
                                  If: [loc isUnusedValue]  Then: []
                                                           Else: [^ ab value]
              ] IfAbsent: [].
            ].
            foundOne  ifTrue: [pb value: res] False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
             pns <- ''.
            | 
            pushingNode ifNil: [^ 'NoPush'].
            poppingNodesAndIndices do: [|:p| pns: pns, p x bci printString, ','].
            '(' , pushingNode bci printString, '->', pns, ')').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: allocation\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         withImmutableLocationDo: iblk IfNone: noneBlk = ( |
            | 
            noneBlk value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         poppingNodesAndIndices <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (0)'
        
         pushingIndex <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         pushingNode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         colocatedValue = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'pushingIndex' From:
             bootstrap remove: 'pushingNode' From:
             globals klein compiler1s abstract parent prototypes dataValues abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> () From: ( |
             {} = 'Comment: A value to represent control flow merging of values.
At a control flow merge, a value may have multiple
location constraints (or multiple locations)
assigned to it, one from each branch.
Those constraints need to be merged, and the value
needs to be allocated a proper location.\x7fModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues colocatedValue.

CopyDowns:
globals klein compiler1s abstract parent prototypes dataValues abstract. copy 
SlotsToOmit: parent pushingIndex pushingNode.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         colocatedValues <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues colocatedValue parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateLocation = ( |
            | 
            [todo allocation refactor]. "what to name \"colocatingValue\" ?????
               myselfOrMyMergedValue
               mergedValue  - Ausch & Ungar, 7/05"


            assert: [colocatedValues isEmpty = false]. "should never have to allocate an empty colocated value"
            assert: [|r <- true| 
              colocatedValues do: [|:v| 
               (v hasLocation && r) ifTrue: [r: hasLocation && [v location = myLocation]]].
              r]. "check consistency of merge"

            hasLocation ifTrue: [^ location].
            myLocation: allocator allocateStackLocationFor: self.

            [todo allocation refactor]. "I'd love to get rid of this loop, but I cant seem
            to do it with the current architecture. -- Ausch 7/05"
            colocatedValues do: [ |:v| v myLocation: myLocation].
            myLocation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         colocatingValueOrMyself = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            (resend.copy colocatedValues: colocatedValues copy)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForPopper: aNode At: index WithVals: vals = ( |
            | 
            (copy addPopper: aNode At: index) initializeFor: vals).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeFor: vals = ( |
            | 
            mustColocateWith: vals.
            compiler: vals first compiler.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         isColocated = ( |
            | true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         isOKToAllocateJustMeTo: loc = ( |
            | 
            colocatedValues allSatisfy: [|:v| v isOKToAllocateJustMeTo: loc]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         mustColocateWith: vals = ( |
            | 
            vals do: [|:v|
             colocatedValues add: v.
             intermediateNodes addAll: v intermediateNodes.
             assert: [v isColocated = false]. "Can't write a testcase that causes this to happen -- Ausch, 07/05"
             v colocatingValue: self
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'colocatedValue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         pushingNode: value = ( |
            | 
            'should never need to set pushing node on a colocated value').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         constant = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'constant' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes dataValues abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'constant' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues constant.

CopyDowns:
globals klein compiler1s abstract parent prototypes dataValues abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'constant' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         oopValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'constant' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'constant' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues constant parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'constant' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         copyCompiler: c Pusher: p At: i Value: v = ( |
            | 
            (copyCompiler: c Pusher: p At: i) oopValue: v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'constant' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         isConstant = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'constant' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'constant' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         withImmutableLocationDo: ib IfNone: nb = ( |
            | 
            ib value:
              compiler locations constant copyForOop: oopValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         expressionStack = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'expressionStack' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes dataValues abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'expressionStack' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues expressionStack.

CopyDowns:
globals klein compiler1s abstract parent prototypes dataValues abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'expressionStack' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'expressionStack' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues expressionStack parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'expressionStack' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         formalOrLocal = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalOrLocal' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes dataValues abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalOrLocal' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues formalOrLocal.

CopyDowns:
globals klein compiler1s abstract parent prototypes dataValues abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalOrLocal' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         bc.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalOrLocal' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalOrLocal' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues formalOrLocal parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalOrLocal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         copyCompiler: c Pusher: p At: i BC: aReadLocal = ( |
            | 
            (copyCompiler: c Pusher: p At: i) bc: aReadLocal).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalOrLocal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         locationOfSlot = ( |
            | 
            allocator locationForLocalBC: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalOrLocal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalOrLocal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         withImmutableLocationDo: ib IfNone: nb = ( |
            | 
            ib value: locationOfSlot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         formalParameter = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalParameter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes dataValues formalOrLocal copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalParameter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues formalParameter.

CopyDowns:
globals klein compiler1s abstract parent prototypes dataValues formalOrLocal. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalParameter' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalParameter' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues formalParameter parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalParameter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalOrLocal' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         localAssignableSlot = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'localAssignableSlot' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes dataValues formalOrLocal copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'localAssignableSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues localAssignableSlot.

CopyDowns:
globals klein compiler1s abstract parent prototypes dataValues formalOrLocal. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'localAssignableSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'localAssignableSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues localAssignableSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'localAssignableSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalOrLocal' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'localAssignableSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         withImmutableLocationDo: ib IfNone: nb = ( |
            | 
            nb value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         localConstantSlot = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'localConstantSlot' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes dataValues formalOrLocal copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'localConstantSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues localConstantSlot.

CopyDowns:
globals klein compiler1s abstract parent prototypes dataValues formalOrLocal. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'localConstantSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'localConstantSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues localConstantSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'localConstantSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         oopValue = ( |
            | 
            bc slot contents reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'localConstantSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'formalOrLocal' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         memoizedBlock = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'memoizedBlock' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes dataValues abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'memoizedBlock' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues memoizedBlock.

CopyDowns:
globals klein compiler1s abstract parent prototypes dataValues abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'memoizedBlock' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         blockProto.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'memoizedBlock' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'memoizedBlock' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues memoizedBlock parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'memoizedBlock' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         copyCompiler: c Pusher: p At: i BlockProto: b = ( |
            | (copyCompiler: c Pusher: p At: i) blockProto: b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'memoizedBlock' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'memoizedBlock' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         withImmutableLocationDo: ib IfNone: nb = ( |
            | 
            preferredScratchLocationIfPresent: [
              |:loc|
              loc isUnusedValue ifTrue: [^ ib value: allocator locationForUnusedValue]
            ] IfAbsent: [].

            ib value: allocator locationForMemoizedBlock: blockProto).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         selfOop = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'selfOop' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes dataValues abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'selfOop' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues selfOop.

CopyDowns:
globals klein compiler1s abstract parent prototypes dataValues abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'selfOop' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'selfOop' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes dataValues selfOop parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'selfOop' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'dataValues' -> 'selfOop' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         withImmutableLocationDo: blk IfNone: nb = ( |
            | blk value: allocator locationForSelf).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         kleinC1_Values = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Values' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Values' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinC1_Values.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Values' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Values' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Values' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Values' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Values' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.21 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_Values' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinC1_Values postFileIn
