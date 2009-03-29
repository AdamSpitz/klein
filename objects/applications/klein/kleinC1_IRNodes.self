 '$Revision: 30.73 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes abstract.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Comment: The object representing the
source (bytecode) operation
from the bytecode interpreter.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         bc.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         blockLiteralNodesThatMayHaveAlreadyRun <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Comment: Backpoint to my compiler1 object.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         compiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Category: control flow links\x7fComment: Set of irNodes that immediately precede me.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         controlFlowPreds <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Category: control flow links\x7fComment: Set of irNodes that immediately succeed me.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         controlFlowSuccs <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         entryStackDepth.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         interferingValues <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         label.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         namesOfLocalsThatMightBeAccessedBeforeBeingAssignedTo <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         < n = ( |
            | bci < n bci).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         = n = ( |
            | == n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         addComment = ( |
            | 
            codeGenerator comment: [nodeName, ' ', statePrintString].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: interference\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         addInterferingValue: v = ( |
            | 
            interferingValues add: v.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fComment: Find the value I push at stack loc n, (0 = top).
If I don\'t push it, keep looking.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         addValuesPushedAt: n To: aSet = ( |
            | 
            "The IfAbsent clause below is required for irNodes that are never executed.
             Although the pushCount is 1, pushedValues is a zero-sized vector. -- dmu 11/03"

            [0 <= n] assert.

            case
             if:   [n < pushCount]  
             Then: [aSet add:  pushedValues at: n IfAbsent: [^ self]]
             Else: [ |vals|
              vals: aSet copyRemoveAll.
              controlFlowPreds do: [|:pred|
                pred addValuesPushedAt: (n - pushCount) + popCount To: vals.
              ].
              vals do: [|:v| v addIntermediateNode: self.  addInterferingValue: v].
              aSet addAll: vals.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateLocations = ( |
            | 
            setEntryStackDepth.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         allocatePoppedLocations = ( |
            | 
            poppedValues do: [|:v| v allocateLocation].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         allocator = ( |
            | compiler allocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         areVolatilesVaporized = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         bci = ( |
            | 
            bc ifNil: -1 IfNotNil: [bc pc]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         bindLabel = ( |
            | 
            codeGenerator bindLabel: label.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         buildPushedValueAt: i = ( |
            | 
            compiler prototypes dataValues expressionStack
              copyCompiler: compiler Pusher: self At: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         buildPushedValues = ( |
            | 
            pushedValues:
              (vector copySize: pushCount) mapBy: [|:e. :i|
                buildPushedValueAt: i
              ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         calculateLocalLiveness = ( |
             assignedTo.
             changed <- bootstrap stub -> 'globals' -> 'false' -> ().
            | 
            assignedTo: namesOfLocalsThatAreDefinitelyAssignedTo.
            namesOfLocalsThatMightBeLiveInAControlFlowSuccessor do: [|:n|
              (namesOfLocalsThatMightBeAccessedBeforeBeingAssignedTo includes: n) ifFalse: [
                (assignedTo includes: n) ifFalse: [
                  namesOfLocalsThatMightBeAccessedBeforeBeingAssignedTo add: n.
                  changed: true.
                ].
              ].
            ].
            changed).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         codeGenerator = ( |
            | compiler codeGenerator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fComment: Do me & my transitive successors such that no
node is done before at least one of its preds is.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         controlFlowOrderDo: blk = ( |
            | 
            controlFlowOrderDo: blk AlreadyDid: set copyRemoveAll.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         controlFlowOrderDo: blk AlreadyDid: done = ( |
             leftToDo.
            | 
            "We made this method iterative instead of recursive,
             to aid in profiling. It actually seems a bit faster
             this way, too. -- Adam & Alex, 4/04"
            leftToDo: list copyRemoveAll.
            leftToDo add: self.
            [leftToDo isEmpty] whileFalse: [|n|
              n: leftToDo removeFirst.
              (done includes: n) ifFalse: [
                blk value: n.
                done add: n.
                leftToDo addAllFirst: n controlFlowSuccs asVector sort.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         controlFlowSuccWhenFallingThrough = ( |
            | 
            sourceSucc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            ((((((
             resend.copy
             controlFlowPreds:                                      controlFlowPreds                                      copy)
             controlFlowSuccs:                                      controlFlowSuccs                                      copy)
             poppedValues:                                          poppedValues                                          copy)
             pushedValues:                                          pushedValues                                          copy)
             interferingValues:                                     interferingValues                                     copy)
             namesOfLocalsThatMightBeAccessedBeforeBeingAssignedTo: namesOfLocalsThatMightBeAccessedBeforeBeingAssignedTo copy)
             blockLiteralNodesThatMayHaveAlreadyRun:                blockLiteralNodesThatMayHaveAlreadyRun                copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC Compiler: c = ( |
            | 
            (copyCompiler: c)
             bc: aBC).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyCompiler: c = ( |
            | 
            (copy compiler: c)
              label: c codeGenerator newLabel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: interference\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         doInterferingValuesInterfereWith: loc = ( |
            | 
            interferingValues anySatisfy: [|:v| v hasLocation && [v location = loc] ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: interference\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         doesIntermediateInterfereWith: loc = ( |
            | 
            (loc isVolatile: allocator) && [areVolatilesVaporized]  ifTrue: [^ true].
            locationsUsedByNodeDo: [|:usedLoc|
              loc = usedLoc ifTrue: [^ true].
            ].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: interference\x7fComment: Loc will always contain a value being consumed; such as
an argument. -- dmu 9/03\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         doesPopperInterfereWith: loc At: i = ( |
            | 
            doInterferingValuesInterfereWith: loc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: interference\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         doesPusherInterfereWith: loc At: i = ( |
            | 
            doInterferingValuesInterfereWith: loc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         exitStackDepth = ( |
            | (entryStackDepth - popCount) + pushCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         findPoppedValues = ( |
            | 
            "In order to do compiler regression testing, the compiler
             must be deterministic. Thus, we cannot just return
             the first element of the set. 
             ( A value is only = to another value if it is the same object,
               and its hash is its identity hash, so sets of values are nondeterministic. )
             So, sort the set. -- dmu 7/05"

            poppedValues: 
              (vector copySize: popCount) mapBy: [|:e. :i. vals. sortedValues |
                vals: set copyRemoveAll.
                controlFlowPreds do: [|:n| n addValuesPushedAt: i To: vals ].
                sortedValues: vals asVector sort.

                "Set this for all poppers because makes code accessing popper cleaner"
                sortedValues do: [|:v| v addPopper: self At: i]. 

                (sortedValues size <= 1) 
                   ifTrue: [sortedValues first] "should allways be at least 1"
                    False: [compiler prototypes dataValues  
                               colocatedValue copyForPopper: self
                                                          At: i
                                                    WithVals: sortedValues] 
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         forgeControlFlowLinkFrom: src To: dst = ( |
            | 
            src controlFlowSuccs add: dst.
            dst controlFlowPreds add: src.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         forgeControlFlowLinks = ( |
            | 
            controlFlowSuccWhenFallingThrough ifNotNil: [|:n|
              forgeControlFlowLinkFrom: self To: n
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         generateCode = ( |
             n.
            | 
            n: nodeName.
            bindLabel.
            codeGenerator generating: n, ' addComment' During: [addComment].
            codeGenerator generating: n, ' generateSpecificCode' During: [generateSpecificCode].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         hash = ( |
            | identityHash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: interference\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         interferingValueLocations = ( |
            | 
            interferingValues copyMappedBy: [|:v| v location]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isDependentOnReceiver = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isLocalLive: slotName = ( |
            | 
            compiler shouldCalculateLocalLiveness ifFalse: [^ true].
            namesOfLocalsThatMightBeAccessedBeforeBeingAssignedTo includes: slotName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isOKInLeafMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         locationsUsedByNodeDo: b = ( |
            | 
            (poppedValues & pushedValues & interferingValues) asVector do: [
              |:vals|
              vals do: [|:v| v hasLocation ifTrue: [b value: v location]]
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         method = ( |
            | compiler method).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         namesOfLocalsThatAreDefinitelyAssignedTo = ( |
            | 
            vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfLocalsThatMightBeAccessed = ( |
            | 
            vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfLocalsThatMightBeLiveInAControlFlowSuccessor = ( |
            | 
            controlFlowSuccs gather: [|:n| n namesOfLocalsThatMightBeAccessedBeforeBeingAssignedTo]
                               Into: set copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         nodeName = ( |
            | 
            asMirror creatorSlotHint name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nodesInControlFlowOrder = ( |
             s.
            | 
            s: orderedSet copyRemoveAll.
            controlFlowOrderDo: [] AlreadyDid: s.
            s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'orderedClonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         pcOffsetIfPresent: pb IfAbsent: ab = ( |
            | 
            label isResolved
             ifTrue: [pb value: label resolvedValue - codeGenerator origin]
              False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         popCount = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         preferredLocationForPoppedValueAt: i IfPresent: pb IfAbsent: ab = ( |
            | 
            i >= popCount ifTrue: [error: '???'].
            ab value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         propagateBlockLiveness = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         pushCount = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         pushResult = ( |
             v.
            | 
            v: resultValue.
            "default for nodes that push something"
            v withImmutableLocationDo: [|:loc| 
                codeGenerator moveLocation: loc ToLocation: v location
            ] IfNone: [].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         resultLoc = ( |
            | 
            resultValue location).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         resultValue = ( |
            | pushedValues first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: source-ordering\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         reverseSourceNodesDo: blk = ( |
            | 
            blk value: self.
            sourcePred reverseSourceNodesDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         setEntryStackDepth = ( |
             p.
            | 
            p: controlFlowPreds findFirst: [|:n| n entryStackDepth isNotNil]
                                IfPresent: [|:n| n]
                                IfAbsent: [error: 'should be one'].
            entryStackDepth: p exitStackDepth.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: special compilation modes\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         setSpecialMode = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: source-ordering\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         sourceNodesDo: blk = ( |
            | 
            blk value: self.
            sourceSucc sourceNodesDo: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            compiler vmKit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (vector)'
        
         poppedValues <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (vector)'
        
         pushedValues <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Category: source-order links\x7fComment: Node preceeding me in source (bytecode) order.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         sourcePred.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> () From: ( | {
         'Category: source-order links\x7fComment: Node succeeding me in source (bytecode) order.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         sourceSucc.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes abstractSend parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         arg1Loc = ( |
            | 
            rcvrOrArgLocAt: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         argIndexForReceiver = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentCount = ( |
            | bc argumentCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         convertToPopIndexFromArgIndex: i = ( |
            | 
            popCount - i - isSelfExplicit asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = ( |
            | 
            compiler codeGenerator a intNN).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         isSelfExplicit = ( |
            | bc isSelfExplicit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         isSelfImplicit = ( |
            | bc isSelfImplicit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
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
                      IsUndirectedResend: [compiler outermostMethodHolder]
                      IsDirectedResend:   [delegateeObject]
                      IsDelegatedPerform: [delegateeObject].

            vmKit lookupKey copyForSelector: sel
                                 LookupType: lt
                                  Delegatee: del).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfLocalsThatMightBeAccessed = ( |
            | 
            blockLiteralNodesThatMayHaveAlreadyRun
                gather: [|:bn| bn namesOfUplevelLocalsAccessedInBlock]
                  Into: set copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         popCount = ( |
            | 
            argumentCount + isSelfExplicit asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         pushCount = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrAndArgCount = ( |
            | 
            argumentCount + 1 "for receiver").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         rcvrAndArgCountForGeneratedSend = ( |
            | 
            rcvrAndArgCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrAndArgLocs = ( |
            | 
            (vector copySize: rcvrAndArgCount) mapBy: [|:e. :i| rcvrOrArgLocAt: i]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrAndArgLocsForGeneratedSend = ( |
            | 
            rcvrAndArgLocs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrLoc = ( |
            | 
            rcvrOrArgLocAt: argIndexForReceiver).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrOrArgLocAt: i = ( |
            | 
            (i = argIndexForReceiver) && [isSelfImplicit] ifTrue: [
              ^ allocator locationForSelf
            ].
            (rcvrOrArgValueAt: i) location).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrOrArgOopValueForConstantLocAt: i = ( |
             loc.
            | 
            loc: rcvrOrArgLocAt: i.
            loc isConstant ifFalse: [error: 'not a constant'].
            loc oopValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrOrArgSmiAt: i = ( |
             oop.
            | 
            oop: rcvrOrArgOopValueForConstantLocAt: i.
            (reflect: oop) isReflecteeInteger ifFalse: [error: 'must be integer'].
            oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         rcvrOrArgValueAt: i = ( |
            | 
            poppedValues at: convertToPopIndexFromArgIndex: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: selector\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         selector = ( |
            | bc selector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: selector\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         selectorForGeneratedSend = ( |
            | 
            selector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            bc ifNil: '' IfNotNil: [selector, ' at: ', bci printString]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractSend = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes abstractSend.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         sendOrPrimitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstractSend copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes sendOrPrimitive.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstractSend. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes sendOrPrimitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateLocations = ( |
            | 
            resend.allocateLocations.
            allocator allocateOutgoingRcvrAndArgLocations: rcvrAndArgCountForGeneratedSend.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         areVolatilesVaporized = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Comment: Ensure popped value will not be overwritten during
setup of arguments for message send.  -- jb 8/03

Loc will always contain a value being consumed; such as
an argument. -- dmu 9/03\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         doesPopperInterfereWith: loc At: i = ( |
            | 
            (resend.doesPopperInterfereWith: loc At: i)
              ifTrue: [^ true].

            "sends and prims do not use nonvolatile locations"
            (loc isVolatile: allocator)  ifFalse: [^ false].

            "At this point loc must be volatile, and we assume that
             1. Any preferred location for a popped value
                (i.e. where it will be copied to) is volatile, and
             2. Each popped value has different preferred location.

             Therefore the following condition conservatively ensures that
             no location to which we will be copying an argument overlaps loc.
              -- dmu & jb 9/03"

            preferredLocationForPoppedValueAt: i
              IfPresent: [|:lc| lc != loc]
               IfAbsent: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'Comment: popindex: i = 0 if it is last arg popped
argindex = 0 for rcvr (no matter whether there is an explicit one or not)\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         ifPoppedValueAt: i IsAnArgumentInTheGeneratedSendThen: argBlk Else: notArgBlk = ( |
             argIndexForGeneratedSend.
            | 
            argIndexForGeneratedSend: popCount - i - isSelfExplicit asInteger.

            argBlk value: argIndexForGeneratedSend).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         locationsThatNeedToBePreserved = ( |
             assignableLocalNames.
             s.
            | 
            s: set copyRemoveAll.
            s add:    allocator locationForIncomingReceiver.
            s addAll: allocator memoizedBlockLocations.
            allocator locationFor_OnNLR_homeScope ifNotNil: [|:loc| s add: loc].

            assignableLocalNames: (allocator assignableLocalSlots copyMappedBy: [|:slot| slot name]) asSet.
            allocator namedLocations filterBy: [|:loc. :n|
              (assignableLocalNames includes: n) not || [isLocalLive: n]
            ] Into: s.

            s addAll: interferingValueLocations.

            [aaa].
            (s anySatisfy: [|:loc| loc isRegister && [loc register = (allocator gprFor: 3)]]) ifTrue: [halt].

            s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         preferredLocationForPoppedValueAt: i IfPresent: pb IfAbsent: ab = ( |
            | 
            i < popCount  ifTrue: [
                                 ifPoppedValueAt: i
              IsAnArgumentInTheGeneratedSendThen: [|:argIndexForGeneratedSend| pb value: allocator locationForOutgoingRcvrOrArgAt: argIndexForGeneratedSend]
                                            Else: ab
            ].
            resend.preferredLocationForPoppedValueAt: i IfPresent: pb IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractPrimitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes sendOrPrimitive copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes abstractPrimitive.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes sendOrPrimitive. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (false)'
        
         isSpecialCompilationMode <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> () From: ( |
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
-- jb 8/03\x7fModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes abstractPrimitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            isSpecialCompilationMode ifTrue: [^ self].
            generateCodeForThisKindOfPrimitive.
            pushResult.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isOKInLeafMethod = ( |
            | 
            [todo optimization "see slot comment"]. "might be true for very simple prims"
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> () From: ( | {
         'Category: fail blocks\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         primitiveFailureMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> 'primitiveFailureMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes abstractPrimitive parent primitiveFailureMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> 'primitiveFailureMixin' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         hasExplicitFailBlock = ( |
            | 
            'IfFail:' isSuffixOf: selector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> 'primitiveFailureMixin' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         selectorToSendOnFailure = ( |
            | 
            [value: ''                With: '']. "browsing"
            [primitiveFailedError: '' Name: '']. "browsing"

            hasExplicitFailBlock ifTrue: 'value:With:'
                                  False: 'primitiveFailedError:Name:').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> () From: ( | {
         'Category: special compilation modes\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         setSpecialMode = ( |
             s.
            | 
            [_NoMapTest. _VariableArguments. _SaveAllNonVolatileRegisters. _NoGCAllowed. _NoSendsAllowed]. "browsing"
            s: selector.
            isSpecialCompilationMode: true.
            case if: [s = '_NoMapTest'                  ]  Then: [compiler noMapTest:             true]
                 If: [s = '_VariableArguments'          ]  Then: [compiler setVariableArguments       ]
                 If: [s = '_SaveAllNonVolatileRegisters']  Then: [compiler saveAllNonVolatileRegisters]
                 If: [s = '_NoGCAllowed'                ]  Then: [compiler noGCAllowed:           true]
                 If: [s = '_NoSendsAllowed'             ]  Then: [compiler noSendsAllowed:        true]
                 Else: [
                        isSpecialCompilationMode: false.
                        case if: [s = '_OnNonLocalReturn:'       ]  Then: [compiler hasOnNonLocalReturn: true]
                             If: [s = '_OnNonLocalReturn:IfFail:']  Then: [compiler hasOnNonLocalReturn: true].
                 ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data access and assignment\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         literal = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'literal' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'literal' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes literal.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'literal' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'literal' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes literal parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'literal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         buildPushedValueAt: i = ( |
            | 
            i = 0 ifFalse: [error: 'only one'].
            compiler prototypes dataValues constant 
              copyCompiler: compiler Pusher: self At: 0 Value: oopToPush).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'literal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            pushResult).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'literal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         oopToPush = ( |
            | bc oopToPush).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'literal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'literal' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         popCount = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'literal' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         pushCount = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'literal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            bc ifNil: [^''].
            (reflect: oopToPush) name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data access and assignment\x7fComment: When pushing a block literal, even though
the bytecodes are the same as pushing 17,
it is necessary to clone the block and set its home scope.
So, make a specialized irNode. -- dmu 2/03\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         blockLiteral = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes literal copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes blockLiteral.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes literal. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         cachedNamesOfUplevelLocalsAccessedInBlock.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes blockLiteral parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateLocations = ( |
            | 
            [cloneBlockHomeFrame_stub: fp]. "browsing"
            resend.allocateLocations.
            allocator allocateOutgoingRcvrAndArgLocations: 2. "rcvr + arg for cloneBlockHomeFrame_stub:"
            allocator allocateLocationForMemoizedBlock: oopToPush.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Comment: We might want to send messages while cloning the block.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         areVolatilesVaporized = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         blockMethodMirror = ( |
            | 
            (reflect: oopToPush) method).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         blockProtoLoc = ( |
            | 
            allocator locationForConstant: oopToPush).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         buildPushedValueAt: i = ( |
            | 
            i = 0 ifFalse: [error: 'only one'].
            compiler prototypes dataValues memoizedBlock 
              copyCompiler: compiler Pusher: self At: 0 BlockProto: oopToPush).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Comment: Ensure popped value will not be overwritten by the
arguments for any messages we may have to send for
block cloning and memoization. -- jb 8/03\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         doesPopperInterfereWith: loc At: i = ( |
            | 
            (loc isVolatile: allocator)  ifTrue: [^ true].
            resend.doesPopperInterfereWith: loc At: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            resultLoc isUnusedValue ifFalse: [
              compiler codeGenerator generateBlockLiteralNode: self.
              resend.generateSpecificCode
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isOKInLeafMethod = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         memoizedBlockLoc = ( |
            | 
            resultLoc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: local liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfLocalsAccessedIn: mm = ( |
             f.
            | 
            f: localAccessFinder copyForMethod: mm.
            f interpretMethod.
            f namesOfLocalsAccessed).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: local liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         namesOfUplevelLocalsAccessedInBlock = ( |
            | 
            cachedNamesOfUplevelLocalsAccessedInBlock ifNil: [
              cachedNamesOfUplevelLocalsAccessedInBlock: namesOfUplevelLocalsAccessedIn: blockMethodMirror.
              cachedNamesOfUplevelLocalsAccessedInBlock
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'literal' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'blockLiteral' -> 'parent' -> () From: ( | {
         'Category: local liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         propagateBlockLiveness = ( |
            | 
            controlFlowOrderDo: [|:n| n blockLiteralNodesThatMayHaveAlreadyRun add: self].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fCategory: primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         fakePrimitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstractPrimitive copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes fakePrimitive.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstractPrimitive. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes fakePrimitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing sendDesc information\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         delegateeObject = ( |
            | 
            vmKit lookupKey delegateeOrMethodHolderForNormalSend).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCodeForThisKindOfPrimitive = ( |
            | 
            [todo cleanup fakePrimitives]. "I'm leaving this object in for now, but I
                                            think it should be unnecessary now that
                                            we're using bytecode-transmogrification
                                            to handle fake primitives. -- Adam, 7/06"
            error: 'should not reach here'.

            codeGenerator generateFakePrimitiveNode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         hasImplicitFailBlock = ( |
            | 
            hasExplicitFailBlock not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         ifPoppedValueAt: i IsAnArgumentInTheGeneratedSendThen: argBlk Else: notArgBlk = ( |
            | 
                        resend.ifPoppedValueAt: i
            IsAnArgumentInTheGeneratedSendThen: [|:r| argBlk value: r + 1  "for the receiver, which is klein primitives"]
                                          Else: notArgBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isTheRightKindOfNodeForSelector: s = ( |
            | 
            "A fake primitive is a primitive that's implemented as just
             an ordinary method. (The ordinary method must be located
             on the vmKit primitives object.)"
            theVM vmKit primitives hasFakePrimitiveForSelector: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForFakePrimitivesHolder = ( |
            | 
            compiler locations constant copyForOop: vmKit primitives).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         locationForImplicitFailBlock = ( |
            | 
            compiler locations constant copyForOop: vmKit primitives objectToUseIfNoExplicitFailBlock).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing sendDesc information\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         lookupType = ( |
            | 
            vmKit lookupType normal).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         primitiveFailureMixin* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> 'primitiveFailureMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         rcvrAndArgCountForGeneratedSend = ( |
            | 
              resend.rcvrAndArgCountForGeneratedSend
            + hasImplicitFailBlock asInteger
            + 1 "for the receiver, which is klein primitives").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrAndArgLocsForGeneratedSend = ( |
             r.
            | 
            r: resend.rcvrAndArgLocsForGeneratedSend.
            r: r copyAddFirst: locationForFakePrimitivesHolder.
            hasImplicitFailBlock ifTrue: [r: r copyAddLast: locationForImplicitFailBlock].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'fakePrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing sendDesc information\x7fComment: For example, _IntComplement is compiled into a message send to
primReceiver:IntComplementIfFail:.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         selectorForGeneratedSend = ( |
            | 
            vmKit primitives fakePrimitiveMethodNameForSelector: selector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         indexedBranch = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'indexedBranch' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'indexedBranch' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes indexedBranch.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'indexedBranch' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'indexedBranch' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes indexedBranch parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'indexedBranch' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         destinationNodes = ( |
            | 
            bc destinations copyMappedBy: [|:bci| compiler irNodesByBCI at: bci]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'indexedBranch' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         forgeControlFlowLinks = ( |
            | 
            resend.forgeControlFlowLinks.
            destinationNodes do: [|:target|
              forgeControlFlowLinkFrom: self To: target
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'indexedBranch' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator genIndexedBranchTo: destinationNodes
                                   IndexedBy: poppedValues first location).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'indexedBranch' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'indexedBranch' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         popCount = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'indexedBranch' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         pushCount = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data access and assignment\x7fCategory: data slots\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         inlinedAccessOrAssignment = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccessOrAssignment' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstractSend copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccessOrAssignment' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes inlinedAccessOrAssignment.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstractSend. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccessOrAssignment' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (nil)'
        
         mySlot.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccessOrAssignment' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccessOrAssignment' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes inlinedAccessOrAssignment parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccessOrAssignment' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: bc Compiler: c Slot: s = ( |
            | 
            (copyBC: bc Compiler: c) mySlot: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccessOrAssignment' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isDependentOnReceiver = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccessOrAssignment' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractSend' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data access and assignment\x7fCategory: data slots\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         inlinedAccess = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes inlinedAccessOrAssignment copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes inlinedAccess.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes inlinedAccessOrAssignment. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccess' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccess' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes inlinedAccess parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator generateInlinedAccess: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccessOrAssignment' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data access and assignment\x7fCategory: data slots\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         inlinedAssignment = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAssignment' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes inlinedAccessOrAssignment copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAssignment' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes inlinedAssignment.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes inlinedAccessOrAssignment. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAssignment' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAssignment' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes inlinedAssignment parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAssignment' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator generateInlinedAssignment: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAssignment' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccessOrAssignment' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data access and assignment\x7fCategory: data slots\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         inlinedConstantAccess = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedConstantAccess' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes inlinedAccess copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedConstantAccess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes inlinedConstantAccess.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes inlinedAccess. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedConstantAccess' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedConstantAccess' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes inlinedConstantAccess parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedConstantAccess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator loadReflecteeOf: mySlot contents
                             IntoLocation: resultLoc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedConstantAccess' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'inlinedAccess' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: returns - includes epilogue\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         localReturn = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes localReturn.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes localReturn parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         bci = ( |
            | method codes size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         controlFlowSuccWhenFallingThrough = ( |
            | 
            "Never falls through."
            ^ nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            needsEpilogue ifTrue: [
              codeGenerator generateEpilogueReturning: poppedValues first
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         needsEpilogue = ( |
            | 
            "Only need the epilogue if the finish node is reachable.
             e.g. Not needed if preceded by an NLR unless there is
                  a branch around it.  -- jb 8/03"
            controlFlowPreds isEmpty not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         popCount = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         preferredLocationForPoppedValueAt: i IfPresent: pb IfAbsent: ab = ( |
            | 
            i = 0  ifTrue: [ 
              ^ pb value: allocator locationForOutgoingResult
            ].
            resend.preferredLocationForPoppedValueAt: i IfPresent: pb IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         pushCount = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'localReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         sourceNodesDo: blk = ( |
            | blk value: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: returns - includes epilogue\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nonlocalReturn = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes nonlocalReturn.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes nonlocalReturn parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         controlFlowSuccWhenFallingThrough = ( |
            | 
            "Never falls through."
            ^ nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( | {
         'Category: interference\x7fComment: Loc will always contain a value being consumed; such as
an argument. -- dmu 9/03\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         doesPopperInterfereWith: loc At: i = ( |
            | 
            (resend.doesPopperInterfereWith: loc At: i)
              ifTrue: [^ true].
                ( loc = allocator locationForIncomingReceiver     )
            ||  [ loc = allocator locationForOutgoingNLRHomeScope ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator generateNLRReturning: poppedValues first.
            "normally the finish node would generateNLRPoints.
             But this node has no control flow successor, so the finish node
             will never generate code. -- dmu 5/04"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         popCount = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         preferredLocationForPoppedValueAt: i IfPresent: pb IfAbsent: ab = ( |
            | 
            i = 0  ifTrue: [
              ^ pb value: allocator locationForOutgoingResult
            ].
            resend.preferredLocationForPoppedValueAt: i IfPresent: pb IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'nonlocalReturn' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         pushCount = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fCategory: primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         performPrimitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstractPrimitive copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes performPrimitive.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstractPrimitive. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: InitializeToExpression: (0)'
        
         lookupType <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes performPrimitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBC: aBC Compiler: c = ( |
             n.
            | 
            n: resend.copyBC: aBC Compiler: c.
            n initializeLookupType.
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         delegateeArgIndex = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         delegateeLoc = ( |
            | 
            case
               if: [isDirectedResend  ] Then: [rcvrOrArgLocAt: delegateeArgIndex]
               If: [isUndirectedResend] Then: [compiler locations constant copyForOop: compiler outermostMethodHolder]
                                        Else: [compiler locations constant copyForOop: 0]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         delegateeObject = ( |
            | 
            delegateeLoc isConstant ifTrue: [delegateeLoc oopValue]
                                     False: [0 "will be patched"]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         firstArgIndex = ( |
            | 
            isDirectedResend ifTrue: [delegateeArgIndex succ]
                              False: [ selectorArgIndex succ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCodeForThisKindOfPrimitive = ( |
            | 
            codeGenerator generatePerformNode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         ifPoppedValueAt: i IsAnArgumentInTheGeneratedSendThen: argBlk Else: notArgBlk = ( |
            | 
                        resend.ifPoppedValueAt: i
            IsAnArgumentInTheGeneratedSendThen: [|:r| r = argIndexForReceiver ifTrue: [^ argBlk value: r].
                                                      r < firstArgIndex ifTrue: [^ notArgBlk value].
                                                      argBlk value:   r
                                                                    - 1                          "for the selector"
                                                                    - isDirectedResend asInteger "for the delegatee"]
                                          Else: notArgBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isCompletelyStatic = ( |
            | 
            selectorLoc isConstant && [delegateeLoc isConstant]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         isDirectedResend = ( |
            | 
            vmKit lookupType isDirectedResend: lookupType).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isTheRightKindOfNodeForSelector: s = ( |
            | 
            (globals selector copyStr: s) isAPerform).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         isUndirectedResend = ( |
            | 
            vmKit lookupType isUndirectedResend: lookupType).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         rcvrAndArgCountForGeneratedSend = ( |
            | 
              resend.rcvrAndArgCountForGeneratedSend
            - 1                          "for the selector"
            - isDirectedResend asInteger "for the delegatee").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrAndArgLocsForGeneratedSend = ( |
             r.
            | 
            r: resend.rcvrAndArgLocsForGeneratedSend.
            (r copyFrom: firstArgIndex) copyAddFirst: r first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         selectorArgIndex = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'performPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing receiver and arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         selectorLoc = ( |
            | 
            rcvrOrArgLocAt: selectorArgIndex).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data access and assignment\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         pop = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'pop' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'pop' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes pop.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'pop' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'pop' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes pop parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'pop' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'pop' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'pop' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         popCount = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'pop' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         preferredLocationForPoppedValueAt: i IfPresent: pb IfAbsent: ab = ( |
            | 
            i = 0  ifTrue: [^ pb value: allocator locationForUnusedValue].
            resend.preferredLocationForPoppedValueAt: i
                       IfPresent: pb
                       IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'pop' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         pushCount = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fCategory: primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         primitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstractPrimitive copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes primitive.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstractPrimitive. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes primitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: allocating data locations\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateLocations = ( |
            | 
            resend.allocateLocations.
            allocator allocateOutgoingRcvrAndArgLocations: rcvrAndArgCountToBeMaterialized.
            allocator allocateOutgoingRcvrAndArgLocations: numberOfLocationsNeededForTheFailBlockAndItsArguments.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCodeForThisKindOfPrimitive = ( |
            | 
            codeGenerator generatePrimitiveNode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: allocating data locations\x7fComment: Failure blocks take two arguments: an error string and the
name of the primitive.\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         numberOfLocationsNeededForTheFailBlockAndItsArguments = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         primitiveFailureMixin* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> 'primitiveFailureMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: materializing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         rcvrAndArgCountToBeMaterialized = ( |
            | 
            rcvrAndArgCount).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> () From: ( | {
         'Category: materializing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvrAndArgLocsToBeMaterialized = ( |
            | 
            rcvrAndArgLocs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data access and assignment\x7fCategory: locals\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         readLocal = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readLocal' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readLocal' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes readLocal.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readLocal' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readLocal' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes readLocal parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readLocal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         buildPushedValueAt: i = ( |
             proto.
             s.
            | 
            i = 0 ifFalse: [error: 'only one']. 
            s: bc slot.
            s isAssignable ifTrue: [^ resend.buildPushedValueAt: i].
            proto: s isArgument ifTrue: [ compiler prototypes dataValues formalParameter   ]
                                 False: [ compiler prototypes dataValues localConstantSlot ].
            proto copyCompiler: compiler Pusher: self At: 0 BC: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readLocal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
             src.
            | 
            bc slot isAssignable ifFalse: [^ self].
            src: allocator locationForLocalBC: bc.
            codeGenerator 
              moveLocation: src
                ToLocation: resultLoc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readLocal' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfLocalsThatMightBeAccessed = ( |
            | 
            set copyRemoveAll add: bc selector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readOrWriteLocal' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readOrWriteLocal' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes readOrWriteLocal parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readLocal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readOrWriteLocal' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readLocal' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         popCount = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readLocal' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         pushCount = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data access and assignment\x7fCategory: locals\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         readOrWriteLocal = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readOrWriteLocal' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readOrWriteLocal' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes readOrWriteLocal.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readOrWriteLocal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readOrWriteLocal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            bc ifNil: [^''].
            bc selector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fCategory: primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         restartPrimitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'restartPrimitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes primitive copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'restartPrimitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes restartPrimitive.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes primitive. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'restartPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'restartPrimitive' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes restartPrimitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'restartPrimitive' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         controlFlowSuccWhenFallingThrough = ( |
            | 
            "Never falls through."
            ^ nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'restartPrimitive' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         forgeControlFlowLinks = ( |
            | 
            forgeControlFlowLinkFrom: self To: nodeToBranchTo).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'restartPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isTheRightKindOfNodeForSelector: s = ( |
            | 
            ('_Restart' = s) || ['_RestartIfFail:' = s]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'restartPrimitive' -> 'parent' -> () From: ( | {
         'Category: control flow\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         nodeToBranchTo = ( |
            | 
            compiler irNodesByBCI first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'restartPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'primitive' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: branches\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         scalarBranch = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'scalarBranch' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'scalarBranch' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes scalarBranch.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'scalarBranch' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'scalarBranch' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes scalarBranch parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'scalarBranch' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         controlFlowSuccWhenFallingThrough = ( |
            | 
            isConditional ifFalse: [^ nil].
            resend.controlFlowSuccWhenFallingThrough).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'scalarBranch' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         destinationNode = ( |
            | compiler irNodesByBCI at: bc destination).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'scalarBranch' -> 'parent' -> () From: ( | {
         'Category: control-flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         forgeControlFlowLinks = ( |
            | 
            resend.forgeControlFlowLinks.
            forgeControlFlowLinkFrom: self To: destinationNode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'scalarBranch' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            isConditional ifTrue: [
              codeGenerator genCondBranchTo: destinationNode
                                         If: poppedValues first location
                                         Is: bc valueToBranchOn
            ]
            False: [
              codeGenerator genBranchTo: destinationNode
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'scalarBranch' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         isConditional = ( |
            | bc isConditional).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'scalarBranch' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'scalarBranch' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         popCount = ( |
            | 
            isConditional ifTrue: 1 False: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'scalarBranch' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         pushCount = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data access and assignment\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         selfNode = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'selfNode' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'selfNode' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes selfNode.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'selfNode' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'selfNode' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes selfNode parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'selfNode' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         buildPushedValueAt: i = ( |
            | 
            i = 0 ifFalse: [error: 'only one'].
            compiler prototypes dataValues selfOop 
              copyCompiler: compiler Pusher: self At: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'selfNode' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            pushResult).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'selfNode' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'selfNode' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         popCount = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'selfNode' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         pushCount = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         send = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes sendOrPrimitive copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes send.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes sendOrPrimitive. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes send parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         delegatee = ( |
            | bc delegatee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         delegateeObject = ( |
            | 
            ((((reflect: compiler outermostMethodHolder) slotAt: delegatee) contents) lookupSoleSlotNamed: selector) holder reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator generateSendNode: self.
            pushResult.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isOKInLeafMethod = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         lookupType = ( |
            | 
            vmKit lookupType forBytecode: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'send' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'sendOrPrimitive' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fCategory: primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         specialPrimitiveNodeProtos = ( |
            | 
            (performPrimitive & systemCallPrimitive & fakePrimitive & restartPrimitive) asList).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: prologue\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         start = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes start.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes start parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         addComment = ( |
            | "don't add anything before method start sentinel"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateLocations = ( |
            | 
            resend.allocateLocations.
            compiler allocateIncomingAndPreallocatedLocations.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         bci = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateInitializationForLocals = ( |
             assignableLocalSlots.
             assignableLocalSlotsToInitialize.
            | 

            assignableLocalSlots: allocator assignableLocalSlots.

            assignableLocalSlotsToInitialize: compiler shouldOmitInitializationOfProvablyDeadLocals ifTrue: [
              assignableLocalSlots copyFilteredBy: [|:s| isLocalLive: s name]
            ] False: [
              assignableLocalSlots
            ].

            codeGenerator numberOfLocalsThatNeedToBeInitialized:                                  assignableLocalSlotsToInitialize size.
            codeGenerator numberOfLocalsThatDoNotNeedToBeInitialized: assignableLocalSlots size - assignableLocalSlotsToInitialize size.

            codeGenerator generateInitializationForLocals: assignableLocalSlotsToInitialize.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator generating: 'generatePrologue' During: [codeGenerator generatePrologue].
            codeGenerator generating: 'generateInitializationForLocals' During: [generateInitializationForLocals].
            allocator isLeafMethod ifFalse: [codeGenerator generating: 'genStackCheck' During: [codeGenerator genStackCheck]].
            codeGenerator generating: 'generateInitializationForMemoizedBlocks' During: [codeGenerator generateInitializationForMemoizedBlocks].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         popCount = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         pushCount = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         reverseSourceNodesDo: blk = ( |
            | 
            blk value: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'start' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         setEntryStackDepth = ( |
            | entryStackDepth: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: sends & primitives\x7fCategory: primitives\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         systemCallPrimitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstractPrimitive copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes systemCallPrimitive.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstractPrimitive. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes systemCallPrimitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: system call number\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         argIndexOfSystemCallNumber = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateCodeForThisKindOfPrimitive = ( |
            | 
            codeGenerator generateSystemCallNode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         indexOfFirstArgToPassIntoSystemCall = ( |
            | 
            argIndexOfSystemCallNumber
                + 1 "for the receiver, which is unused").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isTheRightKindOfNodeForSelector: s = ( |
            | 
            '_SystemCall:' isPrefixOf: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         locsOfArgsToPassIntoSystemCall = ( |
            | 
            (vector copySize: numberOfArgsToPassIntoSystemCall) mapBy: [|:x. :i|
              rcvrOrArgLocAt: indexOfFirstArgToPassIntoSystemCall + i
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: arguments\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         numberOfArgsToPassIntoSystemCall = ( |
            | 
            rcvrAndArgCount - indexOfFirstArgToPassIntoSystemCall).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'abstractPrimitive' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'systemCallPrimitive' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: system call number\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         systemCallNumber = ( |
            | 
            rcvrOrArgSmiAt: argIndexOfSystemCallNumber).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> () From: ( | {
         'Category: data access and assignment\x7fCategory: locals\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         writeLocal = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'writeLocal' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler1s abstract parent prototypes irNodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'writeLocal' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes writeLocal.

CopyDowns:
globals klein compiler1s abstract parent prototypes irNodes abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'writeLocal' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'writeLocal' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes irNodes writeLocal parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'writeLocal' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         buildPushedValueAt: i = ( |
            | 
            i = 0 ifFalse: [error: 'only one'].
            compiler prototypes dataValues selfOop
               copyCompiler: compiler Pusher: self At: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'writeLocal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSpecificCode = ( |
            | 
            codeGenerator moveLocation: poppedValues first location
                            ToLocation: allocator locationForLocalBC: bc.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'writeLocal' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: private'
        
         nameOfLocal = ( |
            | 
            bc selector copyWithoutSuffix: ':').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'writeLocal' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fCategory: liveness\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot\x7fVisibility: public'
        
         namesOfLocalsThatAreDefinitelyAssignedTo = ( |
            | 
            set copyRemoveAll add: nameOfLocal).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'writeLocal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'readOrWriteLocal' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'writeLocal' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         popCount = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'irNodes' -> 'writeLocal' -> 'parent' -> () From: ( | {
         'Category: data flow links\x7fModuleInfo: Module: kleinC1_IRNodes InitialContents: FollowSlot'
        
         pushCount = 1.
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
