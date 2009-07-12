 '$Revision: 30.21 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         dataValue = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         compiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         definers <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         description.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (false)'
        
         isUplevelNamedValue <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         knownType.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (false)'
        
         mustBeLocatedInARegister <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         myLocation.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         originalValueBeforeRenaming.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         addDescription: s = ( |
            | 
            description: description ifNil: [s] IfNotNil: [|:d| d, ' / ', s].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: renaming\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         addRenaming: v = ( |
            | 
            renamings isEmpty ifTrue: [renamings: set copyRemoveAll].
            renamings add: v.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: renaming\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         beRenamingOf: v = ( |
            | 
            [aaaaaaa]. "Maybe copy v instead."
            description:                  v description.
            myLocation:                   v myLocation.
            isUplevelNamedValue:          v isUplevelNamedValue.
            sourceLevelAllocatorNamingMe: v sourceLevelAllocatorNamingMe.
            knownType:                    v knownType.
            originalValueBeforeRenaming:  v originalValueBeforeRenaming ifNil: [v].
            originalValueBeforeRenaming addRenaming: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            ((resend.copy
                  definers: definers copy)
               strongUsers: strongUsers copy)
                 weakUsers: weakUsers copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         copyCompiler: c = ( |
            | 
            (copy compiler: c) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         definersDo: blk = ( |
            | 
            definers do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         hasLocation = ( |
            | 
            myLocation isNotNil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         hasUsers = ( |
            | 
            usersDo: [^ true].
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         ifTypeIsClonedBlock: yesBlk Else: noBlk = ( |
             t.
            | 
            t: mergedType.
            t isClonedBlock ifTrue: [yesBlk value: t] False: [noBlk value: 'not a block']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         intersectTypes: ts = ( |
            | 
            [aaaaaaa]. "Um, what's a good way to write this?"
            t1 ifNil: [^ t2].
            t2 ifNil: [^ t1].
            t1 isConstant && [t2 isSelf] ifTrue: [^ t1].
            t2 isConstant && [t1 isSelf] ifTrue: [^ t2].
            halt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         isPlaceholder = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         kindsOfPossibleTypes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent kindsOfPossibleTypes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'abstractMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent kindsOfPossibleTypes abstractMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'abstractMixin' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         isClonedBlock = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'abstractMixin' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         isSelf = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'abstractMixin' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         theBlockIfFail: fb = ( |
            | 
            fb value: 'not a cloned block').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         clonedBlock = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'clonedBlock' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent kindsOfPossibleTypes clonedBlock.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'clonedBlock' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         blockLiteralValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'clonedBlock' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'clonedBlock' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent kindsOfPossibleTypes clonedBlock parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'clonedBlock' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractTypeParent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'abstractMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'clonedBlock' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         blockLiteralNode = ( |
             n.
            | 
            n: blockLiteralValue strongUsers soleElement.
            [n isBlockLiteral] assert.
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'clonedBlock' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForLiteral: v = ( |
            | 
            copy blockLiteralValue: v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'clonedBlock' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         isClonedBlock = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'clonedBlock' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         knownMapUsingOracle: oracle IfFail: fb = ( |
            | 
            "Slight hack. The compiledBlock hasn't been mapped yet, so we can't get its map,
             so we use the original block's map instead. It'll have the same valueSlot
             anyway. -- Adam, May. 2009"
            oracle mapForOriginalObject: blockLiteralValue location oopValue originalBlock_replaceThisSlotWithTheValueSlot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'clonedBlock' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'clonedBlock' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         theBlockIfFail: fb = ( |
            | 
            blockLiteralValue location oopValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         couldBeAnything = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'couldBeAnything' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent kindsOfPossibleTypes couldBeAnything.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'couldBeAnything' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractTypeParent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'abstractMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'couldBeAnything' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         knownMapUsingOracle: oracle IfFail: fb = ( |
            | 
            fb value: 'could be anything').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'couldBeAnything' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         intersection = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'intersection' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent kindsOfPossibleTypes intersection.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'intersection' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'intersection' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent kindsOfPossibleTypes intersection parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'intersection' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         copyIntersecting: ts = ( |
            | 
            copy types: ts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'intersection' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         isSelf = ( |
            | 
            types anySatisfy: [|:t| t isSelf]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'intersection' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         knownMapUsingOracle: oracle IfFail: fb = ( |
            | 
            types do: [|:t|
              (t knownMapUsingOracle: oracle IfFail: nil) ifNotNil: [|:m| ^ m].
            ].
            fb value: 'no known map').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'intersection' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'intersection' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         sourceLevelAllocator = ( |
            | 
            types findFirst: [|:t| t isSelf]
                  IfPresent: [|:t| t sourceLevelAllocator]
                   IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'intersection' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         theBlockIfFail: fb = ( |
            | 
            types do: [|:t|
              (t theBlockIfFail: nil) ifNotNil: [|:b| ^ b].
            ].
            fb value: 'not a cloned block').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'intersection' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (vector)\x7fVisibility: public'
        
         types <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         selfValue = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'selfValue' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent kindsOfPossibleTypes selfValue.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'selfValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'selfValue' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent kindsOfPossibleTypes selfValue parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'selfValue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractTypeParent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'abstractMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'selfValue' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForAllocator: a = ( |
            | 
            copy sourceLevelAllocator: a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'selfValue' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         isSelf = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'selfValue' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         knownMapUsingOracle: oracle IfFail: fb = ( |
            | 
            "Hack: For now, don't assume that we know the type if
             we're compiling an outer method for a block, because
             (as an optimization) we reuse those nmethods for all
             other blocks. (See cachedBlockNMethodOIDs.) -- Adam, Apr. 2009"
            sourceLevelAllocator isInlined not && [sourceLevelAllocator context isOuterMethodForABlock] ifTrue: [^ fb value: 'hack - block nmethods are reused for other blocks'].

            sourceLevelAllocator isInlined not && [sourceLevelAllocator compiler noMapTest] ifTrue: [^ fb value: '_NoMapTest, so can\'t be sure of the receiver type'].

            sourceLevelAllocator context selfMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'selfValue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'selfValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         sourceLevelAllocator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         undefined = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'undefined' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent kindsOfPossibleTypes undefined.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'undefined' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractTypeParent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'abstractMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'undefined' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         knownMapUsingOracle: oracle IfFail: fb = ( |
            | 
            fb value: 'undefined').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'undefined' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         union = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'union' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent kindsOfPossibleTypes union.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'union' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'union' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent kindsOfPossibleTypes union parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'union' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractTypeParent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'abstractMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'union' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         copyMerging: ts = ( |
            | 
            copy types: ts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'union' -> 'parent' -> () From: ( | {
         'Category: building IR nodes\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: rcvrValue TypeCasesDo: blk Else: elseBlk With: irg = ( |
            | 
            irg generateSwitchForCases: types If: [|:type. :trueFork|
              type generateIfMatchesTypeOf: rcvrValue ThenBranchTo: trueFork With: irg.
            ] Then: [|:type. rcvrValueWithKnownType|
              rcvrValueWithKnownType: irg newValue.
              irg move: rcvrValue To: rcvrValueWithKnownType.
              rcvrValueWithKnownType knownType: type.
              blk value: rcvrValueWithKnownType.
            ] Else: elseBlk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'union' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         knownMapUsingOracle: oracle IfFail: fb = ( |
             map.
            | 
            types do: [|:p. m|
              m: p knownMapUsingOracle: oracle IfFail: [|:e| ^ fb value: e].
              map ifNil: [map: m] IfNotNil: [map == m ifFalse: [^ fb value: 'different possible maps']].
            ].
            map).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'union' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'union' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         types <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         knownConstantValueIfFail: fb = ( |
             t.
            | 
            t: mergedType.
            t isConstant ifFalse: [fb value: 'not a constant']
                            True: [t oopValue]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         knownMapIfFail: fb = ( |
            | mergedType knownMapUsingOracle: compiler objectsOracle IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         location = ( |
            | 
            hasLocation ifFalse: [error: 'should have been allocated'].
            myLocation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         location: loc = ( |
            | 
            [myLocation isNil] assert.
            [mustBeLocatedInARegister not || [loc isRegister]] assert.
            myLocation: loc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: renaming\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         meAndAllRenamingsDo: blk = ( |
            | 
            blk value: self.
            renamings do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         mergeTypes: ts = ( |
            | 
            ts ifNone: [kindsOfPossibleTypes undefined]
                IfOne: [|:t| t]
               IfMany: [kindsOfPossibleTypes union copyMerging: ts]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         mergedType = ( |
            | 
            mergedType_AlreadySeen: set copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         mergedTypeFromDefiners_AlreadySeen: seen = ( |
             ts.
            | 
            "Need the AlreadySeen set in case of loops." 
            seen if: self IsPresentDo: [^ nil] IfAbsentAddAndDo: [].

            hasLocation && [location isConstant] ifTrue: [^ location].

            ts: list copyRemoveAll.
            definers do: [|:n|
              (n mergedTypeFor: self AlreadySeen: seen) ifNotNil: [|:t|
                ts add: t.
              ].
            ].
            mergeTypes: ts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: types\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         mergedType_AlreadySeen: seen = ( |
             t.
            | 
            t: mergedTypeFromDefiners_AlreadySeen: seen.
            knownType ifNil: [t] IfNotNil: [kindsOfPossibleTypes intersection copyIntersecting: (t & knownType) asVector]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         mightBeAccessedUplevel = ( |
            | 
            "Could try to make this more precise."

            isUplevelNamedValue ifTrue: [^ true].

            sourceLevelAllocatorNamingMe
                    ifNil: false
                 IfNotNil: [|:sla| sla memoizedBlockValues isEmpty not]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         shortPrintString = ( |
            | 
            uniqueID printString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
             r <- ''.
            | 
            description ifNotNil: [r: r & description & '[' & uniqueID printString & '] in ' ].
            r: r & myLocation printString.
            r flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         strongUserChains = ( |
             r.
            | 
            r: list copyRemoveAll.
            strongUsers do: [|:u. chain|
              u isMove && [u destinationValue mightBeAccessedUplevel not] ifTrue: [
                u destinationValue strongUserChains do: [|:chain|
                  r add: chain addFirst: u.
                ].
              ] False: [
                r add: list copyRemoveAll addFirst: u.
              ].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         usersDo: blk = ( |
            | 
            strongUsers do: blk.
              weakUsers do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (vector)'
        
         renamings <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         sourceLevelAllocatorNamingMe.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         strongUsers <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         uniqueID.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'Category: data flow\x7fModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         weakUsers <- set copyRemoveAll.
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
