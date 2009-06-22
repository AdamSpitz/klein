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
         'Category: data flow\x7fModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         knownPossibleValues.
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
            description: v description.
            myLocation:  v myLocation.
            originalValueBeforeRenaming: v originalValueBeforeRenaming ifNil: [v].
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
         'Category: possible values\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         ifTypeIsClonedBlock: yesBlk Else: noBlk = ( |
            | 
            [aaaaaaa]. "Rename all these guys - should be 'type', not 'possible value'."
            possibleValues
               ifNone: [noBlk value: 'undefined']
                IfOne: [|:t| t isClonedBlock ifFalse: [noBlk value: 'not a block']
                                                True: [yesBlk value: t]]
               IfMany: [noBlk value: 'not a block']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         isPlaceholder = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: possible values\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
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
        
         theBlock = ( |
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
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         knownMapUsingOracle: oracle IfFail: fb = ( |
            | 
            "Hack: For now, don't assume that we know the type if
             we're compiling an outer method for a block, because
             (as an optimization) we reuse those nmethods for all
             other blocks. (See cachedBlockNMethodOIDs.) -- Adam, Apr. 2009"
            sourceLevelAllocator context isOuterMethodForABlock ifTrue: [^ fb value: 'hack - block nmethods are reused for other blocks'].

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
            copy possibilities: ts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> 'kindsOfPossibleTypes' -> 'union' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         knownMapUsingOracle: oracle IfFail: fb = ( |
             map.
            | 
            possibilities do: [|:p. m|
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
        
         possibilities <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: possible values\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         knownConstantValueIfFail: fb = ( |
            | 
            possibleValues
               ifNone: [fb value: 'undefined']
                IfOne: [|:loc| loc isConstant ifFalse: [fb value: 'not a constant']
                                                 True: [loc oopValue]]
               IfMany: [fb value: 'not a constant']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: possible values\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         knownMapIfFail: fb = ( |
            | 
            mergedType knownMapUsingOracle: compiler oracleForEagerRelocation IfFail: fb).
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
         'Category: possible values\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         mergedType = ( |
             pvs.
            | 
            pvs: possibleValues.
            pvs ifNone: [kindsOfPossibleTypes undefined]
                 IfOne: [|:t| t]
                IfMany: [kindsOfPossibleTypes union copyMerging: pvs]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: possible values\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         possibleValues = ( |
             pvs.
            | 
            pvs: list copyRemoveAll.
            possibleValuesDo: [|:pv| pvs add: pv] AlreadySeen: set copyRemoveAll.
            pvs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: possible values\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         possibleValues: pvs = ( |
            | 
            [knownPossibleValues isNil] assert.
            knownPossibleValues: pvs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: possible values\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         possibleValuesDo: blk AlreadySeen: seen = ( |
            | 
            "Need the AlreadySeen set in case of loops." 
            seen if: self IsPresentDo: [^ self] IfAbsentAddAndDo: [].

            [aaaaa]. "Not sure this is the right way to do this."
            knownPossibleValues ifNotNil: [knownPossibleValues do: blk. ^ self].

            hasLocation && [location isConstant] ifTrue: [
              blk value: location.
            ] False: [
              definers do: [|:n| n possibleValuesFor: self Do: blk AlreadySeen: seen].
            ].
            self).
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
