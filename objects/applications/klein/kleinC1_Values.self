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
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         compiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         definers <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         interferingValues <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         myLocation.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1 parent prototypes dataValue parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         allocator = ( |
            | compiler allocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         constantLocation = ( |
             d.
            | 
            [aaa]. "This is kind of a hack, maybe. It's probably better to just
                    make sure the locationAssigner always coalesces these moves.
                    But for now let's try this, because it seems easy enough
                    and I don't want to mess with the guts of the locationAssigner
                    right now. -- Adam, Apr. 2009"
            location isConstant ifTrue: [^ location].
            definers size = 1 ifFalse: [error: 'not a constant'].
            d: definers first.
            d isMove ifFalse: [error: 'not a constant'].
            d sourceValue constantLocation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         copy = ( |
            | 
            ((resend.copy
               interferingValues: interferingValues copy)
                        definers: definers copy)
                           users: users copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         copyCompiler: c = ( |
            | 
            (copy compiler: c) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         hasLocation = ( |
            | 
            myLocation isNotNil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            interferingValues removeAll.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: public'
        
         isPlaceholder = bootstrap stub -> 'globals' -> 'false' -> ().
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
            loc isRegister && [loc register number = 0] ifTrue: [halt].
            myLocation: loc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (nil)'
        
         slot.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1' -> 'parent' -> 'prototypes' -> 'dataValue' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_Values InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         users <- set copyRemoveAll.
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
