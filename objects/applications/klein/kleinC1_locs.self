 '$Revision: 30.20 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         isConstant = ( |
            | 
            [aaaaa]. "Maybe rename something so that this makes sense."
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         isRegister = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationTypeName = 'register'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: accessing oop\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         oopInActivation: a Frame: f IfFail: fb = ( |
            | 
            a oopInRegister: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         register = ( |
            | 
            [aaaaa]. "For compatibility with the old register locations."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         spOffsetFor: aCodeGenerator InFrame: f = ( |
            | number negate).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToLoadMeToRegister: r = ( |
            | 
            aCodeGenerator loadRegisterLocation: self ToRegister: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToStoreMeFromRegister: r = ( |
            | 
            aCodeGenerator storeRegister: r ToRegisterLocation: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: locations\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         locations = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         isConstant = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         isRegister = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForUplevel: n AccessIn: aSourceLevelAllocator = ( |
            | 
            error: 'uplevel access to this type of location not supported').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: saving location info\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         spOffsetFor: aCodeGenerator InFrame: f = ( |
            | error: 'inappropriate slot').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToLoadMeToRegister: r = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToStoreMeFromRegister: r = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> () From: ( | {
         'Category: stack frame memory words\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractStackFrameMemoryWord = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractStackFrameMemoryWord' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein locations abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractStackFrameMemoryWord' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations abstractStackFrameMemoryWord.

CopyDowns:
globals klein locations abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractStackFrameMemoryWord' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: InitializeToExpression: (nil)'
        
         accessingSourceLevelAllocator.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractStackFrameMemoryWord' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: InitializeToExpression: (0)'
        
         lexicalLevel <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractStackFrameMemoryWord' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractStackFrameMemoryWord' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations abstractStackFrameMemoryWord parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractStackFrameMemoryWord' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         copyLexicalLevel: ll AccessedFrom: aSourceLevelAllocator = ( |
            | 
            (copy lexicalLevel: ll) accessingSourceLevelAllocator: aSourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractStackFrameMemoryWord' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForUplevel: n AccessIn: aSourceLevelAllocator = ( |
            | 
            copyLexicalLevel: lexicalLevel + n AccessedFrom: aSourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractStackFrameMemoryWord' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> () From: ( | {
         'Category: stack frame memory words\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractMemoryArgument = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractMemoryArgument' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein locations abstractStackFrameMemoryWord copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractMemoryArgument' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations abstractMemoryArgument.

CopyDowns:
globals klein locations abstractStackFrameMemoryWord. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractMemoryArgument' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractMemoryArgument' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations abstractMemoryArgument parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | 
            (locationTypeName = x locationTypeName)
            && [(rcvrAndArgNo = x rcvrAndArgNo)
            && [ lexicalLevel = x lexicalLevel ]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         copyRcvrAndArgNo: i = ( |
            | 
            (copyLexicalLevel: 0 AccessedFrom: nil) rcvrAndArgNo: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         copyRcvrAndArgNo: i LexicalLevel: ll AccessedFrom: aSourceLevelAllocator = ( |
            | 
            (copyLexicalLevel: ll AccessedFrom: aSourceLevelAllocator) rcvrAndArgNo: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            locationTypeName hash
             ^^ rcvrAndArgNo hash
             ^^ lexicalLevel hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractMemoryArgument' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractStackFrameMemoryWord' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            ('LL ' & lexicalLevel printString & ', arg ' & rcvrAndArgNo printString) flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractMemoryArgument' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: InitializeToExpression: (0)'
        
         rcvrAndArgNo <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         constant = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein locations abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations constant.

CopyDowns:
globals klein locations abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         oopValue.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations constant parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | 
            x isConstant && [oopMirror = x oopMirror]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForOop: o = ( |
            | copy oopValue: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | oopMirror hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         isConstant = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationForUplevel: n AccessIn: aSourceLevelAllocator = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationTypeName = 'constant'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         oopMirror = ( |
            | reflect: oopValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToLoadMeToRegister: r = ( |
            | 
            aCodeGenerator loadConstantLocation: self ToRegister: r.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'constant' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToStoreMeFromRegister: r = ( |
            | 
            aCodeGenerator storeRegister: r ToConstantLocation: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> () From: ( | {
         'Category: stack frame memory words\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         incomingMemoryArgument = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'incomingMemoryArgument' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein locations abstractMemoryArgument copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'incomingMemoryArgument' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations incomingMemoryArgument.

CopyDowns:
globals klein locations abstractMemoryArgument. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'incomingMemoryArgument' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'incomingMemoryArgument' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations incomingMemoryArgument parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'incomingMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationTypeName = 'incomingMemoryArgument'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'incomingMemoryArgument' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractMemoryArgument' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'incomingMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: saving location info\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         spOffsetFor: aCodeGenerator InFrame: f = ( |
            | aCodeGenerator spOffsetOfIncomingMemoryArgument: self InFrame: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'incomingMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToLoadMeFromFrame: f AtSP: sp ToRegister: r = ( |
            | 
            aCodeGenerator loadIncomingMemoryArgumentLocation: self InFrame: f AtSP: sp ToRegister: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'incomingMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToLoadMeToRegister: r = ( |
            | 
            aCodeGenerator loadIncomingMemoryArgumentLocation: self ToRegister: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'incomingMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToStoreMeFromRegister: r = ( |
            | 
            aCodeGenerator storeRegister: r ToIncomingMemoryArgumentLocation: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> () From: ( | {
         'Category: stack frame memory words\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         nonVolMemoryLocal = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein locations abstractStackFrameMemoryWord copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations nonVolMemoryLocal.

CopyDowns:
globals klein locations abstractStackFrameMemoryWord. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: InitializeToExpression: (0)'
        
         index <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations nonVolMemoryLocal parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | 
            (locationTypeName = x locationTypeName)
             && [(index       = x index)
             && [lexicalLevel = x lexicalLevel]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         copyIndex: i = ( |
            | 
            copy index: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         copyIndex: i AccessedFrom: aSourceLevelAllocator = ( |
            | 
            (copy index: i) accessingSourceLevelAllocator: aSourceLevelAllocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            'nonVolMemoryLocal' hash
              ^^ index          hash
              ^^ lexicalLevel   hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationTypeName = 'nonVolMemoryLocal'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> 'parent' -> () From: ( | {
         'Category: accessing oop\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         oopInActivation: a Frame: f IfFail: fb = ( |
            | 
            [todo cleanup stackRoots]. "Make this deal properly with other lexicalLevels. -- Adam"
            [lexicalLevel = 0] assert.
            a oopAt: a sp + ((f localSPOffsetAt: index) * a myProcess oopSize) IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractStackFrameMemoryWord' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> 'parent' -> () From: ( | {
         'Category: saving location info\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         spOffsetFor: aCodeGenerator InFrame: f = ( |
            | aCodeGenerator spOffsetOfNonVolMemoryLocal: self InFrame: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            ('LL ' & lexicalLevel printString & ', index ' & index printString) flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToLoadMeFromFrame: f AtSP: sp ToRegister: r = ( |
            | 
            aCodeGenerator loadNonVolMemoryLocalLocation: self InFrame: f AtSP: sp ToRegister: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToLoadMeToRegister: r = ( |
            | 
            aCodeGenerator loadNonVolMemoryLocalLocation: self ToRegister: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'nonVolMemoryLocal' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToStoreMeFromRegister: r = ( |
            | 
            aCodeGenerator storeRegister: r ToNonVolMemoryLocalLocation: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         offsetFromOtherLocation = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein locations abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations offsetFromOtherLocation.

CopyDowns:
globals klein locations abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: InitializeToExpression: (nil)'
        
         baseLoc.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: InitializeToExpression: (nil)'
        
         description.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: InitializeToExpression: (nil)'
        
         offset.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations offsetFromOtherLocation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | 
            (locationTypeName = x locationTypeName)
                 && [(offset  = x offset)
                 && [ baseLoc = x baseLoc ]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForOffset: o FromLocation: loc = ( |
            | 
            (copy offset: o) baseLoc: loc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForOffset: o FromRegister: r = ( |
            | 
            copyForOffset: o FromLocation: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | 
            'offsetFromOtherLocation' hash
                     ^^  offset hash
                     ^^ baseLoc hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationTypeName = 'offsetFromOtherLocation'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToLoadMeToRegister: r = ( |
            | 
            description ifNotNil: [|:d| aCodeGenerator comment: 'loading ', d].
            aCodeGenerator materializeSource: baseLoc AndDo: [|:baseReg|
              aCodeGenerator loadValueAtOffset: offset FromAddressInRegister: baseReg ToRegister: r.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'offsetFromOtherLocation' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToStoreMeFromRegister: r = ( |
            | 
            description ifNotNil: [|:d| aCodeGenerator comment: ['setting ', d]].
            aCodeGenerator materializeSource: baseLoc AndDo: [|:baseReg|
              aCodeGenerator storeWordInRegister: r ToOffset: offset FromAddressInRegister: baseReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> () From: ( | {
         'Category: stack frame memory words\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         outgoingMemoryArgument = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'outgoingMemoryArgument' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein locations abstractMemoryArgument copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'outgoingMemoryArgument' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations outgoingMemoryArgument.

CopyDowns:
globals klein locations abstractMemoryArgument. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'outgoingMemoryArgument' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'outgoingMemoryArgument' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein locations outgoingMemoryArgument parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'outgoingMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         locationTypeName = 'outgoingMemoryArgument'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'outgoingMemoryArgument' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'abstractMemoryArgument' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'outgoingMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: saving location info\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         spOffsetFor: aCodeGenerator InFrame: f = ( |
            | aCodeGenerator spOffsetOfOutgoingMemoryArgument: self InFrame: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'outgoingMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToLoadMeToRegister: r = ( |
            | 
            aCodeGenerator loadOutgoingMemoryArgumentLocation: self ToRegister: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'locations' -> 'outgoingMemoryArgument' -> 'parent' -> () From: ( | {
         'Category: generating code\x7fModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         tell: aCodeGenerator ToStoreMeFromRegister: r = ( |
            | 
            aCodeGenerator storeRegister: r ToOutgoingMemoryArgumentLocation: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot'
        
         kleinC1_locs = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_locs' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_locs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinC1_locs.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_locs' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_locs' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_locs' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_locs' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_locs' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.20 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_locs' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_locs InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinC1_locs postFileIn
