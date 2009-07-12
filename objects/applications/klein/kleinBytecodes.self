 '$Revision: 30.7 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: recreating codes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         neededLiteralsDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: recreating codes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         transmogrifyYourselfUsing: t = ( |
            | 
            "Extended bytecodes are simpler because they have no parameters,
             so provide a common default here for the simple ones."

            [isExtended  ||  [childMustImplement]] assert.
            t generateParameterlessBytecode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'abstractBranch' -> 'parent' -> () From: ( | {
         'Category: recreating codes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         neededLiteralsDo: blk = ( |
            | 
            operandIfPresent: blk IfAbsent: raiseError.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'abstractBranch' -> 'parent' -> () From: ( | {
         'Category: recreating codes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         transmogrifyYourselfUsing: t = ( |
            | 
            t generateBranchBytecode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'abstractLocal' -> 'parent' -> () From: ( | {
         'Category: recreating codes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         transmogrifyYourselfUsing: t = ( |
            | 
            t generateLocalBytecode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'abstractScalarBranch' -> 'parent' -> () From: ( | {
         'Category: recreating codes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         mapDestinationsBy: blk = ( |
            | 
            destination: blk value: destination.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: recreating codes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         neededLiteralsDo: blk = ( |
            | 
            blk value: selector.
            hasDelegatee ifTrue: [blk value: delegatee].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'abstractSend' -> 'parent' -> () From: ( | {
         'Category: recreating codes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         transmogrifyYourselfUsing: t = ( |
            | 
            t generateSendBytecode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'branchIndexed' -> 'parent' -> () From: ( | {
         'Category: recreating codes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         mapDestinationsBy: blk = ( |
            | 
            destinations: destinations copyMappedBy: [|:destination| blk value: destination].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'delegatee' -> 'parent' -> () From: ( | {
         'Category: recreating codes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         transmogrifyYourselfUsing: t = ( |
            | 
            t generateDelegateeBytecode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> () From: ( | {
         'Category: klein\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         endInit = bootstrap define: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'endInit' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'interpreter' From:
             bootstrap remove: 'parent' From:
             globals abstractBytecodeInterpreter parent bytecodes abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'endInit' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals abstractBytecodeInterpreter parent bytecodes endInit.

CopyDowns:
globals abstractBytecodeInterpreter parent bytecodes abstract. copy 
SlotsToOmit: interpreter parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'endInit' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: InitializeToExpression: (nil)'
        
         interpreter.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'endInit' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'endInit' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals abstractBytecodeInterpreter parent bytecodes endInit parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'endInit' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: private'
        
         doIt = ( |
            | 
            interpreter endInit: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'endInit' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         isExtended = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'endInit' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'literal' -> 'parent' -> () From: ( | {
         'Category: recreating codes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         neededLiteralsDo: blk = ( |
            | 
            blk value: oopToPush.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> 'bytecodes' -> 'literal' -> 'parent' -> () From: ( | {
         'Category: recreating codes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         transmogrifyYourselfUsing: t = ( |
            | 
            t generateLiteralBytecode: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot'
        
         endInit: bc = ( |
            | 
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinAndYoda = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals bytecodeFormat instructionSets kleinAndYoda.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         browsingTagsSoKleinWillCompileSlotsThatAreOnlyPerformed = ( |
            | 
            error: 'this method is not meant to be called'.

            bytecodeFormat instructionSets kleinAndYoda.

            nonlocalReturn. pop.
            branchAlways. branchIfFalse. branchIfTrue. branchIndexed.
            endInit.
            readLocal. writeLocal.
            argumentCount. delegatee. index. lexicalLevel. undirectedResend.
            literal. pushSelf.
            implicitSelfSend. send).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda' -> () From: ( | {
         'Category: opcodes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: private'
        
         extendedOpcodeNames = [ | x =  ( bootstrap setObjectAnnotationOf: vector _Clone From: ( |
                     {} = 'ModuleInfo: Creator: globals bytecodeFormat instructionSets kleinAndYoda extendedOpcodeNames.
'.
                    | ) ) _Clone: 5 Filler: 0| 
             x _At: 0  Put: ().
             x _At: 1  Put: ().
             x _At: 2  Put: ().
             x _At: 3  Put: ().
             x _At: 4  Put: ().
             x] value.
        } | ) 

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'extendedOpcodeNames') -> () _At: 0 Put: (
     'pushSelf')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'extendedOpcodeNames') -> () _At: 1 Put: (
     'pop')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'extendedOpcodeNames') -> () _At: 2 Put: (
     'nonlocalReturn')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'extendedOpcodeNames') -> () _At: 3 Put: (
     'undirectedResend')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'extendedOpcodeNames') -> () _At: 4 Put: (
     'endInit')

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda' -> () From: ( | {
         'Category: opcodes\x7fModuleInfo: Module: kleinBytecodes InitialContents: InitializeToExpression: (())\x7fVisibility: private'
        
         extendedOpcodes = ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         firstBCI = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         includesArgumentCount = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         indexIfNone: noneBlk = ( |
            | 
            1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda' -> () From: ( | {
         'Category: opcodes\x7fModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: private'
        
         opcodeNames = [ | x =  ( bootstrap setObjectAnnotationOf: vector _Clone From: ( |
                     {} = 'ModuleInfo: Creator: globals bytecodeFormat instructionSets kleinAndYoda opcodeNames.
'.
                    | ) ) _Clone: 14 Filler: 0| 
             x _At: 0  Put: ().
             x _At: 1  Put: ().
             x _At: 2  Put: ().
             x _At: 3  Put: ().
             x _At: 4  Put: ().
             x _At: 5  Put: ().
             x _At: 6  Put: ().
             x _At: 7  Put: ().
             x _At: 8  Put: ().
             x _At: 9  Put: ().
             x _At: 10  Put: ().
             x _At: 11  Put: ().
             x _At: 12  Put: ().
             x _At: 13  Put: ().
             x] value.
        } | ) 

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 0 Put: (
     'index')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 1 Put: (
     'literal')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 10 Put: (
     'branchIfFalse')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 11 Put: (
     'branchIndexed')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 12 Put: (
     'delegatee')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 13 Put: (
     'argumentCount')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 2 Put: (
     'send')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 3 Put: (
     'implicitSelfSend')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 4 Put: (
     'extended')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 5 Put: (
     'readLocal')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 6 Put: (
     'writeLocal')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 7 Put: (
     'lexicalLevel')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 8 Put: (
     'branchAlways')

 ((bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda') \/-> 'opcodeNames') -> () _At: 9 Put: (
     'branchIfTrue')

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda' -> () From: ( | {
         'Category: opcodes\x7fModuleInfo: Module: kleinBytecodes InitialContents: InitializeToExpression: (())\x7fVisibility: private'
        
         opcodes = ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'kleinAndYoda' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'bytecodeFormat' -> 'instructionSets' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot'
        
         kleinBytecodes = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBytecodes' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBytecodes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinBytecodes.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBytecodes' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBytecodes' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBytecodes' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBytecodes' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
            bytecodeFormat instructionSets kleinAndYoda initialize.
            resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBytecodes' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinBytecodes' -> () From: ( | {
         'ModuleInfo: Module: kleinBytecodes InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinBytecodes postFileIn
