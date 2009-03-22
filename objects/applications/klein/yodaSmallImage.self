 '$Revision: 30.14 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot'
        
         yodaSmallImage = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaSmallImage' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaSmallImage' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaSmallImage.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaSmallImage' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaSmallImage' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaSmallImage' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaSmallImage' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaSmallImage' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.14 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaSmallImage' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         smallImage = bootstrap define: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda virtualMachines abstractVM copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines smallImage.

CopyDowns:
globals kleinAndYoda virtualMachines abstractVM. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines smallImage parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         allocateALotOfGarbage = ( |
            | 
            20000 do: [|:i|
              pointerFromOldToNew: (| a. b. c. d. e. f. g. h. i. j. k. l. m. n. o. p. q. r. s. t. u. v. w. x. y. z |) _Clone.
              pointerFromOldToNew: byteVector copySize: i % 64.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: InitializeToExpression: (\'assignableSlot initial contents\')\x7fVisibility: private'
        
         assignableSlot <- 'assignableSlot initial contents'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         check: checkB Against: againstB Named: name = ( |
             against.
             check.
            | 
            'Checking ' _StringPrint.
            name _StringPrint.
            '\n' _StringPrint.
            check: checkB value.
            against: againstB value.
            (check _Eq: against) ifFalse: [
              name _StringPrint.
              'FAILED\n' _StringPrint.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fComment: I\'m sure there\'s a more clever way to get
this number. For now it won\'t hurt much
to just keep upping it by hand.
-- Adam, 2/05

Upped to 50K for debugger work w/ source strings.
-- David 3/06\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         expectedNumberOfObjects = 30000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         exportPolicy = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines smallImage parent exportPolicy.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: expected offsets\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         expectedMapObjOffsetForActivationPartSizes = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: expected offsets\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         expectedMapObjOffsetForCodes = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: expected offsets\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         expectedMapObjOffsetForLiterals = 6.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: expected offsets\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         expectedObjectLocatorOffsetForLastInvalidEntry = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: expected offsets\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         ifSlot: s HasExpectedOffsetThen: blk = ( |
            | 
            [activationPartSizes. codes. literals]. "browsing"
            s holder isReflecteeVMKitActivationMap ifTrue: [
              s name = 'codes'               ifTrue: [^ blk value: expectedMapObjOffsetForCodes              ].
              s name = 'literals'            ifTrue: [^ blk value: expectedMapObjOffsetForLiterals           ].
              s name = 'activationPartSizes' ifTrue: [^ blk value: expectedMapObjOffsetForActivationPartSizes].
            ].

            [lastInvalidEntry]. "browsing"
            s name = 'lastInvalidEntry'      ifTrue: [^ blk value: expectedObjectLocatorOffsetForLastInvalidEntry].

            resend.ifSlot: s HasExpectedOffsetThen: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         isOkayToCompileMethodsForReflecteeOf: rMir = ( |
            | 
            "Don't compile anything - we're building an interpreter."
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlotToBeMapped: s = ( |
            | 
            (resend.isSlotToBeMapped: s) ifFalse: [^ false].

            [todo optimize space]. "We also don't need the segregated stuff for now; how can I achieve that?"

            [todo cleanup moduleSplitting]. "These slots on globals are in the init module."
                (s name = 'fctProxy')
            || [(s name = 'profiler')
            || [(s name = 'proxy'   )]] ifTrue: [^ false].

            "We don't need the memoryLens (or any of the double-dispatched
             methods called by them, but this won't help much with that).
             -- Adam, 5/06"
            (s holder = reflect: kleinAndYoda memoryLens) ifTrue: [^ false].

            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fComment: Don\'t compile anything; the current plan is to write
an interpreter in C++.\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         modulesToCompile = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines smallImage parent exportPolicy modulesToCompile.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAllNamesOfIncludedModules.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfExcludedModules = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfIncludedModules = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfModulesWhoseSubmodulesShouldBeIncludedToo = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         modulesToMap = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines smallImage parent exportPolicy modulesToMap.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAllNamesOfIncludedModules.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfExcludedModules = bootstrap setObjectAnnotationOf: ( (('absBCInterpreter')
	& ('activationText')
	& ('all_OS')
	& ('ancestorTraversal')
	& ('annotation')
	& ('bigInt')
	& ('bytecodeFormat')
	& ('debugger')
	& ('defaultPreferences')
	& ('fakeSlot')
	& ('fakeSlotsIterator')
	& ('fileStream')
	& ('foreign')
	& ('generatedCases')
	& ('hosts')
	& ('interceptor')
	& ('methodText')
	& ('mirrorAnnoInfo')
	& ('mirrorEnvSupport')
	& ('missingSlots')
	& ('moduleInfo')
	& ('mutationObservers')
	& ('oldStyleRectangle')
	& ('orderedDictionary')
	& ('orderedSet')
	& ('patternMatching')
	& ('ping')
	& ('processStack')
	& ('profiler')
	& ('profiling')
	& ('programmingSupport')
	& ('prompt')
	& ('rectangle')
	& ('selectionText')
	& ('sequence')
	& ('shell')
	& ('smallIntTests')
	& ('snapshotAction')
	& ('socketServer')
	& ('sortedSequence')
	& ('stat')
	& ('stdin')
	& ('stringTests')
	& ('systemOddballs')
	& ('textLines')
	& ('tree')
	& ('ttySupport')
	& ('universalSetAndDictionary')
	& ('vmKitCloning')
	& ('vmKitMapImporting')
	& ('vmKitVarHdrs')
	& ('vmKitVerifier')
	& ('yodaVarHdrs')
	& ('yodaWellKnowns')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines smallImage parent exportPolicy modulesToMap namesOfExcludedModules.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfIncludedModules = bootstrap setObjectAnnotationOf: ( (('')
	& ('vmKitActivations')
	& ('vmKitBase')
	& ('vmKitMirrorsGen')
	& ('vmKitReflection')
	& ('vmKitVM')
	& ('vmKits')
	& ('yoda')
	& ('yodaActivations')
	& ('yodaReflection')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines smallImage parent exportPolicy modulesToMap namesOfIncludedModules.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfModulesWhoseSubmodulesShouldBeIncludedToo = bootstrap setObjectAnnotationOf: ( (('allCore')
	& ('init')
	& ('vmKitMemory')
	& ('vmKitObjects')
	& ('vmKitPrims')
	& ('yodaObjects')
	& ('yodaSmallImage')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines smallImage parent exportPolicy modulesToMap namesOfModulesWhoseSubmodulesShouldBeIncludedToo.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBlockValueSlotsBeObjectSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldCanonicalizeEmptyVectors = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldEviscerateModuleObjects = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitActivationPartSizes = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitAnnotations = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitLexicalParent = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitLineAndFile = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitSourceStrings = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         startingPoints = ( |
            | 
            resend.startingPoints, (vector copyAddLast: lobby)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         factorial: n = ( |
            | 
            (n _IntEQ: 1) ifTrue: [^ 1].
            n _IntMul: factorial: n _IntSub: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         from: x UpTo: y Do: blk = ( |
            | 
            (x _IntEQ: y) ifTrue: [^ self].
            blk value: x.
            from: (x _IntAdd: 1) UpTo: y Do: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         indexedInLoop: x = ( |
             i <- 0.
            | 
            __DefineLabel: 'loop'.
                    i: i _IntAdd: 1.
                    (i _IntEQ: 5) ifTrue: [^ 'ok'].
            666 __BranchIndexedBy: x To: 'loop'.
            error: 'just checking').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         iterativeFactorial: n = ( |
             p <- 1.
            | 
            from: 1 UpTo: (n _IntAdd: 1) Do: [|:i| p: p _IntMul: i].
            p).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         nlr1 = ( |
            | 
            [^ 'nlr 1\n'] value.
            'BAD - SHOULD NOT PRINT\n' _StringPrint.
            'THIS SHOULD NOT PRINT EITHER\n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         objectLocatorProto = ( |
            | 
            vmKit indirectPointerObjectLocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         primitiveIfFailTests = ( |
            | 
            3 _IntAdd: 'a' IfFail: [ |:errorName. :primSelector|
              ' Testing primitive failure. Primitive failed with explicit failblock.' _StringPrint.
              ' ErrorName: ' _StringPrint. errorName _StringPrint. '  Selector: ' _StringPrint. primSelector _StringPrint.
              '\n' _StringPrint
            ].
            3 _IntAdd: 'a'.
            _ThisPrimitiveShouldNotExistIfFail: [|:e. :n|
              ('Non existant primitive ', n,' failed with a failblock. Error:', e) _StringPrint].
            _ThisPrimitiveShouldNotExist).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         printArg: a = ( |
            | 
            a _StringPrint.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         printArg: a And: b = ( |
            | 
            a _StringPrint.
            b _StringPrint.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
             i.
             n.
             s = 'This is a cloned byteVector.abcdefghijklmnop' copyMutable.
            | 

            [todo cleanup]. allocateALotOfGarbage. "Should probably move this into a separate place for GC tests."

            'hello world\n' _StringPrint.

             ['testing debugger in block' _Breakpoint] value.
              'testing debugger' _Breakpoint.
            printArg: 'this is an argument being passed in\n'.
            printArg: 'this is another argument being passed in\n'.
            printArg: 'arg 1\n' And: 'arg 2\n'.


            testBlocks. 
            testScalarBranches.
            testIndexedBranches.
            [testLoop]. "This is just an infinite loop."
            from: 0 UpTo: (iterativeFactorial: 4) Do: [|:i| 'this should be printed 24 times\n' _StringPrint].

            (s _CloneBytes: 28 Filler: 97) _StringPrint. '\n' _StringPrint.
            primitiveIfFailTests.

            ('testing string ', 'concatenation', '\n') _StringPrint.

            assignableSlot _StringPrint. '\n' _StringPrint.
            assignableSlot: assignableSlot, ' have been changed!\n'.
            assignableSlot _StringPrint.

            [(3 + 4) printString _StringPrint.
            [3 + 4] value printString _StringPrint.
            i: 0.
            [i < 10] whileTrue: [i print. i: i succ].].

            'finished\n' _StringPrint).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         testBlocks = ( |
             asgLocal <- 'asgLocal
'.
             constLocal = 'constLocal
'.
            | 
            ['this is being printed from inside a block\n' _StringPrint] value.
            ['this is being returned from a block\n'] value _StringPrint.
            [|:arg| arg _StringPrint] value: 'this is an arg being passed into a block and printed\n'.
            [constLocal _StringPrint. asgLocal _StringPrint] value.
            [| innerAsgLocal1 <- 'innerAsgLocal1\n' |
                 [| innerAsgLocal2 <- 'innerAsgLocal2\n' |
                      constLocal _StringPrint. asgLocal _StringPrint. innerAsgLocal1 _StringPrint. innerAsgLocal2 _StringPrint] value] value.
            [asgLocal: 'modifiedAsgLocal\n'] value.
            asgLocal _StringPrint.

            testNLRs.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         testIndexedBranches = ( |
            | 
            check: [ testThreeCases: 'a']
             Against: 'fell'
             Named:   'branch indexed by string'.

            check: [ testThreeCases: -1]
             Against: 'fell'
             Named:   'branch indexed by -1'.

            check: [ testThreeCases: 3]
             Against: 'fell'
             Named:   'branch indexed by out of range'.

            check: [ testThreeCases: 0]
             Against: 'a'
             Named:   'branch indexed by 0'.

            check: [ testThreeCases: 1]
             Against: 'b'
             Named:   'branch indexed by 1'.

            check: [ testThreeCases: 2]
             Against: 'c'
             Named:   'branch indexed by 2'.

            check: [ indexedInLoop: 0 ]
              Against: 'ok'
              Named: 'branch indexed in loop'.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         testLoop = ( |
            | 
            'OK, I just can\'t resist. Infinite loop?\n' _StringPrint.
            _Restart).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         testNLRs = ( |
            | 
            'about to do nlr1\n' _StringPrint.
            nlr1 _StringPrint.
            'done nlr1\n' _StringPrint.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         testScalarBranches = ( |
            | 
            'gonna branch now\n' _StringPrint.
            __BranchIfTrue: true To: 'target1'.
            'SHOULD NOT PRINT THIS\n' _StringPrint.
            __DefineLabel: 'target1'.
            'should see the word "aardvark" on the next line:\n' _StringPrint.
            __BranchIfTrue: false To: 'target2'.
            'aardvark\n' _StringPrint.
            __DefineLabel: 'target2'.
            'should see the word "aardvark" on the previous line\n' _StringPrint.

            __BranchTo: 'target3'.
            'SHOULD NOT PRINT THIS EITHER\n' _StringPrint.
            __DefineLabel: 'target3'.

            'gonna branch on false now\n' _StringPrint.
            __BranchIfFalse: false To: 'target4'.
            'SHOULD NOT PRINT THIS 3\n' _StringPrint.
            __DefineLabel: 'target4'.
            'should see the word "kumquat" on the next line:\n' _StringPrint.
            __BranchIfFalse: true To: 'target5'.
            'kumquat\n' _StringPrint.
            __DefineLabel: 'target5'.
            'should see the word "kumquat" on the previous line\n' _StringPrint.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         testThreeCases: x = ( |
            | 
            666 __BranchIndexedBy: x To: 'zero' To: 'one' To: 'two'.
            'fell' __BranchTo: 'end'.

            ((__DefineLabel: 'zero') _IntEQ: 666) ifFalse: [error: 'bad expr'].
            'a' __BranchTo: 'end'.

            ((__DefineLabel: 'one') _IntEQ: 666) ifFalse: [error: 'bad expr'].
            'b' __BranchTo: 'end'.

            ((__DefineLabel: 'two') _IntEQ: 666) ifFalse: [error: 'bad expr'].
            'c' __BranchTo: 'end'.

            error: 'just checking'.

            'no way' __DefineLabel: 'end').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            yoda).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: InitializeToExpression: (nil)'
        
         pointerFromOldToNew.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAllNamesOfIncludedModules.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfExcludedModules = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfIncludedModules = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfModulesWhoseSubmodulesShouldBeIncludedToo = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAllNamesOfIncludedModules.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfExcludedModules = bootstrap setObjectAnnotationOf: ( (('absBCInterpreter')
	& ('activationText')
	& ('all_OS')
	& ('ancestorTraversal')
	& ('annotation')
	& ('bytecodeFormat')
	& ('debugger')
	& ('defaultPreferences')
	& ('fakeSlot')
	& ('fakeSlotsIterator')
	& ('fileStream')
	& ('foreign')
	& ('generatedCases')
	& ('hosts')
	& ('interceptor')
	& ('methodText')
	& ('mirrorAnnoInfo')
	& ('mirrorEnvSupport')
	& ('missingSlots')
	& ('moduleInfo')
	& ('mutationObservers')
	& ('oldStyleRectangle')
	& ('orderedDictionary')
	& ('orderedSet')
	& ('patternMatching')
	& ('ping')
	& ('processStack')
	& ('profiler')
	& ('profiling')
	& ('programmingSupport')
	& ('prompt')
	& ('rectangle')
	& ('selectionText')
	& ('sequence')
	& ('shell')
	& ('snapshotAction')
	& ('socketServer')
	& ('sortedSequence')
	& ('stat')
	& ('stdin')
	& ('textLines')
	& ('tree')
	& ('ttySupport')
	& ('universalSetAndDictionary')
	& ('vmKitCloning')
	& ('vmKitVerifier')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines vmTestsImage parent exportPolicy modulesToMap namesOfExcludedModules.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfIncludedModules = bootstrap setObjectAnnotationOf: ( (('')
	& ('vmKitActivations')
	& ('vmKitBase')
	& ('vmKitMirrorsGen')
	& ('vmKitReflection')
	& ('vmKitVM')
	& ('vmKits')
	& ('yoda')
	& ('yodaActivations')
	& ('yodaReflection')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines vmTestsImage parent exportPolicy modulesToMap namesOfIncludedModules.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfModulesWhoseSubmodulesShouldBeIncludedToo = bootstrap setObjectAnnotationOf: ( (('allCore')
	& ('init')
	& ('int32and64')
	& ('list')
	& ('smallInt')
	& ('string')
	& ('tests')
	& ('vmKitMemory')
	& ('vmKitObjects')
	& ('vmKitPrims')
	& ('yodaObjects')
	& ('yodaTests')
	& ('yodaVMTests')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines vmTestsImage parent exportPolicy modulesToMap namesOfModulesWhoseSubmodulesShouldBeIncludedToo.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: yodaSmallImage InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> ().
        } | ) 



 '-- Side effects'

 globals modules yodaSmallImage postFileIn
