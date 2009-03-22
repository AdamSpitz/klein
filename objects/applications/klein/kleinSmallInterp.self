 '$Revision: 30.12 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         smallInterpreter = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals yoda virtualMachines smallImage copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines smallInterpreter.

CopyDowns:
globals yoda virtualMachines smallImage. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines smallInterpreter parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fComment: I\'m sure there\'s a more clever way to get
this number. For now it won\'t hurt much
to just keep upping it by hand.
-- Adam, 2/05\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         expectedNumberOfObjects = 35000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         exportPolicy = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines smallInterpreter parent exportPolicy.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlotToBeCompiled: s = ( |
            | 
            [todo cleanup moduleSplitting]. "Put the clone slot in its own module? I don't like having
                                             a whole module just for one method."
            [todo cleanup moduleSplitting]. "The value slots could get their own module, I guess."
            [todo cleanup moduleSplitting]. "What about /+ ?"
            [todo cleanup moduleSplitting]. "true, false, nil, and vector are in the init module. Maybe
                                             we could just compile that module? How big is it?"
            [todo cleanup moduleSplitting]. "Can I get away without compiling the block module?"

               (resend.isSlotToBeCompiled: s)
            || [(s name = '==')
            || [(s name = 'true')
            || [(s name = 'false')
            || [(s name = 'nil')
            || [(s name = 'vector')
            || [(s = ((reflect: defaultBehavior) slotAt: 'clone'))
            || [(s = ((reflect: traits integer ) slotAt: '/+'   ))
            || [((s holder = (reflect: defaultBehavior)) && ['value' isPrefixOf: s name])]]]]]]]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlotToBeMapped: s = ( |
            | 
            (resend.isSlotToBeMapped: s) ifFalse: [^ false].

            [todo cleanup moduleSplitting]. "These slots on globals are in the init module."
                (s name = 'fctProxy')
            || [(s name = 'profiler')
            || [(s name = 'proxy'   )]] ifTrue: [^ false].

            "We don't need the memoryLens, and if we don't map the methods on it then
             we won't compile the double-dispatched methods called by those methods."
            (s holder = reflect: kleinAndYoda memoryLens) ifTrue: [^ false].

            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         modulesToCompile = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines smallInterpreter parent exportPolicy modulesToCompile.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAllNamesOfIncludedModules.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfExcludedModules = bootstrap setObjectAnnotationOf: ( (('kleinBlockInlining')
	& ('vmKitMapImporting')
	& ('vmKitVarHdrs')
	& ('vmKitVarHdrsVerify')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines smallInterpreter parent exportPolicy modulesToCompile namesOfExcludedModules.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfIncludedModules = bootstrap setObjectAnnotationOf: ( (('absBCInterpreter')
	& ('errorHandling')
	& ('klein')
	& ('kleinExport')
	& ('kleinVM')
	& ('vmKitActivations')
	& ('vmKitBase')
	& ('vmKitExport')
	& ('vmKitReflection')
	& ('vmKitVM')
	& ('vmKits')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines smallInterpreter parent exportPolicy modulesToCompile namesOfIncludedModules.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfModulesWhoseSubmodulesShouldBeIncludedToo = bootstrap setObjectAnnotationOf: ( (('block')
	& ('boolean')
	& ('bytecodeFormat')
	& ('integerIteration')
	& ('kleinPrims')
	& ('kleinSmallInterp')
	& ('nil')
	& ('number')
	& ('rootTraits')
	& ('smallInt')
	& ('vmKitMemory')
	& ('vmKitObjects')
	& ('vmKitPrims')
	& ('yodaSmallImage')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines smallInterpreter parent exportPolicy modulesToCompile namesOfModulesWhoseSubmodulesShouldBeIncludedToo.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         modulesToMap = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines smallInterpreter parent exportPolicy modulesToMap.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAllNamesOfIncludedModules.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfExcludedModules = bootstrap setObjectAnnotationOf: ( (('absBCInterpreter')
	& ('activationText')
	& ('all_OS')
	& ('annotation')
	& ('bigInt')
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
	& ('moduleInfo')
	& ('mutationObservers')
	& ('oldStyleRectangle')
	& ('orderedDictionary')
	& ('orderedSet')
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
	& ('stringTests')
	& ('systemOddballs')
	& ('textLines')
	& ('tree')
	& ('ttySupport')
	& ('universalSetAndDictionary')
	& ('vmKitMapImporting')
	& ('vmKitVarHdrs')
	& ('vmKitVarHdrsVerify')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines smallInterpreter parent exportPolicy modulesToMap namesOfExcludedModules.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfIncludedModules = bootstrap setObjectAnnotationOf: ( (('')
	& ('absBCInterpreter')
	& ('klein')
	& ('kleinExport')
	& ('kleinReflection')
	& ('kleinVM')
	& ('vmKitActivations')
	& ('vmKitBase')
	& ('vmKitExport')
	& ('vmKitMirrorsGen')
	& ('vmKitReflection')
	& ('vmKitVM')
	& ('vmKits')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines smallInterpreter parent exportPolicy modulesToMap namesOfIncludedModules.

CopyDowns:
globals set. copyRemoveAll 
SlotsToOmit: comparisonTraits.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfModulesWhoseSubmodulesShouldBeIncludedToo = bootstrap setObjectAnnotationOf: ( (('allCore')
	& ('init')
	& ('kleinFrames')
	& ('kleinNMethod')
	& ('kleinPrims')
	& ('kleinSmallInterp')
	& ('vmKitMemory')
	& ('vmKitObjects')
	& ('vmKitPrims')
	& ('yodaSmallImage')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines smallInterpreter parent exportPolicy modulesToMap namesOfModulesWhoseSubmodulesShouldBeIncludedToo.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBlockValueSlotsBeObjectSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldCanonicalizeEmptyVectors = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldCompileInDebugMode = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldEviscerateModuleObjects = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitActivationPartSizes = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitAnnotations = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitLexicalParent = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitLineAndFile = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitSourceStrings = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         startingPoints = ( |
             r.
            | 
            r: list copyContaining: resend.startingPoints.
            r add: lobby.
            r add: abstractBytecodeInterpreter.
            r add: klein.
            r add: globals.
            r add: bytecodeFormat.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         wellKnownIncompleteObjectsWithSlotsToCompile = ( |
            | 
            [todo cleanup exportPolicyFactoring].
             "Should this exportPolicy have its own set of these? It'd be nice
              to factor them somehow. -- Adam, 12/05"

            klein virtualMachines selfVM exportPolicy wellKnownIncompleteObjectsWithSlotsToCompile).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'smallImage' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         start = ( |
             v <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'verifier' -> ().
            | 
            ["Doesn't work anymore - not sure why. -- Adam, 5/06"
            v: (v copy vm: self) initializeValueReturningFailBlock.
            universe verifyWith: v.
            _Breakpoint: 'Done verification. Now what?'.
            ].

            _Breakpoint: 'Gotta start up the interpreter, I guess'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'smallInterpreter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot'
        
         kleinSmallInterp = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSmallInterp' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSmallInterp' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinSmallInterp.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSmallInterp' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSmallInterp' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSmallInterp' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSmallInterp' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSmallInterp' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.12 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSmallInterp' -> () From: ( | {
         'ModuleInfo: Module: kleinSmallInterp InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinSmallInterp postFileIn
