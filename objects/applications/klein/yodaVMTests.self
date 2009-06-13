 '$Revision: 1.11 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: FollowSlot'
        
         yodaVMTests = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVMTests' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVMTests' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaVMTests.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVMTests' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVMTests' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVMTests' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVMTests' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVMTests' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 1.11 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVMTests' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         vmTestsImage = bootstrap define: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda virtualMachines abstractVM copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines vmTestsImage.

CopyDowns:
globals kleinAndYoda virtualMachines abstractVM. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines vmTestsImage parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         canCollectGarbage = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fComment: I\'m sure there\'s a more clever way to get
this number. For now it won\'t hurt much
to just keep upping it by hand.
-- Adam, 2/05\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         expectedNumberOfObjects = 50000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         exportPolicy = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines vmTestsImage parent exportPolicy.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: expected offsets\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         expectedMapObjOffsetForActivationPartSizes = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: expected offsets\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         expectedMapObjOffsetForCodes = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: expected offsets\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         expectedMapObjOffsetForLiterals = 6.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: expected offsets\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         expectedObjectLocatorOffsetForLastInvalidEntry = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: expected offsets\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         isOkayToCompileMethodsForReflecteeOf: rMir = ( |
            | 
            "Don't compile anything - we're probably going to build
             an interpreter in C."
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlotToBeMapped: s = ( |
            | 
            (resend.isSlotToBeMapped: s) ifFalse: [^ false].

            [todo cleanup moduleSplitting]. "These slots on globals are in the init module."
                (s name = 'fctProxy')
            || [(s name = 'profiler')
            || [(s name = 'proxy'   )]] ifTrue: [^ false].

            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fComment: Don\'t compile anything; the current plan is to write
an interpreter in C++.\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         modulesToCompile = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> 'modulesToCompile' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines vmTestsImage parent exportPolicy modulesToCompile.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         modulesToMap = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines vmTestsImage parent exportPolicy modulesToMap.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldBlockValueSlotsBeObjectSlots = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldCanonicalizeEmptyVectors = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldEviscerateModuleObjects = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldIncludeKleinPrimitives = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitActivationPartSizes = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitAnnotations = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitLexicalParent = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitLineAndFile = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fCategory: activation maps\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldOmitSourceStrings = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         startingPoints = ( |
            | 
            resend.startingPoints, (vector copyAddLast: lobby)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         objectLocatorProto = ( |
            | 
            vmKit indirectPointerObjectLocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         printPrimitiveSanityCheck = ( |
            | 
            'Testing _Print primitive' _StringPrint.
            1 _Print.
            true _Print.
            self _Print.
            vector _Print.
            () _Print).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
             s.
             sCanon.
            | 
            ('abc', 'defghijaardvark') canonicalize _StringPrint.

            s: ('ambiguous', 'Selector:Type:Delegatee:MethodHolder:Arguments:').
            sCanon: s canonicalize.
            sCanon  _StringPrint.

            "s size printString _StringPrint. '\n' _StringPrint.
            sCanon size printString _StringPrint. '\n' _StringPrint.
            'weird!!!' _Breakpoint."

            maxSmallInt _IntAdd:  1 IfFail: [|:e. :p| e _StringPrint. p _StringPrint. '\n' _StringPrint].
            minSmallInt _IntAdd: -1 IfFail: [|:e. :p| e _StringPrint. p _StringPrint. '\n' _StringPrint].
            minSmallInt _IntSub:  1 IfFail: [|:e. :p| e _StringPrint. p _StringPrint. '\n' _StringPrint].
            maxSmallInt _IntSub: -1 IfFail: [|:e. :p| e _StringPrint. p _StringPrint. '\n' _StringPrint].
            maxSmallInt _IntMul:  2 IfFail: [|:e. :p| e _StringPrint. p _StringPrint. '\n' _StringPrint].
            maxSmallInt _IntMul: -2 IfFail: [|:e. :p| e _StringPrint. p _StringPrint. '\n' _StringPrint].

            [todo cleanup modulePostFileIn]. "Gotta run postFileIn when we start up Yoda or Klein, or something. -- Adam, 5/06"
            '' initializeAscii.

            printPrimitiveSanityCheck.
            tests runYodaSuite).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: InitializeToExpression: (globals tests)'
        
         tests = bootstrap stub -> 'globals' -> 'tests' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> 'vmTestsImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaVMTests InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            yoda).
        } | ) 



 '-- Side effects'

 globals modules yodaVMTests postFileIn
