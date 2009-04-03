 '$Revision: 30.117 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         midiVM = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda virtualMachines abstractVM copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM.

CopyDowns:
globals kleinAndYoda virtualMachines abstractVM. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> () From: ( |
             {} = 'Comment: The Klein Self Virtual Machine.
Once created, this object is mapped, exported, and
finally launched in a foreignProcess ``over there\'\'
where it initializes itself then starts executing
Self programs.

-- jb 6/03\x7fModuleInfo: Creator: globals klein virtualMachines midiVM parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fCategory: heap size\x7fComment: I\'m sure there\'s a more clever way to get
this number. For now it won\'t hurt much
to just keep upping it by hand.
-- Adam, 2/05\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         expectedNumberOfObjects = 100000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> () From: ( | {
         'Category: exporting\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         exportPolicy = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'exportPolicy' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent exportPolicy.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         modulesToMap = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent exportPolicy modulesToMap.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAllNamesOfIncludedModules.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfExcludedModules = bootstrap setObjectAnnotationOf: ( (('vmKitMapImporting')
	& ('vmKitVarHdrs')
	& ('vmKitVarHdrsUniv')
	& ('vmKitVarHdrsVerify')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent exportPolicy modulesToMap namesOfExcludedModules.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfIncludedModules = bootstrap setObjectAnnotationOf: ( (('')
	& ('annotation')
	& ('block')
	& ('boolean')
	& ('caseStatement')
	& ('collection')
	& ('collector')
	& ('defaultBehavior')
	& ('errorHandling')
	& ('indexable')
	& ('init')
	& ('int32and64')
	& ('klein')
	& ('kleinDB')
	& ('kleinExport')
	& ('kleinFrames')
	& ('kleinNMethod')
	& ('kleinPrims')
	& ('kleinReflection')
	& ('kleinRelocators')
	& ('kleinSelfVM')
	& ('kleinSendDesc')
	& ('kleinVM')
	& ('list')
	& ('mirror')
	& ('moduleInfo')
	& ('nil')
	& ('number')
	& ('path')
	& ('point')
	& ('rootTraits')
	& ('sending')
	& ('slot')
	& ('string')
	& ('vector')
	& ('vmKitBase')
	& ('vmKitDB')
	& ('vmKitExport')
	& ('vmKitMirrorsGen')
	& ('vmKitReflection')
	& ('vmKitVM')
	& ('vmKits')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent exportPolicy modulesToMap namesOfIncludedModules.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfModulesWhoseSubmodulesShouldBeIncludedToo = bootstrap setObjectAnnotationOf: ( (('integer')
	& ('scheduler')
	& ('setAndDictionary')
	& ('smallInt')
	& ('vmKitBase')
	& ('vmKitGC')
	& ('vmKitMemory')
	& ('vmKitObjects')
	& ('vmKitPrims')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent exportPolicy modulesToMap namesOfModulesWhoseSubmodulesShouldBeIncludedToo.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         startingPoints = ( |
            | 
            resend.startingPoints copyAddLast: lobby).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fComment: Objects that are not complete yet for which we would like to
select the slots to compile by sending kleinSelectorsToCompile.\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         wellKnownIncompleteObjectsWithSlotsToCompile = bootstrap setObjectAnnotationOf: ( ((reflect: kleinAndYoda)
	& (reflect: klein stackFrames)
	& (reflect: klein mirrors)
	& (reflect: slots)
	& (reflect: lobby)
	& (reflect: traits block)
	& (reflect: traits abstractSet values)
	& (reflect: kleinAndYoda maps)
	& (reflect: kleinAndYoda virtualMachines abstractVM stringComparisonMixin)
	& (reflect: klein)
	& (reflect: klein sendDescs)
	& (reflect: kleinAndYoda layouts)
	& (reflect: traits abstractSetOrDictionary equalityComparisonMixin)
	& (reflect: traits abstractSetOrDictionary reflectiveIdentityComparisonMixin)
	& (reflect: traits abstractSetOrDictionary identityComparisonMixin)
	& (reflect: scheduler)) asVmKitExportList) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent exportPolicy wellKnownIncompleteObjectsWithSlotsToCompile.

CopyDowns:
globals set. copy 
SlotsToOmit: parent prototype safety.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
            | 
            "_Breakpoint:  'Entered start method'."

            tests runAllTestsForVM: self.

            _Breakpoint: 'Exiting start method').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         tests = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests abstract.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: blk = ( |
            | 
            blk value ifFalse: [_Breakpoint: 'Klein test failed'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: a Is: b = ( |
            | 
            "This method is just useful because you
             can click on a and b right in the debugger
             to see what the two values are."
            assert: [a = b]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         assertFail: blk = ( |
            | 
            blk value: [^ self].
            fail).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         fail = ( |
            | 
            assert: [false]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         allocation = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'allocation' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests allocation.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'allocation' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'allocation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests allocation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'allocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: blk = ( |
            | 
            blk assert: 'allocation test failed').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'allocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         colocatedLocation1 = ( |
            | 
            17   =    (17 __BranchIfTrue: true To: 'one').
            self foo: (true  __BranchIfTrue: true  To: 'one').
            23   +     __DefineLabel: 'one').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'allocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         colocatedLocation2 = ( |
            | 
            17   = (self __BranchIfTrue: false To: 'one').
            self bar: (true  __BranchIfTrue: true  To: 'one').
            23 foo: __DefineLabel: 'one').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'allocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         colocatedLocation3 = ( |
            | 
            17   =    (self __BranchIfTrue: false To: 'one').
            self foo: (true  __BranchIfTrue: false  To: 'one').
            23   =     __DefineLabel: 'one').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'allocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         colocatedLocation4 = ( |
             local <- 5.
            | 
            foo: local __BranchIfTrue: true To: 'a'.
            2 __BranchIfTrue: true To: 'b'.
            (7 __DefineLabel: 'a') __DefineLabel: 'b').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'allocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         foo: bur = ( |
            | true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'allocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         foo: foo Bar: bar Baz: baz = ( |
            | 
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'allocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'allocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            runColocatedLocationTests.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'allocation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         runColocatedLocationTests = ( |
            | 
            assert: [colocatedLocation1 = 34].
            assert: [colocatedLocation2].
            assert: [colocatedLocation3 = false].
            assert: [colocatedLocation4 = 5].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         blocks = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests blocks.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests blocks parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         blockTest = ( |
            | 
            "Note: Accessing data slots uplevel from blocks is included
                   in the dataSlots test suite and so is not repeated 
                   here.  -- jb 8/03"
            blockTest1.
            blockTest2.
            blockTest3.
            blockTest4.

            blockTestUplevel1.
            blockTestUplevel2.
            blockTestUplevel3: 45).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fCategory: without uplevel accesses\x7fComment: no locals, no args\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         blockTest1 = ( |
            | 
            assert: [[42] value = 42]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fCategory: without uplevel accesses\x7fComment: no locals, some args\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         blockTest2 = ( |
            | 
            assert: [([|:v| v] value: 42) = 42].
            assert: [([|:b. :a. :c| (b * a) - c] value: 10 With: 5 With: 8) = 42]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fCategory: without uplevel accesses\x7fComment: some locals, no args\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         blockTest3 = ( |
            | 
            assert: [([|a = 1. b <- 2. c| c: a + b. c] value) = 3]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fCategory: without uplevel accesses\x7fComment: some locals, some args\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         blockTest4 = ( |
            | 
            assert: [([|:v. :w. a <- 1. b = 2| (v * w) - (a + b)] value: 9 With: 5) = 42].
            assert: [([|:b. :a. :c. z = 9. aa <- 13 | aa: aa + (b * a). (aa / z) - c] 
                        value: 10 With: 5 With: 8) = -1]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fCategory: with uplevel acceses\x7fComment: uplevel access to self\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         blockTestUplevel1 = ( |
            | 
            "lexical level = 1"
            assert: [blockTestUplevel1b].
            assert: [blockTestUplevel1c].
            assert: [blockTestUplevel1d].

            "lexical level = 2"
            [
              assert: [blockTestUplevel1b].
              assert: [blockTestUplevel1c].
              assert: [blockTestUplevel1d].
            ] value.

            "lexical level = 3"
            [
              assert: [[blockTestUplevel1b] value].
              assert: [[blockTestUplevel1c] value].
              assert: [[blockTestUplevel1d] value].
            ] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fCategory: with uplevel acceses\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         blockTestUplevel1b = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fCategory: with uplevel acceses\x7fModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (true)\x7fVisibility: private'
        
         blockTestUplevel1c <- bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fCategory: with uplevel acceses\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         blockTestUplevel1d = ( |
            | true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fCategory: with uplevel acceses\x7fComment: uplevel access to a local, and to self\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         blockTestUplevel2 = ( |
             savedSelf.
            | 
            savedSelf: self.
            assert: [savedSelf == self]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: blocks\x7fCategory: with uplevel acceses\x7fComment: uplevel access to locals and arguments\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         blockTestUplevel3: fortyFive = ( |
             a = 1.
             b <- 2.
             c.
            | 
            "lexical level = 1"
            c: nil.
            assert: [c: a + b. (fortyFive - c) = 42].
            assert: [c = 3].

            "lexical level = 2"
            c: nil.
            [
              assert: [c: a + b. (fortyFive - c) = 42].
            ] value.
            assert: [c = 3].

            "lexical level = 3"
            c: nil.
            [
              assert: [[c: a + b. (fortyFive - c) = 42] value].
            ] value.
            assert: [c = 3]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fCategory: methods with NLRs calling methods with NLRs\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         callThisBlock: b = ( |
            | 
            b value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fCategory: methods with NLRs calling methods with NLRs\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         doSomethingWithNLRsAndThenCallSomethingThatCallsThisBlock: b = ( |
            | 
            0 < 1 ifTrue: [^ callThisBlock: b].
            error: 'zero is not less than one?').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fCategory: methods with NLRs calling methods with NLRs\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         doSomethingWithNLRsAndThenCallThisBlock: b = ( |
            | 
            0 < 1 ifTrue: [^ b value].
            error: 'zero is not less than one?').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fCategory: methods with NLRs calling methods with NLRs\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         doSomethingWithoutNLRsAndThenCallSomethingThatCallsThisBlock: b = ( |
            | 
            0 < 1 ifTrue: [callThisBlock: b].
            error: 'zero is not less than one?').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fCategory: methods with NLRs calling methods with NLRs\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         doesReturnFive = ( |
            | 
            doSomethingWithoutNLRsAndThenCallSomethingThatCallsThisBlock: [^ 5].
            6).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fCategory: methods with NLRs calling methods with NLRs\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         doesReturnSeventySeven = ( |
            | 
            doSomethingWithNLRsAndThenCallSomethingThatCallsThisBlock: [77]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         nlrTest = ( |
            | 
            assert: nlrTest1 Is: 42.
            assert: nlrTest2 Is: 42.
            assert: nlrTest3 Is: 42.

            assert: (nlrTest4: 21) Is: 42.
            assert: (nlrTest5: 21) Is: 42.
            assert: nlrTest6 Is: 1. 
            assert: nlrTest7 Is: 5.
            assert: nlrTest8 Is: 12.

            assert: doesReturnFive Is: 5.
            assert: doesReturnSeventySeven Is: 77.
            assert: shouldReturnFive Is: 5. "Tries to call callThisBlock: on 1 or 5 or something."
            assert: shouldAlsoReturnFive Is: 5. "Returns 6."

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrTest1 = ( |
            | 
            [^ 42] value + 5).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrTest2 = ( |
            | [[^ 42] value + 5] value + 7).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrTest3 = ( |
            | [2 + [3 + [^ 42] value] value] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrTest4: v = ( |
            | [^ v * 2] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrTest5: v = ( |
            | nlrTest5b: [^ v * 2]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrTest5b: blk = ( |
            | blk value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrTest6 = ( |
            | 
            [3.
             1 __BranchTo: 'end'.
             ^ 2 __DefineLabel: 'end'
            ] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fComment: tests nlr in a leaf method\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrTest7 = ( |
            | [^5] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrTest8 = ( |
            | 
            [|:x| ^ x ] value: 12).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            blockTest.
            nlrTest).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fCategory: methods with NLRs calling methods with NLRs\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldAlsoReturnFive = ( |
             r.
            | 
            r: doSomethingWithNLRsAndThenCallThisBlock: [^ 5].
            6).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'blocks' -> 'parent' -> () From: ( | {
         'Category: non-local returns\x7fCategory: methods with NLRs calling methods with NLRs\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldReturnFive = ( |
            | 
            doSomethingWithNLRsAndThenCallSomethingThatCallsThisBlock: [^ 5].
            6).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         branches = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'branches' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'branches' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests branches.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'branches' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'branches' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests branches parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'branches' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         check: checkB Against: againstB Named: name = ( |
             against.
             check.
            | 
            check: checkB value.
            against: againstB value.
            assert: check Is: against.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'branches' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         indexedInLoop: x = ( |
             i <- 0.
            | 
            __DefineLabel: 'loop'.
                    i: i _IntAdd: 1.
                    (i _IntEQ: 5) ifTrue: [^ 'ok'].
            666 __BranchIndexedBy: x To: 'loop'.
            error: 'just checking').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'branches' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'branches' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            testScalarBranches.
            testIndexedBranches.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'branches' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'branches' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testScalarBranches = ( |
             a <- 0.
            | 
            assert: a Is: 0.

            __BranchIfTrue: true To: 'target1'.
            shouldNotGetHere.
            __DefineLabel: 'target1'.

            __BranchIfTrue: false To: 'target2'.
            a: 1.
            __DefineLabel: 'target2'.
            assert: a Is: 1.

            __BranchTo: 'target3'.
            shouldNotGetHere.
            __DefineLabel: 'target3'.

            __BranchIfFalse: false To: 'target4'.
            shouldNotGetHere.
            __DefineLabel: 'target4'.

            __BranchIfFalse: true To: 'target5'.
            a: 2.
            __DefineLabel: 'target5'.
            assert: a Is: 2.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'branches' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         byteVectors = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'byteVectors' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'byteVectors' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests byteVectors.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'byteVectors' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'byteVectors' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests byteVectors parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'byteVectors' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'byteVectors' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            testSuccess.
            testFailure.
            testStringCanonicalization.
            testBoundsChecking.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'byteVectors' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testBoundsChecking = ( |
             bv.
             objectAfterBVOnTheHeap.
            | 
            bv: byteVector copySize: 47.
            objectAfterBVOnTheHeap: copy.

            "Make sure that in-bounds indices work."
            bv at: 0 PutByte: 9.
            assert: (bv byteAt:  0) Is: 9.
            bv at: 18 PutByte: 23.
            assert: (bv byteAt: 18) Is: 23.
            assert: (bv byteAt: 46) Is: 0.

            "Make sure that out-of-bounds indices fail."
            assertFail: [|:fb| bv byteAt: 47              IfAbsent: fb].
            assertFail: [|:fb| bv byteAt: -1              IfAbsent: fb].
            assertFail: [|:fb| bv     at: -7  PutByte: 10 IfAbsent: fb].
            assertFail: [|:fb| bv     at: 293 PutByte: 10 IfAbsent: fb].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'byteVectors' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testFailure = ( |
            | 
            assertFail: [|:fb| 6543 _ByteSizeIfFail: fb].
            assertFail: [|:fb| true _ByteSizeIfFail: fb].

            assertFail: [|:fb|     3 _ByteVectorCompare: 'abc' IfFail: fb].
            assertFail: [|:fb| 'abc' _ByteVectorCompare: false IfFail: fb].
            assertFail: [|:fb|   nil _ByteVectorCompare: false IfFail: fb].

            assertFail: [|:fb| byteVector _CloneBytes: -1 Filler: 0 IfFail: fb].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'byteVectors' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testStringCanonicalization = ( |
             c.
             r1.
             r2.
             s.
            | 

            "This test is meant to ensure that calling _StringCanonicalize on
             a canonicalString does the right thing (i.e. returns self).
             Therefore, we must call _StringCanonicalize directly, because
             calling canonicalize on a canonicalString won't result in a call
             to _StringCanonicalize; it will just return self without
             bothering to call the primitive. -- Adam, 5/05"
            c: 'cccccc' _StringCanonicalize.
            assert: [c == 'cccccc'].

            s: 'adam', 'banana'.
            c: s canonicalize.
            assert: [c isCanonical    ].
            assert: [s isCanonical not].
            c with: s Do: [|:b1. :b2| assert: b1 Is: b2].

            r1: mutableString copySize: 6 FillingWith: 'b'.
            r2: mutableString copySize: 6 FillingWith: 'b'.

            assert: [r1              !== r2             ].
            assert: [r1 canonicalize  == r2 canonicalize].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'byteVectors' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testSuccess = ( |
             s.
            | 
            assert: [    ''  < ' '       ].
            assert: [ 'abc'  < 'abd'     ].
            assert: [ 'abc'  < 'abcd'    ].
            assert: [('abc'  < 'abc') not].
            assert: [('abd'  < 'abc') not].
            assert: [('abcd' < 'abc') not].


            assert: [  'baa' > 'abb'     ].
            assert: [   'aa' > 'a'       ].
            assert: [ ('abb' > 'baa') not].
            assert: [ ('aaa' > 'aaa') not].

            assert: [  ''  = ''      ].
            assert: [  'a' = 'a'     ].
            assert: [ (''  = ' ') not].

            assert: [ ''  <= ''  ].
            assert: [ ''  <= ' ' ].
            assert: [ 'a' <= 'a' ].
            assert: [ 'a' <= 'aa'].

            assert: [ ''   >= '' ].
            assert: [ ' '  >= '' ].
            assert: [ 'a'  >= 'a'].
            assert: [ 'ab' >= 'a'].

            assert: [  ''  isEmpty     ].
            assert: [ (' ' isEmpty) not].

            s: 'abcde' copySize: 3.

            assert: s  size Is: 3.
            assert: '' size Is: 0.

            assert: (s byteAt: 0) Is: 97.
            assert: (s byteAt: 1) Is: 98.
            assert: (s byteAt: 2) Is: 99.

            assert: 'abc', 'defgh' Is: 'abcdefgh'.

            assert: 'a' asByte Is: 97.

            assert: (('aaa', 'bbb') fillFrom: 'cde') Is: 'cdebbb'.
            assert: (('aaa', 'bbb') fillFrom: ''   ) Is: 'aaabbb'.

            assert: ('abc'    copySize: 6 FillingWith: 'x') Is: 'abcxxx'.
            assert: ('abcdef' copySize: 4 FillingWith: 'x') Is: 'abcd'.

            assert: 'abcdef' firstByte Is: 97.

            s: 'abcde' , 'f'.
            s: s do: [ |:c. :i| s at: i Put: (s byteAt: i) + 1].
            assert: s Is: 'bcdefg'.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         cloning = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'cloning' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'cloning' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests cloning.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'cloning' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (17)'
        
         a <- 17.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'cloning' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (nil)'
        
         b.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'cloning' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         c = 42.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'cloning' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'cloning' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests cloning parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'cloning' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'cloning' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            theClone: nil.
            theClone: copy.

            assert: [self !== theClone].
            assert: [b     == theClone b].
            assert: [c     == theClone c].
            assert: [nil   == theClone theClone].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'cloning' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (nil)'
        
         theClone.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         dataSlots = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests dataSlots.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         obj = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'obj' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests dataSlots obj.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'obj' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         one = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'obj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'obj' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests dataSlots obj parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'obj' -> 'parent' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         p_one = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'obj' -> 'parent' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         p_sum.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'obj' -> 'parent' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         p_two <- 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'obj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'obj' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         sum.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'obj' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (2)\x7fVisibility: private'
        
         two <- 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         one = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests dataSlots parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         p_one = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         p_sum.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         p_two <- 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         reset = ( |
            | 
            sum: nil.
            p_sum: nil.
            obj sum: nil.
            obj p_sum: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            testInline.
            testUplevelInline.
            testUplevelTwiceInline.

            testParentInline.
            testUplevelParentInline.
            testUplevelTwiceParentInline.

            testNonInline.
            testUplevelNonInline.
            testUplevelTwiceNonInline.

            testParentNonInline.
            testUplevelParentNonInline.
            testUplevelTwiceParentNonInline.

            reset. "clean up state"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fCategory: inlined data slot access\x7fCategory: on self\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testInline = ( |
            | 
            "Using branch bytecodes rather than 'assert' so we have
             explicit control over when we perform uplevel accesses
             to data slots for testing purposes.  -- jb 8/03"
            sum: nil.
            __BranchIfFalse: one = 1
                         To: 'fail'.
            __BranchIfFalse: two = 2
                         To: 'fail'.
            __BranchIfFalse: self == (sum: one + two)
                         To: 'fail'.
            __BranchIfFalse: sum = 3
                         To: 'fail'.
            __BranchTo: 'success'.

            __DefineLabel: 'fail'.
            error: 'failed'.

            __DefineLabel: 'success'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fCategory: non-inlined data slot access\x7fCategory: on another object\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testNonInline = ( |
            | 
            "Using branch bytecodes rather than 'assert' so we have
             explicit control over when we perform uplevel accesses
             to data slots for testing purposes.  -- jb 8/03"
            obj sum: nil.
            __BranchIfFalse: obj one = 1
                         To: 'fail'.
            __BranchIfFalse: obj two = 2
                         To: 'fail'.
            __BranchIfFalse: obj == (obj sum: obj one + obj two)
                         To: 'fail'.
            __BranchIfFalse: obj sum = 3
                         To: 'fail'.
            __BranchTo: 'success'.

            __DefineLabel: 'fail'.
            error: 'failed'.

            __DefineLabel: 'success'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fCategory: inlined data slot access\x7fCategory: on my parent\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testParentInline = ( |
            | 
            "Using branch bytecodes rather than 'assert' so we have
             explicit control over when we perform uplevel accesses
             to data slots for testing purposes.  -- jb 8/03"
            p_sum: nil.
            __BranchIfFalse: p_one = 1
                         To: 'fail'.
            __BranchIfFalse: p_two = 2
                         To: 'fail'.
            __BranchIfFalse: self == (p_sum: p_one + p_two)
                         To: 'fail'.
            __BranchIfFalse: p_sum = 3
                         To: 'fail'.
            __BranchTo: 'success'.

            __DefineLabel: 'fail'.
            error: 'failed'.

            __DefineLabel: 'success'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fCategory: non-inlined data slot access\x7fCategory: on another object\'s parent\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testParentNonInline = ( |
            | 
            "Using branch bytecodes rather than 'assert' so we have
             explicit control over when we perform uplevel accesses
             to data slots for testing purposes.  -- jb 8/03"
            obj p_sum: nil.
            __BranchIfFalse: obj p_one = 1
                         To: 'fail'.
            __BranchIfFalse: obj p_two = 2
                         To: 'fail'.
            __BranchIfFalse: obj == (obj p_sum: obj p_one + obj p_two)
                         To: 'fail'.
            __BranchIfFalse: obj p_sum = 3
                         To: 'fail'.
            __BranchTo: 'success'.

            __DefineLabel: 'fail'.
            error: 'failed'.

            __DefineLabel: 'success'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fCategory: inlined data slot access\x7fCategory: on self\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testUplevelInline = ( |
            | 
            "Using branch bytecodes rather than 'assert' so we have
             explicit control over when we perform uplevel accesses
             to data slots for testing purposes.  -- jb 8/03"
            sum: nil.
            [
            __BranchIfFalse: one = 1
                         To: 'fail'.
            __BranchIfFalse: two = 2
                         To: 'fail'.
            __BranchIfFalse: self == (sum: one + two)
                         To: 'fail'.
            __BranchIfFalse: sum = 3
                         To: 'fail'.
            __BranchTo: 'success'.

            __DefineLabel: 'fail'.
            error: 'failed'.

            __DefineLabel: 'success'.
            self
            ] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fCategory: non-inlined data slot access\x7fCategory: on another object\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testUplevelNonInline = ( |
            | 
            "Using branch bytecodes rather than 'assert' so we have
             explicit control over when we perform uplevel accesses
             to data slots for testing purposes.  -- jb 8/03"
            obj sum: nil.
            [
            __BranchIfFalse: obj one = 1
                         To: 'fail'.
            __BranchIfFalse: obj two = 2
                         To: 'fail'.
            __BranchIfFalse: obj == (obj sum: obj one + obj two)
                         To: 'fail'.
            __BranchIfFalse: obj sum = 3
                         To: 'fail'.
            __BranchTo: 'success'.

            __DefineLabel: 'fail'.
            error: 'failed'.

            __DefineLabel: 'success'.
            self
            ] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fCategory: inlined data slot access\x7fCategory: on my parent\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testUplevelParentInline = ( |
            | 
            "Using branch bytecodes rather than 'assert' so we have
             explicit control over when we perform uplevel accesses
             to data slots for testing purposes.  -- jb 8/03"
            p_sum: nil.
            [
            __BranchIfFalse: p_one = 1
                         To: 'fail'.
            __BranchIfFalse: p_two = 2
                         To: 'fail'.
            __BranchIfFalse: self == (p_sum: p_one + p_two)
                         To: 'fail'.
            __BranchIfFalse: p_sum = 3
                         To: 'fail'.
            __BranchTo: 'success'.

            __DefineLabel: 'fail'.
            error: 'failed'.

            __DefineLabel: 'success'.
            self
            ] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fCategory: non-inlined data slot access\x7fCategory: on another object\'s parent\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testUplevelParentNonInline = ( |
            | 
            "Using branch bytecodes rather than 'assert' so we have
             explicit control over when we perform uplevel accesses
             to data slots for testing purposes.  -- jb 8/03"
            obj p_sum: nil.
            [
            __BranchIfFalse: obj p_one = 1
                         To: 'fail'.
            __BranchIfFalse: obj p_two = 2
                         To: 'fail'.
            __BranchIfFalse: obj == (obj p_sum: obj p_one + obj p_two)
                         To: 'fail'.
            __BranchIfFalse: obj p_sum = 3
                         To: 'fail'.
            __BranchTo: 'success'.

            __DefineLabel: 'fail'.
            error: 'failed'.

            __DefineLabel: 'success'.
            self
            ] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fCategory: inlined data slot access\x7fCategory: on self\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testUplevelTwiceInline = ( |
            | 
            "Using branch bytecodes rather than 'assert' so we have
             explicit control over when we perform uplevel accesses
             to data slots for testing purposes.  -- jb 8/03"
            sum: nil.
            [[
            __BranchIfFalse: one = 1
                         To: 'fail'.
            __BranchIfFalse: two = 2
                         To: 'fail'.
            __BranchIfFalse: self == (sum: one + two)
                         To: 'fail'.
            __BranchIfFalse: sum = 3
                         To: 'fail'.
            __BranchTo: 'success'.

            __DefineLabel: 'fail'.
            error: 'failed'.

            __DefineLabel: 'success'.
            self
            ] value] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fCategory: non-inlined data slot access\x7fCategory: on another object\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testUplevelTwiceNonInline = ( |
            | 
            "Using branch bytecodes rather than 'assert' so we have
             explicit control over when we perform uplevel accesses
             to data slots for testing purposes.  -- jb 8/03"
            obj sum: nil.
            [[
            __BranchIfFalse: obj one = 1
                         To: 'fail'.
            __BranchIfFalse: obj two = 2
                         To: 'fail'.
            __BranchIfFalse: obj == (obj sum: obj one + obj two)
                         To: 'fail'.
            __BranchIfFalse: obj sum = 3
                         To: 'fail'.
            __BranchTo: 'success'.

            __DefineLabel: 'fail'.
            error: 'failed'.

            __DefineLabel: 'success'.
            self
            ] value] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fCategory: inlined data slot access\x7fCategory: on my parent\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testUplevelTwiceParentInline = ( |
            | 
            "Using branch bytecodes rather than 'assert' so we have
             explicit control over when we perform uplevel accesses
             to data slots for testing purposes.  -- jb 8/03"
            p_sum: nil.
            [[
            __BranchIfFalse: p_one = 1
                         To: 'fail'.
            __BranchIfFalse: p_two = 2
                         To: 'fail'.
            __BranchIfFalse: self == (p_sum: p_one + p_two)
                         To: 'fail'.
            __BranchIfFalse: p_sum = 3
                         To: 'fail'.
            __BranchTo: 'success'.

            __DefineLabel: 'fail'.
            error: 'failed'.

            __DefineLabel: 'success'.
            self
            ] value] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fCategory: non-inlined data slot access\x7fCategory: on another object\'s parent\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testUplevelTwiceParentNonInline = ( |
            | 
            "Using branch bytecodes rather than 'assert' so we have
             explicit control over when we perform uplevel accesses
             to data slots for testing purposes.  -- jb 8/03"
            obj p_sum: nil.
            [[
            __BranchIfFalse: obj p_one = 1
                         To: 'fail'.
            __BranchIfFalse: obj p_two = 2
                         To: 'fail'.
            __BranchIfFalse: obj == (obj p_sum: obj p_one + obj p_two)
                         To: 'fail'.
            __BranchIfFalse: obj p_sum = 3
                         To: 'fail'.
            __BranchTo: 'success'.

            __DefineLabel: 'fail'.
            error: 'failed'.

            __DefineLabel: 'success'.
            self
            ] value] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         sum.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'dataSlots' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (2)\x7fVisibility: private'
        
         two <- 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         identityHashes = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'identityHashes' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'identityHashes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests identityHashes.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'identityHashes' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'identityHashes' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests identityHashes parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'identityHashes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'identityHashes' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
             a.
             ah.
             b.
             bh.
             c.
             ch.
            | 

            "These tests will break if we change the way we assign identity hashes.
             But we probably won't do that very often. -- Adam & Alex, 6/04"

            a: ().
            b: ().
            c: ().
            ah: a _IdentityHash.
            bh: b _IdentityHash.
            ch: c _IdentityHash.
            assert: bh Is: ah + 1.
            assert: ch Is: bh + 1.
            assert: ah Is: a _IdentityHash. "didn't change"

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         int32s = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'int32s' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests int32s.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'int32s' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'int32s' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests int32s parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'int32s' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'int32s' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
             i1.
             i2.
             i3.
            | 
            i1: int32 copy at: 1 PutByte: 7.
            i2: int32 copy at: 3 PutByte: 5.

            "Call the _Int32:Add: primitive directly because the
             add:With: method does conversions; we want to test
             the behavior of the actual primitive. -- Adam, 6/05"
            assert: (int32 _Int32: i1 Add: i2) Is: 16r00070005.
            assert: (int32 _Int32: i1 Add:  6) Is: 16r00070006.
            assert: (int32 _Int32:  3 Add: i2) Is: 16r00000008.
            assert: (int32 _Int32:  9 Add:  4) Is: 16r0000000d.

            i3: int32 _Int32: i1 Add: maxSmallInt.
            "Too big for smi, so test each digit separately."
            assert: (i3 byteAt: 0) Is:  32.
            assert: (i3 byteAt: 1) Is:   6.
            assert: (i3 byteAt: 2) Is: 255.
            assert: (i3 byteAt: 3) Is: 255.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: interactive\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         interactiveRemoteDebugging = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests interactiveRemoteDebugging.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (3)'
        
         d <- 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests interactiveRemoteDebugging parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         blockActivations = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> 'blockActivations' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests interactiveRemoteDebugging parent blockActivations.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> 'blockActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         aardvark = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> 'blockActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (nil)'
        
         apples.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> 'blockActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         banana = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> 'blockActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         breakfast = ( |
            | 
            _Breakpoint: 'Look at value:, make sure breakfast is highlighted.'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> 'blockActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         cheese = ( |
            | 5).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> 'blockActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         chocolate: aBlock = ( |
            | 
            aBlock value: 7).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> 'blockActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> 'blockActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            testHighlighting.
            testLexicalParent: 0 Blocks: vectorOfSizeTen.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> 'blockActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         testHighlighting = ( |
             x <- 6.
            | 
            aardvark.
            banana.
            chocolate: [|:y|
              apples.
              breakfast.
              cheese.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> 'blockActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         testLexicalParent: z Blocks: bs = ( |
            | 
            bs _At: z Put: [|twentyPlusZ|
              twentyPlusZ:  20 _IntAdd: z.
              _Breakpoint: 'Make sure z shows as 3.'.
            ].

            (z _Eq: 7) ifTrue: [
              (bs _At: 3) value.
            ] False: [
              testLexicalParent: (z _IntAdd: 1) Blocks: bs.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> 'blockActivations' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (vector copySize: 10)'
        
         vectorOfSizeTen = vector copySize: 10.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         changeSomething = ( |
            | 
            _Breakpoint: 'get a mirror on self'.
            d: d + 1.
            _Breakpoint: 'did d change?').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         fewArgsAndLocalsInt: int Self: me Vector: vect = ( |
             const = 4.
             local <- 42.
            | 
            manyArgsR1: 1 R2: 2 R3: 3 R4: 4 R5: 5 R6: 6 R7: 7 M1: 8 M2: 9 M3: 10).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> () From: ( | {
         'Category: test data\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         fibonacci = [ | x =  ( bootstrap setObjectAnnotationOf: vector _Clone From: ( |
                     {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests interactiveRemoteDebugging parent fibonacci.
'.
                    | ) ) _Clone: 8 Filler: 0| 
             x _At: 0  Put: ().
             x _At: 1  Put: ().
             x _At: 2  Put: ().
             x _At: 3  Put: ().
             x _At: 4  Put: ().
             x _At: 5  Put: ().
             x _At: 6  Put: ().
             x _At: 7  Put: ().
             x] value.
        } | ) 

 ((bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent') \/-> 'fibonacci') -> () _At: 0 Put: (
     1)

 ((bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent') \/-> 'fibonacci') -> () _At: 1 Put: (
     1)

 ((bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent') \/-> 'fibonacci') -> () _At: 2 Put: (
     2)

 ((bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent') \/-> 'fibonacci') -> () _At: 3 Put: (
     3)

 ((bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent') \/-> 'fibonacci') -> () _At: 4 Put: (
     5)

 ((bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent') \/-> 'fibonacci') -> () _At: 5 Put: (
     8)

 ((bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent') \/-> 'fibonacci') -> () _At: 6 Put: (
     13)

 ((bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent') \/-> 'fibonacci') -> () _At: 7 Put: (
     21)

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         manyArgsR1: r1 R2: r2 R3: r3 R4: r4 R5: r5 R6: r6 R7: r7 M1: m1 M2: m2 M3: m3 = ( |
            | 
            "Lots of arguments: Rx are in registers, Mx are in memory"
            top).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         noArgsOrLocals = ( |
            | 
            fewArgsAndLocalsInt: 1 Self: self Vector: fibonacci).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> () From: ( | {
         'Comment: Invokes methods with arguments of various types so
that the user can interactively verify that the debugger
shows sensible information.  -- jb 8/03\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            changeSomething.
            noArgsOrLocals.
            blockActivations run.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'interactiveRemoteDebugging' -> 'parent' -> () From: ( | {
         'Category: test cases\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         top = ( |
            | 
            "Try inspecting the values of slots in my parent frames using
             the source level debugger."
            _Breakpoint: 'use source level').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         lookup = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests lookup.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests lookup parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parentA* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> 'parentA' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests lookup parent parentA.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         slotInBoth = 10.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         slotInJustA = 55.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (28)\x7fVisibility: private'
        
         slotToFindThroughResend <- 28.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parentB* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> 'parentB' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests lookup parent parentB.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         slotInBoth = 20.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         slotInJustB = 199.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            kleinAndYoda lookup findObjectOnReceiver: self Selector: 'slotToFind' Holder: self IsUndirectedResend: false Delegatee: nil IsSelfImplicit: false.
            assert:  kleinAndYoda lookup resultType Is: kleinAndYoda lookup foundOne.
            assert: (kleinAndYoda lookup slotReference1 holderMap nameAt: kleinAndYoda lookup slotReference1 index) Is: 'slotToFind'.
            assert: [kleinAndYoda lookup succeeded].
            assert:  kleinAndYoda lookup contentsOfFoundSlot Is: 42.

            kleinAndYoda lookup findObjectOnReceiver: self Selector: 'noSlotWithThisName' Holder: self IsUndirectedResend: false Delegatee: nil IsSelfImplicit: false.
            assert:  kleinAndYoda lookup resultType Is: kleinAndYoda lookup foundNone.
            assert: [kleinAndYoda lookup succeeded not].

            kleinAndYoda lookup findObjectOnReceiver: self Selector: 'slotInJustA' Holder: self IsUndirectedResend: false Delegatee: nil IsSelfImplicit: false.
            assert:  kleinAndYoda lookup resultType Is: kleinAndYoda lookup foundOne.
            assert: (kleinAndYoda lookup slotReference1 holderMap nameAt: kleinAndYoda lookup slotReference1 index) Is: 'slotInJustA'.
            assert: [kleinAndYoda lookup succeeded].
            assert:  kleinAndYoda lookup contentsOfFoundSlot Is: 55.

            kleinAndYoda lookup findObjectOnReceiver: self Selector: 'slotInJustB' Holder: self IsUndirectedResend: false Delegatee: nil IsSelfImplicit: false.
            assert:  kleinAndYoda lookup resultType Is: kleinAndYoda lookup foundOne.
            assert: (kleinAndYoda lookup slotReference1 holderMap nameAt: kleinAndYoda lookup slotReference1 index) Is: 'slotInJustB'.
            assert: [kleinAndYoda lookup succeeded].
            assert:  kleinAndYoda lookup contentsOfFoundSlot Is: 199.

            kleinAndYoda lookup findObjectOnReceiver: self Selector: 'slotToFindThroughResend' Holder: parent IsUndirectedResend: true Delegatee: nil IsSelfImplicit: true.
            assert:  kleinAndYoda lookup resultType Is: kleinAndYoda lookup foundOne.
            assert: [kleinAndYoda lookup slotReference1 holder _Eq: parentA].
            assert: (kleinAndYoda lookup slotReference1 holderMap nameAt: kleinAndYoda lookup slotReference1 index) Is: 'slotToFindThroughResend'.
            assert: [kleinAndYoda lookup succeeded].
            assert:  kleinAndYoda lookup contentsOfFoundSlot Is: 28.

            kleinAndYoda lookup findObjectOnReceiver: self Selector: 'slotInBoth' Holder: self IsUndirectedResend: false Delegatee: nil IsSelfImplicit: false.
            assert:  kleinAndYoda lookup resultType Is: kleinAndYoda lookup foundTwo.
            assert: [kleinAndYoda lookup slotReference1 holder _Eq: parentA].
            assert: (kleinAndYoda lookup slotReference1 holderMap nameAt: kleinAndYoda lookup slotReference1 index) Is: 'slotInBoth'.
            assert: [kleinAndYoda lookup slotReference2 holder _Eq: parentB].
            assert: (kleinAndYoda lookup slotReference2 holderMap nameAt: kleinAndYoda lookup slotReference1 index) Is: 'slotInBoth'.
            assert: [kleinAndYoda lookup succeeded not].

            kleinAndYoda lookup findObjectOnReceiver: self Selector: 'slotInBoth' Holder: parent IsUndirectedResend: false Delegatee: 'parentA' IsSelfImplicit: true.
            assert:  kleinAndYoda lookup resultType Is: kleinAndYoda lookup foundOne.
            assert: [kleinAndYoda lookup slotReference1 holder _Eq: parentA].
            assert: (kleinAndYoda lookup slotReference1 holderMap nameAt: kleinAndYoda lookup slotReference1 index) Is: 'slotInBoth'.
            assert: [kleinAndYoda lookup succeeded].
            assert:  kleinAndYoda lookup contentsOfFoundSlot Is: 10.

            kleinAndYoda lookup findObjectOnReceiver: self Selector: 'slotInBoth' Holder: parent IsUndirectedResend: false Delegatee: 'parentB' IsSelfImplicit: true.
            assert:  kleinAndYoda lookup resultType Is: kleinAndYoda lookup foundOne.
            assert: [kleinAndYoda lookup slotReference1 holder _Eq: parentB].
            assert: (kleinAndYoda lookup slotReference1 holderMap nameAt: kleinAndYoda lookup slotReference1 index) Is: 'slotInBoth'.
            assert: [kleinAndYoda lookup succeeded].
            assert:  kleinAndYoda lookup contentsOfFoundSlot Is: 20.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         slotToFind = 42.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'lookup' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         slotToFindThroughResend = -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         maps = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'maps' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'maps' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests maps.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'maps' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'maps' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests maps parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'maps' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'maps' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            testThatDifferentKindsOfObjectsHaveDifferentMapTypes.
            testMapsOfObjectLiterals.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'maps' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testMapsOfObjectLiterals = ( |
            | 
            "This assertion used to fail if you did an incremental
             update of this method."
            assert: [(| apple |) _Map _Map _NMethodCache size > 0].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'maps' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testThatDifferentKindsOfObjectsHaveDifferentMapTypes = ( |
            | 
            assert: [     _Map isProgrammableSlots].
            assert: [4567 _Map isInteger          ].
            assert: [3.14 _Map isFloat            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         messageSending = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'messageSending' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'messageSending' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests messageSending.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'messageSending' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'messageSending' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests messageSending parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'messageSending' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         calledOnlyOnce = ( |
            | 
            3 value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'messageSending' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'messageSending' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
             a <- 2000.
             b <- 4000.
             x <- 250.
             y <- 500.
             z <- 1000.
            | 
            x: calledOnlyOnce.
            a: valueOfWithBreakpoint: 99.
            b: valueOfWithBreakpoint: ['banana'].
            y: valueOf: 66.
            z: valueOf: ['pickle'].
            "Hopefully we'll get here. :)").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'messageSending' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         valueOf: x = ( |
            | 
            x value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'messageSending' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         valueOfWithBreakpoint: x = ( |
            | 
            "We used to have a bug where, if you uncommented this breakpoint,
             the bug goes away. -- Adam & Alex, 5/04"
            "_Breakpoint: 'valueOfWithBreakpoint: x'."
            x value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrors = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests mirrors.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests mirrors parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            testRegularMirror.
            testSmiMirror.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testObj = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> 'testObj' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests mirrors parent testObj.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> 'testObj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         a = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> 'testObj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (4)'
        
         b <- 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> 'testObj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         c = ( |
            | 5).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> 'testObj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> 'testObj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parentA* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> 'testObj' -> 'parentA' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests mirrors parent testObj parentA.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> 'testObj' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (3)'
        
         ambiguous <- 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> 'testObj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parentB* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> 'testObj' -> 'parentB' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests mirrors parent testObj parentB.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> 'testObj' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (4)'
        
         ambiguous <- 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> 'testObj' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (nil)'
        
         inParentB.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testRegularMirror = ( |
             contents.
             parentSlotsOfTestObj.
             result.
             sf.
             slot.
             slots.
             testObjMir.
            | 

            testObjMir: testObj asMirror.

            assert: testObjMir names Is: ('a' & 'b' & 'b:' & 'c' & 'parent' & 'parentA' & 'parentB') asVector.

            slots: (testObjMir lookupKey: 'c').
            slot: slots first.
            assert: [slot isMethod].
            assert: slot storedName Is: 'c'.
            assert: slot mirror Is: reflect: testObj.

            contents: testObjMir primitiveContentsAt: 'a' IfFail: raiseError.
            assert: contents Is: 3 asMirror.

            parentSlotsOfTestObj: list copyRemoveAll.
            testObjMir parentsDo: [|:s| parentSlotsOfTestObj add: s].
            assert: parentSlotsOfTestObj size Is: 3.
            assert: [parentSlotsOfTestObj anySatisfy: [|:s| s name = 'parent' ]].
            assert: [parentSlotsOfTestObj anySatisfy: [|:s| s name = 'parentA']].
            assert: [parentSlotsOfTestObj anySatisfy: [|:s| s name = 'parentB']].
            assert: [((testObjMir at: 'parentA') contents at: 'ambiguous') contents reflectee = 3].
            assert: [((testObjMir at: 'parentB') contents at: 'ambiguous') contents reflectee = 4].

            sf: testObjMir slotFinder copyForMirror: testObjMir Selector: 'ambiguous'.
            result: set copyRemoveAll.
            sf findSlotsIn: (testObjMir at: 'parentA') contents AndAddResultsTo: result.
            assert: result size Is: 1.

            sf: testObjMir slotFinder copyForMirror: testObjMir Selector: 'ambiguous'.
            result: set copyRemoveAll.
            sf findSlotsIn: (testObjMir at: 'parentB') contents AndAddResultsTo: result.
            assert: result size Is: 1.

            sf: testObjMir slotFinder copyForMirror: testObjMir Selector: 'ambiguous'.
            result: set copyRemoveAll.
            sf findSlotsIn: (testObjMir at: 'parent' ) contents AndAddResultsTo: result.
            sf findSlotsIn: (testObjMir at: 'parentA') contents AndAddResultsTo: result.
            sf findSlotsIn: (testObjMir at: 'parentB') contents AndAddResultsTo: result.
            assert: result size Is: 2.

            sf: testObjMir slotFinder copyForMirror: testObjMir Selector: 'ambiguous'.
            result: set copyRemoveAll.
            testObjMir parentsDo: [|:parentSlot. oldSize|
              sf findSlotsIn: parentSlot contents AndAddResultsTo: result.
            ].
            assert: result size Is: 2.

            sf: testObjMir slotFinder copyForMirror: testObjMir Selector: 'ambiguous'.
            result: set copyRemoveAll.
            sf findSlotsInParentsOf: testObjMir AndAddResultsTo: result.
            assert: result size Is: 2.

            slots: (testObjMir lookupKey: 'ambiguous') asList.
            assert: slots size Is: 2.
            slots do: [|:s|
              assert: [s isAssignable].
              assert: s storedName Is: 'ambiguous'.
            ].
            assert: (slots copyMappedBy: [|:s| s holder]) asSet
                Is: (  (reflect: testObj parentA)
                     & (reflect: testObj parentB))        asSet.

            assert: vector asMirror statePrintString Is: 'on vector'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'mirrors' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testSmiMirror = ( |
             names.
             zeroMir.
            | 
            zeroMir: 0 asMirror.
            names: zeroMir names.
            assert: names size Is: 1.
            assert: names first Is: 'parent'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         onNonLocalReturn = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'onNonLocalReturn' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'onNonLocalReturn' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests onNonLocalReturn.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'onNonLocalReturn' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         b1Ran <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'onNonLocalReturn' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         b2Ran <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'onNonLocalReturn' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         mRan <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'onNonLocalReturn' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'onNonLocalReturn' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests onNonLocalReturn parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'onNonLocalReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'onNonLocalReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            testAndCheckNLR1: false  NLR2: false.
            testAndCheckNLR1: true   NLR2: false.
            testAndCheckNLR1: false  NLR2: true.
            testAndCheckNLR1: true   NLR2: true.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'onNonLocalReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testAndCheckNLR1: nlr1 NLR2: nlr2 = ( |
             r.
            | 
            r: testNLR1: nlr1 NLR2: nlr2.
            assert: [b1Ran].
            nlr1 ifFalse: [
              assert: [b2Ran not].
              assert: [mRan].
              assert: r Is: 2.
            ] True: [
              assert: [b2Ran].
              assert: [mRan not].
              assert: r Is:  nlr2 ifTrue: 2 False: 3.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'onNonLocalReturn' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testNLR1: nlr1 NLR2: nlr2 = ( |
             r.
            | 
            b1Ran: false. b2Ran: false. mRan: false.
            r: [
              b1Ran: true.
              nlr1 ifTrue: [^ 1].
              2
            ]
             _OnNonLocalReturn: [|:retVal|
              assert: retVal Is: 1.
              b2Ran: true.
              nlr2 ifTrue: [^ 2].
              3
            ]
            IfFail: [fail].
            mRan: true.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         performing = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests performing.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests performing parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         add: x To: y = ( |
            | 
            x _IntAdd: y).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         one = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         perform: s = ( |
            | 
            _Perform: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         resendObj = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests performing parent resendObj.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         dynamicUndirectedResend: sel = ( |
            | 
            _PerformResend: sel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests performing parent resendObj parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (55)'
        
         a <- 55.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         dynamicUndirectedResendInParent: sel = ( |
            | 
            _PerformResend: sel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parent' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests performing parent resendObj parent parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         w = ( |
            | 7).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         z = 6.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         staticUndirectedResendInParent = ( |
            | 
            _PerformResend: 'z').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         w = 8.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         x = ( |
            | 
            "Just to make sure the resent methods get compiled."
            resend.z + resend.w).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         z = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parentB* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parentB' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests performing parent resendObj parentB.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         a = 44.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         staticUndirectedResend = ( |
            | 
            _PerformResend: 'z').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (9)'
        
         w <- 9.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         y = ( |
            | 
            "Just to make sure the resent methods get compiled."
            resend.z + resend.w + parent.a + parentB.a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         z = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
             parentB = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parentB' -> ().
            | 

            assert: (_Perform: 'one')              Is: 1.
            assert: (_Perform: 'two')              Is: 2.
            assert: (          'one' sendTo: self) Is: 1.
            assert: (          'two' sendTo: self) Is: 2.

            assert: (_Perform: 'add:To:'              With: 3 With: 4 ) Is: 7.
            assert: (          'add:To:' sendTo: self With: 3 With: 4 ) Is: 7.
            assert: (          'add:To:' sendTo: self With: 5 With: -2) Is: 3.

            [
              [todo delegatedPerform]. "Not implemented yet."
              assert: (resendObj _Perform: 'a' DelegatingTo:           parentB) Is: 44.
              assert: (resendObj _Perform: 'a' DelegatingTo: resendObj parentB) Is: 44.
              assert: ('a' sendTo: resendObj   DelegatingTo: resendObj parentB) Is: 44.
              assert: ('a' sendTo: resendObj   DelegatingTo: resendObj parent ) Is: 55.
            ].

            assert:  resendObj  staticUndirectedResend               Is: 5.
            assert: (resendObj dynamicUndirectedResend:         'z') Is: 5.
            assert: (resendObj dynamicUndirectedResend:         'w') Is: 8.
            assert:  resendObj  staticUndirectedResendInParent       Is: 6.
            assert: (resendObj dynamicUndirectedResendInParent: 'z') Is: 6.
            assert: (resendObj dynamicUndirectedResendInParent: 'w') Is: 7.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testSimplePerform = ( |
            | 
            _Perform: 'one').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         two = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         primitives = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests primitives.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests primitives parent.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         argAtTests = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests primitives parent argAtTests.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         add: foo To: bar = ( |
            | 
            _ArgAt: 0 IfFail: -1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         argAtFail1 = ( |
            | 
            _ArgAt: 0 IfFail: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         argAtFail2: arg = ( |
            | 
            _ArgAt: 1 IfFail: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         argAtFail3 = ( |
            | 
            _ArgAt: -1 IfFail: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests primitives parent argAtTests parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         returnFirstArg: arg = ( |
            | 
            _ArgAt: 0 IfFail: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         returnFirstArg: arg Arg: arg2 = ( |
            | 
            _ArgAt: 0 IfFail: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         returnFirstArg: arg Arg: arg2 Arg: arg3 = ( |
            | 
            _ArgAt: 2 IfFail: [^ false].
            _ArgAt: 1 IfFail: [^ false].
            _ArgAt: 0 IfFail: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         returnLastArg: arg Arg: arg2 Arg: arg3 Arg: arg4 Arg: arg5 = ( |
            | 
            _ArgAt: 2 IfFail: [^ false].
            _ArgAt: 1 IfFail: [^ false].
            _ArgAt: 0 IfFail: [^ false].
            _ArgAt: 4 IfFail: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         returnSecondArg: arg Arg: arg2 Arg: arg3 = ( |
            | 
            _ArgAt: 2 IfFail: [^ false].
            _ArgAt: 0 IfFail: [^ false].
            _ArgAt: 1 IfFail: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'argAtTests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            assert: [(add: 1 To: 2) = 3].

            assert: [ returnFirstArg:   true                      ].
            assert: [ returnFirstArg:   true Arg: false           ].
            assert: [ returnFirstArg:   true Arg: false Arg: false].
            assert: [returnSecondArg:  false Arg:  true Arg: false].
            assert: [  returnLastArg:  false Arg: false Arg: false 
                                             Arg: false Arg:  true].


            assert: [argAtFail1].
            assert: [argAtFail2].
            assert: [argAtFail2]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         restartPrim = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests primitives parent restartPrim.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests primitives parent restartPrim parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrInsideALoop = ( |
             sum <- 0.
            | 
            10 do: [|:i| 7 < i ifTrue: [^ sum].
                         sum: sum + i].
            sum).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrInsideALoop2 = ( |
             r <- 999.
             sum <- 0.
            | 
            r: 10 do: [|:i| 7 < i ifTrue: [^ sum].
                            sum: sum + i].
            sum: sum + 100.
            sum + 250).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrInsideALoop3 = ( |
            | 
            100 do: [|:i| ^ 5].
            6).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrInsideALoop4 = ( |
             i <- 0.
             r <- 999.
             sum <- 0.
            | 
            r: [7 < i ifTrue: [^ sum].
                sum: sum + i.
                i: i succ] loop.
            sum: sum + 100.
            sum + 250).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrInsideALoop5 = ( |
            | 
            0 to: 100 ByPositive: 1 Do: [|:i| ^ 5].
            6).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            testLoop.
            testNLRInsideALoop.
            assert: nlrInsideALoop5 Is: 5.
            assert: nlrInsideALoop4 Is: 28.
            assert: nlrInsideALoop3 Is: 5.
            testNLRInsideALoop2.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testLoop = ( |
             sum <- 0.
            | 
            10 do: [|:i| sum: sum + i].
            assert: sum Is: 45).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testNLRInsideALoop = ( |
            | 
            assert: nlrInsideALoop Is: 28).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testNLRInsideALoop2 = ( |
            | 
            assert: nlrInsideALoop2 Is: 28).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         returnErrorStringOnFailure = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'returnErrorStringOnFailure' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests primitives parent returnErrorStringOnFailure.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'returnErrorStringOnFailure' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'returnErrorStringOnFailure' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         primitiveFailedError: e Name: n = ( |
            | 
            e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         returnPrimitiveNameOnFailure = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'returnPrimitiveNameOnFailure' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests primitives parent returnPrimitiveNameOnFailure.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'returnPrimitiveNameOnFailure' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> 'returnPrimitiveNameOnFailure' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         primitiveFailedError: e Name: n = ( |
            | 
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            restartPrim run.
            [todo cleanup tests]. "The argAT: primitive doesn't seem to work right - do we still need it? - Ausch 8/05"
            "argAtTests  run."
            testFailure.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'primitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testFailure = ( |
            | 
            assert:  returnErrorStringOnFailure   _Blah          Is: 'primitiveNotDefinedError'.
            assert: (returnErrorStringOnFailure   _At: 3)        Is: 'badTypeError'.
            assert:  returnErrorStringOnFailure   _IntComplement Is: 'badTypeError'.

            assert:  returnPrimitiveNameOnFailure _Blah          Is: '_Blah'.
            assert: (returnPrimitiveNameOnFailure _At: 3)        Is: '_At:'.
            assert:  returnPrimitiveNameOnFailure _IntComplement Is: '_IntComplement'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         printNMethodUsageNumbers = ( |
             i <- 0.
             nonZero <- 0.
            | 
            theVM universe allNMethods do: [|:nm|
              nm invocationCount = 0 ifFalse: [nonZero: nonZero succ].
              i: i succ.
            ].
            _Breakpoint: 'how many?'.
            theVM universe allNMethods do: [|:nm|
              nm invocationCount asString _StringPrint.
              ' ' _StringPrint.
              nm lookupKey selector _StringPrint.
              ' ' _StringPrint.
              nm lookupKey lookupType asString _StringPrint.
              '\n' _StringPrint.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         resending = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         objectThatInheritsTheSameMethodThatDoesAResend1 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectThatInheritsTheSameMethodThatDoesAResend1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectThatInheritsTheSameMethodThatDoesAResend1.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         objectThatInheritsTheSameMethodThatDoesAResendParent = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectThatInheritsTheSameMethodThatDoesAResendParent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectThatInheritsTheSameMethodThatDoesAResendParent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectThatInheritsTheSameMethodThatDoesAResend1' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectThatInheritsTheSameMethodThatDoesAResendParent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         objectThatInheritsTheSameMethodThatDoesAResend2 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectThatInheritsTheSameMethodThatDoesAResend2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectThatInheritsTheSameMethodThatDoesAResend2.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectThatInheritsTheSameMethodThatDoesAResend2' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectThatInheritsTheSameMethodThatDoesAResendParent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectThatInheritsTheSameMethodThatDoesAResendParent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         aardvark = ( |
            | [resend.aardvark] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectThatInheritsTheSameMethodThatDoesAResendParent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectThatInheritsTheSameMethodThatDoesAResendParent' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectThatInheritsTheSameMethodThatDoesAResendParent parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectThatInheritsTheSameMethodThatDoesAResendParent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         aardvark = ( |
            | 
            4).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectThatInheritsTheSameMethodThatDoesAResendParent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         objectWithMultipleParents = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectWithMultipleParents.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         a = ( |
            | resend.b + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (-1)'
        
         b <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         c = ( |
            | resend.c + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         d = ( |
            | parentA.d + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         e = ( |
            | parentB.e + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         f = ( |
            | parentA.f + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         g = ( |
            | 
            parentA.g + parentB.g + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         h = ( |
            | 
            true ifTrue: [[parentB.h + 1] value] False: [-40]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         i = ( |
            | notAParent.i + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         j = ( |
            | 
            parentA.j + parentB.j + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         notAParent = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'notAParent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectWithMultipleParents notAParent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'notAParent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         i = 60.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parentA* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentA' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectWithMultipleParents parentA.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         b = ( |
            | 
            0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         d = 10.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         e = -20.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         f = ( |
            | 
            parentA.f + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         h = -50.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         j = ( |
            | resend.j).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parentA* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentA' -> 'parentA' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectWithMultipleParents parentA parentA.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentA' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         f = ( |
            | 30).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentA' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         j = ( |
            | 72).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         sharedGrandparent = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'sharedGrandparent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectWithMultipleParents sharedGrandparent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         sharedParent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'sharedGrandparent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parentB* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentB' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectWithMultipleParents parentB.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         e = ( |
            | 20).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         f = ( |
            | -30).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (50)'
        
         h <- 50.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         j = ( |
            | resend.j).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parentB* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentB' -> 'parentB' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectWithMultipleParents parentB parentB.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentB' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         j = ( |
            | 73).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         sharedParent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'sharedGrandparent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'sharedGrandparent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         c = 10.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'sharedGrandparent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         d = ( |
            | -10).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'sharedGrandparent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         f = ( |
            | 
            -35).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'sharedGrandparent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         g = ( |
            | 40).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithMultipleParents' -> 'sharedGrandparent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         objectWithSingleParent = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectWithSingleParent.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         a = ( |
            | resend.a + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         b = ( |
            | resend.b + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         d = ( |
            | 
            resend.e + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (-30)'
        
         e <- -30.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         g = -40.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         h = ( |
            | resend.h + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         i = ( |
            | 
            true ifTrue: [[[[resend.i + 1] value] value] value] False: [-60]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectWithSingleParent parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         a = ( |
            | 
            0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         c = ( |
            | resend.c + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         d = -30.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         e = 30.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         f = ( |
            | resend.g + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         g = -45.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         h = ( |
            | resend.h + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         i = 60.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending objectWithSingleParent parent parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         a = ( |
            | 
            -1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         b = 10.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (20)'
        
         c <- 20.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         g = ( |
            | 40).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         h = ( |
            | 50).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'objectWithSingleParent' -> 'parent' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests resending parent.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            testSimpleResend.
            testResendToGrandparent.
            testResendingMethodInParent.
            testResendingDifferentSelector.
            testResendingDifferentSelectorWithMethodInParent.
            testResendChain.
            testResendInsideBlocks.
            testUndirectedResendWithMultipleParents.
            testUndirectedResendToGrandparentThatCanBeReachedThroughTwoPaths.
            testUndirectedResendToTwoDifferentGrandparents.

            testDirectedResend1.
            testDirectedResend2.
            testDirectedResendChain.
            testDirectedResendToGrandparent.
            testDirectedResendInsideBlock.
            testDirectedResendToANonParent.

            testTwoObjectsInheritingTheSameMethodThatDoesAResend.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testDirectedResend1 = ( |
            | 
            assert: [objectWithMultipleParents d = 11]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testDirectedResend2 = ( |
            | 
            assert: [objectWithMultipleParents e = 21]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testDirectedResendChain = ( |
            | 
            assert: [objectWithMultipleParents f = 32]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testDirectedResendInsideBlock = ( |
            | 
            assert: [objectWithMultipleParents h = 51]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testDirectedResendToANonParent = ( |
            | 
            assert: [objectWithMultipleParents i = 61]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testDirectedResendToGrandparent = ( |
            | 
            assert: [objectWithMultipleParents g = 81]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testResendChain = ( |
            | 
            assert: [objectWithSingleParent h = 52]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testResendInsideBlocks = ( |
            | 
            assert: [objectWithSingleParent i = 61]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testResendToGrandparent = ( |
            | 
            assert: [objectWithSingleParent b = 11]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testResendingDifferentSelector = ( |
            | 
            assert: [objectWithSingleParent d = 31]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testResendingDifferentSelectorWithMethodInParent = ( |
            | 
            assert: [objectWithSingleParent f = 41]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testResendingMethodInParent = ( |
            | 
            assert: [objectWithSingleParent c = 21]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testSimpleResend = ( |
            | 
            assert: [objectWithSingleParent a = 1]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testTwoObjectsInheritingTheSameMethodThatDoesAResend = ( |
            | 
            assert: objectThatInheritsTheSameMethodThatDoesAResend1 aardvark Is: 4.
            assert: objectThatInheritsTheSameMethodThatDoesAResend2 aardvark Is: 4.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testUndirectedResendToGrandparentThatCanBeReachedThroughTwoPaths = ( |
            | 
            assert: [objectWithMultipleParents c = 11]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testUndirectedResendToTwoDifferentGrandparents = ( |
            | 
            assert: [objectWithMultipleParents j = 146]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'resending' -> () From: ( | {
         'Category: resend tests\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testUndirectedResendWithMultipleParents = ( |
            | 
            assert: [objectWithMultipleParents a = 1]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         restartPrim = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests restartPrim.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests restartPrim parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrInsideALoop = ( |
             sum <- 0.
            | 
            10 do: [|:i| 7 < i ifTrue: [^ sum].
                         sum: sum + i].
            sum).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrInsideALoop2 = ( |
             r <- 999.
             sum <- 0.
            | 
            r: 10 do: [|:i| 7 < i ifTrue: [^ sum].
                            sum: sum + i].
            sum: sum + 100.
            sum + 250).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrInsideALoop3 = ( |
            | 
            100 do: [|:i| ^ 5].
            6).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrInsideALoop4 = ( |
             i <- 0.
             r <- 999.
             sum <- 0.
            | 
            r: [7 < i ifTrue: [^ sum].
                sum: sum + i.
                i: i succ] loop.
            sum: sum + 100.
            sum + 250).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         nlrInsideALoop5 = ( |
            | 
            0 to: 100 ByPositive: 1 Do: [|:i| ^ 5].
            6).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            testLoop.
            testNLRInsideALoop.
            assert: nlrInsideALoop5 Is: 5.
            assert: nlrInsideALoop4 Is: 28.
            assert: nlrInsideALoop3 Is: 5.
            testNLRInsideALoop2.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testLoop = ( |
             sum <- 0.
            | 
            10 do: [|:i| sum: sum + i].
            assert: sum Is: 45).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testNLRInsideALoop = ( |
            | 
            assert: nlrInsideALoop Is: 28).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'restartPrim' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testNLRInsideALoop2 = ( |
            | 
            assert: nlrInsideALoop2 Is: 28).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         runAllTestsForVM: aVM = ( |
            | 
            "Not sure why this is necessary - why isn't it already stopped?
             Anyway, the scheduler is only included in the exportPolicy because
             the string print method checks to see whether it's running."
            [aaa]. scheduler stop.

            runAutomatedTestsForVM: aVM.
            runInteractiveTests.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         runAutomatedTestsForVM: aVM = ( |
            | 
            [todo cleanup testing kleinSpecific].
            "Take all the tests that aren't Klein-specific and put them in
             other places (similar to traits universalSetOrDictionary unitTests).
             Then make them part of both the Self test suite (see runSelfSuite)
             and the Klein test suite. -- Adam, 5/05"

            theVMPrimitive runForVM: aVM. [todo cleanup testing kleinSpecific].
            messageSending run.
            smallIntegers run.
            cloning run.
            blocks run.
            dataSlots run.
            resending run.
            systemCalls run.              [todo cleanup testing kleinSpecific].
            restartPrim run.
            branches run.
            onNonLocalReturn run.
            byteVectors run.
            vectors run.
            int32s run.
            maps run.                     [todo cleanup testing kleinSpecific].
            tags run.                     [todo cleanup testing kleinSpecific].
            identityHashes run.
            performing run.
            mirrors run.
            allocation run.
            lookup run.
            stringPrinting run.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         runInteractiveTests = ( |
            | 
            interactiveRemoteDebugging run).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         smallIntegers = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'smallIntegers' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'smallIntegers' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests smallIntegers.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'smallIntegers' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'smallIntegers' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests smallIntegers parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'smallIntegers' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'smallIntegers' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            0 unitTests run.
            testCheckingReceiverMap.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'smallIntegers' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldReturnOne = ( |
            | 
            true ifTrue: 0.
            true ifTrue: [^ 1].
            fail).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'smallIntegers' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testCheckingReceiverMap = ( |
            | 
            assert: shouldReturnOne Is: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         stringPrinting = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'stringPrinting' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'stringPrinting' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests stringPrinting.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'stringPrinting' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'stringPrinting' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests stringPrinting parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'stringPrinting' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'stringPrinting' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            'The string print method works.' printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         systemCalls = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'systemCalls' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'systemCalls' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests systemCalls.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'systemCalls' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'systemCalls' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests systemCalls parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'systemCalls' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'systemCalls' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            assert: 'hello world\n' _StringPrint Is: 12.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         tags = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'tags' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'tags' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests tags.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'tags' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'tags' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests tags parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'tags' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'tags' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            assert:      0 _TagPartOfObjectReference  Is: 0.
            assert:      1 _TagPartOfObjectReference  Is: 0.
            assert:     -1 _TagPartOfObjectReference  Is: 0.
            assert: 'blah' _TagPartOfObjectReference  Is: 1.
            assert:    1.0 _TagPartOfObjectReference  Is: 2.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         theVMPrimitive = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'theVMPrimitive' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'theVMPrimitive' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests theVMPrimitive.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'theVMPrimitive' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'theVMPrimitive' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests theVMPrimitive parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'theVMPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'theVMPrimitive' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         runForVM: aVM = ( |
            | assert: [ _TheVM == aVM ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         vectors = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'vectors' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'vectors' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests vectors.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'vectors' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'vectors' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines midiVM parent tests vectors parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'vectors' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'vectors' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
             v.
            | 
            v: vector copySize: 4.
            assert: v size Is: 4.
            assert: (v at: 1) Is: nil.
            v at: 3 Put: 'banana'.
            assert: (v at: 3) Is: 'banana'.
            v at: 1 Put: 456.
            assert: (v at: 1) Is: 456.

            assertFail: [|:fb| v _At: 4        IfFail: fb].
            assertFail: [|:fb| v _At: 5 Put: 9 IfFail: fb].
            assertFail: [|:fb| v _At: -1       IfFail: fb].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         selfVM = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda virtualMachines abstractVM copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM.

CopyDowns:
globals kleinAndYoda virtualMachines abstractVM. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fComment: I\'m sure there\'s a more clever way to get
this number. For now it won\'t hurt much
to just keep upping it by hand.
-- Adam, 2/05\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         expectedNumberOfObjects = 275000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         exportPolicy = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'exportPolicy' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent exportPolicy.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         modulesToMap = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent exportPolicy modulesToMap.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAllNamesOfIncludedModules <- bootstrap stub -> 'globals' -> 'nil' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfExcludedModules = bootstrap setObjectAnnotationOf: ( (('vmKitMapImporting')
	& ('vmKitRmtMemIntrface')
	& ('vmKitVarHdrs')
	& ('vmKitVarHdrsUniv')
	& ('vmKitVarHdrsVerify')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent exportPolicy modulesToMap namesOfExcludedModules.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfIncludedModules = bootstrap setObjectAnnotationOf: ( (('')
	& ('annotation')
	& ('asmGeneratedSlots')
	& ('block')
	& ('boolean')
	& ('bytecodeFormat')
	& ('caseStatement')
	& ('collection')
	& ('collector')
	& ('defaultBehavior')
	& ('errorHandling')
	& ('indexable')
	& ('init')
	& ('int32and64')
	& ('klein')
	& ('kleinCompilerTester')
	& ('kleinDB')
	& ('kleinExport')
	& ('kleinFrames')
	& ('kleinNMethod')
	& ('kleinPrims')
	& ('kleinRelocators')
	& ('kleinSelfVM')
	& ('kleinSendDesc')
	& ('kleinVM')
	& ('list')
	& ('mirror')
	& ('moduleInfo')
	& ('nil')
	& ('number')
	& ('orderedSet')
	& ('pair')
	& ('path')
	& ('point')
	& ('rootTraits')
	& ('selector')
	& ('sending')
	& ('slot')
	& ('string')
	& ('vector')
	& ('vmKitBase')
	& ('vmKitDB')
	& ('vmKitExport')
	& ('vmKitMirrorsGen')
	& ('vmKitVM')
	& ('vmKits')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent exportPolicy modulesToMap namesOfIncludedModules.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfModulesWhoseSubmodulesShouldBeIncludedToo = bootstrap setObjectAnnotationOf: ( (('absBCInterpreter')
	& ('asmKit')
	& ('integer')
	& ('kleinBytecodes')
	& ('kleinCompiler1')
	& ('kleinReflection')
	& ('mirror')
	& ('scheduler')
	& ('setAndDictionary')
	& ('smallInt')
	& ('vmKitBase')
	& ('vmKitGC')
	& ('vmKitMemInterface')
	& ('vmKitMemory')
	& ('vmKitObjects')
	& ('vmKitPrims')
	& ('vmKitReflection')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent exportPolicy modulesToMap namesOfModulesWhoseSubmodulesShouldBeIncludedToo.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfModulesWhichShouldIgnoreTransporterLink = bootstrap setObjectAnnotationOf: ( (collector copyFirst: ('asmGeneratedSlots')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent exportPolicy namesOfModulesWhichShouldIgnoreTransporterLink.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         startingPoints = ( |
            | 
            resend.startingPoints copyAddLast: lobby).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fComment: Objects that are not complete yet for which we would like to
select the slots to compile by sending kleinSelectorsToCompile.\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         wellKnownIncompleteObjectsWithSlotsToCompile = bootstrap setObjectAnnotationOf: ( ((reflect: klein sendDescs)
	& (reflect: traits abstractSetOrDictionary reflectiveIdentityComparisonMixin)
	& (reflect: klein compiler1s abstract prototypes allocators)
	& (reflect: traits)
	& (reflect: assemblerSystems framework generators)
	& (reflect: assemblerSystems ppc)
	& (reflect: klein compiler1s abstract prototypes irNodes)
	& (reflect: assemblerSystems ppc crFields)
	& (reflect: abstractBytecodeInterpreter bytecodes)
	& (reflect: assemblerSystems ppc operands)
	& (reflect: klein compiler1s abstract prototypes dataValues)
	& (reflect: assemblerSystems framework generators fields intOperandField signDependentOperations)
	& (reflect: lobby)
	& (reflect: assemblerSystems ppc pseudoInstructionTemplates)
	& (reflect: bytecodeFormat instructionSets)
	& (reflect: klein relocators)
	& (reflect: assemblerSystems)
	& (reflect: traits abstractSetOrDictionary equalityComparisonMixin)
	& (reflect: kleinAndYoda layouts)
	& (reflect: klein virtualMachines)
	& (reflect: klein compiler1s abstract)
	& (reflect: assemblerSystems ppc toMasks)
	& (reflect: klein compiler1s abstract prototypes)
	& (reflect: traits abstractSetOrDictionary identityComparisonMixin)
	& (reflect: assemblerSystems ppc fprs)
	& (reflect: assemblerSystems ppc generators)
	& (reflect: klein stackFrames)
	& (reflect: assemblerSystems ppc gprs)
	& (reflect: assemblerSystems ppc instructionTemplates)
	& (reflect: globals)
	& (reflect: assemblerSystems ppc sprs)
	& (reflect: traits block)
	& (reflect: slots)
	& (reflect: klein compiler1s abstract prototypes codeGenerators)
	& (reflect: kleinAndYoda lookupType)
	& (reflect: traits collection ascendingOrder)
	& (reflect: assemblerSystems framework)
	& (reflect: klein locations)
	& (reflect: klein compiler1s)
	& (reflect: traits slots)
	& (reflect: kleinAndYoda virtualMachines abstractVM stringComparisonMixin)
	& (reflect: traits abstractSet values)
	& (reflect: kleinAndYoda maps)
	& (reflect: klein mirrors)
	& (reflect: transporter moduleInfo)
	& (reflect: assemblerSystems ppc crBits)
	& (reflect: klein)
	& (reflect: assemblerSystems ppc fields)
	& (reflect: scheduler)
	& (reflect: kleinAndYoda)
	& (reflect: assemblerSystems ppc instructionAssemblyMethods)) asVmKitExportList) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent exportPolicy wellKnownIncompleteObjectsWithSlotsToCompile.

CopyDowns:
globals set. copy 
SlotsToOmit: parent prototype safety.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
            | 
            "_Breakpoint:  'Entered start method'."

            tests runAllTestsForVM: self.

            _Breakpoint: 'Exiting start method').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         tests = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent tests.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         activations = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'activations' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'activations' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent tests activations.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'activations' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'activations' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent tests activations parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'activations' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         createAnActivationAndCheckItsSelectorAndSender = ( |
             a.
             a2.
            | 
            _SaveAllNonVolatileRegisters.
            a: theVM vmKit mirrors methodActivation activationForLocalProcess: theVM vmKit foreignProcess copyLocal
                                                                           SP: int32 copy _SetInt32FromStackPointer.

            [createAnActivationAndCheckItsSelectorAndSender]. "browsing"
            [testCreatingActivations                       ]. "browsing"
            assert: a        selector Is: 'createAnActivationAndCheckItsSelectorAndSender'.
            assert: a sender selector Is: 'testCreatingActivations'.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'activations' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'activations' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         possiblyLiveOops = ( |
             currentActivation.
             senderActivation.
            | 
            _SaveAllNonVolatileRegisters.
            currentActivation: theVM vmKit mirrors methodActivation activationForLocalProcess: theVM vmKit foreignProcess copyLocal
                                                                                           SP: int32 copy _SetInt32FromStackPointer.
            senderActivation: currentActivation sender.
            possiblyLiveOopsIn: senderActivation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'activations' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         possiblyLiveOopsIn: a = ( |
             oops.
            | 
            oops: reflectiveIdentitySet copyRemoveAll.
            a possiblyLiveOopsDo: [|:oop| oops add: oop].
            oops).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'activations' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            testCreatingActivations.
            testReachingTheBeginningOfTheStack.
            testUsingALotOfNonVolRegs.
            [todo inProgress adam stackRoots]. "Make more tests for individual methods."
            [todo localLiveness]. "Maybe make a separate VM where local-liveness-checking is toggled."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'activations' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testCreatingActivations = ( |
            | 
            createAnActivationAndCheckItsSelectorAndSender.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'activations' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testReachingTheBeginningOfTheStack = ( |
             a.
             p.
            | 
            p: theVM vmKit foreignProcess copyLocal.
            a: theVM vmKit mirrors methodActivation activationForLocalProcess: p SP: int32 _Clone _SetInt32FromStackPointer.
            a meAndSendersDo: [|:sender| a: sender].
            assert: a selector Is: theVM entryMethodName.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'activations' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testUsingALotOfNonVolRegs = ( |
             a.
             b.
             c.
             d.
             e.
             f.
             g.
             h.
             oops.
            | 
            a: 1.
            b: a + 1.
            c: a + b.
            d: b + c.
            e: c + d.
            f: d + e.
            g: e + f.
            h: f + g.
            oops: possiblyLiveOops.
            assert: [oops includesAll: (1 & 2 & 3 & 5 & 8 & 13 & 21 & 34) asVector].
            a + b + c + d + e + f + g + h. "To make sure they're all still live."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         compiling = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent tests compiling.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent tests compiling parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         compileAndInstallSlotNamed: slotName On: targetObject = ( |
             newNM.
            | 
            newNM: compileSlotNamed: slotName On: targetObject.

            "It would sure make me more comfortable to have
             a test case that demonstrates that this flushing
             stuff is actually working. -- Adam, 5/05"
            newNM flushWhateverCachesAreNecessaryAfterModifyingMe.

            targetObject _Map installNewNMethod: newNM.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         compileSlotNamed: slotName On: targetObject = ( |
             c.
             s.
             targetMirror.
            | 
            targetMirror: targetObject _Mirror.
            s: targetMirror slotAt: slotName.
            c:
              theVM compilerPrototype
                     copyForSlot: s
                            Self: targetMirror
                        Receiver: s holder
                      LookupType: klein lookupType normal
            ObjectDoingTheResend: nil
                   OuterNMethods: list copyRemoveAll
                    Architecture: theVM architecture
                          Oracle: oracleForEagerRelocationInsideKlein
                           Debug: true.

            c compileForcingNonLeafIfNecessary buildNMethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         incompleteObject = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'incompleteObject' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent tests compiling parent incompleteObject.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'incompleteObject' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         add: x To: y = ( |
            | 
            x + y).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'incompleteObject' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         locals = ( |
             a <- 4.
             b = 3.
            | 
            a: a + a succ.
            a: a - b.
            a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'incompleteObject' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         returnFive = ( |
            | 
            5).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'incompleteObject' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         returnSelf = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'incompleteObject' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         threePlusFour = ( |
            | 
            3 + 4).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'incompleteObject' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         threePlusFourPrimitively = ( |
            | 
            3 _IntAdd: 4).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         oracleForEagerRelocationInsideKlein = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( |
             {} = 'Comment: Set linearizedObjects to me when you don\'t want to (or
can\'t) do eager relocation. -- Adam, 3/05\x7fModuleInfo: Creator: globals klein virtualMachines selfVM parent tests compiling parent oracleForEagerRelocationInsideKlein.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         findOopForStubNMethodNamed: name IfPresent: pb IfAbsent: ab = ( |
            | 
            klein primitives _Map _NMethodCache
                findFirst: [|:nm| nm lookupKey selector = name]
                IfPresent: pb
                 IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinifiedObjectForOriginalMirror: origMir = ( |
            | 
            origMir reflectee).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         nextOopToAllocateForObjectOfSize: nOops = ( |
            | 
            -1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForOriginalObject: obj IfAbsent: fb = ( |
            | 
            obj).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         originalObjectForOop: oop IfAbsent: fb = ( |
            | 
            oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         relocatorAssembledPlaceholderInstructions = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> 'oracleForEagerRelocationInsideKlein' -> () From: ( | {
         'Category: gathering statistics\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         relocatorAssembledRealInstructions = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'compiling' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
             r.
             targetObject.
            | 
            targetObject: incompleteObject.

            ["These work, but for now I don't want to take the time at runtime to run them."
            compileAndInstallSlotNamed: 'returnSelf' On: targetObject.
            assert: [targetObject returnSelf _Eq: targetObject].

            compileAndInstallSlotNamed: 'returnFive' On: targetObject.
            assert: [targetObject returnFive _Eq: 5].

            compileAndInstallSlotNamed: 'threePlusFourPrimitively' On: targetObject.
            assert: [targetObject threePlusFourPrimitively _Eq: 7].
            ].

            compileAndInstallSlotNamed: 'threePlusFour' On: targetObject.
            assert: [targetObject threePlusFour _Eq: 7].

            ["These work, but for now I don't want to take the time at runtime to run them."
            compileAndInstallSlotNamed: 'add:To:' On: targetObject.
            assert: [(targetObject add: 3 To: 4) = 7].

            compileAndInstallSlotNamed: 'locals' On: targetObject.
            assert: [targetObject locals = 6].
            ].

            ["Put this stuff back in someday once we have GC, but for now
             we run out of heap space."
            _NakedMethods: true.
            klein compilerTestPrograms testsToRun do: [|:t|
              t asMirror safeName _StringPrint. '\n' _StringPrint.
              t regenerateTestIfAutomaticallyGenerated.
              t compileMySlot.
            ].
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> () From: ( | {
         'Category: test cases\x7fCategory: automated\x7fModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         garbageCollection = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'garbageCollection' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein virtualMachines midiVM parent tests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'garbageCollection' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent tests garbageCollection.

CopyDowns:
globals klein virtualMachines midiVM parent tests abstract. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'garbageCollection' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'garbageCollection' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines selfVM parent tests garbageCollection parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'garbageCollection' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'garbageCollection' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            testTranslatingBetweenAddressAndOop.
            testGeneratingALotOfGarbage.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'garbageCollection' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testGeneratingALotOfGarbage = ( |
             approximateNumberOfVectorsNeededToFillTheSpace.
             freeBytesLeft.
             vectorSize = 100000.
            | 
            freeBytesLeft: theVM universe edenSpace sizeOfUnallocatedRegion.
            approximateNumberOfVectorsNeededToFillTheSpace: freeBytesLeft / (vectorSize * theVM oopSize).
            approximateNumberOfVectorsNeededToFillTheSpace * 3 do: [|:i|
              i printString _StringPrint. ' GC test iteration\n' _StringPrint.
              vector copySize: vectorSize.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> 'garbageCollection' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testTranslatingBetweenAddressAndOop = ( |
             a1.
             a2.
             aa2.
             o1.
             o2.
             oo1.
            | 
            o1:  [hereIsABlock].
            a1:  theVM layouts memoryObject addressOfLocalMem:  o1.
            oo1: theVM layouts memoryObject localMemForAddress: a1.
            assert: o1 _Eq: oo1.

            a2:  theVM universe edenSpace objsBottom. "Let's try the first object in the space."
            o2:  theVM layouts memoryObject localMemForAddress: a2.
            aa2: theVM layouts memoryObject addressOfLocalMem:  o2.
            assert: a2 = aa2.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> 'tests' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         runAutomatedTestsForVM: aVM = ( |
            | 
            resend.runAutomatedTestsForVM: aVM.
            [todo gc]. "Was in the middle of getting these activation tests
                        to work when we suspended work on Klein. -- Adam"
            activations run.        [todo cleanup testing kleinSpecific].
            garbageCollection run.  [todo cleanup testing kleinSpecific].
            compiling run.          [todo cleanup testing kleinSpecific].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'selfVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         kleinSelfVM = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSelfVM' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSelfVM' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinSelfVM.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSelfVM' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSelfVM' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSelfVM' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSelfVM' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSelfVM' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.117 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSelfVM' -> () From: ( | {
         'ModuleInfo: Module: kleinSelfVM InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinSelfVM postFileIn
