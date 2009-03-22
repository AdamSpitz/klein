 '$Revision: 30.62 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         miniVM = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda virtualMachines abstractVM copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM.

CopyDowns:
globals kleinAndYoda virtualMachines abstractVM. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         asd = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'asd' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM asd.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'asd' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         a = ( |
            | foo).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'asd' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         p* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         b = ( |
            | 12).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         eleven = 11.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         obj = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM obj.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (1)'
        
         a <- 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         b = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         justHereSoWeWillCompilePP = ( |
            | 
            parent.pp.
            blah.
            parentB.pp).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM obj parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (4)'
        
         pa <- 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         pb = 8.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         pp = 40.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (nil)'
        
         pr.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parentB* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> 'parentB' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM obj parentB.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         pp = 44.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'obj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (nil)'
        
         r.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( |
             {} = 'Comment: A Klein test program.
Once created, this object is mapped, exported, and
finally launched in a foreignProcess ``over there\'\'
where it initializes itself then runs some unit
tests on data slot reading and writing.

-- jb 6/03\x7fModuleInfo: Creator: globals klein virtualMachines miniVM parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         add: x To: y = ( |
            | 
            x _IntAdd: y).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         assertFail: blk = ( |
            | 
            blk value: [|:e. :p| ^ self].
            fail).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fComment: I\'m sure there\'s a more clever way to get
this number. For now it won\'t hurt much
to just keep upping it by hand.
-- Adam, 2/05\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         expectedNumberOfObjects = 12000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         exportPolicy = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent exportPolicy.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         modulesToMap = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent exportPolicy modulesToMap.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedAllNamesOfIncludedModules.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfExcludedModules = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfIncludedModules = bootstrap setObjectAnnotationOf: ( (('')
	& ('boolean')
	& ('klein')
	& ('kleinFrames')
	& ('kleinNMethod')
	& ('kleinPrims')
	& ('kleinRelocators')
	& ('kleinTestVM')
	& ('kleinVM')
	& ('vmKitBase')
	& ('vmKitGeneration')
	& ('vmKitMemory')
	& ('vmKitObjectLocator')
	& ('vmKitOops')
	& ('vmKitSpace')
	& ('vmKitUniverse')
	& ('vmKitVM')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent exportPolicy modulesToMap namesOfIncludedModules.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfModulesWhoseSubmodulesShouldBeIncludedToo = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> 'modulePartitioningProto' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> 'exportPolicy' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: object mapping policy\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldAlwaysMapSlotNoMatterWhatModuleItIsIn: s = ( |
            | 
            [annotation. compiledOop]. "browsing"

                (s name = 'annotation')
            || [ s name = 'compiledOop'] ifTrue: [^ true].

            s holder isReflecteeVMKitActivationMap ifTrue: [
              [file. line]. "browsing"
                  (s name = 'file')
              || [ s name = 'line'] ifTrue: [^ true].
            ].

            resend.shouldAlwaysMapSlotNoMatterWhatModuleItIsIn: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fComment: Objects that are not complete yet for which we would like to
select the slots to compile by sending kleinSelectorsToCompile.\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         wellKnownIncompleteObjectsWithSlotsToCompile = bootstrap setObjectAnnotationOf: ( ((reflect: klein)
	& (reflect: kleinAndYoda)
	& (reflect: kleinAndYoda layouts)) asVmKitExportList) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent exportPolicy wellKnownIncompleteObjectsWithSlotsToCompile.

CopyDowns:
globals set. copy 
SlotsToOmit: parent prototype safety.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         make: obj Perform: s DelegatingTo: d = ( |
            | 
            obj _Perform: s DelegatingTo: d).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         methodForLiveMutation = ( |
            | 
            3 + 4).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         methodWithLocals: x AndArgs: a = ( |
             b <- 2.
             c.
             d = 'four'.
             y <- 'banana'.
            | 
            a + b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         onNonLocalReturnTester = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent onNonLocalReturnTester.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (false)'
        
         b1Ran <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (false)'
        
         b2Ran <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         f = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> 'f' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent onNonLocalReturnTester f.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> 'f' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         ifFalse: b = ( |
            | 
            b value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> 'f' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         ifTrue: b = ( |
             n.
            | 
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> 'f' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         ifTrue: bt False: bf = ( |
            | 
            bf value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> 'f' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (false)'
        
         mRan <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         t = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> 't' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent onNonLocalReturnTester t.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> 't' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         ifFalse: b = ( |
             n.
            | 
            n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> 't' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         ifTrue: b = ( |
            | 
            b value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> 't' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         ifTrue: bt False: bf = ( |
            | bt value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> 't' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         test = ( |
            | 
            testAndCheckNLR1: f  NLR2: f.
            testAndCheckNLR1: t  NLR2: f.
            testAndCheckNLR1: f  NLR2: t.
            testAndCheckNLR1: t  NLR2: t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testAndCheckNLR1: nlr1 NLR2: nlr2 = ( |
             r.
            | 
            r: testNLR1: nlr1 NLR2: nlr2.
            b1Ran ifFalse: [_Breakpoint: 'onNonLocalReturnTester 1'].
            nlr1 ifFalse: [
              b2Ran ifTrue:  [_Breakpoint: 'onNonLocalReturnTester 2'].
              mRan  ifFalse: [_Breakpoint: 'onNonLocalReturnTester 3'].
              __BranchIfTrue: (r _Eq: 2) To: 'ok1'.
             _Breakpoint: 'onNonLocalReturnTester 4'.
             __DefineLabel: 'ok1'.
              ^ self.
            ].
            b2Ran ifFalse: [_Breakpoint: 'onNonLocalReturnTester 5'].
            mRan  ifTrue:  [_Breakpoint: 'onNonLocalReturnTester 6'].
            __BranchIfTrue: ((nlr2 ifTrue: [2] False: [3]) _Eq: r) To: 'ok2'.
            _Breakpoint: 'onNonLocalReturnTester 7'.
            __DefineLabel: 'ok2'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'onNonLocalReturnTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testNLR1: nlr1 NLR2: nlr2 = ( |
             r.
            | 
            b1Ran: f. b2Ran: f. mRan: f.
            r: [
              b1Ran: t.
              nlr1 ifTrue: [^ 1].
              2
            ]
             _OnNonLocalReturn: [|:retVal|
              __BranchIfTrue: (1 _Eq: retVal) To: 'retValOk'.
              _Breakpoint: '_onNonLocalReturn: bad retVal'.
              __DefineLabel: 'retValOk'.
               b2Ran: t.
                nlr2 ifTrue: [^ 2].
               3
            ]
            IfFail: [|:a| _Breakpoint: '_onNonLocalReturn: failed'].
            mRan: t.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         one = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         perform: s = ( |
            | 
            _Perform: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         perform: s With: a With: b = ( |
            | 
            _Perform: s With: a With: b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         primitiveFailureTester = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'primitiveFailureTester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent primitiveFailureTester.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'primitiveFailureTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'primitiveFailureTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         primitiveFailedError: e Name: n = ( |
            | 
            e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         recursiveFactorial: i = ( |
             j <- 1.
            | 
            0 __BranchIfTrue: (i _Eq: 1) To: 'end'.
            j: (compile_me: i _IntAdd: -1) _IntMul: i.
            0 __DefineLabel: 'end'.
            j).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         slotFinderTesterObj = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'slotFinderTesterObj' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent slotFinderTesterObj.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'slotFinderTesterObj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         a = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'slotFinderTesterObj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'slotFinderTesterObj' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent slotFinderTesterObj parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'slotFinderTesterObj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         a = 6.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'slotFinderTesterObj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         m = ( |
            | 
            a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'slotFinderTesterObj' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         start = ( |
            | 
            testPrimitiveFailure.
            testFixAndContinue: 10 With: 11 And: 6.
            testOnNonLocalReturn.
            testMapsOfObjectLiterals.
            "Uncomment this to see the proxy bug. -- Adam, 12/05"
            "I think the bug is fixed now. -- DMU, 12/05"
            [updatingTester testUpdatingUIWhileRunning].
            obj  r: obj  a _IntAdd: obj  b.
            obj pr: obj pa _IntAdd: obj pb.
            x: ten _IntAdd: eleven.
            three: one _IntAdd: two.
            b. "for incremental update test -- dmu 8/04"
            _Breakpoint: 'done'.
            three).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testDataSlots = ( |
            | 
            obj r: obj a _IntAdd:  obj b.
            _Breakpoint: 'Just did: obj r: obj a + obj b'.
            obj pr: obj pa _IntAdd: obj pb.
            _Breakpoint: 'Just did: obj pr: obj pa + obj pb'.
            x: ten _IntAdd: eleven.
            _Breakpoint: 'Just did: x: ten + eleven'.
            three: one _IntAdd: two.
            _Breakpoint: 'Just did: three: one + two'.
            three).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testFixAndContinue: b With: a And: x = ( |
             r <- 0.
            | 
            r: r _IntAdd: a _IntAdd: 1.
            r: r _IntMul: x.
            r: r _IntSub: b.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testMapsOfObjectLiterals = ( |
             mapNMC.
             mapNMCSize.
             nmc.
             nmcSize.
             o.
            | 
            "If you do an incremental update of this method, the size will end up being 0."
            o: (| apple = 55 |).
               nmc:     o _Map      _NMethodCache.
            mapNMC:     o _Map _Map _NMethodCache.
               nmcSize:    nmc _Size.
            mapNMCSize: mapNMC _Size.
            "Can't call apple because o is not complete. But that's OK. The bug here is
             that mapNMCSize is wrong. -- Adam, 6/05"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testOnNonLocalReturn = ( |
            | 
            onNonLocalReturnTester test).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testPrimitiveFailure = ( |
             atPut.
             blah.
            | 
            blah: primitiveFailureTester _Blah.
            atPut: primitiveFailureTester _At: 3 Put: 4.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (nil)'
        
         three.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         two <- 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         updatingTester = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'updatingTester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent updatingTester.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'updatingTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (0)'
        
         i <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'updatingTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         keepIncrementingUpTo: max = ( |
            | 
            __BranchIfTrue: (i _IntEQ: max) To: 'done'.
            i: i _IntAdd: 1.
            _Restart.
            __DefineLabel: 'done'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'updatingTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'updatingTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         testUpdatingUIWhileRunning = ( |
            | 
            _Breakpoint: 'Get an outliner on the receiver. Watch the i slot.'.
            i: 0.
            keepIncrementingUpTo: 100000000.
            _Breakpoint: 'Did i update reasonably often but not too often?'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (10)'
        
         ten <- 10.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (nil)'
        
         x.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         kleinTestVM = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinTestVM' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinTestVM' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinTestVM.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinTestVM' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinTestVM' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinTestVM' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinTestVM' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinTestVM' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.62 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinTestVM' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinTestVM postFileIn
