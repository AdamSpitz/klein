 '$Revision: 30.62 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
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
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         add: x To: y = ( |
            | 
            x _IntAdd: y).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: x Is: y = ( |
            | 
            __BranchIfTrue: (x _Eq: y) To: 'fine'.
            _Breakpoint: 'assertion failed'.
            __DefineLabel: 'fine'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         assertFail: blk = ( |
            | 
            blk value: [|:e. :p| ^ self].
            fail).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         branchTester = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent branchTester.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'Category: assertions\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: x Is: y = ( |
            | 
            __BranchIfTrue: (x _Eq: y) To: 'fine'.
            _Breakpoint: 'assertion failed'.
            __DefineLabel: 'fine'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         colocatedLocation1 = ( |
            | 
            17 _IntEQ: (17   __BranchIfTrue: true To: 'one').
            self foo:  (true __BranchIfTrue: true To: 'one').
            23 _IntAdd: __DefineLabel: 'one').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         colocatedLocation2 = ( |
            | 
            17 _Eq:    (self __BranchIfTrue: false To: 'one').
            self bar:  (true __BranchIfTrue: true  To: 'one').
            23 foo: __DefineLabel: 'one').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         colocatedLocation3 = ( |
            | 
            17 _Eq:   (self __BranchIfTrue: false To: 'one').
            self foo: (true __BranchIfTrue: false To: 'one').
            23 _Eq: __DefineLabel: 'one').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         colocatedLocation4 = ( |
             local <- 5.
            | 
            foo: local __BranchIfTrue: true To: 'a'.
            2 __BranchIfTrue: true To: 'b'.
            (7 __DefineLabel: 'a') __DefineLabel: 'b').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         foo: bur = ( |
             true = bootstrap stub -> 'globals' -> 'true' -> ().
            | 
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         foo: foo Bar: bar Baz: baz = ( |
             true = bootstrap stub -> 'globals' -> 'true' -> ().
            | 
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         indexedInLoop: x = ( |
             i <- 0.
            | 
            __DefineLabel: 'loop'.
                    i: i _IntAdd: 1.
                    (i _IntEQ: 5) ifTrue: [^ 'ok'] False: [].
            666 __BranchIndexedBy: x To: 'loop'.
            error: 'just checking').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            [aaaaa]. "Duplicated from midiVM tests branches, which
                      was duplicated from tests branches. Unify them.
                      Also midiVM tests allocation."
            testScalarBranches.
            testIndexedBranches.
            testColocatedValues.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testColocatedValues = ( |
             false = bootstrap stub -> 'globals' -> 'false' -> ().
             true = bootstrap stub -> 'globals' -> 'true' -> ().
            | 
            assert: colocatedLocation1 Is: 34.
            assert: colocatedLocation2 Is: true.
            assert: colocatedLocation3 Is: false.
            assert: colocatedLocation4 Is: 5.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testIndexedBranches = ( |
            | 
            assert: (testThreeCases: 'a') Is: 'fell'.
            assert: (testThreeCases: -1 ) Is: 'fell'.
            assert: (testThreeCases: 3  ) Is: 'fell'.
            assert: (testThreeCases: 0  ) Is: 'a'.
            assert: (testThreeCases: 1  ) Is: 'b'.
            assert: (testThreeCases: 2  ) Is: 'c'.
            assert: (indexedInLoop:  0  ) Is: 'ok'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testScalarBranches = ( |
             a <- 0.
             false = bootstrap stub -> 'globals' -> 'false' -> ().
             true = bootstrap stub -> 'globals' -> 'true' -> ().
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

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'branchTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testThreeCases: x = ( |
            | 
            666 __BranchIndexedBy: x To: 'zero' To: 'one' To: 'two'.
            'fell' __BranchTo: 'end'.

            assert: (__DefineLabel: 'zero') Is: 666.
            'a' __BranchTo: 'end'.
            assert: (__DefineLabel: 'one' ) Is: 666.
            'b' __BranchTo: 'end'.
            assert: (__DefineLabel: 'two' ) Is: 666.
            'c' __BranchTo: 'end'.

            error: 'just checking'.

            'no way' __DefineLabel: 'end').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         canCollectGarbage = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         dataSlotInliningTester = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent dataSlotInliningTester.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: x Is: y = ( |
            | 
            __BranchIfTrue: (x _Eq: y) To: 'fine'.
            _Breakpoint: 'assertion failed'.
            __DefineLabel: 'fine'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         child1 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> 'child1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent dataSlotInliningTester child1.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> 'child1' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (1)\x7fVisibility: private'
        
         a <- 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         commonParent = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> 'commonParent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent dataSlotInliningTester commonParent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> 'child1' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> 'commonParent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         child2 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> 'child2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent dataSlotInliningTester child2.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> 'child2' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         a = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> 'child2' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> 'commonParent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> 'commonParent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         callA = ( |
            | 
            a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> 'commonParent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         callAFromABlock = ( |
            | 
            [a] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> 'commonParent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         callAFromADoublyNestedBlock = ( |
            | 
            [[a] value] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> 'commonParent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'dataSlotInliningTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            assert: child1 callA                       Is: 1.
            assert: child2 callA                       Is: 2.
            assert: child1 callAFromABlock             Is: 1.
            assert: child2 callAFromABlock             Is: 2.
            assert: child1 callAFromADoublyNestedBlock Is: 1.
            assert: child2 callAFromADoublyNestedBlock Is: 2.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fComment: I\'m sure there\'s a more clever way to get
this number. For now it won\'t hurt much
to just keep upping it by hand.
-- Adam, 2/05\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         expectedNumberOfObjects = 19000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: unmapped\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         exportPolicy = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent exportPolicy.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> () From: ( | {
         'Category: nmethod compilation policy\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlotToBeCompiled: s = ( |
            | 
            (resend.isSlotToBeCompiled: s) || [(s name = 'value') && [s holder = (reflect: defaultBehavior)]]).
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
        
         cachedAllNamesOfIncludedModules <- bootstrap stub -> 'globals' -> 'nil' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfExcludedModules = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfIncludedModules = bootstrap setObjectAnnotationOf: ( (('')
	& ('blockTests')
	& ('boolean')
	& ('klein')
	& ('kleinFrames')
	& ('kleinNMethod')
	& ('kleinPrims')
	& ('kleinRelocators')
	& ('kleinTestVM')
	& ('kleinVM')
	& ('rootTraits')
	& ('vmKitBase')
	& ('vmKitGeneration')
	& ('vmKitMemory')
	& ('vmKitObjectLocator')
	& ('vmKitOops')
	& ('vmKitPrims')
	& ('vmKitSpace')
	& ('vmKitUniverse')
	& ('vmKitVM')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent exportPolicy modulesToMap namesOfIncludedModules.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'exportPolicy' -> 'modulesToMap' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         namesOfModulesWhoseSubmodulesShouldBeIncludedToo = bootstrap setObjectAnnotationOf: ( (('init')
	& ('languageTests')
	& ('testSuite')) asSet) From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent exportPolicy modulesToMap namesOfModulesWhoseSubmodulesShouldBeIncludedToo.
\x7fIsComplete: '.
            | ) .
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

            [value]. "browsing"
            s name = 'value' ifTrue: [^ true].

            "Don't want to include the whole defaultBehavior module, but
             there are a few slots on the defaultBehavior object (like 
             'value' and 'theVM') that need to get compiled and mapped."
            [defaultBehavior]. "browsing"
            s name = 'defaultBehavior' ifTrue: [^ true].

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
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         firstTenIntegers = [ | x =  ( bootstrap setObjectAnnotationOf: vector _Clone From: ( |
                     {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent firstTenIntegers.
'.
                    | ) ) _Clone: 10 Filler: 0| 
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
             x] value.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         in: bv ByteAt: i IfAbsent: blk = ( |
            | 
            bv _ByteAt: i IfFail: [|:e. :p| blk value: e With: p]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         loopAddingNumbersFromZeroTo: n = ( |
             i <- 0.
             sum <- 0.
            | 
            (i _IntLE: n) ifFalse: [^ sum].
            sum: sum _IntAdd: i.
            i: i _IntAdd: 1.
            _Restart).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         methodForLiveMutation = ( |
            | 
            3 + 4).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         methodWithLocals: x AndArgs: a = ( |
             b <- 2.
             c.
             d = 'four'.
             y <- 'banana'.
            | 
            a _IntAdd: b).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         methodWithManyArguments: a And: b And: c And: d And: e And: f And: g And: h And: i And: j And: k = ( |
             x.
            | 
            x: a _IntAdd: b _IntAdd: c _IntAdd: d _IntAdd: e _IntAdd: f _IntAdd: g _IntAdd: h _IntAdd: i _IntAdd: j _IntAdd: k.
            x _IntAdd: [a _IntAdd: k] value).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
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
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         one = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> 'abstractVM' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         performTester = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent performTester.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         add: x To: y = ( |
            | 
            x _IntAdd: y).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: x Is: y = ( |
            | 
            __BranchIfTrue: (x _Eq: y) To: 'done'.
            _Breakpoint: 'assertion failed'.
            __DefineLabel: 'done'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         one = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         perform: s = ( |
            | 
            _Perform: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         resendObj = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent performTester resendObj.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         dynamicUndirectedResend: sel = ( |
            | 
            _PerformResend: sel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         makeSureResentMethodsGetCompiled = ( |
            | 
            resend.makeSureResentMethodsGetCompiled _IntAdd: resend.z _IntAdd: resend.w _IntAdd: parentA.a _IntAdd: parentB.a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parentA* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentA' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent performTester resendObj parentA.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (55)'
        
         a <- 55.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         dynamicUndirectedResendInParent: sel = ( |
            | 
            _PerformResend: sel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         makeSureResentMethodsGetCompiled = ( |
            | 
            "Just to make sure the resent methods get compiled."
            resend.z _IntAdd: resend.w).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentA' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent performTester resendObj parentA parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentA' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentA' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         w = ( |
            | 7).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentA' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         z = 6.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         staticUndirectedResendInParent = ( |
            | 
            _PerformResend: 'z').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         w = 8.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentA' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         z = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         parentB* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentB' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent performTester resendObj parentB.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> 'parentB' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         a = 44.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         staticUndirectedResend = ( |
            | 
            _PerformResend: 'z').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (9)'
        
         w <- 9.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> 'resendObj' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         z = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
             parentB = bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'midiVM' -> 'parent' -> 'tests' -> 'performing' -> 'parent' -> 'resendObj' -> 'parentB' -> ().
            | 

            "Duplicated from the midiVM tests because it can work in the miniVM."

            assert: one                    Is: 1. "Just make sure it gets compiled."
            assert: two                    Is: 2. "Just make sure it gets compiled."
            assert: (_Perform: 'one')      Is: 1.
            assert: (_Perform: 'two')      Is: 2.
            assert: (send: 'one' To: self) Is: 1.
            assert: (send: 'two' To: self) Is: 2.

            assert: (_Perform: 'add:To:'          With: 3 With: 4 ) Is: 7.
            assert: (send:     'add:To:' To: self With: 3 With: 4 ) Is: 7.
            assert: (send:     'add:To:' To: self With: 5 With: -2) Is: 3.

            [
              [todo delegatedPerform]. "Not implemented yet."
              assert: (resendObj _Perform: 'a' DelegatingTo:           parentB) Is: 44.
              assert: (resendObj _Perform: 'a' DelegatingTo: resendObj parentB) Is: 44.
              assert: (send: 'a' To: resendObj DelegatingTo: resendObj parentB) Is: 44.
              assert: (send: 'a' To: resendObj DelegatingTo: resendObj parentA) Is: 55.
            ].

            resendObj makeSureResentMethodsGetCompiled.

            assert:  resendObj  staticUndirectedResend               Is: 5.
            assert: (resendObj dynamicUndirectedResend:         'z') Is: 5.
            assert: (resendObj dynamicUndirectedResend:         'w') Is: 8.
            assert:  resendObj  staticUndirectedResendInParent       Is: 6.
            assert: (resendObj dynamicUndirectedResendInParent: 'z') Is: 6.
            assert: (resendObj dynamicUndirectedResendInParent: 'w') Is: 7.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         send: sel To: rcvr = ( |
            | 
            rcvr _Perform: sel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         send: sel To: rcvr DelegatingTo: del = ( |
            | 
            rcvr _Perform: sel DelegatingTo: del).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         send: sel To: rcvr With: a1 With: a2 = ( |
            | 
            rcvr _Perform: sel With: a1 With: a2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testSimplePerform = ( |
            | 
            _Perform: 'one').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'performTester' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         two = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
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
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         recursiveFactorial: i = ( |
             j <- 1.
            | 
            0 __BranchIfTrue: (i _Eq: 1) To: 'end'.
            j: (recursiveFactorial: i _IntAdd: -1) _IntMul: i.
            0 __DefineLabel: 'end'.
            j).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         resendingTests = bootstrap stub -> 'globals' -> 'tests' -> 'resending' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         simpleMethod1 = ( |
            | 
            simpleMethod2a.
            simpleMethod2b.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         simpleMethod2a = ( |
            | 
            3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         simpleMethod2b = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
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
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
             false = bootstrap stub -> 'globals' -> 'false' -> ().
             true = bootstrap stub -> 'globals' -> 'true' -> ().
            | 
            testReturnValueOfLocalAssignment.
            testLeafMethodWithBrowsingTag.
            testArithmetic.
            testCallingBlocks.
            testInitializingLocals.
            testMethodWithManyArguments.
            testIfTrueAndIfFalse.
            testInliningSimpleMethods.
            tryingToReplicateInliningBug run.
            testCallingFakePrimitive.
            testPrimitiveFailure.
            testFixAndContinue: 10 With: 11 And: 6.
            [pleh] vmTests run.
            testOnNonLocalReturn.
            branchTester run.
            testRestart.
            assert: (recursiveFactorial: 10) Is: 3628800.
            resendingTests run.
            dataSlotInliningTester run.
            testMapsOfObjectLiterals.
            testControlFlowOrderCodeGeneration: true.
            testControlFlowOrderCodeGeneration: false.
            "Uncomment this to see the proxy bug. -- Adam, 12/05"
            "I think the bug is fixed now. -- DMU, 12/05"
            [updatingTester testUpdatingUIWhileRunning].
            obj  r: obj  a _IntAdd: obj  b.
            obj pr: obj pa _IntAdd: obj pb.
            x: ten _IntAdd: eleven.
            three: one _IntAdd: two.
            b. "for incremental update test -- dmu 8/04"
            performTester run.
            testVectors.
            _Breakpoint: 'done'.
            three).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testArithmetic = ( |
            | 
            assert: ( 3 _IntAdd: 4) Is: 7.
            assert: ( 5 _IntSub: 6) Is: -1.
            assert: ( 7 _IntMul: 8) Is: 56.
            assert: (42 _IntDiv: 3) Is: 14.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testCallingBlocks = ( |
            | 
            assert: [53] value Is: 53.
            assert: [19] value Is: 19.
            assert: ([|:x| x _IntAdd: 7] value: 9) Is: 16.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testCallingFakePrimitive = ( |
             vm.
            | 
            vm: 'cccccc' _TheVM.
            assert: [vm eleven _Eq: 11].
            vm: nil.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testControlFlowOrderCodeGeneration: b = ( |
             x.
            | 
            __BranchIfFalse: b To: 'arglebargle'.
            x: 3.
            __BranchTo: 'done'.
            x: 4. "Should never get here."
            __DefineLabel: 'arglebargle'.
            x: 5.
            __DefineLabel: 'done'.
            x: x _IntAdd: 10.
            "x should be 13 if b was true, 15 if it was false. This test used to
             crash the VM in the false case, because the compiler wasn't inserting
             a branch to the controlFlowSuccWhenFallingThrough. -- Adam, Mar. 2009"
            x).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
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
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testFixAndContinue: b With: a And: x = ( |
             r <- 0.
            | 
            r: r _IntAdd: a _IntAdd: 1.
            r: r _IntMul: x.
            r: r _IntSub: b.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testIfTrueAndIfFalse = ( |
            | 
            assert: (true  ifTrue: 3 False: 4) Is: 3.
            assert: (false ifTrue: 5 False: 6) Is: 6.
            false ifTrue: [fail].
            true ifFalse: [fail].
            assert: (true ifTrue: [7]) Is: 7.
            assert: (false ifFalse: 8) Is: 8.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testInitializingLocals = ( |
             a.
             b <- 3.
            | 
            assert: a Is: nil.
            assert: b Is: 3.
            assert: [|x| x] value Is: nil.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testInliningSimpleMethods = ( |
            | 
            simpleMethod1.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testLeafMethodWithBrowsingTag = ( |
            | 
            [argle bargle]. "browsing"
            'argle bargle').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
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
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testMethodWithManyArguments = ( |
             r.
            | 
            r: methodWithManyArguments: 1 And: 2 And: 3 And: 4 And: 5 And: 6 And: 7 And: 8 And: 9 And: 10 And: 11.
            assert: r Is: 78.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testOnNonLocalReturn = ( |
            | 
            onNonLocalReturnTester test).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testPrimitiveFailure = ( |
             atPut.
             blah.
             failBlockRan <- bootstrap stub -> 'globals' -> 'false' -> ().
             seven.
            | 
            blah: primitiveFailureTester _Blah.

            atPut: primitiveFailureTester _At: 3 Put: 4.

            seven: 3 _IntAdd: 4 IfFail: [error: 'lalala'].
            assert: seven Is: 7.

            "Test passing failblocks directly into primitives.
             Failblocks should be cloned lazily."
            3 _IntAdd: false IfFail: [|:e. :p| failBlockRan: true].
            assert: failBlockRan Is: true.
            failBlockRan: false.
            'abc' _ByteAt: 3 IfFail: [|:e. :p| failBlockRan: true].
            assert: failBlockRan Is: true.

            assertFail: [|:fb| 'abc' _ByteAt: 3 IfFail: fb].
            assertFail: [|:fb| in: 'abc' ByteAt: 3 IfAbsent: fb].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testRestart = ( |
            | 
            assert: (loopAddingNumbersFromZeroTo: 10) Is: 55.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testReturnValueOfLocalAssignment = ( |
             local.
             shouldBeSelf.
            | 
            obj b. "Put something else in r3."

            shouldBeSelf: local: 3.

            "Make sure the result is really self."
            shouldBeSelf universe.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         testVectors = ( |
            | 
            assert: (firstTenIntegers _At:  4                          ) Is: 5.
            assert: (firstTenIntegers _At:  7 IfFail: [error: 'lalala']) Is: 8.
            assert: (firstTenIntegers _At: 10 IfFail: [|:e. :p| e     ]) Is: 'badIndexError'.
            assert: (firstTenIntegers _At: -1 IfFail: [|:e. :p| e     ]) Is: 'badIndexError'.
            assert: (19               _At:  3 IfFail: [|:e. :p| e     ]) Is: 'badTypeError'.
            assert: (false            _At:  1 IfFail: [|:e. :p| e     ]) Is: 'badTypeError'.
            assert: (firstTenIntegers _At:  2 Put: 'three'             ) Is: firstTenIntegers.
            assert: (firstTenIntegers _At:  2                          ) Is: 'three'.
            assert: (firstTenIntegers _At:  2 Put: 3                   ) Is: firstTenIntegers.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: InitializeToExpression: (nil)'
        
         three.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         tryingToReplicateInliningBug = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'tryingToReplicateInliningBug' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein virtualMachines miniVM parent tryingToReplicateInliningBug.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'tryingToReplicateInliningBug' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: blk = ( |
            | 
            blk value ifFalse: [_Breakpoint: 'Klein test failed'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'tryingToReplicateInliningBug' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: a Is: b = ( |
            | 
            "This method is just useful because you
             can click on a and b right in the debugger
             to see what the two values are."
            assert: [a _Eq: b]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'tryingToReplicateInliningBug' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'tryingToReplicateInliningBug' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            0.
            testCheckingReceiverMap.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'tryingToReplicateInliningBug' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldReturnOne = ( |
            | 
            true ifTrue: 0.
            true ifTrue: [^ 1].
            fail).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> 'tryingToReplicateInliningBug' -> () From: ( | {
         'ModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
         testCheckingReceiverMap = ( |
            | 
            assert: shouldReturnOne Is: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fCategory: helper slots\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
         two <- 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'virtualMachines' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: tests\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot'
        
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
         'Category: accessing\x7fModuleInfo: Module: kleinTestVM InitialContents: FollowSlot\x7fVisibility: private'
        
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
