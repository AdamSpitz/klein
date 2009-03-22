 '$Revision: 30.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaParserTester InitialContents: FollowSlot\x7fVisibility: public'
        
         tester = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals parseKit tester copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser tester.

CopyDowns:
globals parseKit tester. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> () From: ( | {
         'ModuleInfo: Module: javaParserTester InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser tester parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: javaParserTester InitialContents: FollowSlot\x7fVisibility: private'
        
         commentPrefix = '/* '.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: javaParserTester InitialContents: FollowSlot\x7fVisibility: private'
        
         commentSuffix = ' */'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: javaParserTester InitialContents: FollowSlot\x7fVisibility: private'
        
         commentify: s = ( |
             r <- ''.
            | 
            s do: [|:c|
              ('/*\\' includes: c) ifTrue: [
                r: r, '\\'.
              ].
              r: r, c
            ].
            resend.commentify: r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParserTester InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'tester' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParserTester InitialContents: FollowSlot\x7fVisibility: public'
        
         prototypes* = bootstrap stub -> 'globals' -> 'javaParser' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: files\x7fModuleInfo: Module: javaParserTester InitialContents: FollowSlot\x7fVisibility: private'
        
         testDirectory = 'applications/klein/javaParser'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: files\x7fModuleInfo: Module: javaParserTester InitialContents: FollowSlot'
        
         testFileSuffix = '.java'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParserTester InitialContents: FollowSlot'
        
         tests = ( |
            | 
            (tests:  3 Named: 'arrayInit' ),
            (tests:  2 Named: 'blockDcl'),
            (tests:  5 Named: 'break'  ),
            (tests:  4 Named: 'case'  ),
            (tests:  6 Named: 'class' ),
            (tests:  2 Named: 'constructor' ),
            (tests:  5 Named: 'cont'  ),
            (tests:  2 Named: 'default'  ),
            (tests:  8 Named: 'do'  ),
            (tests:  3 Named: 'emptyStmt' ),
            (tests:  3 Named: 'expr'  ),
            (tests:  8 Named: 'expr1_'  ),
            (tests:  3 Named: 'expr2_'  ),
            (tests: 22 Named: 'expr3_'  ),
            (tests:  2 Named: 'exprStmt'  ),
            (tests:  2 Named: 'extends'  ),
            (tests: 15 Named: 'for' ),
            (tests:  7 Named: 'formal' ),
            (tests: 12 Named: 'if'  ),
            (tests:  3 Named: 'import' ),
            (tests:  4 Named: 'interface' ),
            (tests:  3 Named: 'label'  ),
            (tests:  3 Named: 'lexer'  ),
            (tests:  2 Named: 'missingAttr'  ),
            (tests:  2 Named: 'nestedBlock' ),
            (tests:  8 Named: 'package' ),
            (tests: 60 Named: 'prim' ),
            (tests:  2 Named: 'prototype' ),
            (tests:  5 Named: 'return'  ),
            (tests:  3 Named: 'staticInit'),
            (tests:  6 Named: 'switch'  ),
            (tests:  8 Named: 'sync'  ),
            (tests:  4 Named: 'throw'  ),
            (tests:  3 Named: 'throws'  ),
            (tests:  6 Named: 'try' ),
            (tests:  2 Named: 'unmatched' ),
            (tests:  2 Named: 'var' ),
            (tests:  8 Named: 'while' ),
            (
              'dclInBlock'
            & 'emptyFile' 
            & 'implementsComma'
            & 'intConstructor'
            & 'ivar'
            & 'justTry'
            & 'noTryBlock'
            & 'method1'
            & 'mismatchedBracket'
            & 'missingInit'
            & 'noNameStmt'
            & 'plusFn'
            & 'qualifiedConstructor'
            & 'simpleInit'
            & 'unterminatedStatement'
            & 'wrongFinally'
            ) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaParserTester InitialContents: FollowSlot\x7fVisibility: private'
        
         theParser = ( |
            | compilationUnitParser).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'tester' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: javaParserTester InitialContents: FollowSlot\x7fVisibility: private'
        
         uncommentify: s = ( |
             foundBackSl <- bootstrap stub -> 'globals' -> 'false' -> ().
             r <- ''.
            | 
            (resend.uncommentify: s) do: [|:c|
              foundBackSl not && [c == '\\']
                ifTrue: [ foundBackSl: true ]
                 False: [ foundBackSl: false.  r: r, c ]
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaParserTester InitialContents: FollowSlot'
        
         javaParserTester = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaParserTester' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaParserTester' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaParserTester.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaParserTester' -> () From: ( | {
         'ModuleInfo: Module: javaParserTester InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaParser'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaParserTester' -> () From: ( | {
         'ModuleInfo: Module: javaParserTester InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaParserTester' -> () From: ( | {
         'ModuleInfo: Module: javaParserTester InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaParserTester' -> () From: ( | {
         'ModuleInfo: Module: javaParserTester InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaParserTester' -> () From: ( | {
         'ModuleInfo: Module: javaParserTester InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaParserTester' -> () From: ( | {
         'ModuleInfo: Module: javaParserTester InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules javaParserTester postFileIn
