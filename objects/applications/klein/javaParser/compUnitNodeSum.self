 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: node summaries for outliner\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot'
        
         compilationUnit = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser abstractNodeSummary copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser compilationUnit.

CopyDowns:
globals javaParser abstractNodeSummary. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> () From: ( | {
         'ModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot'
        
         myFileName <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> () From: ( | {
         'ModuleInfo: Module: compUnitNodeSum InitialContents: InitializeToExpression: (\'\')'
        
         myPackageName <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> () From: ( | {
         'ModuleInfo: Module: compUnitNodeSum InitialContents: InitializeToExpression: (\'\')'
        
         mySource <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> () From: ( | {
         'ModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser compilationUnit parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: equality\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot'
        
         = cu = ( |
            | cu equalsCompilationUnit: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: outliner support\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot'
        
         category = ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: classes and interfaces\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         classOrInterfaceNamed: n IfAbsent: ab = ( |
            | 
            hasParseTree ifFalse: [^ ab value].
            javaParser classOrInterface copyParseTree: 
              parseTree classOrInterfaceNamed: n IfAbsent: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: file comment\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         comment: c = ( |
            | 
            unimplemented).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | resend.copy initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: equality\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot'
        
         equalsCompilationUnit: cu = ( |
            | 
            fileName = cu fileName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: file name\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         fileName = ( |
            | myFileName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: file name\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         fileName: n = ( |
            | 
            fileName: n 
              IfFail: [|:e| error: 'Could not set file name in Java class to: ', n]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: file name\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         fileName: n IfFail: fb = ( |
            | 
            myFileName: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         fromFileNamed: cfn = ( |
            | 
            copy initialize readSourceFromFileNamed: cfn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         fromPackageNamed: pn = ( |
            | 
            [todo unimplemented]. "java path goes here"
            fromFileNamed: 
             pn copyMappedBy: [|:c| c = '.' ifTrue: '/' False: c]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: equality\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot'
        
         hash = ( |
            | fileName hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: imports\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot'
        
         importDcls = ( |
            | 
            hasParseTree ifFalse: [^ vector].
            parseTree importDcls copyMappedBy: [|:impDclNode|
              javaParser importDcl copyParseTree: impDclNode
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: outliner support\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         name = ( |
            | 
            fileName isEmpty ifTrue: '(a compilation unit)'
                              False: fileName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: package name\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot'
        
         packageDclIfPresent: pb IfAbsent: ab = ( |
            | 
            hasParseTree && [parseTree hasPackageDcl]
             ifTrue: [
              pb value: 
                javaParser packageDcl copyParseTree:
                   parseTree packageDcl
            ]
            False: ab).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: package name\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         packageName = ( |
            | 
            myPackageName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: package name\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         packageName: n IfFail: fb = ( |
            | 
            fb value:
               parseKit syntaxError reason: 'packageName:IfFail: is unimplemented'
                                    Source: source).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: parsing\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: private'
        
         parseSource: s IfFail: fb = ( |
             cfn.
            | 
            cfn:  javaParser compilationUnitParser    
                     copyParseSource: s
                              IfFail: [|:e| ^ fb value: e].
            myPackageName: cfn packageName.
            cfn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: parsing\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         parseSourceIfFail: fb = ( |
            | 
            myParseTree: parseSource: source IfFail: fb.
            parseTree).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: source code\x7fCategory: reading and writing\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         readSourceFromFileNamed: fn = ( |
            | 
            readSourceFromFileNamed: fn IfFail: [|:e| error: e asString]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: source code\x7fCategory: reading and writing\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         readSourceFromFileNamed: fn IfFail: fb = ( |
             f.
            | 
            fn = '' ifTrue: [^ fileName: ''].
            f: os_file openForReading: fn
              IfFail: [|:e| ^ fb value:
                               parseKit syntaxError reason: 'Could not open <', fn, '>: ', e
                                                    Source: source].
            source: f contents copyMappedBy: [|:c| c = '\r' ifTrue: '\n' False: c].
            f close.
            fileName: fn.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: relationships\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         references = ( |
            | unimplemented).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: source code\x7fCategory: reading and writing\x7fComment: functional\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         saveAs: fn IfFail: fb = ( |
             r.
            | 
               ( os_file exists: fn)
            && [ userQuery askMultipleChoice: fn, ' exists.\n',
                                    'Do you really want to replace it?'
                  Choices: (('Yes, zap ', fn) & 'No, forget it.') asVector
                  Results: (true & false) asVector
            ] ifTrue: [^fb value: fn, ' exists'].
            r: copy.
            r fileName: fn IfFail: [|:e| ^ fb value: e].
            r writeSourceIfFail: [|:e| ^ fb value: e].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: source code\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         source = ( |
            | mySource).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: source code\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         source: s = ( |
            | source: s IfFail: [|:e| error: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: source code\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         source: s IfFail: fb = ( |
            | 
            "fb takes a parseKit syntaxError object argument"
            mySource: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: relationships\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         subclasses = ( |
            | 
            unimplemented).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: classes and interfaces\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         typeDcls = ( |
            | 
            hasParseTree ifFalse: [^ vector].
            parseTree typeDcls copyMappedBy: [|:n|
              javaParser classOrInterface copyParseTree: n]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: source code\x7fCategory: reading and writing\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         writeSourceIfFail: fb = ( |
            | writeSourceToFileNamed: fileName IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: source code\x7fCategory: reading and writing\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         writeSourceToFileNamed: fn = ( |
            | 
            writeSourceToFileNamed: fn IfFail: [|:e|
              error: e
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'compilationUnit' -> 'parent' -> () From: ( | {
         'Category: accessing compilation unit parts\x7fCategory: source code\x7fCategory: reading and writing\x7fModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         writeSourceToFileNamed: fName IfFail: fb = ( |
             f.
            | 
            f: os_file openForWriting:
              fName IfFail: [|:e|  ^ fb value:
                               parseKit syntaxError reason: 'Could not open <', fName, '>: ', e
                                                    Source: source].
            f write: source.
            f close.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot'
        
         compUnitNodeSum = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'compUnitNodeSum' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'compUnitNodeSum' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules compUnitNodeSum.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'compUnitNodeSum' -> () From: ( | {
         'ModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaParser'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'compUnitNodeSum' -> () From: ( | {
         'ModuleInfo: Module: compUnitNodeSum InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'compUnitNodeSum' -> () From: ( | {
         'ModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'compUnitNodeSum' -> () From: ( | {
         'ModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'compUnitNodeSum' -> () From: ( | {
         'ModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'compUnitNodeSum' -> () From: ( | {
         'ModuleInfo: Module: compUnitNodeSum InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules compUnitNodeSum postFileIn
