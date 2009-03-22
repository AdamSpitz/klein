 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'defaultBehavior' -> () From: ( | {
         'Category: doubleDispatching\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         equalsCompilationUnit: cu = ( |
            | false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: node summaries for outliner\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         abstractNodeSummary = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser abstractNodeSummary.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: InitializeToExpression: (nil)'
        
         myParseTree.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser abstractNodeSummary parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         < x = ( |
            | name < x name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         = x = ( |
            | 
            hasParseTree && [x hasParseTree]
             ifFalse: [^ false].
             parseTree = x parseTree).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         categories = ( |
            | vector copyAddFirst: category).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         categoriesString = ( |
            | category).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         categoryList = ( |
            | 
            vector copyAddFirst: category).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         comment = ( |
            | preComment).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         comment: c = ( |
            | 
            [todo unimplementd]. error: 'unimplemented').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         compare: x IfLess: lb Equal: eb Greater: gb = ( |
            | 
            parseTree compare: x parseTree IfLess: lb Equal: eb Greater: gb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         copyParseTree: n = ( |
            | copy myParseTree: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         exists = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: parse tree\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot\x7fVisibility: public'
        
         hasParseTree = ( |
            | nil != parseTree).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: comparing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         hash = ( |
            | 
            hasParseTree ifFalse: [^ 0].
            parseTree hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         isAssignable = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         isAssignment = ( |
            | 
            [todo cleanup]. "really needed?"
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         isClass = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         isClassOrInterface = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         isCopiedDown = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         isImportDcl = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         isInterface = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         isMethod = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         isPackageDcl = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         isReflecteeMethod = ( |
            | isMethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: naming\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         longKey = ( |
            | 
            nameAndTypes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         moduleSummaryString = ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: naming\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         name = ( |
            | 
            hasParseTree ifFalse: [^ 'unparsed'].
            parseTree name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: naming\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         nameSize: n = ( |
            | name copyAtMostWithLeadingEllipsis: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         oneOfEachFakeSlot = ( |
            | 
            vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: parse tree\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot\x7fVisibility: public'
        
         parseTree = ( |
            | myParseTree).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: parse tree\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot\x7fVisibility: public'
        
         parseTreeIfFail: fb = ( |
            | 
            nil = myParseTree ifFalse: [^myParseTree].
            parseSourceIfFail: [|:e| ^ fb value: e].
            myParseTree).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         postComment = ( |
             pc.
             r <- ''.
            | 
            pc: parseTree postCommentsOfLastToken.
            pc isEmpty ifTrue: [^ ''].
            pc do: [|:commentToken|
              r: r, ' ', commentToken contents shrinkwrapped.
            ].
            r copyWithoutFirst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: comments\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         preComment = ( |
             pc.
             r <- ''.
            | 
            pc: parseTree preCommentsOfFirstToken.
            pc isEmpty ifTrue: [^ ''].
            pc do: [|:commentToken|
              r: r, '\n', commentToken contents shrinkwrapped.
            ].
            r copyWithoutFirst).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'Category: slots\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot\x7fVisibility: public'
        
         slots = ( |
            | 
            hasParseTree
              ifFalse: vector
                True: [ parseTree slotsForOutliner ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         source = ( |
            | 
            hasParseTree ifFalse: [mySource]
                            True: [ parseTree source ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         sourceWithoutCurlies = ( |
            | 
            hasParseTree ifFalse: [^'...'].
            parseTree subtreeWithoutCurlies source).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         value = ( |
            | contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         visibility = bootstrap stub -> 'globals' -> 'visibility' -> 'publicSlot' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: node summaries for outliner\x7fModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         methodBody = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodBody' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser abstractNodeSummary copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodBody' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser methodBody.

CopyDowns:
globals javaParser abstractNodeSummary. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodBody' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodBody' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser methodBody parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodBody' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         mustBeDisassembled = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodBody' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodBody' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot\x7fVisibility: public'
        
         source = ( |
            | sourceWithoutCurlies).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         javaNodeSummaries = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaNodeSummaries' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaNodeSummaries' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaNodeSummaries.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaNodeSummaries' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaParser'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaNodeSummaries' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaNodeSummaries' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaNodeSummaries' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaNodeSummaries' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaNodeSummaries' -> () From: ( | {
         'ModuleInfo: Module: javaNodeSummaries InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'classOrIntNodeSum
compUnitNodeSum
impOrPkgDclNodeSum
methOrConNodeSum
varDclNodeSum
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'classOrIntNodeSum' From: 'applications/klein/javaParser'
 bootstrap read: 'compUnitNodeSum' From: 'applications/klein/javaParser'
 bootstrap read: 'impOrPkgDclNodeSum' From: 'applications/klein/javaParser'
 bootstrap read: 'methOrConNodeSum' From: 'applications/klein/javaParser'
 bootstrap read: 'varDclNodeSum' From: 'applications/klein/javaParser'



 '-- Side effects'

 globals modules javaNodeSummaries postFileIn
