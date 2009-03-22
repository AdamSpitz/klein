 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: node summaries for outliner\x7fModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         classOrInterface = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser abstractNodeSummary copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser classOrInterface.

CopyDowns:
globals javaParser abstractNodeSummary. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser classOrInterface parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         = x = ( |
            | name = x name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         category = ( |
            | 
            hasParseTree ifFalse: [^ ''].
            name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         classOrInterface = ( |
            | 
            hasParseTree && [ parseTree classOrInterface ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         hash = ( |
            | name hash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         isClass = ( |
            | classOrInterface = 'class').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         isClassOrInterface = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         isInterface = ( |
            | isClass not).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         name = ( |
            | 
            parseTree nameToken source).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         nameAndTypes = ( |
             m.
            | 
            m: parseTree hasModifiers
              ifTrue: [parseTree modifiers source, ' ']
               False: ''.
            [todo unimplemented]. "extends? implements?"
            parseTree classOrInterfaceToken source, ' ',
            name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'classOrInterface' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         classOrIntNodeSum = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'classOrIntNodeSum' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'classOrIntNodeSum' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules classOrIntNodeSum.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'classOrIntNodeSum' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaParser'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'classOrIntNodeSum' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'classOrIntNodeSum' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'classOrIntNodeSum' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'classOrIntNodeSum' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'classOrIntNodeSum' -> () From: ( | {
         'ModuleInfo: Module: classOrIntNodeSum InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'classNodeSum
intNodeSum
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'classNodeSum' From: 'applications/klein/javaParser'
 bootstrap read: 'intNodeSum' From: 'applications/klein/javaParser'



 '-- Side effects'

 globals modules classOrIntNodeSum postFileIn
