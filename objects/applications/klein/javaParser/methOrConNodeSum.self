 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: node summaries for outliner\x7fModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot'
        
         methodOrConstructorSlot = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser abstractNodeSummary copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser methodOrConstructorSlot.

CopyDowns:
globals javaParser abstractNodeSummary. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: InitializeToExpression: (nil)'
        
         classOrInterface.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser methodOrConstructorSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         category = ( |
            | 
            hasParseTree ifFalse: [^ ''].
            [todo unimplemented]. "impl real cats?"
            "parseTree methodOrConstructor, 's'"
            '').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         copyClassOrInterface: ci MethodOrConstructorDcl: mc = ( |
            | 
            (copyParseTree: mc)
            classOrInterface: ci).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot'
        
         holder = ( |
            | 
            javaParser class copyParseTree: classOrInterface).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot'
        
         isFake = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot'
        
         isMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         methodOrConstructorDcl = ( |
            | parseTree).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot'
        
         myMethodBody = ( |
            | 
            hasParseTree ifFalse: [todo unimplemented].
            javaParser methodBody copyParseTree: methodOrConstructorDcl body).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot'
        
         nameAndTypes = ( |
             m.
             t.
            | 
            hasParseTree ifFalse: [^ ''].
            m: parseTree hasModifiers
              ifTrue: [parseTree modifiers source, ' '] False: ''.
            t: parseTree throws source.
            t isEmpty ifFalse: [t: ' ', t].
            m, parseTree type source, ' ', parseTree name,
            parseTree formalParameterList source,
            t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'methodOrConstructorSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot'
        
         methOrConNodeSum = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'methOrConNodeSum' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'methOrConNodeSum' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules methOrConNodeSum.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'methOrConNodeSum' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaParser'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'methOrConNodeSum' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'methOrConNodeSum' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'methOrConNodeSum' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'methOrConNodeSum' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'methOrConNodeSum' -> () From: ( | {
         'ModuleInfo: Module: methOrConNodeSum InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules methOrConNodeSum postFileIn
