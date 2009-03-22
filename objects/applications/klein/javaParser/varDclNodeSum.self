 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: node summaries for outliner\x7fModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot'
        
         varDclSlot = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser abstractNodeSummary copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser varDclSlot.

CopyDowns:
globals javaParser abstractNodeSummary. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: InitializeToExpression: (nil)'
        
         classOrInterface.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser varDclSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         category = ( |
            | 
            [todo unimplemented]. "impl real cats?"
            "'variables'"
            '').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot'
        
         contents = ( |
            | 
            [todo cleanup]. "a string or something else?"
            hasParseTree ifFalse: [^ ''].
            parseTree equalsAndInitializerIfPresent: [|:e. :i|
              i source
            ] IfAbsent: '').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         copyClassOrInterface: ci VarDclsStatement: vds Declarator: dcl = ( |
            | 
            ((copyParseTree: dcl)
            classOrInterface: ci)
            varDclsStatement: vds).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot'
        
         declarator = ( |
            | parseTree).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot'
        
         holder = ( |
            | classOrInterface).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot'
        
         isFake = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot'
        
         nameAndTypes = ( |
             m.
            | 
            m: varDclsStatement modifiers ifNil: '' IfNotNil: [|:ms| ms source].
            m isEmpty ifFalse: [m: m, ' '].
            (m & varDclsStatement resultType source &
            ' ' & parseTree name &
            (parseTree squares copyMappedBy: [|:s| s source])
            ) flatString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'varDclSlot' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: InitializeToExpression: (nil)'
        
         varDclsStatement.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot'
        
         varDclNodeSum = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'varDclNodeSum' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'varDclNodeSum' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules varDclNodeSum.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'varDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaParser'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'varDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'varDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'varDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'varDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'varDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: varDclNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules varDclNodeSum postFileIn
