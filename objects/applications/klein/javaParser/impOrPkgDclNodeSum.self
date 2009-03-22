 '$Revision: 30.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: node summaries for outliner\x7fModuleInfo: Module: impOrPkgDclNodeSum InitialContents: FollowSlot'
        
         importOrPackageDcl = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'importOrPackageDcl' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser abstractNodeSummary copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'importOrPackageDcl' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser importOrPackageDcl.

CopyDowns:
globals javaParser abstractNodeSummary. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'importOrPackageDcl' -> () From: ( | {
         'ModuleInfo: Module: impOrPkgDclNodeSum InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'importOrPackageDcl' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser importOrPackageDcl parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'importOrPackageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: impOrPkgDclNodeSum InitialContents: FollowSlot'
        
         comment = ( |
            | postComment).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'importOrPackageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: impOrPkgDclNodeSum InitialContents: FollowSlot'
        
         contents = ( |
            | name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'importOrPackageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: impOrPkgDclNodeSum InitialContents: FollowSlot'
        
         longKey = ( |
            | source).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'importOrPackageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: impOrPkgDclNodeSum InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'abstractNodeSummary' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: impOrPkgDclNodeSum InitialContents: FollowSlot'
        
         impOrPkgDclNodeSum = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'impOrPkgDclNodeSum' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'impOrPkgDclNodeSum' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules impOrPkgDclNodeSum.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'impOrPkgDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: impOrPkgDclNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaParser'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'impOrPkgDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: impOrPkgDclNodeSum InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'impOrPkgDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: impOrPkgDclNodeSum InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'impOrPkgDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: impOrPkgDclNodeSum InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'impOrPkgDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: impOrPkgDclNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'impOrPkgDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: impOrPkgDclNodeSum InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'importDclNodeSum
packageDclNodeSum
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'importDclNodeSum' From: 'applications/klein/javaParser'
 bootstrap read: 'packageDclNodeSum' From: 'applications/klein/javaParser'



 '-- Side effects'

 globals modules impOrPkgDclNodeSum postFileIn
