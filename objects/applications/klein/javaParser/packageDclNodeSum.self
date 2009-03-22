 '$Revision: 30.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> () From: ( | {
         'Category: node summaries for outliner\x7fModuleInfo: Module: packageDclNodeSum InitialContents: FollowSlot'
        
         packageDcl = bootstrap define: bootstrap stub -> 'globals' -> 'javaParser' -> 'packageDcl' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaParser importOrPackageDcl copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'packageDcl' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser packageDcl.

CopyDowns:
globals javaParser importOrPackageDcl. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'packageDcl' -> () From: ( | {
         'ModuleInfo: Module: packageDclNodeSum InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaParser' -> 'packageDcl' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaParser packageDcl parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'packageDcl' -> 'parent' -> () From: ( | {
         'Comment: Put package declaration at top level.\x7fModuleInfo: Module: packageDclNodeSum InitialContents: FollowSlot'
        
         category = ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'packageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: packageDclNodeSum InitialContents: FollowSlot'
        
         isPackageDcl = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaParser' -> 'packageDcl' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: packageDclNodeSum InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaParser' -> 'importOrPackageDcl' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: packageDclNodeSum InitialContents: FollowSlot'
        
         packageDclNodeSum = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'packageDclNodeSum' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'packageDclNodeSum' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules packageDclNodeSum.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'packageDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: packageDclNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaParser'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'packageDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: packageDclNodeSum InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'packageDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: packageDclNodeSum InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'packageDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: packageDclNodeSum InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'packageDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: packageDclNodeSum InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'packageDclNodeSum' -> () From: ( | {
         'ModuleInfo: Module: packageDclNodeSum InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules packageDclNodeSum postFileIn
