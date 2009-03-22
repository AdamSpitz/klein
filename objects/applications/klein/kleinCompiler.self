 '$Revision: 30.17 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler InitialContents: FollowSlot'
        
         kleinCompiler = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinCompiler.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.17 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'kleinCompiler0
kleinCompiler1
kleinCompiler2
kleinNMethod
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'kleinCompiler0' From: 'applications/klein'
 bootstrap read: 'kleinCompiler1' From: 'applications/klein'
 bootstrap read: 'kleinCompiler2' From: 'applications/klein'
 bootstrap read: 'kleinNMethod' From: 'applications/klein'



 '-- Side effects'

 globals modules kleinCompiler postFileIn
