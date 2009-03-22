 '$Revision: 30.6 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinJava InitialContents: FollowSlot'
        
         kleinJava = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinJava' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinJava' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinJava.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinJava' -> () From: ( | {
         'ModuleInfo: Module: kleinJava InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinJava' -> () From: ( | {
         'ModuleInfo: Module: kleinJava InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinJava' -> () From: ( | {
         'ModuleInfo: Module: kleinJava InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinJava' -> () From: ( | {
         'ModuleInfo: Module: kleinJava InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinJava' -> () From: ( | {
         'ModuleInfo: Module: kleinJava InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.6 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinJava' -> () From: ( | {
         'ModuleInfo: Module: kleinJava InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'allJavaParser
javaUI2
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'shell' -> () From: ( | {
         'ModuleInfo: Module: kleinJava InitialContents: InitializeToExpression: (nil)'
        
         x.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'allJavaParser' From: 'applications/klein/javaParser'
 bootstrap read: 'javaUI2' From: 'applications/klein/javaUI2'



 '-- Side effects'

 globals modules kleinJava postFileIn
