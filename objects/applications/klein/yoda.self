 '$Revision: 30.4 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yoda InitialContents: FollowSlot'
        
         yoda = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yoda' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yoda' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yoda.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yoda' -> () From: ( | {
         'ModuleInfo: Module: yoda InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yoda' -> () From: ( | {
         'ModuleInfo: Module: yoda InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yoda' -> () From: ( | {
         'ModuleInfo: Module: yoda InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yoda' -> () From: ( | {
         'ModuleInfo: Module: yoda InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yoda' -> () From: ( | {
         'ModuleInfo: Module: yoda InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.4 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yoda' -> () From: ( | {
         'ModuleInfo: Module: yoda InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'yodaVM
yodaDB
yodaObjects
yodaTests
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: applications\x7fCategory: VM Kits\x7fModuleInfo: Module: yoda InitialContents: FollowSlot\x7fVisibility: public'
        
         yoda = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> () From: ( | {
         'ModuleInfo: Module: yoda InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> ().
        } | ) 



 '-- Sub parts'

 bootstrap read: 'yodaVM' From: 'applications/klein'
 bootstrap read: 'yodaDB' From: 'applications/klein'
 bootstrap read: 'yodaObjects' From: 'applications/klein'
 bootstrap read: 'yodaTests' From: 'applications/klein'



 '-- Side effects'

 globals modules yoda postFileIn
