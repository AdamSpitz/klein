 '$Revision: 30.4 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaVM InitialContents: FollowSlot'
        
         yodaVM = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVM' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVM' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaVM.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVM' -> () From: ( | {
         'ModuleInfo: Module: yodaVM InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVM' -> () From: ( | {
         'ModuleInfo: Module: yodaVM InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVM' -> () From: ( | {
         'ModuleInfo: Module: yodaVM InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVM' -> () From: ( | {
         'ModuleInfo: Module: yodaVM InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVM' -> () From: ( | {
         'ModuleInfo: Module: yodaVM InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.4 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaVM' -> () From: ( | {
         'ModuleInfo: Module: yodaVM InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'yodaSmallImage
yodaVMTests
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> () From: ( | {
         'Category: virtual machines\x7fModuleInfo: Module: yodaVM InitialContents: FollowSlot\x7fVisibility: public'
        
         virtualMachines = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda virtualMachines.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'virtualMachines' -> () From: ( | {
         'ModuleInfo: Module: yodaVM InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'virtualMachines' -> ().
        } | ) 



 '-- Sub parts'

 bootstrap read: 'yodaSmallImage' From: 'applications/klein'
 bootstrap read: 'yodaVMTests' From: 'applications/klein'



 '-- Side effects'

 globals modules yodaVM postFileIn
