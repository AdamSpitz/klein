 '$Revision: 1.3 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fModuleInfo: Module: vmKitSelectorFinder InitialContents: FollowSlot\x7fVisibility: public'
        
         selectorFinder = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'selectorFinder' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals selectorFinder copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'selectorFinder' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda selectorFinder.

CopyDowns:
globals selectorFinder. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'selectorFinder' -> () From: ( | {
         'ModuleInfo: Module: vmKitSelectorFinder InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'selectorFinder' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda selectorFinder parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'selectorFinder' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitSelectorFinder InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'selectorFinder' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitSelectorFinder InitialContents: FollowSlot'
        
         vmKitSelectorFinder = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSelectorFinder' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSelectorFinder' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitSelectorFinder.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSelectorFinder' -> () From: ( | {
         'ModuleInfo: Module: vmKitSelectorFinder InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSelectorFinder' -> () From: ( | {
         'ModuleInfo: Module: vmKitSelectorFinder InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSelectorFinder' -> () From: ( | {
         'ModuleInfo: Module: vmKitSelectorFinder InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSelectorFinder' -> () From: ( | {
         'ModuleInfo: Module: vmKitSelectorFinder InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSelectorFinder' -> () From: ( | {
         'ModuleInfo: Module: vmKitSelectorFinder InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 1.3 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSelectorFinder' -> () From: ( | {
         'ModuleInfo: Module: vmKitSelectorFinder InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitSelectorFinder postFileIn
