 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets3 InitialContents: FollowSlot'
        
         parseKitCharSets3 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets3' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets3' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules parseKitCharSets3.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets3' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets3 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/parseKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets3' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets3 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets3' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets3 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets3' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets3 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets3' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets3 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitCharSets3' -> () From: ( | {
         'ModuleInfo: Module: parseKitCharSets3 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'characterSets' -> () From: ( | {
         'Category: language-neutral constants\x7fModuleInfo: Module: parseKitCharSets3 InitialContents: FollowSlot\x7fVisibility: public'
        
         letters = bootstrap setObjectAnnotationOf: ( ('a' asCharacterSet  =>  'z') + ('A' asCharacterSet  =>  'Z')) From: ( |
             {} = 'Comment: A character set defined by the union of two sets.\x7fModuleInfo: Creator: globals parseKit characterSets letters.
\x7fIsComplete: '.
            | ) .
        } | ) 



 '-- Side effects'

 globals modules parseKitCharSets3 postFileIn
