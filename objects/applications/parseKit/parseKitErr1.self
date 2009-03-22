 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr1 InitialContents: FollowSlot'
        
         parseKitErr1 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr1' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules parseKitErr1.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr1' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr1 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/parseKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr1' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr1 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr1' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr1 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr1' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr1 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr1' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr1 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitErr1' -> () From: ( | {
         'ModuleInfo: Module: parseKitErr1 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: lexing\x7fCategory: errors\x7fModuleInfo: Module: parseKitErr1 InitialContents: FollowSlot\x7fVisibility: public'
        
         noError = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'noError' -> () ToBe: bootstrap addSlotsTo: (
             globals parseKit syntaxError copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'noError' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit noError.

CopyDowns:
globals parseKit syntaxError. copy

\x7fIsComplete: '.
            | ) .
        } | ) 



 '-- Side effects'

 globals modules parseKitErr1 postFileIn
