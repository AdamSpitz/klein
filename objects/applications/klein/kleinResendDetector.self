 '$Revision: 30.14 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'resendDetector' -> () From: ( | {
         'ModuleInfo: Module: kleinResendDetector InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'resendDetector' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda objectMapper1 parent resendDetector parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'resendDetector' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinResendDetector InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForMethod: m = ( |
            | 
            resend.initializeForMethod: m.
            resends: list copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'resendDetector' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinResendDetector InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'resendDetector' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinResendDetector InitialContents: FollowSlot\x7fVisibility: public'
        
         resendsInSlot: aSlot = ( |
             i.
            | 
            i:  copyForMethod: aSlot contents.
            i interpretMethod.
            i resends).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'resendDetector' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinResendDetector InitialContents: FollowSlot\x7fVisibility: private'
        
         send: bc = ( |
            | 
            bc isResend ifTrue: [resends add: bc]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'objectMapper1' -> 'parent' -> 'resendDetector' -> () From: ( | {
         'ModuleInfo: Module: kleinResendDetector InitialContents: InitializeToExpression: (nil)'
        
         resends.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinResendDetector InitialContents: FollowSlot'
        
         kleinResendDetector = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinResendDetector' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinResendDetector' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinResendDetector.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinResendDetector' -> () From: ( | {
         'ModuleInfo: Module: kleinResendDetector InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinResendDetector' -> () From: ( | {
         'ModuleInfo: Module: kleinResendDetector InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinResendDetector' -> () From: ( | {
         'ModuleInfo: Module: kleinResendDetector InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinResendDetector' -> () From: ( | {
         'ModuleInfo: Module: kleinResendDetector InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinResendDetector' -> () From: ( | {
         'ModuleInfo: Module: kleinResendDetector InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.14 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinResendDetector' -> () From: ( | {
         'ModuleInfo: Module: kleinResendDetector InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinResendDetector postFileIn
