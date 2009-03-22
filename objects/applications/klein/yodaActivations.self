 '$Revision: 30.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaActivations InitialContents: FollowSlot'
        
         yodaActivations = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaActivations' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaActivations' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaActivations.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaActivations' -> () From: ( | {
         'ModuleInfo: Module: yodaActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaActivations' -> () From: ( | {
         'ModuleInfo: Module: yodaActivations InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaActivations' -> () From: ( | {
         'ModuleInfo: Module: yodaActivations InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaActivations' -> () From: ( | {
         'ModuleInfo: Module: yodaActivations InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaActivations' -> () From: ( | {
         'ModuleInfo: Module: yodaActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaActivations' -> () From: ( | {
         'ModuleInfo: Module: yodaActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () From: ( | {
         'Category: activation state\x7fModuleInfo: Module: yodaActivations InitialContents: InitializeToExpression: (nil)'
        
         myProcess.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () From: ( | {
         'Category: activation state\x7fModuleInfo: Module: yodaActivations InitialContents: InitializeToExpression: (0)'
        
         number <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () From: ( | {
         'ModuleInfo: Module: yodaActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         yodaSpecific* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'yodaSpecific' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda mirrors copyDownParentForActivationPrototypes yodaSpecific.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'yodaSpecific' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: yodaActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         activationMapIfFail: fb = ( |
             mir.
            | 
            mir: methodMirrorIfFail: [|:e| ^ fb value: e].
            mir reflectionPrimitives importReflecteeActivationMapIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'yodaSpecific' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: yodaActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         methodMirrorIfFail: fb = ( |
            | 
            reflectionPrimitives forActivationMirror: self MethodMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> 'yodaSpecific' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: yodaActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         receiverOopIfFail: fb = ( |
            | 
            reflectionPrimitives forActivationMirror: self ReceiverOopIfFail: fb).
        } | ) 



 '-- Side effects'

 globals modules yodaActivations postFileIn
