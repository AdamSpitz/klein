 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: activations\x7fModuleInfo: Module: vmKitActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeActivation: a = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: activations\x7fModuleInfo: Module: vmKitActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         isActivationObsolete: a = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: activations\x7fModuleInfo: Module: vmKitActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeActivation: a = ( |
            | 
            a initializeLocalActivation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: activations\x7fModuleInfo: Module: vmKitActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         isActivationObsolete: a = ( |
            | 
            a isLocalActivationObsolete).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: activations\x7fModuleInfo: Module: vmKitActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         initializeActivation: a = ( |
            | 
            a initializeRemoteActivation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: activations\x7fModuleInfo: Module: vmKitActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         isActivationObsolete: a = ( |
            | 
            a isRemoteActivationObsolete).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitActivations InitialContents: FollowSlot'
        
         vmKitActivations = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitActivations' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitActivations' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitActivations.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitActivations' -> () From: ( | {
         'ModuleInfo: Module: vmKitActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitActivations' -> () From: ( | {
         'ModuleInfo: Module: vmKitActivations InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitActivations' -> () From: ( | {
         'ModuleInfo: Module: vmKitActivations InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitActivations' -> () From: ( | {
         'ModuleInfo: Module: vmKitActivations InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitActivations' -> () From: ( | {
         'ModuleInfo: Module: vmKitActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitActivations' -> () From: ( | {
         'ModuleInfo: Module: vmKitActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'activation' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: constructing prototypes\x7fModuleInfo: Module: vmKitActivations InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDownParentForVMKitMirror = ( |
            | 
            reflectionPrimitives vmKit mirrors copyDownParentForActivationPrototypes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'activation' -> 'liveOnes' -> () From: ( | {
         'Category: klein and yoda\x7fComment: call only from slot\x7fModuleInfo: Module: vmKitActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         primitiveIsAssignmentAt: n IfFail: fb = ( |
            | 
            reflectionPrimitives forActivationMirror: self IsAssignmentAt: n asString canonicalize IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'activation' -> () From: ( | {
         'Category: klein and yoda\x7fCategory: intercepting primitives to check for death and switch parent\x7fModuleInfo: Module: vmKitActivations InitialContents: FollowSlot\x7fVisibility: private'
        
         primitiveIsAssignmentAt: n IfFail: fb = ( |
            | 
            ifDead: [^ false].
            resend.primitiveIsAssignmentAt: n IfFail: fb).
        } | ) 



 '-- Side effects'

 globals modules vmKitActivations postFileIn
