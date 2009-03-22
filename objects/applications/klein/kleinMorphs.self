 '$Revision: 30.64 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: morphs\x7fModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignHostMorph = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'foreignHostMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals kleinAndYoda foreignHostMorph copyRemoveAllMorphs ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignHostMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignHostMorph.

CopyDowns:
globals kleinAndYoda foreignHostMorph. copyRemoveAllMorphs 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignHostMorph' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignHostMorph' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignHostMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         application = ( |
            | 
            'klein').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignHostMorph' -> () From: ( | {
         'Category: filing out\x7fModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            klein foreignHostMorph).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: morphs\x7fModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProgramMorph = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProgramMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals kleinAndYoda foreignProgramMorph copyRemoveAllMorphs ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProgramMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignProgramMorph.

CopyDowns:
globals kleinAndYoda foreignProgramMorph. copyRemoveAllMorphs 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProgramMorph' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProgramMorph' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignProgramMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         programNameSpace = ( |
            | 
            platformName sendTo: platformNameSpace).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProgramMorph' -> () From: ( | {
         'Category: filing out\x7fModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            klein foreignProgramMorph).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> 'virtualMachineSlotSorter' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         element: a Precedes: b = ( |
            | 
            a contents reflectee menuOrder < b contents reflectee menuOrder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot'
        
         kleinMorphs = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinMorphs' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinMorphs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinMorphs.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinMorphs' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinMorphs' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinMorphs' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinMorphs' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
             resend.postFileIn.
            (klein foreignProgramMorph & klein foreignHostMorph) asVector
             do: [|:m|
              m initializePrototype.
              worldMorph addBackgroundMenuContributor: m.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinMorphs' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.64 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinMorphs' -> () From: ( | {
         'ModuleInfo: Module: kleinMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinMorphs postFileIn
