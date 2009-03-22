 '$Revision: 30.3 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot'
        
         yodaMorphs = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaMorphs' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaMorphs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaMorphs.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaMorphs' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaMorphs' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaMorphs' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaMorphs' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
             resend.postFileIn.
            (yoda foreignProgramMorph & yoda foreignHostMorph) asVector
             do: [|:m|
              m initializePrototype.
              worldMorph addBackgroundMenuContributor: m.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaMorphs' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.3 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaMorphs' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: morphs\x7fModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignHostMorph = bootstrap define: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignHostMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals kleinAndYoda foreignHostMorph copyRemoveAllMorphs ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignHostMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda foreignHostMorph.

CopyDowns:
globals kleinAndYoda foreignHostMorph. copyRemoveAllMorphs 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignHostMorph' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda foreignHostMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         application = ( |
            | 
            'yoda').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            yoda).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignHostMorph' -> () From: ( | {
         'Category: filing out\x7fModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            yoda foreignHostMorph).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: morphs\x7fModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProgramMorph = bootstrap define: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProgramMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals kleinAndYoda foreignProgramMorph copyRemoveAllMorphs ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProgramMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda foreignProgramMorph.

CopyDowns:
globals kleinAndYoda foreignProgramMorph. copyRemoveAllMorphs 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProgramMorph' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda foreignProgramMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         programNameSpace = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProgramMorph' -> 'parent' -> 'programNameSpace' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda foreignProgramMorph parent programNameSpace.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            yoda).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'foreignProgramMorph' -> () From: ( | {
         'Category: filing out\x7fModuleInfo: Module: yodaMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            yoda foreignProgramMorph).
        } | ) 



 '-- Side effects'

 globals modules yodaMorphs postFileIn
