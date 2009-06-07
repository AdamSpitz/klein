 '$Revision:$'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fCategory: maps\x7fModuleInfo: Module: vmKitNMethodCache InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodCache = bootstrap define: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'nmethodCache') -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals vector copy ) From: bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'nmethodCache') -> () From: ( |
             {} = 'Comment: We used to use regular vectors for these, but
this way they show up separately when we do a
breakdown of the objects in the image.\x7fModuleInfo: Creator: globals kleinAndYoda nmethodCache.

CopyDowns:
globals vector. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: special fields\x7fModuleInfo: Module: vmKitNMethodCache InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodCacheProto = ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'nmethodCache') -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'nmethodCache') -> () From: ( | {
         'ModuleInfo: Module: vmKitNMethodCache InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'nmethodCache') -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda nmethodCache parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'kleinAndYoda') \/-> 'nmethodCache') -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitNMethodCache InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'vector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitNMethodCache InitialContents: FollowSlot'
        
         vmKitNMethodCache = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitNMethodCache' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitNMethodCache' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitNMethodCache.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitNMethodCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitNMethodCache InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitNMethodCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitNMethodCache InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitNMethodCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitNMethodCache InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitNMethodCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitNMethodCache InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitNMethodCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitNMethodCache InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision:$'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitNMethodCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitNMethodCache InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitNMethodCache postFileIn
