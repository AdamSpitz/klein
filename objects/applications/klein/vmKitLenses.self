 '$Revision: 30.5 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fCategory: lenses -- double-dispatch\x7fModuleInfo: Module: vmKitLenses InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractLens = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( |
             {} = 'Comment: Lenses provide a consistent abstract view of the
structure of objects even as they change form
during mapping, exporting, execution, and
debugging of the Klein VM.

See Klein system documentation.\x7fModuleInfo: Creator: globals kleinAndYoda abstractLens.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLenses InitialContents: FollowSlot\x7fVisibility: private'
        
         layouts = ( |
            | 
            vmKit layouts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'ModuleInfo: Module: vmKitLenses InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLenses InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            [todo optimize]. "Just make children for klein and yoda so that
                              we can code this statically? -- Adam, 5/06"
            theVM vmKit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fCategory: lenses -- double-dispatch\x7fModuleInfo: Module: vmKitLenses InitialContents: FollowSlot\x7fVisibility: public'
        
         localObjectLens = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( |
             {} = 'Comment: This lens is used by the Klein VM ``over there\'\' during
the execution phase of the Klein life-cycle to manipulate
live objects from within the VM.

See Klein system documentation.\x7fModuleInfo: Creator: globals kleinAndYoda localObjectLens.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'ModuleInfo: Module: vmKitLenses InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fCategory: lenses -- double-dispatch\x7fModuleInfo: Module: vmKitLenses InitialContents: FollowSlot\x7fVisibility: public'
        
         memoryLens = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( |
             {} = 'Comment: This lens is used by the Self VM ``over here\'\' during the export
and remote debugging phases of the Klein life-cycle.  Objects
belonging to a Klein VM ``over there\'\' are accessed remotely
via a (possibly buffered) foreignProcessMemoryInterfaces using
memory read and write operations.

See Klein system documentation.\x7fModuleInfo: Creator: globals kleinAndYoda memoryLens.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'ModuleInfo: Module: vmKitLenses InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitLenses InitialContents: FollowSlot'
        
         vmKitLenses = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLenses' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLenses' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitLenses.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLenses' -> () From: ( | {
         'ModuleInfo: Module: vmKitLenses InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLenses' -> () From: ( | {
         'ModuleInfo: Module: vmKitLenses InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLenses' -> () From: ( | {
         'ModuleInfo: Module: vmKitLenses InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLenses' -> () From: ( | {
         'ModuleInfo: Module: vmKitLenses InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLenses' -> () From: ( | {
         'ModuleInfo: Module: vmKitLenses InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.5 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLenses' -> () From: ( | {
         'ModuleInfo: Module: vmKitLenses InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitLenses postFileIn
