 '$Revision: 30.3 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: public'
        
         tag = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'tag' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda tag.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'tag' -> () From: ( | {
         'Category: tag values\x7fModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: public'
        
         float = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'tag' -> () From: ( | {
         'ModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: public'
        
         howMany = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'tag' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: public'
        
         ifOop: oop IsFloat: fb IsSmi: sb IsMark: markb IsMem: mb = ( |
            | 
            ifTag: (tagOfOop: oop)
            IsFloat: fb
            IsSmi: sb
            IsMark: markb
            IsMem: mb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'tag' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: public'
        
         ifTag: t IsFloat: fb IsSmi: sb IsMark: markb IsMem: mb = ( |
            | 
            t = mem   ifTrue: [^    mb value].
            t = smi   ifTrue: [^    sb value].
            t = mark  ifTrue: [^ markb value].
            t = float ifTrue: [^    fb value].
            error: 'bad tag').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'tag' -> () From: ( | {
         'Category: tag values\x7fModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: public'
        
         mark = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'tag' -> () From: ( | {
         'ModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: public'
        
         mask = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'tag' -> () From: ( | {
         'Category: tag values\x7fModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: public'
        
         mem = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'tag' -> () From: ( | {
         'ModuleInfo: Module: vmKitOops InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'tag' -> () From: ( | {
         'ModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: public'
        
         size = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'tag' -> () From: ( | {
         'Category: tag values\x7fModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: public'
        
         smi = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'tag' -> () From: ( | {
         'ModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: public'
        
         tagOfOop: x = ( |
            | 
            x && mask).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitOops InitialContents: FollowSlot'
        
         vmKitOops = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitOops' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitOops' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitOops.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitOops' -> () From: ( | {
         'ModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitOops' -> () From: ( | {
         'ModuleInfo: Module: vmKitOops InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitOops' -> () From: ( | {
         'ModuleInfo: Module: vmKitOops InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitOops' -> () From: ( | {
         'ModuleInfo: Module: vmKitOops InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitOops' -> () From: ( | {
         'ModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.3 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitOops' -> () From: ( | {
         'ModuleInfo: Module: vmKitOops InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitOops postFileIn
