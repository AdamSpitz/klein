 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'Category: models\x7fCategory: slots\x7fModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot'
        
         slotModel = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'slotModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals javaUI2 abstractSlotModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'slotModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 slotModel.

CopyDowns:
globals javaUI2 abstractSlotModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'slotModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'slotModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 slotModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'slotModel' -> 'parent' -> () From: ( | {
         'Category: locals\x7fModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot\x7fVisibility: private'
        
         buildLocals = ( |
            | 
            "no locals on regular slots--yet.
            would need to get them to update"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'slotModel' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot'
        
         isJavaSlotModel = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'slotModel' -> 'parent' -> () From: ( | {
         'Category: accessing the slot\x7fModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot'
        
         method = ( |
            | 
            referrent myMethodBody).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'slotModel' -> 'parent' -> () From: ( | {
         'Category: accessing the slot\x7fModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot'
        
         methodString = ( |
            | 
            methodTextWithoutSlots asString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'slotModel' -> 'parent' -> () From: ( | {
         'Category: accessing the slot\x7fModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot\x7fVisibility: private'
        
         methodTextWithoutSlots = ( |
            | 
            methodText copyForMethod: method).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'slotModel' -> 'parent' -> () From: ( | {
         'Category: contents\x7fModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot\x7fVisibility: private'
        
         oneLinerContentsString = ( |
             elipsis = '...'.
            | 
            case
             if: [hasMethod] Then: [ "test first for top-level activations"
             | m. src. s |
            "hack because I don't want to trace why string starts with newline"
            src: method source.
            src isEmpty ifTrue: [^ ''].
            ('\n\r' includes: src first) ifTrue: [src: src copyWithoutFirst].
             (src includes: '\n') ifTrue: [^elipsis]. "optimize long methods"
             (src includes: '\r') ifTrue: [^elipsis]. "optimize long methods"
             m: methodTextWithoutSlots.
             (m lines size > 1) || [s: m asString. s size > contentsLengthLimit]
               ifTrue: [^elipsis].
             s
            ] If: [slot exists ] Then: [ slot value copyAtMostWithEllipsis: contentsLengthLimit]
              Else: 'non-existent').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'slotModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractSlotModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'slotModel' -> 'parent' -> () From: ( | {
         'Category: accessing the slot\x7fModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot'
        
         receiver = ( |
            | slot holder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'slotModel' -> 'parent' -> () From: ( | {
         'Category: title\x7fModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot'
        
         titleFontSpec = bootstrap setObjectAnnotationOf: ( fontSpec copyName: 'helvetica' Size: 12 Style: '') From: ( |
             {} = 'Comment: I am an abstract, portable, description of a font.
I am also immutable.\x7fModuleInfo: Creator: globals javaUI2 slotModel parent titleFontSpec.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot'
        
         javaUI2SlotModel = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2SlotModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'comment' From:
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2SlotModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaUI2SlotModel.

CopyDowns:
globals modules init. copy 
SlotsToOmit: comment directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2SlotModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaUI2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2SlotModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2SlotModel InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2SlotModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2SlotModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2SlotModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2SlotModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2SlotModel InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules javaUI2SlotModel postFileIn
