 '$Revision: 30.4 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fCategory: maps\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         slotType = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( |
             {} = 'Comment: slotType describes each slot\'s properties (in the map)\x7fModuleInfo: Creator: globals kleinAndYoda slotType.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: transforming\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         asArgumentSlotType: x = ( |
            | 
            slotTypeField setValueOfWord: x To: slotTypeField argumentSlotValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: transforming\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         asAssignmentSlotType: x = ( |
            | 
            slotTypeField setValueOfWord: x To: slotTypeField assignmentSlotValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: transforming\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         asMapSlotType: x = ( |
            | 
            slotTypeField setValueOfWord: x To: slotTypeField mapSlotValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: transforming\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         asObjectSlotType: x = ( |
            | 
            slotTypeField setValueOfWord: x To: slotTypeField objectSlotValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: from slot\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         fromSlot: s = ( |
             r <- 0.
            | 
            fieldsDo: [|:f| r: r || (f wordForValue: f valueForObject: s)].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         isArgumentSlot: x = ( |
            | 
            (slotTypeField valueOfWord: x) = slotTypeField argumentSlotValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignable: x = ( |
            | 
            isAssignableField isValueOfWordTrue: x).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         isAssignableField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'isAssignableField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda slotType isAssignableField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'isAssignableField' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         booleanValueForObject: aSlot = ( |
            | 
            aSlot isAssignable).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'isAssignableField' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         isParentField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'isParentField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda slotType isParentField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'isAssignableField' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'isParentField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         isAssignmentSlot: x = ( |
            | 
            (slotTypeField valueOfWord: x) = slotTypeField assignmentSlotValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         isMapSlot: x = ( |
            | 
            (slotTypeField valueOfWord: x) = slotTypeField mapSlotValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         isObjectSlot: x = ( |
            | 
            (slotTypeField valueOfWord: x) = slotTypeField objectSlotValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         isParent: x = ( |
            | 
            isParentField isValueOfWordTrue: x).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'isParentField' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         booleanValueForObject: aSlot = ( |
            | 
            aSlot isParent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'isParentField' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         slotTypeField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'slotTypeField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda slotType slotTypeField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'isParentField' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'slotTypeField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         lastField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'isAssignableField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'slotTypeField' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         argumentSlotValue = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'slotTypeField' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         assignmentSlotValue = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'slotTypeField' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         mapSlotValue = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'slotTypeField' -> () From: ( | {
         'Category: values\x7fModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         objectSlotValue = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'slotTypeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'slotTypeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> 'noPrecedingField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'slotTypeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForObject: aSlot = ( |
            | 
            case
              if: [aSlot isArgument  ]  Then: [  argumentSlotValue]
              If: [aSlot isAssignable]  Then: [    objectSlotValue]
              If: [aSlot isAssignment]  Then: [assignmentSlotValue]
                                        Else: [       mapSlotValue]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'slotTypeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         valueSlotNamesDo: blk = ( |
            | 
            [objectSlotValue. mapSlotValue. argumentSlotValue].
            blk value: 'objectSlotValue'.
            blk value: 'mapSlotValue'.
            blk value: 'argumentSlotValue'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> 'slotTypeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         width = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'slotType' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         wordLayoutMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot'
        
         vmKitSlotType = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSlotType' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSlotType' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitSlotType.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSlotType' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSlotType' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSlotType' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSlotType' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSlotType' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.4 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitSlotType' -> () From: ( | {
         'ModuleInfo: Module: vmKitSlotType InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitSlotType postFileIn
