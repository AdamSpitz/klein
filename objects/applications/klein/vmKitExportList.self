 '$Revision: 30.5 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: building / exporting programs\x7fComment: Currently unused, may need in the future.\x7fModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: public'
        
         exportList = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             bootstrap remove: 'safety' From:
             globals set copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportList.

CopyDowns:
globals set. copy 
SlotsToOmit: parent prototype safety.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportList parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: public'
        
         collectionName = 'vmKitExportList'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'hashTableSet' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: private'
        
         safety* = bootstrap stub -> 'traits' -> 'abstractSetOrDictionary' -> 'unsafeMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: public'
        
         storeStringForUnkeyedCollectorIfFail: fb = ( |
             ss.
            | 
            "my elements are mirrors on well-known objects"
            ss: copyMappedBy: [|:e| 
                '(reflect: ', e name, ')'.
            ].
            ss reduceWith: [|:e1. :e2| e1, '\n\t& ', e2]
              IfSingleton: [|:e1| ^ 'collector copyFirst: ', e1]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> () From: ( | {
         'Category: unit tests\x7fModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: private'
        
         unitTests = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> 'unitTests' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportList parent unitTests.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> 'unitTests' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> 'unitTests' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportList parent unitTests parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> 'unitTests' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot'
        
         assert: b = ( |
            | 
            b value ifFalse: [error: 'assertion failure in exportList unitTests']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> 'unitTests' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot'
        
         assert: a Equals: b = ( |
            | 
            assert: [a = b]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> 'unitTests' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> 'unitTests' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            testRecreatingFromStoreString.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> 'parent' -> 'unitTests' -> 'parent' -> () From: ( | {
         'Category: transforming\x7fModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: private'
        
         testRecreatingFromStoreString = ( |
             original.
             recreated.
             ss.
            | 
            original: ( (reflect: lobby)
                      & (reflect: kleinAndYoda)
                      & (reflect: traits vector)) asVmKitExportList.
            ss: original storeString.
            recreated: ss eval.
            assert: original Equals: recreated.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportList' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: private'
        
         prototype = ( |
            | 
            kleinAndYoda exportList).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot'
        
         vmKitExportList = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExportList' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExportList' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitExportList.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExportList' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExportList' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExportList' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExportList' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExportList' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.5 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExportList' -> () From: ( | {
         'ModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'collector' -> () From: ( | {
         'Category: conversions\x7fModuleInfo: Module: vmKitExportList InitialContents: FollowSlot\x7fVisibility: public'
        
         asVmKitExportList = ( |
             r.
            | 
            prependIntoGrowable: kleinAndYoda exportList  By: [|:r. :e| r add:      e element]).
        } | ) 



 '-- Side effects'

 globals modules vmKitExportList postFileIn
