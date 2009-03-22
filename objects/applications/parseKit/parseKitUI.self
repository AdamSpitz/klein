 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         parseKitUI = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitUI' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitUI' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules parseKitUI.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitUI' -> () From: ( | {
         'ModuleInfo: Module: parseKitUI InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/parseKit'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitUI' -> () From: ( | {
         'ModuleInfo: Module: parseKitUI InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitUI' -> () From: ( | {
         'ModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitUI' -> () From: ( | {
         'ModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitUI' -> () From: ( | {
         'ModuleInfo: Module: parseKitUI InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'parseKitUI' -> () From: ( | {
         'ModuleInfo: Module: parseKitUI InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: parsing\x7fCategory: parse node outliners\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot\x7fVisibility: public'
        
         parseNodeOutliner = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals nonpluggableOutliner copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodeOutliner.

CopyDowns:
globals nonpluggableOutliner. copy 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> () From: ( | {
         'Category: Parse node outliner\x7fModuleInfo: Module: parseKitUI InitialContents: InitializeToExpression: (parseKit parseNodes node copy)\x7fVisibility: private'
        
         myNode <- parseKit parseNodes node copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> () From: ( | {
         'ModuleInfo: Module: parseKitUI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodeOutliner parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         constructItems = ( |
            | 
            node doSubnodes: [|:s|
              addItem: parseKit parseNodeOutliner copyNode: s
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         copyNode: n = ( |
            | 
            ((prototype copy myNode: n) initialize 
              borderWidth: 1)
              frameStyle: flatStyle).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> 'parent' -> () From: ( | {
         'Category: expanding\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot\x7fVisibility: public'
        
         expand: evt = ( |
            | 
            resend.expand: evt.
            items morphCount = 0  ifTrue: [
              "if no subitems, don't want outliner to be bigger, so collapse it"
              collapse: evt.
              updateExpander.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         headerButtonContents = ( |
            | 
            (
              ('SOURCE' @ 'target showSource: event')
            & ('=' @ 'target sprout: event')
            ) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         initializeRoot = ( |
            | 
            borderWidth: 3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> 'parent' -> () From: ( | {
         'Category: basics\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         morphTypeName = 'parseNodeOutliner'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         node = ( |
            | myNode).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitUI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'nonpluggableOutliner' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> 'parent' -> () From: ( | {
         'Category: button operations\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         showSource: evt = ( |
            | 
            evt sourceHand attach:
              (textViewerMorph copyTitle: 'Source for: ', node shortPrintString
                                   Text: node source) open).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> 'parent' -> () From: ( | {
         'Category: button operations\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         sprout: evt = ( |
            | 
            "get outliner for my node"
            evt sourceHand attach: world outlinerForMirror: node asMirror.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         titleString = ( |
            | 
            node shortPrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> () From: ( | {
         'Category: filing out\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            parseKit parseNodeOutliner).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> () From: ( | {
         'Category: parsing\x7fCategory: parse node outliners\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot\x7fVisibility: public'
        
         parseNodeOuterOutliner = bootstrap define: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOuterOutliner' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals parseKit parseNodeOutliner copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOuterOutliner' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodeOuterOutliner.

CopyDowns:
globals parseKit parseNodeOutliner. copy 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOuterOutliner' -> () From: ( | {
         'ModuleInfo: Module: parseKitUI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOuterOutliner' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals parseKit parseNodeOuterOutliner parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOuterOutliner' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         copyNode: n = ( |
            | 
            (prototype copy myNode: n) initialize
              borderWidth: 3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOuterOutliner' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot\x7fVisibility: private'
        
         headerButtonContents = ( |
            | 
            resend.headerButtonContents,
            (   ('FLUSH' @ 'target flush: event' )
              & dismissButtonContents
            ) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOuterOutliner' -> 'parent' -> () From: ( | {
         'Category: basics\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot'
        
         morphTypeName = 'parseNodeOuterOutliner'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOuterOutliner' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: parseKitUI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOutliner' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'parseKit' -> 'parseNodeOuterOutliner' -> () From: ( | {
         'Category: filing out\x7fModuleInfo: Module: parseKitUI InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            parseKit parseNodeOuterOutliner).
        } | ) 



 '-- Side effects'

 globals modules parseKitUI postFileIn
