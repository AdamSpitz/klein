 '$Revision: 1.6 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot'
        
         yodaWellKnowns = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaWellKnowns' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaWellKnowns' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaWellKnowns.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaWellKnowns' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaWellKnowns' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaWellKnowns' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaWellKnowns' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaWellKnowns' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 1.6 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaWellKnowns' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> () From: ( | {
         'Comment: Corresponds to The in yoda\x7fModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         wellKnownObjects = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda layouts wellKnownObjects.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         ids = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda layouts wellKnownObjects ids.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         active_context = 28.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         blockActivationMap_mapType = 20.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         blockMap_mapType = 17.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         canonicalizedStringVector = 8.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         canonicalizedStrings = 7.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         edenSpace = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         false_object = 14.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         float_map = 10.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         int32_proto = 24.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         last = 31.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         lobby = 29.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         mapMap_mapType = 16.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrors_namespace = 30.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         newGeneration = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         nil_object = 21.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         objectLocator = 6.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         objectVectorMap_mapType = 18.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         oldGeneration = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         outerActivationMap_mapType = 19.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         process_proto = 25.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         restart_selector = 12.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         set_emptyMarker = 26.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         set_removedMarker = 27.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         size_string = 15.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         smi_map = 9.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         start_selector = 11.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         string_proto = 23.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         tenuredSpace = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         true_object = 13.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         universe = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         vector_proto = 22.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'ids' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         vm = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         offsetOfAddressForID: id = ( |
            | 
            (id * offsets length) + offsets address).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         offsetOfAddressForObjectLocator = ( |
            | 
            offsetOfAddressForID: ids objectLocator).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         offsetOfOopForActiveContext = ( |
            | 
            offsetOfOopForID: ids active_context).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         offsetOfOopForID: id = ( |
            | 
            (id * offsets length) + offsets oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         offsets = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'offsets' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda layouts wellKnownObjects offsets.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'offsets' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         address = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'offsets' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         length = 8.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> 'offsets' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot\x7fVisibility: public'
        
         oop = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'layouts' -> 'wellKnownObjects' -> () From: ( | {
         'ModuleInfo: Module: yodaWellKnowns InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> ().
        } | ) 



 '-- Side effects'

 globals modules yodaWellKnowns postFileIn
