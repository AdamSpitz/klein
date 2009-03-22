 '$Revision: 30.3 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: universes\x7fModuleInfo: Module: vmKitVarHdrsUniv InitialContents: FollowSlot\x7fVisibility: public'
        
         compactMapForIndex: i InUniverse: u = ( |
            | 
            u localCompactMapForIndex: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: universes\x7fModuleInfo: Module: vmKitVarHdrsUniv InitialContents: FollowSlot\x7fVisibility: public'
        
         compactMapForIndex: i InUniverse: u = ( |
            | 
            u remoteCompactMapForIndex: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'Category: universe state\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsUniv InitialContents: InitializeToExpression: (vector copySize: kleinAndYoda layouts mark compactMapIndexField encodableNumberCount)\x7fVisibility: private'
        
         compactMapOops <- vector copySize: kleinAndYoda layouts mark compactMapIndexField encodableNumberCount.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> () From: ( | {
         'Category: universe state\x7fCategory: variable-size headers\x7fModuleInfo: Module: vmKitVarHdrsUniv InitialContents: InitializeToExpression: (vector copySize: kleinAndYoda layouts mark compactMapIndexField encodableNumberCount)\x7fVisibility: private'
        
         compactMaps <- vector copySize: kleinAndYoda layouts mark compactMapIndexField encodableNumberCount.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: compact maps\x7fModuleInfo: Module: vmKitVarHdrsUniv InitialContents: FollowSlot\x7fVisibility: public'
        
         compactMapForIndex: i = ( |
            | 
            theVM lens compactMapForIndex: i InUniverse: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: compact maps\x7fModuleInfo: Module: vmKitVarHdrsUniv InitialContents: FollowSlot\x7fVisibility: public'
        
         indexForCompactMap: aMap WithOop: aMapOop IfFail: fb = ( |
            | 
            compactMaps do: [|:m. :i|
              m == aMap ifTrue: [^ i].

              [todo variableSizeHeaders compactMaps]. "compactMapOops is a HACK. What's a better way to do this?"
              m ifNil: [compactMaps    at: i Put: aMap.
                        compactMapOops at: i Put: aMapOop.
                        ^ i].
            ].
            fb value: 'no more room for compact maps').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: compact maps\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitVarHdrsUniv InitialContents: FollowSlot\x7fVisibility: public'
        
         localCompactMapForIndex: i = ( |
            | 
            "Use the primitive so that we can use compact maps
             even in images without the 'at:' method mapped
             and compiled."
            compactMaps _At: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'universe' -> 'parent' -> () From: ( | {
         'Category: compact maps\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitVarHdrsUniv InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteCompactMapForIndex: i = ( |
            | 
            [todo variableSizeHeaders compactMaps]. "compactMapOops is a HACK. What's a better way to do this?"
            compactMapOops at: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsUniv InitialContents: FollowSlot'
        
         vmKitVarHdrsUniv = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsUniv' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsUniv' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitVarHdrsUniv.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsUniv' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsUniv InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsUniv' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsUniv InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsUniv' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsUniv InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsUniv' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsUniv InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsUniv' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsUniv InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.3 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitVarHdrsUniv' -> () From: ( | {
         'ModuleInfo: Module: vmKitVarHdrsUniv InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitVarHdrsUniv postFileIn
