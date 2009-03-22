 '$Revision: 30.4 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: building / exporting programs\x7fModuleInfo: Module: vmKitExport InitialContents: FollowSlot\x7fVisibility: public'
        
         exportPlatforms = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> () From: ( |
             {} = 'Comment: I am a namespace of
of per-host namespaces containing programs
that may be downloaded & debugged. -- dmu 1/02

The category names are significant here.
See klein foreignProgramMorph parent platformNames. -- jb 5/03\x7fModuleInfo: Creator: globals kleinAndYoda exportPlatforms.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> () From: ( | {
         'ModuleInfo: Module: vmKitExport InitialContents: FollowSlot\x7fVisibility: public'
        
         i386 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'i386' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms i386.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> () From: ( | {
         'ModuleInfo: Module: vmKitExport InitialContents: FollowSlot\x7fVisibility: public'
        
         ppc = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> () From: ( |
             {} = 'Comment: The category names are significant here.
See klein foreignProgramMorph parent programNamesForPlatform. -- jb 5/03\x7fModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitExport InitialContents: FollowSlot'
        
         vmKitExport = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExport' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExport' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitExport.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExport' -> () From: ( | {
         'ModuleInfo: Module: vmKitExport InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExport' -> () From: ( | {
         'ModuleInfo: Module: vmKitExport InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExport' -> () From: ( | {
         'ModuleInfo: Module: vmKitExport InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExport' -> () From: ( | {
         'ModuleInfo: Module: vmKitExport InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExport' -> () From: ( | {
         'ModuleInfo: Module: vmKitExport InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.4 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitExport' -> () From: ( | {
         'ModuleInfo: Module: vmKitExport InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitAbsPgmExporter
vmKitVMBuilder
vmKitExportList
vmKitAnnoCache
vmKitVMBuilder
vmKitIncrementalUpd
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitAbsPgmExporter' From: 'applications/klein'
 bootstrap read: 'vmKitVMBuilder' From: 'applications/klein'
 bootstrap read: 'vmKitExportList' From: 'applications/klein'
 bootstrap read: 'vmKitAnnoCache' From: 'applications/klein'
 bootstrap read: 'vmKitVMBuilder' From: 'applications/klein'
 bootstrap read: 'vmKitIncrementalUpd' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitExport postFileIn
