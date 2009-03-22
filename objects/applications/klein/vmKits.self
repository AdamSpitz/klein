 '$Revision: 30.6 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: applications\x7fCategory: VM Kits\x7fModuleInfo: Module: vmKits InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinAndYoda = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( |
             {} = 'Comment: Copyright 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
California 95054, U.S.A. All rights reserved.

Sun Microsystems, Inc. has intellectual property rights relating to
technology embodied in the product that is described in this document.
In particular, and without limitation, these intellectual property rights
may include one or more of the U.S. patents listed at
http://www.sun.com/patents and one or more additional patents or pending
patent applications in the U.S. and in other countries.

U.S. Government Rights - Commercial software.  Government users are subject
to the Sun Microsystems, Inc. standard license agreement and applicable
provisions of the FAR and its supplements.

Use is subject to license terms.

Sun, Sun Microsystems and the Sun logo are trademarks or registered
trademarks of Sun Microsystems, Inc. in the U.S. and other countries.

All SPARC trademarks are used under license and are trademarks or
registered trademarks of SPARC International, Inc. in the U.S. and other
countries. Products bearing SPARC trademarks are based upon architecture
developed by Sun Microsystems, Inc.

This product is covered and controlled by U.S. Export Control laws and
may be subject to the export or import laws in other countries.  Nuclear,
missile, chemical biological weapons or nuclear maritime end uses or end
users, whether direct or indirect, are strictly prohibited.  Export or
reexport to countries subject to U.S. embargo or to entities identified
on U.S. export exclusion lists, including, but not limited to, the denied
persons and specially designated nationals lists is strictly prohibited.\x7fModuleInfo: Creator: globals kleinAndYoda.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKits InitialContents: FollowSlot'
        
         vmKits = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKits' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKits' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKits.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKits' -> () From: ( | {
         'ModuleInfo: Module: vmKits InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKits' -> () From: ( | {
         'ModuleInfo: Module: vmKits InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKits' -> () From: ( | {
         'ModuleInfo: Module: vmKits InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKits' -> () From: ( | {
         'ModuleInfo: Module: vmKits InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
            resend.postFileIn.
            runAutomatedTests.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKits' -> () From: ( | {
         'ModuleInfo: Module: vmKits InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.6 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKits' -> () From: ( | {
         'ModuleInfo: Module: vmKits InitialContents: FollowSlot\x7fVisibility: private'
        
         runAssemblerTests = ( |
            | 
            assemblerSystems ppc generateAll testAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKits' -> () From: ( | {
         'ModuleInfo: Module: vmKits InitialContents: FollowSlot\x7fVisibility: private'
        
         runAutomatedTests = ( |
            | 
            transporter moduleDictionary refill.
            runAssemblerTests.
            runExportTests).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKits' -> () From: ( | {
         'ModuleInfo: Module: vmKits InitialContents: FollowSlot\x7fVisibility: private'
        
         runExportTests = ( |
            | 
            desktop isOpen ifTrue: [
              klein exportTests miniVM copy run.
            ] False: [
               desktop open.
               klein exportTests miniVM copy run.
               "desktop close."
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKits' -> () From: ( | {
         'ModuleInfo: Module: vmKits InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitBase
vmKitDB
vmKitObjects
vmKitVM
vmKitMemInterface
vmKitMemory
vmKitExport
vmKitPrims
vmKitUpdater
klein
yoda
kleinSmallInterp
vmKitInterpreter
vmKitGC
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitBase' From: 'applications/klein'
 bootstrap read: 'vmKitDB' From: 'applications/klein'
 bootstrap read: 'vmKitObjects' From: 'applications/klein'
 bootstrap read: 'vmKitVM' From: 'applications/klein'
 bootstrap read: 'vmKitMemInterface' From: 'applications/klein'
 bootstrap read: 'vmKitMemory' From: 'applications/klein'
 bootstrap read: 'vmKitExport' From: 'applications/klein'
 bootstrap read: 'vmKitPrims' From: 'applications/klein'
 bootstrap read: 'vmKitUpdater' From: 'applications/klein'
 bootstrap read: 'klein' From: 'applications/klein'
 bootstrap read: 'yoda' From: 'applications/klein'
 bootstrap read: 'kleinSmallInterp' From: 'applications/klein'
 bootstrap read: 'vmKitInterpreter' From: 'applications/klein'
 bootstrap read: 'vmKitGC' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKits postFileIn
