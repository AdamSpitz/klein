 '$Revision: 30.38 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: applications\x7fCategory: VM Kits\x7fModuleInfo: Module: klein InitialContents: FollowSlot\x7fVisibility: public'
        
         klein = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> () From: ( |
             {} = 'Comment: Root of the klein namespace.
 -- dmu 6/03

Projects I would like to do soon:

redo module structure
fix system so you can export same vm twice and mirrors are distinct
  needs separate vm objects, and adjustments to oracles.
  also other objects in initializeForArchitecture??
do meta-recursion audit
  ensure prims are only used when needed and comments are explicit
\x7fModuleInfo: Creator: globals klein.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'ModuleInfo: Module: klein InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: klein InitialContents: FollowSlot'
        
         klein = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'klein' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'klein' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules klein.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'klein' -> () From: ( | {
         'ModuleInfo: Module: klein InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'klein' -> () From: ( | {
         'ModuleInfo: Module: klein InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'klein' -> () From: ( | {
         'ModuleInfo: Module: klein InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'klein' -> () From: ( | {
         'ModuleInfo: Module: klein InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
            resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'klein' -> () From: ( | {
         'ModuleInfo: Module: klein InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.38 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'klein' -> () From: ( | {
         'ModuleInfo: Module: klein InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'asmKit
kleinCompiler
kleinExport
kleinDB
kleinSendDesc
kleinFrames
kleinRelocators
kleinVM
kleinPrims
kleinResendDetector
kleinExportTests
kleinSnapshotWriter
kleinBlockInlining
kleinBytecodes
kleinObjects
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'asmKit' From: 'applications/asmKit'
 bootstrap read: 'kleinCompiler' From: 'applications/klein'
 bootstrap read: 'kleinExport' From: 'applications/klein'
 bootstrap read: 'kleinDB' From: 'applications/klein'
 bootstrap read: 'kleinSendDesc' From: 'applications/klein'
 bootstrap read: 'kleinFrames' From: 'applications/klein'
 bootstrap read: 'kleinRelocators' From: 'applications/klein'
 bootstrap read: 'kleinVM' From: 'applications/klein'
 bootstrap read: 'kleinPrims' From: 'applications/klein'
 bootstrap read: 'kleinResendDetector' From: 'applications/klein'
 bootstrap read: 'kleinExportTests' From: 'applications/klein'
 bootstrap read: 'kleinSnapshotWriter' From: 'applications/klein'
 bootstrap read: 'kleinBlockInlining' From: 'applications/klein'
 bootstrap read: 'kleinBytecodes' From: 'applications/klein'
 bootstrap read: 'kleinObjects' From: 'applications/klein'



 '-- Side effects'

 globals modules klein postFileIn
