 '$Revision: 30.21 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinVMExporter InitialContents: FollowSlot'
        
         kleinVMExporter = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVMExporter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVMExporter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinVMExporter.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVMExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinVMExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVMExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinVMExporter InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVMExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinVMExporter InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVMExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinVMExporter InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVMExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinVMExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.21 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinVMExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinVMExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'kleinObjExporter
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'kleinObjExporter' From: 'applications/klein'



 '-- Side effects'

 globals modules kleinVMExporter postFileIn
