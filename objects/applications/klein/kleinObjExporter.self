 '$Revision: 30.17 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinObjExporter InitialContents: FollowSlot'
        
         kleinObjExporter = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinObjExporter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinObjExporter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinObjExporter.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinObjExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinObjExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinObjExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinObjExporter InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinObjExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinObjExporter InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinObjExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinObjExporter InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinObjExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinObjExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.17 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinObjExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinObjExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinObjExporter postFileIn
