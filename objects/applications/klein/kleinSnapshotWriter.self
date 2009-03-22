 '$Revision: 1.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: snapshotting\x7fModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot\x7fVisibility: public'
        
         snapshotWriter = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'snapshotWriter' -> () From: ( |
             {} = 'Comment: This code is duplicated.
Keep in synch with 
unixDebugServer,
class Socket\x7fModuleInfo: Creator: globals klein snapshotWriter.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'snapshotWriter' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: InitializeToExpression: (os_file)\x7fVisibility: private'
        
         osFile <- bootstrap stub -> 'globals' -> 'unixGlobals' -> 'os_file' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'snapshotWriter' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'snapshotWriter' -> 'parent' -> () From: ( |
             {} = 'Comment: This code is duplicated.
Keep in synch with 
unixDebugServer,
class Socket\x7fModuleInfo: Creator: globals klein snapshotWriter parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'snapshotWriter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForFile: osFile = ( |
            | 
            copy osFile: osFile).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'snapshotWriter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'snapshotWriter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot\x7fVisibility: public'
        
         readInteger: int IfFail: fb = ( |
             bv.
            | 
            bv: byteVector copySize: 4.
            osFile readInto: bv Count: 4 IfFail: fb.
            (int32 copyTakeBigEndianBytesFrom: bv) asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'snapshotWriter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot\x7fVisibility: public'
        
         writeData: bv IfFail: fb = ( |
            | 
            osFile writeFrom: bv Count: bv size IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'snapshotWriter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot\x7fVisibility: public'
        
         writeInteger: int IfFail: fb = ( |
             bv.
            | 
            bv: int32 asBigEndianByteVectorFrom:  int.
            osFile writeFrom: bv Count: (bv size) IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'snapshotWriter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot\x7fVisibility: public'
        
         writeVector: aVector = ( |
            | 
            writeVector: aVector Size: (aVector size) IfFail: [|:e| ^ error:( 'Failed to write data', e)]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'snapshotWriter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot\x7fVisibility: public'
        
         writeVector: aVector Size: size IfFail: fb = ( |
             bv.
            | 
            writeInteger:  (size*4) IfFail: fb.
            bv: byteVector copyFromBigEndianInt32: aVector IfFail: [|:e| ^ fb value: e].
            writeData: bv IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot'
        
         kleinSnapshotWriter = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSnapshotWriter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSnapshotWriter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinSnapshotWriter.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSnapshotWriter' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSnapshotWriter' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSnapshotWriter' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSnapshotWriter' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSnapshotWriter' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 1.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSnapshotWriter' -> () From: ( | {
         'ModuleInfo: Module: kleinSnapshotWriter InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinSnapshotWriter postFileIn
