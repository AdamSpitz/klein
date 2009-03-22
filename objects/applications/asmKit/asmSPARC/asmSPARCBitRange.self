 '$Revision: 30.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmSPARCBitRange InitialContents: FollowSlot'
        
         bitRange = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'bitRange' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework littleEndianBitRange copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'bitRange' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc bitRange.

CopyDowns:
globals assemblerSystems framework littleEndianBitRange. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'bitRange' -> () From: ( | {
         'ModuleInfo: Module: asmSPARCBitRange InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'bitRange' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems sparc bitRange parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'bitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmSPARCBitRange InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianBitRange' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'sparc' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Comment: override if notation is relative to some other number\x7fModuleInfo: Module: asmSPARCBitRange InitialContents: FollowSlot\x7fVisibility: public'
        
         totalNumberOfBits = 32.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmSPARCBitRange InitialContents: FollowSlot'
        
         asmSPARCBitRange = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARCBitRange' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARCBitRange' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmSPARCBitRange.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARCBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmSPARCBitRange InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmSPARC'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARCBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmSPARCBitRange InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARCBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmSPARCBitRange InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARCBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmSPARCBitRange InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARCBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmSPARCBitRange InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmSPARCBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmSPARCBitRange InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules asmSPARCBitRange postFileIn
