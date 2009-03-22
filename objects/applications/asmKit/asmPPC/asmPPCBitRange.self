 '$Revision: 30.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmPPCBitRange InitialContents: FollowSlot'
        
         bitRange = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'bitRange' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework bigEndianBitRange copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'bitRange' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc bitRange.

CopyDowns:
globals assemblerSystems framework bigEndianBitRange. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'bitRange' -> () From: ( | {
         'ModuleInfo: Module: asmPPCBitRange InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'bitRange' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc bitRange parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'bitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCBitRange InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianBitRange' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'bitRange' -> 'parent' -> () From: ( | {
         'Comment: override if notation is relative to some other number\x7fModuleInfo: Module: asmPPCBitRange InitialContents: FollowSlot\x7fVisibility: public'
        
         totalNumberOfBits = 32.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmPPCBitRange InitialContents: FollowSlot'
        
         asmPPCBitRange = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCBitRange' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCBitRange' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmPPCBitRange.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmPPCBitRange InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmPPC'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmPPCBitRange InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmPPCBitRange InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmPPCBitRange InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmPPCBitRange InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmPPCBitRange InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules asmPPCBitRange postFileIn
