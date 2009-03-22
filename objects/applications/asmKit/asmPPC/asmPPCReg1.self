 '$Revision: 30.9 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> () From: ( | {
         'Category: helpers\x7fModuleInfo: Module: asmPPCReg1 InitialContents: FollowSlot'
        
         abstractRegister = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'abstractRegister' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators constants parent proto copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'abstractRegister' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc abstractRegister.

CopyDowns:
globals assemblerSystems framework generators constants parent proto. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'abstractRegister' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg1 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'abstractRegister' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems ppc abstractRegister parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'abstractRegister' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg1 InitialContents: FollowSlot'
        
         isCRField = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'abstractRegister' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg1 InitialContents: FollowSlot'
        
         isFPR = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'abstractRegister' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg1 InitialContents: FollowSlot'
        
         isGPR = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'abstractRegister' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg1 InitialContents: FollowSlot'
        
         isSpecial = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'abstractRegister' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg1 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'constants' -> 'parent' -> 'proto' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg1 InitialContents: FollowSlot'
        
         asmPPCReg1 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg1' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg1' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmPPCReg1.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg1' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg1 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmPPC'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg1' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg1 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg1' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg1 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg1' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg1 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg1' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg1 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.9 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmPPCReg1' -> () From: ( | {
         'ModuleInfo: Module: asmPPCReg1 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'asmPPCReg2
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'asmPPCReg2' From: 'applications/asmKit/asmPPC'



 '-- Side effects'

 globals modules asmPPCReg1 postFileIn
