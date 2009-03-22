 '$Revision: 30.9 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: integer operand fields\x7fCategory: prototypes\x7fModuleInfo: Module: asmFrame5 InitialContents: FollowSlot'
        
         branchDispOperandField = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'isRelative' From:
             bootstrap remove: 'parent' From:
             bootstrap remove: 'signOps' From:
             bootstrap remove: 'zeroes' From:
             globals assemblerSystems framework generators fields parent alignedIntOperandField copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent branchDispOperandField.

CopyDowns:
globals assemblerSystems framework generators fields parent alignedIntOperandField. copy 
SlotsToOmit: isRelative parent signOps zeroes.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmFrame5 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent branchDispOperandField parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame5 InitialContents: FollowSlot'
        
         isRelativeForIT: instTemplate = ( |
            | 
            true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame5 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'alignedIntOperandField' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: asmFrame5 InitialContents: FollowSlot'
        
         relativeOperandOffsetAt: lc IT: instTemplate = ( |
            | 
            lc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame5 InitialContents: FollowSlot'
        
         signOps = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> 'signDependentOperations' -> 'signed' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'branchDispOperandField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame5 InitialContents: FollowSlot'
        
         zeroes = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmFrame5 InitialContents: FollowSlot'
        
         asmFrame5 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame5' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame5' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmFrame5.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame5' -> () From: ( | {
         'ModuleInfo: Module: asmFrame5 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmFrame'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame5' -> () From: ( | {
         'ModuleInfo: Module: asmFrame5 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame5' -> () From: ( | {
         'ModuleInfo: Module: asmFrame5 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame5' -> () From: ( | {
         'ModuleInfo: Module: asmFrame5 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame5' -> () From: ( | {
         'ModuleInfo: Module: asmFrame5 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.9 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame5' -> () From: ( | {
         'ModuleInfo: Module: asmFrame5 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules asmFrame5 postFileIn
