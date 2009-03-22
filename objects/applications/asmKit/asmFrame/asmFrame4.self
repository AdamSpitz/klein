 '$Revision: 30.13 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> () From: ( | {
         'Category: operand fields\x7fCategory: integer operand fields\x7fCategory: prototypes\x7fModuleInfo: Module: asmFrame4 InitialContents: FollowSlot'
        
         alignedIntOperandField = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'alignedIntOperandField' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework generators fields parent intOperandField copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'alignedIntOperandField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent alignedIntOperandField.

CopyDowns:
globals assemblerSystems framework generators fields parent intOperandField. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'alignedIntOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmFrame4 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'alignedIntOperandField' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators fields parent alignedIntOperandField parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'alignedIntOperandField' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: asmFrame4 InitialContents: FollowSlot'
        
         copyName: n Bits: bits Keyword: k Asm: myAsm Zeroes: z = ( |
             r.
            | 
            r: copyName: n Bits: bits Keyword: k Asm: myAsm.
            r zeroes: z.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'alignedIntOperandField' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame4 InitialContents: FollowSlot\x7fVisibility: public'
        
         max = ( |
            | 
            (intNN shl: resend.max With: zeroes) asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'alignedIntOperandField' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: asmFrame4 InitialContents: FollowSlot\x7fVisibility: public'
        
         min = ( |
            | 
            (intNN shl: resend.min With: zeroes) asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'alignedIntOperandField' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrame4 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'intOperandField' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'alignedIntOperandField' -> 'parent' -> () From: ( | {
         'Category: assembling\x7fModuleInfo: Module: asmFrame4 InitialContents: FollowSlot\x7fVisibility: private'
        
         shift: i IfFail: fb = ( |
             p.
            | 
            p: grain.
            (i && p pred) = 0
              ifFalse: [^ fb value: i printString, ' is not aligned'].
            i +> zeroes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'alignedIntOperandField' -> 'parent' -> () From: ( | {
         'Category: generating tests & testing\x7fModuleInfo: Module: asmFrame4 InitialContents: FollowSlot\x7fVisibility: public'
        
         unfilteredIllegalTestCasesDo: blk = ( |
            | 
            resend.unfilteredIllegalTestCasesDo: blk.
            1 to: grain pred Do: [|:z|
              blk value: z.
              blk value: z negate.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'alignedIntOperandField' -> 'parent' -> () From: ( | {
         'Category: disassembling\x7fModuleInfo: Module: asmFrame4 InitialContents: FollowSlot\x7fVisibility: private'
        
         unshift: i = ( |
            | 
            i << zeroes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'fields' -> 'parent' -> 'alignedIntOperandField' -> () From: ( | {
         'ModuleInfo: Module: asmFrame4 InitialContents: InitializeToExpression: (0)'
        
         zeroes <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmFrame4 InitialContents: FollowSlot'
        
         asmFrame4 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame4' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame4' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmFrame4.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame4' -> () From: ( | {
         'ModuleInfo: Module: asmFrame4 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmFrame'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame4' -> () From: ( | {
         'ModuleInfo: Module: asmFrame4 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame4' -> () From: ( | {
         'ModuleInfo: Module: asmFrame4 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame4' -> () From: ( | {
         'ModuleInfo: Module: asmFrame4 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame4' -> () From: ( | {
         'ModuleInfo: Module: asmFrame4 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.13 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrame4' -> () From: ( | {
         'ModuleInfo: Module: asmFrame4 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'asmFrame5
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'asmFrame5' From: 'applications/asmKit/asmFrame'



 '-- Side effects'

 globals modules asmFrame4 postFileIn
