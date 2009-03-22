 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: bit ranges\x7fCategory: concrete\x7fModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         bigEndianBitRange = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianBitRange' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework simpleBitRange copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianBitRange' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework bigEndianBitRange.

CopyDowns:
globals assemblerSystems framework simpleBitRange. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianBitRange' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework bigEndianBitRange parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         < x = ( |
            | firstBit < x firstBit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         from: f To: t = ( |
            | 
            [f <= t] assert.
            resend.from: f To: t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfBitsToMyLeft = ( |
            | 
            firstBit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfBitsToMyRight = ( |
            | 
            totalNumberOfBits - lastBit - 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'bigEndianBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         width = ( |
            | (lastBit - firstBit) + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> () From: ( | {
         'Category: helpers\x7fCategory: bit ranges\x7fCategory: concrete\x7fModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         littleEndianBitRange = bootstrap define: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianBitRange' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals assemblerSystems framework simpleBitRange copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianBitRange' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework littleEndianBitRange.

CopyDowns:
globals assemblerSystems framework simpleBitRange. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianBitRange' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianBitRange' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework littleEndianBitRange parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         < x = ( |
            | 
            firstBit > x firstBit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         from: f To: t = ( |
            | 
            [f >= t] assert.
            resend.from: f To: t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfBitsToMyLeft = ( |
            | 
            "e.g. 32 - 31 - 1"
            totalNumberOfBits - firstBit - 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfBitsToMyRight = ( |
            | 
            lastBit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'simpleBitRange' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'littleEndianBitRange' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         width = ( |
            | 
            (firstBit - lastBit) + 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot'
        
         asmBitRange3 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange3' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange3' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmBitRange3.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange3' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmFrame'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange3' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange3' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange3' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange3' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmBitRange3' -> () From: ( | {
         'ModuleInfo: Module: asmBitRange3 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules asmBitRange3 postFileIn
