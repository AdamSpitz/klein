 '$Revision: 30.21 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: building / exporting programs\x7fCategory: exporting assembler programs\x7fComment: My children are programs that can be assembled,
then downloaded and launched inside a foreignProcess.\x7fModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         assemblerProgramBuilder = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda abstractProgramBuilder copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein assemblerProgramBuilder.

CopyDowns:
globals kleinAndYoda abstractProgramBuilder. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> () From: ( | {
         'Category: assembler state\x7fModuleInfo: Module: kleinAsmPgmExporter InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         assembler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein assemblerProgramBuilder parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> () From: ( | {
         'Category: assembler\x7fComment: Convenient shorthand for accessing the assembler.\x7fModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         a = ( |
            | assembler).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         asmImage = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> 'asmImage' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein assemblerProgramBuilder parent asmImage.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> 'asmImage' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: InitializeToExpression: (byteVector)\x7fVisibility: public'
        
         assembledBytes <- bootstrap stub -> 'globals' -> 'byteVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> 'asmImage' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> 'asmImage' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein assemblerProgramBuilder parent asmImage parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> 'asmImage' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         copyAndLaunchFrom: serverProxy Process: aForeignProcess IfFail: fb = ( |
             r.
            | 
            r: copy.
            aForeignProcess launch: r From: serverProxy IfFail: [|:e| ^ fb value: e].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> 'asmImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         launchSize = ( |
            | 
            assembledBytes size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> 'asmImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         myVM = bootstrap stub -> 'globals' -> 'nil' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> 'asmImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> 'asmImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         prepareForJumpToStartMethod = ( |
            | 
            start).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> 'asmImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         propagateDefineOf: ignored = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> 'asmImage' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateTo: s WriteTo: debuggee PointMemoryTo: debugger IfFail: fb = ( |
            | 
            start: s.
            debuggee write: assembledBytes ToMemoryAt: start IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> 'asmImage' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: InitializeToExpression: (-1)'
        
         start <- -1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> () From: ( | {
         'Category: ui support\x7fModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         buildActionName = ( |
            | 'Assemble').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         buildIfFail: fb = ( |
            | 
            statusReporter show: 'assembling...'
            While: [
              assembler: myAssemblerSystem assembler copyRemoveAll.
              genIt.
            ].
            statusReporter show:
              'ready: ', assembler assembledBytesSize printString, ' bytes'.
            asmImage copy assembledBytes: assembler assembledBytes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            resend.copy assembler: assembler copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> () From: ( | {
         'Category: program source\x7fModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         genIt = ( |
            | childShouldOverride.
            forExample: [
              addTo: r3 From: r3 With: r4.
              blr
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> () From: ( | {
         'Category: assembler\x7fModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         myAssemblerSystem = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractProgramBuilder' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> () From: ( | {
         'Category: ui support\x7fModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         rebuildActionName = ( |
            | 'Reassemble').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> () From: ( | {
         'Category: ui support\x7fModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         rebuildProgramBeforeLaunchingPrompt = ( |
            | 'Reassemble program before launching?').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         assemblerProgramExporter = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'assemblerProgramExporter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein assemblerProgramBuilder copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'assemblerProgramExporter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter.

CopyDowns:
globals klein assemblerProgramBuilder. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'assemblerProgramExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'assemblerProgramExporter' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'assemblerProgramExporter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         myAssemblerSystem = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'assemblerProgramExporter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'assemblerProgramBuilder' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'assemblerProgramExporter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         registers* = bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'ppc' -> 'operands' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         conditionCodeTest = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'conditionCodeTest' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'conditionCodeTest' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc conditionCodeTest.

CopyDowns:
globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'conditionCodeTest' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'conditionCodeTest' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc conditionCodeTest parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'conditionCodeTest' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         genIt = ( |
            | 
            a liTo: r3 With: 0.
            a addo_To: r3 From: r3 With: r3.
            a liTo: r3 With: 1.
            a addo_To: r3 From: r3 With: r3.
            a liTo: r3 With: -1.
            a addo_To: r3 From: r3 With: r3.
            a load32To: r3 From: -1.
            a srwiTo: r3 From: r3 By: 1.
            a mullwo_To: r5 From: r3 With: r3.
            a trap.
            a blr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'conditionCodeTest' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'assemblerProgramExporter' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         empty = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'empty' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'empty' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc empty.

CopyDowns:
globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'empty' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'empty' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc empty parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'empty' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         genIt = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'empty' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'assemblerProgramExporter' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         endlessLoop = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'endlessLoop' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'endlessLoop' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc endlessLoop.

CopyDowns:
globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'endlessLoop' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'endlessLoop' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc endlessLoop parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'endlessLoop' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         genIt = ( |
             loop.
            | 
            a liTo: r3 With: 0.
            loop: a defineLabel.
            a addiTo: r3 From: r3 With: 1.
            a bDisp: loop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'endlessLoop' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'assemblerProgramExporter' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         testLoadMacros = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'testLoadMacros' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'testLoadMacros' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc testLoadMacros.

CopyDowns:
globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'testLoadMacros' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'testLoadMacros' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc testLoadMacros parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'testLoadMacros' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         genIt = ( |
            | 
            (0 & 1 & -1 & 16r7fff & 16rffff & -16r8000 & 16r12345678
            & ((16r8765 << 16) || 16r4321)) asVector do: [|:i|
              testLoadsFor: i
            ].
            a blr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'testLoadMacros' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'assemblerProgramExporter' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'testLoadMacros' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         testLoadsFor: val = ( |
             ok.
            | 
            "gen code to test load32 for value val"
            ok: a newLabel.
            a load32To: r3 From: val.
            a liTo: r4 With: 0.
            a oriTo: r4 From: r4 With: int32 and: val With: 16rffff.
            a orisTo: r5 From: r4 With: int32 ushr: val With: 16.
            a cmpwFrom: r3 With: r5.
            a beqDisp: ok.
            a trap.
            a bindLabel: ok.
            a loadAddressTo: r6 From: val.
            a cmpwFrom: r6 With: r5.
            ok: a newLabel.
            a beqDisp: ok.
            a trap.
            a bindLabel: ok).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         trap = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'trap' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'trap' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc trap.

CopyDowns:
globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'trap' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'trap' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc trap parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'trap' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         genIt = ( |
            | 
            a nop.
            a nop.
            a trap.
            a areCommentsEmitted: true.
            a comment: 'comment after trap'.
            a blr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'trap' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'assemblerProgramExporter' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         triangle = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'triangle' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'triangle' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc triangle.

CopyDowns:
globals kleinAndYoda exportPlatforms ppc assemblerProgramExporter. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'triangle' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'triangle' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda exportPlatforms ppc triangle parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'triangle' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         genIt = ( |
             head.
             start.
            | 
            a liTo: r4 With: 0.
            start: a newLabel.
            a bDisp: start.
            head: a defineLabel.
            a addTo: r4 From: r4 With: r3.
            a subiTo: r3 From: r3 With: 1.
            a bindLabel: start.
            a mr_To: r3 From: r3.
            a bneDisp: head.
            a mrTo: r3 From: r4.
            a trap.
            a blr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'triangle' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'exportPlatforms' -> 'ppc' -> 'assemblerProgramExporter' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         kleinAsmPgmExporter = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinAsmPgmExporter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinAsmPgmExporter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinAsmPgmExporter.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinAsmPgmExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinAsmPgmExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinAsmPgmExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinAsmPgmExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinAsmPgmExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.21 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinAsmPgmExporter' -> () From: ( | {
         'ModuleInfo: Module: kleinAsmPgmExporter InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinAsmPgmExporter postFileIn
