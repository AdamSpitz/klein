 '$Revision: 30.2 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing the debuggee\x7fCategory: accessing registers\x7fModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         integerState = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent integerState.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent integerState abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent integerState abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         at: regName = ( |
            | 
            at: regName IfAbsent: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         copyAt: regName Put: aNumber = ( |
            | 
            copy at: regName Put: aNumber).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: private'
        
         myAssemblerSystem = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         i386 = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda debuggingProxyClients noncaching abstract parent integerState abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent integerState i386.

CopyDowns:
globals kleinAndYoda debuggingProxyClients noncaching abstract parent integerState abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         cs.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         ds.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         eax.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         ebp.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         ebx.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         ecx.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         edi.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         edx.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         eflags.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         eip.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         es.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         esi.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         esp.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         fs.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         gs.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent integerState i386 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         asVector = ( |
             v.
            | 
            v: vector copySize: 16.
            v at: 0 Put: eax.
            v at: 1 Put: ebx.
            v at: 2 Put: ecx.
            v at: 3 Put: edx.
            v at: 4 Put: edi.
            v at: 5 Put: esi.
            v at: 6 Put: ebp.
            v at: 7 Put: esp.
            v at: 8 Put: ss.
            v at: 9 Put: eflags.
            v at: 10 Put: eip.
            v at: 11 Put: cs.
            v at: 12 Put: ds.
            v at: 13 Put: es.
            v at: 14 Put: fs.
            v at: 15 Put: gs.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         at: regName IfAbsent: absentBlk = ( |
            | 
            regName sendTo: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         at: regName Put: aNumber = ( |
            | 
            regName, ':'  sendTo: self With: aNumber.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         copyFromVector: v = ( |
             r.
            | 
            r: copy.
            r eax: v at: 0.
            r ebx: v at: 1.
            r ecx: v at: 2.
            r edx: v at: 3.
            r edi: v at: 4.
            r esi: v at: 5.
            r ebp: v at: 6.
            r esp: v at: 7.
            r ss:  v at: 8.
            r eflags: v at: 9.
            r eip: v at: 10.
            r cs: v at: 11.
            r ds: v at: 12.
            r es: v at: 13.
            r fs: v at: 14.
            r gs: v at: 15.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: private'
        
         myAssemblerSystem = ( |
            | 
            [todo platformDependent]. "This whole object is PPC-specific, is it not?"
            assemblerSystems i386).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         pc = ( |
            | eip).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         pc: x = ( |
            | eip: x).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            'eax    = ',  eax    printString, '\n',
            'ebx    = ',  ebx    printString, '\n',
            'ecx    = ',  ecx    printString, '\n',
            'edx    = ',  edx    printString, '\n',
            'edi    = ',  edi    printString, '\n',
            'esi    = ',  esi    printString, '\n',
            'ss     = ',  ss     printString, '\n',
            'eflags = ',  eflags printString, '\n',
            'eip    = ',  eip    printString, '\n',
            'cs     = ',  cs     printString, '\n',
            'ds     = ',  ds     printString, '\n',
            'es     = ',  es     printString, '\n',
            'fs     = ',  fs     printString, '\n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'i386' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         ss.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         ppc = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda debuggingProxyClients noncaching abstract parent integerState abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent integerState ppc.

CopyDowns:
globals kleinAndYoda debuggingProxyClients noncaching abstract parent integerState abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> () From: ( | {
         'Comment: condition register\x7fModuleInfo: Module: integerStates InitialContents: InitializeToExpression: (0)'
        
         cr <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> () From: ( | {
         'Comment: count register\x7fModuleInfo: Module: integerStates InitialContents: InitializeToExpression: (0)'
        
         ctr <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: InitializeToExpression: (vector copySize: 32 FillingWith: 0)'
        
         gprs <- vector copySize: 32 FillingWith: 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> () From: ( | {
         'Comment: link register\x7fModuleInfo: Module: integerStates InitialContents: InitializeToExpression: (0)'
        
         lr <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: InitializeToExpression: (0)'
        
         mq <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent integerState ppc parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         asVector = ( |
             v.
            | 
            v: gprs copySize: gprs size + 2 + 6 FillingWith: 0.
            v at: 0 Put: srr0.
            v at: 1 Put: srr1.
            gprs do: [|:r. :i| v at: 2 + i Put: r].
            v at: 2 + 32 + 0 Put: cr.
            v at: 2 + 32 + 1 Put: xer.
            v at: 2 + 32 + 2 Put: lr.
            v at: 2 + 32 + 3 Put: ctr.
            v at: 2 + 32 + 4 Put: mq.
            v at: 2 + 32 + 5 Put: vrsave.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         at: regName IfAbsent: absentBlk = ( |
            | 
            gprNumberFor: regName
               IfPresent: [|:i| ^ gprs at: i]
                IfAbsent: [].
            regName isEmpty  ifTrue: [^ absentBlk value: regName].

            regName       = 'pc'      ifTrue: [^    pc].
            regName       = 'lr'      ifTrue: [^    lr].
            regName       = 'cr'      ifTrue: [^    cr].
            regName       = 'ctr'     ifTrue: [^   ctr].
            regName       = 'mq'      ifTrue: [^    mq].
            regName       = 'srr0'    ifTrue: [^  srr0].
            regName       = 'srr1'    ifTrue: [^  srr1].
            regName       = 'vrsave'  ifTrue: [^vrsave].
            regName       = 'xer'     ifTrue: [^   xer].

            error: 'what?').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'Comment: Use copyAt:Put: instead to avoid bugs.
-- dmu 1/02\x7fModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         at: regName Put: aNumber = ( |
            | 
            gprNumberFor: regName
               IfPresent: [|:i| gprs at: i Put: aNumber. ^ self]
                IfAbsent: [].
            regName       = 'pc'      ifTrue: [    pc:  aNumber. ^ self].
            regName       = 'lr'      ifTrue: [    lr:  aNumber. ^ self].
            regName       = 'cr'      ifTrue: [    cr:  aNumber. ^ self].
            regName       = 'ctr'     ifTrue: [   ctr:  aNumber. ^ self].
            regName       = 'mq'      ifTrue: [    mq:  aNumber. ^ self].
            regName       = 'srr0'    ifTrue: [  srr0:  aNumber. ^ self].
            regName       = 'srr1'    ifTrue: [  srr1:  aNumber. ^ self].
            regName       = 'vrsave'  ifTrue: [vrsave:  aNumber. ^ self].
            regName       = 'xer'     ifTrue: [   xer:  aNumber. ^ self].
            error: 'what?').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | resend.copy gprs: gprs copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         copyFromVector: v = ( |
            | 
            copy initFromVector: v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         gprNumberFor: regName IfPresent: gprBlk IfAbsent: absentBlk = ( |
            | 
            myAssemblerSystem operands gprNumberFor: regName
                                          IfPresent: gprBlk
                                           IfAbsent: absentBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: private'
        
         gprsPrintString = ( |
             r <- ''.
            | 
            gprs do: [|:v. :k|
              r: r, 'r', k printString, ('  ' copySize: k <= 9 ifTrue: 2 False: 1), v printString, '\n'
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: private'
        
         initFromVector: v = ( |
            | 
            srr0:   v at: 0.
            srr1:   v at: 1.
            gprs:   v copyFrom: 2 UpTo: 2 + 32.
            cr:     v at: 2 + 32 + 0.
            xer:    v at: 2 + 32 + 1.
            lr:     v at: 2 + 32 + 2.
            ctr:    v at: 2 + 32 + 3.
            mq:     v at: 2 + 32 + 4.
            vrsave: v at: 2 + 32 + 5.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         machineState = ( |
            | srr1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         machineState: s = ( |
            | srr1: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: private'
        
         myAssemblerSystem = ( |
            | 
            [todo platformDependent]. "This whole object is PPC-specific, is it not?"
            assemblerSystems ppc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         pc = ( |
            | srr0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         pc: x = ( |
            | srr0: x).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            'pc     = ', pc     printString, '\n',
            'lr     = ', lr     printString, '\n',
            'cr     = ', cr     printString, '\n',
            'ctr    = ', ctr    printString, '\n',
            'mq     = ', mq     printString, '\n',
            'srr1   = ', srr1   printString, '\n',
            'vrsave = ', vrsave printString, '\n',
            'xer    = ', xer    printString, '\n',
            'lr     = ', lr     printString, '\n',
            'gprs   = ', gprsPrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: InitializeToExpression: (0)'
        
         srr0 <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: InitializeToExpression: (0)'
        
         srr1 <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: InitializeToExpression: (0)'
        
         vrsave <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'integerState' -> 'ppc' -> () From: ( | {
         'Comment: integer exception register\x7fModuleInfo: Module: integerStates InitialContents: InitializeToExpression: (0)'
        
         xer <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         integerStates = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'integerStates' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'integerStates' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules integerStates.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'integerStates' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'integerStates' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'integerStates' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'integerStates' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'integerStates' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.2 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'integerStates' -> () From: ( | {
         'ModuleInfo: Module: integerStates InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules integerStates postFileIn
