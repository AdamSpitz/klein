 '$Revision: 30.11 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: compiler\x7fCategory: compiler 2\x7fModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         abstractCompiler2 = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         compileReceiver: r Slot: s Architecture: arch = ( |
             comp.
             cp.
             lk.
            | 
            [ppcCompiler2]. "browsing"
            cp: arch, 'Compiler2' sendTo: klein.
            lk: prototypes compilingLookup
              copyReceiver: r Selector: s key Delegatee: nil SendingMethodHolderOrMap: nil
                SendingVF: nil SendDesc: prototypes fakeSendDesc copy DIDesc: nil Debug: false CanReuseNM: true.
            comp: cp copyForLookup: lk SendDesc: nil DependencyLink: nil.
            todo compiler2).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         copyForLookup: aCompilingLookup SendDesc: sd DependencyLink: dl = ( |
            | 
            ((copy
              myLookup: aCompilingLookup)
              diLink: dl)
              send_desc: sd).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         prototypes = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'simpleLookup' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'simpleLookup' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes simpleLookup parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'simpleLookup' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         copyLookupType: type Receiver: rcvr Selector: sel Delegatee: dgt SendingMethodHolderOrMap: mhOrMap DependencyList: dps AssignableDependencyList: adps = ( |
            | 
            key: lookupKey copyLookupType: type Selector: sel Delegatee: dgt MethodHolder_or_map: mhOrMap.
            todo unused).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'simpleLookup' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> () From: ( | {
         'Category: lookups\x7fModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         simpleLookup = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'simpleLookup' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes simpleLookup.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> () From: ( | {
         'Category: lookups\x7fModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         vframeLookup = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'vframeLookup' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein abstractCompiler2 parent prototypes simpleLookup copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'vframeLookup' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes vframeLookup.

CopyDowns:
globals klein abstractCompiler2 parent prototypes simpleLookup. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'vframeLookup' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'vframeLookup' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes vframeLookup parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'vframeLookup' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         copyLookupType: l Receiver: rcvr Selector: sel Delegatee: dgt SmhOrMap: smhOrMap SendingVF: f DependencyList: dps AssignableDependencyList: adps = ( |
             res.
            | 
            res: copyLookupType: l Receiver: rcvr Selector: sel Delegatee: dgt SmhOrMap: smhOrMap DependencyList: dps AssignableDependencyList: adps.
            res initSendingVF: f).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'vframeLookup' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         initSendingVF: f = ( |
             vfmh.
            | 
            sendingVFrame: f.
            f ifNil: [^ self].
            (klein compiler2 lookupTypes isResend: lookupType)
             ifFalse: [ 
              key set_methodHolder_or_map: key mh_not_a_resend.
              ^ self
            ].
            vfmh: sendingVFrame methodHolder_or_map.
            vfmh is_map  ifFalse: [
              key set_methodHolder_or_map: vfmh.
              ^ self
            ].
            "method holder is same as self; replace it now"
            smhOrMap = key mh_tbd  ifTrue: [
              "don't have run-time smh, so compute it (e.g. for conversions)"
              key set_methodHOlder_or_map: sendingVFrame methodHolder_object.
              ^ self
            ].
            "just use smh"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'vframeLookup' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'simpleLookup' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> () From: ( | {
         'Category: lookups\x7fModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         compilingLookup = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'compilingLookup' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein abstractCompiler2 parent prototypes vframeLookup copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'compilingLookup' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes compilingLookup.

CopyDowns:
globals klein abstractCompiler2 parent prototypes vframeLookup. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'compilingLookup' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'compilingLookup' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes compilingLookup parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'compilingLookup' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         copyReceiver: rcvr Selector: sel Delegatee: dgt SendingMethodHolderOrMap: smhOrMap SendingVF: f SendDesc: s DIDesc: d Debug: debug CanReuseNM: c = ( |
             res.
            | 
            res: copyLookupType: s lookupType
                   Receiver: rcvr
                   Selector: sel
                   Delegatee: dgt
                   SendingMethodHolderOrMap: smhOrMap
                   SendingVF: f
                   DependencyList: list copyRemoveAll
                   AssignableDependencyList: list copyRemoveAll.
            res sd: s.
            res dc: d.
            res needDebug: debug.
            res canReuseNM: c.
            res).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'compilingLookup' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'vframeLookup' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> () From: ( | {
         'Category: send descs\x7fModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         fakeSendDesc = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'fakeSendDesc' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes fakeSendDesc.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'fakeSendDesc' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         lookupType = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'fakeSendDesc' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'fakeSendDesc' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes fakeSendDesc parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'fakeSendDesc' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> () From: ( | {
         'Category: lookup keys\x7fModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         scopeLookupKey = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'scopeLookupKey' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes scopeLookupKey.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'scopeLookupKey' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: InitializeToExpression: (nil)'
        
         delegatee.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'scopeLookupKey' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: InitializeToExpression: (nil)'
        
         lookupType.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'scopeLookupKey' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'scopeLookupKey' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes scopeLookupKey parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'scopeLookupKey' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         copyLookupType: l Selector: s Delegatee: d = ( |
            | 
            ((copy lookupType: l)
              selector: s)
              delegatee: d).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'scopeLookupKey' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         mh_not_a_resend = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'scopeLookupKey' -> 'parent' -> 'mh_not_a_resend' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes scopeLookupKey parent mh_not_a_resend.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'scopeLookupKey' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         mh_tbd = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'scopeLookupKey' -> 'parent' -> 'mh_tbd' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes scopeLookupKey parent mh_tbd.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'scopeLookupKey' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'scopeLookupKey' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: InitializeToExpression: (nil)'
        
         selector.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> () From: ( | {
         'Category: lookup keys\x7fComment: I add the receiver map to that info,
and am specific to a given receiver map.
I am only used during lookups.\x7fModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         methodLookupKey = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'methodLookupKey' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein abstractCompiler2 parent prototypes scopeLookupKey copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'methodLookupKey' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes methodLookupKey.

CopyDowns:
globals klein abstractCompiler2 parent prototypes scopeLookupKey. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'methodLookupKey' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: InitializeToExpression: (false)'
        
         hasMap <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'methodLookupKey' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: InitializeToExpression: (false)'
        
         hasMethodHolder <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'methodLookupKey' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: InitializeToExpression: (nil)'
        
         map.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'methodLookupKey' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: InitializeToExpression: (nil)'
        
         methodHolder.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'methodLookupKey' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'methodLookupKey' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractCompiler2 parent prototypes methodLookupKey parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'methodLookupKey' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> 'prototypes' -> 'scopeLookupKey' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: compiler\x7fCategory: compiler 2\x7fComment: An aborted experiment as of 6/03 -- dmu\x7fModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         compiler2 = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler2' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein abstractCompiler2 copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler2.

CopyDowns:
globals klein abstractCompiler2. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler2' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler2 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler2' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'abstractCompiler2' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: compiler\x7fCategory: compiler 2\x7fModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         ppcCompiler2 = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'ppcCompiler2' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein compiler2 copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'ppcCompiler2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein ppcCompiler2.

CopyDowns:
globals klein compiler2. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'ppcCompiler2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'ppcCompiler2' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein ppcCompiler2 parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'ppcCompiler2' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'compiler2' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         kleinCompiler2 = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler2' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler2' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinCompiler2.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.11 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinCompiler2' -> () From: ( | {
         'ModuleInfo: Module: kleinCompiler2 InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinCompiler2 postFileIn
