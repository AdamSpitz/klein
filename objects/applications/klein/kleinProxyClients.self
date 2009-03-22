 '$Revision: 30.59 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> () From: ( | {
         'Category: application information objects\x7fModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         klein = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'klein' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda debuggingProxyClients noncaching abstract parent appInfoProtos klein.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'klein' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         defaultPort = 9090.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'klein' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         executableName = 'kleinDebugServer'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'klein' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         initiationName = 'debug_server'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'klein' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'klein' -> () From: ( | {
         'Category: downloading\x7fModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         sendBootstrapInfo: aVMImage Via: proxy = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'appInfoProtos' -> 'klein' -> () From: ( | {
         'Category: downloading\x7fModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         startPCForImage: aVMImage Via: proxy = ( |
            | 
            aVMImage prepareForJumpToStartMethod).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'floatState' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         asFloatState = ( |
             v.
            | 
            v: fpregs copySize: fpregs size + 1.
            v at: fpregs size Put: fpscr.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'floatState' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | resend.copy fprs: fprs copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'floatState' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         copyFromFloatState: v = ( |
            | 
            copy initFromFloatState: v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'floatState' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         initFromFloatState: v = ( |
            | 
            fpregs: v copySize: fpregs size.
            fpscr: v last).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'floatState' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'vectorState' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         asVectorState = ( |
            | 
            v: vector copySize: (32 * 4) + 4 + 1.
            save_vr do: [|:avr. :i|
              avr do: [|:x. :j|
                v at: (i*4) + j Put: x
              ].
            ].
            save_vscr do: [|:x. :i|
              v at: (32 * 4) + i Put: x
            ].
            v at: (32* 4) + 4 Put: save_vrvalid.
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'vectorState' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            (resend.copy 
            save_vr: copySave_vr)
            save_vscr: save_vscr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'vectorState' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         copyFromVectorState: vs = ( |
            | 
            copy initFromVectorState: vs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'vectorState' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         copySave_vr = ( |
            | 
            save_vr copyMappedBy: [|:x| x copy]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'vectorState' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         initFromVectorState: vs = ( |
            | 
            32 do: [|:i|
              4 do: [|:j|
                (save_vr at: i) at: j Put: vs at: (i * 4) + j
              ].
            ].
            4 do: [|:i| save_vscr at: i Put: vs at: (32 * 4) + i].
            save_vrvalid: vs at: (32 * 4) + 4.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'vectorState' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'yodaRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         getLayoutConstants = 3.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'yodaRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         getObjectLocatorTimestampAddress = 5.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'yodaRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         getStartFunctionAddress = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'yodaRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         getWellKnownObjectsAddress = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'debuggingProxyClients' -> 'noncaching' -> 'abstract' -> 'parent' -> 'yodaRequestTypes' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         setBootstrapInfo = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         kleinProxyClients = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProxyClients' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProxyClients' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinProxyClients.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProxyClients' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProxyClients' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProxyClients' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProxyClients' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProxyClients' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.59 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinProxyClients' -> () From: ( | {
         'ModuleInfo: Module: kleinProxyClients InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinProxyClients postFileIn
