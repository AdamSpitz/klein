 '$Revision: 1.3 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fCategory: caching\x7fModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrorCache = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda mirrorCache.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> () From: ( | {
         'Category: mirror caching\x7fModuleInfo: Module: vmKitMirrorCache InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         cachedImportedMaps <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> () From: ( | {
         'Category: mirror caching\x7fModuleInfo: Module: vmKitMirrorCache InitialContents: InitializeToExpression: (dictionary copyRemoveAll)\x7fVisibility: private'
        
         cachedImportedMirrors <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> () From: ( | {
         'Category: mirror caching\x7fModuleInfo: Module: vmKitMirrorCache InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myVM.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda mirrorCache parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | error: 'use copyForVM:').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForVM: aVM = ( |
            | 
            (((resend.copy
              sema:                  sema copyBinary)
              cachedImportedMaps:    cachedImportedMaps    copy)
              cachedImportedMirrors: cachedImportedMirrors copy)
              myVM: aVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot\x7fVisibility: public'
        
         importedMapFor: mapOop IfAbsentPut: ab = ( |
            | 
            sema protect: [
              invalidateMyObsoleteCachedItems.
              cachedImportedMaps
                         at: mapOop
                IfAbsentPut: ab
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot\x7fVisibility: public'
        
         importedMirrorFor: oop IfAbsentPut: ab = ( |
            | 
            sema protect: [
              invalidateMyObsoleteCachedItems.
              "cannot use if:IsPresentDo:IfAbsentPut:AndDo:
               because ab can change cachedImportedMirrors -- dmu 10/04"
              cachedImportedMirrors at: oop IfAbsent: [
                |r|
                r: ab value.
                cachedImportedMirrors at: oop Put: r.
                r
              ].
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> 'parent' -> () From: ( | {
         'Category: invalidating\x7fModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateCachedItems = ( |
            | 
            sema protect: [
              cachedImportedMaps    removeAll.
              cachedImportedMirrors removeAll. "GC may change OOPS"
              timestampOfOldestCachedItem: time current.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> 'parent' -> () From: ( | {
         'Category: invalidating\x7fModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot\x7fVisibility: private'
        
         invalidateMyObsoleteCachedItems = ( |
            | 
            sema protect: [
              myVM machineMemory invalidateObsoleteCachedItemsIn: self.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot\x7fVisibility: public'
        
         removeAll = ( |
            | 
            cachedImportedMaps    removeAll.
            cachedImportedMirrors removeAll.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> () From: ( | {
         'Category: mirror caching\x7fModuleInfo: Module: vmKitMirrorCache InitialContents: InitializeToExpression: (recursiveSemaphore copyCount: 0 Capacity: 1)\x7fVisibility: private'
        
         sema <- recursiveSemaphore copyCount: 0 Capacity: 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrorCache' -> () From: ( | {
         'Category: mirror caching\x7fModuleInfo: Module: vmKitMirrorCache InitialContents: InitializeToExpression: (time origin)\x7fVisibility: private'
        
         timestampOfOldestCachedItem <- time origin.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot'
        
         vmKitMirrorCache = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMirrorCache' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMirrorCache' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitMirrorCache.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMirrorCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMirrorCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitMirrorCache InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMirrorCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMirrorCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMirrorCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 1.3 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMirrorCache' -> () From: ( | {
         'ModuleInfo: Module: vmKitMirrorCache InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitMirrorCache postFileIn
