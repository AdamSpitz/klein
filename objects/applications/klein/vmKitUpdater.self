 '$Revision: 30.5 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: building / exporting programs\x7fModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: public'
        
         incrementalUpdater = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda incrementalUpdater.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> () From: ( | {
         'Category: Klein define\x7fModuleInfo: Module: vmKitUpdater InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         newObjMir.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> () From: ( | {
         'ModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda incrementalUpdater parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( | {
         'Category: Klein define\x7fModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: public'
        
         disableUpdatingOfLiveImagesDuring: blk = ( |
             oldValue.
            | 
            oldValue: shouldUpdateLiveImages.
            shouldUpdateLiveImages: false.
            blk onReturn: [shouldUpdateLiveImages: oldValue]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( | {
         'Category: Klein define\x7fModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: public'
        
         doPropagation = ( |
            | 
            [todo optimize incrementalUpdate].
            "It looks like adding an assignment slot does two defines,
             one for the slots and one for the annotation, and
             so two incremental updates happen. Is that a problem?
             Can we fix it? (We could batch them up, couldn't we?
             If there are multiple defines of the same object, one
             incremental update ought to handle it, right?) -- Adam, 5/05"

            vmImagesToUpdateAndTheirMorphsDo: [|:vmi. :fpMorph| vmi propagateDefineOf: newObjMir].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( | {
         'Category: listening for defines\x7fModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: public'
        
         dontListenDuring: aBlock = ( |
            | 
            stopListening.
            aBlock onReturn: [startListening]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( | {
         'Category: playing\x7fModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: private'
        
         ifNotPlaying: blk = ( |
            | 
            "use this to debug with playAFewTimes:"
            "playCount > 0  ifFalse: blk True: [playCount: playCount pred]"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( | {
         'Category: called from mirror define hook\x7fModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: public'
        
         justMutated: newObjMir = ( |
            | 
            ifNotPlaying: [^ self].
            (copy newObjMir: newObjMir) doPropagation.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( | {
         'Category: listening for defines\x7fModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: public'
        
         listenDuring: aBlock = ( |
            | 
            startListening.
            aBlock onReturn: [stopListening]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( | {
         'Category: playing\x7fModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: public'
        
         playAFewTimes = ( |
            | 
            playCount: 3).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( | {
         'Category: playing\x7fModuleInfo: Module: vmKitUpdater InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         playCount <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( | {
         'Category: Klein define\x7fModuleInfo: Module: vmKitUpdater InitialContents: InitializeToExpression: (true)\x7fVisibility: private'
        
         shouldUpdateLiveImages <- bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( | {
         'Category: listening for defines\x7fModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: public'
        
         startListening = ( |
            | 
            asMirror addObjectMutationObserver: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( | {
         'Category: listening for defines\x7fModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: public'
        
         stopListening = ( |
            | 
            asMirror removeObjectMutationObserver: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> 'parent' -> () From: ( | {
         'Category: Klein define\x7fModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: private'
        
         vmImagesToUpdateAndTheirMorphsDo: blk = ( |
            | 
            desktop worlds do: [|:w|
              w morphsDo: [|:m|
                m isForeignProgramMorph ifTrue: [
                  m vmImage ifNotNil: [|:im| blk value: im With: m].
                ].

                shouldUpdateLiveImages ifTrue: [
                  m isPluggableOutliner && [m model isForeignProcessModel] ifTrue: [|p|
                    p: m model referrent.
                    p myVM ifNotNil: [|:vm|
                      "Until we find a need for it, let's not try to figure out how
                       to update running processes. -- Adam, 12/05"
                      p isSuspended ifTrue: [
                        blk value: vm image With: m.
                      ] False: [
                        warning: 'The Klein process is running; it will not be updated.'.
                      ].
                    ].
                  ].
                ].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'incrementalUpdater' -> () From: ( | {
         'Category: Klein define\x7fModuleInfo: Module: vmKitUpdater InitialContents: InitializeToExpression: (vector)\x7fVisibility: private'
        
         vmImages <- ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot'
        
         vmKitUpdater = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUpdater' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUpdater' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitUpdater.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUpdater' -> () From: ( | {
         'ModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUpdater' -> () From: ( | {
         'ModuleInfo: Module: vmKitUpdater InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUpdater' -> () From: ( | {
         'ModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUpdater' -> () From: ( | {
         'ModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
            resend.postFileIn.
            kleinAndYoda incrementalUpdater startListening.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUpdater' -> () From: ( | {
         'ModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.5 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitUpdater' -> () From: ( | {
         'ModuleInfo: Module: vmKitUpdater InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitUpdater postFileIn
