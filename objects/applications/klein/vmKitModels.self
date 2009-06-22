 '$Revision: 30.8 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'generalModel' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         isForeignProcessModel = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: models for debugger\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         abstractMachineLevelForeignActivationModel = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMachineLevelForeignActivationModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalSlotModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMachineLevelForeignActivationModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda abstractMachineLevelForeignActivationModel.

CopyDowns:
globals generalSlotModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMachineLevelForeignActivationModel' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMachineLevelForeignActivationModel' -> 'parent' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'buttonDescriptions' From:
             globals generalActivationModel parent _Clone ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMachineLevelForeignActivationModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda abstractMachineLevelForeignActivationModel parent.

CopyDowns:
globals generalActivationModel parent. _Clone 
SlotsToOmit: buttonDescriptions.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMachineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: menu operations\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot'
        
         buttonDescriptions = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMachineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalActivationModel parent buttonDescriptions _Clone ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMachineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( |
             {} = 'Comment: Holds button descriptions:
category leaf is button name, 
slot name is button name in buttonCache,
method source is button script,
public slots make asynchronous buttons.\x7fModuleInfo: Creator: globals kleinAndYoda abstractMachineLevelForeignActivationModel parent buttonDescriptions.

CopyDowns:
globals generalActivationModel parent buttonDescriptions. _Clone 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMachineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMachineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda abstractMachineLevelForeignActivationModel parent buttonDescriptions parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMachineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalSlotModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: models for debugger\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcessModel = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalProcessModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignProcessModel.

CopyDowns:
globals generalProcessModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( |
             {} = 'Comment: I am the traits for the object that
implements the outliner model for the foreignProcess debugger.

try: [foreignProcessModel debugProcess: foreignProcess copy]
-- dmu 1/02\x7fModuleInfo: Creator: globals kleinAndYoda foreignProcessModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: menu\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         buttonDescriptions = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'buttonDescriptions' -> () ToBe: bootstrap addSlotsTo: (
             globals generalProcessModel parent buttonDescriptions _Clone ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( |
             {} = 'Comment: Holds button descriptions:
category leaf is button name, 
slot name is button name in buttonCache,
method source is button script,
public slots make asynchronous buttons.\x7fModuleInfo: Creator: globals kleinAndYoda foreignProcessModel parent buttonDescriptions.

CopyDowns:
globals generalProcessModel parent buttonDescriptions. _Clone

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: menu\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         buttonsToPutInMenu = ( |
            | 
            resend.buttonsToPutInMenu , (nil & 'verify' & nil & 'invocationCounts' & 'blockCloneCountsByOuterMethodSlot') asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         commandButtonContents = ( |
            | 
            resend.commandButtonContents asVector,
              (
                  ('Suspend'            @ 'target          suspend: event')
                & ('Detach'             @ 'target           detach: event')
            ) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         detach: evt = ( |
            | 
            myProcess isAlive ifTrue: [|m|
              [detach].
              m: message copy receiver: myProcess Selector: 'detach'.
              m fork.
              safelyDo: [
                myOutliner update.
                updateStatus
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         getForeignVMObjectOopIfFail: fb = ( |
            | 
            myProcess stack last receiverIfFail: [ |:e| ^ fb value: 'Couldn\'t get VM oop: ', e printString]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot'
        
         importHeapInformation = ( |
            | 
            myProcess myVM importHeapInformationFrom: getForeignVMObjectOopIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateCaches: evt = ( |
            | 
            myProcess myProxy invalidateCaches.
            safelyDo: [
              myOutliner update.
              updateStatus
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         isCollapsingStackInappropriate = ( |
            | 
               myProcess myVM isNil
            || [resend.isCollapsingStackInappropriate]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         isForeignProcessModel = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Comment: child should override - -dmu 2/06\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         myVMKit = ( |
            | 
            kleinAndYoda).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalProcessModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Comment: Gets or creates the path to the memory image with 
given name. Makes attempt to keep images distinct 
between klein processes. -- Ausch, Dec. 05\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         pathToMemoryImageNamed: imageName = ( |
             prefix.
            | 
            prefix: bootstrap selfObjectsWorkingDir, '/kleinMemoryImages/'.
            (os_file exists: prefix) ifFalse: [
              os mkdir: prefix 
                  Mode: (os_file mode allRWX)  
                IfFail: [
                  ^ error: 'Unable to find or create ',prefix]
            ].
            prefix,imageName, myProcess getDebuggeePidString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         preferredColor = paint copyRed: 0.737048 Green: 0.88172  Blue: 0.761486.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         processStackModel = ( |
            | 
            myVMKit foreignProcessStackModel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         removeMemoryImageDirectory = ( |
            | 
            os command: 'rm -r', bootstrap selfObjectsWorkingDir, '/kleinMemoryImages/').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         savedFileNames = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'savedFileNames' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignProcessModel parent savedFileNames.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         startProcessWatcher = ( |
            | 
            [todo unimplemented kleinDebugger].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         suspend: evt = ( |
            | 
            myProcess isAlive ifTrue: [
              myProcess suspend.
              safelyDo: [
                myOutliner update.
                updateStatus
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: models for debugger\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcessStackModel = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalProcessStackModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignProcessStackModel.

CopyDowns:
globals generalProcessStackModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignProcessStackModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         activationModelProto = ( |
            | 
            myVMKit  sourceLevelForeignActivationModel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         buttonDescriptions = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalProcessStackModel parent buttonDescriptions _Clone ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( |
             {} = 'Comment: Holds button descriptions:
category leaf is button name, 
slot name is button name in buttonCache,
method source is button script,
public slots make asynchronous buttons.\x7fModuleInfo: Creator: globals kleinAndYoda foreignProcessStackModel parent buttonDescriptions.

CopyDowns:
globals generalProcessStackModel parent buttonDescriptions. _Clone 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         isCommentButtonWanted = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         myVMKit = ( |
            | kleinAndYoda).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalProcessStackModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: models for debugger\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         sourceLevelForeignActivationModel = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'sourceLevelForeignActivationModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals selfActivationModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'sourceLevelForeignActivationModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda sourceLevelForeignActivationModel.

CopyDowns:
globals selfActivationModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'sourceLevelForeignActivationModel' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'sourceLevelForeignActivationModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda sourceLevelForeignActivationModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'sourceLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: editing\x7fCategory: method\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         changeContentsEverywhereTo: nSlot Event: evt = ( |
             nSlotName.
             rp.
             selfHolder.
             slotName.
            | 

            [todo dependencies].
            "If we do this the parent's way, the incremental update doesn't work
             right. I think it's because mapsAffectedByChange doesn't take into
             account the possibility that we're defining an object pointed to
             by some constant slot, and so it's not smart enough to recompile
             the maps that point to that object. Maybe it should be. In the long
             run, we'll have dependency information that hopefully we can use to
             make things work properly and efficiently. But for now, overriding
             this method seems like the best way to go. -- Adam, 7/05"

             slotName:  slot name.
            nSlotName: nSlot name.
            [slotName = nSlotName] assert.

            rp: slot holder reflectionPrimitives.
            selfHolder: rp myVM image objectsOracle kleinifiedMirrorForOID: rp reflecteeOID IfAbsent: [
              error: 'There is no corresponding Self object. We do not handle this case yet.'
            ].
            selfHolder at: slotName PutContents: nSlot contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'sourceLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: editing\x7fCategory: method\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         changeReferencesToMyMethodTo: newSlot Event: evt IfFail: fb = ( |
             vmImage.
            | 
            vmImage: myVM image.
            resend.changeReferencesToMyMethodTo: newSlot Event: evt IfFail: [^ fb value].
            vmImage waitUntilDefineHasPropagated).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'sourceLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         myVM = ( |
            | 
            referrent myProcess myVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'sourceLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'selfActivationModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'sourceLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: editing\x7fCategory: method\x7fModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         referencesToContents = ( |
            | 
            [todo cleanup adam]. "Fix this to do the right thing."
            "For now, assume there's just the one reference."
            vector copyAddLast: slot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot'
        
         vmKitModels = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitModels' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitModels' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitModels.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitModels' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitModels' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitModels' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitModels' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitModels' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitModels' -> () From: ( | {
         'ModuleInfo: Module: vmKitModels InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitModels postFileIn
