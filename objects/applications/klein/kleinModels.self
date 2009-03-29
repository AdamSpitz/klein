 '$Revision: 30.43 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: foreign slot models\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         abstractMachineLevelForeignSlotModel = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalSlotModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractMachineLevelForeignSlotModel.

CopyDowns:
globals generalSlotModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> () From: ( | {
         'Category: abstract foreign slot model state\x7fModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myActivationModel.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein abstractMachineLevelForeignSlotModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> () From: ( | {
         'Category: editing whole thing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         acceptEditWholeThing: str Editor: ed Event: evt = ( |
            | 
            str evalObjectBodyInContext: lobby asMirror
                Prefix: '| '
                Postfix: ' |'
                ReportingTo: 
                  newResultReporterForEditingWholeThingIn: ed Event: evt.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> () From: ( | {
         'Category: editing whole thing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         areMorphsRemovedWhenEditingWholeThing = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> () From: ( | {
         'Category: menu operations\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         buttonDescriptions = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> 'buttonDescriptions' -> () ToBe: bootstrap addSlotsTo: (
             globals generalSlotModel parent buttonDescriptions _Clone ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( |
             {} = 'Comment: Holds button descriptions:
category leaf is button name, 
slot name is button name in buttonCache,
method source is button script,
public slots make asynchronous buttons.

Includes some items used only for debugging,
so that language-specific activation models
can copyDown from here.\x7fModuleInfo: Creator: globals klein abstractMachineLevelForeignSlotModel parent buttonDescriptions.

CopyDowns:
globals generalSlotModel parent buttonDescriptions. _Clone

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> () From: ( | {
         'Category: menu operations\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         buttonsToPutInMenu = ( |
            | 
            ('edit' & 'hideMe' & 'followPointer' & nil & 'yank') asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> () From: ( | {
         'Category: menu operations\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         followPointer: evt = ( |
            | 
            myActivationModel showMemoryLocation: slot value reflectee Event: evt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> () From: ( | {
         'Category: contents label string\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         formatOneLinerContentsStringFrom: n = ( |
            | 
            n = 0  ifTrue: [^ '0'].
            n shortIfPossibleHexPrintString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> () From: ( | {
         'Category: comment\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         isCommentButtonWanted = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         newOutlinerFor: aSlot ActivationModel: aForeignActivationModel = ( |
             r.
            | 
            r: newOutlinerFor: aSlot.
            r model myActivationModel: aForeignActivationModel.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> () From: ( | {
         'Category: sprouting\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         objectModelProto = ( |
            | selfObjectModel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> () From: ( | {
         'Category: contents label string\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         oneLinerContentsString = ( |
             n.
            | 
            n: (slot valueIfFail: [|:e| ^ e]) reflectee.
            slot exists ifFalse: [^ n error].
            formatOneLinerContentsStringFrom: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalSlotModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         receiver = ( |
            | slot holder).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: models for debugger\x7fCategory: helpers\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         disassembledMethodText = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals abstractMethodText copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'disassembledMethodText' -> () From: ( |
             {} = 'Comment: language-independent method text\x7fModuleInfo: Creator: globals klein disassembledMethodText.

CopyDowns:
globals abstractMethodText. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: foreign slot models\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignRegisterSlotModel = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein abstractMachineLevelForeignSlotModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignRegisterSlotModel.

CopyDowns:
globals klein abstractMachineLevelForeignSlotModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> () From: ( |
             {} = 'Comment: This may be just a temporary hack;
it shows a machine register in a debugger
as if it were a pseudo-slot. -- dmu 1/02\x7fModuleInfo: Creator: globals klein foreignRegisterSlotModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> () From: ( | {
         'Category: menu operations\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         buttonDescriptions = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> 'buttonDescriptions' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein abstractMachineLevelForeignSlotModel parent buttonDescriptions _Clone ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( |
             {} = 'Comment: Holds button descriptions:
category leaf is button name, 
slot name is button name in buttonCache,
method source is button script,
public slots make asynchronous buttons.

Includes some items used only for debugging,
so that language-specific activation models
can copyDown from here.\x7fModuleInfo: Creator: globals klein foreignRegisterSlotModel parent buttonDescriptions.

CopyDowns:
globals klein abstractMachineLevelForeignSlotModel parent buttonDescriptions. _Clone 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignRegisterSlotModel parent buttonDescriptions parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> () From: ( | {
         'Category: editing whole thing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         editNewWholeThingString = ( |
            | 
            'r4 <- 17').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> () From: ( | {
         'Category: menu operations\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         hideMe: event = ( |
            | 
            myActivationModel hideRegisterSlot: slot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> () From: ( | {
         'Category: editing whole thing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         unprotectedFinishChangingWholeThing: rr = ( |
             contents.
             eo.
             name.
             newSlot.
            | 
            newSlot: (reflect: rr result) first.
            name: newSlot name.
            contents: newSlot contents.
            name = slot name ifFalse: [
              rr event sourceHand attach:
                syntaxErrorMorph
                  copyTitle: 'Cannot change name of register'
                    Message: 'from: ', slot name, ' to: ', name.
              ^ self.
            ].
            slot contents: contents IfFail: [|:e|
              rr event sourceHand attach:
                syntaxErrorMorph 
                  copyTitle: 'Could not change contents' Message: e.
              ^ self.
            ].
            eo: myOutliner enclosingOutlinerIfPresent: [|:o| o]
                                             IfAbsent: [nil].
            myOutliner doneEditingWholeThing.
            safelyDo: [
              myOutliner cancelEditingWholeThingEditor: rr editor Event: rr event.
              eo ifNotNil: [eo safelyDo: [eo update]].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: foreign slot models\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignConditionCodeSlotModel = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein foreignRegisterSlotModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignConditionCodeSlotModel.

CopyDowns:
globals klein foreignRegisterSlotModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> () From: ( | {
         'Category: foreign condition code slot model state\x7fModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (true)\x7fVisibility: private'
        
         isShownAsString <- bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignConditionCodeSlotModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> () From: ( | {
         'Category: menu operations\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         buttonDescriptions = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> 'buttonDescriptions' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein foreignRegisterSlotModel parent buttonDescriptions _Clone ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( |
             {} = 'Comment: Holds button descriptions:
category leaf is button name, 
slot name is button name in buttonCache,
method source is button script,
public slots make asynchronous buttons.

Includes some items used only for debugging,
so that language-specific activation models
can copyDown from here.\x7fModuleInfo: Creator: globals klein foreignConditionCodeSlotModel parent buttonDescriptions.

CopyDowns:
globals klein foreignRegisterSlotModel parent buttonDescriptions. _Clone 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignConditionCodeSlotModel parent buttonDescriptions parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: formatting contents string\x7fCategory: Show as number\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         showAsNumber = ( |
            | 
            target model showAsNumber).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: formatting contents string\x7fCategory: Show as string\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         showAsString = ( |
            | target model showAsString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> () From: ( | {
         'Category: menu operations\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         buttonsToPutInMenu = ( |
            | 
            ('edit' & contentsFormatButton & nil & 'yank' ) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> () From: ( | {
         'Category: menu operations\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         contentsFormatButton = ( |
            | isShownAsString ifTrue: ['showAsNumber'] False: ['showAsString']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> () From: ( | {
         'Category: contents label string\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         formatOneLinerContentsStringFrom: n = ( |
            | 
            isShownAsString ifFalse: [
              ^ resend.formatOneLinerContentsStringFrom: n
            ].
            slot holder myAssemblerSystem disassembler stringForConditionCodeValue: n).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'foreignRegisterSlotModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> () From: ( | {
         'Category: contents label string\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         showAsNumber = ( |
            | 
            isShownAsString: false.
            safelyDo: [myOutliner update].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignConditionCodeSlotModel' -> 'parent' -> () From: ( | {
         'Category: contents label string\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         showAsString = ( |
            | 
            isShownAsString: true.
            safelyDo: [myOutliner update].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: foreign slot models\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignMemorySlotModel = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlotModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein abstractMachineLevelForeignSlotModel copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlotModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignMemorySlotModel.

CopyDowns:
globals klein abstractMachineLevelForeignSlotModel. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlotModel' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlotModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignMemorySlotModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlotModel' -> 'parent' -> () From: ( | {
         'Category: editing whole thing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         editNewWholeThingString = ( |
            | 
            'M[16ra00] <- 17').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlotModel' -> 'parent' -> () From: ( | {
         'Category: menu operations\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         hideMe: event = ( |
            | myActivationModel hideMemorySlot: slot).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlotModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'abstractMachineLevelForeignSlotModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignMemorySlotModel' -> 'parent' -> () From: ( | {
         'Category: editing whole thing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         unprotectedFinishChangingWholeThing: rr = ( |
             contents.
             eo.
             name.
             newSlot.
            | 

            [todo unimplemented kleinDebugger editingForeignMemorySlotModel].
            error: 'unimplemented'.

            newSlot: (reflect: rr result) first.
            name: newSlot name.
            contents: newSlot contents.
            name = slot name ifFalse: [
              rr event sourceHand attach:
                syntaxErrorMorph
                  copyTitle: 'Cannot change name of register'
                    Message: 'from: ', slot name, ' to: ', name.
              ^ self.
            ].
            slot contents: contents IfFail: [|:e|
              rr event sourceHand attach:
                syntaxErrorMorph 
                  copyTitle: 'Could not change contents' Message: e.
              ^ self.
            ].
            eo: myOutliner enclosingOutlinerIfPresent: [|:o| o]
                                             IfAbsent: [nil].
            myOutliner doneEditingWholeThing.
            safelyDo: [
              myOutliner cancelEditingWholeThingEditor: rr editor Event: rr event.
              eo ifNotNil: [eo safelyDo: [eo update]].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: models for debugger\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcessModel = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda foreignProcessModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignProcessModel.

CopyDowns:
globals kleinAndYoda foreignProcessModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> () From: ( | {
         'Category: foreign process model state\x7fModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (klein foreignProcessModel savedFileNames)\x7fVisibility: private'
        
         mySavedFileNames <- bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'savedFileNames' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignProcessModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fCategory: snapshotting\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         autoLoadPrevious = ( |
            | loadSnapshotFromFile: pathToMemoryImageNamed: savedFileNames previousFileNameIfFail: [ ^ error: 'couldn\'t load previous memory image']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         autoSaveEnabled = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fCategory: snapshotting\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         autoSaveNext = ( |
            | 
            saveSnapshotToFile: pathToMemoryImageNamed: savedFileNames nextFileNameIfFail: [^ error: 'couldn\'t save next memory image']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         back: evt = ( |
            | 
            autoSaveEnabled ifTrue: [autoLoadPrevious] False: [error: 'Auto Saving not enabled']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         commandButtonContents = ( |
            | 
            resend.commandButtonContents asVector,
              (
                  ('Multi-step'         @ 'target        multiStep: event')
                & ('Save snapshot'      @ 'target     saveSnapshot: event')  
                & ('Load snapshot'      @ 'target     loadSnapshot: event') 
              ) asVector, 
            (restartEnabled ifTrue: [ 
              vector copyAddLast: 
                  ('Restart'           @ 'target          restart: event')]
             False: [ vector copyRemoveAll ]),
            (autoSaveEnabled ifTrue: [
              vector copyAddLast: 
                  ('Back'              @ 'target             back: event')]
             False: [ vector copyRemoveAll ])).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         continue: evt = ( |
            | 
            autoSaveEnabled ifTrue:[ autoSaveNext].
            resend.continue: evt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         copy = ( |
            | 
            resend.copy mySavedFileNames: mySavedFileNames copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         debugProcess: p = ( |
             r.
            | 
            r: resend.debugProcess: p.
            r model generateRestartPointIfNecessary.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fCategory: snapshotting\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         generateRestartPoint = ( |
            | 
            saveSnapshotToFile: 
               pathToMemoryImageNamed: 'restartPoint').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fCategory: snapshotting\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         generateRestartPointIfNecessary = ( |
            | 
            restartEnabled ifTrue: [generateRestartPoint]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fCategory: snapshotting\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         getStackRegions = ( |
            | 
            getStackRegionsIfFail: [^ error: 'could not get stack regions']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fCategory: snapshotting\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         getStackRegionsIfFail: fb = ( |
             r.
             regions.
             sp.
             stackSize <- 0.
            | 
            r: list copyRemoveAll.
            sp: myProcess spIfFail: [^ vector].
            myProcess stack do: [|:act. last|
              last: stackSize. 
              stackSize: stackSize + (act frameSizeIfFail:  [^ fb value: 'could not read stack frame']).
              (r addLast: (sp + last)) addLast: (sp + stackSize)
            ].
            "((list copyRemoveAll) 
                     addLast: sp)
                     addLast: sp + stackSize"
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fCategory: snapshotting\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         loadSnapshot: evt = ( |
            | 
            loadSnapshotFromFile:
               pathToMemoryImageNamed: userQuery askString: 'Which file to load from?'.
            importHeapInformation).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fCategory: snapshotting\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         loadSnapshotFromFile: fileName = ( |
            | 
            (os_file exists: fileName) ifFalse: [report: 'file does not exist. Memory not altered'. ^ self].
            safelyDo: [ | regions|
              myProcess readMemoryFrom: fileName.
              myProcess myProxy invalidateCaches.
              myOutliner update.
              updateStatus.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         multiStep: evt = ( |
             steps.
            | 
            steps:  (userQuery askString: 'How many steps?') asInteger.
            safelyDo: [
              myProcess multiStep: steps TimesOrToPC: 0 IfFail: [|:e| error: e].
              myOutliner update.
              updateStatus.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         myVMKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fCategory: snapshotting\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         restart: evt = ( |
            | 
            loadSnapshotFromFile:
               pathToMemoryImageNamed:  'restartPoint').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         restartEnabled = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fCategory: snapshotting\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         saveSnapshot: evt = ( |
            | 
            importHeapInformation.
            saveSnapshotToFile:   
              pathToMemoryImageNamed: userQuery askString: 'Image name:').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'Category: commands\x7fCategory: snapshotting\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         saveSnapshotToFile: fileName = ( |
            | 
            safelyDo: [ | regions. aString <- ''|
              regions: referrent allAllocatedHeapRegions asVector, getStackRegions asVector.
              regions: regions copySort mapBy: [|:i| i asInt32].
              myProcess writeMemoryTo: fileName AddressRanges: regions.
              myOutliner update.
              updateStatus.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         titleString = ( |
            | 'Klein ', resend.titleString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: models for debugger\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         foreignProcessStackModel = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda foreignProcessStackModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignProcessStackModel.

CopyDowns:
globals kleinAndYoda foreignProcessStackModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (true)'
        
         isSourceLevel <- bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignProcessStackModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         activationModelProto = ( |
            | 
            isSourceLevel
              ifTrue: [ myVMKit  sourceLevelForeignActivationModel ]
               False: [ myVMKit machineLevelForeignActivationModel ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         buttonDescriptions = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda foreignProcessStackModel parent buttonDescriptions _Clone ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( |
             {} = 'Comment: Holds button descriptions:
category leaf is button name, 
slot name is button name in buttonCache,
method source is button script,
public slots make asynchronous buttons.\x7fModuleInfo: Creator: globals klein foreignProcessStackModel parent buttonDescriptions.

CopyDowns:
globals kleinAndYoda foreignProcessStackModel parent buttonDescriptions. _Clone 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: modes\x7fCategory: Machine Level\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         machineLevel = ( |
            | target model setSourceLevel: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein foreignProcessStackModel parent buttonDescriptions parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignProcessStackModel parent buttonDescriptions parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: modes\x7fCategory: Source Level\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         sourceLevel = ( |
            | target model setSourceLevel: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         buttonsToPutInMenu = ( |
            | 
            resend.buttonsToPutInMenu copyAddFirst:
              isSourceLevel ifTrue: 'machineLevel' False: 'sourceLevel').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         myVMKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessStackModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'foreignProcessStackModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         setSourceLevel: bool = ( |
            | 
            isSourceLevel: bool.
            myProcess isSourceLevel: bool.
            activationMorphs do: [|:m| m delete].
            myOutliner update).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: models\x7fCategory: models for debugger\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         machineLevelForeignActivationModel = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'cachedIsMethod' From:
             bootstrap remove: 'parent' From:
             globals kleinAndYoda abstractMachineLevelForeignActivationModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein machineLevelForeignActivationModel.

CopyDowns:
globals kleinAndYoda abstractMachineLevelForeignActivationModel. copyForSpecialization 
SlotsToOmit: cachedIsMethod parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> () From: ( | {
         'Category: disassembledForeignActivationModel state\x7fModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         memoryAddressesToShow <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> () From: ( | {
         'Category: disassembledForeignActivationModel state\x7fModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         myMethodText.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> () From: ( | {
         'Category: operands selector state\x7fModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         opsSelMorph.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein machineLevelForeignActivationModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: editing\x7fCategory: editing contents\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         acceptContents: str Editor: ed Event: evt = ( |
            | 
            forChangedLinesFrom: myMethodText lines
                             To: ed text contents
                             Do: [|:lineIndex. :newInstrString|
              changeInstructionAt: lineIndex To: newInstrString Editor: ed Event: evt IfFail: [^ self].
            ].
            myOutliner contentsEditor clearPanel.
            safelyDo: [ myOutliner update ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: locals\x7fCategory: conidition codes\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         addConditionCodeSlots = ( |
             ccName.
             condCodeOutliner.
             condCodeSlot.
            | 

            ccName: activation myAssemblerSystem conditionCodeRegisterName.
            condCodeSlot: vmKit foreignRegisterSlot copyMirror: activation Name: ccName.
            condCodeOutliner: vmKit foreignConditionCodeSlotModel 
               newOutlinerFor: condCodeSlot ActivationModel: activation.
            myOutliner addItemLast: condCodeOutliner. 

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: memory\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         addMemorySlots = ( |
            | 
            myOutliner addItems:
             (memoryAddressesToShow asVector sort
              copyMappedBy: [|:a| activation memorySlotForAddress: a])
              copyMappedBy: [|:s| vmKit foreignMemorySlotModel newOutlinerFor: s ActivationModel: self].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: locals\x7fCategory: registers\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         addRegisterSlot: aRegSlot = ( |
            | 
            myOutliner addItemLast: 
              vmKit foreignRegisterSlotModel newOutlinerFor: aRegSlot
                                      ActivationModel: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: locals\x7fCategory: registers\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         addRegisterSlots = ( |
            | 
            visibleRegisterSlots do: [|:r|
              addRegisterSlot: r
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: locals\x7fCategory: registers\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         allRegisters = ( |
            | 
            activation myAssemblerSystem allRegisters).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: event handling\x7fComment: My outliner sends me this so
I can intercept events before
they go to my outliner\'s morphs.
  -- dmu & ma 3/02\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         allowSubmorphsToGetEvent: evt = ( |
            | 
            evt middleMouseDown not
            || [(shouldPopUpProcessControlMenu: evt) not]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: selecting operands\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         areOperandSelectorsShown = ( |
            | 
            opsSelMorph isNotNil && [opsSelMorph isInWorld]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: modes\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         beGeneral = ( |
            | 
            myMethodText beGeneral. safelyDo: [myOutliner update]. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: modes\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         beSpecific = ( |
            | 
            myMethodText beSpecific. safelyDo: [myOutliner update]. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: locals\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         buildLocals = ( |
            | 
            safelyDo: [
              |wereOperandSelectorsShown|
              wereOperandSelectorsShown: areOperandSelectorsShown.
              myOutliner removeAllItems.
              wereOperandSelectorsShown ifTrue: [myOutliner addItem: opsSelMorph].
              addConditionCodeSlots.
              addRegisterSlots.
              addMemorySlots.
              myOutliner colorAll: myOutliner color.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: title\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         buildTitle = ( |
            | 
            buildSlotTitle).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         buttonDescriptions = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalActivationModel parent buttonDescriptions _Clone ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( |
             {} = 'Comment: Holds button descriptions:
category leaf is button name, 
slot name is button name in buttonCache,
method source is button script,
public slots make asynchronous buttons.\x7fModuleInfo: Creator: globals klein machineLevelForeignActivationModel parent buttonDescriptions.

CopyDowns:
globals generalActivationModel parent buttonDescriptions. _Clone 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         buttonsToPutInMenu = ( |
             disMode.
             ieMode.
             opsSelMode.
             r.
            | 
            isForEditingNew ifTrue: [^ vector].
            r: list copyRemoveAll.
            disMode: isSpecific ifTrue: 'beGeneral' False: 'beSpecific'.
            ieMode: isInternal ifTrue: 'showExternalForm' False: 'showInternalForm'.
            opsSelMode: areOperandSelectorsShown ifTrue: 'hideOperandSelectors'
                                                  False: 'showOperandSelectors'.
            r addAll: (disMode & ieMode & nil) asVector.
            r addAll: (opsSelMode & nil) asVector.
            r addAll: ('showMemoryLocation' & 'showMemoryWords' & 'showObject' &  nil) asVector.
            r addLast: 'yank'.

            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: editing\x7fCategory: editing contents\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         changeInstructionAt: selectionRect To: newInstrString Editor: ed Event: evt IfFail: fb = ( |
             address.
             asm.
             newInstrBytes.
             rr.
             selection.
            | 
            "Need to look into copyOrigin: arg - mike"
            address: myMethodText pcForPoint: selectionRect origin.
            asm: activation myAssemblerSystem assembler copyOrigin: address.
            rr: newResultReporterForChangingInstructionAt: address InSelection: selectionRect
                                           AssemblingWith: asm Editor: ed Event: evt.

            (myMethodText isCommentAtPC: address) ifTrue: [
              rr reportError: ('' parseErrorPt copy message: 'cannot change a comment') Title: 'Sorry'.
              ^ fb value: 'cannot change a comment'
            ].
            newInstrString evalObjectBodyInContext: (reflect: asm)
                                       ReportingTo: rr.
            rr wasSuccessful ifFalse: [ fb value: 'failed' ] True: [ self ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: editing\x7fCategory: editing contents\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         contentsEditorPrototype = bootstrap stub -> 'globals' -> 'uglyActivationEditorMorph' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | resend.copy memoryAddressesToShow: memoryAddressesToShow copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: editing\x7fCategory: editing contents\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         finishChangingInstruction: rr = ( |
             addr.
             newInstrBytes.
            | 
            "Write assembled bytes to memory"
            addr: rr address.
            newInstrBytes:   rr assembler assembledBytesFrom: addr
                                                        UpTo: addr + myMethodText instructionSize.
            activation write: newInstrBytes
                  ToMemoryAt: addr
                      IfFail: [|:e| rr reportError: snort Title: 'Write failed: ', el].
            myOutliner cachedContentsString: ''. "force updateContentsEditor to work"
            rr wasSuccessful: true.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: editing\x7fCategory: editing contents\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         forChangedLinesFrom: oldLines To: newLines Do: aBlk = ( |
            | 
            oldLines with: newLines Do: [|:oldLine. :newLine. :lineIndex. colonPositionPlusTwo.
                                          newLineAfterAddressLabel. selectionRect |
              oldLine = newLine ifFalse: [| oldAddress. newAddress |
                oldAddress: oldLine asTokensSeparatedByWhiteSpace firstIfAbsent: ''.
                newAddress: newLine asTokensSeparatedByWhiteSpace firstIfAbsent: ''.
                oldAddress = newAddress ifFalse: [ 
                  error: 'Expected address ', oldAddress, ' but found address ', newAddress
                ].
                colonPositionPlusTwo: newLine findFirst: [|:c| c = ':']
                                              IfPresent: [|:c. :i| i + 2]
                                               IfAbsent: [error: 'expected a colon after instruction address'].
                selectionRect: (colonPositionPlusTwo @ lineIndex) # (newLine size @ lineIndex).
                newLineAfterAddressLabel: newLine copyFrom: colonPositionPlusTwo.
                aBlk value: selectionRect With: newLineAfterAddressLabel.
              ]
            ].

            oldLines size = newLines size ifFalse: [
              error: 'Instructions were added or removed at end'
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: memory\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         getInt: prompt Event: evt IfNone: noneBlk = ( |
             a.
             s <- '16rbffc9e20' copyMutable.
            | 
            s: userQuery askString: prompt.
            s isEmpty ifTrue: [^ noneBlk value].
            (s size = 11)  &&  ['16r' isPrefixOf: s]  ifTrue: [
              [|:exit. hi. lo|
                 hi: s copySize: 7.
                 lo: '16r', (s copyFrom: 7).
                 ^ int32 fromHigh16Bits: (hi evalIfFail: exit)
                              Low16Bits: (lo evalIfFail: exit)
              ] exit.
            ].
            a: s evalIfFail: [|:e. m. world|
              showError: e printString Event: evt.
              ^ noneBlk value
            ].
            a asMirror isReflecteeInteger ifFalse: [
              showError: s, ' is not an integer'
                  Event: evt.
              ^ noneBlk value
            ].
            a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: memory\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         hideMemorySlot: aMemorySlot = ( |
            | 
            memoryAddressesToShow remove: aMemorySlot address.
            constructItems).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: selecting operands\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         hideOperandSelectors: event = ( |
            | 
            safelyDo: [myOutliner removeItem: opsSelMorph].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: locals\x7fCategory: registers\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         hideRegisterSlot: regSlot = ( |
             gpr.
             opSel.
            | 

            gpr: regSlot key sendTo: activation myAssemblerSystem operands. 
            opSel: opsSelMorph operandSelectors at: gpr.

            opSel tristate setStateNo.
            constructItems.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: copying & creating\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            resend.initialize.
            memoryAddressesToShow: memoryAddressesToShow copyRemoveAll.
            myMethodText: vmKit disassembledMethodText copyForActivation: activation.
            opsSelMorph:  operandSelectorsMorph  copyForActivationModel: self Operands: allRegisters).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: editing\x7fCategory: editing contents\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         instructionPatchingResultReporter = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'instructionPatchingResultReporter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'failedInProcess:' From:
             bootstrap remove: 'reportError:Title:' From:
             globals ui2ResultReporter copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'instructionPatchingResultReporter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein machineLevelForeignActivationModel parent instructionPatchingResultReporter.

CopyDowns:
globals ui2ResultReporter. copy 
SlotsToOmit: failedInProcess: reportError:Title:.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'instructionPatchingResultReporter' -> () From: ( | {
         'Category: instruction patching\x7fModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         address <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'instructionPatchingResultReporter' -> () From: ( | {
         'Category: instruction patching\x7fModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         assembler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'instructionPatchingResultReporter' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         failedInProcess: p = ( |
            | 
            selectInstructionInEditor).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'instructionPatchingResultReporter' -> () From: ( | {
         'Category: error reporting\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         reportError: errObj Title: t = ( |
            | 
            event sourceHand safelyDo: [
              selectInstructionInEditor.
              event sourceHand attach: 
                syntaxErrorMorph copyTitle: t, ':' Message: errObj printString.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'instructionPatchingResultReporter' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         selectInstructionInEditor = ( |
            | 
            event sourceHand safelyDo: [
              editor selectionStart: selectionRect origin
                                End: selectionRect corner.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'instructionPatchingResultReporter' -> () From: ( | {
         'Category: instruction patching\x7fModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         selectionRect.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'instructionPatchingResultReporter' -> () From: ( | {
         'Category: instruction patching\x7fModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         wasSuccessful <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: comment\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         isCommentButtonWanted = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: modes\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         isInternal = ( |
            | myMethodText isInternal).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: modes\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         isSpecific = ( |
            | myMethodText isSpecific).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: accessing the slot\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         machineLevelSlot = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'machineLevelSlot' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein machineLevelForeignActivationModel parent machineLevelSlot.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'machineLevelSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         activationMirror.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'machineLevelSlot' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'machineLevelSlot' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein machineLevelForeignActivationModel parent machineLevelSlot parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'machineLevelSlot' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForActivation: a = ( |
            | 
            copy activationMirror: a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'machineLevelSlot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         exists = ( |
            | 
            activationMirror myProcess isAlive).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'machineLevelSlot' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         isMethod = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'machineLevelSlot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         longKey = ( |
            | 
            '<disassembled: ',
            (activationMirror selectorIfFail: [|:e| 'no selector: ', e]), ' ',
             activationMirror sp hexPrintString,
            '>').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'machineLevelSlot' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         name = ( |
            | 
            activationMirror name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> 'machineLevelSlot' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: accessing the slot\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         methodString = ( |
            | 
            myMethodText update asString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: accessing the slot\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         methodText = ( |
            | 
            vmKit disassembledMethodText).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: event handling\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         middleMouseDown: evt = ( |
            | 
            (shouldPopUpProcessControlMenu: evt) ifTrue: [
              popUpProcessControlMenu: evt
            ] False: [
              resend.middleMouseDown: evt
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: editing\x7fCategory: editing contents\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         newResultReporterForChangingInstructionAt: address InSelection: selectionRect AssemblingWith: asm Editor: ed Event: evt = ( |
            | 
            [finishChangingInstruction: nil]. "browsing"
            ((((((instructionPatchingResultReporter copy
              event: evt)
              editor: ed text)
              reportTo: self)
              address: address)
              selectionRect: selectionRect)
              assembler: asm)
              howToReport: 'finishChangingInstruction:').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: execution commands\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         nextCommand: evt = ( |
            | 
            activation number != 0 ifTrue: [
              myProcess finish:  myProcess at: activation number pred
            ]
            False: [
              myProcess step.
                   myProcess isAlive
              && [ myProcess currentActivation hasSender
              && [ myProcess currentActivation sender = activation ]]
                   ifTrue: [ myProcess finish ]
            ].
            stepOverUninterestingStuff.
            updateAfterStepping: evt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: contents label string\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         oneLinerContentsString = ( |
            | 
            myMethodText oneLiner).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: selecting operands\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         operandSelectorsChanged = ( |
            | 
            constructItems.
            myOutliner update).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMachineLevelForeignActivationModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: event handling\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         pcFor: pt = ( |
             gridPoint.
             pc.
            | 
            gridPoint: myOutliner contentsEditor ptToGrid: pt.
            pc: myMethodText pcForPoint: gridPoint.
            pc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         popUpProcessControlMenu: evt = ( |
             m.
            | 

            m: ui2Menu copy.
            m addButtonTarget: self ScriptBlock: [target setPC: event ToLocation: button owner invocationPoint] Label: 'Set PC To Here'.
            m addButtonTarget: self ScriptBlock: [target runTo: event ToLocation: button owner invocationPoint] Label: 'Run To Here'.
            m addButtonTarget: self ScriptBlock: [target stepAction] Label: 'Step'.

            m popUp: evt.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: locals\x7fCategory: registers\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         relevantOperands = ( |
            | 
            myMethodText symbolicOperands).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: event handling\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         runTo: evt ToLocation: pt = ( |
             selectedPC.
            | 

            selectedPC: pcFor: pt.
            activation myProcess multiStep: 5000
                               TimesOrToPC: selectedPC
                                    IfFail: raiseError.
            updateAfterStepping: evt.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: method source & selection\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         setMethodSelection = ( |
             pos.
             selectionStartAndEnd.
            | 
            pos: activation pcIfFail: [^ self].
            selectionStartAndEnd:  myMethodText convertPCToSelection: pos.
                "Don't redundently set selection; forces scroll on the user"
                (myOutliner contentsEditor text insertionPoint = selectionStartAndEnd first)
            &&  [myOutliner contentsEditor text selectionEnd   = selectionStartAndEnd last]
             ifFalse: [
               myOutliner contentsEditor text
                 selectionStart: selectionStartAndEnd first End: selectionStartAndEnd last.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: event handling\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         setPC: evt ToLocation: pt = ( |
             currentPC.
             selectedPC.
            | 

            currentPC:  activation pcIfFail: raiseError.
            selectedPC: pcFor: pt.

            currentPC = selectedPC ifTrue: [ ^ self ].

            activation setPC: selectedPC IfFail: raiseError.
            updateAfterStepping: evt.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: event handling\x7fComment: Only display \"Set PC\" and \"Run to Here\" menu items
when the middle mouse down is pressed on the editor\'s
text widget. -ma 3/02\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldPopUpProcessControlMenu: evt = ( |
            | 
                myOutliner isExpanded
            && [myOutliner contentsEditor globalBounds includes: evt cursorPoint]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: memory\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         showError: s Event: evt = ( |
             m.
             world.
            | 
            m: syntaxErrorMorph copyMessage: s.
            world: evt sourceHand world.
            world safelyDo: [  
              world addMorph: m.
              m globalPosition: evt cursorPoint.
              world moveToFront: m.  
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: modes\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         showExternalForm = ( |
            | 
            myMethodText showExternalForm.
            safelyDo: [myOutliner update].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: modes\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         showInternalForm = ( |
            | 
            myMethodText showInternalForm.
            safelyDo: [myOutliner update].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: memory\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         showMemoryLocation: addr Event: evt = ( |
            | 
            memoryAddressesToShow add: addr.
            myOutliner expand: evt.
            myOutliner isExpanded ifTrue: [constructItems].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: memory\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         showMemoryLocationEvent: evt = ( |
            | 
            showMemoryLocation: (getInt: 'Address?' Event: evt IfNone: [^ self])
                         Event: evt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: memory\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         showMemoryWords: addr Size: howMany Event: evt = ( |
            | 
            evt sourceHand attach: world outlinerForMirror: 
              activation myProcess theVM setTheVMAndDo: [activation wordsAt: addr Size: howMany].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: memory\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         showMemoryWordsEvent: evt = ( |
             addr.
             size.
            | 
            addr: getInt: 'Address?'  Event: evt  IfNone: [^self].
            size: getInt: 'How Many?' Event: evt  IfNone: -1.

            showMemoryWords: (addr && vmKit layouts abstract oopSize negate)
                       Size: size
                      Event: evt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: memory\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         showObject: evt = ( |
            | 
            showObjectAt: (getInt: 'Oop?' Event: evt IfNone: [^ self])
                   Event: evt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: memory\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         showObjectAt: oop Event: evt = ( |
            | 
            evt sourceHand attach:
              world outlinerForMirror:
                activation myProcess theVM mirrorFor: oop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: selecting operands\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         showOperandSelectors: event = ( |
            | 
            myOutliner expand: event.
            opsSelMorph colorAll: myOutliner color.
            safelyDo: [
              myOutliner addItemFirst: opsSelMorph.
              opsSelMorph alignSelectors.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: accessing the slot\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         slot = ( |
            | 
            machineLevelSlot copyForActivation: referrent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: execution commands\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         stepAction = ( |
            | 
            activation number = 0 ifTrue: [
              myProcess stepOneInstruction.
            ] False: [
              myProcess finish: myProcess at: activation number pred
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: execution commands\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         stepOverUninterestingStuff = ( |
            | 
            stepToNextComment).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: execution commands\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         stepToNextComment = ( |
             lastPC <- -1.
             n <- 0.
            | 
            [  |pc|
              pc: activation pcIfFail: [^ self].
              pc = lastPC ifTrue: [^ self].
              lastPC: pc.
              (n < 10) && [myMethodText isCommentAtPC: pc]
            ] whileFalse: [n: n succ. myProcess stepOneInstruction].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: locals\x7fCategory: registers\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         visibleOperands = ( |
            | 
            "Visible operands are determined as follows:
             1) Create a set containing all relevant operands
             2) Iterate through the operand selector morph's list of operands.
                For every item there, it can be in one of three states:
                a) force to show: add it to the above set
                b) force to hide: remove it from the above set
                c) show if relevant: do nothing (operands is already in the
                                     set from step 1)."
            (opsSelMorph selectOperandsGivenDefaults: relevantOperands) asVector 
              sort).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'Category: locals\x7fCategory: registers\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         visibleRegisterSlots = ( |
            | 
            activation registerSlotsForOperands: visibleOperands).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'machineLevelForeignActivationModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractMachineLevelForeignActivationModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalActivationModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: verifying\x7fCategory: Verify\x7fModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         verify = ( |
            | 
            target model verifyVM: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'savedFileNames' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         currentFileAt <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'savedFileNames' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         maxSavedFiles = 10.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'savedFileNames' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'savedFileNames' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignProcessModel parent savedFileNames parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'savedFileNames' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         makeFileName = ( |
            | 
            'savePoint', (currentFileAt asString)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'savedFileNames' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         nextFileNameIfFail: fb = ( |
            | 
            currentFileAt: currentFileAt succ.
            (spaceUsed < maxSavedFiles) ifTrue: [spaceUsed: spaceUsed succ].
            (currentFileAt < maxSavedFiles) ifFalse: [currentFileAt: 0].
            makeFileName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'savedFileNames' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'savedFileNames' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         previousFileNameIfFail: fb = ( |
             name.
            | 
            name: makeFileName.
            (spaceUsed = 0) ifTrue: [ userQuery report: 'no previous file available for loading'].
            currentFileAt: currentFileAt pred.
            spaceUsed: spaceUsed pred.
            name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProcessModel' -> 'parent' -> 'savedFileNames' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (0)'
        
         spaceUsed <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         kleinModels = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinModels' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinModels' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinModels.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinModels' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinModels' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinModels' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinModels' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinModels' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.43 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinModels' -> () From: ( | {
         'ModuleInfo: Module: kleinModels InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinModels postFileIn
