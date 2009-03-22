 '$Revision: 30.8 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> () From: ( | {
         'Category: models\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         compilationUnitModel = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalCategoryModel copyForSpecialization ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 compilationUnitModel.

CopyDowns:
globals generalCategoryModel. copyForSpecialization 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 compilationUnitModel parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         buttonDescriptions = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'buttonDescriptions' -> () ToBe: bootstrap addSlotsTo: (
             globals generalCategoryModel parent buttonDescriptions _Clone ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( |
             {} = 'Comment: Holds button descriptions:
category leaf is button name, 
slot name is button name in buttonCache,
method source is button script,
public slots make asynchronous buttons.\x7fModuleInfo: Creator: globals javaUI2 compilationUnitModel parent buttonDescriptions.

CopyDowns:
globals generalCategoryModel parent buttonDescriptions. _Clone

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: annotations\x7fCategory: Add comment\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         addComment = ( |
            | 
            target addCommentEditorFor: event sourceHand).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: whole file\x7fCategory: Get AST\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         getAST = ( |
            | target model getAST: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'buttonDescriptions' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 compilationUnitModel parent buttonDescriptions parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: whole file\x7fCategory: Parse whole file\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         parseWholeFile = ( |
            | 
            target model parseCompilationUnit: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: whole file\x7fCategory: Read from file...\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         readFromFile = ( |
            | target model readFromFile: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: whole file\x7fCategory: Save\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         save = ( |
            | 
            target model saveEvent: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'buttonDescriptions' -> () From: ( | {
         'Category: whole file\x7fCategory: Save as...\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         saveAs = ( |
            | target model saveAsEvent: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         buttonsToPutInMenu = ( |
             r.
            | 
            r: list copyRemoveAll.
            r addAll: (
              case
               if:   [ isForEditingNew ]
               Then: [ 'readFromFile' & 'edit' & nil]
               If:   [ myOutliner isEditingWholeThing ]
               Then: [ 'revert' & nil      ]
               Else: [ 'edit' & nil & 'parseWholeFile' & 'getAST' & nil ]
            ) asVector.

            r addAll: (myOutliner showOrHideAnnotation & myOutliner showOrHideComment & nil) asVector.
            r addAll: ('save' & 'saveAs' & nil) asVector.
            r addAll: resend.buttonsToPutInMenu.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         categoriesUpdater = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'categoriesUpdater' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalCategoryModel parent categoriesUpdater copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'categoriesUpdater' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 compilationUnitModel parent categoriesUpdater.

CopyDowns:
globals generalCategoryModel parent categoriesUpdater. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'categoriesUpdater' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'categoriesUpdater' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 compilationUnitModel parent categoriesUpdater parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'categoriesUpdater' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         modelPrototypeForThing: t = ( |
            | 
            case
              if: [t isClass      ] Then: [javaUI2 classModel    ]
              If: [t isInterface  ] Then: [javaUI2 interfaceModel]
              If: [t categoriesString = javaParser importDcl category] Then: [javaUI2 pseudoCategoryModel]
              Else: [error: 'unanticipated thing in compilationUnit']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'categoriesUpdater' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         orderThings: t = ( |
            | 
            t "same order as in file").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'categoriesUpdater' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalCategoryModel' -> 'parent' -> 'categoriesUpdater' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: accessing things\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         categoryList = ((bootstrap stub -> 'globals') \/-> 'vector') -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: adding things\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         categoryReferrentProto = ( |
            | 
            javaUI2 javaCategoryReferrent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: accessing things\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         categoryReferrents = ( |
            | 
            isForEditingNew ifTrue: [^ vector].
            compilationUnit typeDcls).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: comment\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         comment = ( |
            | 
            isForEditingNew ifTrue: [^ 'no compilation unit yet'].
            compilationUnit comment).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: comment\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         comment: c = ( |
            | 
            isForEditingNew ifTrue: [
               [todo unimplemented].
             ^ userQuery report: 'Cannot set comment before parsing'.
            ].
            compilationUnit comment: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: comment\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         commentButtonText = ( |
            | 
            [todo cleanup "should be factored out for all java models"].
            '/* ... */').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: accessing things\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         compilationUnit = ( |
            | referrent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: accessing things\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         compilationUnit: cu = ( |
            | 
            referrent: cu.
            myOutliner stopBeingPlaceHolder.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: world background menu\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         contributeToBackgroundMenu: m = ( |
             b.
            | 
            b: ui2Button copy.
            b  scriptBlock: [event sourceHand attach: 
                            javaUI2 compilationUnitModel newOutlinerForPlaceHolder].
            b  label: 'Java File'.
            b  isAsynchronous: true.

            m addButton: b ToGroup: 'applications'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: editing whole thing\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         editWholeThingString = ( |
            | 
            isForEditingNew ifTrue: [
              'package for.you;\nimport swiss.chocolate;\nclass teroil {}\ninterface withoutCode {}'
            ]
            False: [
              compilationUnit source
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: accessing things\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         fakeSlotsIterator = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'fakeSlotsIterator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 compilationUnitModel parent fakeSlotsIterator.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'fakeSlotsIterator' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'fakeSlotsIterator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 compilationUnitModel parent fakeSlotsIterator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'fakeSlotsIterator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         mirror: m OneOfEachDo: b = ( |
             imps.
            | 
            imps: m importDcls.
            imps isEmpty ifFalse: [
              b value: imps first
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'fakeSlotsIterator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: header\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         headerButtonContents = ( |
            | 
            vector copyAddLast: dismissButtonContents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: type tests\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         isObjectModel = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         itemUpdaters = ( |
            | 
            "pseudos first, no slots in comp unit, but try it anyway"
            (pseudoCategoriesUpdater & categoriesUpdater & slotsUpdater) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: accessing things\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         mirror = ( |
            | compilationUnit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: annotation info\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         newAnnotationMorph = ( |
            | 
            javaUI2 compilationUnitAnnotationMorph 
              copyCompilationUnitOutliner: myOutliner
                          Style: annotationInfoStyle).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalCategoryModel' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: parsing\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parseCompilationUnit: evt = ( |
            | 
            compilationUnit parseSourceIfFail: [ |:err. rr |
              myOutliner editWholeThing: evt.
              rr: ui2ResultReporter copy.
              rr event: evt.
              rr editor: compilationUnitEditor text.
              rr reportTo: self.
              rr howToReport: 'finishChangingCompilationUnit:'.
              ^ rr syntaxError: err.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: appearance\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         preferredColor = paint copyRed: 0.917889 Green: 0.897361  Blue: 0.836755.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         pseudoCategoriesUpdater = bootstrap stub -> 'globals' -> 'javaUI2' -> 'abstractCategoryModel' -> 'parent' -> 'pseudoCategoriesUpdater' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: reading from a file\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         readFromFile: evt = ( |
             fn <- ''.
            | 
            fn: userQuery askString: 'File?'.
            readFromFileNamed: fn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: reading from a file\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         readFromFileNamed: fileName = ( |
             r.
            | 
            compilationUnit: javaParser compilationUnit fromFileNamed: fileName.
            safelyDo: [myOutliner update].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: accessing things\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         receiver = ( |
            | 
            [todo unimplemented]. compilationUnit "???").
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: saving to files\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         saveAsEvent: evt = ( |
            | 
            saveAsEvent: evt IfFail: [|:e| userQuery report: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: saving to files\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         saveAsEvent: evt IfFail: fb = ( |
             cfn <- ''.
             cu.
            | 
            "copy even if already have one to not disturb it"
            cu: (isForEditingNew ifTrue: [javaParser compilationUnit] False: [compilationUnit]) copy.
            myOutliner isEditingWholeThing ifTrue: [cu source: wholeThingEditor contentsString IfFail: [|:e| ^ fb value: e].
            ]. "need to get source"

            cfn: userQuery askString: 'File?'.
            compilationUnit: cu saveAs: cfn IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: saving to files\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         saveEvent: evt = ( |
            | 
            saveAsEvent: evt IfFail: [|:e| userQuery report: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: saving to files\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         saveEvent: evt IfFail: fb = ( |
            | 
            isForEditingNew || [compilationUnit fileName isEmpty]
             ifTrue: [^ saveAsEvent: evt IfFail: fb].
            compilationUnit saveIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: accessing things\x7fComment: overridden since no assignment filtering needed\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         shownSlotsInMe = ( |
            | 
            slotsInMe).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: accessing things\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         slotsInMe = ( |
             r.
            | 
            isForEditingNew ifTrue: [ ^ vector].
            r: list copyRemoveAll.
            [
              "moved package to annotation"
              compilationUnit packageDclIfPresent: [|:pd|
                r addFirst: pd
              ] IfAbsent: [].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: accessing things\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         slotsInMeAndSubcategories = ( |
            | 
            isForEditingNew ifTrue: [ ^ vector].
            [todo unimplemented]. "what to do here?"
            vector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: updating\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         slotsUpdater = bootstrap define: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'slotsUpdater' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals generalCategoryModel parent slotsUpdater copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'slotsUpdater' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 compilationUnitModel parent slotsUpdater.

CopyDowns:
globals generalCategoryModel parent slotsUpdater. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'slotsUpdater' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'slotsUpdater' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals javaUI2 compilationUnitModel parent slotsUpdater parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'slotsUpdater' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         modelPrototypeForThing: t = ( |
            | 
            "t is a slot-like think, a node summary"
            case
              if: [t isPackageDcl] Then: [javaUI2 packageDclModel]
              Else: [error: 'what other slots?']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> 'slotsUpdater' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'generalCategoryModel' -> 'parent' -> 'slotsUpdater' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: title\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         titleFontSpec = bootstrap setObjectAnnotationOf: ( fontSpec copyName: 'times' Size: 14 Style: 'bold') From: ( |
             {} = 'Comment: I am an abstract, portable, description of a font.
I am also immutable.\x7fModuleInfo: Creator: globals javaUI2 compilationUnitModel parent titleFontSpec.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: title\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         titleString = ( |
            | 
            isForEditingNew ifTrue: ['<new compilation unit>']
                             False: [ compilationUnit name copyAtMostWithEllipsis: 40]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'javaUI2' -> 'compilationUnitModel' -> 'parent' -> () From: ( | {
         'Category: editing whole thing\x7fModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         unprotectedFinishChangingWholeThing: rr = ( |
             cu.
             evt.
             s.
            | 
            evt: rr event.

            cu: javaParser compilationUnit copy.
            cu      source: rr editor contentsString  IfFail: [|:e| ^ rr syntaxError: e].
            cu                             parseSourceIfFail: [|:e| ^ rr syntaxError: e].
            cu fileName isEmpty ifFalse: [
              cu                           writeSourceIfFail: [|:e| ^ rr syntaxError: e].
            ].

            safelyDo: [
              myOutliner doneEditingWholeThing.
              myOutliner stopBeingPlaceHolder.
              compilationUnit: cu.
              myOutliner initialize update.
              myOutliner enclosingOutlinerIfPresent: [|:eo| eo update] IfAbsent: [].
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         javaUI2CUModel = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CUModel' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CUModel' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules javaUI2CUModel.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CUModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein/javaUI2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CUModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CUModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CUModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CUModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.8 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'javaUI2CUModel' -> () From: ( | {
         'ModuleInfo: Module: javaUI2CUModel InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules javaUI2CUModel postFileIn
