 '$Revision: 30.2 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'layouts' -> 'abstract' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         cClassName = ( |
            | 
            name capitalize, 'Layout').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         cName = ( |
            | 
            cNamePrefix, '_', name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         cNamePrefix = ( |
            | 
            asMirror creatorPath copyWithoutLast last).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         gatherIncludeFileInformationFor: s = ( |
             mf.
             pf.
            | 
            mf: (markMightEncodeMyValueForLayout: klein layouts memoryObject) ifFalse: 'NULL' True: ['MarkLayout().', bitFieldInMark name, '()'].
            pf: precedingField ifNil: 'NULL' IfNotNil: [|:f| f cName].
            s variableType: valueMixin cClassName, '*' Name: cName Value: 'new ', valueMixin cClassName, '(', mf, ', ', pf, ')'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'layouts' -> 'memoryObject' -> 'immediateValueHeaderFieldMixin' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         cClassName = ( |
            | 
            immediateLayoutForValue name capitalize, 'ValueHeaderField').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'layouts' -> 'memoryObject' -> 'oopValueHeaderFieldMixin' -> () From: ( | {
         'ModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         cClassName = 'OopValueHeaderField'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         specialFieldsAfterHeaderDo: blk = ( |
             r.
            | 
            r: list copyRemoveAll.
            [|:exit|
             lastField reverseDo: [|:f| f = lastHeaderField ifTrue: exit False: [r addFirst: f]]] exit.
            r do: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'foreignMap') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldIncludeInSmallImage = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         cClassDeclaration = ( |
            | 
            cClassName, ' : public ', cSuperclassName).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         cClassName = ( |
             p.
            | 
            p: asMirror creatorPath.
            p last = 'parent' ifTrue: [p: p copyWithoutLast].
            p last capitalize, 'Layout').
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         cSuperclassName = ( |
            | 
            == klein maps map parent ifTrue: [klein layouts objVector cClassName] False: [parent cClassName]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         gatherHeaderInformationFor: s = ( |
            | 
            s class: parent cClassDeclaration With: [
              s declareAndDefineMethodNamed: 'mapType' Type: 'virtual char*' Arguments: '' Body: 'return "', mapType, '";'.
              parent simpleTestingSlotsDo: [|:slot|
                s declareAndDefineMethodNamed: slot name Type: 'virtual bool' Arguments: '' Body: 'return ', slot contents reflectee printString, ';'.
              ].
              shouldHaveMyLayoutMethod ifTrue: [
                s declareMethodNamed: 'myLayout' Type: 'virtual ObjectLayout*' Arguments: ''.
              ].

              hasHandWrittenDeclarations ifTrue: [s includeFile: includeFileNameForHandWrittenDeclarations].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         gatherImplementationInformationFor: s = ( |
            | 
            shouldHaveMyLayoutMethod ifTrue: [
              s defineMethodNamed: 'myLayout' On: cClassName Type: 'ObjectLayout*' Arguments: '' Body: 'return new ', myLayout cClassName, '();'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         gatherMapTypeTestersFor: s = ( |
            | 
            s ifExpression: 'compareStrings(mapTypeSize, mapType, "', mapType, '")' Then: 'return new ', cClassName, '();'.
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         hasHandWrittenDeclarations = ( |
            | 
                (== klein maps map)
            || [(== klein maps slotsMap)
            || [ == klein maps methodMap]]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         includeFileNameForHandWrittenDeclarations = ( |
            | 
            cClassName, '_otherDecls.incl.h').
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldHaveMyLayoutMethod = ( |
            | 
            [myLayout]. "browsing"
            (parent asMirror includesKey: 'myLayout') && [myLayout != klein layouts abstract]).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldIncludeInSmallImage = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'map') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         simpleTestingSlotsDo: blk = ( |
            | 
            asMirror do: [|:s|
              s isMethod not
                && [(('is' isPrefixOf: s name) || ['are' isPrefixOf: s name])
                && [(selector copyStr: s name) isUnary]]
                 ifTrue: [blk value: s].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'nmethodMap') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldIncludeInSmallImage = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'profilerMap') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldIncludeInSmallImage = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: ((bootstrap stub -> 'globals' -> 'klein' -> 'maps') \/-> 'vframeMap') -> 'parent' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         shouldIncludeInSmallImage = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         objectSystemSpitterOuter = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein objectSystemSpitterOuter.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: C++ entities\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         class: classDeclaration With: blk = ( |
            | 
            ('class ', classDeclaration, ' {') printLine.
            '  public: ' printLine.
            blk value.
            '};' printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: C++ entities\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         declareAndDefineMethodNamed: methodName Type: returnType Arguments: argString Body: bodyString = ( |
            | 
            ('\t', returnType, ' ', methodName, '(', argString, ') {', bodyString, '}') printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: C++ entities\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         declareMethodNamed: methodName Type: returnType Arguments: argString = ( |
            | 
            ('\t', returnType, ' ', methodName, '(', argString, ');') printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: C++ entities\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         defineMethodNamed: methodName On: className Type: returnType Arguments: argString Body: bodyString = ( |
            | 
            (returnType, ' ', className, '::', methodName, '(', argString, ') {', bodyString, '}') printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: C++ entities\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         ifExpression: exprString Then: thenString = ( |
            | 
            ('if (', exprString, ') {', thenString, '}') printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: C++ entities\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         includeFile: fileName = ( |
            | 
            ('#include "', fileName, '"') printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: layouts\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         layoutsDo: blk = ( |
            | 
            layoutsStartingAt: klein layouts abstract Do: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: layouts\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         layoutsStartingAt: aLayout Do: blk = ( |
            | 
            blk value: aLayout.
            (browseWellKnown childrenOf: aLayout) do: [|:m|
              layoutsStartingAt: m reflectee Do: blk.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: maps\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         mapsDo: blk = ( |
            | 
            mapsStartingAt: klein maps map parent Do: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: maps\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         mapsStartingAt: mt Do: blk = ( |
             prototypes.
             traits.
            | 
            prototypes: list copyRemoveAll.
            traits:     list copyRemoveAll.
            (browseWellKnown childrenOf: mt) do: [|:m|
              (m creatorSlotHint name = 'parent' ifTrue: [traits] False: [prototypes]) add: m reflectee.
            ].
            prototypes do: [|:map| map shouldIncludeInSmallImage ifTrue: [blk value: map]].
            traits     do: [|:mapTraits| mapsStartingAt: mapTraits Do: blk].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'ModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         spitOutAutoGenerationWarning = ( |
            | 
            ('/* THIS FILE WAS AUTO-GENERATED BY ', asMirror name, ' */') printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         spitOutFileNamed: n WithContents: blk = ( |
            | 
            ('------ ', n, ' ------') printLine.
            '' printLine.
            spitOutAutoGenerationWarning.
            '' printLine.
            blk value: n.
            '' printLine.
            spitOutAutoGenerationWarning.
            '' printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         spitOutIncludeFilesForVM: aVM = ( |
            | 
            aVM setTheVMAndDo: [
              spitOutFileNamed: 'markFields.incl.h' WithContents: [
                klein layouts mark fieldsReverseDo: [|:f| f gatherIncludeFileInformationFor: self].
              ].
              spitOutFileNamed: 'slotTypeFields.incl.h' WithContents: [
                klein slotType fieldsReverseDo: [|:f| f gatherIncludeFileInformationFor: self].
              ].
              spitOutFileNamed: 'headerFields.incl.h' WithContents: [
                klein layouts memoryObject fieldsDo: [|:f| f gatherIncludeFileInformationFor: self].
                layoutsStartingAt: klein layouts memoryObject Do: [|:layout|
                  (layout asMirror includesKey: 'lastField') ifTrue: [
                    layout specialFieldsAfterHeaderDo: [|:f| f gatherIncludeFileInformationFor: self].
                  ].
                ].
              ].
              spitOutFileNamed: 'maps.incl.h' WithContents: [
                mapsDo: [|:map| map gatherHeaderInformationFor: self].
              ].
              spitOutFileNamed: 'maps.incl.impl.h' WithContents: [
                mapsDo: [|:map| map gatherImplementationInformationFor: self].
              ].
              spitOutFileNamed: 'mapTypeTesters.incl.impl.h' WithContents: [
                mapsDo: [|:map| map gatherMapTypeTestersFor: self].
              ].
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'objectSystemSpitterOuter' -> () From: ( | {
         'Category: C++ entities\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         variableType: t Name: n Value: v = ( |
            | 
            (t, ' ', n, ' = ', v, ';') printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'slotType' -> 'slotTypeField' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         gatherIncludeFileInformationFor: s = ( |
            | 
            resend.gatherIncludeFileInformationFor: s.
            valueSlotNamesDo: [|:n|
              s variableType: 'const FieldValue' Name: cName, '_', n Value: (n sendTo: self) printString.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         cName = ( |
            | 
            cNamePrefix, '_', name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         cNamePrefix = ( |
            | 
            asMirror creatorPath copyWithoutLast last).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'wordLayoutMixin' -> 'abstractField' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         gatherIncludeFileInformationFor: s = ( |
            | 
            s variableType: 'const BitCount' Name: cName, '_width' Value: width printString.
            s variableType: 'const BitCount' Name: cName, '_shift'
                                            Value: precedingField ifNil: [shift printString]
                                                               IfNotNil: [|:f| f cName, '_width + ',
                                                                               f cName, '_shift'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'wordLayoutMixin' -> 'abstractNumberField' -> () From: ( | {
         'Category: spitting out stuff\x7fModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         gatherIncludeFileInformationFor: s = ( |
            | 
            resend.gatherIncludeFileInformationFor: s.
            s variableType: 'const int' Name: cName, '_lowestEncodableNumber' Value: lowestEncodableNumber printString.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot'
        
         kleinSpitterOuter = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSpitterOuter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSpitterOuter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinSpitterOuter.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSpitterOuter' -> () From: ( | {
         'ModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSpitterOuter' -> () From: ( | {
         'ModuleInfo: Module: kleinSpitterOuter InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSpitterOuter' -> () From: ( | {
         'ModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSpitterOuter' -> () From: ( | {
         'ModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSpitterOuter' -> () From: ( | {
         'ModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.2 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinSpitterOuter' -> () From: ( | {
         'ModuleInfo: Module: kleinSpitterOuter InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinSpitterOuter postFileIn
