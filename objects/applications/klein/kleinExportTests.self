 '$Revision: 1.29 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> () From: ( | {
         'Category: building / exporting programs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         exportTests = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> () From: ( | {
         'Category: abstract exportTest state\x7fModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         debuggerOutliner.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> () From: ( | {
         'Category: abstract exportTest state\x7fModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         hostMorph.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> () From: ( | {
         'Category: abstract exportTest state\x7fModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (set copyRemoveAll)\x7fVisibility: private'
        
         morphs <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> () From: ( | {
         'Category: abstract exportTest state\x7fModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (list copyRemoveAll)\x7fVisibility: private'
        
         morphsInOrder <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> () From: ( | {
         'Category: abstract exportTest state\x7fModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (100@100)\x7fVisibility: private'
        
         oldProgramMorphPosition <- 100@100.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing framework\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         assert: blk = ( |
            | 
            blk value ifFalse: [error: 'failed'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: status\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         breakpointShouldBe: bp = ( |
            | 
            trapCommentShouldBe: '_Breakpoint: ', bp).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         buildAndTestProgram = ( |
            | 
            show: 'Testing unmodified program'  While: [
              | dbg |
              selectProgram.
              dbg: launchProgram.
              testProgram.
              deleteMorph: dbg.
              self
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         chooseProgramOrVM = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: controlling execution\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         continue = ( |
            | 
            debugger continue: testEvent.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: background menu\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         contributeToBackgroundMenu: m = ( |
            | 
            m addButton: ((
              ui2Button copy scriptBlock: [ klein exportTests abstract popUpSubmenu: event ])
                                   label: 'Klein export tests')
                ToGroup: 'applications'.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            ((resend.copy
              testEvent: process this birthEvent)
              phasesAndTimesStack: phasesAndTimesStack copyRemoveAll)
              morphsInOrder: morphsInOrder copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: accessing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         debugger = ( |
            | debuggerOutliner model).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: wrangling morphs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         deleteMorph: m = ( |
            | 
            morphsInOrder remove: m IfAbsent: [].
            m animatedDelete.
            world cleanUp: testEvent copy sourceHand: world hands first.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing framework\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         error: e = ( |
            | resend.error: 'test failed: ', e).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: expanding & menuing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         expandActivation: i = ( |
            | 
            ((debugger stack ifNil: [^ self]) items morphs asVector at: i) expand: testEvent.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: expanding & menuing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         expandStack = ( |
            | 
            debuggerOutliner update.
            debuggerOutliner expand: testEvent.
            ( debugger stack ifNil: [^ self]) expandAll: testEvent.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: expanding & menuing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         expandTop = ( |
            | expandStack.
            expandActivation: 0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: wrangling morphs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         firstMorphPosition = ( |
            | 
            spacing @ spacing).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: accessing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         foreignProcess = ( |
            | debugger myProcess).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: wrangling morphs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         getHostMorph = ( |
            | 
            hostMorph: vmKit foreignHostMorph copy.
            world morphs findFirst: [|:wm| wm morphTypeName = 'foreignHostMorph' ]
               IfPresent: [|:wm| hostMorph: wm]
                IfAbsent: [world addMorph: hostMorph].
            putInWorld: hostMorph.
            hostMorph startOnLocalHost.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: wrangling morphs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         getProgramMorph = ( |
             pm.
            | 
            pm: vmKit foreignProgramMorph copy.
            programMorph: putInWorld:
              hand world morphs
                 findFirst: [|:m|  (m morphTypeName = pm morphTypeName)
                               && [(m programName = programName)
                               && [ 'ready' isPrefixOf: m status ]]]
                 IfPresent: [|:m| oldProgramMorphPosition: m position.
                                  m ]
                 IfAbsent:  [     oldProgramMorphPosition: testEvent cursorPoint.
                                  pm ].
            programMorph).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing UI pieces\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         hand = ( |
            | testEvent sourceHand).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: status\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         isStoppedBy: sig = ( |
            | 
            'stopped by signal: ', sig  isPrefixOf: status).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         launchProgram = ( |
            | 
            show: 'launching' While: [
              debuggerOutliner:
                programMorph launchFromProxy: hostMorph newProxy
                                       Event: testEvent
                            DebugPerformance: false
                                      IfFail: [|:e| ^ error: e].
            ].
            hand drop: debuggerOutliner Event: testEvent. "drop debugger"
            putInWorld: debuggerOutliner).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: wrangling morphs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         mostRecentMorph = ( |
            | 
            mostRecentMorphIfNone: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: wrangling morphs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         mostRecentMorphIfNone: nb = ( |
            | 
            [
                morphsInOrder isEmpty ifTrue: [^ nb value].
                morphsInOrder last isInWorld
            ] whileFalse: [morphsInOrder removeLast].
            morphsInOrder last).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: wrangling morphs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         openNewWorld = ( |
            | 
            testEvent ifNil: [desktop open. world: desktop w. ^ self].
            world: desktop openNewWorldOnDisplay: hand world anyOpenWindowCanvas displayName 
                                          Bounds: worldBounds.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing framework\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         phaseAndTime = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> 'phaseAndTime' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstract parent phaseAndTime.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> 'phaseAndTime' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot'
        
         copyPhase: p Time: ms = ( |
            | (copy phase: p) time: ms).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> 'phaseAndTime' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> 'phaseAndTime' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot'
        
         phase <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> 'phaseAndTime' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot'
        
         printString = ( |
            | 
            phase , ': ', (time / 1000) printString, ' secs.').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> 'phaseAndTime' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot'
        
         time <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: background menu\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         popUpSubmenu: evt = ( |
             m.
             tests.
            | 
            tests: ((
              reflect: vmKit exportTests) 
              asList copyFilteredBy: [|:s| s visibility isPublic])
              asVector sort copyMappedBy: [|:s| s contents reflectee].

            m: ui2Menu copy.
            tests do: [|:t|
              m         addButtonTarget: t
                AsynchronousScriptBlock: [ target copy run]
                                  Label: t asMirror creatorSlotHint name
            ].
            m popUp: evt.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         programName = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: wrangling morphs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         putInWorld: aMorph = ( |
             activity.
             p.
            | 
            aMorph owner ifNotNil: aMorph delete.
            world addMorph: aMorph.
            world moveToFront: aMorph.
            p: topLeftForNewMorph: aMorph.
            activity: aMorph moveToPosition: p.
            morphsInOrder addLast: aMorph.
            [activity done] whileFalse: [times delay: 100].
            aMorph).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: wrangling morphs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         removeMorphs = ( |
            | 
            programMorph delete.
            hand world addMorph: programMorph.
            programMorph position: testEvent cursorPoint.
            hand world moveToFront: programMorph.
            world close.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing framework\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         reportSuccess = ( |
            | 
            phasesAndTimesStack do: [|:pats. :level|
              pats do: [|:pt| level do: ['    ' print]. pt printString printLine ].
            ].
            userQuery reportAndContinue: 'Success!'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            openNewWorld.
            show: 'Running ', printString  While: [

              show: 'Starting tests'  While: [
                getHostMorph.
                getProgramMorph.
              ].
              buildAndTestProgram.
              vmKit incrementalUpdater disableUpdatingOfLiveImagesDuring: [
                testDefines.
              ].
              reportSuccess.
            ].
            removeMorphs).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: controlling execution\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         runToStop = ( |
            | continue.
            waitTillNotRunning).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         selectProgram = ( |
            | 
            programMorph status = 'ready'  ifTrue: [^ self].

            show: 'Building ', programName
             While: [
               programMorph proxyForBuilding: hostMorph newProxy.
               chooseProgramOrVM.
               programMorph buildProgramForEvent: testEvent.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: expanding & menuing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         setSourceLevel: aBool = ( |
            | 
            debugger stack model setSourceLevel: aBool.
            expandTop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: status\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldBeStoppedBy: sig = ( |
            | 
            (isStoppedBy: sig)  ifTrue: [^ self].
            error: 'Should be stopped by ', sig, ' but status is ', status).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: testing framework\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         show: msg While: blk = ( |
             pats.
             r.
             t.
            | 
            phasesAndTimesSP: phasesAndTimesSP succ.
            t: [userQuery showEverybody: msg While: [r: blk value]] cpuTime.
            phasesAndTimesSP: phasesAndTimesSP pred.
            pats: phasesAndTimesStack
                         at: phasesAndTimesSP  
                   IfAbsent: [|s| s: list copyRemoveAll. phasesAndTimesStack addLast: s. s].
            pats addLast: phaseAndTime copyPhase: msg Time: t.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: wrangling morphs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         showMirror: m = ( |
            | 
            putInWorld: selfObjectModel outlinerFor: m InWorld: world).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: wrangling morphs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         spacing = 10.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: status\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         status = ( |
            | 
            foreignProcess status).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: status\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         statusShouldBe: s = ( |
             xxxxxx <- 0.
            | 
            [
              |ss|
              ss: status.
              ss = s ifTrue: [^ self].
              ss = 'process sema busy'  ifFalse: [
                error: 'status is ', ss, ' but should be ', s
              ].
              times delay: 200.
              xxxxxx: xxxxxx succ.
            ] loop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: controlling execution\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         step = ( |
            | 
            debugger stepTop: testEvent.
            waitTillNotRunning.
            shouldBeStoppedBy: 'SIGTRAP'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: incremental update testing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testDefines = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testProgram = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: wrangling morphs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         topLeftForNewMorph: aMorph = ( |
            | 
            (mostRecentMorphIfNone: [^ firstMorphPosition])
               baseBounds bottomLeft + (0 @ spacing)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: status\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         trapCommentShouldBe: bp = ( |
            | 
            statusShouldBe: 'stopped by signal: SIGTRAP: trace trap // ', bp).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            klein).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: debugger manipulation\x7fCategory: status\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         waitTillNotRunning = ( |
            | 
            [ |:exit|
              case if: [status = 'running'  ] Then: [times delay: 100]
                   If: [isStoppedBy: 'SIGIO'] Then: [continue]
                   Else: exit
            ] loopExit.
            expandTop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> () From: ( | {
         'Category: wrangling morphs\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         worldBounds = ((20)@(20)) # ((800)@(600)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> () From: ( | {
         'Category: abstract exportTest state\x7fModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (0)\x7fVisibility: private'
        
         phasesAndTimesSP <- 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> () From: ( | {
         'Category: abstract exportTest state\x7fModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (sequence copyRemoveAll)\x7fVisibility: private'
        
         phasesAndTimesStack <- sequence copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> () From: ( | {
         'Category: abstract exportTest state\x7fModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         programMorph.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> () From: ( | {
         'Category: abstract exportTest state\x7fModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         testEvent.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> () From: ( | {
         'Category: abstract exportTest state\x7fModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         world.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractVM = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM.

CopyDowns:
globals klein exportTests abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         chooseProgramOrVM = ( |
            | 
            programMorph chooseVM: vmName.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: incremental update testing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         deleteDebuggerAndReexport = ( |
            | 
            debuggerOutliner ifNotNil: [deleteMorph: debuggerOutliner].
            launchProgram).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: reflecting\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         foreignKleinMirrorFor: selfMir = ( |
             im.
             mirrorProto.
             oid.
             selfObjCopiedForLaunch.
            | 
            im: launchedVMImage.
            selfObjCopiedForLaunch: 
              case if:   [selfMir = vm universe asMirror]
                   Then: [im myVM universe]
                   If:   [selfMir = vm asMirror]
                   Then: [im myVM]
                   If:   [selfMir = vm parent asMirror]
                   Then: [im myVM parent]
                   Else: [selfMir reflectee].
            oid: im oidForOriginalObject: selfObjCopiedForLaunch.
            mirrorProto: (reflect: selfObjCopiedForLaunch) mirrorPrototypeFromNamespace: im myVM mirrorPrototypes.
            mirrorProto copyForVM: im myVM
                              OID: oid
                           IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: incremental update testing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         incrementalUpdateTesters = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters abstract.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         exportTester.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> () From: ( | {
         'Category: mirrors\x7fModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         exportedMirrorToMutate.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> () From: ( | {
         'Category: mirrors\x7fModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         foreignMirrorToMutate.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> () From: ( | {
         'Category: mirrors\x7fModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         mirrorToMutate.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (set copyRemoveAll)'
        
         morphs <- set copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters abstract parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         cleanupMorphs = ( |
            | 
            morphs do: [|:m|
              m isInWorld ifTrue: [exportTester deleteMorph: m]].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         copyFor: et = ( |
            | 
            (copy exportTester: et)
             morphs: morphs copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         deleteDebuggerAndReexportIfNecessary = ( |
            | 
            exportTester deleteDebuggerAndReexport).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         doMirrors: blk = ( |
            | 
            blk value: mirrorToMutate.
            blk value: exportedMirrorToMutate.
            blk value: foreignMirrorToMutate.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotChangedIn: m = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsChanged = ( |
            | 
            doMirrors: [|:m|
              showMirror: m.
              ensureSlotChangedIn: m.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsClean = ( |
            | 
            doMirrors: [|:m|
              ensureSlotIsCleanIn: m.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsCleanAfterMutating = ( |
            | 
            ensureSlotIsClean).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsCleanBeforeMutating = ( |
            | 
            ensureSlotIsClean).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsCleanIn: m = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         getKleinMirrors = ( |
            | 
            exportedMirrorToMutate: exportTester kleinMirrorFor: mirrorToMutate.
            showMirror: exportedMirrorToMutate.
            deleteDebuggerAndReexportIfNecessary.
            foreignMirrorToMutate: exportTester foreignKleinMirrorFor: mirrorToMutate.
            showMirror: foreignMirrorToMutate.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         getSelfMirror = ( |
            | 
            mirrorToMutate: reflect: selfObjectToMutate.
            pickSlotName.
            showMirror: mirrorToMutate.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         mutateSlot = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         pickSlotName = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         restoreSlot = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         run = ( |
            | 
            show: wholeTestMessage
            While: [
              getSelfMirror.
              getKleinMirrors.
              ensureSlotIsCleanBeforeMutating.
              mutateSlot.
              getKleinMirrors.
              ensureSlotIsChanged.
              restoreSlot.
              getKleinMirrors.
              ensureSlotIsCleanAfterMutating.
              cleanupMorphs.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         selfObjectToMutate = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot'
        
         show: msg While: blk = ( |
            | exportTester show: msg While: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot'
        
         showMirror: m = ( |
             outliner.
            | 
            outliner: exportTester showMirror: m.
            morphs add: outliner.
            outliner showSlot: (m at: slotName IfAbsent: [^ self]) Event: exportTester testEvent.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         updateDebugger = ( |
            | 
            exportTester expandStack.
            exportTester debuggerOutliner update.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         wholeTestMessage = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (\'\')\x7fVisibility: private'
        
         slotName <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractMethodMutation = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstractVM parent incrementalUpdateTesters abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters abstractMethodMutation.

CopyDowns:
globals klein exportTests abstractVM parent incrementalUpdateTesters abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters abstractMethodMutation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotChangedIn: m = ( |
            | 
            exportTester assert: [hasMethodBeenMutatedIn: m].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsCleanIn: m = ( |
            | 
            exportTester assert: [ (hasMethodBeenMutatedIn: m) not ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         hasMethodBeenMutatedIn: m = ( |
            | 
            (m at: slotName) contents source includesSubstring: myBreakpoint).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         inMethodReplace: old With: new = ( |
             ka.
             nslot.
             nsrc.
            | 
            ka: (mirrorToMutate at: slotName) longerKeyWithAssigner.
            nsrc: ka, '(', (methodSource replace: old With: new), ')'.
            nslot: nsrc asSlotIfFail: raiseError.
            nslot first annotation: (mirrorToMutate at: slotName) annotation.
            mutateToHaveSlots: nslot.
            waitUntilDefineHasPropagated.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         methodPrefix = ( |
            | '_Breakpoint: \'', myBreakpoint, '\'.\n').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         methodSource = ( |
            | (mirrorToMutate at: slotName) contents source).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         modifiedMethodSource: ms = ( |
            | 
            methodPrefix, ms).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         mutateSlot = ( |
             ms.
            | 
            ms: methodSource.
            show: 'Changing method'
             While: [inMethodReplace: ms With:  modifiedMethodSource: ms].
            ms = methodSource ifTrue: [error: 'no mutation'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         mutateToHaveSlots: newSlots = ( |
            | 
            mirrorToMutate addSlots: newSlots.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         restoreSlot = ( |
            | 
            show: 'Restoring method'
             While: [inMethodReplace: methodPrefix With: '']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         waitUntilDefineHasPropagated = ( |
            | 
            show: 'waiting for method mutation to propagate'
             While: [exportTester vmImage waitUntilDefineHasPropagated].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         fastSlotAddition = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstractVM parent incrementalUpdateTesters abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters fastSlotAddition.

CopyDowns:
globals klein exportTests abstractVM parent incrementalUpdateTesters abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters fastSlotAddition parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         addSlot: name With: contents To: mir = ( |
            | 
            mir at: name PutContents: contents.
            show: 'waiting for addition of slot: ', name, ' to propagate'
              While: [exportTester vmImage waitUntilDefineHasPropagated].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotChangedIn: m = ( |
            | 
            exportTester assert: [m includesKey: slotName].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsCleanIn: m = ( |
            | 
            exportTester assert: [(m includesKey: slotName) not].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         mutateSlot = ( |
            | 
            addSlot: slotName With: 'addedSlot' asMirror To: mirrorToMutate.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         namePrefix = 'fastDefineTestSlot'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         pickSlotName = ( |
            | 
            slotName: pickSlotNameStartingWith: namePrefix In: mirrorToMutate).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         pickSlotNameStartingWith: prefix In: mir = ( |
            | 
            1 to: 999 Do: [|:i. n|
              n: prefix, '_', i printString.
              mir at: n IfAbsent: [^ n].
            ].
            error: 'huh?').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         removeSlot: name From: mir = ( |
            | 
            mir removeSlot: name IfFail: [].
            show: 'waiting for removal of slot: ', name, ' to propagate'
              While: [exportTester vmImage waitUntilDefineHasPropagated].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         restoreSlot = ( |
            | 
            removeSlot: slotName From: mirrorToMutate.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         selfObjectToMutate = ( |
            | 
            exportTester vm universe).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         wholeTestMessage = 'Testing fast slot addition'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         liveUpdateParentMethodMutation = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstractVM parent incrementalUpdateTesters abstractMethodMutation copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters liveUpdateParentMethodMutation.

CopyDowns:
globals klein exportTests abstractVM parent incrementalUpdateTesters abstractMethodMutation. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters liveUpdateParentMethodMutation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         deleteDebuggerAndReexportIfNecessary = ( |
            | 
            "For live update tests, don't need to re-export."
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsChanged = ( |
            | 
            updateDebugger.
            resend.ensureSlotIsChanged).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsClean = ( |
            | 
            updateDebugger.
            resend.ensureSlotIsClean).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         myBreakpoint = 'liveUpdateMutatedParentMethod'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         pickSlotName = ( |
            | 
            slotName: 'methodForLiveMutation').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         selfObjectToMutate = ( |
            | 
            exportTester vm parent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         waitUntilDefineHasPropagated = ( |
            | 
            resend.waitUntilDefineHasPropagated.

            show: 'waiting for method mutation to propagate in launched image'
             While: [exportTester launchedVMImage waitUntilDefineHasPropagated].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         wholeTestMessage = 'Testing live update parent method mutation'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         fixAndContinueParentMethodMutation = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fixAndContinueParentMethodMutation' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstractVM parent incrementalUpdateTesters liveUpdateParentMethodMutation copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fixAndContinueParentMethodMutation' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters fixAndContinueParentMethodMutation.

CopyDowns:
globals klein exportTests abstractVM parent incrementalUpdateTesters liveUpdateParentMethodMutation. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fixAndContinueParentMethodMutation' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fixAndContinueParentMethodMutation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters fixAndContinueParentMethodMutation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fixAndContinueParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsChanged = ( |
            | 
            resend.ensureSlotIsChanged.
            exportTester trapCommentShouldBe: 'preempted'.
            exportTester runToStop.
            exportTester breakpointShouldBe: myBreakpoint.
            exportTester runToStop.
            exportTester breakpointShouldBe: 'done'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fixAndContinueParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsCleanAfterMutating = ( |
            | 
            resend.ensureSlotIsCleanAfterMutating.
            exportTester trapCommentShouldBe: 'preempted'.
            exportTester runToStop.
            exportTester breakpointShouldBe: 'done'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fixAndContinueParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         mutateToHaveSlots: newSlots = ( |
             actModel.
             newSlot.
            | 
            newSlot: newSlots first.
            exportTester expandStack.
            actModel: exportTester debuggerOutliner model stack model activationMorphs last model.
            actModel changeMyMethodTo: newSlot Event: exportTester testEvent IfFail: raiseError.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fixAndContinueParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         myBreakpoint = 'fixAndContinueMutatedParentMethod'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fixAndContinueParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'liveUpdateParentMethodMutation' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fixAndContinueParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         pickSlotName = ( |
            | 
            slotName: 'start').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fixAndContinueParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         selfObjectToMutate = ( |
            | 
            exportTester vm parent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fixAndContinueParentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         wholeTestMessage = 'Testing fix-and-continue parent method mutation'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         receiverMethodMutation = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'receiverMethodMutation' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstractVM parent incrementalUpdateTesters abstractMethodMutation copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'receiverMethodMutation' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters receiverMethodMutation.

CopyDowns:
globals klein exportTests abstractVM parent incrementalUpdateTesters abstractMethodMutation. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'receiverMethodMutation' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'receiverMethodMutation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters receiverMethodMutation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'receiverMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsChanged = ( |
            | 
            resend.ensureSlotIsChanged.
            exportTester runToStop.
            exportTester breakpointShouldBe: myBreakpoint.
            exportTester testProgram.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'receiverMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsClean = ( |
            | 
            resend.ensureSlotIsClean.
            exportTester testProgram.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'receiverMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         myBreakpoint = 'mutatedReceiverMethod'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'receiverMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'abstractMethodMutation' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'receiverMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         pickSlotName = ( |
            | slotName: 'b').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'receiverMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         selfObjectToMutate = ( |
            | 
            exportTester vm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'receiverMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         wholeTestMessage = 'Testing receiver method mutation'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         literalMapsDuringMethodMutation = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'literalMapsDuringMethodMutation' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstractVM parent incrementalUpdateTesters receiverMethodMutation copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'literalMapsDuringMethodMutation' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters literalMapsDuringMethodMutation.

CopyDowns:
globals klein exportTests abstractVM parent incrementalUpdateTesters receiverMethodMutation. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'literalMapsDuringMethodMutation' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'literalMapsDuringMethodMutation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters literalMapsDuringMethodMutation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'literalMapsDuringMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         methodSuffix = ( |
            | 
            '.
            _Breakpoint: \'', myBreakpoint, '\'.
            self').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'literalMapsDuringMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         modifiedMethodSource: ms = ( |
            | 
            ms, methodSuffix).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'literalMapsDuringMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         myBreakpoint = 'literalMaps'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'literalMapsDuringMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'receiverMethodMutation' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'literalMapsDuringMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         pickSlotName = ( |
            | 
            slotName: 'testMapsOfObjectLiterals').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'literalMapsDuringMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         restoreSlot = ( |
            | 
            show: 'Restoring method'
             While: [inMethodReplace: methodSuffix With: '']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'literalMapsDuringMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         selfObjectToMutate = ( |
            | 
            exportTester vm tests maps parent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'literalMapsDuringMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         wholeTestMessage = 'Testing compilation of literal maps'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         parentMethodMutation = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'parentMethodMutation' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstractVM parent incrementalUpdateTesters receiverMethodMutation copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'parentMethodMutation' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters parentMethodMutation.

CopyDowns:
globals klein exportTests abstractVM parent incrementalUpdateTesters receiverMethodMutation. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'parentMethodMutation' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'parentMethodMutation' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters parentMethodMutation parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'parentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         myBreakpoint = 'mutatedParentMethod'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'parentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'receiverMethodMutation' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'parentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         pickSlotName = ( |
            | 
            slotName: 'start').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'parentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         selfObjectToMutate = ( |
            | 
            exportTester vm parent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'parentMethodMutation' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         wholeTestMessage = 'Testing parent method mutation'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         slowSlotAddition = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'slowSlotAddition' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstractVM parent incrementalUpdateTesters fastSlotAddition copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'slowSlotAddition' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters slowSlotAddition.

CopyDowns:
globals klein exportTests abstractVM parent incrementalUpdateTesters fastSlotAddition. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'slowSlotAddition' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'slowSlotAddition' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests abstractVM parent incrementalUpdateTesters slowSlotAddition parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'slowSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         assignmentSlotName = ( |
            | 
            slotName, ':').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'slowSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotChangedIn: m = ( |
            | 
            resend.ensureSlotChangedIn: m.
            exportTester assert: [m includesKey: assignmentSlotName].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'slowSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         ensureSlotIsCleanIn: m = ( |
            | 
            resend.ensureSlotIsCleanIn: m.
            exportTester assert: [(m includesKey: assignmentSlotName) not].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'slowSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         mutateSlot = ( |
            | 
            resend.mutateSlot.
            addSlot: assignmentSlotName With: mirrors assignment To: mirrorToMutate).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'slowSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         namePrefix = 'slowDefineTestSlot'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'slowSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'fastSlotAddition' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> 'incrementalUpdateTesters' -> 'slowSlotAddition' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         wholeTestMessage = 'Testing slow slot addition'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: reflecting\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         kleinMirrorFor: selfMir = ( |
             oid.
            | 
            oid: vmImage oidForOriginalObject: selfMir reflectee.

            "caching mirror wont work locally"
            [todo cleanup justCurious]. "Why do we not use a caching mirror? The above comment says
                                         caching mirrors won't work locally, but this one is isn't
                                         gonna be used locally. -- Adam, 4/06"
            (selfMir mirrorPrototypeFromNamespace: klein mirrors)
                             copyForVM: vm
                                   OID: oid
                                IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         launchedVMImage = ( |
            | 
            programMorph copiedImages last).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         programName = ( |
            | vmName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: incremental update testing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testDefines = ( |
            | 
            show: 'Testing slot addition'  While: [
              testFastDefine.
              testSlowDefine
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: incremental update testing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testFastDefine = ( |
            | 
            (incrementalUpdateTesters fastSlotAddition copyFor: self) run.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: incremental update testing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testSlowDefine = ( |
            | 
            (incrementalUpdateTesters slowSlotAddition copyFor: self) run.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         vm = ( |
            | vmImage myVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         vmImage = ( |
            | programMorph vmImage).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         vmName = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         asmProgram = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'asmProgram' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstract copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'asmProgram' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests asmProgram.

CopyDowns:
globals klein exportTests abstract. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'asmProgram' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'asmProgram' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests asmProgram parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'asmProgram' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         chooseProgramOrVM = ( |
            | 
            programMorph chooseProgram: programName.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'asmProgram' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstract' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'asmProgram' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         programName = 'conditionCodeTest'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'asmProgram' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testProgram = ( |
            | 
            setSourceLevel: false.
            shouldBeStoppedBy: 'SIGSTOP'.
            step. 
            step.
            runToStop.
            shouldBeStoppedBy: 'SIGTRAP'.
            runToStop.
            statusShouldBe: 'exited'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         midiVM = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstractVM copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests midiVM.

CopyDowns:
globals klein exportTests abstractVM. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests midiVM parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fCategory: tests\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         breakfastActivationSource = '
              apples.
              breakfast.
              cheese.
            '.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fCategory: tests\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         checkArgValues = ( |
            | 
            show: 'Checking argument values'
             While: [
                breakpointShouldBe: 'use source level'.
                "check arg values"
                (foreignProcess stack at: 1) asVector do: [|:s. :i. | 
                  assert: [i succ = s contents reflectee].
                ].
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fCategory: tests\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         checkExit = ( |
            | 
            show: 'Checking graceful exit'
             While: [
                breakpointShouldBe: 'Exiting start method'.
                runToStop.

                statusShouldBe: 'exited'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fCategory: tests\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         checkLexicalParent = ( |
            | 
            show: 'Checking lexical parent'
             While: [
                |lp. z|
                breakpointShouldBe: 'Make sure z shows as 3.'.
                lp: foreignProcess stack first lexicalParent.
                z: (lp at: 'z') contents reflectee.
                assert: [ z = 3 ].
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fCategory: tests\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         checkLocalMutation = ( |
            | 
            show: 'Checking local mutation'
             While: [
                |m1. d. m2|
                breakpointShouldBe: 'get a mirror on self'.
                m1: foreignProcess stack first receiver.
                d: (m1 at: 'd') contents reflectee.
                runToStop.

                breakpointShouldBe: 'did d change?'.
                m2: foreignProcess stack first receiver.
                assert: [d != (m2 at: 'd') contents reflectee].
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fCategory: tests\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         checkSourcePosition = ( |
            | 
            show: 'Checking source position'
             While: [
                |a|
                breakpointShouldBe: 'Look at value:, make sure breakfast is highlighted.'.
                a: foreignProcess stack at: 1.
                assert: [ a isReflecteeBlockMethodActivation ].
                assert: [ a source = breakfastActivationSource ].
                assert: [ a position = 4 ]. " I think that position reflects the bytecode
                                              offset, and it looks like the position should be 4. -- Ausch"
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fCategory: tests\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         runAutoTests = ( |
            | 
            show: 'Running noninteractive tests'
             While: [runToStop]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> 'parent' -> () From: ( | {
         'Category: incremental update testing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testDefines = ( |
            | 
            (incrementalUpdateTesters literalMapsDuringMethodMutation copyFor: self) run.
            resend.testDefines.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testProgram = ( |
            | 
            show: 'Running ', vmName
             While: [
               runAutoTests.
               checkLocalMutation.   runToStop.
               checkArgValues.       runToStop.
               checkSourcePosition.  runToStop.
               checkLexicalParent.   runToStop.
               checkExit
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'midiVM' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         vmName = 'midiVM'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         miniVM = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVM' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstractVM copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVM' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests miniVM.

CopyDowns:
globals klein exportTests abstractVM. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVM' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVM' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests miniVM parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVM' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: incremental update testing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testDefines = ( |
            | 
            (incrementalUpdateTesters            parentMethodMutation copyFor: self) run.
            (incrementalUpdateTesters          receiverMethodMutation copyFor: self) run.
            resend.testDefines.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testProgram = ( |
            | 
            show: 'Running ', vmName
             While: [
                runToStop.
                breakpointShouldBe: 'done'.
                runToStop.
                statusShouldBe: 'exited'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVM' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         vmName = 'miniVM'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         miniVMLiveUpdate = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMLiveUpdate' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstractVM copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMLiveUpdate' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests miniVMLiveUpdate.

CopyDowns:
globals klein exportTests abstractVM. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMLiveUpdate' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMLiveUpdate' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests miniVMLiveUpdate parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMLiveUpdate' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMLiveUpdate' -> 'parent' -> () From: ( | {
         'Category: incremental update testing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testDefines = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMLiveUpdate' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testProgram = ( |
            | 
            show: 'Running live update test'
             While: [
              setSourceLevel: true.
              runToStop.
              breakpointShouldBe: 'done'.
              (incrementalUpdateTesters     liveUpdateParentMethodMutation copyFor: self) run.
              (incrementalUpdateTesters fixAndContinueParentMethodMutation copyFor: self) run.
              breakpointShouldBe: 'done'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMLiveUpdate' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         vmName = 'miniVM'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         miniVMSingleStepping = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMSingleStepping' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals klein exportTests abstractVM copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMSingleStepping' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests miniVMSingleStepping.

CopyDowns:
globals klein exportTests abstractVM. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMSingleStepping' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMSingleStepping' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein exportTests miniVMSingleStepping parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMSingleStepping' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'abstractVM' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMSingleStepping' -> 'parent' -> () From: ( | {
         'Category: incremental update testing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testDefines = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMSingleStepping' -> 'parent' -> () From: ( | {
         'Category: program manipulation\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         testProgram = ( |
            | 
            show: 'Running ', vmName
             While: [
              setSourceLevel: true.
              step.
              trapCommentShouldBe: 'preempted'.
              runToStop.
              breakpointShouldBe: 'done'.
              runToStop.
              statusShouldBe: 'exited'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'exportTests' -> 'miniVMSingleStepping' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         vmName = 'miniVM'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot'
        
         kleinExportTests = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExportTests' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExportTests' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinExportTests.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExportTests' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExportTests' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExportTests' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExportTests' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
            resend.postFileIn.
            worldMorph addBackgroundMenuContributor: klein exportTests abstract.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExportTests' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 1.29 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinExportTests' -> () From: ( | {
         'ModuleInfo: Module: kleinExportTests InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinExportTests postFileIn
