 '$Revision: 30.10 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: morphs\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         foreignHostMorph = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals abstractSimpleApplicationMorph copyRemoveAllMorphs ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignHostMorph.

CopyDowns:
globals abstractSimpleApplicationMorph. copyRemoveAllMorphs 
SlotsToOmit: parent prototype.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> () From: ( | {
         'Category: debuggingHost Morph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (labelMorph copy)\x7fVisibility: private'
        
         applicationLabel <- labelMorph copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> () From: ( | {
         'Category: debuggingHost Morph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (\'\')\x7fVisibility: private'
        
         architecture <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> () From: ( | {
         'Category: debuggingHost Morph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (smallEditorMorph copy)\x7fVisibility: private'
        
         hostNameEditor <- smallEditorMorph copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> () From: ( | {
         'Category: debuggingHost Morph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         isPerformanceBeingDebugged <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( |
             {} = 'Comment: I am the traits object for a graphical
widget that represents a machine that the user
can run and debug programs on.\x7fModuleInfo: Creator: globals kleinAndYoda foreignHostMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: style\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         acceptButtonStyle = ( |
            | generalModel acceptButtonStyle).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         addButtonsToMenu: menu = ( |
            | 
            addDuplicateButtonTo: menu.
            menu      addButtonTarget: self
              AsynchronousScriptBlock: [target startMacServerOn: 'localhost']
                                Label: 'Start Mac server on localhost'.
            isUp ifTrue: [
              menu addButtonTarget: self
                            ScriptBlock: [target getProgram: event]
                                  Label: 'Get program'.
            ].

            isPerformanceBeingDebugged ifTrue: [
              menu addButtonTarget: self
                       ScriptBlock: [target isPerformanceBeingDebugged: false]
                             Label: 'Disable logging'.
            ] False: [
              menu addButtonTarget: self
                       ScriptBlock: [target isPerformanceBeingDebugged: true]
                             Label: 'Enable logging'.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: dropping programs on me & downloading\x7fComment: This message is sent when morph m is dropped onto the receiver,
which chose to accept m by returning true to wantsMorph:Event:.
By default, this method simply adds the given morph.\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         addDroppingMorph: m Event: evt = ( |
            | 
            receiveDroppedProgram: m Event: evt.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         application = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: background menu\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         backgroundMenuButtonLabel = ( |
            | 
            application capitalize, ' host').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: style\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         cancelButtonStyle = ( |
            | generalModel cancelButtonStyle).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         defaultButtonHolder <- bootstrap stub -> 'globals' -> 'nil' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: style\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         defaultColor = paint copyRed: 0.938416 Green: 1.0  Blue: 0.581623.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         getProgram: evt = ( |
             menu.
             pm.
            | 
            pm: vmKit foreignProgramMorph copyForArchitecture: architecture.
            launchOrBuildForeignProgramMorph: pm Event: evt.
            menu: world morphs first.
            pm position: baseBounds bottomLeft + (0@4).
            world addMorph: pm.
            world moveToFront: pm.
            world moveToFront: menu.
            pm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: status\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         getStatus = ( |
            | 
            [alex]. 'status not updating'.
            newProxy
              pingIfSucceed: [|:proxy|   architecture: proxy architecture.
                                         'up on ', (architecture first isVowel ifTrue: 'an' False: 'a') , ' ', architecture ]
                     IfFail: [|:e    |   'down: ', e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: host name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         hostName = ( |
            | 
            (hostNameAndPortString asTokensSeparatedByCharactersIn: portSeparator) first).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: host name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         hostName: hn = ( |
            | 
            (hostNameEditor string: hn) updateLabels.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: host name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         hostNameAccepter = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> 'hostNameAccepter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignHostMorph parent hostNameAccepter.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: host name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         hostNameAndPortString = ( |
            | 
            hostNameEditor ifNil: [^''].
            hostNameEditor contentsString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: host name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         hostNameCanceler = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> 'hostNameCanceler' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignHostMorph parent hostNameCanceler.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: host name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         hostNameStyle = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: host name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         ifPortIsSpecifiedThen: portBlk Else: noneBlk = ( |
            | 
            portBlk value:
              (hostNameAndPortString asTokensSeparatedByCharactersIn: portSeparator)
              at: 1  IfAbsent: [^ noneBlk value]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         initialize = ( |
            | 
            resend.initialize.
            initializeHostNameEditor.
            initializeApplicationLabel.
            initializeStatus.
            hostName: hostName.
            colorAll: defaultColor.
            stepProcess: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot'
        
         initializeApplicationLabel = ( |
            | 
            applicationLabel: applicationLabel copy.
            applicationLabel fontSpec:  fontSpec copy style: 'bold'.
            applicationLabel color: fontColor.
            applicationLabel label: application.
            addMorphLast: applicationLabel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: host name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeHostNameEditor = ( |
            | 
            hostNameEditor: 
              hostNameEditor 
                copyString: 'localhost'
                Target: self
                Accept: hostNameAccepter
                Cancel: hostNameCanceler
                 Style: hostNameStyle.
            addMorphLast: hostNameEditor).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: status\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeStatus = ( |
            | 
            statusLabel: statusLabel copy.
            statusLabel fontSpec:  fontSpec.
            statusLabel color: fontColor.
            statusLabel label: 'status: unknown'.
            addMorphLast: statusLabel.
            isInWorld ifTrue: [startGettingStepped].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: status\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         isUp = ( |
            | newProxy pingIfSucceed: true IfFail: false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: status\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         justDroppedInto: m Event: evt = ( |
            | 
            resend.justDroppedInto: m Event: evt.
            isInWorld ifTrue: [ startGettingStepped ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: dropping programs on me & downloading\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         launchOrBuildForeignProgramMorph: aForeignProgramMorph Event: evt = ( |
            | 
            aForeignProgramMorph launchOrBuildFromProxy: newProxy copyAndInitiate
                                                  Event: evt
                                       DebugPerformance: isPerformanceBeingDebugged
                                                 IfFail: [|:e| ^ reportError: e Event: evt].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: basics\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         morphTypeName = 'foreignHostMorph'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: using ptracer debugging proxy client\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         newProxy = ( |
             dp.
             r.
            | 
            dp: kleinAndYoda debuggingProxyClients noncaching macOSX copyForApplication: application.
            r: ifPortIsSpecifiedThen: [|:p| dp copyForHost: hostName Port: p]
                                Else: [     dp copyForHost: hostName].
            r: r interposeCache.
            isPerformanceBeingDebugged 
             ifFalse: [r]
                True: [processLoggingSender copyForTarget: r]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'abstractSimpleApplicationMorph' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: host name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         portSeparator = ':'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: dropping programs on me & downloading\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         receiveDroppedProgram: aForeignProgramMorph Event: evt = ( |
             req.
             w.
            | 
            w: evt sourceHand world.
            w addMorph: aForeignProgramMorph.
            w moveToFront: aForeignProgramMorph.

            [launchOrBuildForeignProgramMorph: nil Event: nil]. "browsing"
            (message copy receiver: self 
                          Selector: 'launchOrBuildForeignProgramMorph:Event:'
                              With: aForeignProgramMorph
                              With: evt)
              forkForBirthEvent: evt.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: stepping\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         setStatus: s = ( |
            | 
            s = statusLabel label ifFalse: [
              safelyDo: [
                statusLabel label: s.

              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: stepping\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         startGettingStepped = ( |
            | 
            getSteppedEveryMSecs: 2000).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         startMacServerOn: hostName = ( |
             proxy.
            | 
            proxy: newProxy.
            proxy
              startServerOn: hostName
                       Port: ifPortIsSpecifiedThen: [|:p| p]
                                              Else: [proxy defaultPort]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         startOnLocalHost = ( |
            | 
            ('up' isPrefixOf: getStatus) ifTrue: [^ self].
            startMacServerOn: 'localhost'.
            ['up' isPrefixOf: getStatus] whileFalse: [times delay: 100].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            resend.statePrintString, ' on ', hostName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: stepping\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         step = ( |
            | 
            hostNameEditor editMode ifTrue: [^ setStatus: ''].
                stepProcess isNotNil
            && [stepProcess isAlive]
             ifTrue: [^ self "wait for the step process"].
            stepProcess: 
              (message copy receiver: self Selector: 'updateStatus') fork.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: status\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         updateStatus = ( |
            | 
            isPerformanceBeingDebugged ifFalse: [|s|
              s: getStatus.
              setStatus: s.
            ] True: [
              setStatus: 'not updating status'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> 'parent' -> () From: ( | {
         'Category: dropping programs on me & downloading\x7fComment: Drop a program on me to send it and run it.\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         wantsMorph: m Event: evt = ( |
            | 
                (resend.wantsMorph: m Event: evt)
            || [ m doesDebuggingHostMorphWantMe ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> () From: ( | {
         'Category: filing out\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            kleinAndYoda foreignHostMorph).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> () From: ( | {
         'Category: debuggingHost Morph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (labelMorph copy)\x7fVisibility: private'
        
         statusLabel <- labelMorph copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignHostMorph' -> () From: ( | {
         'Category: debuggingHost Morph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         stepProcess.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: user interface\x7fCategory: morphs\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         foreignProgramMorph = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             bootstrap remove: 'prototype' From:
             globals abstractSimpleApplicationMorph copyRemoveAllMorphs ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignProgramMorph.

CopyDowns:
globals abstractSimpleApplicationMorph. copyRemoveAllMorphs 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> () From: ( | {
         'Category: foreignProgramMorph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         copiedImages.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( |
             {} = 'Comment: I am the traits object for the UI widget that
represents a an assembler program that can be
downloaded and debugged. -- dmu 1/02\x7fModuleInfo: Creator: globals kleinAndYoda foreignProgramMorph parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fCategory: choosing a program\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         addAssemblerProgramsToMenu: menu = ( |
            | 
            assemblerProgramSlots do: [|:s|
              menu addButtonTarget: self
                    AsynchronousScriptBlock: [target chooseProgram: button label.
                                              target buildProgramForEvent: event]
                                      Label: s name
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         addBuildingButtonsToMenu: menu = ( |
            | 
            case
             if: [platformName = noPlatform]  Then: [addPlatformButtonsTo: menu]
             If: [isReadyToLaunch not      ]  Then: [ addProgramButtonsTo: menu]
                                              Else: [  addRebuildButtonTo: menu].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         addButtonsToMenu: menu = ( |
            | 
            addRebuildButtonTo: menu.
            vmImage ifNotNil: [
              menu addDivider.
              addStatisticsButtonsTo: menu.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fCategory: choosing a platform\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         addPlatformButtonsTo: menu = ( |
            | 
            platformNames do: [|:platformName|
              menu addButtonTarget: self
                       ScriptBlock: [target choosePlatform: buttonArgs first]
                        ButtonArgs: (vector copyAddFirst: platformName)
                             Label: 'Choose a ', platformName, ' program'.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         addProgramButtonsTo: menu = ( |
            | 
            addVMsToMenu: menu.
            menu addDivider.
            addAssemblerProgramsToMenu: menu.
            programButtonHolder: menu initializeDefaultButtonHolder: programButtonHolder.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         addRebuildButtonTo: menu = ( |
            | 
            menu         addButtonTarget: self
                 AsynchronousScriptBlock: [target buildProgramForEvent: event]
                                   Label: programBuilder rebuildActionName.

            menu addDivider.

            menu         addButtonTarget: self
                             ScriptBlock: [target showVMForEvent: event]
                                   Label: 'Show VM'.
            menu         addButtonTarget: self
                             ScriptBlock: [target showExportedVMForEvent: event]
                                   Label: 'Show exported VM'.
            menu         addButtonTarget: self
                             ScriptBlock: [target showExportPolicyForEvent: event]
                                   Label: 'Show export policy'.
            menu         addButtonTarget: self
                 AsynchronousScriptBlock: [target showExportedObjectForEvent: event]
                                   Label: 'Show object...'.

            menu addDivider.

            menu         addButtonTarget: self
                 AsynchronousScriptBlock: [target verifyVM: event]
                                   Label: 'Verify VM'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         addStatisticsButtonsTo: menu = ( |
            | 
            vmImage statisticsMethodNames do: [|:n|
              [showObject: 0 ForEvent: event].
              menu addButtonTarget: self
                            Script: 'target showObject: target vmImage ', n, ' ForEvent: event'
                             Label: n.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fCategory: choosing a program\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         addVMsToMenu: menu = ( |
            | 
            virtualMachineSlots do: [|:s|
              menu addButtonTarget: self
                    AsynchronousScriptBlock: [target chooseVM: button label.
                                              target buildProgramForEvent: event]
                         Label: s name
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fCategory: choosing a program\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         assemblerProgramSlots = ( |
            | 
            (reflect: programNameSpace) asList copyFilteredBy: [|:s| s visibility isPublic]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         buildFromProxy: aProxy Event: evt IfFail: fb = ( |
            | 
            proxyForBuilding: aProxy.
            popUpMenuForBuilding: evt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         buildProgramForEvent: evt = ( |
            | 
            isProfilingOn ifTrue: [[buildProgramIfFail: [|:e| ^ reportError: e Event: evt]] profile: 0.02 ]
                           False: [ buildProgramIfFail: [|:e| ^ reportError: e Event: evt]                ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         buildProgramIfFail: fb = ( |
            | 
            vmImage:
              programBuilder buildUsingProxy: proxyForBuilding
                                 ReportingTo: self
                                      IfFail: [|:e| fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fCategory: choosing a platform\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         choosePlatform: platName = ( |
            | 
            platformName: platName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fCategory: choosing a program\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         chooseProgram: progName = ( |
            | 
            programName: progName.
            [conditionCodeTest. empty. endlessLoop. testLoadMacros. trap. triangle]. "browsing"
            programBuilder: progName sendTo: programNameSpace.
            status: programBuilder buildActionName, ' with the middle button').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fCategory: choosing a program\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         chooseVM: vmName = ( |
            | 
            programName: vmName.
            [miniVM. midiVM. selfVM]. "browsing"
            programBuilder:
              vmKit vmBuilder1 copyForBuilding:
                  (vmName sendTo: virtualMachineNameSpace) copyForArchitecture: platformName.

            status: programBuilder buildActionName, ' with the middle button').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: background menu\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         contributeToBackgroundMenu: m = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         copyForArchitecture: arch = ( |
            | resend.copy initializeForArchitecture: arch).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         defaultButtonHolder <- bootstrap stub -> 'globals' -> 'nil' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: style\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         defaultColor = paint copyRed: 0.259042 Green: 0.445748  Blue: 0.353861.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: dropping\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         doesDebuggingHostMorphWantMe = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForArchitecture: arch = ( |
            | 
            resend.initialize.
            initializeProgramName.
            initializePlatformName: arch.
            initializeProgram.
            initializeStatus.
            colorAll: defaultColor.
            copiedImages: list copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: platform name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializePlatformName: arch = ( |
             n.
            | 
            n: arch.
            platformNameLabel:
              platformNameLabel  copyLabel: platformPrefix, n
                                  FontSpec: fontSpec
                                     Color: paint named: 'black'.
            addMorphLast: platformNameLabel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeProgram = ( |
            | 
            program: nil).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: program name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeProgramName = ( |
            | 
            programNameLabel: 
              programNameLabel  copyLabel: noProgram
                                 FontSpec: fontSpec
                                    Color: paint named: 'black'.
            addMorphLast: programNameLabel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: status\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeStatus = ( |
            | 
            statusLabel:
              statusLabel copyLabel: 'pick a program by dropping me on a foreignHostMorph'
                           FontSpec: fontSpec
                              Color: paint named: 'black'.
            addMorphLast: statusLabel).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         isForeignProgramMorph = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: building\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         isProfilingOn = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         isReadyToLaunch = ( |
            | 
            programName != noProgram).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: launching\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         launchFromProxy: aProxy DebugPerformance: aBool IfFail: fb = ( |
             proc.
            | 
            proc: vmKit foreignProcess copyAndLaunch: vmImage 
                                       OnDebugServer: aProxy
                                DebuggingPerformance: aBool
                                              IfFail: [|:e| ^ fb value: e].

            "nil for asm image"
            proc myVM ifNotNil: [copiedImages addLast: proc myVM image].

            "return debugger outliner for testing"
            vmKit foreignProcessModel debugProcess: proc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: launching\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         launchFromProxy: aProxy Event: evt DebugPerformance: aBool IfFail: fb = ( |
            | 
            launchFromProxy: aProxy DebugPerformance: aBool IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: dropping\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         launchOrBuildFromProxy: aProxy Event: evt DebugPerformance: aBool IfFail: fb = ( |
            | 
            isReadyToLaunch ifTrue: [ launchFromProxy: aProxy Event: evt DebugPerformance: aBool IfFail: fb ]
                             False: [  buildFromProxy: aProxy Event: evt                         IfFail: fb ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: basics\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         morphTypeName = 'foreignProgramMorph'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: platform name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         noPlatform = ( |
            | 'no platform').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: program name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         noProgram = ( |
            | 'no program').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         parent* = bootstrap stub -> 'globals' -> 'abstractSimpleApplicationMorph' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: platform name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         platformName = ( |
            | 
            platformNameLabel label
              copyFrom: platformPrefix size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: platform name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         platformName: pn = ( |
            | 
            platformNameLabel label: platformPrefix, pn.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fCategory: choosing a platform\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         platformNameSpace = ( |
            | 
            vmKit exportPlatforms).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fCategory: choosing a platform\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         platformNames = ( |
            | 
            ((reflect: platformNameSpace) asList
                copyMappedBy: [|:s| s name])
              asVector sort).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: platform name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         platformPrefix = 'on '.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         popUpMenu: evt = ( |
            | 
            isReadyToLaunch ifFalse: [
              ^ reportError: 'Please drop me on a host morph instead'
                      Event: evt
            ].
            resend.popUpMenu:evt).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         popUpMenuForBuilding: evt = ( |
             menu.
            | 
            menu: ui2Menu copy color: nonpluggableOutliner menuColor.
            addBuildingButtonsToMenu: menu.
            defaultButtonHolder: menu initializeDefaultButtonHolder: defaultButtonHolder.
            menu popUp: evt.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         programButtonHolder <- bootstrap stub -> 'globals' -> 'nil' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: program name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         programName = ( |
            | 
            programNameLabel label).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: program name\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         programName: pn = ( |
            | 
            programNameLabel label: pn.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fCategory: choosing a program\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         programNameSpace = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: status\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         show: s = ( |
            | status: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: status\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         show: s While: blk = ( |
             os.
            | 
            os: status.
            status: s.
            blk onReturn: [status: os]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         showExportPolicyForEvent: event = ( |
            | 
            showObject: vmImage myVM exportPolicy ForEvent: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         showExportedObjectForEvent: event = ( |
             oop.
            | 
            oop:  (userQuery askString: 'Oop?') asIntegerIfFail: [^ self].
            showExportedObjectWithOop: oop ForEvent: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         showExportedObjectWithOop: oop ForEvent: event = ( |
            | 
            showMirror: (vmImage myVM noncachingMirrorFor: oop)
              ForEvent: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         showExportedVMForEvent: event = ( |
            | 
            showMirror: vmImage mirrorOnTheVM ForEvent: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         showMirror: m ForEvent: event = ( |
            | 
            safelyDo: [
              event sourceHand attach:
                event sourceHand world
                  outlinerForMirror: m
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         showObject: o ForEvent: event = ( |
            | 
            showMirror: (reflect: o) ForEvent: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         showVMForEvent: event = ( |
            | 
            showObject: vmImage myVM ForEvent: event).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: printing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         statePrintString = ( |
            | 
            resend.statePrintString, ' program: ', programName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: status\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         status = ( |
            | statusLabel label).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: status\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         status: s = ( |
            | statusLabel label: s. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fCategory: choosing a program\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         virtualMachineNameSpace = ( |
            | 
            vmKit virtualMachines).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fCategory: choosing a program\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         virtualMachineSlotSorter = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> 'virtualMachineSlotSorter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda foreignProgramMorph parent virtualMachineSlotSorter.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: menuing\x7fCategory: choosing a program\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         virtualMachineSlots = ( |
            | 
            (
              (reflect: virtualMachineNameSpace) asList copyFilteredBy: [|:s| s visibility isPublic]
            ) asVector sortBy: virtualMachineSlotSorter).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> () From: ( | {
         'Category: foreignProgramMorph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (labelMorph copy)\x7fVisibility: private'
        
         platformNameLabel <- labelMorph copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> () From: ( | {
         'Category: foreignProgramMorph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         program.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> () From: ( | {
         'Category: foreignProgramMorph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         programBuilder.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> () From: ( | {
         'Category: foreignProgramMorph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (labelMorph copy)\x7fVisibility: private'
        
         programNameLabel <- labelMorph copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> () From: ( | {
         'Category: filing out\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            kleinAndYoda foreignProgramMorph).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> () From: ( | {
         'Category: foreignProgramMorph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         proxyForBuilding.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> () From: ( | {
         'Category: foreignProgramMorph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (labelMorph copy)\x7fVisibility: private'
        
         statusLabel <- labelMorph copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'foreignProgramMorph' -> () From: ( | {
         'Category: foreignProgramMorph State\x7fModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         vmImage.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot'
        
         vmKitMorphs = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMorphs' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMorphs' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitMorphs.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMorphs' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMorphs' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMorphs' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMorphs' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMorphs' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.10 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitMorphs' -> () From: ( | {
         'ModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'morph' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         doesDebuggingHostMorphWantMe = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'morph' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitMorphs InitialContents: FollowSlot\x7fVisibility: public'
        
         isForeignProgramMorph = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 



 '-- Side effects'

 globals modules vmKitMorphs postFileIn
