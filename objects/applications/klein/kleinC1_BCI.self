 '$Revision: 30.22 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         bytecodeInterpreter = bootstrap define: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals abstractBytecodeInterpreter copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes bytecodeInterpreter.

CopyDowns:
globals abstractBytecodeInterpreter. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (list copyRemoveAll)'
        
         irNodes <- list copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'Category: compiler1\x7fModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (nil)'
        
         myCompiler.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals klein compiler1s abstract parent prototypes bytecodeInterpreter parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         accessLocal: bc = ( |
            | 
            addNodeProto: ( bc isWrite ifTrue: [irNodeProtos writeLocal] False: [irNodeProtos readLocal] )
                       BC: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: building IR\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         addNode: n = ( |
            | 
            irNodes addLast: n.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: building IR\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         addNodeProto: irNodeProto BC: bc = ( |
            | 
            addNode: irNodeProto copyBC: bc Compiler: myCompiler.
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            resend.copy
            irNodes: irNodes copyRemoveAll).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForCompiler: c = ( |
            | 
            copy initializeForCompiler: c).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         indexedBranch: bc = ( |
            | 
            addNodeProto: irNodeProtos indexedBranch BC: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         initializeForCompiler: c = ( |
            | 
            myCompiler: c.
            initializeForMethod: c method.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: building IR\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         irNodeProtos = ( |
            | 
            myCompiler prototypes irNodes).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         nonlocalReturn: bc = ( |
            | 
            "This also gets called for ^'s in non-block methods.
             (for syntactical compatibility with Smalltalk),
              but in that case the bytecode is a nop. -- dmu 5/05"
            method isReflecteeBlockMethod ifTrue: [
              addNodeProto: irNodeProtos nonlocalReturn BC: bc
            ].
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         outermostMethodHolderFor: bc = ( |
            | 
            bc isResend
                 ifTrue: [reflect: myCompiler outermostMethodHolder]
                  False: [selfMirror]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'abstractBytecodeInterpreter' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         pop: bc = ( |
            | 
            addNodeProto: irNodeProtos pop BC: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         pushLiteral: bc = ( |
             p.
            | 
            p: (reflect: bc oopToPush) isReflecteeBlock
                 ifTrue: [irNodeProtos blockLiteral]
                  False: [irNodeProtos      literal].
            addNodeProto: p BC: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         pushSelf: bc = ( |
            | 
            addNodeProto: irNodeProtos selfNode BC: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         scalarBranch: bc = ( |
            | 
            addNodeProto: irNodeProtos scalarBranch BC: bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         selfMirror = ( |
            | myCompiler selfMirror).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: interpreting particular codes, return bc by default\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         send: bc = ( |
             slot.
             slots.
            | 
            bc isPrimitive    ifTrue:  [^ addNodeProto: (irNodeProtos primitiveProtoForBC: bc) BC: bc].
            shouldDoInlining  ifFalse: [^ addNodeProto:  irNodeProtos send                     BC: bc].
            bc isSelfExplicit ifTrue:  [^ addNodeProto:  irNodeProtos send                     BC: bc].

            slots:  bc asMessage lookupSlotsUsing: 
               theVM exportPolicy slotFinder
                  copyForMirror: outermostMethodHolderFor: bc.

            slots size = 1    ifFalse: [^ addNodeProto: irNodeProtos send      BC: bc]. "will be error"
            slot: slots first.
            [todo dynamicInheritance. "what if a parent is assignable?"].

            "Don't bother inlining accesses to data slots that are
             up in parents, because there are lots of methods with those
             (anything that accesses a global, for example), and we want
             more nmethods to be reusable (for now). -- Adam & Alex, 6/04"
            shouldInlineAccessesToDataSlotsInParents ifFalse: [
              slot holder = selfMirror ifFalse: [^ addNodeProto: irNodeProtos send BC: bc].
            ].

            case 
              if: [slot isAssignment] Then: [
                addNode: irNodeProtos inlinedAssignment copyBC: bc Compiler: myCompiler Slot: slot.
              ] 
              If: [slot isMethod] Then: [
                addNodeProto: irNodeProtos send BC: bc
              ]
              If: [slot isAssignable] Then: [
                addNode: irNodeProtos inlinedAccess copyBC: bc Compiler: myCompiler Slot: slot.
              ]
              Else: [
                [todo dependencies].
                addNode: irNodeProtos inlinedConstantAccess 
                 copyBC: bc Compiler: myCompiler
                   Slot: slot.
              ].
            bc).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldDoInlining = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'klein' -> 'compiler1s' -> 'abstract' -> 'parent' -> 'prototypes' -> 'bytecodeInterpreter' -> 'parent' -> () From: ( | {
         'Category: inlining\x7fModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         shouldInlineAccessesToDataSlotsInParents = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         kleinC1_BCI = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules kleinC1_BCI.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.22 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'kleinC1_BCI' -> () From: ( | {
         'ModuleInfo: Module: kleinC1_BCI InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules kleinC1_BCI postFileIn
