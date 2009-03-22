 '$Revision: 30.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: yodaReflection InitialContents: FollowSlot'
        
         yodaReflection = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'yodaReflection' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'yodaReflection' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules yodaReflection.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaReflection' -> () From: ( | {
         'ModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaReflection' -> () From: ( | {
         'ModuleInfo: Module: yodaReflection InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaReflection' -> () From: ( | {
         'ModuleInfo: Module: yodaReflection InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaReflection' -> () From: ( | {
         'ModuleInfo: Module: yodaReflection InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
            resend.postFileIn.
            yoda mirrors createPrototypes.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaReflection' -> () From: ( | {
         'ModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'yodaReflection' -> () From: ( | {
         'ModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'yodaActivations
yodaProcess
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         mirrors = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'mirrors' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda mirrors.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'mirrors' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         copyDownParentForActivationPrototypes = bootstrap define: bootstrap stub -> 'globals' -> 'yoda' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () ToBe: bootstrap addSlotsTo: (
             globals kleinAndYoda mirrors copyDownParentForMirrorPrototypes copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'mirrors' -> 'copyDownParentForActivationPrototypes' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda mirrors copyDownParentForActivationPrototypes.

CopyDowns:
globals kleinAndYoda mirrors copyDownParentForMirrorPrototypes. copy

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'mirrors' -> () From: ( | {
         'ModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'mirrors' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'mirrors' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = bootstrap stub -> 'globals' -> 'yoda' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         reflectionPrimitives = bootstrap define: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda reflectionPrimitives copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda reflectionPrimitives.

CopyDowns:
globals kleinAndYoda reflectionPrimitives. copy 
SlotsToOmit: parent.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> () From: ( | {
         'ModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals yoda reflectionPrimitives parent.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ContentsAt: n IfFail: fb = ( |
            | 
            forMirror: m ContentsAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ExpressionStackIfFail: fb = ( |
             end.
             r.
             start.
            | 
            start: vmKit layouts activation first_stack_offset.
            end: m reflecteeAt: vmKit layouts activation sp_field fixedIndex IfFail: [|:e| ^ fb value: e].
            r: vector copySize: end - start.
            start upTo: end Do: [|:i|
              r at: i - start Put: m reflecteeMirrorAt: i IfFail: [|:e| ^ fb value: e]
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IfDead: blk = ( |
            | 
            "Yoda activations are always live if reachable, I guess. -- dmu 3/6"
            m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IsArgumentAt: n IfFail: fb = ( |
            | 
            forMirror: m IsArgumentAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IsAssignableAt: n IfFail: fb = ( |
            | 
            forMirror: m IsAssignableAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IsAssignmentAt: n IfFail: fb = ( |
            | 
            forMirror: m IsAssignmentAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IsParentAt: n IfFail: fb = ( |
            | 
            forMirror: m IsParentAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m LexicalParentIfFail: fb = ( |
             rcvr.
            | 
            rcvr: m receiverIfFail: [|:e| ^ fb value: e].
            [rcvr isReflecteeBlock] assert.
            rcvr reflectionPrimitives forBlockMirror: rcvr HomeFrameIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m MethodHolderIfFail: fb = ( |
            | 
            m reflecteeMirrorAt:  vmKit layouts activation methodHolder_field fixedIndex
                         IfFail:  fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m MethodMirrorIfFail: fb = ( |
            | 
            m mapMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m NamesIfFail: fb = ( |
            | 
            forMirror: m NamesIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m PositionIfFail: fb = ( |
            | 
            (m reflecteeAt: vmKit layouts activation pc_field fixedIndex IfFail: [|:e| ^ fb value: e]) pred).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ReceiverIfFail: fb = ( |
            | 
            m reflecteeMirrorAt:  vmKit layouts activation rcvr_field fixedIndex
                         IfFail:  fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ReceiverOopIfFail: fb = ( |
            | 
            withMapDo: [
              map         for: (reflecteeOopIfFail: [|:e| ^ fb value: e])
                  IndexableAt: vmKit layouts activation rcvr_field fixedIndex
                       IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ReflecteeEq: x IfFail: fb = ( |
            | 
            forMirror: m ReflecteeEq: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ReflecteeIdentityHashIfFail: fb = ( |
            | 
            forMirror: m ReflecteeIdentityHashIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SelectorIfFail: fb = ( |
             bci.
             s.
             sels.
            | 
            s: m senderIfFail: [^ myVM startSelector].
            bci: s position.
            sels: vmKit selectorFinder copyInterpretMethod: s To: bci.
            sels at: bci).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SelectorMirrorIfFail: fb = ( |
            | 
            reflect: m selectorIfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SelfIfFail: fb = ( |
            | 
            m reflecteeMirrorAt:  vmKit layouts activation self_field fixedIndex
                         IfFail:  fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SenderIfFail: fb = ( |
             r.
            | 
            r: m reflecteeMirrorAt:  vmKit layouts activation sender_field fixedIndex
                            IfFail: [|:e| ^ fb value: e].
            r isReflecteeInteger ifTrue: [^ fb value: 'no sender'].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SourceIfFail: fb = ( |
            | 
            withMapDo: [map sourceString] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SourceMirrorIfFail: fb = ( |
            | 
            [todo wrongKindOfMirror]. "Could change this to get a Yoda mirror on the actual remote object."
            reflect: m sourceStringIfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: blocks\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         forBlockMirror: m HomeFrameIfFail: fb = ( |
            | 
            withMapDo: [
              mirrorFor: (vmKit layouts block homeFramePointerOf: (reflecteeOopIfFail: [|:e| ^ fb value: e])
                                                          IfFail: [|:e| ^ fb value: e])
                 IfFail: fb
            ] IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         numberOfMirrorsToImportIfFail: fb = ( |
            | 
            myMirror isReflecteeActivation ifTrue: [
              withMapDo: [
                vmKit layouts smi decode:
                  map for: (reflecteeOopIfFail: [|:e| ^ fb value: e])
                      IndexableAt: vmKit layouts activation sp_field fixedIndex
                      IfFail: [|:e| ^ fb value: e]
              ] IfFail: fb
            ] False: [
              resend.numberOfMirrorsToImportIfFail: fb
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'reflectionPrimitives' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            yoda reflectionPrimitives).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'yoda' -> 'reflectionPrimitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: yodaReflection InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKit = ( |
            | 
            yoda).
        } | ) 



 '-- Sub parts'

 bootstrap read: 'yodaActivations' From: 'applications/klein'
 bootstrap read: 'yodaProcess' From: 'applications/klein'



 '-- Side effects'

 globals modules yodaReflection postFileIn
