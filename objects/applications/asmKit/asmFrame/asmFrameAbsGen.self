 '$Revision: 30.14 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot'
        
         abstractGenerator = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals assemblerSystems framework generators abstractGenerator.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: InitializeToExpression: (false)\x7fVisibility: private'
        
         hasBeenFinished <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         myAssemblerSystem.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( |
             {} = 'Comment: Utility methods rely on object structure:
that main assembler object has name space slots each of which have
the same name as the slot in the generator object that refers
to my receiver. -- dmu 9/1\x7fModuleInfo: Creator: globals assemblerSystems framework generators abstractGenerator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: utilities that rely on generator structure\x7fComment: Get the leaf category from the sender method and
pass it on as subcategory in the genrator result.\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         assignSubcategoryFromSender = ( |
             activation.
             holder.
            | 

            activation: process this stack findFirst: [|:a| a selector = 'assignSubcategoryFromSender']
                                  IfPresent: [|:a| a]
                                  IfAbsent: [error: 'not found'].
            activation: activation sender.

            holder: activation methodHolder at: activation selector.
            subcategory: holder categories last).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: creating slots\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         at: name Put: contents = ( |
            | 
            at: name PutContents: reflect: contents).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: creating slots\x7fComment: Central functionality of the generator protocol.
Creates a slot in target name space named \"name\"
containing reflectee of \"contentsMirror\".
Optimizes performance by using copying operators
to build temporary namespace in my \"result\" slot.
Sender must send \"finish\" message after last
call to me. -- DU 09/01\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         at: name PutContents: contentsMirror = ( |
             aSlot.
             resultMirror.
            | 
            ('Generated: ', name) printLine.

            "Code below contorted to use faster functional (copying) operations."

            resultMirror:  reflect: result.
            contentsMirror isReflecteeMethod ifFalse: [
              contentsMirror do: [|:slot| slot module: moduleNameForGeneratedSlots]. "hack to avoid filing out"
            ].
            resultMirror:  resultMirror copyAt: name PutContents: contentsMirror.

            aSlot: resultMirror at: name.
            aSlot: aSlot copyHolderForModule: moduleNameForGeneratedSlots. "avoid filing out"
            aSlot: aSlot copyHolderForCategory: targetCategory.
            aSlot: aSlot copyHolderForComment:  targetComment.

            result: aSlot holder reflectee.

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: public'
        
         cleanAll = ( |
             cs.
             m.
             orig.
            | 
            orig: reflect: targetNameSpace.
            m: orig.
            cs: m creatorSlotHint.
            orig do: [|:s|
              s categories isEmpty not && [s categories first = targetCategoryPrefix]
                ifTrue: [m: (m at: s name) copyHolderForRemove].
            ].
            orig frozenDefine: m.
            orig creatorSlot: cs.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForAssemblerSystem: a = ( |
            | 
            copy myAssemblerSystem: a).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: creating slots\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: public'
        
         createModuleForGeneratedSlots = ( |
            | 
            transporter moduleDictionary add: moduleNameForGeneratedSlots.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: creating slots\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         finish = ( |
             cs.
             isClean <- bootstrap stub -> 'globals' -> 'false' -> ().
             mod.
             new.
             old.
            | 
            finishOnlyOnce.
            old: reflect: targetNameSpace.
            new: reflect: result.
            cs: old creatorSlotHint.
            old frozenDefine: new.
            cs module isEmpty ifFalse: [
              mod: transporter moduleDictionary at: cs module.
              isClean: mod isClean.
            ].
            cs makeCreator.
            isClean ifTrue: [ mod beClean ].

            ["Making them all well-known means that they all have
              different maps, which means that when we map the
              assembler over to Klein, it does way more work
              than it needs to. So we don't make them well-known
              anymore. -- Adam, 11/04"
            makeAllWellKnownIn: old.
            ].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: creating slots\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         finishOnlyOnce = ( |
            | 
            hasBeenFinished ifTrue: [
              error: 'finish can only be called once for each generator object'
            ].
            hasBeenFinished: true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: public'
        
         generateAll = ( |
            | childResponsibility).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: creating slots\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         makeAllWellKnownIn: aNameSpaceMirror = ( |
            | 
            aNameSpaceMirror do: [|:s. c|
              s contents isReflecteeMethod ifFalse: [
                c: s categories. 
                c isEmpty not && [c first = targetCategoryPrefix]
                  ifTrue: [s contents creatorSlot: s].
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: creating slots\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         moduleNameForGeneratedSlots = 'asmGeneratedSlots'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: utilities that rely on generator structure\x7fComment: Returns the last component of my path name.

Example:
\"assemblerSystems framework generators registers myLastName\"
returns \'registers\'\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         myLastName = ( |
            | asMirror creatorSlotHint name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: creating slots\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         start = ( |
             resultMirror.
            | 
            "initialize result object to whatever is in target name space
            MINUS any automatically-created slots"
            result ifNotNil: [
              error: 'start was already called, should only be called once per generator object'
            ].
            resultMirror: reflect: targetNameSpace.
            resultMirror do: [|:s. c|
               c: s categories. 
               c isEmpty not  &&  [ c first  =  targetCategoryPrefix ]  ifTrue: [
                 resultMirror: resultMirror copyRemoveSlot: s name
               ].
            ].
            result: resultMirror reflectee.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: utilities that rely on generator structure\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         targetCategory = ( |
             cats.
            | 
            cats: (targetCategoryPrefix & targetCategorySuffix) asVector.
            subcategory isEmpty ifFalse: [cats: cats copyAddLast: subcategory].
            traits cachedSlotAnnotation convertCategoryListToString: cats).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: utilities that rely on generator structure\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         targetCategoryPrefix = ( |
            | 'automatically generated').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: utilities that rely on generator structure\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         targetCategorySuffix = ( |
            | myLastName).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: utilities that rely on generator structure\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         targetComment = ( |
            | 
            'automatically generated by: ', asMirror name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> 'parent' -> () From: ( | {
         'Category: generating and cleaning\x7fCategory: utilities that rely on generator structure\x7fModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         targetNameSpace = ( |
            | 
            myLastName sendTo: myAssemblerSystem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: InitializeToExpression: (nil)'
        
         result.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'assemblerSystems' -> 'framework' -> 'generators' -> 'abstractGenerator' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: InitializeToExpression: (\'\')'
        
         subcategory <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot'
        
         asmFrameAbsGen = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameAbsGen' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameAbsGen' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules asmFrameAbsGen.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameAbsGen' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/asmKit/asmFrame'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameAbsGen' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameAbsGen' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameAbsGen' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot'
        
         postFileIn = ( |
            | 
            resend.postFileIn.
            assemblerSystems framework generators abstractGenerator createModuleForGeneratedSlots).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameAbsGen' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.14 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'asmFrameAbsGen' -> () From: ( | {
         'ModuleInfo: Module: asmFrameAbsGen InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'asmFrameGens
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'asmFrameGens' From: 'applications/asmKit/asmFrame'



 '-- Side effects'

 globals modules asmFrameAbsGen postFileIn
