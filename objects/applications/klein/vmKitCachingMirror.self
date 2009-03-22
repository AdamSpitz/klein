 '$Revision: 1.7 $'
 '
Copyright 2006 Sun Microsystems, Inc. All rights reserved. Use is subject to license terms.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: remote running & debugging\x7fCategory: reflection objects\x7fCategory: mirrors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         cachingReflectionPrimitives = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda cachingReflectionPrimitives.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> () From: ( | {
         'Category: caching\x7fCategory: fake slots\x7fCategory: vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedMethodPointer.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> () From: ( | {
         'Category: caching\x7fCategory: fake slots\x7fCategory: vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         cachedMethodPointerError.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         noncachingReflectionPrimitives.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda cachingReflectionPrimitives parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksContainingKeyedMessagesToCache = ( |
            | 
            ( 
              [forMirror:           m AnnotationAt:   n IfFail: fb]
            & [forMirror:           m ContentsAt:     n IfFail: fb]
            & [forMirror:           m IsArgumentAt:   n IfFail: fb]
            & [forMirror:           m IsAssignmentAt: n IfFail: fb]
            & [forMirror:           m IsAssignableAt: n IfFail: fb]
            & [forMirror:           m IsParentAt:     n IfFail: fb]

            & [forActivationMirror: m AnnotationAt:   n IfFail: fb]
            & [forActivationMirror: m ContentsAt:     n IfFail: fb]
            & [forActivationMirror: m IsArgumentAt:   n IfFail: fb]
            & [forActivationMirror: m IsAssignmentAt: n IfFail: fb]
            & [forActivationMirror: m IsAssignableAt: n IfFail: fb]
            & [forActivationMirror: m IsParentAt:     n IfFail: fb]

            ) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         blocksContainingMessagesToCache = ( |
            | 
            [inProgress adam aaaa refactoringMirrors showDave]. "Are the commented-out ones necessary? If so, we're going to
                                                                 end up with more stuff on the reflectionPrimitives object
                                                                 that's notExactlyAPrimitive."

            ( [forMirror:           m                   MapMirrorIfFail: fb]
            & [forMirror:           m                NMethodCacheIfFail: fb]
            & [forMirror:           m                       NamesIfFail: fb]
            & [forMirror:           m                  AnnotationIfFail: fb]
            "& [annotationIfFail: fb]"  "the above line only caches the unparsed annotation"

            & [forNMethodMirror:    m                MethodMirrorIfFail: fb]
            & [forNMethodMirror:    m     DisassembledMachineCodeIfFail: fb]
            & [forNMethodMirror:    m                EntryAddressIfFail: fb]
            "& [disassembledCodesIfFail: fb]" "doesn't go over the wire, but does a whole interpretation of the method"

            & [forMethodMirror:     m  InitialContentsBySlotIndexIfFail: fb]

            & [forActivationMirror: m                      SenderIfFail: fb]
            & [forActivationMirror: m               NMethodMirrorIfFail: fb]
            & [forActivationMirror: m                       NamesIfFail: fb]

            & [forByteVectorMirror: m              ReflecteeBytesIfFail: fb]
            & [forByteVectorMirror: m               ReflecteeSizeIfFail: fb]

            & [forVectorMirror:     m               ReflecteeSizeIfFail: fb]
            & [forVectorMirror:     m         MirrorsOnIndexablesIfFail: fb]

            ) asVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | 
            resend.copy initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: copying\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         copyForNoncachingReflectionPrimitives: ncrp = ( |
            | 
            (resend.copy noncachingReflectionPrimitives: ncrp) initialize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ActivationMapIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m ActivationMapIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m AnnotationAt: n IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m AnnotationAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m CodesIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m CodesIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m CodesMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m CodesMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ContentsAt: n IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m ContentsAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ExpressionStackIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m ExpressionStackIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IfDead: blk = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m IfDead: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IsArgumentAt: n IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m IsArgumentAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IsAssignableAt: n IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m IsAssignableAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IsAssignmentAt: n IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m IsAssignmentAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m IsParentAt: n IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m IsParentAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m LexicalParentIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m LexicalParentIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m LiteralsIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m LiteralsIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m LiteralsMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m LiteralsMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m MethodHolderIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m MethodHolderIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m MethodMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m MethodMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m NMethodMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m NMethodMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m NamesIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m NamesIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m PositionIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m PositionIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ReceiverIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m ReceiverIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ReceiverOopIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m ReceiverOopIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ReflecteeEq: x IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m ReflecteeEq: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m ReflecteeIdentityHashIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m ReflecteeIdentityHashIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SelectorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m SelectorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SelectorMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m SelectorMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SelfIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m SelfIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SenderIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m SenderIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SourceIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m SourceIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: activations\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forActivationMirror: m SourceMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forActivationMirror: m SourceMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fCategory: block methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forBlockMethodMirror: m CreateBlockIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forBlockMethodMirror: m CreateBlockIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fCategory: block methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forBlockMethodMirror: m LexicalParentIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forBlockMethodMirror: m LexicalParentIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: blocks\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forBlockMirror: m HomeFrameIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forBlockMirror: m HomeFrameIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: blocks\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forBlockMirror: m LexicalParentIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forBlockMirror: m LexicalParentIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forByteVectorMirror: m ReflecteeAt: idx IfFail: fb = ( |
            | 
            (m reflecteeBytesIfFail: [|:e| ^ fb value: e]) at: idx IfAbsent: [fb value: 'absent']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forByteVectorMirror: m ReflecteeAt: idx Put: val IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forByteVectorMirror: m ReflecteeAt: idx Put: val IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forByteVectorMirror: m ReflecteeBytesIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forByteVectorMirror: m ReflecteeBytesIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forByteVectorMirror: m ReflecteeMirrorAt: idx IfFail: fb = ( |
            | 
            reflect: forByteVectorMirror: m ReflecteeAt: idx IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forByteVectorMirror: m ReflecteeSizeIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forByteVectorMirror: m ReflecteeSizeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m AllSlotsOnThisMethodIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m AllSlotsOnThisMethodIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m CodesIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m CodesIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m CodesMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m CodesMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m FileIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m FileIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m FileMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m FileMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         forMethodMirror: m InitialContentsBySlotIndexIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m InitialContentsBySlotIndexIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m LineIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m LineIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m LineMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m LineMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m LiteralsIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m LiteralsIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m LiteralsMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m LiteralsMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m NameAt: index IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m NameAt: index IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m PositionForPositionTableAtBCI: bci = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m PositionForPositionTableAtBCI: bci).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m PrimitiveContentsAtIndex: i IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m PrimitiveContentsAtIndex: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m ReflecteeAt: i IfFail: fb = ( |
            | 
            forVectorMirror: m ReflecteeAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m ReflecteeAt: i Put: val IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m ReflecteeAt: i Put: val IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m ReflecteeMirrorAt: i IfFail: fb = ( |
            | 
            forVectorMirror: m ReflecteeMirrorAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m ReflecteeSizeIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m ReflecteeSizeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m SourceIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m SourceIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m SourceLengthIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m SourceLengthIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m SourceLengthMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m SourceLengthMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m SourceMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m SourceMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m SourceOffsetIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m SourceOffsetIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMethodMirror: m SourceOffsetMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMethodMirror: m SourceOffsetMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: slot-like primitives\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m AnnotationAt: n IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m AnnotationAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: annotation\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m AnnotationIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m AnnotationIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: slot-like primitives\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m ContentsAt: n IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m ContentsAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: annotation\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m CopyAnnotation: a IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m CopyAnnotation: a IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: programming primitives\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m CopyAt: n Put: objMirr IsParent: p IsArgument: isA Annotation: a IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m CopyAt: n Put: objMirr IsParent: p IsArgument: isA Annotation: a IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: programming primitives\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m CopyRemoveSlot: n IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m CopyRemoveSlot: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: replacing one object with another\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m Define: newObjMir IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m Define: newObjMir IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: naming\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m EvalNameIfNoStoreStringOrCreatorNameIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m EvalNameIfNoStoreStringOrCreatorNameIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: evaluating\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m Evaluate: otherMir = ( |
            | 
            noncachingReflectionPrimitives forMirror: m Evaluate: otherMir).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: slot-like primitives\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m IsArgumentAt: n IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m IsArgumentAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: slot-like primitives\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m IsAssignableAt: n IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m IsAssignableAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: slot-like primitives\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m IsAssignmentAt: n IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m IsAssignmentAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: testing\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m IsOKToSend: selector = ( |
            | 
            noncachingReflectionPrimitives forMirror: m IsOKToSend: selector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: slot-like primitives\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m IsParentAt: n IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m IsParentAt: n IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m MapMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m MapMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m NMethodCacheIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m NMethodCacheIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: naming\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m NameSuffixIfFail: fb = ( |
            | 
            (noncachingReflectionPrimitives forMirror: m NameSuffixIfFail: [|:e| ^ fb value: e]), ' (cached)').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: names\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m NamesIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m NamesIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: comparing\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m ReflecteeEq: x IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m ReflecteeEq: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: naming\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m ReflecteeIDIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m ReflecteeIDIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: comparing\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m ReflecteeIdentityHashIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m ReflecteeIdentityHashIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: reflectee\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forMirror: m ReflecteeIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forMirror: m ReflecteeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forNMethodMirror: m DisassembledMachineCodeIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forNMethodMirror: m DisassembledMachineCodeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forNMethodMirror: m EntryAddressIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forNMethodMirror: m EntryAddressIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forNMethodMirror: m MethodHolderIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forNMethodMirror: m MethodHolderIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forNMethodMirror: m MethodMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forNMethodMirror: m MethodMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: nmethods\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forNMethodMirror: m SelectorMirrorIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forNMethodMirror: m SelectorMirrorIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ExportContentsOf: aVector IntoReflecteeIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forVectorMirror: m ExportContentsOf: aVector IntoReflecteeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ImportReflecteeAsVectorOfImmediatesIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forVectorMirror: m ImportReflecteeAsVectorOfImmediatesIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m MirrorsOnIndexablesIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forVectorMirror: m MirrorsOnIndexablesIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ReflecteeAt: idx IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forVectorMirror: m ReflecteeAt: idx IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ReflecteeAt: idx Put: val IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forVectorMirror: m ReflecteeAt: idx Put: val IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ReflecteeMethodPointerIfFail: fb = ( |
            | 
            invalidateMyObsoleteCachedItems.
            cachedMethodPointerError ifNil: [
              cachedMethodPointer: noncachingReflectionPrimitives forVectorMirror: m ReflecteeMethodPointerIfFail: [|:e|
                cachedMethodPointerError: e. ^ fb value: e]
            ].
            cachedMethodPointer ifNil: [fb value: cachedMethodPointerError]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: different kinds of objects\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ReflecteeMirrorAt: idx IfFail: fb = ( |
            | 
            (forVectorMirror: m MirrorsOnIndexablesIfFail: [|:e| ^ fb value: e]) at: idx IfAbsent: [fb value: 'absent']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         forVectorMirror: m ReflecteeSizeIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives forVectorMirror: m ReflecteeSizeIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSlots = ( |
            | 
            slotGenerator copyGenerateSlotsIn: self.
            keyedSlotGenerator copyGenerateSlotsIn: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         generatedSlots = ( |
            | 
            asMirror asList copyFilteredBy: [|:s| slotGenerator isSlotGenerated: s]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fCategory: importing activation map\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         importReflecteeActivationMap = ( |
            | 
            importReflecteeActivationMapIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fCategory: importing activation map\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         importReflecteeActivationMapIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives importReflecteeActivationMapIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot'
        
         initialize = ( |
            | 
            invalidateCachedItems).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: initializing\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         initializePrototype = ( |
            | 
            removeGeneratedSlots.
            generateSlots.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: caching\x7fComment: Discards all cached information.\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateCachedItems = ( |
            | 
            noncachingReflectionPrimitives invalidateCachedItems.

            invalidateAutoGeneratedCacheSlots.
            invalidateAutoGeneratedKeyedCacheSlots.

            cachedMethodPointer: nil.
            cachedMethodPointerError: nil.

            timestampOfOldestCachedItem: time current.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         invalidateMyObsoleteCachedItems = ( |
            | 
            isMapCurrent ifFalse: [^ invalidateCachedItems].
            (myProxy ifNil: [^ nil]) invalidateObsoleteCachedItemsIn: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: testing\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         isForVM: aVM = ( |
            | 
            noncachingReflectionPrimitives isForVM: aVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: testing\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         isMapCurrent = ( |
            | 
            noncachingReflectionPrimitives isMapCurrent).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: testing\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         isMapSet = ( |
            | 
            noncachingReflectionPrimitives isMapSet).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: annotation\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         isOKToTransformAnnotationOfAbstractMirrorAfterParsing = ( |
            | 
            noncachingReflectionPrimitives isOKToTransformAnnotationOfAbstractMirrorAfterParsing).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         slotGenerator = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda cachingReflectionPrimitives parent slotGenerator.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda cachingReflectionPrimitives parent slotGenerator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         addSlotsIn: m = ( |
            | 
            m do: [|:s|
              s name = 'parent' ifTrue: [halt].
              s category: generatedSlotCategory.
              s module: moduleNameForGeneratedSlots.
              s comment: generatedSlotCategory.
              s visibility: s isMethod ifTrue: [visibility publicSlot] False: [visibility privateSlot].
              s initialContents:   transporter moduleInfo initializeToExpression copyForExpression: cacheSlotInitializer.
              targetMirror addSlot: s
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         assignmentToCacheSlot: bm = ( |
            | 
            (cacheSlotNameFor: bm), ': ').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         blocksContainingMessagesToCache = ( |
            | 
            targetMirror reflectee blocksContainingMessagesToCache).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         cacheSlotInitializer = 'nil'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         cacheSlotNameFor: blkMirror = ( |
             firstKwd.
             kwds.
             r <- ''.
            | 
            "No need to have two different dictionaries to cache the results of
                 forMirror:          ContentsAt:IfFail: and
                 forActivationMirror:ContentsAt:IfFail:,
             etc. -- Adam, 4/06"
            kwds: blkMirror getSelectorFromSource asSelector keywords.
            firstKwd: kwds removeFirst.
            ['for'     isPrefixOf: firstKwd] assert.
            ['Mirror:' isSuffixOf: firstKwd] assert.
            kwds do: [|:k| r: r & k].
            'cached_', (r flatString replace: ':' With: '_')).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         cacheSlotValue: bm = ( |
            | 
            cacheSlotNameFor: bm).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         copyGenerateSlotsIn: o = ( |
            | 
            (copy targetMirror: o asMirror) generateSlots).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         generateInvalidateMethod = ( |
             s.
            | 
            s: invalidationMethodName & ' = (
            "WARNING: this code generated by generateInvalidateMethod"\n'.
            blocksContainingMessagesToCache do: [|:b|  s: s & (cacheSlotNameFor: b asMirror) & ': ' & cacheSlotInitializer & '.\n'].
            s: s & 'self)'.
            addSlotsIn:  s flatString asSlotIfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSlots = ( |
            | 
            blocksContainingMessagesToCache do: [
              |:m| generateSlotsToCache: m].
            generateInvalidateMethod.
            moduleForGeneratedSlots beClean.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         generateSlotsToCache: blk = ( |
            | 
            addSlotsIn: slotsToAddToCache: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         generatedSlotCategory = 'automatically generated by klein cachingMirror slotGenerator'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         invalidationMethodName = 'invalidateAutoGeneratedCacheSlots'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         isSlotCached: s = ( |
            | s categories includes: 'cache me in child').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         isSlotGenerated: s = ( |
            | s categories includes: generatedSlotCategory).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         moduleForGeneratedSlots = ( |
            | 
            moduleNameForGeneratedSlots sendTo: modules).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         moduleNameForGeneratedSlots = 'init'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         slotsToAddToCache: blk = ( |
             accessMethodSource.
             bm.
             cacheSlotName.
             sourceOfSlotsToAdd.
             ss.
            | 
            bm: blk asMirror.
            ss: bm methodSource.
            [invalidateMyObsoleteCachedItems. ifNil: b]. "browsing"
            cacheSlotName: cacheSlotNameFor: bm.

            accessMethodSource: 
              ss, ' = (
            "WARNING: this code generated by slotsToAddToCache:"
            invalidateMyObsoleteCachedItems.
            ', (cacheSlotValue: bm), ' ifNil: [|result|
              result: resend.', ss, '.
              ', (assignmentToCacheSlot: bm), 'result.
              result
            ]).'.

            sourceOfSlotsToAdd: '(| ', accessMethodSource, ' ', cacheSlotName, ' <- ', cacheSlotInitializer, ' |)'.
            reflect: sourceOfSlotsToAdd eval).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: InitializeToExpression: (nil)\x7fVisibility: private'
        
         targetMirror.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         keyedSlotGenerator = bootstrap define: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'keyedSlotGenerator' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'parent' From:
             globals kleinAndYoda cachingReflectionPrimitives parent slotGenerator copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'keyedSlotGenerator' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda cachingReflectionPrimitives parent keyedSlotGenerator.

CopyDowns:
globals kleinAndYoda cachingReflectionPrimitives parent slotGenerator. copy 
SlotsToOmit: parent.

'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'keyedSlotGenerator' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'keyedSlotGenerator' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda cachingReflectionPrimitives parent keyedSlotGenerator parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'keyedSlotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         assignmentToCacheSlot: bm = ( |
            | 
            (cacheSlotNameFor: bm), ' at: n Put: ').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'keyedSlotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         blocksContainingMessagesToCache = ( |
            | 
            targetMirror reflectee blocksContainingKeyedMessagesToCache).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'keyedSlotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         cacheSlotInitializer = 'dictionary copyRemoveAll'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'keyedSlotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         cacheSlotValue: bm = ( |
            | 
            '(', (cacheSlotNameFor: bm), ' at: n IfAbsent: nil)').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'keyedSlotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         invalidationMethodName = 'invalidateAutoGeneratedKeyedCacheSlots'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'keyedSlotGenerator' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> 'slotGenerator' -> 'parent' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: comparing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         localReflecteeEq: x IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives localReflecteeEq: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: comparing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         localReflecteeIdentityHash = ( |
            | 
            noncachingReflectionPrimitives localReflecteeIdentityHash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: mirror state\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         map = ( |
            | 
            noncachingReflectionPrimitives map).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         myProxy = ( |
            | 
            (myVM machineMemory foreignProcess ifNil: [^ nil]) myProxy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: accessing\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         myVM = ( |
            | 
            noncachingReflectionPrimitives myVM).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         prototype = ( |
            | 
            kleinAndYoda cachingReflectionPrimitives).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeAddress = ( |
            | 
            noncachingReflectionPrimitives reflecteeAddress).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeAddressIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives reflecteeAddressIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeOID = ( |
            | 
            noncachingReflectionPrimitives reflecteeOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeOop = ( |
            | 
            noncachingReflectionPrimitives reflecteeOop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeOopIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives reflecteeOopIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         reflecteeTaggedOID = ( |
            | 
            noncachingReflectionPrimitives reflecteeTaggedOID).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: comparing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteReflecteeEq: x IfFail: fb = ( |
            | 
            noncachingReflectionPrimitives remoteReflecteeEq: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: comparing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteReflecteeIdentityHash = ( |
            | 
            noncachingReflectionPrimitives remoteReflecteeIdentityHash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: generating slots\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         removeGeneratedSlots = ( |
            | 
            generatedSlots do: [|:s| asMirror removeSlot: s name IfFail: []].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: different kinds of objects\x7fCategory: methods\x7fCategory: mapping bci\'s to source position\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         selfMethodFor: foreignMethodMir In: selfMethodMir = ( |
            | 
            noncachingReflectionPrimitives selfMethodFor: foreignMethodMir In: selfMethodMir).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: importing the map\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         setMapIfFail: fb = ( |
            | 
            noncachingReflectionPrimitives setMapIfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: accessing\x7fCategory: reflectee information\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         setReflecteeToImmediateOop: immOop = ( |
            | 
            noncachingReflectionPrimitives setReflecteeToImmediateOop: immOop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: creating mirrors\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot'
        
         thisKindOfMirrorFor: o IfFail: fb = ( |
            | 
            myVM mirrorFor: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: accessing\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         vmKit = ( |
            | 
            noncachingReflectionPrimitives vmKit).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> 'parent' -> () From: ( | {
         'Category: forwarded to noncachingReflectionPrimitives\x7fCategory: importing the map\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         withMapDo: blk IfFail: failBlk = ( |
            | 
            noncachingReflectionPrimitives withMapDo: blk IfFail: failBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'cachingReflectionPrimitives' -> () From: ( | {
         'Category: caching\x7fModuleInfo: Module: vmKitCachingMirror InitialContents: InitializeToExpression: (time origin)\x7fVisibility: private'
        
         timestampOfOldestCachedItem <- time origin.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot'
        
         vmKitCachingMirror = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCachingMirror' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCachingMirror' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitCachingMirror.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCachingMirror' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCachingMirror' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCachingMirror' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCachingMirror' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCachingMirror' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 1.7 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitCachingMirror' -> () From: ( | {
         'ModuleInfo: Module: vmKitCachingMirror InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 



 '-- Side effects'

 globals modules vmKitCachingMirror postFileIn
