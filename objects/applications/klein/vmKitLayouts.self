 '$Revision: 30.11 $'
 '
Copyright 1992-2006 Sun Microsystems, Inc. and Stanford University.
See the LICENSE file for license information.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfBytesPart: bpRef = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfMem: mem = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: byte vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesFrom: o WithLayout: layout IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesOfBytesPart: bpRef IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesPartRefForAddress: address = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o At: i IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o At: i Put: x IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o ByteAt: i IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o ByteAt: i Put: x IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IsMarkAt: i IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetMarkValue: markValue = ( |
            | 
            for: o SetMarkValue: markValue IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetMarkValue: markValue IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: byte vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o WithLayout: layout At: i PutBytesPartRef: bpRef IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: object vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o WithLayout: aLayout AtMost: n IndexablesDo: blk IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: byte vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o WithLayout: layout BytesPartRefAt: i IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: byte vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o WithLayout: layout SetBytes: bv IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef At: i IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef SetBytes: bv IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef SetIndexableSize: s IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef UncheckedAt: i Put: b IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: free list layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forEntryAtAddress: addr SetNextEntry: nextEntryShiftedAddr Layout: aLayout = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: free list layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forEntryAtAddress: addr SetSize: s Layout: aLayout = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeOfBytesPart: bpRef IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOf: o = ( |
            | mapOf: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOf: o IfFail: fb = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         markValueOf: o IfFail: fb = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         memForAddress: address = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         memForAddress: address OID: oid = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: free list layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         nextEntryForEntryAtAddress: addr Layout: aLayout = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: immediate layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForValue: v WithLayout: lo = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oopOfMem: mem = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: free list layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         sizeForEntryAtAddress: addr Layout: aLayout = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         tagOf: o = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'abstractLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: immediate layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         valueOf: oop WithLayout: lo = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> () From: ( | {
         'Category: object prototypes, formats & pieces\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         layouts = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         abstract = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts abstract.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> () From: ( | {
         'Category: layout constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         intNN = bootstrap stub -> 'globals' -> 'int32' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         layouts = ( |
            | 
            vmKit layouts).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot'
        
         lens = ( |
            | theVM lens).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot'
        
         machineMemory = ( |
            | theVM machineMemory).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         maps = ( |
            | 
            vmKit maps).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         name = ( |
            | 
            asMirror creatorSlotHint name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> () From: ( | {
         'Category: layout constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oopSize = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         untaggedAddressesLookLikeSMIs = ( |
            | 
                (vmKit tag size = 2)
            && [(vmKit tag smi  = 0)
            && [ oopSize = 4        ]]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         vmKit = ( |
            | 
            kleinAndYoda).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractUnsegregatedVector = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts abstractUnsegregatedVector.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         addressOfFirstIndexableIn: o IfFail: fb = ( |
            | 
            for: o UncheckedAddressOfIndexableAt: 0 IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: getting size of whole object\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         elementsPerWord = ( |
            | 
            oopSize / elementByteSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o AddressOfIndexableAt: i IfFail: fb = ( |
            | 
            for: o IfIndex: i IsOutOfBoundsThen: [|:e| ^ fb value: e].
            for: o UncheckedAddressOfIndexableAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o AtMost: n IndexablesDo: blk IfFail: fb = ( |
             size.
            | 
            size: indexableSizeOf: o IfFail: [|:e| ^ fb value: e].
            lens for: o WithLayout: self AtMost: (n min: size) IndexablesDo: blk IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i IfFail: fb = ( |
            | 
            for: o IfIndex: i IsOutOfBoundsThen: [|:e| ^ fb value: e].
            for: o UncheckedIndexableAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i Put: x IfFail: fb = ( |
            | 
            for: o IfIndex: i IsOutOfBoundsThen: [|:e| ^ fb value: e].
            for: o UncheckedIndexableAt: i Put: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexablesDo: blk IfFail: fb = ( |
             size.
            | 
            size: indexableSizeOf: o IfFail: [|:e| ^ fb value: e].
            lens for: o WithLayout: self AtMost: size IndexablesDo: blk IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o PopulateIndexablesBy: blk = ( |
             origin.
            | 
            "Optimization: don't keep doing the bounds check, or keep recalculating
             the indexable origin, every iteration. -- Adam, 2/05"
            origin:  indexableOriginOf: o.
            (indexableSizeOf: o) do: [|:i|
                                  for: o
                 UncheckedIndexableAt: i
                                  Put: (blk value: i)
                      IndexableOrigin: origin
                               IfFail: raiseError.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable origin field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetIndexableOrigin: index = ( |
            | for: o SetIndexableOrigin: index IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable origin field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetIndexableOrigin: index IfFail: fb = ( |
            | 
            indexableOriginField setValueFor: o To: index Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable size field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetIndexableSize: size = ( |
            | for: o SetIndexableSize: size IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable size field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetIndexableSize: size IfFail: fb = ( |
            | 
            indexableSizeField setValueFor: o To: size Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o UncheckedAddressOfIndexableAt: i IfFail: fb = ( |
            | 
                                      for: o
            UncheckedAddressOfIndexableAt: i
                          IndexableOrigin: (indexableOriginOf: o IfFail: [|:e| ^ fb value: e])
                                   IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o UncheckedIndexableAt: i IfFail: fb = ( |
            | 
                             for: o
            UncheckedIndexableAt: i
                 IndexableOrigin: (indexableOriginOf: o IfFail: [|:e| ^ fb value: e])
                          IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o UncheckedIndexableAt: i Put: x IfFail: fb = ( |
            | 
                             for: o
            UncheckedIndexableAt: i
                             Put: x
                 IndexableOrigin: (indexableOriginOf: o IfFail: [|:e| ^ fb value: e])
                          IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: iterating\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocal: o AtMost: n IndexablesDo: blk IfFail: fb = ( |
             origin.
            | 
            n = 0 ifTrue: [^ self].

            "Optimization: don't keep doing the bounds check, or keep recalculating
             the indexable origin, every iteration. -- Adam, 2/05"

            origin:  indexableOriginOf: o IfFail: [|:e| ^ fb value: e].

            0 upTo: n By: 1 WithoutCloningDo: [|:i|
              blk value: (                  for: o
                           UncheckedIndexableAt: i
                                IndexableOrigin: origin
                                         IfFail: raiseError)
                   With: i.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable size field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forObjectWithAddress: addr SetIndexableSize: s IfFail: fb = ( |
            | 
            indexableSizeField setValueForObjectWithAddress: addr To: s Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: iterating\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemote: o AtMost: n IndexablesDo: blk IfFail: fb = ( |
             firstAddr.
             i <- 0.
            | 
            n = 0 ifTrue: [^ self].

            "Optimization: don't keep doing the bounds check, or keep recalculating
             the indexable origin, every iteration. -- Adam, 2/05"

            firstAddr: for: o AddressOfIndexableAt: 0 IfFail: [|:e| ^ fb value: e].

            indexablesAtAddress: firstAddr Size: n Do: [|:x|
              blk value: x With: i.
              i: i succ.
            ] IfFail: [|:e| ^ fb value: e].

            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: vectReg AddressOfIndexableAt: indexSmiReg Into: dstAddressReg With: cg = ( |
            | 
            [indexSmiReg != dstAddressReg] assert.
            generateFor: vectReg UntaggedByteOffsetForIndex: indexSmiReg Into: dstAddressReg With: cg.
            cg withTemporaryRegisterDo: [|:vectAddressReg|
              generateAddressOf: vectReg Into: vectAddressReg With: cg.
              cg add: vectAddressReg MaybeSetCCFrom: dstAddressReg To: dstAddressReg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: vectReg AddressOfIndexableAtConstant: index Into: dstAddressReg With: cg = ( |
            | 
            cg withTemporaryRegisterDo: [|:vectAddressReg|
              generateAddressOf:         vectReg Into: vectAddressReg With: cg.
              generateIndexableOriginOf: vectReg Into:  dstAddressReg With: cg.
              cg add: vectAddressReg MaybeSetCCFrom: dstAddressReg To: dstAddressReg.
              index = 0 ifFalse: [
                cg addImm: index * elementByteSize MaybeSetCCFrom: dstAddressReg To: dstAddressReg.
              ].
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: vectReg IndexableAt: indexSmiReg Into: dstObjReg With: cg = ( |
            | 
            [vmKit tag smi  = 0] assert.
            [vmKit tag size = 2] assert.
            cg withTemporaryRegisterDo: [|:untaggedByteOffsetReg|
              generateFor: vectReg UntaggedByteOffsetForIndex: indexSmiReg Into: untaggedByteOffsetReg With: cg.
              generateFor: vectReg IndexableAtUntaggedByteOffset: untaggedByteOffsetReg Into: dstObjReg With: cg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         generateFor: vectReg IndexableAt: indexSmiReg Put: valueReg Temp: tempReg With: cg = ( |
            | 
            [vmKit tag smi  = 0] assert.
            [vmKit tag size = 2] assert.
            generateFor: vectReg UntaggedByteOffsetForIndex: indexSmiReg Into: tempReg With: cg.
            cg withTemporaryRegisterDo: [|:temp2Reg|
              generateFor: vectReg IndexableAtUntaggedByteOffset: tempReg Put: valueReg Temp: temp2Reg With: cg.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexables\x7fCategory: code generation\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         generateFor: vectReg UntaggedByteOffsetForIndex: indexSmiReg Into: dstUntaggedByteOffsetReg With: cg = ( |
            | 
            [(dstUntaggedByteOffsetReg != indexSmiReg) && [dstUntaggedByteOffsetReg != vectReg]] assert.
            generateIndexableOriginOf: vectReg Into: dstUntaggedByteOffsetReg With: cg.
            generateAddUntaggedByteOffsetForIndex: indexSmiReg To: dstUntaggedByteOffsetReg With: cg.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable origin field\x7fCategory: code generation\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIndexableOriginOf: objVectReg Into: dstOriginSmiReg With: cg = ( |
            | 
            indexableOriginField generateValueFor: objVectReg Into: dstOriginSmiReg With: cg Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable size field\x7fCategory: code generation\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         generateIndexableSizeOf: objVectReg Into: dstSizeSmiReg With: cg = ( |
            | 
            indexableSizeField generateValueFor: objVectReg Into: dstSizeSmiReg With: cg Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable origin field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableOriginField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableOriginField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts abstractUnsegregatedVector indexableOriginField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractHeaderField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts memoryObject abstractHeaderField.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableOriginField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing map\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         mapField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts memoryObject mapField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableOriginField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         smiValueHeaderFieldMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'smiValueHeaderFieldMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts memoryObject smiValueHeaderFieldMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableOriginField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'smiValueHeaderFieldMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable origin field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableOriginOf: o = ( |
            | indexableOriginOf: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable origin field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableOriginOf: o IfFail: fb = ( |
            | 
            indexableOriginField valueFor: o Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable origin field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableOriginOfObjectWithAddress: addr = ( |
            | 
            indexableOriginField valueForObjectWithAddress: addr Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable size field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableSizeField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts abstractUnsegregatedVector indexableSizeField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableSizeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableSizeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableOriginField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableSizeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'smiValueHeaderFieldMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable size field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeOf: o IfFail: fb = ( |
            | 
            indexableSizeField valueFor: o Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: accessing indexable size field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeOfObjectWithAddress: addr = ( |
            | 
            indexableSizeField valueForObjectWithAddress: addr Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: layout constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         lastField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> 'indexableSizeField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractVector = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractVector' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts abstractVector.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: getting size of whole object\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         wordSizeOf: o IfFail: fb = ( |
            | 
              (            indexableOriginOf: o IfFail: [|:e| ^ fb value: e])
            + (wordsNeededToHoldIndexablesOf: o IfFail: [|:e| ^ fb value: e])).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: getting size of whole object\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         wordSizeOfObjectWithAddress: addr = ( |
             io.
             s.
            | 
            "Using primitives to avoid cloning."
            io:  indexableOriginOfObjectWithAddress: addr.
             s:  indexableSizeOfObjectWithAddress:   addr.
            io _IntAdd: wordsNeededToHoldIndexablesOfSize: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> () From: ( | {
         'Category: getting size of whole object\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         wordsNeededToHoldIndexablesOf: o IfFail: fb = ( |
            | 
            (indexableSizeOf: o IfFail: [|:e| ^ fb value: e]) /+ elementsPerWord).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o AddressOfIndexableAt: i = ( |
            | for: o AddressOfIndexableAt: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractVector' -> () From: ( | {
         'Category: accessing indexable size field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IfIndex: i IsOutOfBoundsThen: blk = ( |
             size.
            | 
            size: indexableSizeOf: o IfFail: [|:e| ^ blk value: e].
            (i >= 0) && [i < size] ifFalse: [^ blk value: 'out of bounds'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i = ( |
            | for: o IndexableAt: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i Put: x = ( |
            | for: o IndexableAt: i Put: x IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractVector' -> () From: ( | {
         'Category: accessing indexable size field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeOf: o = ( |
            | 
            indexableSizeOf: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: memory objects\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         memoryObject = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts memoryObject.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractVector' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: object vectors\x7fCategory: activations\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         activation = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts activation.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         first_stack_offset = ( |
            | 
            lastIndexableField fixedIndexAfterMe).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( | {
         'Category: iterating\x7fComment: Invokes the block for each word in the object starting
with the word at ``firstIndex\'\', and ending when the
end of the expression stack is reached.  -- Adam, 7/06\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o StartingAt: firstIndex Do: blk IfFail: fb = ( |
             end.
            | 
            end: offsetOfEndOfLiveOopsOf: o IfFail: [|:e| ^ fb value: e].

            "Iterate until we reach the end of the live oops."

                   for: o
            StartingAt: firstIndex
                 Until: [|:i| i = end]
                    Do: blk
                IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( | {
         'Category: fields in indexable part\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         pc_after_endInit_field = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'pc_after_endInit_field' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts activation pc_after_endInit_field.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( | {
         'Category: fields in indexable part\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         lastIndexableField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'pc_after_endInit_field' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( | {
         'Category: fields in indexable part\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         methodHolder_field = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'methodHolder_field' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts activation methodHolder_field.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         abstractIndexableField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> 'abstractIndexableField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts objVector abstractIndexableField.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'methodHolder_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> 'abstractIndexableField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( | {
         'Category: fields in indexable part\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         sender_field = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'sender_field' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts activation sender_field.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'methodHolder_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'sender_field' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         oopValueHeaderFieldMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'oopValueHeaderFieldMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts memoryObject oopValueHeaderFieldMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'methodHolder_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'oopValueHeaderFieldMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         offsetOfEndOfLiveOopsOf: o IfFail: fb = ( |
            | 
            (indexableOriginOf: o IfFail: [|:e| ^ fb value: e]) + (sp_of: o)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         objVector = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts objVector.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'pc_after_endInit_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> 'abstractIndexableField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'pc_after_endInit_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'methodHolder_field' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'pc_after_endInit_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'smiValueHeaderFieldMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( | {
         'Category: fields in indexable part\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         pc_field = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'pc_field' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts activation pc_field.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'pc_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> 'abstractIndexableField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( | {
         'Category: fields in indexable part\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         sp_field = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'sp_field' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts activation sp_field.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'pc_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'sp_field' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'pc_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'smiValueHeaderFieldMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( | {
         'Category: fields in indexable part\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         rcvr_field = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'rcvr_field' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts activation rcvr_field.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'rcvr_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> 'abstractIndexableField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> () From: ( | {
         'Category: fields in indexable part\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         self_field = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'self_field' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts activation self_field.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'rcvr_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'self_field' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'rcvr_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'oopValueHeaderFieldMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'self_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> 'abstractIndexableField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'self_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'pc_field' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'self_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'oopValueHeaderFieldMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'sender_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> 'abstractIndexableField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'sender_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'rcvr_field' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'sender_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'oopValueHeaderFieldMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'sp_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> 'abstractIndexableField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: sentinel for the chain of fields\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         noPrecedingField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> 'noPrecedingField' -> () From: ( |
             {} = 'Comment: I kinda like ending the chain of fields with this sentinel object,
rather than doing isNil checks all the time. (Saves us the trouble
of creating the ifNil: blocks, too, which might be handy until we
teach Klein and Yoda how to optimize them away.) -- Adam, 4/06\x7fModuleInfo: Creator: globals kleinAndYoda layouts memoryObject abstractHeaderField noPrecedingField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'sp_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> 'noPrecedingField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'activation' -> 'sp_field' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'smiValueHeaderFieldMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: memory objects\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         block = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts block.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( | {
         'Category: accessing home frame pointer field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         homeFramePointerField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> 'homeFramePointerField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts block homeFramePointerField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> 'homeFramePointerField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> 'homeFramePointerField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> 'homeFramePointerField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'oopValueHeaderFieldMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( | {
         'Category: accessing home frame pointer field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         homeFramePointerOf: o IfFail: fb = ( |
            | 
            homeFramePointerField valueFor: o Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( | {
         'Category: layout constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         lastField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> 'homeFramePointerField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfWordsInABlock = ( |
            | 
            "We're maintaining the invariant that all blocks are the same size."
            "Use the primitive so as not to clone any blocks. -- Adam, 7/06"
                     lastField fixedIndexAfterMe
            _IntAdd: theVM exportPolicy shouldBlockValueSlotsBeObjectSlots asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'block' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         byteVector = ( |
            | 
            unsegregatedByteVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: bytes part\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesPart = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts bytesPart.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: converting reference to address\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfBytesPart: bpRef = ( |
            | 
            lens addressOfBytesPart: bpRef).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: addressing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfFirstByteInBytesPart: bpRef = ( |
            | 
            (addressOfBytesPart: bpRef) + firstByteOffset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable size field\x7fCategory: addressing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         addressOfIndexableSizeFieldInBytesPart: bpRef = ( |
            | (addressOfBytesPart: bpRef) + indexableSizeFieldOffset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: converting reference to address\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfLocalBytesPart: bpRef = ( |
            | 
            _NoGCAllowed.
            bpRef asInt32 <+ vmKit tag size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: converting reference to address\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfRemoteBytesPart: bpRef = ( |
            | 
            [todo bytesPartRefFormat].
            bpRef).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         allocateBytesPartWithIndexableSize: nBytes = ( |
             bpRef.
            | 
            bpRef:  bytesPartRefForAddress:  theVM universe allocateBytes: nBytes + bytesHeaderSize.
            forBytesPart: bpRef SetIndexableSize: nBytes.
            bpRef).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: layout constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         byteOffsetFromIndexableSizeToFirstByte = ( |
            | 
            firstByteOffset - (indexableSizeFieldOffset * oopSize)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: layout constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         bytesHeaderSize = ( |
            | 
            lastBytesHeaderWordOffset succ * oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesOfBytesPart: bpRef = ( |
            | 
            bytesOfBytesPart: bpRef IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesOfBytesPart: bpRef IfFail: fb = ( |
            | 
            lens bytesOfBytesPart: bpRef IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesOfLocalBytesPart: bpRef IfFail: fb = ( |
             bv.
            | 
            bv: byteVector copySize: indexableSizeOfLocalBytesPart: bpRef IfFail: [|:e| ^ fb value: e].
            forBytesPart: bpRef
                      Do: [|:x. :i| bv at: i Put: x]
                  IfFail: [|:e| ^ fb value: e].
            bv).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesOfRemoteBytesPart: bpRef IfFail: fb = ( |
            | 
            machineMemory bytesAt: (addressOfFirstByteInBytesPart: bpRef)
                             Size: (indexableSizeOfBytesPart: bpRef IfFail: [|:e| ^ fb value: e])
                           IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: converting address to reference\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesPartRefForAddress: address = ( |
            | 
            lens bytesPartRefForAddress: address).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: creating\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBytesFrom: srcBPRef WhichHasSize: srcSize To: dstBPRef WhichHasSize: dstSize FillingWith: filler IfFail: fb = ( |
             failBlock.
            | 
            "Optimization: don't keep cloning the failblock over and over. -- Adam, 6/06"
            failBlock: [|:e| ^ fb value: e].

            [|:exit|
              forBytesPart: srcBPRef Do: [|:x. :i|
                i >= dstSize ifTrue: exit.
                forBytesPart: dstBPRef UncheckedAt: i Put: x IfFail: failBlock.
              ] IfFail: failBlock.

              srcSize upTo: dstSize Do: [|:i|
                forBytesPart: dstBPRef UncheckedAt: i Put: filler IfFail: failBlock.
              ].
            ] exit.
            dstBPRef).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: layout constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         firstByteOffset = ( |
            | bytesHeaderSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: getting address of indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef AddressAt: i = ( |
            | forBytesPart: bpRef AddressAt: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: getting address of indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef AddressAt: i IfFail: fb = ( |
            | 
            forBytesPart: bpRef IfIndex: i IsOutOfBoundsThen: [|:e| ^ fb value: e].
            forBytesPart: bpRef UncheckedAddressAt: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef At: i = ( |
            | forBytesPart: bpRef At: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef At: i IfFail: fb = ( |
            | 
            lens forBytesPart: bpRef
                           At: i asSmallInteger
                       IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef At: i Put: b = ( |
            | forBytesPart: bpRef At: i Put: b IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef At: i Put: b IfFail: fb = ( |
            | 
            forBytesPart: bpRef IfIndex: i IsOutOfBoundsThen: [|:e| ^ fb value: e].
            forBytesPart: bpRef
             UncheckedAt: i
                     Put: b
                  IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         forBytesPart: bpRef Do: blk = ( |
            | forBytesPart: bpRef Do: blk IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         forBytesPart: bpRef Do: blk IfFail: fb = ( |
            | 
            (indexableSizeOfBytesPart: bpRef IfFail: [|:e| ^ fb value: e]) do: [|:i|
              blk value: (forBytesPart: bpRef At: i IfFail: [|:e| ^ fb value: e]) With: i
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: bounds-check\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         forBytesPart: bpRef IfIndex: i IsOutOfBoundsThen: blk = ( |
             size.
            | 
            size: indexableSizeOfBytesPart: bpRef IfFail: [|:e| ^ blk value: e].
            (i >= 0) && [i < size] ifFalse: [^ blk value: 'out of bounds'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef SetBytes: bv = ( |
            | forBytesPart: bpRef SetBytes: bv IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef SetBytes: bv IfFail: fb = ( |
            | 
            lens forBytesPart: bpRef SetBytes: bv IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable size field\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         forBytesPart: bpRef SetIndexableSize: s = ( |
            | forBytesPart: bpRef SetIndexableSize: s IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable size field\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         forBytesPart: bpRef SetIndexableSize: s IfFail: fb = ( |
            | 
            lens forBytesPart: bpRef SetIndexableSize: s IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: getting address of indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         forBytesPart: bpRef UncheckedAddressAt: i = ( |
            | 
            i + addressOfFirstByteInBytesPart: bpRef).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef UncheckedAt: i Put: b IfFail: fb = ( |
            | 
            lens forBytesPart: bpRef
                  UncheckedAt: i asSmallInteger
                          Put: b asSmallInteger
                       IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalBytesPart: bpRef At: i IfFail: fb = ( |
            | 
            _NoGCAllowed.
            forBytesPart: bpRef IfIndex: i IsOutOfBoundsThen: [|:e| ^ fb value: e].
            _UnsafeForBytesPart: bpRef At: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: writing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalBytesPart: bpRef SetBytes: bv IfFail: fb = ( |
             failBlock.
             size.
            | 
            "Optimization: don't keep cloning the failblock over and over. -- Adam, 6/06"
            failBlock: [|:e| ^ fb value: e].

            size: indexableSizeOfBytesPart: bpRef IfFail: failBlock.
            bv size = size  ifFalse: [^ fb value: 'size mismatch'].

            bv do: [|:x. :i|
              forBytesPart: bpRef UncheckedAt: i Put: x IfFail: failBlock.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable size field\x7fCategory: writing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalBytesPart: bpRef SetIndexableSize: s IfFail: fb = ( |
            | 
            _NoGCAllowed.
            _UnsafeForBytesPart: bpRef SetIndexableSize: s IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: writing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalBytesPart: bpRef UncheckedAt: i Put: b IfFail: fb = ( |
            | 
            _NoGCAllowed.
            _UnsafeForBytesPart: bpRef At: i Put: b IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteBytesPart: bpRef At: i IfFail: fb = ( |
            | 
            machineMemory byteAt: (forBytesPart: bpRef AddressAt: i IfFail: [|:e| ^ fb value: e])
                          IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: writing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteBytesPart: bpRef SetBytes: bv IfFail: fb = ( |
             s.
            | 
            s: indexableSizeOfBytesPart: bpRef IfFail: [|:e| ^ fb value: e].
            bv size = s  ifFalse: [^ fb value: 'size mismatch'].

            machineMemory at: (addressOfFirstByteInBytesPart: bpRef)
                    PutBytes: bv
                      IfFail: fb.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable size field\x7fCategory: writing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteBytesPart: bpRef SetIndexableSize: s IfFail: fb = ( |
            | 
            machineMemory at: (addressOfIndexableSizeFieldInBytesPart: bpRef)
                      PutOop: (layouts smi oopForValue: s)
                      IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable contents\x7fCategory: writing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteBytesPart: bpRef UncheckedAt: i Put: b IfFail: fb = ( |
            | 
            machineMemory at: (forBytesPart: bpRef UncheckedAddressAt: i)
                     PutByte: b
                      IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: layout constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         indexableSizeFieldOffset = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable size field\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeOfBytesPart: bpRef = ( |
            | indexableSizeOfBytesPart: bpRef IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable size field\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeOfBytesPart: bpRef IfFail: fb = ( |
            | 
            lens indexableSizeOfBytesPart: bpRef IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable size field\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeOfLocalBytesPart: bpRef IfFail: fb = ( |
            | 
            _NoGCAllowed.
            _UnsafeIndexableSizeOfBytesPart: bpRef IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: accessing indexable size field\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeOfRemoteBytesPart: bpRef IfFail: fb = ( |
             a.
            | 
            a: addressOfIndexableSizeFieldInBytesPart: bpRef.
            layouts smi valueOf:
              machineMemory wordAt: a IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: layout constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         lastBytesHeaderWordOffset = ( |
            | indexableSizeFieldOffset).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: converting address to reference\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         localBytesPartRefForAddress: address = ( |
            | 
            _NoGCAllowed.
            address +> vmKit tag size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: getting address of next bytes part\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         nextBytesPartAfter: bpRef = ( |
             s.
            | 
            s: indexableSizeOfBytesPart: bpRef.
            s: s roundUpTo: oopSize.
            bytesPartRefForAddress: forBytesPart: bpRef UncheckedAddressAt: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'bytesPart' -> () From: ( | {
         'Category: converting address to reference\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteBytesPartRefForAddress: address = ( |
            | 
            [todo bytesPartRefFormat].
            address).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: immediates\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         float = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'float' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts float.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'float' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         decode: i = ( |
            | 
            [todo floats].
            (resend.decode: i) asFloat).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'float' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         encode: f = ( |
            | 
            [todo floats].
            "Just convert the float to a smi for now, because
             we don't want to take the time to implement floats
             yet.
             Treat infinity specially, because it can't be
             converted to an integer. -- Adam, 6/05"
            resend.encode: f = infinity ifTrue: [maxSmallInt]
                                         False: [f floor asInteger]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'float' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         exemplar = 0.0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'float' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         mapPrototype = ( |
            | 
            vmKit maps floatMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'float' -> () From: ( | {
         'Category: oop constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         myTag = ( |
            | 
            vmKit tag float).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: immediates\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         immediate = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts immediate.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'float' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: freelist entry\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         freeOopsListEntry = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts freeOopsListEntry.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: next entry field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forEntryAtAddress: addr SetNextEntry: nextEntryShiftedAddr = ( |
            | 
            lens forEntryAtAddress: addr SetNextEntry: nextEntryShiftedAddr Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: size field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forEntryAtAddress: addr SetSize: s = ( |
            | 
            lens forEntryAtAddress: addr SetSize: s Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: next entry field\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalEntryAtAddress: addr SetNextEntry: nextEntryShiftedAddr = ( |
            | 
            "Encode it as a mark so as to serve as the end-of-object sentinel."
            (layouts mark encode: nextEntryShiftedAddr) _UnsafePutWordAtAddress: addr + (nextEntryIndex * oopSize)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: size field\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalEntryAtAddress: addr SetSize: s = ( |
            | 
            _NoGCAllowed.
            s _UnsafePutOopAtAddress: addr + (sizeIndex * oopSize).
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: next entry field\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteEntryAtAddress: addr SetNextEntry: nextEntryShiftedAddr = ( |
            | 
            "Encode it as a mark so as to serve as the end-of-object sentinel."
            machineMemory at: (addr + (nextEntryIndex * oopSize))
                      PutOop: layouts mark encode: nextEntryShiftedAddr.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: size field\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteEntryAtAddress: addr SetSize: s = ( |
            | 
            machineMemory at: (addr + (sizeIndex * oopSize))
                      PutOop: layouts smi encode: s.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isAnEntryOfSize: s AcceptableForHoldingAnObjectOfSize: nOops = ( |
            | 
            (s = nOops) || [(s - nOops) >= numberOfFields]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: next entry field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         nextEntryForEntryAtAddress: addr = ( |
            | 
            lens nextEntryForEntryAtAddress: addr Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: next entry field\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         nextEntryForLocalEntryAtAddress: addr = ( |
            | 
            "It's encoded as a mark so as to serve as the end-of-object sentinel."
            layouts mark decode:  intNN copy _UnsafeWordAtAddress: addr + (nextEntryIndex * oopSize)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: next entry field\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         nextEntryForRemoteEntryAtAddress: addr = ( |
            | 
            "It's encoded as a mark so as to serve as the end-of-object sentinel."
            layouts mark decode: machineMemory oopAt: addr + (nextEntryIndex * oopSize)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: hard-coded indices\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         nextEntryIndex = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: hard-coded indices\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         numberOfFields = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: size field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         sizeForEntryAtAddress: addr = ( |
            | 
            lens sizeForEntryAtAddress: addr Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: size field\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         sizeForLocalEntryAtAddress: addr = ( |
            | 
            _NoGCAllowed.
            _UnsafeObjectForOopAtAddress: addr + (sizeIndex * oopSize)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: size field\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         sizeForRemoteEntryAtAddress: addr = ( |
            | 
            layouts smi decode: machineMemory oopAt: addr + (sizeIndex * oopSize)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'freeOopsListEntry' -> () From: ( | {
         'Category: hard-coded indices\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         sizeIndex = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         decode: i = ( |
            | 
            intNN shr: i With: vmKit tag size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         encode: i = ( |
            | 
            (intNN shl: i With: vmKit tag size) || myTag).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isImmediate = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'Category: converting value to reference\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         localOopForValue: v = ( |
            | 
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'Category: converting value to reference\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForValue: v = ( |
            | 
            lens oopForValue: v WithLayout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         object = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts object.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'Category: converting value to reference\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteOopForValue: v = ( |
            | 
            encode: v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'Category: converting reference to value\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         valueOf: oop = ( |
            | 
            lens valueOf: oop WithLayout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'Category: converting reference to value\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         valueOfLocal: oop = ( |
            | 
            oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> () From: ( | {
         'Category: converting reference to value\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         valueOfRemote: oop = ( |
            | 
            decode: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: object vectors\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         map = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'map' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts map.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'map' -> () From: ( | {
         'Category: accessing map type field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         generateMapTypeOf: mapReg Into: dstReg With: cg = ( |
            | 
            generateFor:           mapReg
              IndexableAtConstant: mapTypeIndex
                             Into: dstReg
                             With: cg).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'map' -> () From: ( | {
         'Category: indexable layout constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         mapTypeIndex = ( |
            | 
            maps mapMap mapTypeIndex).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'map' -> () From: ( | {
         'Category: indexable layout constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         nmethodCacheIndex = ( |
            | 
            maps mapMap nmethodCacheIndex).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'map' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: immediates\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         mark = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( |
             {} = 'Comment: Send formatString to me to get a printout of my format.\x7fModuleInfo: Creator: globals kleinAndYoda layouts mark.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fCategory: fields that are not being used right now\x7fComment: See readMe.\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         compactMapIndexField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'compactMapIndexField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts mark compactMapIndexField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'compactMapIndexField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'compactMapIndexField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = ( |
            | 
            error: 'should not use this bit field until we\'re ready to turn',
                   'the encoding-stuff-in-the-mark space optimization back on'.

            used to be a constant slot pointing to
            vmKit layouts mark isByteVectorField).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'compactMapIndexField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         width = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         defaultMarkValue = ( |
             mv <- 0.
            | 
            fieldsDo: [|:f| mv: mv || (f wordForValue: f defaultValue)].
            mv).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         hasBeenVisitedForGCField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hasBeenVisitedForGCField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts mark hasBeenVisitedForGCField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hasBeenVisitedForGCField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         booleanValueForObject: aMirror = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hasBeenVisitedForGCField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         hasBeenVisitedForLookupField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hasBeenVisitedForLookupField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts mark hasBeenVisitedForLookupField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hasBeenVisitedForGCField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hasBeenVisitedForLookupField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hasBeenVisitedForLookupField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         booleanValueForObject: aMirror = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hasBeenVisitedForLookupField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         isActivationField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isActivationField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts mark isActivationField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hasBeenVisitedForLookupField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isActivationField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         hashField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hashField' -> () From: ( |
             {} = 'Comment: Not used right now; we use oidField instead.\x7fModuleInfo: Creator: globals kleinAndYoda layouts mark hashField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hashField' -> () From: ( | {
         'Category: hash\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         firstValue = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hashField' -> () From: ( | {
         'Category: hash\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         hashOfWord: w IfNone: blk = ( |
             h.
            | 
            h: valueOfWord: w.
            h = noValueYet ifTrue: blk False: h).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hashField' -> () From: ( | {
         'Category: hash\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         newWordFor: w WithHash: h = ( |
             newValue.
            | 
            newValue: h = noValueYet ifTrue: [firstValue] False: [h].
            setValueOfWord: w To: newValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hashField' -> () From: ( | {
         'Category: hash\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         noValueYet = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hashField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         isOnMarkStackForGCField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isOnMarkStackForGCField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts mark isOnMarkStackForGCField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hashField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isOnMarkStackForGCField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hashField' -> () From: ( | {
         'Category: hash\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         valueOfWord: w IfNoneCreateANewOneAndDo: blk = ( |
            | 
            "Don't use this code anymore; we encode the actual OID in the mark instead.
             This may need to change someday if we end up having more objects than we
             can count in the bits available for the oidField. -- Adam, 5/06"

            hashOfWord: w IfNone: [| h |
              h: theVM nextIdentityHash.
              blk value: (newWordFor: w WithHash: h)
                   With: h
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hashField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         width = ( |
            | 
            (vmKit layouts abstract oopSize * 8) - vmKit tag size - shift).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: hash\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         hashOfMarkValue: mv IfNoneCreateANewOneAndDo: blk = ( |
            | 
            "Don't use hashField anymore; we encode the actual OID in the mark instead.
             This may need to change someday if we end up having more objects than we
             can count in the bits available for the oidField. -- Adam, 5/06"

            [hashField valueOfWord: mv IfNoneCreateANewOneAndDo: blk].

            oidOfMarkValue: mv).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isActivationField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         booleanValueForObject: aMirror = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isActivationField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         isActivationMapField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isActivationMapField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts mark isActivationMapField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isActivationField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isActivationMapField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isActivationMapField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         booleanValueForObject: aMirror = ( |
            | aMirror isReflecteeVMKitActivationMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isActivationMapField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         isByteVectorField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isByteVectorField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts mark isByteVectorField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isActivationMapField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isByteVectorField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isByteVectorField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         booleanValueForObject: aMirror = ( |
            | 
            aMirror isReflecteeByteVector).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isByteVectorField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isByteVectorField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> 'noPrecedingField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         isInRememberedSetForGCField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isInRememberedSetForGCField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts mark isInRememberedSetForGCField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isInRememberedSetForGCField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         booleanValueForObject: aMirror = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isInRememberedSetForGCField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isInRememberedSetForGCField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isOnMarkStackForGCField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: isActivationMap\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isMarkForActivationMap: oop = ( |
            | 
            isMarkValueForActivationMap: decode: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: isByteVector\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isMarkForByteVector: oop = ( |
            | 
            isMarkValueForByteVector: decode: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: isActivationMap\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isMarkValueForActivationMap: mv = ( |
            | 
            isActivationMapField isValueOfWordTrue: mv).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: isByteVector\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isMarkValueForByteVector: mv = ( |
            | 
            isByteVectorField isValueOfWordTrue: mv).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isOnMarkStackForGCField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         booleanValueForObject: aMirror = ( |
            | 
            false).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isOnMarkStackForGCField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractBooleanField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isOnMarkStackForGCField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'hasBeenVisitedForGCField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         oidField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'oidField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts mark oidField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         lastField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'oidField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: converting value to reference\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         localOopForValue: v = ( |
            | error: 'Marks cannot be directly manipulated inside the running VM').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         myTag = ( |
            | 
            vmKit tag mark).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fCategory: fields that are not being used right now\x7fComment: See readMe.\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         objVectorIndexableOriginField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'objVectorIndexableOriginField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts mark objVectorIndexableOriginField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'objVectorIndexableOriginField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         lowestEncodableNumber = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'objVectorIndexableOriginField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'objVectorIndexableOriginField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = ( |
            | 
            error: 'should not use this bit field until we\'re ready to turn',
                   'the encoding-stuff-in-the-mark space optimization back on'.

            used to be a constant slot pointing to
            vmKit layouts mark compactMapIndexField).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'objVectorIndexableOriginField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         width = 2.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fCategory: fields that are not being used right now\x7fComment: See readMe.\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         objVectorIndexableSizeField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'objVectorIndexableSizeField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts mark objVectorIndexableSizeField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'objVectorIndexableSizeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractNumberField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'objVectorIndexableSizeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = ( |
            | 
            error: 'should not use this bit field until we\'re ready to turn',
                   'the encoding-stuff-in-the-mark space optimization back on'.

            used to be a constant slot pointing to
            vmKit layouts mark objVectorIndexableOriginField).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'objVectorIndexableSizeField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         width = 4.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'oidField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> 'abstractField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'oidField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'isInRememberedSetForGCField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> 'oidField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         width = ( |
            | 
            ((vmKit layouts abstract oopSize _IntMul: 8) _IntSub: vmKit tag size) _IntSub: shift).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: object ID\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oidOfMarkAtLocalAddress: addr = ( |
            | 
            addr _UnsafeOIDOfMarkAtAddress).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: object ID\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oidOfMarkValue: mv = ( |
            | 
            oidField valueOfWord: mv).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: fields\x7fCategory: fields that are not being used right now\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         readMe = ( |
            | 
            [todo encodingStuffInTheMark spaceOptimizations].
            "We used to have a space optimization where we encoded
             some kinds of information about an object into its
             mark, like its map, or (if it's an object vector) its
             indexable origin and indexable size. That optimization
             is turned off right now - see " [canWeEncodeStuffInTheMark].
            "If you ever want to turn this optimization back on, you'll
             need to include these field objects somewhere in the chain
             of fields (and hopefully refactor the chain of fields so that
             different VM objects can specify different chains of fields.
             -- Adam and Alex, 12/05"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         trailingMark = ( |
            | 
            oopForValue: trailingMarkValue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         trailingMarkValue = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'Category: converting reference to value\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         valueOfLocal: oop = ( |
            | 
            error: 'Marks cannot be directly manipulated inside the running VM\n',
                   'Use _MarkValueOfMemoryObject: instead.').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'mark' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         wordLayoutMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'wordLayoutMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         baseName = ( |
            | 
            name copyWithoutSuffix: 'Field').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         basicValueFor: o At: i Layout: aLayout IfFail: fb = ( |
            | 
            valueOf:
              aLayout
                   for: o
                    At: i
                IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: indices\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         fixedIndex = ( |
            | 
            "Assumes we are not using variable-size headers."
            precedingField fixedIndexAfterMe).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: indices\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         fixedIndexAfterMe = ( |
            | 
            "Assumes we are not using variable-size headers."

            "Use _IntAdd: 1 instead of succ so as not to clone
             any blocks, so that this method can be called from
             the local cloning code. -- Adam, 4/06"
            fixedIndex _IntAdd: 1).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forwardDo: blk = ( |
            | 
            precedingField forwardDo: blk.
            blk value: self.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: indices\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexAfterMeFor: o Layout: aLayout = ( |
            | 
            (indexFor: o Layout: aLayout) succ).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: indices\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         indexFor: o Layout: aLayout = ( |
            | 
            precedingField indexAfterMeFor: o Layout: aLayout).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isFirstField = ( |
            | 
            precedingField isNoField).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         isNoField = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         name = ( |
            | 
            asMirror creatorSlotHint name).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> 'noPrecedingField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         fixedIndexAfterMe = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> 'noPrecedingField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forwardDo: blk = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> 'noPrecedingField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexAfterMeFor: o Layout: aLayout = ( |
            | 
            0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> 'noPrecedingField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isNoField = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> 'noPrecedingField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: indices\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         offsetFor: o Layout: aLayout = ( |
            | 
            "Use _IntAdd: instead of + so as not to clone
             any blocks, so that this method can be called from
             the local cloning code. -- Adam, 4/06"
            (offsetOfIndexZeroFor: o Layout: aLayout)  _IntAdd:  indexFor: o Layout: aLayout).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: indices\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         offsetOfIndexZeroFor: o Layout: aLayout = ( |
            | 
            0).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         reverseDo: blk = ( |
             f.
            | 
            f: self.
            [blk value: f.
             f isFirstField] whileFalse: [f: f precedingField].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         setBasicValueFor: o At: i To: v Layout: aLayout IfFail: fb = ( |
            | 
            aLayout
                 for: o 
                  At: i
                 Put: (oopForValue: v)
              IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         setValueFor: o To: v Layout: aLayout IfFail: fb = ( |
            | 
            setBasicValueFor: o At: (offsetFor: o Layout: aLayout) To: v Layout: aLayout IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         setValueForObjectWithAddress: addr To: v Layout: aLayout IfFail: fb = ( |
            | 
            aLayout machineMemory at: addr + (fixedIndex * aLayout oopSize)
                              PutOop: (oopForValue: v)
                              IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         valueFor: o Layout: aLayout IfFail: fb = ( |
            | 
            basicValueFor: o At: (offsetFor: o Layout: aLayout) Layout: aLayout IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForObjectWithAddress: addr Layout: aLayout = ( |
            | 
            "Need a version that doesn't take a failblock and doesn't
             clone anything. -- Adam, Mar. 2009"
            valueOf:
              aLayout machineMemory oopAtOffset: fixedIndex
                                           From: addr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> () From: ( | {
         'Category: accessing value\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         valueForObjectWithAddress: addr Layout: aLayout IfFail: fb = ( |
            | 
            valueOf:
              aLayout machineMemory oopAtOffset: fixedIndex
                                           From: addr
                                         IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: converting reference to address\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfLocalMem: mem = ( |
            | 
            theVM objectLocator addressOfLocalMem: mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: converting reference to address\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfMem: mem = ( |
            | lens addressOfMem: mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: converting reference to address\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfRemoteMem: mem = ( |
            | 
            theVM objectLocator addressOfRemoteMem: mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         decode: i = ( |
            | 
            theVM objectLocator
              decodeAddressOrOIDFromMemOop: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: header\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         emptyObjectSizeFor: o = ( |
            | 
            lastField indexAfterMeFor: o Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         encode: i = ( |
            | 
            theVM objectLocator
              encodeMemOopFromAddressOrOID: i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: header\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         fieldsDo: blk = ( |
            | 
            lastField forwardDo: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: header\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         fieldsReverseDo: blk = ( |
            | 
            lastField reverseDo: blk.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: addressing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o AddressAt: i = ( |
            | for: o AddressAt: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: addressing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o AddressAt: i IfFail: fb = ( |
            | 
            for: o AddressOfByteAt: i * oopSize IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing bytes\x7fCategory: addressing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o AddressOfByteAt: i = ( |
            | 
            for: o AddressOfByteAt: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing bytes\x7fCategory: addressing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o AddressOfByteAt: i IfFail: fb = ( |
            | 
            (addressOfMem: o) + i).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o At: i = ( |
            | 
            for: o At: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o At: i IfFail: fb = ( |
            | 
            lens for: o At: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o At: i Put: x = ( |
            | for: o At: i Put: x IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o At: i Put: x IfFail: fb = ( |
            | 
            lens for: o At: i Put: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing bytes\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o ByteAt: i = ( |
            | 
            for: o ByteAt: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing bytes\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o ByteAt: i IfFail: fb = ( |
            | 
            lens for: o ByteAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing bytes\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o ByteAt: i Put: x = ( |
            | 
            for: o ByteAt: i Put: x IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing bytes\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o ByteAt: i Put: x IfFail: fb = ( |
            | 
            lens for: o ByteAt: i Put: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o Do: blk IfFail: fb = ( |
            | 
                   for: o
            StartingAt: (markField indexAfterMeFor: o Layout: self) "skip mark"
                    Do: blk
                IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IsMarkAt: i = ( |
            | for: o IsMarkAt: i IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IsMarkAt: i IfFail: fb = ( |
            | 
            lens for: o IsMarkAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing map\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetMap: m = ( |
            | for: o SetMap: m IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing map\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetMap: m IfFail: fb = ( |
            | 
            mapField setValueFor: o To: m Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetMarkValue: markValue = ( |
            | for: o SetMarkValue: markValue IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: writing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetMarkValue: markValue IfFail: fb = ( |
            | 
            lens for: o SetMarkValue: markValue IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: iterating\x7fComment: Invokes the block for each word in the object starting
with the word at ``firstIndex\'\', and ending when the
trailing mark is reached.  -- jb 7/03\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o StartingAt: firstIndex Do: blk IfFail: fb = ( |
            | 
            "Iterate until we find the trailing mark word."

                   for: o
            StartingAt: firstIndex
                 Until: [|:i| for: o IsMarkAt: i]
                    Do: blk
                IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o StartingAt: firstIndex Until: untilBlk Do: blk IfFail: fb = ( |
             nextWordIndex.
             topWordIndex.
            | 
            "Note: We could walk right off of the end of the object heap
                   if the trailing mark is missing (due to a bug).
                   The assertion is designed to detect this case.
                   However, the assertion might fail when remotely
                   debugging a VM, unless the local copy of the universe
                   is kept up-to-date.  -- jb 7/03"

            theVM assert: [| addr |
              addr: addressOfMem: o.
              topWordIndex: ((theVM universe objsTopFor: addr) - addr) / oopSize.
              true
            ].

            "Iterate until untilBlk returns true."
            nextWordIndex: firstIndex.
            [(untilBlk value: nextWordIndex) || [nextWordIndex = 0]]  whileFalse: [
              theVM assert: [ nextWordIndex < topWordIndex ].
              blk value: (for: o At: nextWordIndex IfFail: [|:e| ^ fb value: e])
                   With: nextWordIndex.
              nextWordIndex: nextWordIndex succ.
            ].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalObject: o At: i IfFail: fb = ( |
            | 
            _ForMemoryObject: o At: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: writing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalObject: o At: i Put: x IfFail: fb = ( |
            | _ForMemoryObject: o At: i Put: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing bytes\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalObject: o ByteAt: i IfFail: fb = ( |
            | 
            _ForMemoryObject: o ByteAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing bytes\x7fCategory: writing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalObject: o ByteAt: i Put: x IfFail: fb = ( |
            | 
            _ForMemoryObject: o ByteAt: i Put: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalObject: o IsMarkAt: i IfFail: fb = ( |
            | 
            _ForMemoryObject: o IsMarkAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: writing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalObject: o SetMarkValue: markValue IfFail: fb = ( |
            | 
            _NoGCAllowed.
            _ForMemoryObject: o SetMarkValue: markValue IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteObject: o At: i IfFail: fb = ( |
            | 
            machineMemory oopAt: (for: o AddressAt: i IfFail: [|:e| ^ fb value: e])
                         IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fCategory: writing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteObject: o At: i Put: x IfFail: fb = ( |
            | 
            machineMemory at: (for: o AddressAt: i IfFail: [|:e| ^ fb value: e])
                      PutOop: x
                      IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing bytes\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteObject: o ByteAt: i IfFail: fb = ( |
            | 
            machineMemory byteAt: (for: o AddressOfByteAt: i IfFail: [|:e| ^ fb value: e])
                          IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing bytes\x7fCategory: writing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteObject: o ByteAt: i Put: x IfFail: fb = ( |
            | 
            machineMemory at: (for: o AddressOfByteAt: i IfFail: [|:e| ^ fb value: e])
                     PutByte: x
                      IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteObject: o IsMarkAt: i IfFail: fb = ( |
            | 
            isMark: for: o At: i IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: writing\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteObject: o SetMarkValue: markValue IfFail: fb = ( |
            | 
            markField setValueFor: o To: markValue Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing all words of object including header\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         immediateValueHeaderFieldMixin = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'immediateValueHeaderFieldMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts memoryObject immediateValueHeaderFieldMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'immediateValueHeaderFieldMixin' -> () From: ( | {
         'Category: verifying\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         basicVerifyObject: o At: i Layout: aLayout With: aVerifier = ( |
            | 
            aLayout for: o VerifyImmediate: immediateLayoutForValue At: i With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'immediateValueHeaderFieldMixin' -> () From: ( | {
         'Category: accessing value\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         oopForValue: v = ( |
            | 
            immediateLayoutForValue oopForValue: v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'immediateValueHeaderFieldMixin' -> () From: ( | {
         'Category: accessing value\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueOf: o = ( |
            | 
            immediateLayoutForValue valueOf: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isActivationMap: o IfFail: fb = ( |
            | layouts mark isMarkValueForActivationMap:  markValueOf: o IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isImmediate = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: header\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         lastField = ( |
            | 
            lastHeaderField).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: header\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         lastHeaderField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: converting address to reference\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         localMemForAddress: address = ( |
            | 
            theVM objectLocator
              localMemForAddress: address).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: converting address to reference\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         localMemForAddress: address OID: oid = ( |
            | 
            theVM objectLocator
              localMemForAddress: address OID: oid).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         markField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'markField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts memoryObject markField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'markField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueMixin* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'oopValueHeaderFieldMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: header\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         mapIndex = ( |
            | 
            mapField fixedIndex).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing map\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOf: o IfFail: fb = ( |
            | 
            mapField valueFor: o Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing map\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOfObjectWithAddress: addr = ( |
            | 
            mapField valueForObjectWithAddress: addr Layout: self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'markField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'markField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> 'noPrecedingField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'markField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueMixin* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'markField' -> 'valueMixin' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts memoryObject markField valueMixin.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'markField' -> 'valueMixin' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         immediateLayoutForValue = ( |
            | 
            0 kleinAndYoda layouts mark).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'markField' -> 'valueMixin' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'immediateValueHeaderFieldMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         markValueOf: o = ( |
            | 
            markValueOf: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         markValueOf: o IfFail: fb = ( |
            | 
            lens markValueOf: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         markValueOfLocalObject: o IfFail: fb = ( |
            | 
            _MarkValueOfMemoryObject: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         markValueOfLocalObjectWithAddress: addr IfFail: fb = ( |
            | 
            markField valueForObjectWithAddress: addr Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         markValueOfRemoteObject: o IfFail: fb = ( |
            | 
            markField valueFor: o Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing mark\x7fCategory: reading\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         markValueOfRemoteObjectWithAddress: addr IfFail: fb = ( |
            | 
            markField valueForObjectWithAddress: addr Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: converting address to reference\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         memForAddress: address = ( |
            | 
            lens memForAddress: address).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: converting address to reference\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         memForAddress: address OID: oid = ( |
            | 
            lens memForAddress: address OID: oid).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: secondary (i.e. what kind of mem) tests\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         memIsByteVector: o = ( |
            | 
            layouts mark isMarkValueForByteVector: markValueOf: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: secondary (i.e. what kind of mem) tests\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         memIsYoung: o = ( |
            | 
            theVM universe edenSpace includesAddress: addressOfMem: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: oop constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         myTag = ( |
            | 
            vmKit tag mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing OID\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oidOf: o = ( |
            | 
            oidOf: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing OID\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oidOf: o IfFail: fb = ( |
            | 
            layouts mark oidOfMarkValue:  markValueOf: o IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: accessing OID\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oidOfRemoteObjectWithAddress: addr IfFail: fb = ( |
            | 
            layouts mark oidOfMarkValue:
               markValueOfRemoteObjectWithAddress: addr IfFail: [|:e| ^ fb value: e]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: converting reference to address\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oopOfLocalMem: mem = ( |
            | 
            theVM objectLocator oopOfLocalMem: mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: converting reference to address\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oopOfMem: mem = ( |
            | 
            lens oopOfMem: mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: converting reference to address\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oopOfRemoteMem: mem = ( |
            | 
            mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'oopValueHeaderFieldMixin' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         oopForValue: v = ( |
            | 
            v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'oopValueHeaderFieldMixin' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         valueOf: o = ( |
            | 
            o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: converting address to reference\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteMemForAddress: address = ( |
            | 
            theVM objectLocator
              remoteMemForAddress: address).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: converting address to reference\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         remoteMemForAddress: address OID: oid = ( |
            | 
            theVM objectLocator
              remoteMemForAddress: address OID: oid).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'smiValueHeaderFieldMixin' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         immediateLayoutForValue = ( |
            | 
            0 kleinAndYoda layouts smi).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'smiValueHeaderFieldMixin' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'immediateValueHeaderFieldMixin' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: getting size\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         wordSizeOf: o = ( |
            | wordSizeOf: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: getting size\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         wordSizeOf: o IfFail: fb = ( |
             size.
            | 
            size: emptyObjectSizeFor: o.
            for: o StartingAt: size Do: [size: size succ] IfFail: [|:e| ^ fb value: e].
            size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> () From: ( | {
         'Category: getting size\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         wordSizeOfObjectWithAddress: addr = ( |
             isMark.
             mm.
             oopSize.
             size.
            | 
            "Avoiding cloning so we can use this when recycling oops."
            mm: machineMemory.
            size: lastField fixedIndexAfterMe.
            oopSize: self oopSize.

            __DefineLabel: 'startOfLoop'.
            isMark:  mm isMarkAtOffset: size From: addr.
            __BranchIfTrue: isMark To: 'foundNextMark'.
            size: size _IntAdd: 1.
            __BranchTo: 'startOfLoop'.
            __DefineLabel: 'foundNextMark'.

            size).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> 'abstractIndexableField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         offsetOfIndexZeroFor: o Layout: aLayout = ( |
            | 
            aLayout indexableOriginOf: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> 'abstractIndexableField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> 'abstractIndexableField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         setValueFor: o To: v Origin: io Layout: aLayout IfFail: fb = ( |
            | 
            "Use _IntAdd: instead of + so as not to clone
             any blocks. -- Adam, 4/06"
            setBasicValueFor: o At: (io _IntAdd: indexFor: o Layout: aLayout) To: v Layout: aLayout IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> 'abstractIndexableField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         valueFor: o Origin: io Layout: aLayout IfFail: fb = ( |
            | 
            "Use _IntAdd: instead of + so as not to clone
             any blocks. -- Adam, 4/06"
            basicValueFor: o At: (io _IntAdd: indexFor: o Layout: aLayout) Layout: aLayout IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         elementByteSize = ( |
            | 
            oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o UncheckedAddressOfIndexableAt: i IndexableOrigin: origin IfFail: fb = ( |
            | 
                  for: o
            AddressAt: i + origin
               IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o UncheckedIndexableAt: i IndexableOrigin: origin IfFail: fb = ( |
            | 
               for: o
                At: i + origin
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o UncheckedIndexableAt: i Put: x IndexableOrigin: origin IfFail: fb = ( |
            | 
               for: o
                At: i + origin
               Put: x
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: iterating\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         indexablesAtAddress: firstAddr Size: s Do: blk IfFail: fb = ( |
            | 
            theVM machineMemory wordsAt: firstAddr Size: s Do: blk IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: layout constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         wordsNeededForAssignableFakeSlotsIn: mir = ( |
            | 
              (resend.wordsNeededForAssignableFakeSlotsIn: mir)
            + mir reflecteeHasMethodPointer asInteger).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'objVector' -> () From: ( | {
         'Category: getting size of whole object\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         wordsNeededToHoldIndexablesOfSize: s = ( |
            | 
            s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         decode: i = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         does: o HaveTag: t = ( |
            | 
            "Using the primitive directly so that this can be
             called inside Klein in places where cloning is
             disallowed. -- Adam, Mar. 2009"
            (tagOf: o) _IntEQ: t).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: encoding & decoding tagged oops\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         encode: i = ( |
            | 
            childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         if: o IsFloat: fb IsSmi: sb IsMark: markb IsMem: mb = ( |
            | 
            vmKit tag
                ifTag: (tagOf: o)
              IsFloat: [   fb value: layouts float       ]
                IsSmi: [   sb value: layouts smi         ]
               IsMark: [markb value: layouts mark        ]
                IsMem: [   mb value: layouts memoryObject]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         if: o IsImmediate: immBlk IsMark: markBlk IsMem: memBlk = ( |
            | 
                 if: o
            IsFloat: immBlk
              IsSmi: immBlk
             IsMark: markBlk
              IsMem: memBlk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: secondary (i.e. what kind of mem) tests\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isByteVector: o = ( |
            | 
            (isMem: o) && [layouts memoryObject memIsByteVector: o]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: isFloat\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isFloat: o = ( |
            | 
            does: o HaveTag: vmKit tag float).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: testing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isImmediate = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: isMark\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isMark: o = ( |
            | 
            does: o HaveTag: vmKit tag mark).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: isMem\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isMem: o = ( |
            | 
            does: o HaveTag: vmKit tag mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: primary  (i.e. tag) type tests\x7fCategory: isSmi\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isSmi: o = ( |
            | 
            does: o HaveTag: vmKit tag smi).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: type tests -- placed here because we don\'t know the nature of a thing until we ask\x7fCategory: secondary (i.e. what kind of mem) tests\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         isYoung: o = ( |
            | 
            (isMem: o) && [layouts memoryObject memIsYoung: o]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: accessing map\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOf: o = ( |
            | mapOf: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: accessing map\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOf: o IfFail: fb = ( |
            | 
            lens mapOf: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: accessing map\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOfLocalObject: o IfFail: fb = ( |
            | o _Map).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: accessing map\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOfRemoteObject: o IfFail: fb = ( |
            | 
                     if: o
            IsImmediate: [|:layout| theVM oopForOID: (theVM image objectsOracle mapOIDForExemplar: layout exemplar) IfAbsent: fb]
                 IsMark: [error: 'mark']
                  IsMem: [layouts memoryObject mapOf: o IfFail: fb]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: oop constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         myTag = ( |
            | childMustImplement).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstract' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: getting the tag from an object reference\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         tagOf: o = ( |
            | lens tagOf: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: getting the tag from an object reference\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         tagOfLocalObject: o = ( |
            | 
            o _TagPartOfObjectReference).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'object' -> () From: ( | {
         'Category: getting the tag from an object reference\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         tagOfRemoteObject: o = ( |
            | 
            vmKit tag tagOfOop: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         segregatedByteVector = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts segregatedByteVector.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         addressOfFirstIndexableIn: o IfFail: fb = ( |
             bpRef.
            | 
            bpRef: bytesPartRefFor: o IfFail: [|:e| ^ fb value: e].
            layouts bytesPart addressOfFirstByteInBytesPart: bpRef).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: importing objects (mirrors)\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesFrom: o IfFail: fb = ( |
            | 
            layouts bytesPart bytesOfBytesPart: (bytesPartRefFor: o IfFail: [|:e| ^ fb value: e])
                                        IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         bytesPartRefField = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> 'bytesPartRefField' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts segregatedByteVector bytesPartRefField.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> 'bytesPartRefField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         basicValueFor: o At: i Layout: aLayout IfFail: fb = ( |
            | 
            aLayout for: o BytesPartRefAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> 'bytesPartRefField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         basicVerifyObject: o At: i Layout: aLayout With: aVerifier = ( |
            | 
            aLayout for: o VerifySmiAt: i With: aVerifier).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> 'bytesPartRefField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         cClassName = 'BytesPartRefValueHeaderField'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> 'bytesPartRefField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'abstractHeaderField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> 'bytesPartRefField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         precedingField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'memoryObject' -> 'mapField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> 'bytesPartRefField' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         setBasicValueFor: o At: i To: v Layout: aLayout IfFail: fb = ( |
            | 
            aLayout for: o At: i PutBytesPartRef: v IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         bytesPartRefFor: o = ( |
            | 
            bytesPartRefFor: o IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         bytesPartRefFor: o IfFail: fb = ( |
            | 
            bytesPartRefField valueFor: o Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: cloning\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBytesFrom: o To: c IfFail: fb = ( |
             newBPRef.
             newSize.
             theOriginalBPRef.
             theOriginalSize.
            | 
            theOriginalBPRef:  bytesPartRefFor: o IfFail: [|:e| ^ fb value: e].
            theOriginalSize:   indexableSizeOf: o IfFail: [|:e| ^ fb value: e].

            newSize:   theOriginalSize.
            newBPRef: layouts bytesPart allocateBytesPartWithIndexableSize: newSize.
            for: c SetBytesPartRef: newBPRef IfFail: [|:e| ^ fb value: e].

            layouts bytesPart
                  copyBytesFrom: theOriginalBPRef
                   WhichHasSize: theOriginalSize
                             To: newBPRef
                   WhichHasSize: newSize
                    FillingWith: 0
                         IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: cloning\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         copyBytesFrom: o To: c IndexableSize: newSize FillingWith: filler IfFail: fb = ( |
             newBPRef.
             theOriginalBPRef.
             theOriginalSize.
            | 
            theOriginalBPRef:  bytesPartRefFor: o IfFail: [|:e| ^ fb value: e].
            theOriginalSize:   indexableSizeOf: o IfFail: [|:e| ^ fb value: e].

            newBPRef: layouts bytesPart allocateBytesPartWithIndexableSize: newSize.
            for: c SetBytesPartRef: newBPRef IfFail: [|:e| ^ fb value: e].

            layouts bytesPart
                  copyBytesFrom: theOriginalBPRef
                   WhichHasSize: theOriginalSize
                             To: newBPRef
                   WhichHasSize: newSize
                    FillingWith: 0
                         IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o AddressOfIndexableAt: i IfFail: fb = ( |
            | 
            layouts bytesPart
              forBytesPart: (bytesPartRefFor: o IfFail: [|:e| ^ fb value: e])
                 AddressAt: i
                    IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o At: i PutBytesPartRef: bpRef = ( |
            | 
            for: o At: i PutBytesPartRef: bpRef IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o At: i PutBytesPartRef: bpRef IfFail: fb = ( |
            | 
            lens for: o WithLayout: self At: i PutBytesPartRef: bpRef IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o BytesPartRefAt: i IfFail: fb = ( |
            | 
            lens for: o WithLayout: self BytesPartRefAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i IfFail: fb = ( |
            | 
            layouts bytesPart
              forBytesPart: (bytesPartRefFor: o IfFail: [|:e| ^ fb value: e])
                        At: i
                    IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IndexableAt: i Put: x IfFail: fb = ( |
            | 
            layouts bytesPart
              forBytesPart: (bytesPartRefFor: o IfFail: [|:e| ^ fb value: e])
                        At: i
                       Put: x
                    IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o SetBytesPartRef: bpRef = ( |
            | 
            for: o SetBytesPartRef: bpRef IfFail: raiseError).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o SetBytesPartRef: bpRef IfFail: fb = ( |
            | 
            bytesPartRefField setValueFor: o To: bpRef Layout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o UncheckedIndexableAt: i Put: x IfFail: fb = ( |
            | 
            layouts bytesPart
              forBytesPart: (bytesPartRefFor: o IfFail: [|:e| ^ fb value: e])
               UncheckedAt: i
                       Put: x
                    IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalObject: o At: i PutBytesPartRef: bpRef IfFail: fb = ( |
            | 
            [todo bytesPartRefFormat].
            _ForMemoryObject: o At: i Put: bpRef IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forLocalObject: o BytesPartRefAt: i IfFail: fb = ( |
            | 
            [todo bytesPartRefFormat].
            _ForMemoryObject: o At: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forObjectWithAddress: addr SetBytesPartRef: bpRef IfFail: fb = ( |
            | 
            machineMemory at: addr + (bytesPartRefField fixedIndex * oopSize)
                     PutWord: bpRef
                      IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteObject: o At: i PutBytesPartRef: bpRef IfFail: fb = ( |
            | 
            [todo bytesPartRefFormat].
            machineMemory at: (for: o AddressAt: i IfFail: [|:e| ^ fb value: e])
                     PutWord: bpRef
                      IfFail: [|:e| ^ fb value: e].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fCategory: double dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forRemoteObject: o BytesPartRefAt: i IfFail: fb = ( |
            | 
            [todo bytesPartRefFormat].
            machineMemory wordAt: (for: o AddressAt: i IfFail: [|:e| ^ fb value: e])
                          IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing indexable size field\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeOf: o IfFail: fb = ( |
            | 
            layouts bytesPart
              indexableSizeOfBytesPart: (bytesPartRefFor: o IfFail: [|:e| ^ fb value: e])
                                IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: layout constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         lastField = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> 'bytesPartRefField' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'segregatedByteVector' -> () From: ( | {
         'Category: accessing bytes part reference field\x7fCategory: relocation\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateBytesPartRefAt: addr By: delta = ( |
             o.
             r.
            | 
            o: memForAddress: addr.
            r: (delta + bytesPartRefFor: o) asInt32.
            for: o SetBytesPartRef: r.
            theVM assert: [theVM universe bytesIncludesAddress: layouts bytesPart addressOfBytesPart: r].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: immediates\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         smi = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'smi' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts smi.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'smi' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         exemplar = 0.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'smi' -> () From: ( | {
         'Category: accessing\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         mapPrototype = ( |
            | 
            vmKit maps smiMap).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'smi' -> () From: ( | {
         'Category: oop constants\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         myTag = ( |
            | 
            vmKit tag smi).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'smi' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'immediate' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> () From: ( | {
         'Category: objects\x7fCategory: memory objects\x7fCategory: vectors\x7fCategory: byte vectors\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         unsegregatedByteVector = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals kleinAndYoda layouts unsegregatedByteVector.
\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: importing objects (mirrors)\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesFrom: o IfFail: fb = ( |
            | 
            lens bytesFrom: o WithLayout: self IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: getting size of whole object\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesNeededToHoldBytes: nBytes = ( |
            | 
            (oopsNeededToHoldBytes: nBytes) _IntMul: oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: importing objects (mirrors)\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesOfLocalObject: o IfFail: fb = ( |
            | 
            o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: importing objects (mirrors)\x7fCategory: double-dispatch\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesOfRemoteObject: o IfFail: fb = ( |
             addr.
             size.
            | 
            addr: addressOfFirstIndexableIn: o IfFail: [|:e| ^ fb value: e].
            size: indexableSizeOf:           o IfFail: [|:e| ^ fb value: e].

            machineMemory bytesAt: addr Size: size IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         elementByteSize = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: iterating\x7fComment: Invokes the block for each word in the object starting
with the word at ``firstIndex\'\', and ending when the
end of the oops part of the byteVector is reached.  -- Adam, 4/06\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o StartingAt: firstIndex Do: blk IfFail: fb = ( |
             origin.
            | 
            origin: indexableOriginOf: o IfFail: [|:e| ^ fb value: e].

            "Iterate until we reach the end of the oops part of the byteVector."

                   for: o
            StartingAt: firstIndex
                 Until: [|:i| i = origin]
                    Do: blk
                IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o UncheckedAddressOfIndexableAt: i IndexableOrigin: origin IfFail: fb = ( |
            | 
            for: o AddressOfByteAt: (origin * oopSize) + i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o UncheckedIndexableAt: i IndexableOrigin: origin IfFail: fb = ( |
            | 
               for: o
            ByteAt: (origin * oopSize) + i
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: accessing indexables\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         for: o UncheckedIndexableAt: i Put: x IndexableOrigin: origin IfFail: fb = ( |
            | 
               for: o
            ByteAt: (origin * oopSize) + i
               Put: x
            IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: getting size of whole object\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oopsNeededToHoldBytes: nBytes = ( |
            | 
            (nBytes _IntAdd:  oopSize _IntSub: 1) _IntDiv: oopSize).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'abstractUnsegregatedVector' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: relocation\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         relocateBytesPartRefAt: addr By: delta = ( |
            | 
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'layouts' -> 'unsegregatedByteVector' -> () From: ( | {
         'Category: getting size of whole object\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         wordsNeededToHoldIndexablesOfSize: s = ( |
            | 
            oopsNeededToHoldBytes: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfBytesPart: bpRef = ( |
            | layouts bytesPart addressOfLocalBytesPart: bpRef).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfMem: mem = ( |
            | layouts memoryObject addressOfLocalMem: mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: byte vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesFrom: o WithLayout: layout IfFail: fb = ( |
            | 
            layout bytesOfLocalObject: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesOfBytesPart: bpRef IfFail: fb = ( |
            | layouts bytesPart bytesOfLocalBytesPart: bpRef IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesPartRefForAddress: address = ( |
            | layouts bytesPart localBytesPartRefForAddress: address).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o At: i IfFail: fb = ( |
            | layouts memoryObject forLocalObject: o At: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o At: i Put: x IfFail: fb = ( |
            | layouts memoryObject forLocalObject: o At: i Put: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o ByteAt: i IfFail: fb = ( |
            | layouts memoryObject forLocalObject: o ByteAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o ByteAt: i Put: x IfFail: fb = ( |
            | layouts memoryObject forLocalObject: o ByteAt: i Put: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IsMarkAt: i IfFail: fb = ( |
            | layouts memoryObject forLocalObject: o IsMarkAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetMarkValue: markValue IfFail: fb = ( |
            | layouts memoryObject forLocalObject: o SetMarkValue: markValue IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: byte vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o WithLayout: layout At: i PutBytesPartRef: bpRef IfFail: fb = ( |
            | 
            layout forLocalObject: o At: i PutBytesPartRef: bpRef IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: object vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o WithLayout: aLayout AtMost: n IndexablesDo: blk IfFail: fb = ( |
            | 
            aLayout forLocal: o AtMost: n IndexablesDo: blk IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: byte vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o WithLayout: layout BytesPartRefAt: i IfFail: fb = ( |
            | 
            layout forLocalObject: o BytesPartRefAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: byte vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o WithLayout: layout SetBytes: bv IfFail: fb = ( |
            | 
            layout forLocalObject: o SetBytes: bv IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef At: i IfFail: fb = ( |
            | layouts bytesPart forLocalBytesPart: bpRef At: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef SetBytes: bv IfFail: fb = ( |
            | layouts bytesPart forLocalBytesPart: bpRef SetBytes: bv IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef SetIndexableSize: s IfFail: fb = ( |
            | layouts bytesPart forLocalBytesPart: bpRef SetIndexableSize: s IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef UncheckedAt: i Put: b IfFail: fb = ( |
            | 
            layouts bytesPart forLocalBytesPart: bpRef UncheckedAt: i Put: b IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: free list layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forEntryAtAddress: addr SetNextEntry: nextEntryShiftedAddr Layout: aLayout = ( |
            | 
            aLayout forLocalEntryAtAddress: addr SetNextEntry: nextEntryShiftedAddr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: free list layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forEntryAtAddress: addr SetSize: s Layout: aLayout = ( |
            | 
            aLayout forLocalEntryAtAddress: addr SetSize: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeOfBytesPart: bpRef IfFail: fb = ( |
            | layouts bytesPart indexableSizeOfLocalBytesPart: bpRef IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOf: o IfFail: fb = ( |
            | layouts object mapOfLocalObject: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         markValueOf: o IfFail: fb = ( |
            | layouts memoryObject markValueOfLocalObject: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         memForAddress: address = ( |
            | layouts memoryObject localMemForAddress: address).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         memForAddress: address OID: oid = ( |
            | 
            layouts memoryObject localMemForAddress: address OID: oid).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: free list layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         nextEntryForEntryAtAddress: addr Layout: aLayout = ( |
            | 
            aLayout nextEntryForLocalEntryAtAddress: addr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: immediate layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForValue: v WithLayout: lo = ( |
            | 
            lo localOopForValue: v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oopOfMem: mem = ( |
            | layouts memoryObject oopOfLocalMem: mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: free list layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         sizeForEntryAtAddress: addr Layout: aLayout = ( |
            | 
            aLayout sizeForLocalEntryAtAddress: addr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         tagOf: o = ( |
            | layouts object tagOfLocalObject: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'localObjectLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: immediate layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         valueOf: oop WithLayout: lo = ( |
            | 
            lo valueOfLocal: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfBytesPart: bpRef = ( |
            | layouts bytesPart addressOfRemoteBytesPart: bpRef).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         addressOfMem: mem = ( |
            | layouts memoryObject addressOfRemoteMem: mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: byte vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesFrom: o WithLayout: layout IfFail: fb = ( |
            | 
            layout bytesOfRemoteObject: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesOfBytesPart: bpRef IfFail: fb = ( |
            | layouts bytesPart bytesOfRemoteBytesPart: bpRef IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         bytesPartRefForAddress: address = ( |
            | layouts bytesPart remoteBytesPartRefForAddress: address).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o At: i IfFail: fb = ( |
            | layouts memoryObject forRemoteObject: o At: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o At: i Put: x IfFail: fb = ( |
            | layouts memoryObject forRemoteObject: o At: i Put: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o ByteAt: i IfFail: fb = ( |
            | layouts memoryObject forRemoteObject: o ByteAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o ByteAt: i Put: x IfFail: fb = ( |
            | layouts memoryObject forRemoteObject: o ByteAt: i Put: x IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o IsMarkAt: i IfFail: fb = ( |
            | layouts memoryObject forRemoteObject: o IsMarkAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o SetMarkValue: markValue IfFail: fb = ( |
            | layouts memoryObject forRemoteObject: o SetMarkValue: markValue IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: byte vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o WithLayout: layout At: i PutBytesPartRef: bpRef IfFail: fb = ( |
            | 
            layout forRemoteObject: o At: i PutBytesPartRef: bpRef IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: object vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o WithLayout: aLayout AtMost: n IndexablesDo: blk IfFail: fb = ( |
            | 
            aLayout forRemote: o AtMost: n IndexablesDo: blk IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: byte vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o WithLayout: layout BytesPartRefAt: i IfFail: fb = ( |
            | 
            layout forRemoteObject: o BytesPartRefAt: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: byte vector layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         for: o WithLayout: layout SetBytes: bv IfFail: fb = ( |
            | 
            layout forRemoteObject: o SetBytes: bv IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef At: i IfFail: fb = ( |
            | layouts bytesPart forRemoteBytesPart: bpRef At: i IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef SetBytes: bv IfFail: fb = ( |
            | layouts bytesPart forRemoteBytesPart: bpRef SetBytes: bv IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef SetIndexableSize: s IfFail: fb = ( |
            | layouts bytesPart forRemoteBytesPart: bpRef SetIndexableSize: s IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forBytesPart: bpRef UncheckedAt: i Put: b IfFail: fb = ( |
            | 
            layouts bytesPart forRemoteBytesPart: bpRef UncheckedAt: i Put: b IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: free list layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forEntryAtAddress: addr SetNextEntry: nextEntryShiftedAddr Layout: aLayout = ( |
            | 
            aLayout forRemoteEntryAtAddress: addr SetNextEntry: nextEntryShiftedAddr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: free list layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         forEntryAtAddress: addr SetSize: s Layout: aLayout = ( |
            | 
            aLayout forRemoteEntryAtAddress: addr SetSize: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: bytes part layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         indexableSizeOfBytesPart: bpRef IfFail: fb = ( |
            | layouts bytesPart indexableSizeOfRemoteBytesPart: bpRef IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         mapOf: o IfFail: fb = ( |
            | layouts object mapOfRemoteObject: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         markValueOf: o IfFail: fb = ( |
            | layouts memoryObject markValueOfRemoteObject: o IfFail: fb).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         memForAddress: address = ( |
            | layouts memoryObject remoteMemForAddress: address).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         memForAddress: address OID: oid = ( |
            | 
            layouts memoryObject remoteMemForAddress: address OID: oid).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: free list layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         nextEntryForEntryAtAddress: addr Layout: aLayout = ( |
            | 
            aLayout nextEntryForRemoteEntryAtAddress: addr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: immediate layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oopForValue: v WithLayout: lo = ( |
            | 
            lo remoteOopForValue: v).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: memory object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         oopOfMem: mem = ( |
            | layouts memoryObject oopOfRemoteMem: mem).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: free list layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         sizeForEntryAtAddress: addr Layout: aLayout = ( |
            | 
            aLayout sizeForRemoteEntryAtAddress: addr).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: object layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         tagOf: o = ( |
            | layouts object tagOfRemoteObject: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'kleinAndYoda' -> 'memoryLens' -> () From: ( | {
         'Category: double-dispatch\x7fCategory: layouts\x7fCategory: immediate layout\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         valueOf: oop WithLayout: lo = ( |
            | 
            lo valueOfRemote: oop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot'
        
         vmKitLayouts = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLayouts' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLayouts' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules vmKitLayouts.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLayouts' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/klein'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLayouts' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLayouts' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLayouts' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLayouts' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision: 30.11 $'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'vmKitLayouts' -> () From: ( | {
         'ModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'vmKitVarHdrs
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'float' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinAndYodaLayout = ( |
            | 
            kleinAndYoda layouts float).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'mirrors' -> 'smallInt' -> () From: ( | {
         'Category: klein and yoda\x7fModuleInfo: Module: vmKitLayouts InitialContents: FollowSlot\x7fVisibility: public'
        
         kleinAndYodaLayout = ( |
            | 
            kleinAndYoda layouts smi).
        } | ) 



 '-- Sub parts'

 bootstrap read: 'vmKitVarHdrs' From: 'applications/klein'



 '-- Side effects'

 globals modules vmKitLayouts postFileIn
